#!/usr/bin/env python3
"""
Run the Blitz test suite N times each on two commits (base and head) and run a
one-sided Welch's t-test to detect a runtime regression.

Fails (exit 1) if the candidate (head) is statistically slower than the
baseline (base) at the configured significance level.

Timings are parsed from the C-backend test runner's summary line:
    test result: ok. <P> passed; <F> failed; finished in <MS>ms
"""

from __future__ import annotations

import argparse
import math
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
INTERPRETER_DIR = REPO_ROOT / "bootstrap" / "interpreter"
COMPILER_DIR = REPO_ROOT / "compiler"

# Strip ANSI color codes the runner emits.
ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")
# `finished in 12.34ms`
FINISHED_RE = re.compile(r"finished in\s+([0-9]+(?:\.[0-9]+)?)ms")


def run(cmd: list[str], cwd: Path | None = None, env: dict | None = None) -> subprocess.CompletedProcess:
    return subprocess.run(
        cmd,
        cwd=cwd,
        env=env,
        check=True,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )


def checkout(commit: str) -> None:
    print(f"\n=== Checking out {commit} ===", flush=True)
    # Use --detach so we don't disturb branch state.
    run(["git", "-C", str(REPO_ROOT), "checkout", "--detach", commit])


def build_release() -> None:
    print("=== cargo build --release ===", flush=True)
    run(["cargo", "build", "--release"], cwd=INTERPRETER_DIR)


def parse_runtime(output: str) -> float:
    clean = ANSI_RE.sub("", output)
    matches = FINISHED_RE.findall(clean)
    if not matches:
        raise RuntimeError(
            "Could not find 'finished in <ms>ms' in test runner output:\n" + clean[-2000:]
        )
    # The runner prints the summary once per invocation; take the last match to
    # be safe in case of nested output.
    return float(matches[-1])


def measure_once() -> float:
    # Use the already-built release binary directly to avoid cargo overhead in
    # the timing path. cargo build was a no-op-ish step earlier.
    binary = INTERPRETER_DIR / "target" / "release" / "blitz"
    if not binary.exists():
        # Fallback: locate it via cargo metadata-style guess
        candidates = list((INTERPRETER_DIR / "target" / "release").glob("*"))
        raise FileNotFoundError(
            f"Release binary not found at {binary}. Candidates: {candidates}"
        )
    proc = subprocess.run(
        [str(binary), "test", "-c", str(COMPILER_DIR)],
        check=False,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"Test suite failed (exit {proc.returncode}):\n{proc.stdout[-4000:]}"
        )
    return parse_runtime(proc.stdout)


def measure_many(label: str, runs: int) -> list[float]:
    print(f"\n=== Measuring '{label}' ({runs} runs) ===", flush=True)
    samples: list[float] = []
    for i in range(1, runs + 1):
        ms = measure_once()
        samples.append(ms)
        print(f"  [{label} {i:>2}/{runs}] {ms:.2f} ms", flush=True)
    return samples


def mean(xs: list[float]) -> float:
    return sum(xs) / len(xs)


def variance(xs: list[float]) -> float:
    m = mean(xs)
    return sum((x - m) ** 2 for x in xs) / (len(xs) - 1)


def welch_t(base: list[float], head: list[float]) -> tuple[float, float]:
    """Returns (t_statistic, degrees_of_freedom) for Welch's t-test."""
    m1, m2 = mean(base), mean(head)
    v1, v2 = variance(base), variance(head)
    n1, n2 = len(base), len(head)
    se = math.sqrt(v1 / n1 + v2 / n2)
    if se == 0.0:
        # Degenerate case: identical samples → no evidence of regression.
        return 0.0, float(n1 + n2 - 2)
    # Test direction: head - base > 0 means regression (head is slower).
    t = (m2 - m1) / se
    df_num = (v1 / n1 + v2 / n2) ** 2
    df_den = (v1 / n1) ** 2 / (n1 - 1) + (v2 / n2) ** 2 / (n2 - 1)
    df = df_num / df_den if df_den > 0 else float(n1 + n2 - 2)
    return t, df


def regularized_incomplete_beta(a: float, b: float, x: float) -> float:
    """I_x(a, b) via Lentz's continued fraction. Used for the t-distribution CDF."""
    if x <= 0.0:
        return 0.0
    if x >= 1.0:
        return 1.0

    # log of the prefactor B(a,b,x) = x^a (1-x)^b / (a B(a,b))
    log_bt = (
        math.lgamma(a + b)
        - math.lgamma(a)
        - math.lgamma(b)
        + a * math.log(x)
        + b * math.log(1.0 - x)
    )
    bt = math.exp(log_bt)

    # Use the symmetry: I_x(a,b) = 1 - I_{1-x}(b,a) when x is in the upper tail
    # of the series, for numerical stability.
    if x < (a + 1.0) / (a + b + 2.0):
        return bt * _betacf(a, b, x) / a
    return 1.0 - bt * _betacf(b, a, 1.0 - x) / b


def _betacf(a: float, b: float, x: float, max_iter: int = 200, eps: float = 3e-16) -> float:
    qab = a + b
    qap = a + 1.0
    qam = a - 1.0
    c = 1.0
    d = 1.0 - qab * x / qap
    if abs(d) < 1e-300:
        d = 1e-300
    d = 1.0 / d
    h = d
    for m in range(1, max_iter + 1):
        m2 = 2 * m
        aa = m * (b - m) * x / ((qam + m2) * (a + m2))
        d = 1.0 + aa * d
        if abs(d) < 1e-300:
            d = 1e-300
        c = 1.0 + aa / c
        if abs(c) < 1e-300:
            c = 1e-300
        d = 1.0 / d
        h *= d * c
        aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
        d = 1.0 + aa * d
        if abs(d) < 1e-300:
            d = 1e-300
        c = 1.0 + aa / c
        if abs(c) < 1e-300:
            c = 1e-300
        d = 1.0 / d
        delta = d * c
        h *= delta
        if abs(delta - 1.0) < eps:
            return h
    raise RuntimeError("betacf failed to converge")


def t_sf(t: float, df: float) -> float:
    """Survival function P(T > t) for Student's t with `df` degrees of freedom."""
    # P(T > t) = 0.5 * I_{df/(df + t^2)}(df/2, 1/2)  for t >= 0
    # P(T > t) = 1 - 0.5 * I_{...}                    for t < 0
    x = df / (df + t * t)
    half_ix = 0.5 * regularized_incomplete_beta(df / 2.0, 0.5, x)
    return half_ix if t >= 0 else 1.0 - half_ix


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--base", required=True, help="Baseline commit SHA")
    parser.add_argument("--head", required=True, help="Candidate commit SHA")
    parser.add_argument("--runs", type=int, default=30)
    parser.add_argument("--alpha", type=float, default=0.05)
    args = parser.parse_args()

    if args.runs < 3:
        print("Need at least 3 runs per side for the t-test.", file=sys.stderr)
        return 2

    if args.base == args.head:
        print(f"Base and head are the same commit ({args.base}); nothing to compare.")
        return 0

    # Clean up any stale build artefacts that could skew the first measurement.
    c_out = INTERPRETER_DIR / "c-out"
    if c_out.exists():
        shutil.rmtree(c_out)

    # --- Baseline ---
    checkout(args.base)
    build_release()
    base_samples = measure_many("base", args.runs)

    # --- Candidate ---
    checkout(args.head)
    build_release()
    head_samples = measure_many("head", args.runs)

    base_mean = mean(base_samples)
    head_mean = mean(head_samples)
    base_sd = math.sqrt(variance(base_samples))
    head_sd = math.sqrt(variance(head_samples))

    t, df = welch_t(base_samples, head_samples)
    # One-sided test: H1 = head is slower than base (mean_head > mean_base).
    p = t_sf(t, df)

    delta = head_mean - base_mean
    pct = (delta / base_mean) * 100.0 if base_mean > 0 else float("nan")

    print("\n=== Results ===")
    print(f"  base: n={len(base_samples)}  mean={base_mean:.2f} ms  sd={base_sd:.2f} ms")
    print(f"  head: n={len(head_samples)}  mean={head_mean:.2f} ms  sd={head_sd:.2f} ms")
    print(f"  delta: {delta:+.2f} ms ({pct:+.2f}%)")
    print(f"  Welch's t = {t:.4f}, df = {df:.2f}")
    print(f"  one-sided p (head > base) = {p:.6f}")
    print(f"  alpha = {args.alpha}")

    summary_path = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary_path:
        with open(summary_path, "a") as f:
            f.write("## Performance regression check\n\n")
            f.write(f"| metric | base | head | delta |\n")
            f.write(f"|---|---|---|---|\n")
            f.write(
                f"| mean (ms) | {base_mean:.2f} | {head_mean:.2f} | {delta:+.2f} ({pct:+.2f}%) |\n"
            )
            f.write(f"| sd (ms)   | {base_sd:.2f} | {head_sd:.2f} | |\n\n")
            f.write(
                f"Welch's one-sided t-test: t={t:.4f}, df={df:.2f}, p={p:.6f}, alpha={args.alpha}\n"
            )

    if p < args.alpha and delta > 0:
        print(
            f"\nFAIL: head is slower than base "
            f"(p={p:.6f} < {args.alpha}, +{pct:.2f}%)."
        )
        return 1

    print("\nOK: no statistically significant slowdown detected.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
