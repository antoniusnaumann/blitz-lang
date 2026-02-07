use std::collections::HashMap;
use std::env::args;
use std::fs;
use std::panic;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use std::sync::RwLock;

use interpreter::DEBUG;
use interpreter::{run_checked, Body, Builtin, Registry, ROOT};
use parser::{Ast, Parser};

#[derive(Debug, Clone, PartialEq)]
enum Backend {
    Interpreter,
    C,
}

#[derive(Debug, Clone)]
enum Subcommand {
    Run,
    Test,
    Build,
}

fn print_usage() {
    eprintln!("Usage: interpreter <command> [options] <file-or-directory>");
    eprintln!();
    eprintln!("Commands:");
    eprintln!("  run     Run the program");
    eprintln!("  test    Run tests");
    eprintln!("  build   Build (compile) the program");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -c      Use the C backend instead of the interpreter");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  interpreter run main.blitz");
    eprintln!("  interpreter test -c ./compiler");
    eprintln!("  interpreter build -c ./compiler");
}

fn main() {
    let mut args: Vec<String> = args().collect();

    // Handle debug flag
    if args.len() > 1 && args[1] == "debug" {
        assert!(DEBUG.get_or_init(|| true));
        args.remove(1);
    } else {
        assert!(!DEBUG.get_or_init(|| false));
    }

    if args.len() < 2 {
        print_usage();
        std::process::exit(1);
    }

    // Handle help flag
    if args[1] == "--help" || args[1] == "-h" {
        print_usage();
        std::process::exit(0);
    }

    // Parse subcommand
    let subcommand = match args[1].as_str() {
        "run" => Subcommand::Run,
        "test" => Subcommand::Test,
        "build" => Subcommand::Build,
        // Legacy support: if first arg is a path, assume "run"
        path if Path::new(path).exists() || path.ends_with(".blitz") => {
            // Insert "run" as the subcommand
            args.insert(1, "run".to_string());
            Subcommand::Run
        }
        _ => {
            eprintln!("Unknown command: {}", args[1]);
            print_usage();
            std::process::exit(1);
        }
    };

    // Remove subcommand from args
    args.remove(1);

    // Parse backend flag (-c)
    let backend = if args.len() > 1 && args[1] == "-c" {
        args.remove(1);
        Backend::C
    } else {
        Backend::Interpreter
    };

    // Remaining args are input paths
    if args.len() < 2 {
        eprintln!("Error: No input file or directory specified");
        print_usage();
        std::process::exit(1);
    }

    let input_paths: Vec<PathBuf> = args[1..].iter().map(|s| PathBuf::from(s)).collect();

    // Determine the base directory for running tests/programs
    let base_dir = if input_paths[0].is_dir() {
        input_paths[0].clone()
    } else {
        input_paths[0]
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."))
    };

    // Collect all ASTs from input paths
    let mut all_asts = Vec::new();
    for input_path in &input_paths {
        let asts = collect_definitions(input_path);
        all_asts.extend(asts);
    }

    // Execute based on subcommand and backend
    match (subcommand, backend) {
        (Subcommand::Run, Backend::Interpreter) => {
            run_interpreter(all_asts, &input_paths[0]);
        }
        (Subcommand::Run, Backend::C) => {
            std::process::exit(run_c(&all_asts, &base_dir));
        }
        (Subcommand::Test, Backend::Interpreter) => {
            run_test_suite_interpreter(all_asts, &input_paths[0]);
        }
        (Subcommand::Test, Backend::C) => {
            std::process::exit(run_c_tests(&all_asts, &base_dir));
        }
        (Subcommand::Build, Backend::Interpreter) => {
            eprintln!("Note: 'build' with interpreter backend just validates the code.");
            // The interpreter doesn't need a separate build step
            // Just validate that the code parses and type-checks
            eprintln!("Parsed {} file(s) successfully.", all_asts.len());
        }
        (Subcommand::Build, Backend::C) => {
            std::process::exit(build_c(&all_asts));
        }
    }
}

/// Run the program using the interpreter
fn run_interpreter(asts: Vec<Ast>, path: &Path) {
    // Set ROOT for the interpreter
    assert_eq!(path, ROOT.get_or_init(|| path.into()));

    let mut reg = Registry::from(asts);
    reg.add_builtins();

    let main = reg.func("main").expect("Did not find main function");
    assert_eq!(main.len(), 1);
    let main = &main[0];
    let mut vars = HashMap::new();

    let Body::Defined(statements) = &main.body else {
        unreachable!()
    };

    println!("--- PROGRAM OUTPUT ---\n");
    install_panic_hook();
    for s in statements {
        run_checked(s, &mut vars, &reg);
    }
}

/// Run tests using the interpreter
fn run_test_suite_interpreter(asts: Vec<Ast>, path: &Path) {
    // Set ROOT for the interpreter
    assert_eq!(path, ROOT.get_or_init(|| path.into()));

    let mut reg = Registry::from(asts);
    reg.add_builtins();

    let tests = &reg.tests;

    if tests.is_empty() {
        println!("No tests found.");
        return;
    }

    // Set up panic hook to suppress backtraces in test mode
    let last_panic = Arc::new(RwLock::new(String::new()));

    let mut passed = 0;
    let mut failed = 0;

    println!("--- TEST OUTPUT ---\n");
    println!("Running {} test(s)...\n", tests.len());

    for test in tests {
        let mut vars = HashMap::new();

        let slot = last_panic.clone();
        let default_hook = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            // Do nothing - we handle the panic message ourselves
            if let Some(info) = info.payload().downcast_ref::<interpreter::UserPanic>() {
                *slot.write().unwrap() = format!("{}", info.0);
                return;
            }

            let msg = info
                .payload()
                .downcast_ref::<&str>()
                .copied()
                .or_else(|| info.payload().downcast_ref::<String>().map(String::as_str))
                .unwrap_or("panic");

            *slot.write().unwrap() = if let Some(loc) = info.location() {
                format!("\x1b[91m{msg} ({}:{})\x1b[0m", loc.file(), loc.line())
            } else {
                format!("\x1b[91m{msg}\x1b[0m")
            };
        }));
        // Run the test and catch panics
        let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
            for statement in &test.body {
                run_checked(statement, &mut vars, &reg);
            }
        }));
        // Restore original panic hook
        panic::set_hook(default_hook);

        match result {
            Ok(_) => {
                print!("\x1b[92mPASS\x1b[0m");
                passed += 1;
                println!(" ... \"{}\"", test.name);
            }
            Err(_) => {
                print!("\x1b[91mFAIL\x1b[0m");
                failed += 1;
                println!(" ... \"{}\"", test.name);
                println!("  -> {}", last_panic.read().unwrap())
            }
        }
    }

    println!(
        "\ntest result: {}. {} passed; {} failed\n",
        if failed == 0 {
            "\x1b[92mok\x1b[0m"
        } else {
            "\x1b[91mFAILED\x1b[0m"
        },
        passed,
        failed
    );

    if failed > 0 {
        std::process::exit(1);
    }
}

/// Build (transpile and compile) using the C backend
fn build_c(asts: &[Ast]) -> i32 {
    let output_dir = Path::new("c-out");

    // Step 1: Transpile to C
    eprintln!("Transpiling to C...");
    match interpreter::c_codegen::transpile_to_c(asts, output_dir) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("\x1b[91mTranspilation failed: {}\x1b[0m", e);
            return 1;
        }
    }

    // Step 2: Compile the generated C code
    eprintln!("Compiling C code...");
    let compile_result = Command::new("gcc")
        .args([
            "-std=c11",
            "-O2",
            "-w",
            "-I",
            "c-out",
            "-o",
            "c-out/blitz",
            "c-out/blitz.c",
        ])
        .output();

    match compile_result {
        Ok(output) => {
            if !output.status.success() {
                eprintln!("\x1b[91mC compilation failed:\x1b[0m");
                let stderr = String::from_utf8_lossy(&output.stderr);
                // Only show error lines (skip warnings and notes) for readability
                for line in stderr.lines() {
                    if line.contains("error:") {
                        eprintln!("{}", line);
                    }
                }
                return 1;
            }
        }
        Err(e) => {
            eprintln!("\x1b[91mFailed to run gcc: {}\x1b[0m", e);
            eprintln!("Make sure gcc is installed and in your PATH.");
            return 1;
        }
    }

    eprintln!("\x1b[92mBuild successful:\x1b[0m c-out/blitz");
    0
}

/// Run the program using the C backend
fn run_c(asts: &[Ast], base_dir: &Path) -> i32 {
    // First build
    let build_result = build_c(asts);
    if build_result != 0 {
        return build_result;
    }

    // Then run
    eprintln!("Running...\n");

    // Get the absolute path to the binary since we'll be changing directories
    let binary = std::env::current_dir()
        .map(|cwd| cwd.join("c-out/blitz"))
        .unwrap_or_else(|_| PathBuf::from("./c-out/blitz"));

    let run_result = Command::new(&binary).current_dir(base_dir).output();

    match run_result {
        Ok(output) => {
            print!("{}", String::from_utf8_lossy(&output.stdout));
            if !output.stderr.is_empty() {
                eprint!("{}", String::from_utf8_lossy(&output.stderr));
            }
            output.status.code().unwrap_or(1)
        }
        Err(e) => {
            eprintln!("\x1b[91mFailed to run binary: {}\x1b[0m", e);
            1
        }
    }
}

/// Run tests using the C backend
fn run_c_tests(asts: &[Ast], base_dir: &Path) -> i32 {
    let output_dir = Path::new("c-out");

    // Step 1: Transpile to C with test support
    eprintln!("Transpiling to C with test support...");
    match interpreter::c_codegen::transpile_to_c_with_tests(asts, output_dir) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("\x1b[91mTranspilation failed: {}\x1b[0m", e);
            return 1;
        }
    }

    // Step 2: Compile the generated C code
    eprintln!("Compiling C code...");
    let compile_result = Command::new("gcc")
        .args([
            "-std=c11",
            "-w",
            "-I",
            "c-out",
            "-o",
            "c-out/blitz_tests",
            "c-out/blitz.c",
        ])
        .output();

    match compile_result {
        Ok(output) => {
            if !output.status.success() {
                eprintln!("\x1b[91mC compilation failed:\x1b[0m");
                let stderr = String::from_utf8_lossy(&output.stderr);
                // Only show error lines (skip warnings and notes) for readability
                for line in stderr.lines() {
                    if line.contains("error:") {
                        eprintln!("{}", line);
                    }
                }
                return 1;
            }
        }
        Err(e) => {
            eprintln!("\x1b[91mFailed to run gcc: {}\x1b[0m", e);
            eprintln!("Make sure gcc is installed and in your PATH.");
            return 1;
        }
    }

    // Step 3: Run the test binary from the source directory
    eprintln!("Running C tests from {:?}...\n", base_dir);

    // Get the absolute path to the test binary since we'll be changing directories
    let test_binary = std::env::current_dir()
        .map(|cwd| cwd.join("c-out/blitz_tests"))
        .unwrap_or_else(|_| PathBuf::from("./c-out/blitz_tests"));

    let run_result = Command::new(&test_binary).current_dir(base_dir).output();

    match run_result {
        Ok(output) => {
            print!("{}", String::from_utf8_lossy(&output.stdout));
            if !output.stderr.is_empty() {
                eprint!("{}", String::from_utf8_lossy(&output.stderr));
            }
            output.status.code().unwrap_or(1)
        }
        Err(e) => {
            eprintln!("\x1b[91mFailed to run test binary: {}\x1b[0m", e);
            1
        }
    }
}

pub fn install_panic_hook() {
    let default = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
        if let Some(info) = info.payload().downcast_ref::<interpreter::UserPanic>() {
            eprintln!("{}", info.0);
            return;
        }

        // everything else: behave exactly like Rust normally does
        default(info);
    }));
}

fn collect_definitions(path: &Path) -> Vec<Ast> {
    let mut all_definitions = Vec::new();

    if path.is_file() {
        if path.extension().and_then(|s| s.to_str()) == Some("blitz") {
            match fs::read_to_string(path) {
                Ok(content) => {
                    let parser = Parser::new(&content);
                    all_definitions.push(Ast {
                        defs: parser.collect(),
                        source: content,
                    });
                }
                Err(e) => eprintln!("Failed to read file {}: {}", path.display(), e),
            }
        } else {
            eprintln!("File is not a .blitz file: {}", path.display());
        }
    } else if path.is_dir() {
        let blitz_files = find_blitz_files(path);

        for file_path in blitz_files {
            match fs::read_to_string(&file_path) {
                Ok(content) => {
                    let parser = Parser::new(&content);
                    all_definitions.push(Ast {
                        defs: parser.collect(),
                        source: content,
                    });
                }
                Err(e) => eprintln!("Failed to read file {}: {}", file_path.display(), e),
            }
        }
    } else {
        eprintln!("Path does not exist: {}", path.display());
    }

    all_definitions
}

fn find_blitz_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("blitz") {
                files.push(path);
            } else if path.is_dir() {
                files.extend(find_blitz_files(&path));
            }
        }
    }

    files
}
