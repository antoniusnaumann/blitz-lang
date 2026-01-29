use std::collections::HashMap;
use std::env::args;
use std::fs;
use std::panic;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::RwLock;

use interpreter::DEBUG;
use interpreter::{run_checked, Body, Builtin, Registry, ROOT};
use parser::{Ast, Definition, Parser};

fn main() {
    let mut args: Vec<String> = args().collect();
    if args.len() > 2 && args[1] == "debug" {
        assert!(DEBUG.get_or_init(|| true));
        args.remove(1);
    } else {
        assert!(!DEBUG.get_or_init(|| false));
    }

    // Check for --transpile-c flag
    let transpile_c = args.len() > 2 && args[1] == "--transpile-c";

    if transpile_c {
        args.remove(1); // remove --transpile-c

        // For transpilation, collect all remaining args as input paths
        if args.len() < 2 {
            eprintln!("Usage: interpreter --transpile-c <file-or-directory> [...]");
            std::process::exit(1);
        }

        let input_paths: Vec<PathBuf> = args[1..].iter().map(|s| PathBuf::from(s)).collect();
        let mut all_asts = Vec::new();

        for input_path in input_paths {
            let asts = collect_definitions(&input_path);
            all_asts.extend(asts);
        }

        let output_path = Path::new("c-out");
        match interpreter::c_codegen::transpile_to_c(&all_asts, output_path) {
            Ok(()) => {
                println!("Successfully transpiled to C in c-out/");
                std::process::exit(0);
            }
            Err(e) => {
                eprintln!("Transpilation failed: {}", e);
                std::process::exit(1);
            }
        }
    }

    let (run_tests, path) = if args.len() > 2 && args[1] == "test" {
        (true, args[2].clone())
    } else if args.len() > 1 {
        (false, args[1].clone())
    } else {
        eprintln!("Usage: interpreter [--transpile-c] [test] <file-or-directory>");
        std::process::exit(1);
    };

    let path = Path::new(&path);
    assert_eq!(path, ROOT.get_or_init(|| path.into()));

    let asts = collect_definitions(path);

    // If transpiling to C, do that and exit (handled above)

    for ast in &asts {
        // println!("DEBUG: Collected {} definitions", ast.defs.len());
        for def in &ast.defs {
            match def {
                Definition::Fn(_f) => {
                    // println!(
                    //     "{}({})",
                    //     f.name,
                    //     f.args.print().trim().trim_end_matches("()")
                    // );
                }
                _ => {}
            }
        }
    }

    let mut reg = Registry::from(asts);
    reg.add_builtins();

    if run_tests {
        run_test_suite(&reg);
    } else {
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
}

fn run_test_suite(reg: &Registry) {
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
                run_checked(statement, &mut vars, reg);
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
        eprintln!(
            "Path does not exist or is neither a file nor directory: {}",
            path.display()
        );
    }

    all_definitions
}

fn find_blitz_files(dir: &Path) -> Vec<PathBuf> {
    let mut blitz_files = Vec::new();

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();

            if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("blitz") {
                blitz_files.push(path);
            } else if path.is_dir() {
                // Recursively search subdirectories
                blitz_files.extend(find_blitz_files(&path));
            }
        }
    }

    blitz_files
}
