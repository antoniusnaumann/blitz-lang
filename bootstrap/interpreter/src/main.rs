use std::collections::HashMap;
use std::env::args;
use std::fs;
use std::panic;
use std::path::{Path, PathBuf};

use interpreter::{Body, Builtin, Registry, run_checked, ROOT};
use parser::{Ast, Definition, Parser};

fn main() {
    let args: Vec<String> = args().collect();
    
    let (run_tests, path) = if args.len() > 2 && args[1] == "test" {
        (true, args[2].clone())
    } else if args.len() > 1 {
        (false, args[1].clone())
    } else {
        eprintln!("Usage: interpreter [test] <file-or-directory>");
        std::process::exit(1);
    };
    
    let path = Path::new(&path);
    assert_eq!(path, ROOT.get_or_init(|| path.into()));

    let asts = collect_definitions(path);

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
        for s in statements {
            run_checked(s.clone(), &mut vars, &reg);
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
    let default_hook = panic::take_hook();
    panic::set_hook(Box::new(|_| {
        // Do nothing - we handle the panic message ourselves
    }));
    
    let mut passed = 0;
    let mut failed = 0;
    
    println!("Running {} test(s)...\n", tests.len());
    
    for test in tests {
        print!("test {} ... ", test.name);
        
        let mut vars = HashMap::new();
        
        // Run the test and catch panics
        let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
            for statement in &test.body {
                run_checked(statement.clone(), &mut vars, reg);
            }
        }));
        
        match result {
            Ok(_) => {
                println!("\x1b[92mok\x1b[0m");
                passed += 1;
            }
            Err(_) => {
                println!("\x1b[91mFAILED\x1b[0m");
                failed += 1;
            }
        }
    }
    
    // Restore original panic hook
    panic::set_hook(default_hook);
    
    println!("\ntest result: {}. {} passed; {} failed\n", 
        if failed == 0 { "\x1b[92mok\x1b[0m" } else { "\x1b[91mFAILED\x1b[0m" },
        passed, 
        failed
    );
    
    if failed > 0 {
        std::process::exit(1);
    }
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
