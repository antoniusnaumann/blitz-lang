use std::collections::HashMap;
use std::env::args;
use std::fs;
use std::path::{Path, PathBuf};

use interpreter::{Body, Builtin, ROOT, Registry, run_checked};
use parser::{Ast, Definition, Parser};

fn main() {
    let path = args()
        .nth(1)
        .expect("Please provide a file or directory path");
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
