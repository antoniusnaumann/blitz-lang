use parser::{Ast, Definition, Type};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

/// Main entry point for C transpilation
pub fn transpile_to_c(asts: &[Ast], output_dir: &Path) -> Result<(), String> {
    let mut codegen = CCodegen::new(output_dir);

    // First pass: collect all type definitions
    for ast in asts {
        for def in &ast.defs {
            codegen.collect_definition(def)?;
        }
    }

    // Second pass: generate code
    for ast in asts {
        for def in &ast.defs {
            codegen.generate_definition(def)?;
        }
    }

    // Write output files
    codegen.write_files()?;

    Ok(())
}

struct CCodegen {
    output_dir: std::path::PathBuf,
    /// Header content (type declarations and forward declarations)
    header: String,
    /// Implementation content (function definitions)
    impl_code: String,
    /// Track seen types to avoid duplicates
    seen_types: HashSet<String>,
    /// Track generic instantiations (e.g., "Option_Type", "List_Token")
    generic_instances: HashMap<String, Vec<String>>,
}

impl CCodegen {
    fn new(output_dir: &Path) -> Self {
        Self {
            output_dir: output_dir.to_path_buf(),
            header: String::new(),
            impl_code: String::new(),
            seen_types: HashSet::new(),
            generic_instances: HashMap::new(),
        }
    }

    /// First pass: collect type information
    fn collect_definition(&mut self, def: &Definition) -> Result<(), String> {
        match def {
            Definition::Struct(s) => {
                self.seen_types.insert(s.sig.name.clone());
            }
            Definition::Union(u) => {
                self.seen_types.insert(u.sig.name.clone());
            }
            Definition::Pub(p) => {
                self.collect_definition(&p.item)?;
            }
            Definition::Test(_) => {
                // Skip tests entirely
            }
            _ => {}
        }
        Ok(())
    }

    /// Second pass: generate code
    fn generate_definition(&mut self, def: &Definition) -> Result<(), String> {
        match def {
            Definition::Struct(s) => {
                self.generate_struct(&s.sig, &s.fields)?;
            }
            Definition::Union(u) => {
                self.generate_union(&u.sig, &u.cases)?;
            }
            Definition::Fn(f) => {
                self.generate_function(f)?;
            }
            Definition::Pub(p) => {
                self.generate_definition(&p.item)?;
            }
            Definition::Test(_) => {
                // Skip tests entirely
            }
            Definition::Alias(_) | Definition::Actor(_) => {
                // Not implemented yet
            }
        }
        Ok(())
    }

    fn generate_struct(&mut self, sig: &Type, fields: &[parser::Field]) -> Result<(), String> {
        let struct_name = &sig.name;

        // Handle empty structs
        if fields.is_empty() {
            self.header.push_str(&format!(
                "typedef struct {{ char _dummy; }} {};\n\n",
                struct_name
            ));
            return Ok(());
        }

        // Generate struct typedef
        self.header.push_str(&format!("typedef struct {{\n"));

        for field in fields {
            let field_type = self.map_type(&field.r#type);
            self.header
                .push_str(&format!("    {} {};\n", field_type, field.name));
        }

        self.header.push_str(&format!("}} {};\n\n", struct_name));

        Ok(())
    }

    fn generate_union(&mut self, sig: &Type, cases: &[parser::Case]) -> Result<(), String> {
        // TODO: implement
        self.header
            .push_str(&format!("// union {} - NOT IMPLEMENTED\n", sig.name));
        Ok(())
    }

    fn generate_function(&mut self, func: &parser::Fn) -> Result<(), String> {
        // TODO: implement
        self.impl_code
            .push_str(&format!("// fn {} - NOT IMPLEMENTED\n", func.name));
        Ok(())
    }

    fn map_type(&self, ty: &Type) -> String {
        match ty.name.as_str() {
            "Int" => "int64_t".to_string(),
            "Bool" => "bool".to_string(),
            "Float" => "double".to_string(),
            "String" => "char*".to_string(),
            "Rune" => "uint32_t".to_string(),
            "Void" => "void".to_string(),
            _ => {
                // Generic or user-defined type
                if ty.params.is_empty() {
                    ty.name.clone()
                } else {
                    // Monomorphize: Option(Type) -> Option_Type
                    format!(
                        "{}_{}",
                        ty.name,
                        ty.params
                            .iter()
                            .map(|p| self.map_type(p))
                            .collect::<Vec<_>>()
                            .join("_")
                    )
                }
            }
        }
    }

    fn write_files(&self) -> Result<(), String> {
        // Create output directory if it doesn't exist
        fs::create_dir_all(&self.output_dir)
            .map_err(|e| format!("Failed to create output directory: {}", e))?;

        // Write blitz_types.h with built-in types
        let types_h = r#"#ifndef BLITZ_TYPES_H
#define BLITZ_TYPES_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

typedef int64_t Int;
typedef bool Bool;
typedef double Float;
typedef uint32_t Rune;
typedef char* String;

#endif // BLITZ_TYPES_H
"#;

        let types_path = self.output_dir.join("blitz_types.h");
        fs::write(&types_path, types_h)
            .map_err(|e| format!("Failed to write {}: {}", types_path.display(), e))?;

        // Write main header file
        let header_path = self.output_dir.join("blitz.h");
        let mut full_header = String::new();
        full_header.push_str("#ifndef BLITZ_H\n");
        full_header.push_str("#define BLITZ_H\n\n");
        full_header.push_str("#include \"blitz_types.h\"\n\n");
        full_header.push_str(&self.header);
        full_header.push_str("\n#endif // BLITZ_H\n");

        fs::write(&header_path, full_header)
            .map_err(|e| format!("Failed to write {}: {}", header_path.display(), e))?;

        // Write implementation file
        let impl_path = self.output_dir.join("blitz.c");
        let mut full_impl = String::new();
        full_impl.push_str("#include \"blitz.h\"\n\n");
        full_impl.push_str(&self.impl_code);

        fs::write(&impl_path, full_impl)
            .map_err(|e| format!("Failed to write {}: {}", impl_path.display(), e))?;

        println!("Generated C files in {}", self.output_dir.display());
        println!("  - blitz_types.h");
        println!("  - blitz.h");
        println!("  - blitz.c");

        Ok(())
    }
}
