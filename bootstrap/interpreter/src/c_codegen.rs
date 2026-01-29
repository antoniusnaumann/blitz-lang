use parser::{Ast, Definition, Type};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

/// Main entry point for C transpilation
pub fn transpile_to_c(asts: &[Ast], output_dir: &Path) -> Result<(), String> {
    eprintln!("DEBUG: transpiling {} ASTs", asts.len());
    for (i, ast) in asts.iter().enumerate() {
        eprintln!("  AST {}: {} definitions", i, ast.defs.len());
    }

    let mut codegen = CCodegen::new(output_dir);

    // First pass: collect all type definitions
    for ast in asts {
        for def in &ast.defs {
            codegen.collect_definition(def)?;
        }
    }

    // Second pass: analyze types to find generic instantiations needed
    for ast in asts {
        for def in &ast.defs {
            codegen.analyze_definition(def)?;
        }
    }

    // Third pass: generate code
    for ast in asts {
        for def in &ast.defs {
            // First generate all enums (simple unions)
            if let Definition::Union(u) = def {
                let is_simple_enum = !u
                    .cases
                    .iter()
                    .any(|c| c.label.is_some() && c.r#type.is_some());
                if is_simple_enum {
                    codegen.generate_definition(def)?;
                }
            } else if let Definition::Pub(p) = def {
                if let Definition::Union(u) = &*p.item {
                    let is_simple_enum = !u
                        .cases
                        .iter()
                        .any(|c| c.label.is_some() && c.r#type.is_some());
                    if is_simple_enum {
                        codegen.generate_definition(def)?;
                    }
                }
            }
        }
    }

    // Then generate all other definitions
    for ast in asts {
        for def in &ast.defs {
            // Skip enums we already generated
            let skip = if let Definition::Union(u) = def {
                !u.cases
                    .iter()
                    .any(|c| c.label.is_some() && c.r#type.is_some())
            } else if let Definition::Pub(p) = def {
                if let Definition::Union(u) = &*p.item {
                    !u.cases
                        .iter()
                        .any(|c| c.label.is_some() && c.r#type.is_some())
                } else {
                    false
                }
            } else {
                false
            };

            if !skip {
                codegen.generate_definition(def)?;
            }
        }
    }

    // Write output files
    codegen.write_files()?;

    Ok(())
}

struct CCodegen {
    output_dir: std::path::PathBuf,
    /// Forward declarations
    forward_decls: String,
    /// Enum definitions (simple unions)
    enum_defs: String,
    /// Header content (struct declarations)
    header: String,
    /// Implementation content (function definitions)
    impl_code: String,
    /// Track seen types to avoid duplicates
    seen_types: HashSet<String>,
    /// Track which types are enums (not structs)
    enum_types: HashSet<String>,
    /// Track which types have been generated
    generated_types: HashSet<String>,
    /// Track generic instantiations needed (e.g., "List_Arg" -> ["List", "Arg"])
    generic_instances: HashMap<String, (String, Vec<String>)>,
}

impl CCodegen {
    fn new(output_dir: &Path) -> Self {
        Self {
            output_dir: output_dir.to_path_buf(),
            forward_decls: String::new(),
            enum_defs: String::new(),
            header: String::new(),
            impl_code: String::new(),
            seen_types: HashSet::new(),
            enum_types: HashSet::new(),
            generated_types: HashSet::new(),
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
                // Check if this is a symbolic-only union (simple enum)
                let has_typed_variants = u
                    .cases
                    .iter()
                    .any(|c| c.label.is_some() && c.r#type.is_some());
                if !has_typed_variants {
                    self.enum_types.insert(u.sig.name.clone());
                }
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

    /// Second pass: analyze types to find generic instantiations
    fn analyze_definition(&mut self, def: &Definition) -> Result<(), String> {
        match def {
            Definition::Struct(s) => {
                for field in &s.fields {
                    self.analyze_type(&field.r#type);
                }
            }
            Definition::Union(u) => {
                for case in &u.cases {
                    if let Some(ty) = &case.r#type {
                        self.analyze_type(ty);
                    }
                }
            }
            Definition::Pub(p) => {
                self.analyze_definition(&p.item)?;
            }
            Definition::Test(_) => {
                // Skip tests
            }
            _ => {}
        }
        Ok(())
    }

    /// Analyze a type to track generic instantiations
    fn analyze_type(&mut self, ty: &Type) {
        // Check if this is a generic instantiation
        if !ty.params.is_empty() {
            let instance_name = format!(
                "{}_{}",
                ty.name,
                ty.params
                    .iter()
                    .map(|p| self.type_name_for_instance(p))
                    .collect::<Vec<_>>()
                    .join("_")
            );

            let param_names: Vec<String> = ty
                .params
                .iter()
                .map(|p| self.type_name_for_instance(p))
                .collect();

            self.generic_instances
                .entry(instance_name)
                .or_insert((ty.name.clone(), param_names));

            // Recursively analyze parameters
            for param in &ty.params {
                self.analyze_type(param);
            }
        } else {
            // For parameterless types, recursively analyze if it's a composite type
            // (This handles nested types in the Type struct itself, e.g., Type.params)
        }
    }

    /// Get a simple type name for use in instance names
    fn type_name_for_instance(&self, ty: &Type) -> String {
        if ty.params.is_empty() {
            ty.name.clone()
        } else {
            format!(
                "{}_{}",
                ty.name,
                ty.params
                    .iter()
                    .map(|p| self.type_name_for_instance(p))
                    .collect::<Vec<_>>()
                    .join("_")
            )
        }
    }

    /// Third pass: generate code
    fn generate_definition(&mut self, def: &Definition) -> Result<(), String> {
        match def {
            Definition::Struct(s) => {
                let struct_name = &s.sig.name;
                // Generate forward declaration if not already done
                if !self.generated_types.contains(struct_name) {
                    self.forward_decls.push_str(&format!(
                        "typedef struct {} {};\n",
                        struct_name, struct_name
                    ));
                    self.generated_types.insert(struct_name.clone());
                }
                self.generate_struct(&s.sig, &s.fields)?;
            }
            Definition::Union(u) => {
                let union_name = &u.sig.name;
                // Check if this needs a forward declaration (tagged unions do, simple enums don't)
                let has_typed_variants = u
                    .cases
                    .iter()
                    .any(|c| c.label.is_some() && c.r#type.is_some());

                if has_typed_variants && !self.generated_types.contains(union_name) {
                    self.forward_decls
                        .push_str(&format!("typedef struct {} {};\n", union_name, union_name));
                    self.generated_types.insert(union_name.clone());
                }
                // Note: simple enum unions don't need forward declarations
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

        // Skip generic structs for now
        if !sig.params.is_empty() {
            eprintln!("Skipping generic struct {}", struct_name);
            self.header.push_str(&format!(
                "// TODO: Generic struct {} - NOT IMPLEMENTED YET\n\n",
                struct_name
            ));
            return Ok(());
        }

        eprintln!("Generating struct {}", struct_name);

        // Handle empty structs
        if fields.is_empty() {
            self.header
                .push_str(&format!("struct {} {{ char _dummy; }};\n\n", struct_name));
            return Ok(());
        }

        // Generate struct definition
        self.header
            .push_str(&format!("struct {} {{\n", struct_name));

        for field in fields {
            let field_type = self.map_type(&field.r#type);
            self.header
                .push_str(&format!("    {} {};\n", field_type, field.name));
        }

        self.header.push_str(&format!("}};\n\n"));

        Ok(())
    }

    fn generate_union(&mut self, sig: &Type, cases: &[parser::Case]) -> Result<(), String> {
        let union_name = &sig.name;

        // Skip generic unions for now
        if !sig.params.is_empty() {
            self.header.push_str(&format!(
                "// TODO: Generic union {} - NOT IMPLEMENTED YET\n\n",
                union_name
            ));
            return Ok(());
        }

        // Check if this is a purely symbolic union (no typed variants with labels)
        let has_typed_variants = cases
            .iter()
            .any(|c| c.label.is_some() && c.r#type.is_some());

        if !has_typed_variants {
            // Generate simple enum for symbolic-only unions
            self.enum_defs.push_str(&format!("typedef enum {{\n"));
            for (i, case) in cases.iter().enumerate() {
                // For symbolic variants: label is the name, type might be present but is same as label
                let variant_name = if let Some(label) = &case.label {
                    label.clone()
                } else if let Some(ty) = &case.r#type {
                    // Handle parameterized cases like Lit(Bool)
                    if !ty.params.is_empty() {
                        format!(
                            "{}_{}",
                            ty.name,
                            ty.params
                                .iter()
                                .map(|p| p.name.as_str())
                                .collect::<Vec<_>>()
                                .join("_")
                        )
                    } else {
                        ty.name.clone()
                    }
                } else {
                    return Err(format!(
                        "Union {} has case {} without label or type",
                        union_name, i
                    ));
                };

                self.enum_defs
                    .push_str(&format!("    {}_{}", union_name, variant_name));
                if i < cases.len() - 1 {
                    self.enum_defs.push_str(",");
                }
                self.enum_defs.push_str("\n");
            }
            self.enum_defs.push_str(&format!("}} {};\n\n", union_name));
        } else {
            // Generate tagged union for mixed/typed unions
            // First generate the tag enum
            self.header.push_str(&format!("typedef enum {{\n"));
            for (i, case) in cases.iter().enumerate() {
                let variant_name = if let Some(label) = &case.label {
                    label.clone()
                } else if let Some(ty) = &case.r#type {
                    ty.name.clone()
                } else {
                    return Err(format!(
                        "Union {} has case {} without label or type",
                        union_name, i
                    ));
                };

                self.header
                    .push_str(&format!("    {}_tag_{}", union_name, variant_name));
                if i < cases.len() - 1 {
                    self.header.push_str(",");
                }
                self.header.push_str("\n");
            }
            self.header.push_str(&format!("}} {}_Tag;\n\n", union_name));

            // Then generate the tagged union struct
            self.header.push_str(&format!("struct {} {{\n", union_name));
            self.header
                .push_str(&format!("    {}_Tag tag;\n", union_name));

            // Only add union if there are typed variants
            let typed_cases: Vec<&parser::Case> = cases
                .iter()
                .filter(|c| c.label.is_some() && c.r#type.is_some())
                .collect();
            if !typed_cases.is_empty() {
                self.header.push_str("    union {\n");
                for case in typed_cases {
                    let variant_name = case.label.as_ref().unwrap();
                    let variant_type = case.r#type.as_ref().unwrap();
                    let c_type = self.map_type(variant_type);
                    self.header
                        .push_str(&format!("        {} as_{};\n", c_type, variant_name));
                }
                self.header.push_str("    } data;\n");
            }

            self.header.push_str(&format!("}};\n\n"));
        }

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
            "Box" => {
                // Box(T) becomes T*
                if ty.params.len() == 1 {
                    format!("{}*", self.map_type(&ty.params[0]))
                } else {
                    "void*".to_string() // Fallback
                }
            }
            _ => {
                // Generic or user-defined type
                if ty.params.is_empty() {
                    // Check if this is a known struct type (not enum) - if so, use pointer for forward-declaration safety
                    if self.seen_types.contains(&ty.name) && !self.enum_types.contains(&ty.name) {
                        format!("{}*", ty.name)
                    } else {
                        ty.name.clone()
                    }
                } else {
                    // Monomorphize: Option(Type) -> Option_Type
                    format!(
                        "{}_{}",
                        ty.name,
                        ty.params
                            .iter()
                            .map(|p| self.type_name_for_instance(p))
                            .collect::<Vec<_>>()
                            .join("_")
                    )
                }
            }
        }
    }

    fn write_files(&self) -> Result<(), String> {
        eprintln!(
            "DEBUG: forward_decls length = {} bytes",
            self.forward_decls.len()
        );
        eprintln!("DEBUG: header length = {} bytes", self.header.len());
        eprintln!("DEBUG: impl_code length = {} bytes", self.impl_code.len());
        eprintln!(
            "DEBUG: generic_instances count = {}",
            self.generic_instances.len()
        );

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

// Built-in Range type (TODO: verify structure)
typedef struct {
    int64_t begin;
    int64_t end;
} Range;

#endif // BLITZ_TYPES_H
"#;

        let types_path = self.output_dir.join("blitz_types.h");
        fs::write(&types_path, types_h)
            .map_err(|e| format!("Failed to write {}: {}", types_path.display(), e))?;

        // Write main header file with forward declarations first
        let header_path = self.output_dir.join("blitz.h");
        let mut full_header = String::new();
        full_header.push_str("#ifndef BLITZ_H\n");
        full_header.push_str("#define BLITZ_H\n\n");
        full_header.push_str("#include \"blitz_types.h\"\n\n");

        // Add forward declarations section
        if !self.forward_decls.is_empty() {
            full_header.push_str("// Forward declarations\n");
            full_header.push_str(&self.forward_decls);
            full_header.push_str("\n");
        }

        // Add enum definitions (these must come before generic instantiations)
        if !self.enum_defs.is_empty() {
            full_header.push_str("// Enum definitions\n");
            full_header.push_str(&self.enum_defs);
            full_header.push_str("\n");
        }

        // Add generic type instantiations
        if !self.generic_instances.is_empty() {
            full_header.push_str("// Generic type instantiations\n");
            let mut instances: Vec<_> = self.generic_instances.iter().collect();
            instances.sort_by_key(|(name, _)| *name);

            for (instance_name, (base_type, params)) in instances {
                if base_type == "Box" && params.len() == 1 {
                    // Box(T) -> typedef T* Box_T;
                    let param_type = &params[0];
                    // Box is just a pointer to the type
                    full_header.push_str(&format!("typedef {}* {};\n", param_type, instance_name));
                } else if base_type == "List" && params.len() == 1 {
                    // List(T) -> struct with data, len, cap
                    let param_type = &params[0];
                    // Determine the C type for the list elements
                    let c_type = if param_type == "Arg"
                        || param_type == "Field"
                        || param_type == "Case"
                        || param_type == "CallArg"
                        || param_type == "SwitchCase"
                        || param_type == "Statement"
                        || param_type == "Expression"
                    {
                        // These are struct types, use pointer
                        format!("{}*", param_type)
                    } else if param_type == "Type" {
                        // Type is also a struct
                        "Type*".to_string()
                    } else {
                        // Fallback
                        param_type.clone()
                    };
                    full_header.push_str(&format!(
                        "typedef struct {{\n    {}* data;\n    size_t len;\n    size_t cap;\n}} {};\n\n",
                        c_type, instance_name
                    ));
                } else if base_type == "Option" && params.len() == 1 {
                    // Option(T) -> tagged union
                    let param_type = &params[0];
                    // Determine the C type for the option value
                    let c_type = if param_type == "Type" {
                        "Type*".to_string()
                    } else if param_type == "Ident" {
                        "Ident*".to_string()
                    } else {
                        param_type.clone()
                    };

                    full_header.push_str(&format!(
                        "typedef enum {{\n    {}_tag_none,\n    {}_tag_some\n}} {}_Tag;\n\n",
                        instance_name, instance_name, instance_name
                    ));
                    full_header.push_str(&format!(
                        "typedef struct {{\n    {}_Tag tag;\n    {} value;\n}} {};\n\n",
                        instance_name, c_type, instance_name
                    ));
                } else if base_type == "Lit" && params.len() == 1 {
                    // Lit(T) -> struct with value and span
                    let param_type = &params[0];
                    let c_type = match param_type.as_str() {
                        "Bool" => "bool",
                        "Int" => "int64_t",
                        "Float" => "double",
                        "String" => "char*",
                        "Rune" => "uint32_t",
                        _ => param_type.as_str(),
                    };
                    full_header.push_str(&format!(
                        "typedef struct {{\n    {} value;\n    Span* span;\n}} {};\n\n",
                        c_type, instance_name
                    ));
                } else {
                    // Unknown generic type - keep as void* stub
                    full_header.push_str(&format!(
                        "typedef void* {}; // TODO: implement generic {}\n",
                        instance_name, instance_name
                    ));
                }
            }
            full_header.push_str("\n");
        }

        // Add type definitions
        full_header.push_str(&self.header);
        full_header.push_str("\n#endif // BLITZ_H\n");

        eprintln!(
            "DEBUG: about to write {} bytes to {}",
            full_header.len(),
            header_path.display()
        );

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
