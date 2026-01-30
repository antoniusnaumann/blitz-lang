use parser::{Ast, Definition, Type};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

use crate::c_codegen_patch::{TypeKind, TypeNameRegistry};

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

    // Third pass: build dependency graph for type definitions
    for ast in asts {
        for def in &ast.defs {
            codegen.build_type_dependencies(def)?;
        }
    }

    // Fourth pass: generate code in dependency order
    // First generate all enums (simple unions) - they have no dependencies
    for ast in asts {
        for def in &ast.defs {
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

    // Then generate structs and tagged unions in topologically sorted order
    let ordered_types = codegen.topological_sort_types()?;
    eprintln!("DEBUG: Type dependency order: {:?}", ordered_types);

    for type_name in ordered_types {
        // Clone the definition to avoid borrow checker issues
        if let Some(def) = codegen.type_definitions.get(&type_name).cloned() {
            codegen.generate_definition(&def)?;
        }
    }

    // Finally generate functions and other non-type definitions
    for ast in asts {
        for def in &ast.defs {
            match def {
                Definition::Fn(_) => {
                    codegen.generate_definition(def)?;
                }
                Definition::Pub(p) => {
                    if matches!(&*p.item, Definition::Fn(_)) {
                        codegen.generate_definition(def)?;
                    }
                }
                _ => {}
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
    /// Current function return type (for context-sensitive code generation)
    current_return_type: Option<Type>,
    /// Track function signatures for overload resolution: name -> list of (param_types, mangled_name)
    function_signatures: HashMap<String, Vec<(Vec<String>, String)>>,
    /// Track function return types: func_name -> return_type
    function_return_types: HashMap<String, Type>,
    /// Type name registry for collision detection and resolution
    type_name_registry: TypeNameRegistry,
    /// Type dependency graph: type_name -> set of types it depends on
    type_dependencies: HashMap<String, HashSet<String>>,
    /// Store type definitions for ordered generation
    type_definitions: HashMap<String, Definition>,
    /// Track variable name mappings to handle C stdlib collisions (original_name -> mangled_name)
    variable_name_mappings: HashMap<String, String>,
    /// Track all function names for forward declarations (c_func_name, signature)
    all_functions: Vec<(String, String)>,
    /// Track enum variants: enum_name -> set of variant names
    enum_variants: HashMap<String, HashSet<String>>,
    /// Track which unwrap functions need to be generated (inner_type_name)
    unwrap_needed: HashSet<String>,
    /// Track variable types in current function scope (variable_name -> c_type)
    variable_types: HashMap<String, String>,
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
            current_return_type: None,
            function_signatures: HashMap::new(),
            function_return_types: HashMap::new(),
            type_name_registry: TypeNameRegistry::new(),
            type_dependencies: HashMap::new(),
            type_definitions: HashMap::new(),
            variable_name_mappings: HashMap::new(),
            all_functions: Vec::new(),
            enum_variants: HashMap::new(),
            unwrap_needed: HashSet::new(),
            variable_types: HashMap::new(),
        }
    }

    /// First pass: collect type information
    fn collect_definition(&mut self, def: &Definition) -> Result<(), String> {
        match def {
            Definition::Struct(s) => {
                let c_name = self
                    .type_name_registry
                    .register_type(&s.sig.name, TypeKind::Struct);
                eprintln!("DEBUG: Registering struct {} -> {}", s.sig.name, c_name);
                self.seen_types.insert(c_name);
            }
            Definition::Union(u) => {
                // Check if this is a symbolic-only union (simple enum)
                let has_typed_variants = u
                    .cases
                    .iter()
                    .any(|c| c.label.is_some() && c.r#type.is_some());

                let kind = if has_typed_variants {
                    TypeKind::TaggedUnion
                } else {
                    TypeKind::Enum
                };

                let c_name = self.type_name_registry.register_type(&u.sig.name, kind);
                eprintln!(
                    "DEBUG: Registering union {} ({:?}) -> {}",
                    u.sig.name, kind, c_name
                );
                self.seen_types.insert(c_name.clone());

                if !has_typed_variants {
                    self.enum_types.insert(c_name);
                }
            }
            Definition::Fn(f) => {
                // Collect function signatures for overload detection
                let param_types: Vec<String> = f
                    .args
                    .iter()
                    .map(|arg| self.type_signature(&arg.r#type))
                    .collect();

                // Store the parameter types - we'll determine mangling later
                self.function_signatures
                    .entry(f.name.clone())
                    .or_insert_with(Vec::new)
                    .push((param_types, f.name.clone())); // Initially store unmangled name

                // Store the return type for type inference
                if let Some(ref ret_ty) = f.r#type {
                    self.function_return_types
                        .insert(f.name.clone(), ret_ty.clone());
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
            Definition::Fn(f) => {
                // Analyze function return type
                if let Some(ref ret_ty) = f.r#type {
                    self.analyze_type(ret_ty);
                }
                // Analyze function parameter types
                for arg in &f.args {
                    self.analyze_type(&arg.r#type);
                }
                // Analyze types in function body (variable declarations)
                for stmt in &f.body {
                    self.analyze_statement(stmt);
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

    /// Build type dependency graph for topological sorting
    fn build_type_dependencies(&mut self, def: &Definition) -> Result<(), String> {
        match def {
            Definition::Struct(s) => {
                let blitz_name = s.sig.name.clone();
                let c_name = self
                    .type_name_registry
                    .register_type(&blitz_name, TypeKind::Struct);

                // Store the definition using C name as key
                self.type_definitions.insert(c_name.clone(), def.clone());

                // Extract dependencies from struct fields
                let mut deps = HashSet::new();
                for field in &s.fields {
                    self.extract_type_dependencies(&field.r#type, &mut deps);
                }

                self.type_dependencies.insert(c_name.clone(), deps);
                eprintln!(
                    "DEBUG: Type '{}' depends on: {:?}",
                    c_name,
                    self.type_dependencies.get(&c_name).unwrap()
                );
            }
            Definition::Union(u) => {
                let blitz_name = u.sig.name.clone();
                let has_typed_variants = u
                    .cases
                    .iter()
                    .any(|c| c.label.is_some() && c.r#type.is_some());
                let kind = if has_typed_variants {
                    TypeKind::TaggedUnion
                } else {
                    TypeKind::Enum
                };
                let c_name = self.type_name_registry.register_type(&blitz_name, kind);

                // Store the definition using C name as key
                self.type_definitions.insert(c_name.clone(), def.clone());

                // Extract dependencies from union cases
                let mut deps = HashSet::new();
                for case in &u.cases {
                    if let Some(ty) = &case.r#type {
                        self.extract_type_dependencies(ty, &mut deps);
                    }
                }

                self.type_dependencies.insert(c_name.clone(), deps);
                eprintln!(
                    "DEBUG: Type '{}' depends on: {:?}",
                    c_name,
                    self.type_dependencies.get(&c_name).unwrap()
                );
            }
            Definition::Pub(p) => {
                self.build_type_dependencies(&p.item)?;
            }
            _ => {
                // Functions and other definitions don't go in the type dependency graph
            }
        }
        Ok(())
    }

    /// Extract all type dependencies from a Type (handles generics like Box, Vec, Option)
    fn extract_type_dependencies(&self, ty: &Type, deps: &mut HashSet<String>) {
        // Skip primitive types
        if self.is_primitive_type(&ty.name) {
            return;
        }

        // Skip generic type parameters (single uppercase letters) ONLY if they're not user-defined types
        // Check if this is a known user-defined type in our registry
        if ty.name.len() == 1 && ty.name.chars().next().unwrap().is_uppercase() {
            if !self.seen_types.contains(&ty.name) {
                return;
            }
        }

        // Handle special generic containers that don't create real dependencies
        match ty.name.as_str() {
            "Box" | "Vec" | "Option" | "Result" => {
                // These are wrappers - extract dependencies from their parameters
                for param in &ty.params {
                    self.extract_type_dependencies(param, deps);
                }
            }
            _ => {
                // Regular user-defined type - add as dependency ONLY if it's in our type registry
                if self.seen_types.contains(&ty.name) {
                    deps.insert(ty.name.clone());
                }

                // Also process any generic parameters
                for param in &ty.params {
                    self.extract_type_dependencies(param, deps);
                }
            }
        }
    }

    /// Topologically sort types based on dependencies
    /// Uses Kahn's algorithm for topological sorting
    /// Returns types in order where dependencies come before dependents
    fn topological_sort_types(&self) -> Result<Vec<String>, String> {
        // Build in-degree map (count of dependencies for each type)
        let mut in_degree: HashMap<String, usize> = HashMap::new();
        let mut adj_list: HashMap<String, Vec<String>> = HashMap::new();

        // Initialize all types with in-degree 0 and empty adjacency list
        for type_name in self.type_dependencies.keys() {
            in_degree.entry(type_name.clone()).or_insert(0);
            adj_list.entry(type_name.clone()).or_insert_with(Vec::new);
        }

        // Build the graph: if A depends on B, then B -> A (B must come before A)
        for (dependent, dependencies) in &self.type_dependencies {
            for dependency in dependencies {
                // Only count dependencies on types we're actually generating
                if self.type_dependencies.contains_key(dependency) {
                    // Prevent self-dependencies (which indicate circular references)
                    if dependency != dependent {
                        adj_list
                            .entry(dependency.clone())
                            .or_insert_with(Vec::new)
                            .push(dependent.clone());
                        *in_degree.entry(dependent.clone()).or_insert(0) += 1;
                    } else {
                        eprintln!(
                            "WARNING: Type '{}' has self-dependency (circular reference)",
                            dependent
                        );
                    }
                }
            }
        }

        // Find all types with no dependencies (in-degree = 0)
        let mut queue: Vec<String> = in_degree
            .iter()
            .filter(|(_, degree)| **degree == 0)
            .map(|(name, _)| name.clone())
            .collect();

        let mut result = Vec::new();

        // Process types in order
        while let Some(current) = queue.pop() {
            result.push(current.clone());

            // For each type that depends on current, decrease its in-degree
            if let Some(dependents) = adj_list.get(&current) {
                for dependent in dependents {
                    if let Some(degree) = in_degree.get_mut(dependent) {
                        *degree -= 1;
                        if *degree == 0 {
                            queue.push(dependent.clone());
                        }
                    }
                }
            }
        }

        // Check if all types were processed (detect cycles)
        if result.len() != self.type_dependencies.len() {
            let unprocessed: Vec<String> = self
                .type_dependencies
                .keys()
                .filter(|name| !result.contains(name))
                .cloned()
                .collect();

            eprintln!(
                "WARNING: Circular dependencies detected among types: {:?}",
                unprocessed
            );
            eprintln!(
                "These types will be ordered arbitrarily (forward declarations handle circularity)"
            );

            // Add remaining types in arbitrary order - forward declarations will handle circularity
            for name in unprocessed {
                result.push(name);
            }
        }

        Ok(result)
    }

    /// Analyze a statement to find generic instantiations
    fn analyze_statement(&mut self, stmt: &parser::Statement) {
        match stmt {
            parser::Statement::Declaration(decl) => {
                self.analyze_type(&decl.r#type);
            }
            parser::Statement::Expression(_expr) => {
                // For now, we don't analyze expressions
                // A full implementation would analyze nested types
            }
        }
    }

    /// Check if a type is a concrete type (not a generic template variable)
    /// Generic template variables are single uppercase letters like T, E, A, etc.
    fn is_concrete_type(&self, ty: &Type) -> bool {
        // If the type name is a single uppercase letter, it's a template variable
        if ty.name.len() == 1 && ty.name.chars().next().unwrap().is_uppercase() {
            return false;
        }

        // For types with parameters, all parameters must also be concrete
        if !ty.params.is_empty() {
            return ty.params.iter().all(|p| self.is_concrete_type(p));
        }

        true
    }

    /// Analyze a type to track generic instantiations
    fn analyze_type(&mut self, ty: &Type) {
        // Check if this is a generic instantiation
        if !ty.params.is_empty() {
            // Only add this instantiation if all parameters are concrete types
            // This filters out template-style types like Option(T) or Result(T, E)
            let all_params_concrete = ty.params.iter().all(|p| self.is_concrete_type(p));

            if all_params_concrete {
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
            }

            // Recursively analyze parameters (even if this instance wasn't concrete)
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

    /// Generate a type signature string for overload resolution
    /// This creates a canonical representation of the type for matching
    fn type_signature(&self, ty: &Type) -> String {
        if ty.params.is_empty() {
            ty.name.clone()
        } else {
            format!(
                "{}_{}",
                ty.name,
                ty.params
                    .iter()
                    .map(|p| self.type_signature(p))
                    .collect::<Vec<_>>()
                    .join("_")
            )
        }
    }

    /// Mangle function name based on parameter types
    /// Returns the original name if no mangling needed, or mangled name if there are overloads
    fn mangle_function_name(&self, func_name: &str, param_types: &[String]) -> String {
        // Special functions that should never be mangled
        if func_name == "main" {
            return func_name.to_string();
        }

        // If there are no parameters, use the original name
        if param_types.is_empty() {
            return func_name.to_string();
        }

        // Create mangled name: funcname_Type1_Type2_...
        // Replace pointer indicators and generic brackets for C identifiers
        let mangled_params = param_types
            .iter()
            .map(|t| {
                t.replace("*", "Ptr")
                    .replace("(", "_")
                    .replace(")", "")
                    .replace(",", "_")
                    .replace(" ", "")
                    .replace("<", "_")
                    .replace(">", "")
            })
            .collect::<Vec<_>>()
            .join("_");

        format!("{}_{}", func_name, mangled_params)
    }

    /// Check if a type is a primitive type (not a struct)
    fn is_primitive_type(&self, type_name: &str) -> bool {
        matches!(
            type_name,
            "Int"
                | "Bool"
                | "Float"
                | "Rune"
                | "String"
                | "int64_t"
                | "bool"
                | "double"
                | "uint32_t"
                | "char*"
        )
    }

    /// Check if a type is a built-in type defined in blitz_types.h
    fn is_builtin_type(&self, type_name: &str) -> bool {
        matches!(type_name, "Range" | "List_Rune" | "Option_String")
    }

    /// Check if a function name collides with C standard library function names
    /// Returns true if the name is a C stdlib function that should be mangled
    /// Note: This is used for FUNCTIONS only. Variables can have the same names
    /// as C stdlib functions without issues (e.g., `int time = 0;` is valid C).
    fn is_reserved_c_name(name: &str) -> bool {
        // Common C standard library functions that shouldn't be shadowed
        const C_STDLIB_NAMES: &[&str] = &[
            // Time functions
            "time",
            "clock",
            "difftime",
            "mktime",
            "strftime",
            "gmtime",
            "localtime",
            // I/O functions
            "read",
            "write",
            "open",
            "close",
            "printf",
            "scanf",
            "fprintf",
            "fscanf",
            "fopen",
            "fclose",
            "fread",
            "fwrite",
            "puts",
            "gets",
            "getchar",
            "putchar",
            // Memory functions
            "malloc",
            "calloc",
            "realloc",
            "free",
            "memcpy",
            "memset",
            "memmove",
            // String functions
            "strlen",
            "strcmp",
            "strcpy",
            "strcat",
            "strchr",
            "strstr",
            "strtok",
            // Math functions
            "abs",
            "sqrt",
            "pow",
            "exp",
            "log",
            "sin",
            "cos",
            "tan",
            "floor",
            "ceil",
            // Other common functions
            "exit",
            "abort",
            "assert",
            "signal",
            "raise",
            "setjmp",
            "longjmp",
            // C keywords (though the Blitz parser should catch most of these)
            "auto",
            "break",
            "case",
            "char",
            "const",
            "continue",
            "default",
            "do",
            "double",
            "else",
            "enum",
            "extern",
            "float",
            "for",
            "goto",
            "if",
            "inline",
            "int",
            "long",
            "register",
            "restrict",
            "return",
            "short",
            "signed",
            "sizeof",
            "static",
            "struct",
            "switch",
            "typedef",
            "union",
            "unsigned",
            "void",
            "volatile",
            "while",
            "_Bool",
            "_Complex",
            "_Imaginary",
        ];

        C_STDLIB_NAMES.contains(&name)
    }

    /// Mangle a variable name if it collides with C keywords or function names
    /// Returns the mangled name (e.g., "int" -> "blitz_int", "return" -> "blitz_return", "time" -> "time_var")
    ///
    /// This handles three types of collisions:
    /// 1. C keywords: cannot be used as variable names at all
    /// 2. C stdlib functions: technically OK, but we mangle Blitz functions that call them (time -> blitz_time)
    ///    so we need to check if the variable shadows a mangled Blitz function
    /// 3. User-defined Blitz functions: variables cannot shadow these in their declaration scope
    fn mangle_variable_name(&mut self, name: &str) -> String {
        // Check if we've already mangled this name
        if let Some(mangled) = self.variable_name_mappings.get(name) {
            return mangled.clone();
        }

        // Check for C keywords first
        const C_KEYWORDS: &[&str] = &[
            "auto",
            "break",
            "case",
            "char",
            "const",
            "continue",
            "default",
            "do",
            "double",
            "else",
            "enum",
            "extern",
            "float",
            "for",
            "goto",
            "if",
            "inline",
            "int",
            "long",
            "register",
            "restrict",
            "return",
            "short",
            "signed",
            "sizeof",
            "static",
            "struct",
            "switch",
            "typedef",
            "union",
            "unsigned",
            "void",
            "volatile",
            "while",
            "_Bool",
            "_Complex",
            "_Imaginary",
        ];

        if C_KEYWORDS.contains(&name) {
            let mangled = format!("blitz_{}", name);
            eprintln!(
                "DEBUG: Mangling variable '{}' -> '{}' (C keyword collision)",
                name, mangled
            );
            self.variable_name_mappings
                .insert(name.to_string(), mangled.clone());
            return mangled;
        }

        // Check if this variable name matches a C stdlib function name
        // Since we mangle all Blitz functions with C stdlib names to `blitz_<name>`,
        // we must also mangle variables with those names to avoid shadowing
        // For example: `let time = time()` becomes `int64_t time = blitz_time()`
        // which is invalid C (variable shadows function). We mangle to: `int64_t time_var = blitz_time()`
        if Self::is_reserved_c_name(name) {
            let mangled = format!("{}_var", name);
            eprintln!(
                "DEBUG: Mangling variable '{}' -> '{}' (would shadow 'blitz_{}' function)",
                name, mangled, name
            );
            self.variable_name_mappings
                .insert(name.to_string(), mangled.clone());
            return mangled;
        }

        // Check if this variable name matches any user-defined Blitz function
        // This prevents: int64_t func_name = func_name()
        if self.function_signatures.contains_key(name) {
            let mangled = format!("{}_var", name);
            eprintln!(
                "DEBUG: Mangling variable '{}' -> '{}' (shadows Blitz function)",
                name, mangled
            );
            self.variable_name_mappings
                .insert(name.to_string(), mangled.clone());
            return mangled;
        }

        // No collision, use original name
        self.variable_name_mappings
            .insert(name.to_string(), name.to_string());
        name.to_string()
    }

    /// Get the C name for a variable, returning the mangled version if it was previously mangled
    fn get_variable_name(&self, name: &str) -> String {
        self.variable_name_mappings
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.to_string())
    }

    /// Guess the type of an argument expression (simple heuristic-based approach)
    /// This is not a full type inference system, just enough to disambiguate common overloads
    /// Returns type names without pointer notation (e.g., "Range" not "Range*")
    fn guess_arg_type(&self, expr: &parser::Expression) -> String {
        match expr {
            // Member access like first.range or second.range suggests the member's type
            parser::Expression::Member(member) => {
                // If accessing .range member, the type is likely Range
                if member.member == "range" {
                    return "Range".to_string();
                }
                // If accessing .span member, the type is likely Span
                if member.member == "span" {
                    return "Span".to_string();
                }
                // Default: try to guess from parent
                self.guess_arg_type(&member.parent)
            }
            // Identifier - try to look up in variable mappings
            parser::Expression::Ident(ident) => {
                // For common identifiers, make educated guesses
                if ident.name.contains("parser") {
                    return "Parser".to_string();
                }
                if ident.name.contains("lexer") {
                    return "Lexer".to_string();
                }
                "void*".to_string()
            }
            // Constructor gives us the type directly
            parser::Expression::Constructor(ctor) => ctor.r#type.name.clone(),
            // For function calls, we can't easily determine the return type without full type tracking
            parser::Expression::Call(_call) => "void*".to_string(),
            // Literals
            parser::Expression::Number(_) => "Int".to_string(),
            parser::Expression::String(_) => "String".to_string(),
            parser::Expression::BoolLit(_) => "Bool".to_string(),
            _ => "void*".to_string(),
        }
    }

    /// Get the mangled name for a function call based on argument types
    fn resolve_function_call(&self, func_name: &str, arg_types: &[String]) -> String {
        // Special case: main function is never mangled
        if func_name == "main" {
            return func_name.to_string();
        }

        // Check if function name collides with C stdlib first
        if Self::is_reserved_c_name(func_name) {
            eprintln!(
                "DEBUG: Mangling function call '{}' -> 'blitz_{}' (C stdlib collision)",
                func_name, func_name
            );
            return format!("blitz_{}", func_name);
        }

        // Check if this function has overloads
        if let Some(signatures) = self.function_signatures.get(func_name) {
            // If there's only one signature, no mangling needed
            if signatures.len() == 1 {
                return func_name.to_string();
            }

            // Multiple signatures - try to match by argument types
            for (param_types, _) in signatures {
                if param_types == arg_types {
                    // Found exact match - return mangled name
                    return self.mangle_function_name(func_name, param_types);
                }
            }

            // No exact match - try to find a signature with matching argument count
            for (param_types, _) in signatures {
                if param_types.len() == arg_types.len() {
                    eprintln!(
                        "DEBUG: Matched function '{}' by arg count {} (param types: {:?}, arg types: {:?})",
                        func_name, arg_types.len(), param_types, arg_types
                    );
                    return self.mangle_function_name(func_name, param_types);
                }
            }

            // If we have signatures but no match, use the first one as a fallback
            eprintln!(
                "DEBUG: No match for function '{}', using first signature",
                func_name
            );
            let (param_types, _) = &signatures[0];
            return self.mangle_function_name(func_name, param_types);
        }

        // No overloads found, use original name
        func_name.to_string()
    }

    /// Third pass: generate code
    fn generate_definition(&mut self, def: &Definition) -> Result<(), String> {
        match def {
            Definition::Struct(s) => {
                let struct_name = &s.sig.name;
                // Get the C name with potential collision suffix
                let c_name = self
                    .type_name_registry
                    .register_type(struct_name, TypeKind::Struct);

                // Skip if already generated
                if self.generated_types.contains(&c_name) {
                    return Ok(());
                }

                // Mark as generated to prevent infinite loops
                self.generated_types.insert(c_name.clone());

                // Generate forward declaration (unless it's a built-in type)
                if !self.is_builtin_type(&c_name) {
                    self.forward_decls
                        .push_str(&format!("typedef struct {} {};\n", c_name, c_name));
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

                let kind = if has_typed_variants {
                    TypeKind::TaggedUnion
                } else {
                    TypeKind::Enum
                };

                // Get the C name with potential collision suffix
                let c_name = self.type_name_registry.register_type(union_name, kind);

                // Skip if already generated
                if self.generated_types.contains(&c_name) {
                    return Ok(());
                }

                // Mark as generated to prevent infinite loops
                self.generated_types.insert(c_name.clone());

                if has_typed_variants && !self.is_builtin_type(&c_name) {
                    self.forward_decls
                        .push_str(&format!("typedef struct {} {};\n", c_name, c_name));
                }

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

        // Get the C name for this struct
        let c_name = self
            .type_name_registry
            .register_type(struct_name, TypeKind::Struct);

        // Skip if it's a built-in type
        if self.is_builtin_type(&c_name) {
            return Ok(());
        }

        // Skip generic structs for now
        if !sig.params.is_empty() {
            eprintln!("Skipping generic struct {}", struct_name);
            self.header.push_str(&format!(
                "// TODO: Generic struct {} - NOT IMPLEMENTED YET\n\n",
                c_name
            ));
            return Ok(());
        }

        eprintln!("Generating struct {} as {}", struct_name, c_name);

        // Handle empty structs
        if fields.is_empty() {
            self.header
                .push_str(&format!("struct {} {{ char _dummy; }};\n\n", c_name));
            return Ok(());
        }

        // Generate struct definition
        self.header.push_str(&format!("struct {} {{\n", c_name));

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

        // Determine type kind and get C name
        let kind = if has_typed_variants {
            TypeKind::TaggedUnion
        } else {
            TypeKind::Enum
        };
        let c_name = self.type_name_registry.register_type(union_name, kind);

        if !has_typed_variants {
            // Generate simple enum for symbolic-only unions
            eprintln!("Generating enum {} as {}", union_name, c_name);

            // Track enum variants for identifier resolution
            let mut variants = HashSet::new();

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

                // Track this variant
                variants.insert(variant_name.clone());

                self.enum_defs
                    .push_str(&format!("    {}_{}", c_name, variant_name));
                if i < cases.len() - 1 {
                    self.enum_defs.push_str(",");
                }
                self.enum_defs.push_str("\n");
            }
            self.enum_defs.push_str(&format!("}} {};\n\n", c_name));

            // Store enum variants for later lookup
            eprintln!(
                "DEBUG: Storing enum '{}' with {} variants",
                &c_name,
                variants.len()
            );
            self.enum_variants.insert(c_name.clone(), variants);
        } else {
            // Generate tagged union for mixed/typed unions
            eprintln!("Generating tagged union {} as {}", union_name, c_name);
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
                    .push_str(&format!("    {}_tag_{}", c_name, variant_name));
                if i < cases.len() - 1 {
                    self.header.push_str(",");
                }
                self.header.push_str("\n");
            }
            self.header.push_str(&format!("}} {}_Tag;\n\n", c_name));

            // Then generate the tagged union struct
            self.header.push_str(&format!("struct {} {{\n", c_name));
            self.header.push_str(&format!("    {}_Tag tag;\n", c_name));

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
        // Clear variable name mappings and types for new function scope
        self.variable_name_mappings.clear();
        self.variable_types.clear();

        // Track function for forward declaration - collect parameter types
        let param_types: Vec<String> = func
            .args
            .iter()
            .map(|arg| self.type_signature(&arg.r#type))
            .collect();

        // Store the return type for context-sensitive code generation
        self.current_return_type = func.r#type.clone();

        // Map return type
        let mut return_type = if let Some(ref ret_ty) = func.r#type {
            self.map_type(ret_ty)
        } else {
            "void".to_string()
        };

        // Special case: main function must return int in C
        let is_main = func.name == "main";
        if is_main {
            // main() in C must always return int
            return_type = "int".to_string();
        }

        // Generate parameter list and collect parameter types for mangling
        let mut params = Vec::new();
        let mut param_types = Vec::new();
        for arg in &func.args {
            let param_type = self.map_type(&arg.r#type);
            let param_type_sig = self.type_signature(&arg.r#type);
            param_types.push(param_type_sig);
            // Mangle parameter name if it collides with C stdlib
            let param_name = self.mangle_variable_name(&arg.name);
            // Track parameter type for type inference
            self.variable_types
                .insert(param_name.clone(), param_type.clone());
            // Note: mutability is handled in Blitz semantics, not at C level
            // In C, all parameters are passed by value (or pointer for structs)
            params.push(format!("{} {}", param_type, param_name));
        }
        let params_str = if params.is_empty() {
            "void".to_string()
        } else {
            params.join(", ")
        };

        // Get the mangled function name
        let c_func_name = if is_main {
            func.name.clone()
        } else {
            // Check if function name collides with C stdlib first
            // This takes precedence over overload mangling
            if Self::is_reserved_c_name(&func.name) {
                // Mangle to avoid collision: time -> blitz_time, read -> blitz_read
                eprintln!(
                    "DEBUG: Mangling function definition '{}' -> 'blitz_{}' (C stdlib collision)",
                    func.name, func.name
                );
                format!("blitz_{}", func.name)
            } else if let Some(signatures) = self.function_signatures.get(&func.name) {
                if signatures.len() > 1 {
                    // Multiple overloads, use mangled name
                    self.mangle_function_name(&func.name, &param_types)
                } else {
                    // Only one signature, use original name
                    func.name.clone()
                }
            } else {
                // Not found in signatures (shouldn't happen), use original name
                func.name.clone()
            }
        };

        // Check if this function has generic type parameters
        // If so, skip generating it entirely (generic functions need monomorphization)
        let has_generic_params = func
            .args
            .iter()
            .any(|arg| !self.is_concrete_type(&arg.r#type))
            || func
                .r#type
                .as_ref()
                .map_or(false, |ty| !self.is_concrete_type(ty));

        if has_generic_params {
            // Skip generic functions - they need to be monomorphized first
            eprintln!(
                "Skipping generic function {} (has type parameters)",
                func.name
            );
            return Ok(());
        }

        // Generate function signature
        let func_signature = format!("{} {}({});", return_type, c_func_name, params_str);
        self.all_functions
            .push((c_func_name.clone(), func_signature));

        self.impl_code.push_str(&format!(
            "{} {}({}) {{
",
            return_type, c_func_name, params_str
        ));

        // Generate function body from statements
        let body_len = func.body.len();
        for (i, stmt) in func.body.iter().enumerate() {
            let is_last = i == body_len - 1;
            let stmt_code = if is_last && !is_main && func.r#type.is_some() {
                // For the last statement in a non-void, non-main function,
                // add implicit return if it's an expression
                match stmt {
                    parser::Statement::Expression(expr)
                        if !matches!(expr, parser::Expression::Return(_)) =>
                    {
                        // Check if it's a switch expression - need special handling
                        if let parser::Expression::Switch(switch_expr) = expr {
                            // Get the return type
                            let ret_type = if let Some(ref ty) = func.r#type {
                                self.map_type(ty)
                            } else {
                                "void".to_string()
                            };

                            // Use a temp variable and then return it
                            let temp_var = "_switch_result";
                            let mut switch_code = self.generate_switch_as_statement(
                                switch_expr,
                                is_main,
                                &ret_type,
                                temp_var,
                            );
                            switch_code.push_str(&format!("\n    return {};", temp_var));
                            switch_code
                        } else {
                            let expr_code = self.generate_expression(expr, is_main);
                            format!("return {};", expr_code)
                        }
                    }
                    _ => self.generate_statement(stmt, is_main),
                }
            } else {
                self.generate_statement(stmt, is_main)
            };
            self.impl_code.push_str("    ");
            self.impl_code.push_str(&stmt_code);
            self.impl_code.push_str("\n");
        }

        self.impl_code.push_str("}\n\n");

        Ok(())
    }

    fn generate_statement(&mut self, stmt: &parser::Statement, is_main: bool) -> String {
        match stmt {
            parser::Statement::Expression(expr) => {
                let expr_code = self.generate_expression(expr, is_main);
                // Control flow statements and return don't need semicolons
                if matches!(
                    expr,
                    parser::Expression::Return(_)
                        | parser::Expression::If(_)
                        | parser::Expression::While(_)
                        | parser::Expression::For(_)
                        | parser::Expression::Switch(_)
                ) {
                    expr_code
                } else {
                    format!("{};", expr_code)
                }
            }
            parser::Statement::Declaration(decl) => {
                // Generate variable declaration: <type> <name> = <init_expr>;
                let mut c_type = self.map_type(&decl.r#type);
                // Mangle variable name if it collides with C stdlib
                let var_name = self.mangle_variable_name(&decl.name);

                // If type is empty, "_", or a generic type parameter (single uppercase letter),
                // try to infer from initialization expression
                let is_generic_param =
                    c_type.len() == 1 && c_type.chars().next().map_or(false, |c| c.is_uppercase());
                if c_type.is_empty() || c_type == "_" || is_generic_param {
                    if let Some(init_expr) = &decl.init {
                        c_type = self.infer_expr_type(init_expr);
                        eprintln!(
                            "DEBUG: Inferred type '{}' for variable '{}'",
                            c_type, decl.name
                        );
                    } else {
                        // No type and no init - error, but use int64_t as fallback
                        c_type = "int64_t".to_string();
                    }
                }

                // Generate initialization expression if present
                if let Some(init_expr) = &decl.init {
                    // Special handling for switch expressions in assignment context
                    if let parser::Expression::Switch(switch_expr) = init_expr {
                        // Transform switch expression to statement form with temp variable
                        return self.generate_switch_as_statement(
                            switch_expr,
                            is_main,
                            &c_type,
                            &var_name,
                        );
                    }

                    let init_code = self.generate_expression(init_expr, is_main);
                    // Track variable type for type inference
                    self.variable_types.insert(var_name.clone(), c_type.clone());
                    format!("{} {} = {};", c_type, var_name, init_code)
                } else {
                    // Declaration without initialization
                    // Track variable type for type inference
                    self.variable_types.insert(var_name.clone(), c_type.clone());
                    format!("{} {};", c_type, var_name)
                }
                // Note: is_mut is semantic only in Blitz, not represented in C
            }
        }
    }

    /// Check if an identifier might be an enum variant and qualify it
    fn qualify_identifier(&self, ident_name: &str) -> String {
        // First, check if this identifier is a known variable (parameter or local)
        // If it is, it should NOT be qualified as an enum variant
        if self.variable_name_mappings.contains_key(ident_name) {
            // This is a variable in scope, not an enum variant
            return ident_name.to_string();
        }

        // Check all enum types to see if this identifier is a variant
        for (enum_name, variants) in &self.enum_variants {
            if variants.contains(ident_name) {
                return format!("{}_{}", enum_name, ident_name);
            }
        }
        // Not an enum variant, return as-is
        ident_name.to_string()
    }

    /// Infer the C type of an expression
    fn infer_expr_type(&self, expr: &parser::Expression) -> String {
        match expr {
            parser::Expression::Number(n) => {
                if n.fract() == 0.0 {
                    "int64_t".to_string()
                } else {
                    "double".to_string()
                }
            }
            parser::Expression::BoolLit(_) => "bool".to_string(),
            parser::Expression::String(_) => "char*".to_string(),
            parser::Expression::Rune(_) => "uint32_t".to_string(),
            parser::Expression::Constructor(ctor) => {
                // Constructor returns pointer to the constructed type
                format!("{}*", ctor.r#type.name)
            }
            parser::Expression::Call(call) => {
                // Special handling for unwrap - needs to infer from argument type
                if call.name == "unwrap" && !call.args.is_empty() {
                    let arg_type = self.infer_expr_type(&call.args[0].init);
                    // If argument is Option_T, return T
                    if let Some(inner) = arg_type.strip_prefix("Option_") {
                        // Check if the inner type is a generic parameter (single uppercase letter)
                        // If so, we can't infer the concrete type, fallback to void*
                        let is_generic = inner.len() == 1
                            && inner.chars().next().map_or(false, |c| c.is_uppercase());
                        if is_generic {
                            return "void*".to_string();
                        }
                        // Map the inner type name to its C equivalent
                        return self.map_type_name(inner);
                    }
                    return "void*".to_string(); // Generic fallback
                }

                // Try to look up the function's return type from our registry
                if let Some(ret_type) = self.function_return_types.get(&call.name) {
                    // We have the actual return type - convert it to a C type
                    return self.map_type(ret_type);
                }

                // Fallback to hardcoded heuristics for built-in functions
                match call.name.as_str() {
                    // String functions
                    "read" | "blitz_read" => return "Option_String".to_string(),
                    "chars" => return "List_Rune".to_string(),
                    "substring" => return "char*".to_string(),

                    // List/collection functions
                    "len" => return "int64_t".to_string(),

                    // Parser functions (with mangled names for overloads)
                    "new_parser" => return "Parser*".to_string(),
                    "parse_Parser" => return "List_Definition".to_string(),
                    "parse_String" => return "List_Definition".to_string(),
                    "peek_Parser" => return "Token*".to_string(),
                    "peek_Parser_Int" => return "Token*".to_string(),
                    "tok_Parser" => return "Token*".to_string(),
                    "last_Parser" => return "Token*".to_string(),
                    "next_Parser" => return "Option_Definition".to_string(),
                    "eof_Parser" => return "bool".to_string(),
                    "has_Parser_TokenKind" => return "bool".to_string(),
                    "expect_Parser_TokenKind" => return "Option_Token".to_string(),
                    "accept_Parser_TokenKind" => return "Option_Token".to_string(),
                    "accept_Parser_List_TokenKind" => return "Option_Token".to_string(),
                    "span_Parser" => return "Span*".to_string(),
                    "span_Parser_Token" => return "Span*".to_string(),
                    "span_Parser_Token_Token" => return "Span*".to_string(),
                    "span_Parser_Span" => return "Span*".to_string(),
                    "span_Parser_Span_Span" => return "Span*".to_string(),

                    // Parser expression/statement functions
                    "parse_expression" => return "Option_Expression".to_string(),
                    "parse_expression_bp" => return "Option_Expression".to_string(),
                    "parse_statement" => return "Option_Statement".to_string(),
                    "parse_declaration" => return "Option_Declaration".to_string(),

                    // Parser definition functions
                    "parse_def" => return "Option_Definition".to_string(),
                    "parse_fn" => return "Option_Fn".to_string(),
                    "parse_struct" => return "Option_Struct".to_string(),
                    "parse_union" => return "Option_Union".to_string(),
                    "parse_alias" => return "Option_Alias".to_string(),
                    "parse_actor" => return "Option_Actor".to_string(),
                    "parse_test" => return "Option_Test".to_string(),

                    // Parser component functions
                    "parse_type" => return "Option_Type".to_string(),
                    "parse_type_ident" => return "Option_Type".to_string(),
                    "parse_type_params" => return "List_Type".to_string(),
                    "parse_ident" => return "Option_Ident".to_string(),
                    "parse_arg" => return "Option_Arg".to_string(),
                    "parse_args" => return "Option_List_CallArg".to_string(),
                    "parse_fields" => return "List_Field".to_string(),
                    "parse_cases" => return "List_Case".to_string(),
                    "parse_body" => return "Option_Block".to_string(),

                    // Parser control flow functions
                    "parse_if" => return "Option_If".to_string(),
                    "parse_while" => return "Option_While".to_string(),
                    "parse_for" => return "Option_For".to_string(),
                    "parse_switch" => return "Option_Switch".to_string(),
                    "parse_switch_label" => return "Option_SwitchLabel".to_string(),

                    // Parser literal functions
                    "parse_int_lit" => return "Option_Lit_Int".to_string(),
                    "parse_float_lit" => return "Option_Lit_Float".to_string(),
                    "parse_string_lit" => return "Option_Lit_String".to_string(),
                    "parse_char_lit" => return "Option_Lit_Rune".to_string(),

                    // Parser call/constructor functions
                    "parse_call" => return "Option_Call".to_string(),
                    "parse_member_call" => return "Option_Call".to_string(),
                    "parse_constructor" => return "Option_Constructor".to_string(),
                    "parse_group" => return "Option_Expression".to_string(),
                    "parse_list" => return "Option_List_".to_string(),
                    "parse_precedence" => return "Option_Expression".to_string(),

                    // Lexer functions
                    "new_lexer" => return "Lexer*".to_string(),
                    "new_lexer_List_Rune" => return "Lexer*".to_string(),
                    "new_lexer_String" => return "Lexer*".to_string(),
                    "lex_Lexer" => return "List_Token".to_string(),
                    "lex_String" => return "List_Token".to_string(),
                    "next_Lexer" => return "Option_Token".to_string(),

                    // Token/utility functions
                    "kinds" => return "List_TokenKind".to_string(),

                    // Numeric parsing
                    "parse_int" => return "int64_t".to_string(),

                    _ => {}
                }
                // Default for unknown function calls
                "int64_t".to_string()
            }
            parser::Expression::BinaryOp(binop) => {
                // Comparison operators return bool
                match binop.op {
                    parser::Operator::Eq
                    | parser::Operator::Ne
                    | parser::Operator::Lt
                    | parser::Operator::Le
                    | parser::Operator::Gt
                    | parser::Operator::Ge => "bool".to_string(),
                    parser::Operator::Else => {
                        // Else operator unwraps Option types
                        // If left is Option_T, the result is T* (value field is always a pointer for non-primitives)
                        let left_type = self.infer_expr_type(&binop.left);
                        if let Some(inner) = left_type.strip_prefix("Option_") {
                            // The .value field of Option always holds pointers for struct/enum types
                            format!("{}*", inner)
                        } else {
                            left_type
                        }
                    }
                    _ => self.infer_expr_type(&binop.left), // Arithmetic ops return operand type
                }
            }
            parser::Expression::UnaryOp(unop) => match unop.op {
                parser::Operator::Not => "bool".to_string(),
                _ => self.infer_expr_type(&unop.expr),
            },
            parser::Expression::Group(group) => self.infer_expr_type(&group.expr),
            parser::Expression::Member(member) => {
                // Try to infer the type of a member access
                // For method calls without parens (like parser.parse_def), check if it matches a known function
                let member_name = &member.member;

                // Check if this matches a known function return type
                if let Some(ret_type) = self.function_return_types.get(member_name) {
                    return self.map_type(ret_type);
                }

                // Fallback: try hardcoded heuristics for common parser methods
                match member_name.as_str() {
                    "parse_def" => return "Option_Definition".to_string(),
                    "parse_fn" => return "Option_Fn".to_string(),
                    "parse_struct" => return "Option_Struct".to_string(),
                    "parse_union" => return "Option_Union".to_string(),
                    "parse_type" => return "Option_Type".to_string(),
                    "parse_ident" => return "Option_Ident".to_string(),
                    "parse_expression" => return "Option_Expression".to_string(),
                    "parse_statement" => return "Option_Statement".to_string(),
                    _ => {}
                }

                // Default: can't infer member type
                "int64_t".to_string()
            }
            parser::Expression::Ident(ident) => {
                // Look up identifier type in variable types map
                if let Some(var_type) = self.variable_types.get(&ident.name) {
                    return var_type.clone();
                }
                // Also check mangled name
                let mangled = self.get_variable_name(&ident.name);
                if let Some(var_type) = self.variable_types.get(&mangled) {
                    return var_type.clone();
                }
                // Default fallback for unknown identifiers
                "int64_t".to_string()
            }
            _ => "int64_t".to_string(), // Fallback for complex expressions
        }
    }

    fn generate_expression(&mut self, expr: &parser::Expression, is_main: bool) -> String {
        match expr {
            parser::Expression::Number(n) => {
                // Check if this is an integer or float
                if n.fract() == 0.0 {
                    format!("{}", *n as i64)
                } else {
                    format!("{}", n)
                }
            }
            parser::Expression::BoolLit(b) => {
                if b.value {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
            parser::Expression::String(s) => {
                // Escape the string properly for C
                format!("\"{}\"", s.replace("\\", "\\\\").replace("\"", "\\\""))
            }
            parser::Expression::Rune(r) => {
                // Escape the character properly for C
                let escaped = match *r {
                    '\'' => "\\'".to_string(),
                    '\\' => "\\\\".to_string(),
                    '\n' => "\\n".to_string(),
                    '\r' => "\\r".to_string(),
                    '\t' => "\\t".to_string(),
                    c => c.to_string(),
                };
                format!("'{}'", escaped)
            }
            parser::Expression::Return(ret_expr) => {
                // Check if this is a void return (returns a block or empty expression)
                match &**ret_expr {
                    parser::Expression::Block(stmts) if stmts.is_empty() => {
                        // For main function with void return, return 0
                        if is_main {
                            "return 0;".to_string()
                        } else {
                            "return;".to_string()
                        }
                    }
                    _ => {
                        // Check if it\'s a switch expression - need special handling
                        if let parser::Expression::Switch(switch_expr) = &**ret_expr {
                            // We can\'t determine the return type here easily without context
                            // For now, use int64_t as default - this is a limitation
                            // TODO: proper type inference or pass return type context
                            let temp_var = "_switch_result";
                            let mut switch_code = self.generate_switch_as_statement(
                                switch_expr,
                                is_main,
                                "int64_t", // Default type
                                temp_var,
                            );
                            switch_code.push_str(&format!("\n    return {};", temp_var));
                            switch_code
                        } else {
                            let expr_code = self.generate_expression(ret_expr, is_main);
                            format!("return {};", expr_code)
                        }
                    }
                }
            }
            parser::Expression::Ident(ident) => {
                // Special handling for Option's 'none' constructor
                if ident.name == "none" {
                    // Check if we're in a function that returns an Option type
                    if let Some(ref return_type) = self.current_return_type {
                        if return_type.name == "Option" && !return_type.params.is_empty() {
                            // Generate the monomorphized type name
                            let type_name = self.type_name_for_instance(return_type);
                            // Generate: (Option_Int){.tag = Option_Int_tag_none}
                            return format!("({}){{.tag = {}_tag_none}}", type_name, type_name);
                        }
                    }
                }
                // Check if this is an enum variant that needs qualification
                let var_name = self.get_variable_name(&ident.name);
                // If the variable doesn't exist in our mappings, it might be an enum variant
                if var_name == ident.name {
                    // Try to qualify as enum variant
                    self.qualify_identifier(&ident.name)
                } else {
                    var_name
                }
            }
            parser::Expression::BinaryOp(binop) => {
                // Handle special operators that need custom codegen
                match binop.op {
                    parser::Operator::Concat => {
                        // String concatenation - use blitz_string_concat helper
                        let left = self.generate_expression(&binop.left, is_main);
                        let right = self.generate_expression(&binop.right, is_main);
                        return format!("blitz_string_concat({}, {})", left, right);
                    }
                    parser::Operator::Else => {
                        // Error handling: left else right
                        // This operator is used for error propagation: expr else fallback
                        // If expr is None/null/empty, evaluate fallback (which might be a return statement)
                        // We need to handle this differently depending on whether right is a statement or expression

                        // Infer the type from the left expression
                        let left_type = self.infer_expr_type(&binop.left);

                        // Check if the right side is a return statement
                        let is_right_return =
                            matches!(&*binop.right, parser::Expression::Return(_));

                        // Determine how to check for "none" based on the type
                        if left_type.starts_with("Option_") {
                            // Option type: check .tag field
                            let tag_name = format!("{}_tag_none", left_type);

                            if is_right_return {
                                let left = self.generate_expression(&binop.left, is_main);
                                let right = self.generate_expression(&binop.right, is_main);
                                return format!(
                                    "({{ if (({}).tag == {}) {{ {}; }} ({}).value; }})",
                                    left, tag_name, right, left
                                );
                            } else {
                                let left = self.generate_expression(&binop.left, is_main);
                                let right = self.generate_expression(&binop.right, is_main);
                                return format!(
                                    "(({}).tag == {} ? ({}) : ({}).value)",
                                    left, tag_name, right, left
                                );
                            }
                        } else {
                            // Non-Option type: check for NULL/zero/empty
                            // For List types, check len == 0
                            // For pointer types, check == NULL
                            // For int types, check == 0
                            let check_expr = if left_type.starts_with("List_") {
                                format!("({}).len == 0", "LEFT_EXPR")
                            } else if left_type.ends_with("*") || left_type == "char*" {
                                format!("({}) == NULL", "LEFT_EXPR")
                            } else if left_type == "int64_t" || left_type == "bool" {
                                format!("!({})", "LEFT_EXPR")
                            } else {
                                // Unknown type - assume pointer
                                format!("({}) == NULL", "LEFT_EXPR")
                            };

                            let left = self.generate_expression(&binop.left, is_main);

                            if is_right_return {
                                let right = self.generate_expression(&binop.right, is_main);
                                let check = check_expr.replace("LEFT_EXPR", &left);
                                return format!(
                                    "({{ if ({}) {{ {}; }} ({}); }})",
                                    check, right, left
                                );
                            } else {
                                let right = self.generate_expression(&binop.right, is_main);
                                let check = check_expr.replace("LEFT_EXPR", &left);
                                return format!("({} ? ({}) : ({}))", check, right, left);
                            }
                        }
                    }
                    parser::Operator::Member => {
                        // Member access should be handled by Expression::Member, not BinaryOp
                        // If we get here, something is wrong - but handle it gracefully
                        eprintln!(
                            "WARNING: Member operator in BinaryOp - should be Expression::Member"
                        );
                        let left = self.generate_expression(&binop.left, is_main);
                        let right = self.generate_expression(&binop.right, is_main);
                        return format!("({}).{}", left, right);
                    }
                    _ => {
                        // Map standard Blitz operators to C operators
                        let op_str = match binop.op {
                            parser::Operator::Add => "+",
                            parser::Operator::Sub => "-",
                            parser::Operator::Mul => "*",
                            parser::Operator::Div => "/",
                            parser::Operator::Rem => "%",
                            parser::Operator::Eq => "==",
                            parser::Operator::Ne => "!=",
                            parser::Operator::Lt => "<",
                            parser::Operator::Le => "<=",
                            parser::Operator::Gt => ">",
                            parser::Operator::Ge => ">=",
                            parser::Operator::And => "&&",
                            parser::Operator::Or => "||",
                            _ => {
                                eprintln!(
                                    "ERROR: Unsupported operator in BinaryOp: {:?}",
                                    binop.op
                                );
                                // Instead of generating malformed C code, generate a placeholder
                                return format!("(/*UNSUPPORTED_OP {:?}*/ 0)", binop.op);
                            }
                        };

                        // Recursively generate left and right expressions
                        let left = self.generate_expression(&binop.left, is_main);
                        let right = self.generate_expression(&binop.right, is_main);

                        // Generate with parentheses for proper precedence
                        format!("({} {} {})", left, op_str, right)
                    }
                }
            }
            parser::Expression::UnaryOp(unary_op) => {
                // Map Blitz unary operators to C operators
                let op_str = match unary_op.op {
                    parser::Operator::Not => "!",
                    parser::Operator::Neg => "-",
                    _ => {
                        eprintln!("ERROR: Unsupported operator in UnaryOp: {:?}", unary_op.op);
                        // Instead of generating malformed C code, generate a placeholder
                        return format!("(/*UNSUPPORTED_UNARY_OP {:?}*/ 0)", unary_op.op);
                    }
                };

                // Recursively generate the operand expression
                let operand = self.generate_expression(&unary_op.expr, is_main);

                // Generate with parentheses for proper precedence
                format!("({}{})", op_str, operand)
            }
            parser::Expression::Call(call) => {
                // Generate function name
                let func_name = &call.name;

                // Generate arguments
                let args: Vec<String> = call
                    .args
                    .iter()
                    .map(|arg| {
                        // For now, ignore labels (named arguments) - just use the expression
                        self.generate_expression(&arg.init, is_main)
                    })
                    .collect();

                // Special handling for Option/Result constructors
                // some(x) -> (Option_T){.tag = Option_T_tag_some, .value = x}
                // ok(x) -> (Result_T_E){.tag = Result_T_E_tag_ok, .value.ok = x}
                // err(e) -> (Result_T_E){.tag = Result_T_E_tag_err, .value.err = e}
                if (func_name == "some" || func_name == "ok" || func_name == "err")
                    && args.len() == 1
                {
                    if let Some(ref return_type) = self.current_return_type {
                        // Check if this is an Option or Result type
                        if return_type.name == "Option"
                            && func_name == "some"
                            && return_type.params.len() == 1
                        {
                            // Generate the monomorphized type name
                            let type_name = self.type_name_for_instance(return_type);
                            // Generate: (Option_Int){.tag = Option_Int_tag_some, .value = arg}
                            return format!(
                                "({}){{.tag = {}_tag_{}, .value = {}}}",
                                type_name, type_name, func_name, args[0]
                            );
                        } else if return_type.name == "Result"
                            && (func_name == "ok" || func_name == "err")
                            && return_type.params.len() == 2
                        {
                            // Generate the monomorphized type name
                            let type_name = self.type_name_for_instance(return_type);
                            // Generate: (Result_Int_String){.tag = Result_Int_String_tag_ok, .value.ok = arg}
                            return format!(
                                "({}){{.tag = {}_tag_{}, .value.{} = {}}}",
                                type_name, type_name, func_name, func_name, args[0]
                            );
                        }
                    }
                }

                // UFCS method calls: obj.method(args) becomes method(obj, args)
                // For UFCS calls, the first argument is the receiver object
                if call.ufcs {
                    // Special handling for unwrap() - needs monomorphization
                    if func_name == "unwrap" && args.len() == 1 {
                        // Infer the Option type from the receiver (first argument)
                        let arg_expr = &call.args[0].init;
                        let option_type = self.infer_expr_type(arg_expr);

                        // Check if this is an Option type
                        if option_type.starts_with("Option_") {
                            // Extract the inner type from Option_T
                            let inner_type = option_type.strip_prefix("Option_").unwrap();
                            // Check if the inner type is a generic parameter (can't monomorphize)
                            let is_generic = inner_type.len() == 1
                                && inner_type
                                    .chars()
                                    .next()
                                    .map_or(false, |c| c.is_uppercase());
                            if !is_generic {
                                // Track that we need to generate this unwrap function
                                self.unwrap_needed.insert(inner_type.to_string());
                                // Also track the Option generic instance
                                self.generic_instances
                                    .entry(option_type.clone())
                                    .or_insert((
                                        "Option".to_string(),
                                        vec![inner_type.to_string()],
                                    ));
                                // Generate call to monomorphized unwrap function
                                return format!("unwrap_{}({})", inner_type, args[0]);
                            }
                            // Fallback for generic types - cast to void* (not ideal but prevents invalid C)
                            eprintln!("WARNING: Cannot monomorphize unwrap for generic type parameter '{}'", inner_type);
                        }
                    }

                    // Special handling for built-in string/list methods
                    match func_name.as_str() {
                        "chars" if args.len() == 1 => {
                            // String.chars() -> blitz_string_chars(str)
                            return format!("blitz_string_chars({})", args[0]);
                        }
                        "len" if args.len() == 1 => {
                            // List.len() -> list.len (access the len field)
                            return format!("({}).len", args[0]);
                        }
                        "substring" if args.len() == 3 => {
                            // List(Rune).substring(start, until) -> blitz_substring(list, start, until)
                            return format!(
                                "blitz_substring({}, {}, {})",
                                args[0], args[1], args[2]
                            );
                        }
                        _ => {
                            // For other UFCS calls, the receiver is the first argument
                            // Try to guess argument types for overload resolution
                            let arg_types: Vec<String> = call
                                .args
                                .iter()
                                .map(|arg| self.guess_arg_type(&arg.init))
                                .collect();

                            // Resolve to mangled name if needed
                            let resolved_name = self.resolve_function_call(func_name, &arg_types);

                            // Format as: func_name(receiver, arg1, arg2, ...)
                            format!("{}({})", resolved_name, args.join(", "))
                        }
                    }
                } else {
                    // Special handling for unwrap() - needs monomorphization
                    if func_name == "unwrap" && args.len() == 1 {
                        // Infer the Option type from the argument
                        let arg_expr = &call.args[0].init;
                        let option_type = self.infer_expr_type(arg_expr);

                        // Check if this is an Option type
                        if option_type.starts_with("Option_") {
                            // Extract the inner type from Option_T
                            let inner_type = option_type.strip_prefix("Option_").unwrap();
                            // Check if the inner type is a generic parameter (can't monomorphize)
                            let is_generic = inner_type.len() == 1
                                && inner_type
                                    .chars()
                                    .next()
                                    .map_or(false, |c| c.is_uppercase());
                            if !is_generic {
                                // Track that we need to generate this unwrap function
                                self.unwrap_needed.insert(inner_type.to_string());
                                // Also track the Option generic instance
                                self.generic_instances
                                    .entry(option_type.clone())
                                    .or_insert((
                                        "Option".to_string(),
                                        vec![inner_type.to_string()],
                                    ));
                                // Generate call to monomorphized unwrap function
                                return format!("unwrap_{}({})", inner_type, args[0]);
                            }
                            // Fallback for generic types - cast to void* (not ideal but prevents invalid C)
                            eprintln!("WARNING: Cannot monomorphize unwrap for generic type parameter '{}'", inner_type);
                        }
                    }

                    // Regular function call - try to guess argument types for overload resolution
                    let arg_types: Vec<String> = call
                        .args
                        .iter()
                        .map(|arg| self.guess_arg_type(&arg.init))
                        .collect();

                    // Resolve to mangled name if needed
                    let resolved_name = self.resolve_function_call(func_name, &arg_types);

                    format!("{}({})", resolved_name, args.join(", "))
                }
            }
            parser::Expression::Constructor(ctor) => {
                // Generate C99 compound literal with designated initializers
                // Blitz: Lexer(source: source.chars(), index: 0)
                // C: (Lexer){.source = source_chars(), .index = 0}

                // For constructors, we need the raw struct name, not the pointer type
                // that map_type would give us
                let type_name = &ctor.r#type.name;

                // Generate field initializers
                let mut field_inits = Vec::new();
                for arg in &ctor.args {
                    let field_name = &arg.label.name;
                    let field_value = self.generate_expression(&arg.init, is_main);
                    field_inits.push(format!(".{} = {}", field_name, field_value));
                }

                // Check if we need to return a pointer to this struct
                // This happens when the struct type is known and not an enum
                let needs_pointer =
                    self.seen_types.contains(type_name) && !self.enum_types.contains(type_name);

                // Format as compound literal: (TypeName){.field1 = val1, .field2 = val2}
                let compound_literal = format!("({}){{{}}}", type_name, field_inits.join(", "));

                if needs_pointer {
                    // Heap-allocate and return pointer: memcpy(malloc(sizeof(T)), &(T){...}, sizeof(T))
                    format!(
                        "memcpy(malloc(sizeof({})), &{}, sizeof({}))",
                        type_name, compound_literal, type_name
                    )
                } else {
                    compound_literal
                }
            }
            parser::Expression::Member(member) => {
                // Generate member access: obj.field or obj->field
                let parent_code = self.generate_expression(&member.parent, is_main);
                let member_name = &member.member;

                // Special case: .mut is a Blitz mutability marker, not a real field
                // Just return the parent expression
                if member_name == "mut" {
                    return parent_code;
                }

                // For C struct member access, we need to decide between '.' and '->'
                // Since most structs are passed as pointers in our C codegen (see map_type),
                // we should use '->' for struct types

                // Simple heuristic: if parent_code starts with '&' or contains '->', use '.'
                // Otherwise, use '->' for struct member access (assuming pointers)
                if parent_code.starts_with('&') || parent_code.contains("->") {
                    format!("{}.{}", parent_code.trim_start_matches('&'), member_name)
                } else if parent_code.starts_with('*') {
                    // Dereferenced pointer: (*ptr).field can be simplified to ptr->field
                    format!("{}->{}", parent_code.trim_start_matches('*'), member_name)
                } else {
                    // Default: use arrow operator for pointer-based struct access
                    format!("{}->{}", parent_code, member_name)
                }
            }
            parser::Expression::Index(idx) => {
                // Generate array/list indexing
                // For List(T) types, we need to access via .data[index]
                // For plain arrays, we use direct indexing

                let target_expr = self.generate_expression(&idx.target, is_main);
                let index_expr = self.generate_expression(&idx.index, is_main);

                // Check if this is a member access on a List type
                // For now, we'll generate the simple form and rely on the user
                // to provide the correct syntax. A full implementation would need
                // type inference to determine if target is a List type.
                //
                // Simple heuristic: if target is a member access that looks like it might
                // be a List field, we need to add .data
                //
                // For List types: target.data[index]
                // For arrays: target[index]

                // Check if the target is a Member expression accessing a List field
                if let parser::Expression::Member(member) = &*idx.target {
                    // This is something like lexer.source[i] or obj.field[index]
                    // For List types, we need: lexer->source.data[i]
                    // We'll use a simple heuristic: if the member name
                    // suggests it's a list (like "source", "elems", "items"), add .data
                    let member_name = &member.member;
                    if member_name == "source"
                        || member_name == "elems"
                        || member_name == "items"
                        || member_name == "data"
                        || member_name.ends_with("List")
                    {
                        // Likely a List type field, add .data
                        format!("{}.data[{}]", target_expr, index_expr)
                    } else {
                        // Regular array access
                        format!("{}[{}]", target_expr, index_expr)
                    }
                } else {
                    // Simple array or direct identifier indexing
                    format!("{}[{}]", target_expr, index_expr)
                }
            }
            parser::Expression::Assignment(assign) => {
                // Generate assignment: lval = rhs
                let lval_code = match &assign.left {
                    parser::Lval::Ident(ident) => self.get_variable_name(&ident.name),
                    parser::Lval::Member(member) => {
                        // Use the same logic as Expression::Member for consistency
                        let parent_code = self.generate_expression(&member.parent, is_main);
                        let member_name = &member.member;

                        // Special case: .mut is a Blitz mutability marker
                        if member_name == "mut" {
                            parent_code
                        } else if parent_code.starts_with('&') || parent_code.contains("->") {
                            format!("{}.{}", parent_code.trim_start_matches('&'), member_name)
                        } else if parent_code.starts_with('*') {
                            format!("{}->{}", parent_code.trim_start_matches('*'), member_name)
                        } else {
                            format!("{}->{}", parent_code, member_name)
                        }
                    }
                    parser::Lval::Index(index) => {
                        let target = self.generate_expression(&index.target, is_main);
                        let idx = self.generate_expression(&index.index, is_main);
                        format!("{}[{}]", target, idx)
                    }
                };
                let rhs = self.generate_expression(&assign.right, is_main);
                format!("{} = {}", lval_code, rhs)
            }
            parser::Expression::For(for_loop) => {
                // For loops iterate over ranges or lists
                // For Range iteration: for Range(begin: 0, until: 10) |i| { body }
                // Generate C for-loop: for (int64_t i = 0; i < 10; i++) { body }

                // Mangle the loop variable name if it collides with C stdlib
                let elem_name = self.mangle_variable_name(&for_loop.elem);

                // Check if the iter expression is a Range constructor
                if let parser::Expression::Constructor(ctor) = &*for_loop.iter {
                    if ctor.r#type.name == "Range" {
                        // Extract begin and until values from Range constructor
                        let mut begin_expr = None;
                        let mut until_expr = None;

                        for arg in &ctor.args {
                            match arg.label.name.as_str() {
                                "begin" => {
                                    begin_expr = Some(self.generate_expression(&arg.init, is_main));
                                }
                                "until" => {
                                    until_expr = Some(self.generate_expression(&arg.init, is_main));
                                }
                                _ => {}
                            }
                        }

                        if let (Some(begin), Some(until)) = (begin_expr, until_expr) {
                            // Generate body statements
                            let mut body_code = String::new();
                            for stmt in &for_loop.body {
                                let stmt_code = self.generate_statement(stmt, is_main);
                                body_code.push_str("        ");
                                body_code.push_str(&stmt_code);
                                body_code.push_str("\n");
                            }

                            // Generate C for-loop with proper iterator variable declaration
                            return format!(
                                "for (int64_t {} = {}; {} < {}; {}++) {{\n{}    }}",
                                elem_name, begin, elem_name, until, elem_name, body_code
                            );
                        }
                    }
                }

                // Fallback: generate a TODO comment for non-Range iterations
                // DO NOT emit the loop body since the iterator variable is not declared
                let iter_code = self.generate_expression(&for_loop.iter, is_main);

                format!(
                    "// TODO: For loop over non-Range/non-List: for {} in {} {{ ... }}",
                    elem_name, iter_code
                )
            }
            parser::Expression::While(while_loop) => {
                // Generate while loop: while (condition) { body }
                let cond = self.generate_expression(&while_loop.cond, is_main);

                // Generate body statements
                let mut body_code = String::new();
                for stmt in &while_loop.body {
                    let stmt_code = self.generate_statement(stmt, is_main);
                    body_code.push_str("        ");
                    body_code.push_str(&stmt_code);
                    body_code.push_str("\n");
                }

                format!("while ({}) {{\n{}    }}", cond, body_code)
            }
            parser::Expression::If(if_expr) => {
                // Generate if expression/statement
                // If can be used as expression (returns value) or statement
                let cond = self.generate_expression(&if_expr.cond, is_main);

                // Generate body statements
                let mut body_code = String::new();
                for stmt in &if_expr.body {
                    let stmt_code = self.generate_statement(stmt, is_main);
                    body_code.push_str("        ");
                    body_code.push_str(&stmt_code);
                    body_code.push_str("\n");
                }

                format!("if ({}) {{\n{}    }}", cond, body_code)
            }
            parser::Expression::Switch(switch_expr) => self.generate_switch(switch_expr, is_main),
            parser::Expression::List(list) => {
                // Generate list literal: List(T) with malloc'd data array
                // For empty lists: []
                // For initialized lists: [a, b, c]

                if list.elems.is_empty() {
                    // Empty list: We can't determine the element type without context
                    // For now, generate a zero initializer that will work with any List_T type
                    "(List_Int){.data = NULL, .len = 0, .cap = 0}".to_string()
                } else {
                    // Non-empty list: allocate array and initialize elements
                    // We use a GNU C statement expression for complex initialization
                    let len = list.elems.len();
                    let mut code = String::new();
                    code.push_str("({\n");

                    // Check if all elements are integers for simple type inference
                    let all_ints = list
                        .elems
                        .iter()
                        .all(|e| matches!(e, parser::Expression::Number(_)));

                    if all_ints {
                        // All integers - use int64_t*
                        code.push_str(&format!(
                            "        int64_t* _tmp = malloc(sizeof(int64_t) * {});\n",
                            len
                        ));
                        for (i, elem) in list.elems.iter().enumerate() {
                            let elem_code = self.generate_expression(elem, is_main);
                            code.push_str(&format!("        _tmp[{}] = {};\n", i, elem_code));
                        }
                        code.push_str(&format!(
                            "        (List_Int){{.data = _tmp, .len = {}, .cap = {}}};\n",
                            len, len
                        ));
                    } else {
                        // Mixed/complex types - we can't infer the type properly
                        // Just generate a stub for now
                        code.push_str("        /* TODO: non-integer list literal requires type inference */\n");
                        code.push_str(&format!("        void* _tmp = NULL;\n"));
                        code.push_str("        (void*){{}};\n");
                    }

                    code.push_str("    })");
                    code
                }
            }
            parser::Expression::Group(group) => {
                // Parenthesized expression
                let inner = self.generate_expression(&group.expr, is_main);
                format!("({})", inner)
            }
            parser::Expression::Block(stmts) => {
                // Block expression: { stmt1; stmt2; ... }
                let mut block_code = String::new();
                block_code.push_str("{\n");
                for stmt in stmts {
                    let stmt_code = self.generate_statement(stmt, is_main);
                    block_code.push_str("        ");
                    block_code.push_str(&stmt_code);
                    block_code.push_str("\n");
                }
                block_code.push_str("    }");
                block_code
            }
            parser::Expression::Continue => "continue".to_string(),
            parser::Expression::Break => "break".to_string(),
        }
    }

    fn generate_switch(&mut self, switch_expr: &parser::Switch, is_main: bool) -> String {
        let cond = self.generate_expression(&switch_expr.cond, is_main);

        // Check if we can use a C switch statement or need if-else chain
        // C switch only works with integer/char constants, not strings or complex patterns
        let can_use_c_switch = switch_expr.cases.iter().all(|case| {
            matches!(
                case.label,
                parser::SwitchLabel::CharLit(_)
                    | parser::SwitchLabel::NumberLit(_)
                    | parser::SwitchLabel::Ident(_)
                    | parser::SwitchLabel::Else(_)
            )
        });

        if can_use_c_switch {
            self.generate_c_switch(&cond, &switch_expr.cases, is_main)
        } else {
            self.generate_if_else_chain(&cond, &switch_expr.cases, is_main)
        }
    }

    fn generate_c_switch(
        &mut self,
        cond: &str,
        cases: &[parser::SwitchCase],
        is_main: bool,
    ) -> String {
        let mut code = format!("switch ({}) {{\n", cond);

        for case in cases {
            // Generate case label
            match &case.label {
                parser::SwitchLabel::CharLit(ch) => {
                    // Character literal
                    let escaped = match ch.value {
                        '\'' => "\\'".to_string(),
                        '\\' => "\\\\".to_string(),
                        '\n' => "\\n".to_string(),
                        '\r' => "\\r".to_string(),
                        '\t' => "\\t".to_string(),
                        c => c.to_string(),
                    };
                    code.push_str(&format!("        case '{}':\n", escaped));
                }
                parser::SwitchLabel::NumberLit(num) => {
                    // Number literal
                    if num.value.fract() == 0.0 {
                        code.push_str(&format!("        case {}:\n", num.value as i64));
                    } else {
                        // Can't use float in C switch, fall back to if-else
                        return self.generate_if_else_chain(cond, cases, is_main);
                    }
                }
                parser::SwitchLabel::Ident(ident) => {
                    // Identifier - could be enum variant or "else"
                    if ident.name == "else" {
                        code.push_str("        default:\n");
                    } else {
                        // Enum variant - qualify it with enum type
                        let qualified = self.qualify_identifier(&ident.name);
                        code.push_str(&format!("        case {}:\n", qualified));
                    }
                }
                parser::SwitchLabel::Else(_) => {
                    // "otherwise" variant
                    code.push_str("        default:\n");
                }
                parser::SwitchLabel::Type(_) => {
                    // Type pattern - not supported in simple switch, use if-else chain
                    return self.generate_if_else_chain(cond, cases, is_main);
                }
                parser::SwitchLabel::StringLit(_) => {
                    // String pattern - not supported in C switch, use if-else chain
                    return self.generate_if_else_chain(cond, cases, is_main);
                }
            }

            // Generate case body
            code.push_str("        {\n");
            for stmt in &case.body {
                let stmt_code = self.generate_statement(stmt, is_main);
                code.push_str("            ");
                code.push_str(&stmt_code);
                code.push_str("\n");
            }

            // Add break unless the body ends with return/break/continue
            let needs_break = if let Some(last_stmt) = case.body.last() {
                match last_stmt {
                    parser::Statement::Expression(expr) => !matches!(
                        expr,
                        parser::Expression::Return(_)
                            | parser::Expression::Break
                            | parser::Expression::Continue
                    ),
                    _ => true,
                }
            } else {
                true
            };

            if needs_break {
                code.push_str("            break;\n");
            }
            code.push_str("        }\n");
        }

        code.push_str("    }");
        code
    }

    fn generate_if_else_chain(
        &mut self,
        cond: &str,
        cases: &[parser::SwitchCase],
        is_main: bool,
    ) -> String {
        let mut code = String::new();
        let mut first = true;

        for case in cases {
            // Check if this is a default case
            let is_default = matches!(case.label, parser::SwitchLabel::Else(_))
                || (matches!(&case.label, parser::SwitchLabel::Ident(i) if i.name == "else"));

            if is_default {
                // Default case - just use else
                if !first {
                    code.push_str(" else {\n");
                } else {
                    code.push_str("{\n");
                }
            } else {
                // Generate condition
                let condition = match &case.label {
                    parser::SwitchLabel::CharLit(ch) => {
                        let escaped = match ch.value {
                            '\'' => "\\'".to_string(),
                            '\\' => "\\\\".to_string(),
                            '\n' => "\\n".to_string(),
                            '\r' => "\\r".to_string(),
                            '\t' => "\\t".to_string(),
                            c => c.to_string(),
                        };
                        format!("{} == '{}'", cond, escaped)
                    }
                    parser::SwitchLabel::NumberLit(num) => {
                        if num.value.fract() == 0.0 {
                            format!("{} == {}", cond, num.value as i64)
                        } else {
                            format!("{} == {}", cond, num.value)
                        }
                    }
                    parser::SwitchLabel::StringLit(s) => {
                        // String comparison - use strcmp
                        format!(
                            "strcmp({}, \"{}\") == 0",
                            cond,
                            s.value.replace("\\", "\\\\").replace("\"", "\\\"")
                        )
                    }
                    parser::SwitchLabel::Ident(ident) => {
                        // Enum variant comparison - qualify the identifier
                        let qualified = self.qualify_identifier(&ident.name);
                        format!("{} == {}", cond, qualified)
                    }
                    parser::SwitchLabel::Type(ty) => {
                        // Type pattern - check tag field for tagged unions
                        format!("{}->tag == {}_tag_{}", cond, ty.name, ty.name)
                    }
                    parser::SwitchLabel::Else(_) => unreachable!(), // handled above
                };

                if first {
                    code.push_str(&format!("if ({}) {{\n", condition));
                    first = false;
                } else {
                    code.push_str(&format!(" else if ({}) {{\n", condition));
                }
            }

            // Generate case body
            for stmt in &case.body {
                let stmt_code = self.generate_statement(stmt, is_main);
                code.push_str("        ");
                code.push_str(&stmt_code);
                code.push_str("\n");
            }
            code.push_str("    }");
        }

        code
    }

    /// Generate switch expression as a statement with assignment to variable
    /// This is needed because C switch is a statement, not an expression
    fn generate_switch_as_statement(
        &mut self,
        switch_expr: &parser::Switch,
        is_main: bool,
        var_type: &str,
        var_name: &str,
    ) -> String {
        let cond = self.generate_expression(&switch_expr.cond, is_main);

        // Declare the variable first
        let mut code = format!("{} {};\n    ", var_type, var_name);

        // Check if we can use a C switch statement or need if-else chain
        let can_use_c_switch = switch_expr.cases.iter().all(|case| {
            matches!(
                case.label,
                parser::SwitchLabel::CharLit(_)
                    | parser::SwitchLabel::NumberLit(_)
                    | parser::SwitchLabel::Ident(_)
                    | parser::SwitchLabel::Else(_)
            )
        });

        if can_use_c_switch {
            code.push_str(&self.generate_c_switch_as_statement(
                &cond,
                &switch_expr.cases,
                is_main,
                var_name,
            ));
        } else {
            code.push_str(&self.generate_if_else_chain_as_statement(
                &cond,
                &switch_expr.cases,
                is_main,
                var_name,
            ));
        }

        code
    }

    /// Generate C switch statement form with assignments to variable
    fn generate_c_switch_as_statement(
        &mut self,
        cond: &str,
        cases: &[parser::SwitchCase],
        is_main: bool,
        var_name: &str,
    ) -> String {
        let mut code = format!("switch ({}) {{\n", cond);

        for case in cases {
            // Generate case label
            match &case.label {
                parser::SwitchLabel::CharLit(ch) => {
                    let escaped = match ch.value {
                        '\'' => "\\'".to_string(),
                        '\\' => "\\\\".to_string(),
                        '\n' => "\\n".to_string(),
                        '\r' => "\\r".to_string(),
                        '\t' => "\\t".to_string(),
                        c => c.to_string(),
                    };
                    code.push_str(&format!("        case \'{}\':\n", escaped));
                }
                parser::SwitchLabel::NumberLit(num) => {
                    if num.value.fract() == 0.0 {
                        code.push_str(&format!("        case {}:\n", num.value as i64));
                    } else {
                        // Can't use float in C switch, fall back to if-else
                        return self
                            .generate_if_else_chain_as_statement(cond, cases, is_main, var_name);
                    }
                }
                parser::SwitchLabel::Ident(ident) => {
                    if ident.name == "else" {
                        code.push_str("        default:\n");
                    } else {
                        code.push_str(&format!("        case {}:\n", ident.name));
                    }
                }
                parser::SwitchLabel::Else(_) => {
                    code.push_str("        default:\n");
                }
                parser::SwitchLabel::Type(_) => {
                    return self
                        .generate_if_else_chain_as_statement(cond, cases, is_main, var_name);
                }
                parser::SwitchLabel::StringLit(_) => {
                    return self
                        .generate_if_else_chain_as_statement(cond, cases, is_main, var_name);
                }
            }

            // Generate case body with assignment
            code.push_str("        {\n");

            // If the body has statements, process them
            if !case.body.is_empty() {
                // Check if the last statement is an expression that should be assigned
                let last_idx = case.body.len() - 1;

                for (idx, stmt) in case.body.iter().enumerate() {
                    if idx == last_idx {
                        // Last statement - check if it\'s an expression to assign
                        match stmt {
                            parser::Statement::Expression(expr) => {
                                // If it\'s a control flow expression, generate it normally
                                if matches!(
                                    expr,
                                    parser::Expression::Return(_)
                                        | parser::Expression::Break
                                        | parser::Expression::Continue
                                ) {
                                    let stmt_code = self.generate_statement(stmt, is_main);
                                    code.push_str("            ");
                                    code.push_str(&stmt_code);
                                    code.push_str("\n");
                                } else {
                                    // Regular expression - assign its value
                                    let expr_code = self.generate_expression(expr, is_main);
                                    code.push_str(&format!(
                                        "            {} = {};\n",
                                        var_name, expr_code
                                    ));
                                }
                            }
                            parser::Statement::Declaration(_) => {
                                // Declaration as last statement - just generate it
                                let stmt_code = self.generate_statement(stmt, is_main);
                                code.push_str("            ");
                                code.push_str(&stmt_code);
                                code.push_str("\n");
                            }
                        }
                    } else {
                        // Not the last statement - generate normally
                        let stmt_code = self.generate_statement(stmt, is_main);
                        code.push_str("            ");
                        code.push_str(&stmt_code);
                        code.push_str("\n");
                    }
                }
            }

            // Add break unless the body ends with return/break/continue
            let needs_break = if let Some(last_stmt) = case.body.last() {
                match last_stmt {
                    parser::Statement::Expression(expr) => !matches!(
                        expr,
                        parser::Expression::Return(_)
                            | parser::Expression::Break
                            | parser::Expression::Continue
                    ),
                    _ => true,
                }
            } else {
                true
            };

            if needs_break {
                code.push_str("            break;\n");
            }
            code.push_str("        }\n");
        }

        code.push_str("    }");
        code
    }

    /// Generate if-else chain statement form with assignments to variable
    fn generate_if_else_chain_as_statement(
        &mut self,
        cond: &str,
        cases: &[parser::SwitchCase],
        is_main: bool,
        var_name: &str,
    ) -> String {
        let mut code = String::new();
        let mut first = true;

        for case in cases {
            let is_default = matches!(case.label, parser::SwitchLabel::Else(_))
                || (matches!(&case.label, parser::SwitchLabel::Ident(i) if i.name == "else"));

            if is_default {
                if !first {
                    code.push_str(" else {\n");
                } else {
                    code.push_str("{\n");
                }
            } else {
                let condition = match &case.label {
                    parser::SwitchLabel::CharLit(ch) => {
                        let escaped = match ch.value {
                            '\'' => "\\'".to_string(),
                            '\\' => "\\\\".to_string(),
                            '\n' => "\\n".to_string(),
                            '\r' => "\\r".to_string(),
                            '\t' => "\\t".to_string(),
                            c => c.to_string(),
                        };
                        format!("{} == '{}'", cond, escaped)
                    }
                    parser::SwitchLabel::NumberLit(num) => {
                        if num.value.fract() == 0.0 {
                            format!("{} == {}", cond, num.value as i64)
                        } else {
                            format!("{} == {}", cond, num.value)
                        }
                    }
                    parser::SwitchLabel::StringLit(s) => {
                        format!(
                            "strcmp({}, \"{}\") == 0",
                            cond,
                            s.value.replace("\\", "\\\\").replace("\"", "\\\"")
                        )
                    }
                    parser::SwitchLabel::Ident(ident) => {
                        format!("{} == {}", cond, ident.name)
                    }
                    parser::SwitchLabel::Type(ty) => {
                        format!("{}->tag == {}_tag_{}", cond, ty.name, ty.name)
                    }
                    parser::SwitchLabel::Else(_) => unreachable!(),
                };

                if first {
                    code.push_str(&format!("if ({}) {{\n", condition));
                    first = false;
                } else {
                    code.push_str(&format!(" else if ({}) {{\n", condition));
                }
            }

            // Generate case body with assignment
            if !case.body.is_empty() {
                let last_idx = case.body.len() - 1;

                for (idx, stmt) in case.body.iter().enumerate() {
                    if idx == last_idx {
                        match stmt {
                            parser::Statement::Expression(expr) => {
                                if matches!(
                                    expr,
                                    parser::Expression::Return(_)
                                        | parser::Expression::Break
                                        | parser::Expression::Continue
                                ) {
                                    let stmt_code = self.generate_statement(stmt, is_main);
                                    code.push_str("        ");
                                    code.push_str(&stmt_code);
                                    code.push_str("\n");
                                } else {
                                    let expr_code = self.generate_expression(expr, is_main);
                                    code.push_str(&format!(
                                        "        {} = {};\n",
                                        var_name, expr_code
                                    ));
                                }
                            }
                            parser::Statement::Declaration(_) => {
                                let stmt_code = self.generate_statement(stmt, is_main);
                                code.push_str("        ");
                                code.push_str(&stmt_code);
                                code.push_str("\n");
                            }
                        }
                    } else {
                        let stmt_code = self.generate_statement(stmt, is_main);
                        code.push_str("        ");
                        code.push_str(&stmt_code);
                        code.push_str("\n");
                    }
                }
            }

            code.push_str("    }");
        }

        code
    }

    /// Map a type name string to its C equivalent
    /// This is used when we have a type name as a string (from infer_expr_type)
    /// rather than a Type struct
    fn map_type_name(&self, type_name: &str) -> String {
        match type_name {
            "Int" => "int64_t".to_string(),
            "Bool" => "bool".to_string(),
            "Float" => "double".to_string(),
            "String" => "char*".to_string(),
            "Rune" => "uint32_t".to_string(),
            "Void" => "void".to_string(),
            _ => {
                // For other types, check if it's a struct type (should be pointer)
                // or an enum type (should be value)
                if self.seen_types.contains(type_name) && !self.enum_types.contains(type_name) {
                    format!("{}*", type_name)
                } else {
                    type_name.to_string()
                }
            }
        }
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
                    // Look up the C name for this type (handles collision resolution)
                    // Check if this type has been registered
                    let c_name = if self.seen_types.contains(&ty.name) {
                        // Use the registered C name
                        // Note: get_c_name returns the first registered instance
                        // This is a limitation but works for simple cases
                        ty.name.clone() // For now, use original name as we stored C names in seen_types
                    } else {
                        ty.name.clone()
                    };

                    // Check if this is a known struct type (not enum) - if so, use pointer for forward-declaration safety
                    if self.seen_types.contains(&c_name) && !self.enum_types.contains(&c_name) {
                        format!("{}*", c_name)
                    } else {
                        c_name
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
#include <stdio.h>

typedef int64_t Int;
typedef bool Bool;
typedef double Float;
typedef uint32_t Rune;
typedef char* String;

// Built-in Range type (TODO: verify structure)
typedef struct {
    int64_t begin;
    int64_t until;  // exclusive upper bound (matches Blitz source)
} Range;

// List type definition for Rune
typedef struct List_Rune {
    uint32_t* data;
    size_t len;
    size_t cap;
} List_Rune;

// Option type for String (used by blitz_read)
typedef enum {
    Option_String_tag_none,
    Option_String_tag_some
} Option_String_Tag;

typedef struct {
    Option_String_Tag tag;
    char* value;
} Option_String;

// Built-in string method implementations

// String.chars() - converts a C string to a List(Rune)
static inline List_Rune blitz_string_chars(const char* str) {
    if (!str) {
        return (List_Rune){.data = NULL, .len = 0, .cap = 0};
    }
    
    size_t byte_len = strlen(str);
    size_t cap = byte_len + 1; // Initial capacity estimate
    uint32_t* data = (uint32_t*)malloc(sizeof(uint32_t) * cap);
    size_t len = 0;
    
    // Simple ASCII handling for now - full UTF-8 would be more complex
    for (size_t i = 0; i < byte_len; i++) {
        if (len >= cap) {
            cap *= 2;
            data = (uint32_t*)realloc(data, sizeof(uint32_t) * cap);
        }
        // For now, just treat each byte as a character
        // TODO: proper UTF-8 decoding
        data[len++] = (uint32_t)(unsigned char)str[i];
    }
    
    return (List_Rune){.data = data, .len = len, .cap = cap};
}

// List(Rune).substring(start, until) - extracts substring from rune list
static inline String blitz_substring(List_Rune list, int64_t start, int64_t until) {
    if (start < 0) start = 0;
    if (until > (int64_t)list.len) until = list.len;
    if (start >= until) return strdup("");
    
    size_t len = until - start;
    char* result = (char*)malloc(len + 1);
    
    // Simple ASCII conversion - each rune becomes one byte
    // TODO: proper UTF-8 encoding for non-ASCII runes
    for (size_t i = 0; i < len; i++) {
        uint32_t rune = list.data[start + i];
        result[i] = (char)(rune & 0xFF); // Take low byte
    }
    result[len] = '\0';
    
    return result;
}

// ============================================================================
// Runtime function declarations
// ============================================================================
// These functions are required by the Blitz runtime but are not yet implemented.
// They provide core functionality for I/O, error handling, and type operations.

// Print functions - overloaded in Blitz, mapped to _Generic macro in C
// Usage: print(x) expands to the appropriate function based on type
#define print(x) _Generic((x), \
    char*: print_str, \
    const char*: print_str, \
    int64_t: print_int, \
    int: print_int, \
    bool: print_bool, \
    double: print_float, \
    List_Rune: print_list_rune, \
    List_Definition: print_unknown, \
    default: print_unknown \
)(x)

void print_str(const char* str);
void print_int(int64_t val);
void print_bool(bool val);
void print_float(double val);
void print_list_rune(List_Rune list);
void print_unknown(void* ptr);

// Panic function - terminates the program with an error message
// Used for unrecoverable errors
void panic(const char* message) __attribute__((noreturn));

// Unwrap functions for Option types
// These extract the value from Some(x) or panic if None
// Note: Unwrap functions are now generated automatically by the transpiler
// for each Option type that is actually used. The generated functions are
// type-safe and take Option_T as a parameter rather than void*.
//
// The old macro-based approach is commented out:
// #define DECLARE_UNWRAP(T) \
//     T unwrap_##T(void* option_ptr);
//
// DECLARE_UNWRAP(Int)
// DECLARE_UNWRAP(Bool)
// DECLARE_UNWRAP(String)
// DECLARE_UNWRAP(Float)

// Read functions for basic I/O
// These are placeholders for file/stdin reading operations
typedef struct {
    char* data;
    size_t len;
    bool ok;
} ReadResult;

ReadResult read_line(void);
ReadResult read_file(const char* path);
char* read_to_string(const char* path);

// ============================================================================
// Built-in Blitz runtime functions
// ============================================================================
// These functions are provided by the Blitz runtime and are available to all
// Blitz programs without explicit import.

// Note: time() and read() are typically user-defined functions in Blitz programs.
// When they are called, the transpiler automatically mangles them to avoid C stdlib
// collisions:
//   - time() -> blitz_time() (returns int64_t milliseconds since epoch)
//   - read() -> blitz_read() (returns Option(String))
//
// If these functions are NOT defined in user code, you can provide implementations
// in blitz_runtime.c. The forward declarations will be generated automatically in
// blitz.h when user code defines them. For built-in implementations, declare them here:

// Forward declarations for built-in runtime functions (if not user-defined)
// These are declared here and implemented in blitz_runtime.c
int64_t blitz_time(void);
Option_String blitz_read(char* path);  // Returns Option(String)

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

        // Add generic type instantiations (must come before function forward declarations)
        if !self.generic_instances.is_empty() {
            full_header.push_str("// Generic type instantiations\n");
            let mut instances: Vec<_> = self.generic_instances.iter().collect();
            instances.sort_by_key(|(name, _)| *name);

            for (instance_name, (base_type, params)) in instances {
                // Skip built-in types
                if self.is_builtin_type(instance_name) {
                    continue;
                }

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
                    } else if param_type == "Rune" {
                        // Rune is uint32_t
                        "uint32_t".to_string()
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
                    // Use pointers for all non-primitive types to avoid incomplete type errors
                    let c_type = if self.is_primitive_type(param_type) {
                        param_type.clone()
                    } else {
                        format!("{}*", param_type)
                    };

                    full_header.push_str(&format!(
                        "typedef enum {{\n    {}_tag_none,\n    {}_tag_some\n}} {}_Tag;\n\n",
                        instance_name, instance_name, instance_name
                    ));
                    full_header.push_str(&format!(
                        "typedef struct {{\n    {}_Tag tag;\n    {} value;\n}} {};\n\n",
                        instance_name, c_type, instance_name
                    ));
                } else if base_type == "Result" && params.len() == 2 {
                    // Result(T, E) -> tagged union with ok/err
                    let ok_type = &params[0];
                    let err_type = &params[1];
                    // Determine the C types
                    let ok_c_type = if ok_type == "Type" || ok_type == "Ident" {
                        format!("{}*", ok_type)
                    } else {
                        ok_type.clone()
                    };
                    let err_c_type = if err_type == "Type" || err_type == "Ident" {
                        format!("{}*", err_type)
                    } else {
                        err_type.clone()
                    };

                    full_header.push_str(&format!(
                        "typedef enum {{\n    {}_tag_ok,\n    {}_tag_err\n}} {}_Tag;\n\n",
                        instance_name, instance_name, instance_name
                    ));
                    // For Result, we need a union to hold either ok or err value
                    full_header.push_str(&format!(
                        "typedef struct {{\n    {}_Tag tag;\n    union {{\n        {} ok;\n        {} err;\n    }} value;\n}} {};\n\n",
                        instance_name, ok_c_type, err_c_type, instance_name
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

        // Add function forward declarations (after generic type instantiations)
        if !self.all_functions.is_empty() {
            full_header.push_str(
                "// Function forward declarations
",
            );
            for (_, signature) in &self.all_functions {
                full_header.push_str(signature);
                full_header.push_str(
                    "
",
                );
            }
            full_header.push_str(
                "
",
            );
        }

        // Add unwrap function forward declarations
        if !self.unwrap_needed.is_empty() {
            full_header.push_str("// Unwrap function forward declarations\n");
            for inner_type in &self.unwrap_needed {
                // Determine the C type for the unwrap return value
                // This should match the type used in the Option_T value field
                let c_type = if self.is_primitive_type(inner_type) {
                    self.map_type_name(inner_type)
                } else {
                    // Non-primitive: use pointer (match Option value field type)
                    format!("{}*", inner_type)
                };
                full_header.push_str(&format!(
                    "{} unwrap_{}(Option_{} opt);\n",
                    c_type, inner_type, inner_type
                ));
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

        // Generate unwrap function implementations for all Option types that are used
        if !self.unwrap_needed.is_empty() {
            full_impl.push_str("// Unwrap function implementations\n");
            full_impl.push_str(
                "// These functions extract values from Option types or panic if None\n\n",
            );

            for inner_type in &self.unwrap_needed {
                // Determine the C type for the unwrap return value
                // This should match the type used in the Option_T value field
                // Use the same logic as Option generation:
                // - primitives use value type
                // - non-primitives use pointer type
                let c_type = if self.is_primitive_type(inner_type) {
                    self.map_type_name(inner_type)
                } else {
                    // Non-primitive: use pointer (match Option value field type)
                    format!("{}*", inner_type)
                };

                // Generate the unwrap function for this type
                // unwrap_String(Option_String opt) -> char*
                full_impl.push_str(&format!(
                    "{} unwrap_{}(Option_{} opt) {{\n",
                    c_type, inner_type, inner_type
                ));
                full_impl.push_str(&format!(
                    "    if (opt.tag == Option_{}_tag_none) {{\n",
                    inner_type
                ));
                full_impl.push_str("        panic(\"Unwrapped a None value!\");\n");
                full_impl.push_str("    }\n");
                full_impl.push_str("    return opt.value;\n");
                full_impl.push_str("}\n\n");
            }
        }

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
