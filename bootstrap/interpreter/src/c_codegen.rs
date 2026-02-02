use parser::{Ast, Definition, Type};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

use crate::c_codegen_patch::{TypeKind, TypeNameRegistry};

/// Main entry point for C transpilation
pub fn transpile_to_c(asts: &[Ast], output_dir: &Path) -> Result<(), String> {
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
                let is_simple_enum = !u.cases.iter().any(|c| c.r#type.is_some());
                if is_simple_enum {
                    codegen.generate_definition(def)?;
                }
            } else if let Definition::Pub(p) = def {
                if let Definition::Union(u) = &*p.item {
                    let is_simple_enum = !u.cases.iter().any(|c| c.r#type.is_some());
                    if is_simple_enum {
                        codegen.generate_definition(def)?;
                    }
                }
            }
        }
    }

    // Then generate structs and tagged unions in topologically sorted order
    let ordered_types = codegen.topological_sort_types()?;

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
    /// Track which types are tagged unions (should be pointers)
    tagged_union_types: HashSet<String>,
    /// Track which types have been generated
    generated_types: HashSet<String>,
    /// Track generic instantiations needed (e.g., "List_Arg" -> ["List", "Arg"])
    generic_instances: HashMap<String, (String, Vec<String>)>,
    /// Current function return type (for context-sensitive code generation)
    current_return_type: Option<Type>,
    /// Track function signatures for overload resolution: name -> list of (param_types, mangled_name)
    function_signatures: HashMap<String, Vec<(Vec<String>, String)>>,
    /// Track function return types: func_name (can be mangled) -> return_type
    function_return_types: HashMap<String, Type>,
    /// Track function return types by signature: func_name -> list of (param_types, return_type)
    function_return_by_signature: HashMap<String, Vec<(Vec<String>, Type)>>,
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
    /// Track variant types to their parent tagged unions: variant_name (e.g., "Lit_Bool") -> set of union names (e.g., {"Expression"})
    /// A variant can belong to multiple unions (e.g., "Ident" is in both Expression and SwitchLabel)
    variant_to_union: HashMap<String, HashSet<String>>,
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
            tagged_union_types: HashSet::new(),
            generated_types: HashSet::new(),
            generic_instances: {
                let mut instances = HashMap::new();
                // Pre-populate List_Int since it's used for empty list literals and integer list literals
                instances.insert(
                    "List_Int".to_string(),
                    ("List".to_string(), vec!["Int".to_string()]),
                );
                instances
            },
            current_return_type: None,
            function_signatures: HashMap::new(),
            function_return_types: HashMap::new(),
            function_return_by_signature: HashMap::new(),
            type_name_registry: TypeNameRegistry::new(),
            type_dependencies: HashMap::new(),
            type_definitions: HashMap::new(),
            variable_name_mappings: HashMap::new(),
            all_functions: Vec::new(),
            enum_variants: HashMap::new(),
            unwrap_needed: HashSet::new(),
            variable_types: HashMap::new(),
            variant_to_union: HashMap::new(),
        }
    }

    /// First pass: collect type information
    fn collect_definition(&mut self, def: &Definition) -> Result<(), String> {
        match def {
            Definition::Struct(s) => {
                let c_name = self
                    .type_name_registry
                    .register_type(&s.sig.name, TypeKind::Struct);
                self.seen_types.insert(c_name);
            }
            Definition::Union(u) => {
                // Check if this is a symbolic-only union (simple enum)
                // A union is a tagged union if ANY case has a type associated with it
                let has_typed_variants = u.cases.iter().any(|c| c.r#type.is_some());

                let kind = if has_typed_variants {
                    TypeKind::TaggedUnion
                } else {
                    TypeKind::Enum
                };

                let c_name = self.type_name_registry.register_type(&u.sig.name, kind);
                self.seen_types.insert(c_name.clone());

                if !has_typed_variants {
                    self.enum_types.insert(c_name);
                } else {
                    self.tagged_union_types.insert(c_name.clone());

                    // Track variant types to their parent union for constructor generation
                    for case in &u.cases {
                        if let Some(ty) = &case.r#type {
                            let variant_name = self.type_name_for_instance(ty);
                            eprintln!(
                                "DEBUG: Registering variant '{}' -> union '{}'",
                                variant_name, c_name
                            );
                            self.variant_to_union
                                .entry(variant_name)
                                .or_insert_with(HashSet::new)
                                .insert(c_name.clone());
                        }
                    }
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
                    .push((param_types.clone(), f.name.clone())); // Initially store unmangled name

                // Store the return type by signature for overloaded function lookup
                if let Some(ref ret_ty) = f.r#type {
                    self.function_return_types
                        .insert(f.name.clone(), ret_ty.clone());

                    // Also store by signature for overload resolution
                    self.function_return_by_signature
                        .entry(f.name.clone())
                        .or_insert_with(Vec::new)
                        .push((param_types, ret_ty.clone()));
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
            }
            Definition::Union(u) => {
                let blitz_name = u.sig.name.clone();
                let has_typed_variants = u.cases.iter().any(|c| c.r#type.is_some());
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
            // Check if this type has been renamed due to collision
            // If there's a collision and one version is an enum, prefer the enum version
            // for use in generic types like Option(T) and List(T)
            let name = &ty.name;

            // Check if there's a collision suffix version that's an enum
            let suffixed_name = format!("{}_1", name);
            if self.enum_types.contains(&suffixed_name) {
                // Prefer the enum version for generic type instantiation
                return suffixed_name;
            }

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
    /// This includes simple enum types since they are value types
    fn is_primitive_type(&self, type_name: &str) -> bool {
        // Check basic primitives
        if matches!(
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
        ) {
            return true;
        }
        // Enum types are also primitives (value types, not pointers)
        self.enum_types.contains(type_name)
    }

    /// Check if a type is a built-in type defined in blitz_types.h
    fn is_builtin_type(&self, type_name: &str) -> bool {
        matches!(
            type_name,
            "Range" | "List_Rune" | "Option_String" | "List_String"
        )
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
        // Use the more robust infer_expr_type which checks variable_types
        self.infer_expr_type(expr)
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
            // Helper to strip pointer notation for type comparison
            fn strip_ptr(s: &str) -> &str {
                s.trim_end_matches('*').trim_end_matches("Ptr")
            }

            // If there's only one signature, no mangling needed
            if signatures.len() == 1 {
                return func_name.to_string();
            }

            // Multiple signatures - try to match by argument types (ignoring pointer notation)
            for (param_types, _) in signatures {
                if param_types.len() == arg_types.len() {
                    let matches = param_types
                        .iter()
                        .zip(arg_types.iter())
                        .all(|(p, a)| strip_ptr(p) == strip_ptr(a));
                    if matches {
                        // Found match - return mangled name
                        return self.mangle_function_name(func_name, param_types);
                    }
                }
            }

            // No match found - try to find a signature with matching argument count
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
                let has_typed_variants = u.cases.iter().any(|c| c.r#type.is_some());

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

                // Helper for common fields (like span) in tagged unions
                if has_typed_variants && (union_name == "Expression" || union_name == "Statement") {
                    self.generate_union_span_helper(&c_name, &u.cases)?;
                }
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

        // Check if this is a purely symbolic union (no typed variants)
        let has_typed_variants = cases.iter().any(|c| c.r#type.is_some());

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
                // For variant names, use the full monomorphized name if the type has parameters
                // e.g., Lit(Bool) -> Lit_Bool, not just Lit
                let variant_name = if let Some(label) = &case.label {
                    label.clone()
                } else if let Some(ty) = &case.r#type {
                    self.type_name_for_instance(ty)
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
            let typed_cases: Vec<&parser::Case> =
                cases.iter().filter(|c| c.r#type.is_some()).collect();
            if !typed_cases.is_empty() {
                self.header.push_str("    union {\n");
                for case in typed_cases {
                    // For typed cases, use the full monomorphized name if the type has parameters
                    // e.g., Lit(Bool) -> as_Lit_Bool, not just as_Lit
                    let variant_name = if let Some(label) = &case.label {
                        label.clone()
                    } else if let Some(ty) = &case.r#type {
                        self.type_name_for_instance(ty)
                    } else {
                        // Should be unreachable given filter
                        "unknown".to_string()
                    };

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

    fn generate_union_span_helper(
        &mut self,
        c_name: &str,
        cases: &[parser::Case],
    ) -> Result<(), String> {
        // Generate helper function to access span from tagged union
        // Span* Expression_span(Expression* expr)

        let sig = format!("Span* {}_span({}* expr);", c_name, c_name);
        if !self.header.contains(&sig) {
            self.header.push_str(&sig);
            self.header.push_str("\n\n");
        }

        // Types that DON'T have a span field (need special handling)
        let types_without_span = ["Assignment", "Expression", "Statement"];

        // Types that are Lit variants (have span as Span* pointer, not value)
        let is_lit_type = |name: &str| name.starts_with("Lit_");

        let mut code = format!("Span* {}_span({}* expr) {{\n", c_name, c_name);
        code.push_str("    if (!expr) return NULL;\n");
        code.push_str(&format!("    switch (expr->tag) {{\n"));

        for case in cases {
            let variant_name = if let Some(label) = &case.label {
                label.clone()
            } else if let Some(ty) = &case.r#type {
                self.type_name_for_instance(ty)
            } else {
                continue;
            };

            code.push_str(&format!("        case {}_tag_{}:\n", c_name, variant_name));

            if let Some(variant_type) = &case.r#type {
                let type_name = &variant_type.name;
                let c_type = self.map_type(variant_type);

                // Check if this type doesn't have a span field
                if types_without_span.contains(&type_name.as_str()) {
                    if type_name == "Expression" {
                        // For Expression variant in Statement, recurse to Expression_span
                        code.push_str(&format!(
                            "            return Expression_span(expr->data.as_{});\n",
                            variant_name
                        ));
                    } else {
                        // Assignment doesn't have span - return NULL or get from left expression
                        // left is Expression* (not Expression**), so no dereference needed
                        code.push_str(&format!(
                            "            return Expression_span(expr->data.as_{}->left);\n",
                            variant_name
                        ));
                    }
                } else if is_lit_type(&variant_name) {
                    // Lit variants store span as Span* pointer, access directly (no &)
                    code.push_str(&format!(
                        "            return expr->data.as_{}.span;\n",
                        variant_name
                    ));
                } else if c_type.ends_with("*") {
                    // Pointer type - access with ->
                    code.push_str(&format!(
                        "            return expr->data.as_{}->span;\n",
                        variant_name
                    ));
                } else {
                    // Value type - access with . and take address (span is Span value)
                    code.push_str(&format!(
                        "            return &expr->data.as_{}.span;\n",
                        variant_name
                    ));
                }
            } else {
                code.push_str("            return NULL;\n");
            }
        }

        code.push_str("        default: return NULL;\n");
        code.push_str("    }\n");
        code.push_str("}\n\n");

        self.impl_code.push_str(&code);
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

                            // If returning an Option type, we might need to wrap the result
                            if ret_type.starts_with("Option_") {
                                // Check if it's already an Option (by variable type)
                                // If the switch result is a pointer (from 'some' branch), wrap it?
                                // No, generate_switch_as_statement handles the assignment types

                                // Just return the temp var
                                switch_code.push_str(&format!("\n    return {};", temp_var));
                            } else {
                                switch_code.push_str(&format!("\n    return {};", temp_var));
                            }
                            switch_code
                        } else {
                            let mut expr_code = self.generate_expression(expr, is_main);

                            // Auto-wrap return value if needed (Ptr -> Option)
                            if let Some(ref ret_ty) = func.r#type {
                                let c_ret_ty = self.map_type(ret_ty);
                                if c_ret_ty.starts_with("Option_") {
                                    // Check if the expression already returns an Option type
                                    let expr_type = self.infer_expr_type(expr);

                                    // Only wrap if the expression doesn't already return an Option
                                    if !expr_type.starts_with("Option_") {
                                        // It might be a pointer being returned where Option is expected
                                        // Wrap it: (Option_T){.tag = Option_T_tag_some, .value = expr}
                                        let inner_val = expr_code.clone();
                                        expr_code = format!(
                                            "({}){{.tag = {}_tag_some, .value = {}}}",
                                            c_ret_ty, c_ret_ty, inner_val
                                        );
                                    }
                                }
                            }
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
                // Special handling for switches used as pure statements
                // When a switch is the entire statement (not part of a larger expression),
                // it should be generated as a pure statement without the _switch_result wrapper
                if let parser::Expression::Switch(switch_expr) = expr {
                    return self.generate_switch_as_pure_statement(switch_expr, is_main);
                }

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

                // Agent 1 Fix: Check if we are unwrapping an Option
                // If the init expr is a call to 'unwrap', infer_expr_type returns the inner type (T or T*)
                // But map_type(&decl.r#type) might have returned Option_T if the type was explicit.
                // However, here we handle the case where type was inferred OR explicit.

                // If init is unwrap(), infer_expr_type already returns the unwrapped type.
                // We should ensure that if we have an explicit type, it matches what we expect.
                // But actually, the issue described is: "infer_expr_type knows the correct type, but variable declarations don't use it to fix LHS types."

                // If we have an init expression, we should generally trust infer_expr_type over map_type for the *initializer's* type,
                // BUT we must respect the explicit type annotation if present.
                // The issue specifically mentions unwrapping.

                if let Some(init_expr) = &decl.init {
                    let inferred_type = self.infer_expr_type(init_expr);

                    // If explicit type is Option_T but inferred type is T* (because of unwrap),
                    // we have a mismatch if we stick to explicit type.
                    // But wait, if the user wrote `let x: Option(T) = opt.unwrap()`, that's a type error in Blitz.
                    // If they wrote `let x = opt.unwrap()`, then c_type was inferred from init_expr, so it should be correct (T*).

                    // The problem might be when map_type returns Option_T for `let x: T = ...`? No.

                    // Let's look at the "Specifics": "If infer_expr_type returns a pointer type, use that. If it returns Option_T, check if we are unwrapping."

                    // If explicit type is missing (c_type was inferred), we are good.
                    // If explicit type is present, we should use it?
                    // Maybe the issue is when infer_expr_type returns a pointer (T*) but we somehow defaulted to Option_T?

                    // Actually, if decl.r#type IS provided, map_type uses it.
                    // If the user code is correct, the explicit type matches the init expression.
                    // If `let x: T = opt.unwrap()`, map_type(T) -> T*. infer(unwrap) -> T*. Match.

                    // What if `let x = opt.unwrap()`?
                    // c_type comes from infer_expr_type(unwrap) -> T*.
                    // So `T* x = unwrap(...)`. This seems correct.

                    // What if `let x = opt` where opt is Option(T)?
                    // infer -> Option_T. `Option_T x = opt`. Correct.

                    // The issue description says: "Variables are declared as Option_T instead of T* when unwrap happens."
                    // This implies that for `let x = unwrap(...)`, we are getting Option_T.
                    // This means `infer_expr_type` for unwrap call might be returning Option_T incorrectly?
                    // Let's check `infer_expr_type` for Call "unwrap".
                    // It returns `self.map_type_name(inner)`.
                    // map_type_name returns T* for structs.
                    // So infer_expr_type seems correct.

                    // Re-reading Agent 1 task: "Modify generate_statement... If init involves unwrapping... ensure x is declared as the unwrapped type"
                    // Perhaps the issue is when `c_type` is NOT empty/inferred, i.e. when it IS explicit?
                    // Or maybe there's a case where `infer_expr_type` is not being called?

                    // Ah, `c_type` is initialized with `self.map_type(&decl.r#type)`.
                    // If `decl.r#type` is empty/infer, `map_type` returns "_" or similar?
                    // In `map_type`: if name is "Void" -> "void".
                    // If it's a generic parameter?

                    // If the parser produces `Type { name: "", ... }` for inferred types, map_type might behave oddly.
                    // But the code checks `c_type.is_empty() || c_type == "_"`.

                    // Let's ensure we use the inferred type if it's more specific/pointer-y than the explicit type?
                    // No, that's dangerous.

                    // Let's just forcefully update c_type if it looks wrong for an unwrap.
                    if let parser::Expression::Call(call) = init_expr {
                        if call.name == "unwrap" {
                            let inferred = self.infer_expr_type(init_expr);
                            if inferred != c_type && !c_type.is_empty() && c_type != "_" {
                                // If we have an explicit type but it conflicts with inferred unwrap type
                                // This is technically a type mismatch in source, but maybe we should trust inferred for C gen?
                                // e.g. if user wrote `let x: any = unwrap(...)` and we mapped `any` to something else?

                                // Actually, if c_type is Option_... and inferred is T*, we definitely want T*.
                                if c_type.starts_with("Option_") && !inferred.starts_with("Option_")
                                {
                                    eprintln!("DEBUG: Fixing variable declaration type mismatch for unwrapped value. Declared: {}, Inferred: {}", c_type, inferred);
                                    c_type = inferred;
                                }
                            }
                        }
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
        self.qualify_identifier_with_hint(ident_name, None)
    }

    /// Check if an identifier might be an enum variant and qualify it, with optional type hint
    /// The type_hint helps disambiguate when the same variant name exists in multiple enums
    fn qualify_identifier_with_hint(&self, ident_name: &str, type_hint: Option<&str>) -> String {
        // First, check if this identifier is a known variable (parameter or local)
        // If it is, it should NOT be qualified as an enum variant
        if self.variable_name_mappings.contains_key(ident_name) {
            // This is a variable in scope, not an enum variant
            return ident_name.to_string();
        }

        // If we have a type hint, try to find the variant in that enum first
        if let Some(hint) = type_hint {
            // Extract the base enum name from hints like "Option_Assignment_1" or "Assignment_1"
            let enum_name = hint
                .strip_prefix("Option_")
                .unwrap_or(hint)
                .trim_end_matches('*');

            // First try exact match
            if let Some(variants) = self.enum_variants.get(enum_name) {
                if variants.contains(ident_name) {
                    eprintln!(
                        "DEBUG qualify_identifier_with_hint: '{}' found in hinted enum '{}', returning '{}'",
                        ident_name,
                        enum_name,
                        format!("{}_{}", enum_name, ident_name)
                    );
                    return format!("{}_{}", enum_name, ident_name);
                }
            }

            // Try with collision suffixes (e.g., Assignment -> Assignment_1)
            for suffix in 1..=5 {
                let suffixed_name = format!("{}_{}", enum_name, suffix);
                if let Some(variants) = self.enum_variants.get(&suffixed_name) {
                    if variants.contains(ident_name) {
                        eprintln!(
                            "DEBUG qualify_identifier_with_hint: '{}' found in suffixed enum '{}', returning '{}'",
                            ident_name,
                            suffixed_name,
                            format!("{}_{}", suffixed_name, ident_name)
                        );
                        return format!("{}_{}", suffixed_name, ident_name);
                    }
                }
            }
        }

        // Check all enum types to see if this identifier is a variant
        for (enum_name, variants) in &self.enum_variants {
            if variants.contains(ident_name) {
                eprintln!(
                    "DEBUG qualify_identifier: '{}' found in enum '{}', returning '{}'",
                    ident_name,
                    enum_name,
                    format!("{}_{}", enum_name, ident_name)
                );
                return format!("{}_{}", enum_name, ident_name);
            }
        }

        // Debug: print when identifier is NOT found in any enum
        if ident_name == "int"
            || ident_name == "float"
            || ident_name == "char"
            || ident_name == "ch"
        {
            eprintln!(
                "DEBUG qualify_identifier: '{}' NOT FOUND in any enum. Enums available: {:?}",
                ident_name,
                self.enum_variants.keys().collect::<Vec<_>>()
            );
        }

        // Check for special reserved identifier mappings for enum variants
        if ident_name == "for_" {
            // Check if TokenKind exists and has for_ variant
            if let Some(variants) = self.enum_variants.get("TokenKind") {
                if variants.contains("for_") {
                    return "TokenKind_for_".to_string();
                }
            }
        }
        if ident_name == "mut_" {
            // Check if TokenKind exists and has mut_ variant
            if let Some(variants) = self.enum_variants.get("TokenKind") {
                if variants.contains("mut_") {
                    return "TokenKind_mut_".to_string();
                }
            }
        }
        if ident_name == "true_" {
            if let Some(variants) = self.enum_variants.get("TokenKind") {
                if variants.contains("true_") {
                    return "TokenKind_true_".to_string();
                }
            }
        }
        if ident_name == "false_" {
            if let Some(variants) = self.enum_variants.get("TokenKind") {
                if variants.contains("false_") {
                    return "TokenKind_false_".to_string();
                }
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
                // Infer the actual type that would be generated by this constructor
                // First, get the monomorphized type name
                let mut type_name = self.type_name_for_instance(&ctor.r#type);

                // Special case: Lit without type parameter - infer from value
                if type_name == "Lit" {
                    for arg in &ctor.args {
                        if arg.label.name == "value" {
                            let value_type = self.infer_expr_type(&arg.init);
                            type_name = match value_type.as_str() {
                                "bool" => "Lit_Bool".to_string(),
                                "int64_t" => "Lit_Int".to_string(),
                                "double" => "Lit_Float".to_string(),
                                "char*" => "Lit_String".to_string(),
                                "Rune" | "uint32_t" => "Lit_Rune".to_string(),
                                _ => "Lit_Bool".to_string(),
                            };
                            break;
                        }
                    }
                }

                // Check if this type is a variant of a tagged union
                // If so, the constructor returns a pointer to the union type
                if let Some(union_set) = self.variant_to_union.get(&type_name) {
                    let union_name = union_set.iter().next().cloned().unwrap();
                    format!("{}*", union_name)
                } else {
                    // Regular constructor returns pointer to the type itself
                    format!("{}*", type_name)
                }
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

                // Collect argument types for overload resolution
                let arg_types: Vec<String> = call
                    .args
                    .iter()
                    .map(|arg| self.infer_expr_type(&arg.init))
                    .collect();

                // Try to look up return type by signature match (for overloaded functions)
                if let Some(signatures) = self.function_return_by_signature.get(&call.name) {
                    // Helper to strip pointer notation for type comparison
                    fn strip_ptr(s: &str) -> &str {
                        s.trim_end_matches('*').trim_end_matches("Ptr")
                    }

                    // Find matching signature
                    for (param_types, ret_type) in signatures {
                        if param_types.len() == arg_types.len() {
                            let matches = param_types
                                .iter()
                                .zip(arg_types.iter())
                                .all(|(p, a)| strip_ptr(p) == strip_ptr(a));
                            if matches {
                                return self.map_type(ret_type);
                            }
                        }
                    }
                }

                // Fall back to looking up by function name (for non-overloaded functions)
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
                    "parse_member_call" => return "Option_Expression".to_string(),

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
                            // Check if it's a primitive type that doesn't use pointers
                            if self.is_primitive_type(inner) {
                                self.map_type_name(inner)
                            } else {
                                // The .value field of Option always holds pointers for struct/enum types
                                format!("{}*", inner)
                            }
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
                    // Struct field types - these are pointer fields
                    "range" => return "Range*".to_string(),
                    "span" => return "Span*".to_string(),
                    "path" => return "char*".to_string(),
                    "name" => return "char*".to_string(),
                    "kind" => return "TokenKind".to_string(),
                    "value" => {
                        // For Option types, return the inner type
                        let parent_type = self.infer_expr_type(&member.parent);
                        if parent_type.starts_with("Option_") {
                            let inner = parent_type.trim_start_matches("Option_");
                            return format!("{}*", inner);
                        }
                        return "void*".to_string();
                    }
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

                // Check if this identifier is an enum variant
                for (enum_name, variants) in &self.enum_variants {
                    if variants.contains(&ident.name) {
                        return enum_name.clone();
                    }
                }

                // Default fallback for unknown identifiers
                "int64_t".to_string()
            }
            parser::Expression::Switch(switch_expr) => {
                // Infer type from the first non-else case body's last expression
                for case in &switch_expr.cases {
                    if !case.body.is_empty() {
                        if let parser::Statement::Expression(last_expr) = case.body.last().unwrap()
                        {
                            let inferred = self.infer_expr_type(last_expr);
                            // If we got a valid type (not the fallback), return it
                            if inferred != "int64_t" {
                                // Check if this is an Option of an Expression variant
                                // Expression variants: For, While, If, Switch, Return, etc.
                                let expression_variants = [
                                    "Option_For",
                                    "Option_While",
                                    "Option_If",
                                    "Option_Switch",
                                    "Option_Return",
                                    "Option_Continue",
                                    "Option_Break",
                                    "Option_Assert",
                                    "Option_Block",
                                    "Option_Ident",
                                    "Option_Call",
                                    "Option_Member",
                                    "Option_Index",
                                    "Option_BinaryOp",
                                    "Option_UnaryOp",
                                    "Option_Constructor",
                                    "Option_Group",
                                    "Option_List_",
                                    "Option_Lit",
                                    "Option_Mut",
                                    "Option_Assignment",
                                ];
                                if expression_variants.iter().any(|v| inferred.starts_with(v)) {
                                    // This is an Option of an expression variant, unify to Option_Expression
                                    return "Option_Expression".to_string();
                                }
                                return inferred;
                            }
                        }
                    }
                }
                // Fallback to int64_t since we couldn't infer a more specific type
                "int64_t".to_string()
            }
            parser::Expression::List(list) => {
                // Infer list type from element types
                if list.elems.is_empty() {
                    // Empty list - can't infer element type
                    return "List_Int".to_string();
                }

                // Check if all elements are numbers
                let all_ints = list
                    .elems
                    .iter()
                    .all(|e| matches!(e, parser::Expression::Number(_)));
                if all_ints {
                    return "List_Int".to_string();
                }

                // Check if all elements are identifiers (possibly enum variants)
                let all_idents = list
                    .elems
                    .iter()
                    .all(|e| matches!(e, parser::Expression::Ident(_)));
                if all_idents {
                    if let parser::Expression::Ident(first_ident) = &list.elems[0] {
                        // Find which enum this identifier belongs to
                        for (enum_name, variants) in &self.enum_variants {
                            if variants.contains(&first_ident.name) {
                                return format!("List_{}", enum_name);
                            }
                        }
                    }
                }

                // Fallback to generic list
                "List_Int".to_string()
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
                            // Try to use current_return_type if available
                            let ret_type = if let Some(ref ty) = self.current_return_type {
                                self.map_type(ty)
                            } else {
                                "int64_t".to_string()
                            };

                            let mut switch_code = self.generate_switch_as_statement(
                                switch_expr,
                                is_main,
                                &ret_type,
                                temp_var,
                            );
                            switch_code.push_str(&format!("\n    return {};", temp_var));
                            switch_code
                        } else {
                            let mut expr_code = self.generate_expression(ret_expr, is_main);

                            // Auto-wrap return value if needed (Ptr -> Option)
                            if let Some(ref ret_ty) = self.current_return_type {
                                let c_ret_ty = self.map_type(ret_ty);
                                if c_ret_ty.starts_with("Option_") {
                                    if !expr_code.contains("Option_")
                                        && !expr_code.contains("_tag_")
                                        && expr_code != "none"
                                    {
                                        // Wrap it
                                        let inner_val = expr_code.clone();
                                        expr_code = format!(
                                            "({}){{.tag = {}_tag_some, .value = {}}}",
                                            c_ret_ty, c_ret_ty, inner_val
                                        );
                                    }
                                }
                            }

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
                                    "({{ {} _opt = {}; if (_opt.tag == {}) {{ {}; }} _opt.value; }})",
                                    left_type, left, tag_name, right
                                );
                            } else {
                                let left = self.generate_expression(&binop.left, is_main);
                                let right = self.generate_expression(&binop.right, is_main);

                                // Need to wrap result in 'some' constructor if the result is assigned to an Option
                                // But here we're unwrapping, so we return the value directly
                                return format!(
                                    "({{ {} _opt = {}; _opt.tag == {} ? ({}) : _opt.value; }})",
                                    left_type, left, tag_name, right
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

                // Try to find the function signature to guide argument generation
                // Use the first signature found (overloading might make this inexact but it helps)
                // CLONE the signature to avoid borrowing self while calling generate_expression later
                let signature = self
                    .function_signatures
                    .get(func_name)
                    .and_then(|sigs| sigs.first())
                    .cloned();

                // Generate arguments
                let args: Vec<String> = call
                    .args
                    .iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        // For now, ignore labels (named arguments) - just use the expression
                        let mut arg_code = self.generate_expression(&arg.init, is_main);

                        // Auto-unwrap Option -> Ptr if needed to match function signature
                        if let Some((param_types, _)) = &signature {
                            if i < param_types.len() {
                                let param_type = &param_types[i];
                                // Check if param is Ptr type and arg is Option
                                // param_type is a Blitz type signature (e.g., "Ident", not "Ident*")
                                // A struct type in Blitz becomes a pointer in C
                                let param_is_ptr_type = param_type.ends_with("*")
                                    || (self.seen_types.contains(param_type)
                                        && !self.enum_types.contains(param_type))
                                    || self.tagged_union_types.contains(param_type);

                                if param_is_ptr_type {
                                    // Check if arg is Option type
                                    let arg_type = self.infer_expr_type(&arg.init);
                                    if arg_type.starts_with("Option_") {
                                        // It's an Option, but we need a Ptr.
                                        // Assume we want the value (implicit unwrap for C interop/transpilation)
                                        // Check if we haven't already unwrapped it manually (unlikely in arg_code)
                                        if !arg_code.ends_with(".value") {
                                            arg_code = format!("{}.value", arg_code);
                                        }
                                    }
                                }
                            }
                        }
                        arg_code
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

                // For constructors, we need the monomorphized struct name (e.g., Lit_Bool not Lit)
                // but not the pointer type that map_type would give us
                let mut type_name = self.type_name_for_instance(&ctor.r#type);

                // Special case: Lit(value: ...) without type parameter
                // Infer the type parameter from the value argument
                if type_name == "Lit" {
                    // Find the 'value' argument and infer its type
                    for arg in &ctor.args {
                        if arg.label.name == "value" {
                            let value_type = self.infer_expr_type(&arg.init);
                            type_name = match value_type.as_str() {
                                "bool" => "Lit_Bool".to_string(),
                                "int64_t" => "Lit_Int".to_string(),
                                "double" => "Lit_Float".to_string(),
                                "char*" => "Lit_String".to_string(),
                                "Rune" | "uint32_t" => "Lit_Rune".to_string(),
                                _ => {
                                    eprintln!(
                                        "WARNING: Cannot infer Lit type from value type '{}'",
                                        value_type
                                    );
                                    "Lit_Bool".to_string() // Default fallback
                                }
                            };
                            break;
                        }
                    }
                }

                // Generate field initializers
                let mut field_inits = Vec::new();
                for arg in &ctor.args {
                    let field_name = &arg.label.name;

                    // Check if this is an empty list - we need to determine the correct type
                    let mut field_value = if let parser::Expression::List(list) = &*arg.init {
                        if list.elems.is_empty() {
                            // Empty list - try to infer the correct List type from the struct field
                            // For common patterns, use hardcoded mappings
                            let list_type = match (type_name.as_str(), field_name.as_str()) {
                                ("Block", "statements") | (_, "statements") => "List_Statement",
                                ("SwitchCase", "body") | (_, "body") => "List_Statement",
                                ("Fn", "args") | (_, "args") => "List_Arg",
                                ("Struct", "fields") | (_, "fields") => "List_Field",
                                ("Union", "cases") | (_, "cases") => "List_Case",
                                ("Call", "args") => "List_CallArg",
                                (_, "elems") => "List_Expression",
                                _ => "List_Int", // Default fallback
                            };
                            format!("({}){{.data = NULL, .len = 0, .cap = 0}}", list_type)
                        } else {
                            self.generate_expression(&arg.init, is_main)
                        }
                    } else {
                        self.generate_expression(&arg.init, is_main)
                    };

                    // Auto-unwrap Option types for struct fields that expect pointers
                    // If the field value has Option type but the field likely expects a pointer,
                    // append .value to unwrap
                    let field_type = self.infer_expr_type(&arg.init);
                    if field_type.starts_with("Option_") && !field_value.ends_with(".value") {
                        // Check if the inner type is a struct (would be a pointer in C)
                        let inner_type = field_type.strip_prefix("Option_").unwrap_or(&field_type);
                        let inner_is_struct = self.seen_types.contains(inner_type)
                            && !self.enum_types.contains(inner_type);
                        let inner_is_tagged_union = self.tagged_union_types.contains(inner_type);

                        // For now, always unwrap Option types in constructor fields
                        // This assumes the Blitz code is correct and Options are used where values are expected
                        if inner_is_struct || inner_is_tagged_union {
                            field_value = format!("{}.value", field_value);
                        }
                    }

                    field_inits.push(format!(".{} = {}", field_name, field_value));
                }

                // Check if this is a variant of a tagged union (like Lit_Bool -> Expression)
                // Note: a variant can belong to multiple unions, we pick the first one
                if let Some(union_set) = self.variant_to_union.get(&type_name) {
                    let union_name = union_set.iter().next().cloned().unwrap();
                    // This is a tagged union variant - wrap in the union struct
                    eprintln!(
                        "DEBUG Constructor: type_name={} is variant of union {}, generating union wrapper",
                        type_name, union_name
                    );

                    // Determine if this variant's field in the union is a pointer type
                    // Non-parameterized types (like Index, Mut) that are registered structs become pointers
                    // Parameterized types (like Lit_Bool) are stored as values
                    let variant_is_pointer = self.seen_types.contains(&type_name)
                        && !self.enum_types.contains(&type_name);

                    let variant_literal = format!("({}){{{}}}", type_name, field_inits.join(", "));

                    let variant_assignment = if variant_is_pointer {
                        // Need to heap-allocate the variant data: memcpy(malloc(sizeof(T)), &T{...}, sizeof(T))
                        format!(
                            "memcpy(malloc(sizeof({})), &{}, sizeof({}))",
                            type_name, variant_literal, type_name
                        )
                    } else {
                        // Can use the variant literal directly (value type in union)
                        variant_literal
                    };

                    let union_literal = format!(
                        "(({}){{.tag = {}_tag_{}, .data.as_{} = {}}})",
                        union_name, union_name, type_name, type_name, variant_assignment
                    );

                    return format!(
                        "memcpy(malloc(sizeof({})), &{}, sizeof({}))",
                        union_name, union_literal, union_name
                    );
                }

                // Check if we need to return a pointer to this struct
                // This happens when the struct type is known and not an enum
                let needs_pointer =
                    self.seen_types.contains(&type_name) && !self.enum_types.contains(&type_name);

                eprintln!(
                    "DEBUG Constructor: type_name={}, seen_types.contains={}, enum_types.contains={}, needs_pointer={}",
                    type_name,
                    self.seen_types.contains(&type_name),
                    self.enum_types.contains(&type_name),
                    needs_pointer
                );

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

                // Special case: .span access on Expression/Statement tagged unions
                // These don't have a direct .span field, we need to use the helper function
                if member_name == "span" {
                    // Check if parent type is Expression or Statement (inferred or known)
                    // infer_expr_type returns things like "Option_Expression", "Expression*"
                    let parent_type = self.infer_expr_type(&member.parent);

                    // Strip pointer/Option to get base type
                    let base_type = parent_type
                        .trim_end_matches('*')
                        .trim_start_matches("Option_");

                    if base_type == "Expression" || base_type == "Statement" {
                        // Use helper function: Expression_span(expr)
                        // If parent_type is Option, we need to unwrap/access value first
                        if parent_type.starts_with("Option_") {
                            return format!("{}_span({}.value)", base_type, parent_code);
                        }
                        return format!("{}_span({})", base_type, parent_code);
                    }
                }

                // Special case: Option types need .value to access members of inner type
                // Use type inference to detect Option types reliably
                let parent_type = self.infer_expr_type(&member.parent);
                let parent_is_option = parent_type.starts_with("Option_");

                // For C struct member access, we need to decide between '.' and '->'
                // Since most structs are passed as pointers in our C codegen (see map_type),
                // we should use '->' for struct types

                // For C struct member access, we need to decide between '.' and '->'
                // Use the inferred type to make this decision more reliably
                let parent_is_pointer = parent_type.ends_with('*');

                let separator = if parent_is_pointer {
                    // Parent type is a pointer (like Token*), use ->
                    "->"
                } else if parent_code.starts_with('&') {
                    // Taking address of something, result is pointer-like but accessed with .
                    "."
                } else if parent_code.contains("->") {
                    // Already dereferenced a pointer, result is value, use .
                    "."
                } else if parent_is_option {
                    // Option types are structs, use . to access .value
                    "."
                } else if parent_code.ends_with("]") {
                    // Array access returns value/struct
                    "."
                } else if parent_code.ends_with(")") && !parent_is_pointer {
                    // Function call returning struct (not pointer), use .
                    "."
                } else if parent_code.starts_with('*') {
                    // Dereferenced pointer: (*ptr).field can be simplified to ptr->field
                    "->"
                } else {
                    // Default: use arrow operator for pointer-based struct access
                    "->"
                };

                if parent_is_option {
                    // Auto-unwrap Option types: opt.field -> opt.value->field
                    // Check if the value field is a pointer or struct
                    // If inner type is a struct/pointer, use ->, else use .
                    // But wait, our Option types for structs store T* or T?
                    // List, Box, etc are structs/pointers. Primitives are values.

                    // We can assume that if we are accessing a member, the inner type MUST be a struct/pointer
                    // (primitives don't have members, except maybe methods which are UFCS)
                    // So .value is likely a pointer (if it's a user struct) or a struct (if List)

                    // If inner type is List, .value is List_T (struct). So .value.field
                    // If inner type is Expression*, .value is Expression*. So .value->field

                    if parent_type.contains("List_") || parent_type.contains("Range") {
                        format!("{}.value.{}", parent_code, member_name)
                    } else {
                        // Assume pointer for other user types
                        format!("{}.value->{}", parent_code, member_name)
                    }
                } else if parent_code.starts_with('*') {
                    format!("{}->{}", parent_code.trim_start_matches('*'), member_name)
                } else {
                    format!("{}{}{}", parent_code, separator, member_name)
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
                    // Also check if the target variable itself is a List type
                    if let parser::Expression::Ident(ident) = &*idx.target {
                        let var_type = if let Some(t) = self.variable_types.get(&ident.name) {
                            t.clone()
                        } else {
                            self.get_variable_name(&ident.name)
                                .split('_')
                                .next()
                                .unwrap_or("")
                                .to_string()
                        };

                        if var_type.starts_with("List_")
                            || var_type == "String"
                            || var_type == "char*"
                        {
                            // List types and Strings are accessed differently
                            if var_type == "String" || var_type == "char*" {
                                format!("{}[{}]", target_expr, index_expr)
                            } else {
                                format!("{}.data[{}]", target_expr, index_expr)
                            }
                        } else {
                            format!("{}[{}]", target_expr, index_expr)
                        }
                    } else {
                        // Simple array or direct identifier indexing
                        format!("{}[{}]", target_expr, index_expr)
                    }
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

                // Get the lval type from variable_types
                let lval_var_name = match &assign.left {
                    parser::Lval::Ident(ident) => Some(self.get_variable_name(&ident.name)),
                    _ => None,
                };
                let lval_type = lval_var_name
                    .as_ref()
                    .and_then(|name| self.variable_types.get(name))
                    .cloned();

                // Get the rhs type
                let rhs_type = self.infer_expr_type(&assign.right);
                let rhs = self.generate_expression(&assign.right, is_main);

                // Check if we need to convert Option_X to Y* where X is a variant of Y
                if let Some(lval_t) = &lval_type {
                    // lval_t might be "Expression*"
                    let lval_base = lval_t.trim_end_matches('*');

                    // Check if rhs_type is Option_X where X is a variant of lval_base
                    if rhs_type.starts_with("Option_") {
                        let option_inner = &rhs_type[7..]; // Remove "Option_" prefix

                        // Check if option_inner is a variant of lval_base
                        if let Some(parent_unions) = self.variant_to_union.get(option_inner) {
                            if parent_unions.contains(lval_base) {
                                // Need to: unwrap the Option, then wrap in the parent union
                                // Generate: memcpy(malloc(sizeof(Y)), &((Y){.tag = Y_tag_X, .data.as_X = rhs.value}), sizeof(Y))
                                return format!(
                                    "{} = memcpy(malloc(sizeof({})), &(({}){{.tag = {}_tag_{}, .data.as_{} = {}.value}}), sizeof({}))",
                                    lval_code, lval_base, lval_base, lval_base, option_inner, option_inner, rhs, lval_base
                                );
                            }
                        }

                        // Also check if option_inner IS the lval_base (e.g., Option_Expression to Expression*)
                        if option_inner == lval_base {
                            // Just unwrap the option
                            return format!("{} = {}.value", lval_code, rhs);
                        }
                    }
                } else {
                    // lval_type is not known, try to infer from the lval expression
                    // For simple identifiers like 'lhs', try to get the mapped variable name and look it up
                    if let parser::Lval::Ident(ident) = &assign.left {
                        let var_name = self.get_variable_name(&ident.name);
                        if let Some(lval_t) = self.variable_types.get(&var_name) {
                            let lval_base = lval_t.trim_end_matches('*');

                            if rhs_type.starts_with("Option_") {
                                let option_inner = &rhs_type[7..];

                                // Check if option_inner IS the lval_base
                                if option_inner == lval_base {
                                    return format!("{} = {}.value", lval_code, rhs);
                                }

                                // Check if option_inner is a variant of lval_base
                                if let Some(parent_unions) = self.variant_to_union.get(option_inner)
                                {
                                    if parent_unions.contains(lval_base) {
                                        return format!(
                                            "{} = memcpy(malloc(sizeof({})), &(({}){{.tag = {}_tag_{}, .data.as_{} = {}.value}}), sizeof({}))",
                                            lval_code, lval_base, lval_base, lval_base, option_inner, option_inner, rhs, lval_base
                                        );
                                    }
                                }
                            }
                        }
                    }
                }

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
                        // Check if all elements are identifiers (possibly enum variants)
                        let all_idents = list
                            .elems
                            .iter()
                            .all(|e| matches!(e, parser::Expression::Ident(_)));

                        if all_idents {
                            // Try to infer the enum type from the first element
                            if let parser::Expression::Ident(first_ident) = &list.elems[0] {
                                // Find which enum this identifier belongs to
                                let mut enum_type: Option<String> = None;
                                for (enum_name, variants) in &self.enum_variants {
                                    if variants.contains(&first_ident.name) {
                                        enum_type = Some(enum_name.clone());
                                        break;
                                    }
                                }

                                if let Some(enum_name) = enum_type {
                                    // Generate array of enum values
                                    code.push_str(&format!(
                                        "        {}* _tmp = malloc(sizeof({}) * {});\n",
                                        enum_name, enum_name, len
                                    ));
                                    for (i, elem) in list.elems.iter().enumerate() {
                                        let elem_code = self.generate_expression(elem, is_main);
                                        code.push_str(&format!(
                                            "        _tmp[{}] = {};\n",
                                            i, elem_code
                                        ));
                                    }
                                    code.push_str(&format!(
                                        "        (List_{}){{.data = _tmp, .len = {}, .cap = {}}};\n",
                                        enum_name, len, len
                                    ));
                                } else {
                                    // Fall back to stub
                                    code.push_str("        /* TODO: non-integer list literal requires type inference */\n");
                                    code.push_str(&format!("        void* _tmp = NULL;\n"));
                                    code.push_str("        (void*){{}};\n");
                                }
                            } else {
                                code.push_str("        /* TODO: non-integer list literal requires type inference */\n");
                                code.push_str(&format!("        void* _tmp = NULL;\n"));
                                code.push_str("        (void*){{}};\n");
                            }
                        } else {
                            // Mixed/complex types - we can't infer the type properly
                            // Just generate a stub for now
                            code.push_str("        /* TODO: non-integer list literal requires type inference */\n");
                            code.push_str(&format!("        void* _tmp = NULL;\n"));
                            code.push_str("        (void*){{}};\n");
                        }
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

    /// Infer the result type of a switch expression from its case bodies
    fn infer_switch_result_type(&self, cases: &[parser::SwitchCase]) -> String {
        let mut collected_types: Vec<String> = Vec::new();

        for case in cases {
            if let Some(parser::Statement::Expression(last_expr)) = case.body.last() {
                // Skip control flow expressions - they don't produce a value
                if matches!(
                    last_expr,
                    parser::Expression::Return(_)
                        | parser::Expression::Break
                        | parser::Expression::Continue
                ) {
                    continue;
                }
                let inferred = self.infer_expr_type(last_expr);
                if inferred != "int64_t" && !inferred.is_empty() {
                    collected_types.push(inferred);
                }
            }
        }

        if collected_types.is_empty() {
            return "int64_t".to_string();
        }

        // If all types are the same, return that
        if collected_types.iter().all(|t| t == &collected_types[0]) {
            return collected_types[0].clone();
        }

        // Check if all types are Option_X where X are variants of the same union
        let option_types: Vec<_> = collected_types
            .iter()
            .filter(|t| t.starts_with("Option_"))
            .map(|t| &t[7..]) // Extract inner type
            .collect();

        if option_types.len() == collected_types.len() && !option_types.is_empty() {
            // All are Option types - try to find common parent union
            eprintln!(
                "DEBUG infer_switch_result_type: all Option types: {:?}",
                option_types
            );
            // Find the intersection of all parent unions
            let mut common_unions: Option<HashSet<String>> = None;
            for inner in &option_types {
                eprintln!("DEBUG: Looking up variant '{}' in variant_to_union", inner);
                if let Some(parents) = self.variant_to_union.get(*inner) {
                    eprintln!("DEBUG: Found parents '{:?}'", parents);
                    match &mut common_unions {
                        None => {
                            common_unions = Some(parents.clone());
                        }
                        Some(common) => {
                            // Keep only unions that are in both
                            *common = common.intersection(parents).cloned().collect();
                        }
                    }
                } else {
                    eprintln!("DEBUG: No parent found for '{}'", inner);
                    common_unions = Some(HashSet::new()); // No common parent
                }
            }
            if let Some(common) = common_unions {
                if common.len() == 1 {
                    let parent = common.iter().next().unwrap();
                    eprintln!("DEBUG: Unified to Option_{}", parent);
                    return format!("Option_{}", parent);
                } else if common.len() > 1 {
                    // Multiple common parents - prefer "Expression" if it's there
                    if common.contains("Expression") {
                        eprintln!("DEBUG: Unified to Option_Expression (preferred)");
                        return "Option_Expression".to_string();
                    }
                    // Otherwise, just pick one
                    let parent = common.iter().next().unwrap();
                    eprintln!("DEBUG: Unified to Option_{} (arbitrary)", parent);
                    return format!("Option_{}", parent);
                }
            }
        }

        // Check for MIXED Option and non-Option types that are all variants of the same union
        // e.g., Option_For + Return* should unify to Option_Expression
        if !option_types.is_empty() && option_types.len() < collected_types.len() {
            eprintln!(
                "DEBUG infer_switch_result_type: mixed Option and non-Option types: {:?}",
                collected_types
            );
            // Get all base types (strip Option_ prefix and * suffix)
            let all_base_types: Vec<&str> = collected_types
                .iter()
                .map(|t| {
                    if t.starts_with("Option_") {
                        &t[7..]
                    } else {
                        t.trim_end_matches('*')
                    }
                })
                .collect();

            // Find common parent union for all base types
            let mut common_unions: Option<HashSet<String>> = None;
            for base in &all_base_types {
                if let Some(parents) = self.variant_to_union.get(*base) {
                    match &mut common_unions {
                        None => {
                            common_unions = Some(parents.clone());
                        }
                        Some(common) => {
                            *common = common.intersection(parents).cloned().collect();
                        }
                    }
                }
            }
            if let Some(common) = common_unions {
                if !common.is_empty() {
                    // Prefer "Expression" if available
                    let parent = if common.contains("Expression") {
                        "Expression"
                    } else {
                        common.iter().next().unwrap()
                    };
                    eprintln!("DEBUG: Mixed types unified to Option_{}", parent);
                    return format!("Option_{}", parent);
                }
            }
        }

        // Check if all types are variants of the same union (non-Option)
        let mut common_unions: Option<HashSet<String>> = None;
        for t in &collected_types {
            let base_type = t.trim_end_matches('*');
            if let Some(parents) = self.variant_to_union.get(base_type) {
                match &mut common_unions {
                    None => {
                        common_unions = Some(parents.clone());
                    }
                    Some(common) => {
                        *common = common.intersection(parents).cloned().collect();
                    }
                }
            }
        }
        if let Some(common) = common_unions {
            if common.len() == 1 {
                let parent = common.iter().next().unwrap();
                return format!("{}*", parent);
            } else if common.len() > 1 && common.contains("Expression") {
                return "Expression*".to_string();
            }
        }

        // Check if it's an Expression variant that should unify to Expression*
        let expression_types = [
            "For*",
            "While*",
            "If*",
            "Return*",
            "Block*",
            "BinaryOp*",
            "UnaryOp*",
            "Call*",
            "Member*",
            "Index*",
            "Constructor*",
            "Mut*",
            "Assignment*",
            "Ident*",
            "Lit_*",
        ];
        for t in &collected_types {
            for etype in expression_types {
                if etype.ends_with("*") && t.starts_with(etype.trim_end_matches('*')) {
                    return "Expression*".to_string();
                }
            }
        }

        // Return first non-trivial type
        collected_types
            .into_iter()
            .next()
            .unwrap_or_else(|| "int64_t".to_string())
    }

    /// Convert an expression of `from_type` to `to_type` if needed.
    /// Returns the converted expression code.
    fn convert_expr_to_type(&self, expr_code: &str, from_type: &str, to_type: &str) -> String {
        // If types are the same, no conversion needed
        if from_type == to_type {
            return expr_code.to_string();
        }

        // Handle Option_X to Option_Y conversions where X is a variant of Y
        if from_type.starts_with("Option_") && to_type.starts_with("Option_") {
            let from_inner = &from_type[7..]; // e.g., "Call"
            let to_inner = &to_type[7..]; // e.g., "Expression"

            // Check if from_inner is a variant of to_inner (a union)
            if let Some(parents) = self.variant_to_union.get(from_inner) {
                if parents.contains(to_inner) {
                    // Convert Option_Call to Option_Expression:
                    // If the source is some, wrap the inner value in the union
                    // We need to evaluate expr_code only once, so we use a statement expression
                    // Pattern:
                    // ({ Option_Call _tmp = expr_code;
                    //    (_tmp.tag == Option_Call_tag_some)
                    //    ? (Option_Expression){.tag = Option_Expression_tag_some, .value = memcpy(malloc(sizeof(Expression)), &((Expression){.tag = Expression_tag_Call, .data.as_Call = _tmp.value}), sizeof(Expression))}
                    //    : (Option_Expression){.tag = Option_Expression_tag_none};
                    // })

                    // Some types are stored by value in the union (not as pointers)
                    // For these, we need to dereference _conv_tmp.value
                    let value_types = [
                        "Lit_Bool",
                        "Lit_String",
                        "Lit_Rune",
                        "Lit_Float",
                        "Lit_Int",
                        "BinaryOperator",
                        "UnaryOperator",
                    ];
                    let value_access = if value_types.contains(&from_inner) {
                        "*_conv_tmp.value" // dereference for value types
                    } else {
                        "_conv_tmp.value" // pointer types use directly
                    };

                    return format!(
                        "({{ {} _conv_tmp = {}; \
                        (_conv_tmp.tag == {}_tag_some) \
                        ? ({}){{.tag = {}_tag_some, .value = memcpy(malloc(sizeof({})), &(({}){{.tag = {}_tag_{}, .data.as_{} = {}}}), sizeof({}))}} \
                        : ({}){{.tag = {}_tag_none}}; }})",
                        from_type, expr_code,
                        from_type,
                        to_type, to_type, to_inner, to_inner, to_inner, from_inner, from_inner, value_access, to_inner,
                        to_type, to_type
                    );
                }
            }
        }

        // Handle non-Option variant to union conversion: X to Y* where X is variant of Y
        let from_base = from_type.trim_end_matches('*');
        let to_base = to_type.trim_end_matches('*');

        if let Some(parents) = self.variant_to_union.get(from_base) {
            if parents.contains(to_base) {
                // Wrap in union: memcpy(malloc(sizeof(Y)), &((Y){.tag = Y_tag_X, .data.as_X = expr}), sizeof(Y))
                return format!(
                    "memcpy(malloc(sizeof({})), &(({}){{.tag = {}_tag_{}, .data.as_{} = {}}}), sizeof({}))",
                    to_base, to_base, to_base, from_base, from_base, expr_code, to_base
                );
            }
        }

        // Handle non-Option to Option conversion: X* to Option_Y where X is variant of Y
        // e.g., Return* to Option_Expression
        if to_type.starts_with("Option_") && !from_type.starts_with("Option_") {
            let to_inner = &to_type[7..]; // e.g., "Expression"

            // Direct case: X* to Option_X (same type, just wrap in Option)
            if from_base == to_inner {
                return format!(
                    "({}){{.tag = {}_tag_some, .value = {}}}",
                    to_type, to_type, expr_code
                );
            }

            // Check if from_base is a variant of to_inner
            if let Some(parents) = self.variant_to_union.get(from_base) {
                if parents.contains(to_inner) {
                    // Wrap in union and then in Option:
                    // (Option_Expression){.tag = Option_Expression_tag_some, .value = memcpy(malloc(sizeof(Expression)), &((Expression){.tag = Expression_tag_Return, .data.as_Return = expr}), sizeof(Expression))}
                    return format!(
                        "({}){{.tag = {}_tag_some, .value = memcpy(malloc(sizeof({})), &(({}){{.tag = {}_tag_{}, .data.as_{} = {}}}), sizeof({}))}}",
                        to_type, to_type, to_inner, to_inner, to_inner, from_base, from_base, expr_code, to_inner
                    );
                }
            }
        }

        // No conversion found, return as-is
        expr_code.to_string()
    }

    /// Check if a switch is used purely as a statement (for side effects, not for a value)
    /// A switch is a pure statement if all cases end with control flow or have empty bodies
    fn is_pure_statement_switch(&self, switch_expr: &parser::Switch) -> bool {
        // Check if all cases either:
        // 1. Have empty bodies
        // 2. End with control flow (continue, break, return)
        for case in &switch_expr.cases {
            if case.body.is_empty() {
                continue; // Empty body is fine for pure statement
            }
            if let Some(last_stmt) = case.body.last() {
                match last_stmt {
                    parser::Statement::Expression(expr) => {
                        if !matches!(
                            expr,
                            parser::Expression::Continue
                                | parser::Expression::Break
                                | parser::Expression::Return(_)
                        ) {
                            // This case produces a value, so it's not a pure statement switch
                            return false;
                        }
                    }
                    parser::Statement::Declaration(_) => {
                        // Declarations don't produce values, so this is fine
                    }
                }
            }
        }
        true
    }

    /// Generate a switch as a pure statement (no _switch_result wrapper)
    fn generate_switch_as_pure_statement(
        &mut self,
        switch_expr: &parser::Switch,
        is_main: bool,
    ) -> String {
        let cond = self.generate_expression(&switch_expr.cond, is_main);
        let cond_type = self.infer_expr_type(&switch_expr.cond);

        // Check if this is an Option switch
        let is_option_switch = switch_expr.cases.iter().any(|case| {
            if let parser::SwitchLabel::Ident(ident) = &case.label {
                ident.name == "some" || ident.name == "none"
            } else {
                false
            }
        });

        let cond_is_option = cond_type.starts_with("Option_");

        if is_option_switch || cond_is_option {
            self.generate_if_else_chain_pure_statement(
                &cond,
                &cond_type,
                &switch_expr.cases,
                is_main,
            )
        } else {
            // Can use C switch for pure statement
            self.generate_c_switch_pure_statement(&cond, &switch_expr.cases, is_main)
        }
    }

    /// Generate C switch as a pure statement (no expression wrapper)
    fn generate_c_switch_pure_statement(
        &mut self,
        cond: &str,
        cases: &[parser::SwitchCase],
        is_main: bool,
    ) -> String {
        let mut code = format!("switch ({}) {{\n", cond);

        for case in cases {
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
                    code.push_str(&format!("        case '{}':\n", escaped));
                }
                parser::SwitchLabel::NumberLit(num) => {
                    if num.value.fract() == 0.0 {
                        code.push_str(&format!("        case {}:\n", num.value as i64));
                    }
                }
                parser::SwitchLabel::Ident(ident) => {
                    if ident.name == "else" {
                        code.push_str("        default:\n");
                    } else {
                        let qualified = self.qualify_identifier(&ident.name);
                        code.push_str(&format!("        case {}:\n", qualified));
                    }
                }
                parser::SwitchLabel::Else(_) => {
                    code.push_str("        default:\n");
                }
                _ => {}
            }

            code.push_str("        {\n");
            for stmt in &case.body {
                let stmt_code = self.generate_statement(stmt, is_main);
                code.push_str("            ");
                code.push_str(&stmt_code);
                code.push_str("\n");
            }
            code.push_str("            break;\n");
            code.push_str("        }\n");
        }

        code.push_str("    }");
        code
    }

    /// Generate if-else chain as a pure statement (no expression wrapper)
    fn generate_if_else_chain_pure_statement(
        &mut self,
        cond: &str,
        cond_type: &str,
        cases: &[parser::SwitchCase],
        is_main: bool,
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
                    parser::SwitchLabel::Ident(ident) => {
                        if ident.name == "some" && cond_type.starts_with("Option_") {
                            format!("{}.tag == {}_tag_some", cond, cond_type)
                        } else if ident.name == "none" && cond_type.starts_with("Option_") {
                            format!("{}.tag == {}_tag_none", cond, cond_type)
                        } else {
                            let qualified = self.qualify_identifier(&ident.name);
                            format!("{} == {}", cond, qualified)
                        }
                    }
                    parser::SwitchLabel::Type(ty) => {
                        if ty.name == "Operator" || ty.name == "BinaryOperator" {
                            let ops = vec![
                                "add", "sub", "mul", "div", "rem", "concat", "dot", "eq", "ne",
                                "gt", "ge", "lt", "le", "and_", "or_", "else_",
                            ];
                            let checks: Vec<String> = ops
                                .iter()
                                .map(|op| format!("{} == TokenKind_{}", cond, op))
                                .collect();
                            checks.join(" || ")
                        } else if ty.name == "UnaryOperator" {
                            format!("{} == TokenKind_not || {} == TokenKind_sub", cond, cond)
                        } else {
                            format!("{}->tag == {}_tag_{}", cond, ty.name, ty.name)
                        }
                    }
                    _ => continue,
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

    fn generate_switch(&mut self, switch_expr: &parser::Switch, is_main: bool) -> String {
        let cond = self.generate_expression(&switch_expr.cond, is_main);
        let cond_type = self.infer_expr_type(&switch_expr.cond);

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

        // Check if this is a switch on Option type (has some/none cases)
        // Options are structs, can't use C switch on them
        let is_option_switch = switch_expr.cases.iter().any(|case| {
            if let parser::SwitchLabel::Ident(ident) = &case.label {
                ident.name == "some" || ident.name == "none"
            } else {
                false
            }
        });

        // Also check if condition type is an Option
        let cond_is_option = cond_type.starts_with("Option_");

        if can_use_c_switch && !is_option_switch && !cond_is_option {
            self.generate_c_switch(&cond, &switch_expr.cases, is_main)
        } else {
            self.generate_if_else_chain(&cond, &cond_type, &switch_expr.cases, is_main)
        }
    }

    fn generate_c_switch(
        &mut self,
        cond: &str,
        cases: &[parser::SwitchCase],
        is_main: bool,
    ) -> String {
        // Infer the result type from the first non-control-flow case body
        let result_type = self.infer_switch_result_type(cases);

        // Wrap switch in a GCC statement expression so it can be used as an expression
        // Pattern: ({ T _switch_result; switch(...) { case: _switch_result = val; break; } _switch_result; })
        let mut code = format!("({{ {} _switch_result; switch (", result_type);
        code.push_str(cond);
        code.push_str(") {\n");

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
                        return self.generate_if_else_chain(cond, "int64_t", cases, is_main);
                    }
                }
                parser::SwitchLabel::Ident(ident) => {
                    // Identifier - could be enum variant or "else"
                    if ident.name == "else" {
                        code.push_str("        default:\n");
                    } else {
                        // Enum variant - qualify it with enum type
                        let qualified = self.qualify_identifier(&ident.name);

                        // Handle bare enum variants like 'none', 'some', 'mut'
                        let final_label = if ident.name == "none" {
                            // Try to infer enum type from condition
                            let cond_type = cond.split("->").next().unwrap_or("").trim();
                            // This is a hacky way to guess the type, but sufficient for simple switches
                            // A better way would be passing down type context
                            if cond_type.contains("Option") {
                                // Assume Option type - e.g. Option_Token_tag_none
                                format!("{}_tag_none", cond_type.replace("*", ""))
                            } else {
                                "default".to_string()
                            }
                        } else if ident.name == "mut_" {
                            "TokenKind_mut_".to_string()
                        } else if ident.name == "for_" {
                            "TokenKind_for_".to_string()
                        } else if ident.name == "some" {
                            // Try to infer enum type from condition
                            let cond_type = cond.split("->").next().unwrap_or("").trim();
                            if cond_type.contains("Option") {
                                // Assume Option type - e.g. Option_Token_tag_some
                                format!("{}_tag_some", cond_type.replace("*", ""))
                            } else {
                                "default".to_string()
                            }
                        } else if ident.name.ends_with("_") {
                            // Check if it's a TokenKind variant
                            format!("TokenKind_{}", ident.name)
                        } else {
                            qualified
                        };

                        if final_label == "default" {
                            code.push_str("        default:\n");
                        } else {
                            code.push_str(&format!("        case {}:\n", final_label));
                        }
                    }
                }
                parser::SwitchLabel::Else(_) => {
                    // "otherwise" variant
                    code.push_str("        default:\n");
                }
                parser::SwitchLabel::Type(_) => {
                    // Type pattern - not supported in simple switch, use if-else chain
                    return self.generate_if_else_chain(cond, "void*", cases, is_main);
                }
                parser::SwitchLabel::StringLit(_) => {
                    // String pattern - not supported in C switch, use if-else chain
                    return self.generate_if_else_chain(cond, "char*", cases, is_main);
                }
            }

            // Generate case body with assignment to _switch_result for last expression
            code.push_str("        {\n");

            if !case.body.is_empty() {
                let last_idx = case.body.len() - 1;
                for (idx, stmt) in case.body.iter().enumerate() {
                    if idx == last_idx {
                        // Last statement - assign to _switch_result if it's an expression
                        match stmt {
                            parser::Statement::Expression(expr) => {
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
                                    let expr_code = self.generate_expression(expr, is_main);
                                    let expr_type = self.infer_expr_type(expr);
                                    let converted = self.convert_expr_to_type(
                                        &expr_code,
                                        &expr_type,
                                        &result_type,
                                    );
                                    code.push_str(&format!(
                                        "            _switch_result = {};\n",
                                        converted
                                    ));
                                }
                            }
                            parser::Statement::Declaration(_) => {
                                let stmt_code = self.generate_statement(stmt, is_main);
                                code.push_str("            ");
                                code.push_str(&stmt_code);
                                code.push_str("\n");
                            }
                        }
                    } else {
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

        code.push_str("    } _switch_result; })");
        code
    }

    fn generate_if_else_chain(
        &mut self,
        cond: &str,
        cond_type: &str,
        cases: &[parser::SwitchCase],
        is_main: bool,
    ) -> String {
        // Infer the result type from the case bodies
        let result_type = self.infer_switch_result_type(cases);

        // Wrap in a GCC statement expression so it can be used as an expression
        let mut code = format!("({{ {} _switch_result; ", result_type);
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
                        // Handle Option variants: some/none
                        if ident.name == "some" && cond_type.starts_with("Option_") {
                            format!("{}.tag == {}_tag_some", cond, cond_type)
                        } else if ident.name == "none" && cond_type.starts_with("Option_") {
                            format!("{}.tag == {}_tag_none", cond, cond_type)
                        } else {
                            // Enum variant comparison - qualify the identifier
                            let qualified = self.qualify_identifier(&ident.name);
                            format!("{} == {}", cond, qualified)
                        }
                    }
                    parser::SwitchLabel::Type(ty) => {
                        // Type pattern - check tag field for tagged unions
                        // Special case: matching Operator type against TokenKind enum
                        // This checks if the TokenKind value is one of the operator variants
                        if ty.name == "Operator" || ty.name == "BinaryOperator" {
                            // Generate check for all binary operator TokenKind variants
                            let ops = vec![
                                "add", "sub", "mul", "div", "rem", "concat", "dot", "eq", "ne",
                                "gt", "ge", "lt", "le", "and_", "or_", "else_",
                            ];
                            let checks: Vec<String> = ops
                                .iter()
                                .map(|op| format!("{} == TokenKind_{}", cond, op))
                                .collect();
                            checks.join(" || ")
                        } else if ty.name == "UnaryOperator" {
                            // Generate check for unary operator TokenKind variants
                            format!("{} == TokenKind_not || {} == TokenKind_sub", cond, cond)
                        } else {
                            // Normal tagged union check
                            format!("{}->tag == {}_tag_{}", cond, ty.name, ty.name)
                        }
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

            // Generate 'it' binding for pattern matches that need it
            // For Option 'some' patterns, 'it' is the unwrapped value
            if let parser::SwitchLabel::Ident(ident) = &case.label {
                if ident.name == "some" && cond_type.starts_with("Option_") {
                    // Extract inner type from Option_X
                    let inner_type = &cond_type[7..]; // e.g., "Assignment" from "Option_Assignment"
                    let c_inner_type = self.map_type_name(inner_type);
                    code.push_str(&format!("        {} it = {}.value;\n", c_inner_type, cond));
                    // Register 'it' in variable_types for this scope
                    self.variable_types
                        .insert("it".to_string(), c_inner_type.clone());
                }
            }
            // For Type patterns, 'it' is the matched value itself
            if let parser::SwitchLabel::Type(ty) = &case.label {
                if ty.name == "UnaryOperator" {
                    // For TokenKind matching UnaryOperator, 'it' is the TokenKind value
                    code.push_str(&format!("        TokenKind it = {};\n", cond));
                    self.variable_types
                        .insert("it".to_string(), "TokenKind".to_string());
                } else if ty.name == "Operator" || ty.name == "BinaryOperator" {
                    // For TokenKind matching BinaryOperator, 'it' is the TokenKind value
                    code.push_str(&format!("        TokenKind it = {};\n", cond));
                    self.variable_types
                        .insert("it".to_string(), "TokenKind".to_string());
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
                                    let expr_type = self.infer_expr_type(expr);
                                    let converted = self.convert_expr_to_type(
                                        &expr_code,
                                        &expr_type,
                                        &result_type,
                                    );
                                    code.push_str(&format!(
                                        "            _switch_result = {};\n",
                                        converted
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

    /// Generate switch expression as a statement with assignment to variable
    /// This is the main entry point for switch-as-statement generation
    fn generate_switch_as_statement(
        &mut self,
        switch_expr: &parser::Switch,
        is_main: bool,
        var_type: &str,
        var_name: &str,
    ) -> String {
        let cond = self.generate_expression(&switch_expr.cond, is_main);
        let cond_type = self.infer_expr_type(&switch_expr.cond);

        // Check if this is an Option switch (has some/none cases)
        let is_option_switch = switch_expr.cases.iter().any(|case| {
            if let parser::SwitchLabel::Ident(ident) = &case.label {
                ident.name == "some" || ident.name == "none"
            } else {
                false
            }
        });

        // Also check if condition type is an Option
        let cond_is_option = cond_type.starts_with("Option_");

        // Check if we can use a C switch (only for simple cases without Option or Type patterns)
        let can_use_c_switch = !is_option_switch
            && !cond_is_option
            && switch_expr.cases.iter().all(|case| {
                matches!(
                    case.label,
                    parser::SwitchLabel::CharLit(_)
                        | parser::SwitchLabel::NumberLit(_)
                        | parser::SwitchLabel::Ident(_)
                        | parser::SwitchLabel::Else(_)
                )
            });

        // Start with variable declaration
        let mut code = format!("{} {};\n    ", var_type, var_name);

        // Register the variable type for type inference in later expressions
        self.variable_types
            .insert(var_name.to_string(), var_type.to_string());

        // For Option conditions that are function calls, we need a temp variable
        // to avoid calling the function multiple times
        let (actual_cond, needs_temp) = if cond_is_option {
            // Check if cond looks like a function call (contains parentheses)
            if cond.contains('(') && !cond.starts_with('(') {
                // Store in temp variable
                code = format!(
                    "{} _opt_cond = {};\n    {} {};\n    ",
                    cond_type, cond, var_type, var_name
                );
                ("_opt_cond".to_string(), true)
            } else {
                (cond.clone(), false)
            }
        } else {
            (cond.clone(), false)
        };

        if can_use_c_switch {
            code.push_str(&self.generate_c_switch_as_statement(
                &actual_cond,
                &cond_type,
                &switch_expr.cases,
                is_main,
                var_name,
                var_type,
            ));
        } else {
            code.push_str(&self.generate_if_else_chain_as_statement(
                &actual_cond,
                &cond_type,
                &switch_expr.cases,
                is_main,
                var_name,
                var_type,
            ));
        }

        code
    }

    /// Generate C switch statement form with assignments to variable
    fn generate_c_switch_as_statement(
        &mut self,
        cond: &str,
        cond_type: &str,
        cases: &[parser::SwitchCase],
        is_main: bool,
        var_name: &str,
        var_type: &str,
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
                    code.push_str(&format!("        case '{}':\n", escaped));
                }
                parser::SwitchLabel::NumberLit(num) => {
                    if num.value.fract() == 0.0 {
                        code.push_str(&format!("        case {}:\n", num.value as i64));
                    }
                }
                parser::SwitchLabel::Ident(ident) => {
                    if ident.name == "else" {
                        code.push_str("        default:\n");
                    } else {
                        let qualified = self.qualify_identifier(&ident.name);
                        code.push_str(&format!("        case {}:\n", qualified));
                    }
                }
                parser::SwitchLabel::Else(_) => {
                    code.push_str("        default:\n");
                }
                _ => {}
            }

            code.push_str("        {\n");

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
                                    code.push_str("            ");
                                    code.push_str(&stmt_code);
                                    code.push_str("\n");
                                } else {
                                    // For simple identifier expressions, use type hint for enum disambiguation
                                    let expr_code = if let parser::Expression::Ident(ident) = expr {
                                        // Use var_type as hint for enum variant disambiguation
                                        let var_name_local = self.get_variable_name(&ident.name);
                                        if var_name_local == ident.name {
                                            // Try to qualify as enum variant with type hint
                                            self.qualify_identifier_with_hint(
                                                &ident.name,
                                                Some(var_type),
                                            )
                                        } else {
                                            var_name_local
                                        }
                                    } else {
                                        self.generate_expression(expr, is_main)
                                    };
                                    let expr_type = self.infer_expr_type(expr);
                                    let converted =
                                        self.convert_expr_to_type(&expr_code, &expr_type, var_type);
                                    code.push_str(&format!(
                                        "            {} = {};\n",
                                        var_name, converted
                                    ));
                                }
                            }
                            parser::Statement::Declaration(_) => {
                                let stmt_code = self.generate_statement(stmt, is_main);
                                code.push_str("            ");
                                code.push_str(&stmt_code);
                                code.push_str("\n");
                            }
                        }
                    } else {
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
        cond_type: &str,
        cases: &[parser::SwitchCase],
        is_main: bool,
        var_name: &str,
        var_type: &str,
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
                        // Handle Option variants: some/none
                        if ident.name == "some" && cond_type.starts_with("Option_") {
                            format!("{}.tag == {}_tag_some", cond, cond_type)
                        } else if ident.name == "none" && cond_type.starts_with("Option_") {
                            format!("{}.tag == {}_tag_none", cond, cond_type)
                        } else {
                            // Qualify enum variants
                            let qualified = self.qualify_identifier(&ident.name);
                            format!("{} == {}", cond, qualified)
                        }
                    }
                    parser::SwitchLabel::Type(ty) => {
                        // Type pattern - check tag field for tagged unions
                        // Special case: matching Operator type against TokenKind enum
                        if ty.name == "Operator" || ty.name == "BinaryOperator" {
                            let ops = vec![
                                "add", "sub", "mul", "div", "rem", "concat", "dot", "eq", "ne",
                                "gt", "ge", "lt", "le", "and_", "or_", "else_",
                            ];
                            let checks: Vec<String> = ops
                                .iter()
                                .map(|op| format!("{} == TokenKind_{}", cond, op))
                                .collect();
                            checks.join(" || ")
                        } else if ty.name == "UnaryOperator" {
                            format!("{} == TokenKind_not || {} == TokenKind_sub", cond, cond)
                        } else {
                            format!("{}->tag == {}_tag_{}", cond, ty.name, ty.name)
                        }
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

            // Generate 'it' binding for pattern matches that need it
            // For Option 'some' patterns, 'it' is the unwrapped value
            if let parser::SwitchLabel::Ident(ident) = &case.label {
                if ident.name == "some" && cond_type.starts_with("Option_") {
                    // Extract inner type from Option_X
                    let inner_type = &cond_type[7..]; // e.g., "Assignment" from "Option_Assignment"
                    let c_inner_type = self.map_type_name(inner_type);
                    code.push_str(&format!("        {} it = {}.value;\n", c_inner_type, cond));
                }
            }
            // For Type patterns, 'it' is the matched value itself
            if let parser::SwitchLabel::Type(ty) = &case.label {
                if ty.name == "UnaryOperator" {
                    // For TokenKind matching UnaryOperator, 'it' is the TokenKind value
                    code.push_str(&format!("        TokenKind it = {};\n", cond));
                } else if ty.name == "Operator" || ty.name == "BinaryOperator" {
                    // For TokenKind matching BinaryOperator, 'it' is the TokenKind value
                    code.push_str(&format!("        TokenKind it = {};\n", cond));
                }
            }

            // Generate case body with assignment
            if !case.body.is_empty() {
                let last_idx = case.body.len() - 1;

                for (idx, stmt) in case.body.iter().enumerate() {
                    if idx == last_idx {
                        eprintln!(
                            "DEBUG: last_stmt is_expr={}, is_decl={}",
                            matches!(stmt, parser::Statement::Expression(_)),
                            matches!(stmt, parser::Statement::Declaration(_))
                        );
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
                                    let expr_type = self.infer_expr_type(expr);

                                    eprintln!("DEBUG if_else_chain_as_statement: var_name='{}', var_type='{}', expr_type='{}'", var_name, var_type, expr_type);

                                    // Check if we need to unwrap an Option type
                                    let should_unwrap = expr_type.starts_with("Option_")
                                        && !var_type.starts_with("Option_");

                                    if expr_type == "Option_Expression" {
                                        eprintln!("DEBUG: should_unwrap={}, expr_code first 200 chars: {}", should_unwrap, &expr_code[..std::cmp::min(200, expr_code.len())]);
                                    }

                                    let final_expr = if should_unwrap {
                                        // Unwrap the option and get the inner type
                                        let inner_type = &expr_type[7..]; // Strip "Option_"
                                        let inner_type_ptr = format!("{}*", inner_type);
                                        let unwrapped = format!("{}.value", expr_code);

                                        // Now convert the unwrapped value to var_type if needed
                                        self.convert_expr_to_type(
                                            &unwrapped,
                                            &inner_type_ptr,
                                            var_type,
                                        )
                                    } else {
                                        // No unwrap needed, but may still need type conversion
                                        self.convert_expr_to_type(&expr_code, &expr_type, var_type)
                                    };

                                    code.push_str(&format!(
                                        "        {} = {};\n",
                                        var_name, final_expr
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
                // For other types, check if it's a struct type or tagged union (should be pointer)
                // or an enum type (should be value)
                if self.seen_types.contains(type_name) && !self.enum_types.contains(type_name) {
                    format!("{}*", type_name)
                } else if self.tagged_union_types.contains(type_name) {
                    format!("{}*", type_name)
                } else {
                    type_name.to_string()
                }
            }
        }
    }

    /// Maps a type to its C representation as a value type (no automatic pointer for structs).
    /// This is used when we want the raw type name, e.g., for Box(T) where Box adds its own pointer.
    fn map_type_as_value(&self, ty: &Type) -> String {
        match ty.name.as_str() {
            "Int" => "int64_t".to_string(),
            "Bool" => "bool".to_string(),
            "Float" => "double".to_string(),
            "String" => "char*".to_string(),
            "Rune" => "uint32_t".to_string(),
            "Void" => "void".to_string(),
            "Box" => {
                // Box(T) becomes T* - recursively use value mapping
                if ty.params.len() == 1 {
                    format!("{}*", self.map_type_as_value(&ty.params[0]))
                } else {
                    "void*".to_string()
                }
            }
            _ => {
                // For non-parameterized types, just return the name without pointer
                if ty.params.is_empty() {
                    ty.name.clone()
                } else {
                    // Monomorphize: Option(Type) -> Option_Type, etc.
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
                // We use map_type_as_value to avoid double-pointer for struct/union types
                // e.g., Box(Expression) should be Expression*, not Expression**
                if ty.params.len() == 1 {
                    format!("{}*", self.map_type_as_value(&ty.params[0]))
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

                    // Check if this is a known struct type or tagged union (not enum) - if so, use pointer for forward-declaration safety
                    if (self.seen_types.contains(&c_name) && !self.enum_types.contains(&c_name))
                        || self.tagged_union_types.contains(&c_name)
                    {
                        format!("{}*", c_name)
                    } else {
                        c_name
                    }
                } else {
                    // Monomorphize: Option(Type) -> Option_Type, Lit(Bool) -> Lit_Bool
                    let mono_name = format!(
                        "{}_{}",
                        ty.name,
                        ty.params
                            .iter()
                            .map(|p| self.type_name_for_instance(p))
                            .collect::<Vec<_>>()
                            .join("_")
                    );

                    // For Option types, don't add pointer - they're value types
                    // For List types, don't add pointer - they're value types (contains a pointer inside)
                    // For other monomorphized types that are variants of tagged unions,
                    // we need to match the union field type (which uses map_type during generation)
                    // Since Lit_Bool etc. are NOT registered as structs in seen_types,
                    // they won't be treated as pointers here - this is correct for union field types
                    mono_name
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

// String concatenation - creates a new string from two input strings
char* blitz_string_concat(char* a, char* b);

// Todo function - marks unimplemented code, panics at runtime
void todo(char* msg) __attribute__((noreturn));

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
                        // Fallback - check if it's a struct type that needs a pointer
                        if (self.seen_types.contains(param_type)
                            && !self.enum_types.contains(param_type)
                            && !self.is_primitive_type(param_type))
                            || self.tagged_union_types.contains(param_type)
                        {
                            format!("{}*", param_type)
                        } else {
                            param_type.clone()
                        }
                    };
                    full_header.push_str(&format!(
                        "typedef struct {} {{\n    {}* data;\n    size_t len;\n    size_t cap;\n}} {};\n\n",
                        instance_name, c_type, instance_name
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

        // Built-in runtime function stub implementations
        full_impl.push_str(
            "// ============================================================================\n",
        );
        full_impl.push_str("// Built-in runtime function implementations\n");
        full_impl.push_str(
            "// ============================================================================\n\n",
        );

        // blitz_string_concat implementation
        full_impl.push_str("char* blitz_string_concat(char* a, char* b) {\n");
        full_impl.push_str("    if (!a && !b) return strdup(\"\");\n");
        full_impl.push_str("    if (!a) return strdup(b);\n");
        full_impl.push_str("    if (!b) return strdup(a);\n");
        full_impl.push_str("    size_t len_a = strlen(a);\n");
        full_impl.push_str("    size_t len_b = strlen(b);\n");
        full_impl.push_str("    char* result = (char*)malloc(len_a + len_b + 1);\n");
        full_impl.push_str("    memcpy(result, a, len_a);\n");
        full_impl.push_str("    memcpy(result + len_a, b, len_b + 1);\n");
        full_impl.push_str("    return result;\n");
        full_impl.push_str("}\n\n");

        // todo implementation
        full_impl.push_str("void todo(char* msg) {\n");
        full_impl
            .push_str("    fprintf(stderr, \"TODO: %s\\n\", msg ? msg : \"not implemented\");\n");
        full_impl.push_str("    abort();\n");
        full_impl.push_str("}\n\n");

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
