use parser::{Ast, Definition, Test, Type};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

use crate::c_codegen_patch::{TypeKind, TypeNameRegistry};

/// Macro for debug output that's only shown when the debug flag is set.
/// Use like: `debug_println!(self, "message {}", value);`
macro_rules! debug_println {
    ($self:expr, $($arg:tt)*) => {
        if $self.debug {
            eprintln!($($arg)*);
        }
    };
}

/// Main entry point for C transpilation (without tests)
pub fn transpile_to_c(asts: &[Ast], output_dir: &Path) -> Result<(), String> {
    transpile_to_c_impl(asts, output_dir, false)
}

/// Transpile to C with test support - generates test functions and a test runner main
pub fn transpile_to_c_with_tests(asts: &[Ast], output_dir: &Path) -> Result<(), String> {
    transpile_to_c_impl(asts, output_dir, true)
}

/// Extract the module name from a file path.
/// The module is the immediate parent directory of the file.
/// e.g., "../../compiler/hir/expression.blitz" -> "hir"
/// e.g., "../../compiler/main.blitz" -> "compiler"
/// e.g., "hir/lower.blitz" -> "hir"
fn module_from_path(path: &str) -> String {
    let path = std::path::Path::new(path);
    path.parent()
        .and_then(|p| p.file_name())
        .and_then(|n| n.to_str())
        .unwrap_or("_root")
        .to_string()
}

/// Implementation for C transpilation with optional test support
fn transpile_to_c_impl(asts: &[Ast], output_dir: &Path, include_tests: bool) -> Result<(), String> {
    let mut codegen = CCodegen::new(output_dir, include_tests);

    // First pass: collect all type definitions
    for ast in asts {
        codegen.current_module = module_from_path(&ast.path);
        for def in &ast.defs {
            codegen.collect_definition(def)?;
        }
    }

    // Register variant-to-union mappings after all types have been collected.
    // This ensures type_name_for_instance returns consistent collision-resolved names.
    for ast in asts {
        codegen.current_module = module_from_path(&ast.path);
        for def in &ast.defs {
            codegen.register_variants(def)?;
        }
    }

    // Second pass: analyze types to find generic instantiations needed
    for ast in asts {
        codegen.current_module = module_from_path(&ast.path);
        for def in &ast.defs {
            codegen.analyze_definition(def)?;
        }
    }

    // Third pass: build dependency graph for type definitions
    for ast in asts {
        codegen.current_module = module_from_path(&ast.path);
        for def in &ast.defs {
            codegen.build_type_dependencies(def)?;
        }
    }

    // Fourth pass: generate code in dependency order
    // First generate all enums (simple unions) - they have no dependencies
    for ast in asts {
        codegen.current_module = module_from_path(&ast.path);
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
        // Set the module context for this type definition
        if let Some(module) = codegen.c_name_to_module.get(&type_name).cloned() {
            codegen.current_module = module;
        }
        // Clone the definition to avoid borrow checker issues
        if let Some(def) = codegen.type_definitions.get(&type_name).cloned() {
            codegen.generate_definition(&def)?;
        }
    }

    // Finally generate functions and other non-type definitions
    for ast in asts {
        codegen.current_module = module_from_path(&ast.path);
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

    // Generate test functions if in test mode
    if include_tests {
        for ast in asts {
            codegen.current_module = module_from_path(&ast.path);
            for def in &ast.defs {
                if let Definition::Test(_) = def {
                    codegen.generate_definition(def)?;
                }
            }
        }
    }

    // Check for accumulated codegen errors before writing output
    if !codegen.codegen_errors.is_empty() {
        eprintln!();
        for err in &codegen.codegen_errors {
            eprintln!("\x1b[91;1merror:\x1b[0m {}", err);
        }
        eprintln!();
        return Err(format!(
            "{} error{} detected during transpilation",
            codegen.codegen_errors.len(),
            if codegen.codegen_errors.len() == 1 {
                ""
            } else {
                "s"
            }
        ));
    }

    // Write output files (test mode writes test runner main instead of user main)
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
    /// Track which list append functions need to be generated (element_type_name)
    list_append_needed: HashSet<String>,
    /// Track which list concat functions need to be generated (element_type_name)
    list_concat_needed: HashSet<String>,
    /// Track variable types in current function scope (variable_name -> c_type)
    variable_types: HashMap<String, String>,
    /// Track variant types to their parent tagged unions: variant_name (e.g., "Lit_Bool") -> set of union names (e.g., {"Expression"})
    /// A variant can belong to multiple unions (e.g., "Ident" is in both Expression and SwitchLabel)
    variant_to_union: HashMap<String, HashSet<String>>,
    /// Track struct field types: struct_name -> field_name -> field_type (Blitz type name)
    /// Used for type-aware code generation (e.g., qualifying enum variants correctly)
    struct_field_types: HashMap<String, HashMap<String, String>>,
    /// Track label-only variants in tagged unions: union_name -> set of label names
    /// These are variants without associated data (e.g., "default" in SwitchLabel)
    tagged_union_labels: HashMap<String, HashSet<String>>,
    /// Track functions that return void (no return type in Blitz source)
    void_functions: HashSet<String>,
    /// Stack of loop labels for break statements - when inside a switch inside a loop,
    /// break should goto the loop exit label, not break the switch
    loop_label_stack: Vec<String>,
    /// Counter for generating unique loop labels
    loop_label_counter: usize,
    /// Track if we're currently inside a switch statement (for break semantics)
    in_switch_depth: usize,
    /// Whether to include test code generation
    include_tests: bool,
    /// Collected test definitions for test mode
    test_definitions: Vec<Test>,
    /// Track which list equality functions need to be generated (element_type_name)
    list_eq_needed: HashSet<String>,
    /// Expected list element type for list literal generation (used in comparisons)
    expected_list_elem_type: Option<String>,
    /// Expected type for the current expression being generated (set by outer constructor when
    /// generating field values). Used to decide whether to wrap a constructor in its parent
    /// tagged union or produce a bare struct. For example, when generating `Ident(...)` as
    /// a field value for `TypeDef.name` (which is type `Ident`), this is set to `"Ident"` so
    /// the Ident constructor produces a bare `Ident*` rather than wrapping in `SwitchLabel`.
    expected_expr_type: Option<String>,
    /// Track which display functions need to be generated for assertion output (c_type_name)
    display_needed: HashSet<String>,
    /// Whether to show debug output during code generation
    debug: bool,
    /// Track mutability flags for function parameters: func_name -> list of (param_type_sigs, mut_flags)
    /// Used to pass value-type mut params by pointer and dereference them in the body
    function_mut_params: HashMap<String, Vec<(Vec<String>, Vec<bool>)>>,
    /// Track which variables in the current function scope are mut-pointer params
    /// (i.e., originally value-type params marked `mut` that we pass as pointers)
    /// Maps mangled variable name -> original C type (before adding pointer)
    mut_pointer_params: HashMap<String, String>,
    /// Accumulated codegen errors (e.g., unknown struct fields, undefined variables).
    /// Collected during code generation and reported before C compilation.
    codegen_errors: Vec<String>,
    /// Current module name (derived from AST path, e.g., "ast", "hir", "parser")
    /// Set before processing each AST's definitions to provide module context.
    current_module: String,
    /// Map from C type name to the module it was defined in.
    /// Used to set current_module when generating types in topological sort order.
    c_name_to_module: HashMap<String, String>,
}

impl CCodegen {
    fn new(output_dir: &Path, include_tests: bool) -> Self {
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
            list_append_needed: HashSet::new(),
            list_concat_needed: HashSet::new(),
            variable_types: HashMap::new(),
            variant_to_union: HashMap::new(),
            struct_field_types: HashMap::new(),
            tagged_union_labels: HashMap::new(),
            void_functions: HashSet::new(),
            loop_label_stack: Vec::new(),
            loop_label_counter: 0,
            in_switch_depth: 0,
            include_tests,
            test_definitions: Vec::new(),
            list_eq_needed: HashSet::new(),
            expected_list_elem_type: None,
            expected_expr_type: None,
            display_needed: HashSet::new(),
            debug: std::env::var("BLITZ_DEBUG").is_ok(),
            function_mut_params: HashMap::new(),
            mut_pointer_params: HashMap::new(),
            codegen_errors: Vec::new(),
            current_module: String::new(),
            c_name_to_module: HashMap::new(),
        }
    }

    /// First pass: collect type information
    fn collect_definition(&mut self, def: &Definition) -> Result<(), String> {
        match def {
            Definition::Struct(s) => {
                let c_name = self.type_name_registry.register_type(
                    &s.sig.name,
                    TypeKind::Struct,
                    &self.current_module,
                );
                self.seen_types.insert(c_name.clone());
                self.c_name_to_module
                    .insert(c_name.clone(), self.current_module.clone());

                // Collect field types for this struct
                let mut field_types = HashMap::new();
                for field in &s.fields {
                    // Store the Blitz type name for each field
                    let field_type_name = self.type_signature(&field.r#type);
                    field_types.insert(field.name.clone(), field_type_name);
                }
                self.struct_field_types.insert(c_name, field_types);
            }
            Definition::Union(u) => {
                // Skip the Bool union - it's defined in std but we use C's native bool type
                if u.sig.name == "Bool" {
                    return Ok(());
                }

                // Check if this is a symbolic-only union (simple enum)
                // A union is a tagged union if ANY case has a type associated with it
                let has_typed_variants = u.cases.iter().any(|c| c.r#type.is_some());

                let kind = if has_typed_variants {
                    TypeKind::TaggedUnion
                } else {
                    TypeKind::Enum
                };

                let c_name =
                    self.type_name_registry
                        .register_type(&u.sig.name, kind, &self.current_module);
                self.seen_types.insert(c_name.clone());
                self.c_name_to_module
                    .insert(c_name.clone(), self.current_module.clone());

                if !has_typed_variants {
                    self.enum_types.insert(c_name);
                } else {
                    self.tagged_union_types.insert(c_name.clone());

                    // Note: variant registration is deferred to register_variants()
                    // which is called after all types have been collected.
                    // This ensures type_name_for_instance returns consistent results.
                }
            }
            Definition::Fn(f) => {
                // Collect function signatures for overload detection
                let param_types: Vec<String> = f
                    .args
                    .iter()
                    .map(|arg| self.type_signature(&arg.r#type))
                    .collect();

                // Collect mutability flags for each parameter
                let mut_flags: Vec<bool> = f.args.iter().map(|arg| arg.mutable).collect();

                // Store mutability info alongside param types
                self.function_mut_params
                    .entry(f.name.clone())
                    .or_insert_with(Vec::new)
                    .push((param_types.clone(), mut_flags));

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
                } else {
                    // Function has no return type - it returns void
                    self.void_functions.insert(f.name.clone());
                }
            }
            Definition::Pub(p) => {
                self.collect_definition(&p.item)?;
            }
            Definition::Test(test) => {
                // Collect tests if in test mode
                if self.include_tests {
                    self.test_definitions.push(test.clone());
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Register variant types to their parent unions.
    /// This must be called AFTER all types have been collected so that
    /// type_name_for_instance returns consistent collision-resolved names.
    fn register_variants(&mut self, def: &Definition) -> Result<(), String> {
        match def {
            Definition::Union(u) => {
                // Skip the Bool union - it's defined in std but we use C's native bool type
                if u.sig.name == "Bool" {
                    return Ok(());
                }

                // Skip generic unions - their label-only variants (like `none` in `Option(T)`)
                // can't be resolved without knowing the concrete type parameter
                if !u.sig.params.is_empty() {
                    debug_println!(
                        self,
                        "DEBUG: Skipping generic union '{}' with params {:?}",
                        u.sig.name,
                        u.sig.params
                    );
                    return Ok(());
                }

                let has_typed_variants = u.cases.iter().any(|c| c.r#type.is_some());
                if has_typed_variants {
                    let c_name = self.type_name_registry.register_type(
                        &u.sig.name,
                        TypeKind::TaggedUnion,
                        &self.current_module,
                    );

                    // Track variant types to their parent union for constructor generation
                    for case in &u.cases {
                        if let Some(ty) = &case.r#type {
                            let variant_name = self.type_name_for_instance(ty);
                            debug_println!(
                                self,
                                "DEBUG: Registering variant '{}' -> union '{}'",
                                variant_name,
                                c_name
                            );
                            self.variant_to_union
                                .entry(variant_name)
                                .or_insert_with(HashSet::new)
                                .insert(c_name.clone());
                        } else {
                            // This is a label-only variant (no associated data)
                            // Track it for identifier resolution
                            // The variant name comes from the label field
                            if let Some(label) = &case.label {
                                debug_println!(
                                    self,
                                    "DEBUG: Registering label-only variant '{}' -> union '{}'",
                                    label,
                                    c_name
                                );
                                self.tagged_union_labels
                                    .entry(c_name.clone())
                                    .or_insert_with(HashSet::new)
                                    .insert(label.clone());
                            }
                        }
                    }
                }
            }
            Definition::Pub(p) => {
                self.register_variants(&p.item)?;
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
                // Skip the Bool union - it's defined in std but we use C's native bool type
                if u.sig.name == "Bool" {
                    return Ok(());
                }

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
            Definition::Test(test) => {
                // Analyze types in test body if in test mode
                if self.include_tests {
                    for stmt in &test.body {
                        self.analyze_statement(stmt);
                    }
                }
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
                let c_name = self.type_name_registry.register_type(
                    &blitz_name,
                    TypeKind::Struct,
                    &self.current_module,
                );

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
                // Skip the Bool union - it's defined in std but we use C's native bool type
                if u.sig.name == "Bool" {
                    return Ok(());
                }

                let blitz_name = u.sig.name.clone();
                let has_typed_variants = u.cases.iter().any(|c| c.r#type.is_some());
                let kind = if has_typed_variants {
                    TypeKind::TaggedUnion
                } else {
                    TypeKind::Enum
                };
                let c_name =
                    self.type_name_registry
                        .register_type(&blitz_name, kind, &self.current_module);

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
                debug_println!(
                    self,
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

    /// Register that we need a list append function for the given element type.
    /// This also ensures the corresponding List_<elem_type> is registered as a generic instance.
    fn register_list_append_needed(&mut self, elem_type: String) {
        // Strip pointer suffix - list type names must be valid C identifiers (e.g. "TypeRef" not "TypeRef*")
        let elem_type = elem_type.trim_end_matches('*').to_string();

        // Add to list_append_needed
        self.list_append_needed.insert(elem_type.clone());

        // Also register the List_<elem_type> as a generic instance
        let list_type_name = format!("List_{}", elem_type);
        self.generic_instances
            .entry(list_type_name)
            .or_insert(("List".to_string(), vec![elem_type]));
    }

    /// Register that we need a list concat function for the given element type.
    /// This also ensures the corresponding List_<elem_type> is registered as a generic instance.
    fn register_list_concat_needed(&mut self, elem_type: String) {
        // Add to list_concat_needed
        self.list_concat_needed.insert(elem_type.clone());

        // Also register the List_<elem_type> as a generic instance
        let list_type_name = format!("List_{}", elem_type);
        self.generic_instances
            .entry(list_type_name)
            .or_insert(("List".to_string(), vec![elem_type]));
    }

    /// Resolve the C name for a blitz type given its name and module.
    /// Used by infer_expr_type and other places that work with raw type name strings
    /// rather than Type AST nodes.
    fn resolve_c_name_for(&self, blitz_name: &str, module: &str) -> String {
        if let Some(c_name) = self
            .type_name_registry
            .get_c_name_for_module(blitz_name, module)
        {
            return c_name;
        }
        // Fall back to name-only lookup
        self.type_name_registry.get_c_name(blitz_name)
    }

    /// Convenience: return `"Option_{c_name}"` for a blitz type in a given module.
    fn option_c_name(&self, blitz_name: &str, module: &str) -> String {
        format!("Option_{}", self.resolve_c_name_for(blitz_name, module))
    }

    /// Resolve a field type string from `struct_field_types` into the correct C type.
    /// Field types are stored as blitz type signatures (e.g., "List_Expression", "Expression",
    /// "Option_Ident"). The inner type names need module-aware resolution because the same
    /// blitz name (e.g., "Expression") can map to different C names depending on the module
    /// (e.g., "Expression" in hir vs "ast_Expression" in ast).
    ///
    /// `struct_c_name` is the C name of the struct that owns this field, used to determine
    /// the module context for resolution.
    fn resolve_field_type_to_c(&self, field_type: &str, struct_c_name: &str) -> String {
        let module = self
            .c_name_to_module
            .get(struct_c_name)
            .map(|s| s.as_str())
            .unwrap_or(&self.current_module);

        if field_type.starts_with("List_") {
            let inner = &field_type[5..];
            let resolved = self.resolve_c_name_for(inner, module);
            format!("List_{}", resolved)
        } else if field_type.starts_with("List(") && field_type.ends_with(")") {
            let inner = &field_type[5..field_type.len() - 1];
            let resolved = self.resolve_c_name_for(inner, module);
            format!("List_{}", resolved)
        } else if field_type.starts_with("Option_") {
            let inner = &field_type[7..];
            let resolved = self.resolve_c_name_for(inner, module);
            format!("Option_{}", resolved)
        } else if field_type.starts_with("Option(") && field_type.ends_with(")") {
            let inner = &field_type[7..field_type.len() - 1];
            let resolved = self.resolve_c_name_for(inner, module);
            format!("Option_{}", resolved)
        } else {
            // Plain type name - resolve directly
            self.resolve_c_name_for(field_type, module)
        }
    }

    /// Resolve the C name for a type, taking its module qualifier into account.
    /// If the type has an explicit module (e.g., `ast::Return`), use that for lookup.
    /// If no explicit module, fall back to current_module for disambiguation.
    /// Returns the original name if no module-specific registration is found.
    fn resolve_c_name_for_type(&self, ty: &Type) -> String {
        let module = ty.module.as_deref().unwrap_or(&self.current_module);

        // Try module-aware lookup first
        if let Some(c_name) = self
            .type_name_registry
            .get_c_name_for_module(&ty.name, module)
        {
            return c_name;
        }

        // If explicit module was given but not found, it might be that
        // the type was registered from a different module name (e.g., path differences).
        // Fall back to the default name-only lookup.
        self.type_name_registry.get_c_name(&ty.name)
    }

    /// Get a simple type name for use in instance names
    fn type_name_for_instance(&self, ty: &Type) -> String {
        if ty.params.is_empty() {
            // For non-generic types, resolve the C name using module context
            let name = &ty.name;

            // Check if there's a collision suffix version that's an enum
            // This is needed for Option/List generic types when both struct and enum exist
            let suffixed_name = format!("{}_1", name);
            if self.enum_types.contains(&suffixed_name) {
                // If both struct and enum exist with the same original name,
                // prefer the enum for generic type instances (Option, List, etc.)
                // because enums are passed by value while structs are passed by pointer
                return suffixed_name;
            }

            // Also check if there's a collision detected in the type registry
            // This handles the case where we're called before enum_types is fully populated
            if self.type_name_registry.has_collision(name) {
                // There's a collision - for struct types that are variants of unions,
                // we need to use the suffixed name that matches how the union was generated
                // Look up what the struct was registered as
                if let Some(instances) = self.type_name_registry.get_instances(name) {
                    if instances.len() > 1 {
                        // Check if ANY of the instances is an enum with suffix
                        for (c_name, kind, _module) in instances {
                            if *kind == TypeKind::Enum && c_name.ends_with("_1") {
                                return suffixed_name;
                            }
                        }
                    }
                }
            }

            // Use module-aware name resolution
            self.resolve_c_name_for_type(ty)
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

    /// Check if a mapped C type is a "value type" (passed by value, not by pointer).
    /// Value types include primitives, enums, List_T, and Option_T.
    /// Pointer types (ending with *) are already indirected, so `mut` doesn't change them.
    fn is_c_value_type(&self, c_type: &str) -> bool {
        !c_type.ends_with('*')
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
        let mangled = self.compute_mangled_name(name);
        self.register_variable(name, &mangled);
        mangled
    }

    /// Compute the mangled name for a variable WITHOUT registering it
    /// This allows generating initializer expressions before the variable is registered,
    /// which is important when the initializer contains enum variants with the same name
    fn compute_mangled_name(&self, name: &str) -> String {
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
            return format!("blitz_{}", name);
        }

        // Check if this variable name matches a C stdlib function name
        if Self::is_reserved_c_name(name) {
            return format!("{}_var", name);
        }

        // Check if this variable name matches any user-defined Blitz function
        if self.function_signatures.contains_key(name) {
            return format!("{}_var", name);
        }

        // No collision, use original name
        name.to_string()
    }

    /// Register a variable name mapping
    fn register_variable(&mut self, original_name: &str, mangled_name: &str) {
        if !self.variable_name_mappings.contains_key(original_name) {
            self.variable_name_mappings
                .insert(original_name.to_string(), mangled_name.to_string());
        }
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
            debug_println!(
                self,
                "DEBUG: Mangling function call '{}' -> 'blitz_{}' (C stdlib collision)",
                func_name,
                func_name
            );
            return format!("blitz_{}", func_name);
        }

        // Check if this function has overloads
        if let Some(signatures) = self.function_signatures.get(func_name) {
            // Helper to strip pointer notation for type comparison
            #[allow(dead_code)]
            fn strip_ptr(s: &str) -> &str {
                s.trim_end_matches('*').trim_end_matches("Ptr")
            }

            // Helper to normalize type names for comparison
            fn normalize_type(s: &str) -> &str {
                match s.trim_end_matches('*') {
                    "char" | "String" => "String",
                    "int64_t" | "Int" => "Int",
                    "bool" | "Bool" => "Bool",
                    "double" | "Float" => "Float",
                    "uint32_t" | "Rune" => "Rune",
                    other => other,
                }
            }

            // If there's only one signature, no mangling needed
            if signatures.len() == 1 {
                return func_name.to_string();
            }

            // Multiple signatures - try to match by argument types (using normalized comparison)
            for (param_types, _) in signatures {
                if param_types.len() == arg_types.len() {
                    let matches = param_types
                        .iter()
                        .zip(arg_types.iter())
                        .all(|(p, a)| normalize_type(p) == normalize_type(a));
                    if matches {
                        // Found match - return mangled name
                        debug_println!(self, 
                            "DEBUG: Exact match for function '{}' (param types: {:?}, arg types: {:?})",
                            func_name, param_types, arg_types
                        );
                        return self.mangle_function_name(func_name, param_types);
                    }
                }
            }

            // No match found - try to find a signature with matching argument count
            for (param_types, _) in signatures {
                if param_types.len() == arg_types.len() {
                    debug_println!(self, 
                        "DEBUG: Matched function '{}' by arg count {} (param types: {:?}, arg types: {:?})",
                        func_name, arg_types.len(), param_types, arg_types
                    );
                    return self.mangle_function_name(func_name, param_types);
                }
            }

            // If we have signatures but no match, use the first one as a fallback
            debug_println!(
                self,
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
                let c_name = self.type_name_registry.register_type(
                    struct_name,
                    TypeKind::Struct,
                    &self.current_module,
                );

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
                let c_name =
                    self.type_name_registry
                        .register_type(union_name, kind, &self.current_module);

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
                // Generate for Expression, Statement, and Literal (all have span in their variants)
                if has_typed_variants
                    && (union_name == "Expression"
                        || union_name == "Statement"
                        || union_name == "Literal")
                {
                    self.generate_union_span_helper(&c_name, &u.cases)?;
                }
            }
            Definition::Fn(f) => {
                self.generate_function(f)?;
            }
            Definition::Pub(p) => {
                self.generate_definition(&p.item)?;
            }
            Definition::Test(test) => {
                // Generate test function if in test mode
                if self.include_tests {
                    self.generate_test_function(test)?;
                }
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
        let c_name = self.type_name_registry.register_type(
            struct_name,
            TypeKind::Struct,
            &self.current_module,
        );

        // Skip if it's a built-in type
        if self.is_builtin_type(&c_name) {
            return Ok(());
        }

        // Skip generic structs for now
        if !sig.params.is_empty() {
            debug_println!(self, "Skipping generic struct {}", struct_name);
            self.header.push_str(&format!(
                "// TODO: Generic struct {} - NOT IMPLEMENTED YET\n\n",
                c_name
            ));
            return Ok(());
        }

        debug_println!(self, "Generating struct {} as {}", struct_name, c_name);

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

        // Skip the Bool union - it's defined in std but we use C's native bool type
        if union_name == "Bool" {
            return Ok(());
        }

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
        let c_name = self
            .type_name_registry
            .register_type(union_name, kind, &self.current_module);

        if !has_typed_variants {
            // Generate simple enum for symbolic-only unions
            debug_println!(self, "Generating enum {} as {}", union_name, c_name);

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
            debug_println!(
                self,
                "DEBUG: Storing enum '{}' with {} variants",
                &c_name,
                variants.len()
            );
            self.enum_variants.insert(c_name.clone(), variants);
        } else {
            // Generate tagged union for mixed/typed unions
            debug_println!(self, "Generating tagged union {} as {}", union_name, c_name);
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

        // Types that are nested tagged unions - need to call their own _span helper
        let nested_tagged_unions = ["Literal"];

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
                let resolved_variant_c_name = self.resolve_c_name_for_type(variant_type);

                // Check if this type doesn't have a span field
                if types_without_span.contains(&type_name.as_str()) {
                    if type_name == "Expression" {
                        // For Expression variant in Statement, recurse to Expression's _span
                        code.push_str(&format!(
                            "            return {}_span(expr->data.as_{});\n",
                            resolved_variant_c_name, variant_name
                        ));
                    } else {
                        // Assignment doesn't have span - return NULL or get from left expression
                        // left is Expression* (not Expression**), so no dereference needed
                        // We recurse to the PARENT union's _span (i.e., c_name), not the variant's
                        code.push_str(&format!(
                            "            return {}_span(expr->data.as_{}->left);\n",
                            c_name, variant_name
                        ));
                    }
                } else if nested_tagged_unions.contains(&type_name.as_str()) {
                    // For nested tagged unions like Literal, call their _span helper
                    code.push_str(&format!(
                        "            return {}_span(expr->data.as_{});\n",
                        resolved_variant_c_name, variant_name
                    ));
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
        let _param_types: Vec<String> = func
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

        // Skip main function when in test mode (test runner provides its own main)
        if is_main && self.include_tests {
            debug_println!(
                self,
                "Skipping main function (test mode generates test runner main)"
            );
            return Ok(());
        }

        if is_main {
            // main() in C must always return int
            return_type = "int".to_string();
        }

        // Generate parameter list and collect parameter types for mangling
        let mut params = Vec::new();
        let mut param_types = Vec::new();
        // Clear mut_pointer_params for this function scope
        self.mut_pointer_params.clear();
        for arg in &func.args {
            let param_type = self.map_type(&arg.r#type);
            let param_type_sig = self.type_signature(&arg.r#type);
            param_types.push(param_type_sig);
            // Mangle parameter name if it collides with C stdlib
            let param_name = self.mangle_variable_name(&arg.name);

            // Handle mut parameters on value types: pass by pointer
            if arg.mutable && self.is_c_value_type(&param_type) {
                debug_println!(
                    self,
                    "DEBUG: mut value-type param '{}' of type '{}' -> '{}*'",
                    param_name,
                    param_type,
                    param_type
                );
                // Track this as a mut-pointer param so the body can dereference it
                self.mut_pointer_params
                    .insert(param_name.clone(), param_type.clone());
                // Track variable type as the ORIGINAL value type (not pointer) for type inference
                // The pointer indirection is handled by mut_pointer_params in code generation
                self.variable_types
                    .insert(param_name.clone(), param_type.clone());
                params.push(format!("{}* {}", param_type, param_name));
            } else {
                // Track parameter type for type inference
                self.variable_types
                    .insert(param_name.clone(), param_type.clone());
                params.push(format!("{} {}", param_type, param_name));
            }
        }
        let params_str = if params.is_empty() {
            "void".to_string()
        } else {
            params.join(", ")
        };

        // Get the mangled function name
        let c_func_name =
            if is_main {
                func.name.clone()
            } else {
                // Check if function name collides with C stdlib first
                // This takes precedence over overload mangling
                if Self::is_reserved_c_name(&func.name) {
                    // Mangle to avoid collision: time -> blitz_time, read -> blitz_read
                    debug_println!(self, 
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
            debug_println!(
                self,
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
        // First, pre-analyze declarations with empty lists to infer their types from usage
        self.preanalyze_empty_lists(&func.body);

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
                        } else if let parser::Expression::BinaryOp(binop) = expr {
                            // Check if this is an if-else expression (If else Block/If)
                            if matches!(binop.op, parser::Operator::Else)
                                && matches!(&*binop.left, parser::Expression::If(_))
                            {
                                // This is an if-else that needs to be the final expression
                                // Generate it as if-else with returns in each branch
                                let ret_type = if let Some(ref ty) = func.r#type {
                                    self.map_type(ty)
                                } else {
                                    "void".to_string()
                                };
                                self.generate_if_else_with_returns(
                                    &binop.left,
                                    &binop.right,
                                    &ret_type,
                                    is_main,
                                )
                            } else {
                                // Other binary op - regular handling
                                let mut expr_code = self.generate_expression(expr, is_main);

                                if let Some(ref ret_ty) = func.r#type {
                                    let c_ret_ty = self.map_type(ret_ty);
                                    if c_ret_ty.starts_with("Option_") {
                                        let expr_type = self.infer_expr_type(expr);
                                        if !expr_type.starts_with("Option_") {
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
                        } else {
                            // Check if it's a noreturn call like todo() or panic()
                            let is_noreturn_call = matches!(expr, parser::Expression::Call(call) 
                                if matches!(call.name.as_str(), "todo" | "panic" | "unreachable" | "assert"));

                            // Check if it's a control flow expression that shouldn't be wrapped in return
                            // For-expressions that collect values ARE expressions and should be returned
                            let is_for_expression = if let parser::Expression::For(for_expr) = expr
                            {
                                // Check if it's a for-expression (collects values) vs for-statement
                                for_expr.body.len() == 1
                                    && matches!(&for_expr.body[0], parser::Statement::Expression(e)
                                    if {
                                        let expr_type = self.infer_expr_type(e);
                                        expr_type != "void"
                                    })
                            } else {
                                false
                            };

                            let is_control_flow = matches!(
                                expr,
                                parser::Expression::While(_) | parser::Expression::If(_)
                            ) || (matches!(expr, parser::Expression::For(_))
                                && !is_for_expression);

                            if is_noreturn_call || is_control_flow {
                                // For noreturn functions and control flow, just generate without return
                                let expr_code = self.generate_expression(expr, is_main);
                                if is_control_flow {
                                    expr_code // Control flow doesn't need semicolon
                                } else {
                                    format!("{};", expr_code)
                                }
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

        // Check if the last statement is a `while true` loop in a non-void function.
        // If so, add an unreachable return to silence "non-void function does not return a value" warnings.
        if !is_main && func.r#type.is_some() {
            if let Some(last_stmt) = func.body.last() {
                if Self::is_infinite_while_loop(last_stmt) {
                    // Add unreachable return with default value for the return type
                    let default_return = self.generate_default_return_value(&return_type);
                    self.impl_code
                        .push_str(&format!("    return {}; // unreachable\n", default_return));
                }
            }
        }

        self.impl_code.push_str("}\n\n");

        Ok(())
    }

    /// Generate a test function from a Blitz test block
    fn generate_test_function(&mut self, test: &Test) -> Result<(), String> {
        // Clear variable name mappings and types for new test scope
        self.variable_name_mappings.clear();
        self.variable_types.clear();

        // Generate a safe function name from the test name
        // Replace spaces and special chars with underscores
        let safe_name: String = test
            .name
            .chars()
            .map(|c| if c.is_alphanumeric() { c } else { '_' })
            .collect();
        let c_func_name = format!("blitz_test_{}", safe_name);

        // Store return type (tests have no return type, so void)
        self.current_return_type = None;

        // Add forward declaration for the test function
        let func_signature = format!("void {}(void);", c_func_name);
        self.all_functions
            .push((c_func_name.clone(), func_signature));

        // Generate function definition
        self.impl_code
            .push_str(&format!("void {}(void) {{\n", c_func_name));

        // Pre-analyze declarations with empty lists to infer their types from usage
        // (same as generate_function does for regular functions)
        self.preanalyze_empty_lists(&test.body);

        // Generate statements in the test body
        for stmt in &test.body {
            let stmt_code = self.generate_statement(stmt, false);
            for line in stmt_code.lines() {
                if !line.is_empty() {
                    self.impl_code.push_str(&format!("    {}\n", line));
                }
            }
        }

        self.impl_code.push_str("}\n\n");

        Ok(())
    }

    /// Check if a statement is an infinite while loop (while true { ... })
    fn is_infinite_while_loop(stmt: &parser::Statement) -> bool {
        if let parser::Statement::Expression(expr) = stmt {
            if let parser::Expression::While(while_loop) = expr {
                // Check if the condition is a boolean literal `true`
                if let parser::Expression::BoolLit(bool_lit) = &*while_loop.cond {
                    return bool_lit.value;
                }
            }
        }
        false
    }

    /// Generate a default return value for a given C type.
    /// This is used for unreachable code paths to silence compiler warnings.
    fn generate_default_return_value(&self, c_type: &str) -> String {
        match c_type {
            "void" => String::new(),
            "int" | "int64_t" | "int32_t" | "int16_t" | "int8_t" => "0".to_string(),
            "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t" | "size_t" => "0".to_string(),
            "bool" => "false".to_string(),
            "double" | "float" => "0.0".to_string(),
            "char*" => "NULL".to_string(),
            _ if c_type.ends_with("*") => "NULL".to_string(),
            // For enum types, use the first variant (tag 0)
            _ if self.enum_types.contains(c_type) => format!("({})0", c_type),
            // For struct/tagged union types, return a zeroed value
            _ => format!("({}){{0}}", c_type),
        }
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

                // Special handling for standalone if expressions (without else)
                // When used as a statement, generate a simple if statement, not a statement expression
                if let parser::Expression::If(if_expr) = expr {
                    return self.generate_if_as_pure_statement(if_expr, is_main);
                }

                // Special handling for for loops used as pure statements
                // When a for loop is the entire statement (not part of a larger expression),
                // it should NOT collect results - just execute for side effects
                if let parser::Expression::For(for_loop) = expr {
                    return self.generate_for_as_pure_statement(for_loop, is_main);
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

                // IMPORTANT: We compute the variable name but do NOT register it yet.
                // This is because the initializer expression may contain enum variants
                // that have the same name as the variable (e.g., `let type = if has(parser, type) {...}`)
                // If we register the variable first, the enum variant `type` would be seen
                // as the variable, not as `TokenKind_type`.
                let var_name = self.compute_mangled_name(&decl.name);

                // If type is empty, "_", or a generic type parameter (single uppercase letter),
                // try to infer from initialization expression
                let is_generic_param =
                    c_type.len() == 1 && c_type.chars().next().map_or(false, |c| c.is_uppercase());
                if c_type.is_empty() || c_type == "_" || is_generic_param {
                    // First check if preanalysis determined the type (for empty lists)
                    if let Some(preanalyzed_type) = self.variable_types.get(&decl.name) {
                        c_type = preanalyzed_type.clone();
                        debug_println!(
                            self,
                            "DEBUG: Using preanalyzed type '{}' for variable '{}'",
                            c_type,
                            decl.name
                        );
                    } else if let Some(init_expr) = &decl.init {
                        // Add debug for BinaryOp expressions
                        if let parser::Expression::BinaryOp(binop) = init_expr {
                            debug_println!(
                                self,
                                "DEBUG: Declaration init is BinaryOp with op={:?}",
                                binop.op
                            );
                        }
                        // Special handling for for-expression inits: temporarily register
                        // the iteration variable's type so body inference can resolve method calls.
                        if let parser::Expression::For(for_expr) = init_expr {
                            let iter_type = self.infer_expr_type(&for_expr.iter);
                            if iter_type.starts_with("List_") {
                                let inner = &iter_type[5..]; // strip "List_" prefix
                                let is_struct = self.seen_types.contains(inner)
                                    && !self.enum_types.contains(inner);
                                let is_tagged = self.tagged_union_types.contains(inner);
                                let elem_c_type = if is_struct || is_tagged {
                                    format!("{}*", inner)
                                } else {
                                    self.normalize_c_type(inner)
                                };
                                let elem_var = self.mangle_variable_name(&for_expr.elem);
                                self.variable_types.insert(elem_var.clone(), elem_c_type);
                                c_type = self.infer_expr_type(init_expr);
                                self.variable_types.remove(&elem_var);
                            } else {
                                c_type = self.infer_expr_type(init_expr);
                            }
                        } else {
                            c_type = self.infer_expr_type(init_expr);
                        }
                        debug_println!(
                            self,
                            "DEBUG: Inferred type '{}' for variable '{}' (init expr kind: {:?})",
                            c_type,
                            decl.name,
                            std::mem::discriminant(init_expr)
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
                    let _inferred_type = self.infer_expr_type(init_expr);

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
                                    debug_println!(self, "DEBUG: Fixing variable declaration type mismatch for unwrapped value. Declared: {}, Inferred: {}", c_type, inferred);
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
                        // Register the variable before returning
                        self.register_variable(&decl.name, &var_name);
                        // Transform switch expression to statement form with temp variable
                        return self.generate_switch_as_statement(
                            switch_expr,
                            is_main,
                            &c_type,
                            &var_name,
                        );
                    }

                    // Special handling for empty list literals - use the declared type
                    if let parser::Expression::List(list) = init_expr {
                        if list.elems.is_empty() {
                            // Register the variable before returning
                            self.register_variable(&decl.name, &var_name);
                            // Generate empty list with correct type from declaration
                            self.variable_types.insert(var_name.clone(), c_type.clone());
                            return format!(
                                "{} {} = ({}){{{}.data = NULL, .len = 0, .cap = 0}};",
                                c_type, var_name, c_type, ""
                            );
                        }
                    }

                    // Special handling for `none` initializer - use the preanalyzed Option type
                    if let parser::Expression::Ident(ident) = init_expr {
                        if ident.name == "none" {
                            // Register the variable before returning
                            self.register_variable(&decl.name, &var_name);
                            // The c_type should already be set from preanalysis (e.g., Option_Ident)
                            self.variable_types.insert(var_name.clone(), c_type.clone());
                            if c_type.starts_with("Option_") {
                                return format!(
                                    "{} {} = ({}){{.tag = {}_tag_none}};",
                                    c_type, var_name, c_type, c_type
                                );
                            } else {
                                // Fallback: if we couldn't infer the type, at least generate something
                                eprintln!("WARNING: Could not infer Option type for none-initialized variable '{}', type '{}'", decl.name, c_type);
                                return format!(
                                    "{} {} = ({}){{.tag = {}_tag_none}};",
                                    c_type, var_name, c_type, c_type
                                );
                            }
                        }
                    }

                    // Generate the initializer BEFORE registering the variable
                    // This is important for cases like `let type = if has(parser, type) {...}`
                    // where `type` in the condition should be qualified as `TokenKind_type`
                    let init_code = self.generate_expression(init_expr, is_main);

                    // NOW register the variable (after init_code is generated)
                    self.register_variable(&decl.name, &var_name);

                    // Track variable type for type inference
                    self.variable_types.insert(var_name.clone(), c_type.clone());
                    format!("{} {} = {};", c_type, var_name, init_code)
                } else {
                    // Declaration without initialization
                    // Register the variable
                    self.register_variable(&decl.name, &var_name);
                    // Track variable type for type inference
                    self.variable_types.insert(var_name.clone(), c_type.clone());
                    format!("{} {};", c_type, var_name)
                }
                // Note: is_mut is semantic only in Blitz, not represented in C
            }
        }
    }

    /// Pre-analyze declarations with empty lists to infer their types from usage
    /// This scans the function body for patterns like `var ++= Constructor(...)` to determine list element types
    fn preanalyze_empty_lists(&mut self, stmts: &[parser::Statement]) {
        // First, collect all declarations with empty list initializers
        let mut empty_list_vars: HashMap<String, ()> = HashMap::new();
        // Also collect declarations with `none` initializers (need Option type inference)
        let mut none_vars: HashMap<String, ()> = HashMap::new();

        // Recursively collect all declarations in the function body
        self.collect_declarations_recursive(stmts, &mut empty_list_vars, &mut none_vars);

        // Analyze none-initialized variables first
        if !none_vars.is_empty() {
            debug_println!(
                self,
                "DEBUG preanalyze: {} none vars to analyze",
                none_vars.len()
            );
            self.analyze_none_usage_in_stmts_recursive(stmts, &none_vars);
        }

        if empty_list_vars.is_empty() {
            return;
        }

        // Pre-populate variable_types for declarations initialized with constructors or
        // other expressions whose type can be inferred. This allows infer_expr_type to
        // resolve Ident references during list usage analysis (e.g., `asts ++= ast` where
        // `ast` was declared as `let ast = Ast(...)`).
        // We only do this when there are empty lists to analyze, and we clean up afterward
        // to avoid interfering with subsequent code generation.
        let prepopulated_keys = self.prepopulate_variable_types(stmts);

        debug_println!(
            self,
            "DEBUG preanalyze_empty_lists: {} empty list vars to analyze",
            empty_list_vars.len()
        );

        // Now scan for usages that tell us the element type
        self.analyze_list_usage_in_stmts(stmts, &empty_list_vars);

        // Clean up prepopulated types that aren't for empty list vars,
        // so they don't interfere with subsequent code generation
        for key in prepopulated_keys {
            if !empty_list_vars.contains_key(&key) {
                self.variable_types.remove(&key);
            }
        }
    }

    /// Pre-populate variable_types for declarations whose types can be statically inferred.
    /// This runs before list usage analysis so that `infer_expr_type` can resolve Ident
    /// references to variables like `ast` (from `let ast = Ast(...)`) when analyzing
    /// patterns like `asts ++= ast`.
    fn prepopulate_variable_types(&mut self, stmts: &[parser::Statement]) -> Vec<String> {
        let mut inserted_keys = Vec::new();
        for stmt in stmts {
            if let parser::Statement::Declaration(decl) = stmt {
                // Skip if already has a type from previous analysis
                if self.variable_types.contains_key(&decl.name) {
                    continue;
                }
                if let Some(init) = &decl.init {
                    let inferred = self.infer_expr_type(init);
                    if inferred != "int64_t" && inferred != "void" {
                        debug_println!(
                            self,
                            "DEBUG prepopulate_variable_types: '{}' -> '{}'",
                            decl.name,
                            inferred
                        );
                        self.variable_types
                            .insert(decl.name.clone(), inferred.clone());
                        inserted_keys.push(decl.name.clone());
                    }
                }
            }
        }
        inserted_keys
    }

    /// Recursively collect declarations from statement lists, including nested control flow
    fn collect_declarations_recursive(
        &self,
        stmts: &[parser::Statement],
        empty_list_vars: &mut HashMap<String, ()>,
        none_vars: &mut HashMap<String, ()>,
    ) {
        for stmt in stmts {
            if let parser::Statement::Declaration(decl) = stmt {
                if let Some(init) = &decl.init {
                    if let parser::Expression::List(list) = init {
                        if list.elems.is_empty() {
                            debug_println!(
                                self,
                                "DEBUG preanalyze_empty_lists: found empty list declaration '{}'",
                                decl.name
                            );
                            empty_list_vars.insert(decl.name.clone(), ());
                        }
                    }
                    // Check for `none` initializer - indicates Option type
                    if let parser::Expression::Ident(ident) = init {
                        if ident.name == "none" {
                            debug_println!(
                                self,
                                "DEBUG preanalyze: found none-initialized declaration '{}'",
                                decl.name
                            );
                            none_vars.insert(decl.name.clone(), ());
                        }
                    }
                }
            }
            if let parser::Statement::Expression(expr) = stmt {
                self.collect_declarations_in_expr(expr, empty_list_vars, none_vars);
            }
        }
    }

    /// Recursively collect declarations from expressions (control flow bodies)
    fn collect_declarations_in_expr(
        &self,
        expr: &parser::Expression,
        empty_list_vars: &mut HashMap<String, ()>,
        none_vars: &mut HashMap<String, ()>,
    ) {
        match expr {
            parser::Expression::While(w) => {
                self.collect_declarations_recursive(&w.body, empty_list_vars, none_vars);
            }
            parser::Expression::For(for_expr) => {
                self.collect_declarations_recursive(&for_expr.body, empty_list_vars, none_vars);
            }
            parser::Expression::If(if_expr) => {
                self.collect_declarations_recursive(&if_expr.body, empty_list_vars, none_vars);
            }
            parser::Expression::BinaryOp(binop) => {
                // Handle else blocks which are BinaryOp with Else operator
                if matches!(binop.op, parser::Operator::Else) {
                    self.collect_declarations_in_expr(&binop.left, empty_list_vars, none_vars);
                    self.collect_declarations_in_expr(&binop.right, empty_list_vars, none_vars);
                }
            }
            parser::Expression::Block(stmts) => {
                self.collect_declarations_recursive(stmts, empty_list_vars, none_vars);
            }
            parser::Expression::Switch(switch_expr) => {
                // Recurse into switch case bodies
                for case in &switch_expr.cases {
                    self.collect_declarations_recursive(&case.body, empty_list_vars, none_vars);
                }
            }
            _ => {}
        }
    }

    /// Recursively analyze none usage in statements, including nested control flow
    fn analyze_none_usage_in_stmts_recursive(
        &mut self,
        stmts: &[parser::Statement],
        none_vars: &HashMap<String, ()>,
    ) {
        for stmt in stmts {
            match stmt {
                parser::Statement::Expression(expr) => {
                    self.analyze_none_usage_in_expr(expr, none_vars);
                }
                _ => {}
            }
        }
    }

    /// Analyze an expression to infer Option types from assignments to none-initialized variables
    fn analyze_none_usage_in_expr(
        &mut self,
        expr: &parser::Expression,
        none_vars: &HashMap<String, ()>,
    ) {
        match expr {
            parser::Expression::Assignment(assign) => {
                // Look for patterns like: label = parse_ident(parser)
                // This tells us that `label` should be `Option_Ident`
                if let parser::Lval::Ident(lval_ident) = &assign.left {
                    if none_vars.contains_key(&lval_ident.name) {
                        // Infer the type from the RHS
                        let inferred = self.infer_expr_type(&assign.right);
                        debug_println!(
                            self,
                            "DEBUG analyze_none_usage: '{}' assigned with type '{}'",
                            lval_ident.name,
                            inferred
                        );
                        // The RHS should already be an Option type if it's from a function returning Option
                        // If it's a direct value, we need to wrap it
                        if inferred.starts_with("Option_") {
                            self.variable_types
                                .insert(lval_ident.name.clone(), inferred);
                        } else if inferred != "void" && inferred != "int64_t" {
                            // Wrap non-Option types
                            let option_type = format!("Option_{}", inferred.trim_end_matches('*'));
                            self.variable_types
                                .insert(lval_ident.name.clone(), option_type);
                        }
                    }
                }
            }
            parser::Expression::BinaryOp(binop) => {
                // Recursively analyze both sides
                self.analyze_none_usage_in_expr(&binop.left, none_vars);
                self.analyze_none_usage_in_expr(&binop.right, none_vars);
            }
            parser::Expression::While(w) => {
                self.analyze_none_usage_in_stmts_recursive(&w.body, none_vars);
            }
            parser::Expression::For(for_expr) => {
                self.analyze_none_usage_in_stmts_recursive(&for_expr.body, none_vars);
            }
            parser::Expression::If(if_expr) => {
                self.analyze_none_usage_in_stmts_recursive(&if_expr.body, none_vars);
            }
            parser::Expression::Block(stmts) => {
                self.analyze_none_usage_in_stmts_recursive(stmts, none_vars);
            }
            _ => {}
        }
    }

    /// Recursively scan statements for list usage patterns
    fn analyze_list_usage_in_stmts(
        &mut self,
        stmts: &[parser::Statement],
        empty_list_vars: &HashMap<String, ()>,
    ) {
        for stmt in stmts {
            match stmt {
                parser::Statement::Expression(expr) => {
                    self.analyze_list_usage_in_expr(expr, empty_list_vars);
                }
                _ => {}
            }
        }
    }

    /// Analyze an expression to find list element types from usage patterns
    fn analyze_list_usage_in_expr(
        &mut self,
        expr: &parser::Expression,
        empty_list_vars: &HashMap<String, ()>,
    ) {
        match expr {
            parser::Expression::Assignment(assign) => {
                // Look for patterns like: cases ++= SwitchCase(...)
                // This parses as Assignment { left: cases, right: BinaryOp(Concat, cases, SwitchCase(...)) }
                if let parser::Lval::Ident(lval_ident) = &assign.left {
                    debug_println!(
                        self,
                        "DEBUG analyze_list_usage: found assignment to '{}'",
                        lval_ident.name
                    );
                    if empty_list_vars.contains_key(&lval_ident.name) {
                        debug_println!(
                            self,
                            "DEBUG analyze_list_usage: '{}' is in empty_list_vars",
                            lval_ident.name
                        );
                        // Check if RHS is a concat operation (from ++=)
                        if let parser::Expression::BinaryOp(binop) = &*assign.right {
                            debug_println!(
                                self,
                                "DEBUG analyze_list_usage: RHS is BinaryOp, op = {:?}",
                                binop.op
                            );
                            if matches!(binop.op, parser::Operator::Concat) {
                                // Check if the right side of concat is a constructor
                                if let parser::Expression::Constructor(ctor) = &*binop.right {
                                    let elem_type = self.type_name_for_instance(&ctor.r#type);
                                    let list_type = format!("List_{}", elem_type);
                                    debug_println!(self, 
                                        "DEBUG preanalyze_empty_lists: variable '{}' concat-assigned with constructor '{}', inferred type '{}'",
                                        lval_ident.name, elem_type, list_type
                                    );
                                    self.variable_types
                                        .insert(lval_ident.name.clone(), list_type);
                                }
                                // Also check if right side is a direct constructor (for = concat case)
                                else if let parser::Expression::Constructor(ctor) = &*binop.left {
                                    let elem_type = self.type_name_for_instance(&ctor.r#type);
                                    let list_type = format!("List_{}", elem_type);
                                    debug_println!(self, 
                                        "DEBUG preanalyze_empty_lists: variable '{}' concat with constructor '{}', inferred type '{}'",
                                        lval_ident.name, elem_type, list_type
                                    );
                                    self.variable_types
                                        .insert(lval_ident.name.clone(), list_type);
                                }
                                // Use type inference for other expressions (calls, member access, etc.)
                                else {
                                    let inferred = self.infer_expr_type(&binop.right);
                                    // If the inferred type is Option_T, unwrap to get T
                                    let elem_type =
                                        if let Some(inner) = inferred.strip_prefix("Option_") {
                                            inner.to_string()
                                        } else {
                                            inferred.trim_end_matches('*').to_string()
                                        };
                                    if elem_type != "int64_t" && elem_type != "void" {
                                        let list_type = format!("List_{}", elem_type);
                                        debug_println!(self, 
                                            "DEBUG preanalyze_empty_lists: variable '{}' concat-assigned with inferred elem type '{}', list type '{}'",
                                            lval_ident.name, elem_type, list_type
                                        );
                                        self.variable_types
                                            .insert(lval_ident.name.clone(), list_type);
                                    }
                                }
                            }
                        }
                        // Direct constructor assignment (rare but possible)
                        else if let parser::Expression::Constructor(ctor) = &*assign.right {
                            let elem_type = self.type_name_for_instance(&ctor.r#type);
                            let list_type = format!("List_{}", elem_type);
                            debug_println!(self, 
                                "DEBUG preanalyze_empty_lists: variable '{}' assigned constructor '{}', inferred type '{}'",
                                lval_ident.name, elem_type, list_type
                            );
                            self.variable_types
                                .insert(lval_ident.name.clone(), list_type);
                        }
                    }
                }
            }
            parser::Expression::BinaryOp(binop) => {
                // Recursively analyze
                self.analyze_list_usage_in_expr(&binop.left, empty_list_vars);
                self.analyze_list_usage_in_expr(&binop.right, empty_list_vars);
            }
            parser::Expression::While(w) => {
                // Scan the while body
                self.analyze_list_usage_in_stmts(&w.body, empty_list_vars);
            }
            parser::Expression::For(for_expr) => {
                // Temporarily register the iteration variable's type so that
                // infer_expr_type can resolve it when analyzing calls inside the body.
                // e.g., `for asts |ast| { stubs.mut.insert(ast) }` — we need to know
                // that `ast` is `Ast*` to disambiguate the insert() overload.
                let iter_type = self.infer_expr_type(&for_expr.iter);
                let mut temp_var: Option<String> = None;
                if iter_type.starts_with("List_") {
                    let inner = &iter_type[5..]; // strip "List_" prefix
                    let is_struct =
                        self.seen_types.contains(inner) && !self.enum_types.contains(inner);
                    let is_tagged = self.tagged_union_types.contains(inner);
                    let elem_c_type = if is_struct || is_tagged {
                        format!("{}*", inner)
                    } else {
                        inner.to_string()
                    };
                    let elem_var = for_expr.elem.clone();
                    if !self.variable_types.contains_key(&elem_var) {
                        self.variable_types.insert(elem_var.clone(), elem_c_type);
                        temp_var = Some(elem_var);
                    }
                }
                // Scan the for body
                self.analyze_list_usage_in_stmts(&for_expr.body, empty_list_vars);
                // Clean up temporary variable type
                if let Some(var) = temp_var {
                    self.variable_types.remove(&var);
                }
            }
            parser::Expression::If(if_expr) => {
                // If only has cond and body; else is handled via BinaryOp::Else
                self.analyze_list_usage_in_stmts(&if_expr.body, empty_list_vars);
            }
            parser::Expression::Block(stmts) => {
                // Block is just Vec<Statement>
                self.analyze_list_usage_in_stmts(stmts, empty_list_vars);
            }
            parser::Expression::Constructor(ctor) => {
                // Look for patterns like Block(statements: body, ...) where body is an empty list var
                // This tells us that body should have the type of Block.statements
                let type_name = self.type_name_for_instance(&ctor.r#type);
                debug_println!(
                    self,
                    "DEBUG analyze_list_usage Constructor: type={}, args count={}",
                    type_name,
                    ctor.args.len()
                );
                for arg in &ctor.args {
                    if let parser::Expression::Ident(ident) = &*arg.init {
                        debug_println!(self, 
                            "DEBUG analyze_list_usage Constructor: arg {} = ident {} (in empty_list_vars={})",
                            arg.label.name, ident.name, empty_list_vars.contains_key(&ident.name)
                        );
                        if empty_list_vars.contains_key(&ident.name) {
                            // Look up the expected field type from struct_field_types
                            debug_println!(self, 
                                "DEBUG analyze_list_usage Constructor: looking up {}.{} in struct_field_types",
                                type_name, arg.label.name
                            );
                            if let Some(struct_fields) = self.struct_field_types.get(&type_name) {
                                debug_println!(self, 
                                    "DEBUG analyze_list_usage Constructor: found struct_fields for {}: {:?}",
                                    type_name, struct_fields.keys().collect::<Vec<_>>()
                                );
                                if let Some(field_type) = struct_fields.get(&arg.label.name) {
                                    debug_println!(self, 
                                    "DEBUG analyze_list_usage Constructor: field {} has type {}",
                                    arg.label.name, field_type
                                );
                                    // field_type might be like "List(Statement)" or already "List_Statement"
                                    // Resolve inner type names using module-aware lookup
                                    let list_type = if field_type.starts_with("List(")
                                        && field_type.ends_with(")")
                                    {
                                        self.resolve_field_type_to_c(field_type, &type_name)
                                    } else if field_type.starts_with("List_") {
                                        self.resolve_field_type_to_c(field_type, &type_name)
                                    } else {
                                        continue; // Not a list type
                                    };
                                    debug_println!(self, 
                                        "DEBUG preanalyze_empty_lists: variable '{}' used in constructor {}.{}, inferred type '{}'",
                                        ident.name, type_name, arg.label.name, list_type
                                    );
                                    self.variable_types.insert(ident.name.clone(), list_type);
                                }
                            }
                        }
                    }
                    // Also recurse into arg initializers
                    self.analyze_list_usage_in_expr(&*arg.init, empty_list_vars);
                }
            }
            parser::Expression::Ident(ident) => {
                // Check if this identifier is an empty list variable being returned
                // In that case, we should infer its type from the function return type
                if empty_list_vars.contains_key(&ident.name) {
                    if let Some(ref ret_type) = self.current_return_type {
                        let mapped = self.map_type(ret_type);
                        if mapped.starts_with("List_") {
                            debug_println!(self, 
                                "DEBUG preanalyze_empty_lists: variable '{}' is returned, inferred type '{}' from return type",
                                ident.name, mapped
                            );
                            self.variable_types.insert(ident.name.clone(), mapped);
                        }
                    }
                }
            }
            parser::Expression::Return(ret_expr) => {
                // Analyze return expressions for list usage
                self.analyze_list_usage_in_expr(&**ret_expr, empty_list_vars);
            }
            parser::Expression::Call(call) => {
                // Check if any argument to this call is an empty-list variable.
                // If so, look up the function signature to infer the expected list type.
                // This handles patterns like: stubs.mut.insert(ast)
                // where `insert(mut stubs List(ModuleStub), ast Ast)` tells us stubs is List(ModuleStub).
                if let Some(signatures) = self.function_signatures.get(&call.name) {
                    // Find which arg positions are empty-list variables
                    let mut empty_list_positions: Vec<(usize, String)> = Vec::new();
                    for (i, arg) in call.args.iter().enumerate() {
                        if let Some(var_name) = Self::extract_root_ident(&arg.init) {
                            if empty_list_vars.contains_key(&var_name) {
                                empty_list_positions.push((i, var_name));
                            }
                        }
                    }
                    if !empty_list_positions.is_empty() {
                        // Infer types for all non-empty-list arguments for disambiguation
                        let arg_types: Vec<Option<String>> = call
                            .args
                            .iter()
                            .enumerate()
                            .map(|(i, arg)| {
                                if empty_list_positions.iter().any(|(pos, _)| *pos == i) {
                                    None // unknown — this is the empty list we're trying to infer
                                } else {
                                    let inferred = self.infer_expr_type(&arg.init);
                                    if inferred == "int64_t" || inferred == "void" {
                                        None
                                    } else {
                                        Some(inferred)
                                    }
                                }
                            })
                            .collect();

                        // Try to find the best matching signature
                        fn normalize_type_for_match(s: &str) -> &str {
                            let s = s.trim_end_matches('*');
                            match s {
                                "char" | "String" => "String",
                                "int64_t" | "Int" => "Int",
                                "bool" | "Bool" => "Bool",
                                "double" | "Float" => "Float",
                                "uint32_t" | "Rune" => "Rune",
                                other => other,
                            }
                        }

                        let mut best_match: Option<&Vec<String>> = None;
                        for (param_types, _) in signatures.iter() {
                            if param_types.len() != call.args.len() {
                                continue;
                            }
                            // Check if all known arg types match this signature
                            let all_match = arg_types.iter().enumerate().all(|(i, arg_type)| {
                                match arg_type {
                                    None => true, // unknown, matches anything
                                    Some(at) => {
                                        normalize_type_for_match(&param_types[i])
                                            == normalize_type_for_match(at)
                                    }
                                }
                            });
                            if all_match {
                                best_match = Some(param_types);
                                break;
                            }
                        }

                        // If no exact match, fall back to first signature with matching arg count
                        if best_match.is_none() {
                            for (param_types, _) in signatures.iter() {
                                if param_types.len() == call.args.len() {
                                    best_match = Some(param_types);
                                    break;
                                }
                            }
                        }

                        if let Some(param_types) = best_match {
                            for (pos, var_name) in &empty_list_positions {
                                if let Some(param_type) = param_types.get(*pos) {
                                    if param_type.starts_with("List_") {
                                        debug_println!(self,
                                            "DEBUG preanalyze_empty_lists: variable '{}' passed to {}() param {}, inferred type '{}'",
                                            var_name, call.name, pos, param_type
                                        );
                                        self.variable_types
                                            .insert(var_name.clone(), param_type.clone());
                                    }
                                }
                            }
                        }
                    }
                }
                // Recurse into call arguments
                for arg in &call.args {
                    self.analyze_list_usage_in_expr(&*arg.init, empty_list_vars);
                }
            }
            parser::Expression::Switch(switch_expr) => {
                // Recurse into switch case bodies
                for case in &switch_expr.cases {
                    self.analyze_list_usage_in_stmts(&case.body, empty_list_vars);
                }
            }
            _ => {}
        }
    }

    /// Extract the root identifier name from an expression, stripping `.mut` member accesses.
    /// For example: `stubs.mut` -> Some("stubs"), `stubs` -> Some("stubs"), `foo.bar` -> None
    fn extract_root_ident(expr: &parser::Expression) -> Option<String> {
        match expr {
            parser::Expression::Ident(ident) => Some(ident.name.clone()),
            parser::Expression::Member(member) => {
                if member.member == "mut" {
                    Self::extract_root_ident(&member.parent)
                } else {
                    None
                }
            }
            _ => None,
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
                    debug_println!(self, 
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
                        debug_println!(self, 
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
        // IMPORTANT: Prioritize TokenKind since many variants (concat, add, sub, etc.)
        // exist in both TokenKind and Operator/Assignment, and TokenKind is more common
        // in the lexer context where these ambiguities arise
        if let Some(variants) = self.enum_variants.get("TokenKind") {
            if variants.contains(ident_name) {
                debug_println!(
                    self,
                    "DEBUG qualify_identifier: '{}' found in enum 'TokenKind', returning '{}'",
                    ident_name,
                    format!("TokenKind_{}", ident_name)
                );
                return format!("TokenKind_{}", ident_name);
            }
        }
        // Then check other enums
        for (enum_name, variants) in &self.enum_variants {
            if enum_name == "TokenKind" {
                continue; // Already checked
            }
            if variants.contains(ident_name) {
                debug_println!(
                    self,
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
            debug_println!(
                self,
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

    /// Get a printf format specifier for a given C type
    #[allow(dead_code)]
    fn get_format_specifier(&self, c_type: &str) -> &'static str {
        match c_type {
            "char*" | "String" => "%s",
            "int64_t" | "Int" => "%lld",
            "int" => "%d",
            "double" | "Float" | "float" => "%f",
            "bool" | "Bool" => "%s", // We'll need to convert bool to "true"/"false"
            "uint32_t" | "Rune" => "%u",
            _ if c_type.starts_with("List_") => "<list>",
            _ if c_type.ends_with("*") => "%p",
            _ => "%p", // Default to pointer format for unknown types
        }
    }

    /// Generate a C expression that converts a value of the given type to a char* string.
    /// Registers the type in `display_needed` if a helper function is required.
    fn get_display_expr(&mut self, c_type: &str, val_expr: &str) -> String {
        if c_type == "bool" || c_type == "Bool" {
            format!("({} ? \"true\" : \"false\")", val_expr)
        } else if c_type == "int64_t" || c_type == "Int" {
            // Inline: allocate and snprintf an integer
            format!("({{ char* _buf = (char*)malloc(32); snprintf(_buf, 32, \"%lld\", (long long)({})); _buf; }})", val_expr)
        } else if c_type == "char*" || c_type == "String" {
            val_expr.to_string()
        } else if c_type == "double" || c_type == "Float" {
            format!("({{ char* _buf = (char*)malloc(64); snprintf(_buf, 64, \"%g\", (double)({})); _buf; }})", val_expr)
        } else if c_type == "uint32_t" || c_type == "Rune" {
            format!("({{ char* _buf = (char*)malloc(16); snprintf(_buf, 16, \"%u\", (unsigned)({})); _buf; }})", val_expr)
        } else if c_type.starts_with("List_") {
            // Generate a display function for this list type
            let elem_type = c_type.strip_prefix("List_").unwrap().to_string();
            self.display_needed.insert(c_type.to_string());
            // Also need display for element type (for recursion)
            self.ensure_display_for_type(&elem_type);
            format!("blitz_display_{}({})", c_type, val_expr)
        } else if c_type.starts_with("Option_") {
            self.display_needed.insert(c_type.to_string());
            let inner = c_type.strip_prefix("Option_").unwrap().to_string();
            self.ensure_display_for_type(&inner);
            format!("blitz_display_{}({})", c_type, val_expr)
        } else if self.enum_types.contains(c_type) {
            self.display_needed.insert(c_type.to_string());
            format!("blitz_display_{}({})", c_type, val_expr)
        } else if self.tagged_union_types.contains(c_type) {
            // Tagged union pointer - show tag name
            self.display_needed.insert(c_type.to_string());
            format!("blitz_display_{}({})", c_type, val_expr)
        } else if c_type.ends_with("*") {
            // Pointer to struct - try to recognize the base type
            let base = c_type.trim_end_matches('*').trim();
            if self.tagged_union_types.contains(base) {
                self.display_needed.insert(base.to_string());
                format!("blitz_display_{}({})", base, val_expr)
            } else {
                // Generic struct pointer - show address
                format!("({{ char* _buf = (char*)malloc(32); snprintf(_buf, 32, \"<ptr %p>\", (void*)({})); _buf; }})", val_expr)
            }
        } else {
            // Unknown type - show as pointer
            format!("({{ char* _buf = (char*)malloc(32); snprintf(_buf, 32, \"<ptr %p>\", (void*)({})); _buf; }})", val_expr)
        }
    }

    /// Ensure that a display function will be generated for the given type.
    /// For primitive types this is a no-op (they are handled inline).
    fn ensure_display_for_type(&mut self, type_name: &str) {
        match type_name {
            "bool" | "Bool" | "int64_t" | "Int" | "char*" | "String" | "double" | "Float"
            | "uint32_t" | "Rune" | "void" => {
                // Primitive types are handled inline, no function needed
            }
            _ => {
                self.display_needed.insert(type_name.to_string());
            }
        }
    }

    /// Get the C parameter type for a display function.
    /// Enums and List types are passed by value; tagged unions and structs by pointer.
    fn display_param_type(&self, type_name: &str) -> String {
        if type_name.starts_with("List_") {
            type_name.to_string()
        } else if type_name.starts_with("Option_") {
            type_name.to_string()
        } else if self.enum_types.contains(type_name) {
            type_name.to_string()
        } else if self.tagged_union_types.contains(type_name) {
            format!("{}*", type_name)
        } else {
            // Struct or unknown - pass by pointer
            format!("{}*", type_name)
        }
    }

    /// Generate inline C expression to convert a primitive value to char*.
    /// Used inside generated display functions for element rendering.
    fn display_elem_expr(type_name: &str, val_expr: &str) -> String {
        match type_name {
            "bool" | "Bool" => format!("({} ? \"true\" : \"false\")", val_expr),
            "int64_t" | "Int" => format!("({{ char* _b = (char*)malloc(32); snprintf(_b, 32, \"%lld\", (long long)({})); _b; }})", val_expr),
            "char*" | "String" => val_expr.to_string(),
            "double" | "Float" => format!("({{ char* _b = (char*)malloc(64); snprintf(_b, 64, \"%g\", (double)({})); _b; }})", val_expr),
            "uint32_t" | "Rune" => format!("({{ char* _b = (char*)malloc(16); snprintf(_b, 16, \"%u\", (unsigned)({})); _b; }})", val_expr),
            _ => format!("blitz_display_{}({})", type_name, val_expr),
        }
    }

    /// Infer the C type of an expression
    fn infer_expr_type(&self, expr: &parser::Expression) -> String {
        // Debug: log what expression we're inferring
        if let parser::Expression::BinaryOp(binop) = expr {
            if matches!(binop.op, parser::Operator::Else) {
                if let parser::Expression::If(if_expr) = &*binop.left {
                    debug_println!(
                        self,
                        "DEBUG infer_expr_type: ENTERING if-else case, body len={}",
                        if_expr.body.len()
                    );
                }
            }
        }
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
                // Handle noreturn functions that return void
                if call.name == "panic" || call.name == "todo" {
                    return "void".to_string();
                }

                // Handle err() which also returns void in parser context
                if call.name == "err" && call.args.len() == 2 {
                    // err(parser, msg) is a void function
                    return "void".to_string();
                }

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
                    // Normalize type names so that Blitz types (from signatures) and
                    // C types (from infer_expr_type) compare equal.  This must match
                    // the normalize_type helper used in resolve_function_call.
                    fn normalize_type(s: &str) -> &str {
                        match s.trim_end_matches('*') {
                            "char" | "String" => "String",
                            "int64_t" | "Int" => "Int",
                            "bool" | "Bool" => "Bool",
                            "double" | "Float" => "Float",
                            "uint32_t" | "Rune" => "Rune",
                            other => other,
                        }
                    }

                    // Find matching signature
                    for (param_types, ret_type) in signatures {
                        if param_types.len() == arg_types.len() {
                            let matches = param_types
                                .iter()
                                .zip(arg_types.iter())
                                .all(|(p, a)| normalize_type(p) == normalize_type(a));
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

                // Check if this is a known void function
                if self.void_functions.contains(&call.name) {
                    return "void".to_string();
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
                    "parse_expression" => return self.option_c_name("Expression", "ast"),
                    "parse_expression_bp" => return self.option_c_name("Expression", "ast"),
                    "parse_statement" => return self.option_c_name("Statement", "ast"),
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
                    "parse_if" => return self.option_c_name("If", "ast"),
                    "parse_while" => return "Option_While".to_string(),
                    "parse_for" => return "Option_For".to_string(),
                    "parse_switch" => return self.option_c_name("Switch", "ast"),
                    "parse_switch_label" => return "Option_SwitchLabel".to_string(),
                    "parse_member_call" => return self.option_c_name("Expression", "ast"),

                    // Parser literal functions - these return Expression variants, not standalone types
                    "parse_int_lit" => return self.option_c_name("Expression", "ast"),
                    "parse_float_lit" => return self.option_c_name("Expression", "ast"),
                    "parse_string_lit" => return self.option_c_name("Expression", "ast"),
                    "parse_char_lit" => return self.option_c_name("Expression", "ast"),

                    // Parser call/constructor functions - these return Expression variants
                    "parse_call" => return self.option_c_name("Expression", "ast"),
                    "parse_constructor" => return self.option_c_name("Expression", "ast"),
                    "parse_group" => return self.option_c_name("Expression", "ast"),
                    "parse_list" => return self.option_c_name("Expression", "ast"),
                    "parse_precedence" => return self.option_c_name("Expression", "ast"),

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
                        // Check if this is an if-else expression (left is If) vs Option unwrapping
                        if let parser::Expression::If(if_expr) = &*binop.left {
                            // This is an if-else expression: if cond { a } else { b }
                            // Infer from the last expression in the then body
                            if let Some(parser::Statement::Expression(last_expr)) =
                                if_expr.body.last()
                            {
                                return self.infer_expr_type(last_expr);
                            }
                            // Fallback: try the else branch
                            return self.infer_expr_type(&binop.right);
                        }
                        // Else operator unwraps Option types
                        // If left is Option_T, the result is T* for structs, or T for primitives/lists
                        let left_type = self.infer_expr_type(&binop.left);
                        if let Some(inner) = left_type.strip_prefix("Option_") {
                            // Check if it's a primitive type or List type that doesn't use pointers
                            let is_generic_list = inner.starts_with("List_") && inner != "List_";
                            if self.is_primitive_type(inner) || is_generic_list {
                                self.map_type_name(inner)
                            } else {
                                // The .value field of Option holds pointers for struct/enum types
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
                    "parse_expression" => return self.option_c_name("Expression", "ast"),
                    "parse_statement" => return self.option_c_name("Statement", "ast"),
                    // Struct field types - these are pointer fields
                    "range" => return "Range*".to_string(),
                    "span" => return "Span*".to_string(),
                    "path" => return "char*".to_string(),
                    "name" => {
                        // 'name' is String for Ident, Type, Module but Ident for Fn, Arg, Field, Call, Declaration, etc.
                        let parent_type = self.infer_expr_type(&member.parent);
                        let struct_name = parent_type.trim_end_matches('*');
                        if let Some(fields) = self.struct_field_types.get(struct_name) {
                            if let Some(field_type) = fields.get("name") {
                                return self.map_type_name(field_type);
                            }
                        }
                        // Fallback to char* (String) for backwards compatibility
                        return "char*".to_string();
                    }
                    "kind" => {
                        // Check the parent type to determine which 'kind' field this is
                        let parent_type = self.infer_expr_type(&member.parent);
                        let struct_name = parent_type.trim_end_matches('*');
                        if let Some(fields) = self.struct_field_types.get(struct_name) {
                            if let Some(field_type) = fields.get("kind") {
                                return self.map_type_name(field_type);
                            }
                        }
                        // Fallback to TokenKind for Token and other common types
                        return "TokenKind".to_string();
                    }
                    // List fields
                    "args" => {
                        // 'args' is List(CallArg) for Call/Constructor, List(Arg) for Fn, List(TypeRef) for Nominal
                        let parent_type = self.infer_expr_type(&member.parent);
                        let struct_name = parent_type.trim_end_matches('*');
                        if let Some(fields) = self.struct_field_types.get(struct_name) {
                            if let Some(field_type) = fields.get("args") {
                                return self.map_type_name(field_type);
                            }
                        }
                        // Fallback to List_CallArg for backwards compatibility
                        return "List_CallArg".to_string();
                    }
                    "elems" => {
                        return format!("List_{}", self.resolve_c_name_for("Expression", "ast"))
                    }
                    "cases" => {
                        // Could be List_SwitchCase or List_Case depending on context
                        let parent_type = self.infer_expr_type(&member.parent);
                        if parent_type.contains("Switch") {
                            return "List_SwitchCase".to_string();
                        } else if parent_type.contains("Union") {
                            return "List_Case".to_string();
                        }
                        return "List_SwitchCase".to_string();
                    }
                    "body" => {
                        // 'body' can be Block (for Fn, While, For, etc.) or List_Statement
                        // Check the parent type to determine which struct's body field
                        let parent_type = self.infer_expr_type(&member.parent);
                        let struct_name = parent_type.trim_end_matches('*');
                        if let Some(fields) = self.struct_field_types.get(struct_name) {
                            if let Some(field_type) = fields.get("body") {
                                return self.map_type_name(field_type);
                            }
                        }
                        // Fallback to List_Statement for backwards compatibility
                        return "List_Statement".to_string();
                    }
                    "params" => {
                        // 'params' is List(Type) for Type, List(TypeRef) for FuncDef
                        let parent_type = self.infer_expr_type(&member.parent);
                        let struct_name = parent_type.trim_end_matches('*');
                        if let Some(fields) = self.struct_field_types.get(struct_name) {
                            if let Some(field_type) = fields.get("params") {
                                return self.map_type_name(field_type);
                            }
                        }
                        // Fallback to List_Arg for backwards compatibility
                        return "List_Arg".to_string();
                    }
                    "fields" => {
                        // 'fields' is List(Field) for Struct, List(FieldDef) for StructDef
                        let parent_type = self.infer_expr_type(&member.parent);
                        let struct_name = parent_type.trim_end_matches('*');
                        if let Some(fields_map) = self.struct_field_types.get(struct_name) {
                            if let Some(field_type) = fields_map.get("fields") {
                                return self.map_type_name(field_type);
                            }
                        }
                        // Fallback to List_Field for backwards compatibility
                        return "List_Field".to_string();
                    }
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

                // Try to infer parent type and look up field type from struct_field_types
                let parent_type = self.infer_expr_type(&member.parent);
                // Strip pointer notation to get struct name
                let struct_name = parent_type.trim_end_matches('*');

                // Special case: parser method references without parentheses
                // e.g., `parser.mut.parse_ident` is a method call, not a field access
                if member_name.starts_with("parse_") && struct_name == "Parser" {
                    // Look up the return type as if it were a function call
                    if let Some(ret_type) = self.function_return_types.get(member_name) {
                        return self.map_type(ret_type);
                    }
                    // Fall back to hardcoded return types for known parser functions
                    match member_name.as_str() {
                        "parse_def" => return "Option_Definition".to_string(),
                        "parse_ident" => return "Option_Ident".to_string(),
                        "parse_type" => return "Option_Type".to_string(),
                        "parse_expression" => return self.option_c_name("Expression", "ast"),
                        "parse_statement" => return self.option_c_name("Statement", "ast"),
                        _ => {}
                    }
                }

                if let Some(fields) = self.struct_field_types.get(struct_name) {
                    if let Some(field_type) = fields.get(member_name) {
                        return self.map_type_name(field_type);
                    }
                }

                // Default: can't infer member type
                "int64_t".to_string()
            }
            parser::Expression::Ident(ident) => {
                // Special case: 'none' is an Option literal that we can't type without context
                // Return a placeholder that will hopefully be corrected by context
                if ident.name == "none" {
                    // If we have a current return type that's Option, use that
                    if let Some(ref ret_ty) = self.current_return_type {
                        let mapped = self.map_type(ret_ty);
                        if mapped.starts_with("Option_") {
                            return mapped;
                        }
                    }
                    // Otherwise return a placeholder
                    return "Option_void".to_string();
                }

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
                // Prioritize TokenKind since it's the most common case in the lexer
                // and many TokenKind variants have the same name as Operator/Assignment variants
                if let Some(variants) = self.enum_variants.get("TokenKind") {
                    if variants.contains(&ident.name) {
                        return "TokenKind".to_string();
                    }
                }
                // Then check other enums
                for (enum_name, variants) in &self.enum_variants {
                    if enum_name == "TokenKind" {
                        continue; // Already checked
                    }
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
                                // Check if this is an Option of a variant type that belongs to a tagged union
                                // e.g., Option_For -> Option_ast_Expression, Option_ast_If -> Option_ast_Expression
                                if let Some(inner) = inferred.strip_prefix("Option_") {
                                    let inner_base = inner.trim_end_matches('*');
                                    if let Some(union_set) = self.variant_to_union.get(inner_base) {
                                        let union_name = union_set.iter().next().cloned().unwrap();
                                        return format!("Option_{}", union_name);
                                    }
                                }

                                // Check if this is a pointer to a variant type that belongs to a tagged union
                                // e.g., For* -> ast_Expression*, ast_If* -> ast_Expression*
                                if let Some(base) = inferred.strip_suffix('*') {
                                    if let Some(union_set) = self.variant_to_union.get(base) {
                                        let union_name = union_set.iter().next().cloned().unwrap();
                                        return format!("{}*", union_name);
                                    }
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
                    // Empty list - try to infer from return type context
                    if let Some(ref ret_type) = self.current_return_type {
                        let mapped = self.map_type(ret_type);
                        if mapped.starts_with("List_") {
                            return mapped;
                        }
                    }
                    // Fallback - can't infer element type
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

                // Check if all elements are constructors
                let all_constructors = list
                    .elems
                    .iter()
                    .all(|e| matches!(e, parser::Expression::Constructor(_)));
                if all_constructors && !list.elems.is_empty() {
                    if let parser::Expression::Constructor(first_ctor) = &list.elems[0] {
                        let elem_type = self.type_name_for_instance(&first_ctor.r#type);
                        return format!("List_{}", elem_type);
                    }
                }

                // General fallback: infer from first element
                if !list.elems.is_empty() {
                    let elem_type = self.infer_expr_type(&list.elems[0]);
                    let base_type = elem_type.trim_end_matches('*');
                    return format!("List_{}", base_type);
                }

                // Fallback to generic list
                "List_Int".to_string()
            }
            parser::Expression::If(if_expr) => {
                // Standalone if (no else) returns Option<T> where T is the body's result type
                // Try to infer T from the last expression in the body
                if let Some(parser::Statement::Expression(last_expr)) = if_expr.body.last() {
                    // If the last expression is an identifier, check if it was declared
                    // earlier in the if body and infer its type from the declaration.
                    let body_type = if let parser::Expression::Ident(ident) = last_expr {
                        let mut found_type = None;
                        for stmt in &if_expr.body {
                            if let parser::Statement::Declaration(decl) = stmt {
                                if decl.name == ident.name {
                                    if let Some(init_expr) = &decl.init {
                                        found_type = Some(self.infer_expr_type(init_expr));
                                    }
                                }
                            }
                        }
                        found_type.unwrap_or_else(|| self.infer_expr_type(last_expr))
                    } else {
                        self.infer_expr_type(last_expr)
                    };
                    // Wrap in Option type
                    if body_type.starts_with("Option_") {
                        return body_type;
                    } else if body_type == "void" || body_type == "int64_t" {
                        // For void or int64_t, the if is likely used as a statement
                        return "void".to_string();
                    } else {
                        // The result is Option<T>
                        let base = body_type.trim_end_matches('*');
                        return format!("Option_{}", base);
                    }
                }
                // If body is empty or has no expression, treat as void
                "void".to_string()
            }
            parser::Expression::Index(idx) => {
                // Indexing into a List_T returns T or T* depending on whether T is a value type
                let target_type = self.infer_expr_type(&idx.target);
                if let Some(elem_type) = target_type.strip_prefix("List_") {
                    // Value types are returned directly (not as pointers)
                    // - Rune (uint32_t) is a value type
                    // - Int (int64_t) is a value type
                    // - Float (double) is a value type
                    // - Bool (bool) is a value type
                    match elem_type {
                        "Rune" => "uint32_t".to_string(),
                        "Int" => "int64_t".to_string(),
                        "Float" => "double".to_string(),
                        "Bool" => "bool".to_string(),
                        // Struct types get pointer
                        _ => format!("{}*", elem_type),
                    }
                } else if target_type == "char*" || target_type == "String" {
                    // String indexing returns a character
                    "uint32_t".to_string()
                } else {
                    // Unknown indexing, fallback to the target type
                    target_type
                }
            }
            parser::Expression::Assignment(assign) => {
                // Assignment expressions return the type of the assigned value
                // This is important for if-else expressions that end with assignments
                self.infer_expr_type(&assign.right)
            }
            parser::Expression::Block(stmts) => {
                // Block expressions return the type of the last expression
                if let Some(parser::Statement::Expression(last_expr)) = stmts.last() {
                    self.infer_expr_type(last_expr)
                } else {
                    "void".to_string()
                }
            }
            parser::Expression::For(for_expr) => {
                // A for-expression with a single expression body collects values into a list.
                // Infer the element type from the body expression and wrap it in List_.
                if for_expr.body.len() == 1 {
                    if let parser::Statement::Expression(body_expr) = &for_expr.body[0] {
                        let elem_type = self.infer_expr_type(body_expr);
                        if elem_type != "void" {
                            let base = elem_type.trim_end_matches('*');
                            return format!("List_{}", base);
                        }
                    }
                }
                "void".to_string()
            }
            _ => "int64_t".to_string(), // Fallback for complex expressions
        }
    }

    /// Infer the result type of an if-else expression
    fn infer_if_else_result_type(
        &self,
        then_body: &[parser::Statement],
        else_expr: &parser::Expression,
    ) -> String {
        // Try to infer from the last expression of the then body
        if let Some(parser::Statement::Expression(expr)) = then_body.last() {
            let then_type = self.infer_expr_type(expr);
            if then_type != "int64_t" {
                return then_type;
            }
        }

        // Try to infer from the else body
        match else_expr {
            parser::Expression::Block(stmts) => {
                if let Some(parser::Statement::Expression(expr)) = stmts.last() {
                    return self.infer_expr_type(expr);
                }
            }
            _ => return self.infer_expr_type(else_expr),
        }

        "int64_t".to_string()
    }

    /// Generate the result expression from a block of statements
    /// Returns the last expression as a string
    #[allow(dead_code)]
    fn generate_block_result(&mut self, stmts: &[parser::Statement], is_main: bool) -> String {
        if stmts.is_empty() {
            return "(void)0".to_string();
        }

        // Generate all but the last statement as side effects
        let mut result = String::new();
        for stmt in stmts.iter().take(stmts.len().saturating_sub(1)) {
            let stmt_code = self.generate_statement(stmt, is_main);
            result.push_str(&format!("{} ", stmt_code));
        }

        // The last statement is the result
        if let Some(last_stmt) = stmts.last() {
            match last_stmt {
                parser::Statement::Expression(expr) => {
                    if result.is_empty() {
                        return self.generate_expression(expr, is_main);
                    } else {
                        let expr_code = self.generate_expression(expr, is_main);
                        // Use statement expression for multi-statement blocks
                        // The last expression in a GCC statement expression still
                        // needs a trailing semicolon to form a valid expression statement.
                        return format!("({{ {}{}; }})", result, expr_code);
                    }
                }
                _ => {
                    let stmt_code = self.generate_statement(last_stmt, is_main);
                    return format!("({{ {}{} 0; }})", result, stmt_code);
                }
            }
        }

        "(void)0".to_string()
    }

    /// Normalize a type name to a valid C type name
    fn normalize_c_type(&self, type_name: &str) -> String {
        match type_name {
            "int64_t" | "Int" => "Int".to_string(),
            "bool" | "Bool" => "bool".to_string(),
            "char*" | "String" | "Str" => "char*".to_string(),
            "uint32_t" | "Rune" => "uint32_t".to_string(),
            "void" => "void".to_string(),
            other => other.to_string(),
        }
    }

    /// Generate the body of an if-else branch, producing statements plus an assignment to _if_result
    fn generate_if_else_branch_body(
        &mut self,
        stmts: &[parser::Statement],
        is_main: bool,
    ) -> String {
        self.generate_if_else_branch_body_with_type(stmts, is_main, "int64_t")
    }

    /// Generate the body of an if-else branch with expected result type for empty list handling
    fn generate_if_else_branch_body_with_type(
        &mut self,
        stmts: &[parser::Statement],
        is_main: bool,
        expected_type: &str,
    ) -> String {
        if stmts.is_empty() {
            return "_if_result = (void)0;".to_string();
        }

        let mut result = String::new();

        // Generate all but the last statement
        for stmt in stmts.iter().take(stmts.len().saturating_sub(1)) {
            let stmt_code = self.generate_statement(stmt, is_main);
            result.push_str(&stmt_code);
            result.push(' ');
        }

        // The last statement should be assigned to _if_result
        if let Some(last_stmt) = stmts.last() {
            match last_stmt {
                parser::Statement::Expression(expr) => {
                    // Check if this is a control flow statement - those don't need assignment
                    if matches!(
                        expr,
                        parser::Expression::Return(_)
                            | parser::Expression::Break
                            | parser::Expression::Continue
                    ) {
                        let expr_code = self.generate_expression(expr, is_main);
                        result.push_str(&format!("{};", expr_code));
                    } else if let parser::Expression::List(list) = expr {
                        // Special handling for empty list with expected type
                        if list.elems.is_empty() && expected_type.starts_with("List_") {
                            result.push_str(&format!(
                                "_if_result = ({}){{{}.data = NULL, .len = 0, .cap = 0}};",
                                expected_type, ""
                            ));
                        } else {
                            let expr_code = self.generate_expression(expr, is_main);
                            result.push_str(&format!("_if_result = {};", expr_code));
                        }
                    } else if let parser::Expression::Ident(ident_expr) = expr {
                        if ident_expr.name == "none" && expected_type.starts_with("Option_") {
                            // 'none' in an if-else branch — use the expected Option type
                            result.push_str(&format!(
                                "_if_result = ({}){{.tag = {}_tag_none}};",
                                expected_type, expected_type
                            ));
                        } else {
                            let expr_code = self.generate_expression(expr, is_main);
                            let expr_type = self.infer_expr_type(expr);
                            if expr_type == "void" {
                                result.push_str(&format!("{};", expr_code));
                            } else {
                                result.push_str(&format!("_if_result = {};", expr_code));
                            }
                        }
                    } else {
                        let expr_code = self.generate_expression(expr, is_main);
                        // Check if expression returns void
                        let expr_type = self.infer_expr_type(expr);
                        if expr_type == "void" {
                            // Void expression - execute it, but don't assign
                            result.push_str(&format!("{};", expr_code));
                        } else {
                            result.push_str(&format!("_if_result = {};", expr_code));
                        }
                    }
                }
                _ => {
                    // Non-expression statement (like variable declaration)
                    let stmt_code = self.generate_statement(last_stmt, is_main);
                    result.push_str(&stmt_code);
                    result.push_str(" _if_result = 0;");
                }
            }
        }

        result
    }

    /// Generate the body of a standalone if branch, wrapping result in Option
    fn generate_if_branch_body_with_option_wrap(
        &mut self,
        stmts: &[parser::Statement],
        is_main: bool,
        option_type: &str,
    ) -> String {
        if stmts.is_empty() {
            return format!(
                "_if_result = ({}){{.tag = {}_tag_none}};",
                option_type, option_type
            );
        }

        let mut result = String::new();

        // Generate all but the last statement
        for stmt in stmts.iter().take(stmts.len().saturating_sub(1)) {
            let stmt_code = self.generate_statement(stmt, is_main);
            result.push_str(&stmt_code);
            result.push(' ');
        }

        // The last statement should be wrapped in Option and assigned to _if_result
        if let Some(last_stmt) = stmts.last() {
            match last_stmt {
                parser::Statement::Expression(expr) => {
                    // Check if this is a control flow statement - those don't need assignment
                    if matches!(
                        expr,
                        parser::Expression::Return(_)
                            | parser::Expression::Break
                            | parser::Expression::Continue
                    ) {
                        let expr_code = self.generate_expression(expr, is_main);
                        result.push_str(&format!("{};", expr_code));
                    } else {
                        let expr_code = self.generate_expression(expr, is_main);
                        // Check if the expression already returns an Option type
                        let expr_type = self.infer_expr_type(expr);
                        let expr_c_type = self.normalize_c_type(&expr_type);

                        if expr_c_type.starts_with("Option_") {
                            // Expression already returns Option, just assign directly
                            result.push_str(&format!("_if_result = {};", expr_code));
                        } else if expr_c_type == "void" {
                            // Void-returning expression - execute it, then set result to none
                            result.push_str(&format!(
                                "{}; _if_result = ({}){{.tag = {}_tag_none}};",
                                expr_code, option_type, option_type
                            ));
                        } else {
                            // Wrap in Option
                            result.push_str(&format!(
                                "_if_result = ({}){{.tag = {}_tag_some, .value = {}}};",
                                option_type, option_type, expr_code
                            ));
                        }
                    }
                }
                _ => {
                    // Non-expression statement (like variable declaration)
                    let stmt_code = self.generate_statement(last_stmt, is_main);
                    result.push_str(&stmt_code);
                    result.push_str(&format!(
                        " _if_result = ({}){{.tag = {}_tag_none}};",
                        option_type, option_type
                    ));
                }
            }
        }

        result
    }

    /// Collect all branches of an if-elif-else chain
    #[allow(dead_code)]
    fn collect_if_else_branches(
        &mut self,
        expr: &parser::Expression,
        is_main: bool,
        branches: &mut Vec<(String, String)>,
        final_else: &mut Option<String>,
    ) {
        self.collect_if_else_branches_with_type(
            expr,
            is_main,
            branches,
            final_else,
            &"int64_t".to_string(),
        )
    }

    /// Collect all branches of an if-elif-else chain, with expected result type
    fn collect_if_else_branches_with_type(
        &mut self,
        expr: &parser::Expression,
        is_main: bool,
        branches: &mut Vec<(String, String)>,
        final_else: &mut Option<String>,
        result_type: &str,
    ) {
        debug_println!(
            self,
            "DEBUG collect_if_else_branches_with_type: expr={:?}, result_type='{}'",
            std::mem::discriminant(expr),
            result_type
        );
        match expr {
            parser::Expression::BinaryOp(binop)
                if matches!(binop.op, parser::Operator::Else)
                    && matches!(&*binop.left, parser::Expression::If(_)) =>
            {
                // This is an elif: if cond { body } else ...
                if let parser::Expression::If(if_expr) = &*binop.left {
                    let cond = self.generate_expression(&if_expr.cond, is_main);
                    let body = self.generate_if_else_branch_body(&if_expr.body, is_main);
                    branches.push((cond, body));

                    // Recursively process the rest
                    self.collect_if_else_branches_with_type(
                        &binop.right,
                        is_main,
                        branches,
                        final_else,
                        result_type,
                    );
                }
            }
            parser::Expression::Block(stmts) => {
                // Final else block - pass result type for empty list handling
                *final_else =
                    Some(self.generate_if_else_branch_body_with_type(stmts, is_main, result_type));
            }
            parser::Expression::List(list) if list.elems.is_empty() => {
                // Empty list in else branch - use the inferred result type
                debug_println!(
                    self,
                    "DEBUG collect_if_else: empty list, result_type='{}'",
                    result_type
                );
                if result_type.starts_with("List_") {
                    *final_else = Some(format!(
                        "_if_result = ({}){{{}.data = NULL, .len = 0, .cap = 0}};",
                        result_type, ""
                    ));
                } else {
                    // Fall back to default generation
                    let expr_code = self.generate_expression(expr, is_main);
                    *final_else = Some(format!("_if_result = {};", expr_code));
                }
            }
            _ => {
                // Single expression as final else
                // Handle 'none' by generating proper Option none constructor
                if let parser::Expression::Ident(ident_expr) = expr {
                    if ident_expr.name == "none" && result_type.starts_with("Option_") {
                        *final_else = Some(format!(
                            "_if_result = ({}){{.tag = {}_tag_none}};",
                            result_type, result_type
                        ));
                        return;
                    }
                }
                let expr_code = self.generate_expression(expr, is_main);
                *final_else = Some(format!("_if_result = {};", expr_code));
            }
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
                    '\0' => "\\0".to_string(),
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
                            // But NOT if the expression is a noreturn/void function like todo() or panic()
                            let is_noreturn_call = matches!(&**ret_expr, parser::Expression::Call(call) 
                                if matches!(call.name.as_str(), "todo" | "panic" | "unreachable" | "assert"));

                            if !is_noreturn_call {
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
                    // For none outside of a function returning Option, we need context
                    // Generate a placeholder that will need to be typed by context
                    // This happens in declarations like `let x = none`
                    // For now, return 'none' and let it fail - this is a limitation
                    debug_println!(
                        self,
                        "DEBUG: Found 'none' without Option context, generating bare 'none'"
                    );
                    return "none".to_string();
                }

                // Check if this is a label-only tagged union variant
                // e.g., "default" in SwitchLabel union
                debug_println!(self, 
                    "DEBUG generate_expression Ident: checking '{}' against tagged_union_labels: {:?}",
                    ident.name, self.tagged_union_labels
                );
                for (union_name, labels) in &self.tagged_union_labels {
                    if labels.contains(&ident.name) {
                        // Escape C reserved keywords in the tag name
                        let tag_label = if ident.name == "default" {
                            "default".to_string() // Will be used as SwitchLabel_tag_default
                        } else {
                            ident.name.clone()
                        };
                        debug_println!(self, 
                            "DEBUG: Found label-only variant '{}' in union '{}', generating constructor",
                            ident.name, union_name
                        );
                        // Generate: memcpy(malloc(sizeof(UnionName)), &((UnionName){.tag = UnionName_tag_label}), sizeof(UnionName))
                        return format!(
                            "memcpy(malloc(sizeof({union})), &(({union}){{.tag = {union}_tag_{label}}}), sizeof({union}))",
                            union = union_name,
                            label = tag_label
                        );
                    }
                }

                // Check if this is an enum variant that needs qualification
                let var_name = self.get_variable_name(&ident.name);

                // Check if this is a mut value-type parameter passed as pointer - dereference it
                if self.mut_pointer_params.contains_key(&var_name) {
                    format!("(*{})", var_name)
                } else if var_name == ident.name {
                    // If the variable doesn't exist in our mappings, it might be an enum variant
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
                        // Concatenation - could be string, list++element, or list++list
                        let left = self.generate_expression(&binop.left, is_main);
                        let right = self.generate_expression(&binop.right, is_main);

                        // Infer type of operands to decide what kind of concatenation
                        let left_type = self.infer_expr_type(&binop.left);
                        let right_type = self.infer_expr_type(&binop.right);

                        if left_type.starts_with("List_") {
                            // List operation
                            let elem_type = left_type
                                .strip_prefix("List_")
                                .unwrap_or("void")
                                .to_string();

                            if right_type.starts_with("List_") {
                                // List ++ List concatenation
                                self.register_list_concat_needed(elem_type.clone());
                                return format!(
                                    "blitz_list_concat_{}({}, {})",
                                    elem_type, left, right
                                );
                            } else {
                                // List ++ element append
                                self.register_list_append_needed(elem_type.clone());

                                // If right is an Option type, we need to unwrap it with .value
                                let right_unwrapped = if right_type.starts_with("Option_") {
                                    format!("{}.value", right)
                                } else {
                                    right
                                };

                                return format!(
                                    "blitz_list_append_{}({}, {})",
                                    elem_type, left, right_unwrapped
                                );
                            }
                        } else {
                            // String concatenation
                            // Check if right side is a Rune (uint32_t) - use blitz_string_append_rune
                            if right_type == "uint32_t" || right_type == "Rune" {
                                return format!("blitz_string_append_rune({}, {})", left, right);
                            }
                            // Otherwise use blitz_string_concat for string ++ string
                            return format!("blitz_string_concat({}, {})", left, right);
                        }
                    }
                    parser::Operator::Else => {
                        // The 'else' operator is used for two different purposes:
                        // 1. If-else branching: if cond { a } else { b }
                        // 2. Error handling/unwrapping: expr else fallback

                        // Check if the left side is an If expression - then this is if-else branching
                        if let parser::Expression::If(if_expr) = &*binop.left {
                            // This is an if-else expression
                            // Collect all branches of the if-elif-else chain
                            let mut branches: Vec<(String, String)> = Vec::new(); // (condition, body)
                            let mut final_else: Option<String> = None;

                            // Infer the result type from first branch BEFORE collecting
                            // This allows us to use it when generating empty list branches
                            let result_type =
                                self.infer_if_else_result_type(&if_expr.body, &binop.right);

                            // First branch from the initial if
                            let first_cond = self.generate_expression(&if_expr.cond, is_main);
                            let first_body =
                                self.generate_if_else_branch_body(&if_expr.body, is_main);
                            branches.push((first_cond, first_body));

                            // Collect elif branches and final else, passing the result type
                            debug_println!(self, "DEBUG: calling collect_if_else_branches_with_type, result_type='{}'", result_type);
                            self.collect_if_else_branches_with_type(
                                &binop.right,
                                is_main,
                                &mut branches,
                                &mut final_else,
                                &result_type,
                            );

                            // Generate the statement expression
                            // ({ T _if_result; if (c1) { ... _if_result = v1; } else if (c2) { ... _if_result = v2; } else { ... _if_result = v3; } _if_result; })

                            // Special case: if result type is void, generate a plain if-else statement
                            if result_type == "void" {
                                let mut code = String::new();
                                for (i, (cond, body)) in branches.iter().enumerate() {
                                    if i == 0 {
                                        code.push_str(&format!("if ({}) {{ {} }}", cond, body));
                                    } else {
                                        code.push_str(&format!(
                                            " else if ({}) {{ {} }}",
                                            cond, body
                                        ));
                                    }
                                }

                                if let Some(else_body) = final_else {
                                    code.push_str(&format!(" else {{ {} }}", else_body));
                                }
                                return code;
                            }

                            let mut code = format!("({{ {} _if_result; ", result_type);

                            for (i, (cond, body)) in branches.iter().enumerate() {
                                if i == 0 {
                                    code.push_str(&format!("if ({}) {{ {} }}", cond, body));
                                } else {
                                    code.push_str(&format!(" else if ({}) {{ {} }}", cond, body));
                                }
                            }

                            if let Some(else_body) = final_else {
                                code.push_str(&format!(" else {{ {} }}", else_body));
                            }

                            code.push_str(" _if_result; })");
                            return code;
                        }

                        // Error handling: left else right
                        // This operator is used for error propagation: expr else fallback
                        // If expr is None/null/empty, evaluate fallback (which might be a return statement)
                        // We need to handle this differently depending on whether right is a statement or expression

                        // Infer the type from the left expression
                        let left_type = self.infer_expr_type(&binop.left);

                        // Check if the right side is a control flow statement (return, break, continue)
                        let is_right_control_flow = matches!(
                            &*binop.right,
                            parser::Expression::Return(_)
                                | parser::Expression::Break
                                | parser::Expression::Continue
                        );

                        // Check if the right side is a void expression (panic, todo, etc.)
                        let is_right_void = match &*binop.right {
                            parser::Expression::Call(call) => {
                                matches!(
                                    call.name.as_str(),
                                    "panic" | "todo" | "unreachable" | "assert"
                                )
                            }
                            _ => false,
                        };

                        // Determine how to check for "none" based on the type
                        if left_type.starts_with("Option_") {
                            // Option type: check .tag field
                            let tag_name = format!("{}_tag_none", left_type);

                            if is_right_control_flow || is_right_void {
                                // Use if-statement form for control flow or void expressions
                                let left = self.generate_expression(&binop.left, is_main);
                                let right = self.generate_expression(&binop.right, is_main);
                                return format!(
                                    "({{ {} _opt = {}; if (_opt.tag == {}) {{ {}; }} _opt.value; }})",
                                    left_type, left, tag_name, right
                                );
                            } else {
                                let left = self.generate_expression(&binop.left, is_main);

                                // Special handling for Block expressions in else branch
                                // Need to evaluate the block and return the last expression's value
                                let right = if let parser::Expression::Block(stmts) = &*binop.right
                                {
                                    self.generate_block_result(&stmts, is_main)
                                } else {
                                    self.generate_expression(&binop.right, is_main)
                                };

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
                            let left = self.generate_expression(&binop.left, is_main);

                            // Store result in a temp variable to avoid double evaluation
                            let check_expr = if left_type.starts_with("List_") {
                                "_else_tmp.len == 0".to_string()
                            } else if left_type.ends_with("*") || left_type == "char*" {
                                "_else_tmp == NULL".to_string()
                            } else if left_type == "int64_t" || left_type == "bool" {
                                "!_else_tmp".to_string()
                            } else {
                                // Unknown type - assume pointer
                                "_else_tmp == NULL".to_string()
                            };

                            if is_right_control_flow || is_right_void {
                                let right = self.generate_expression(&binop.right, is_main);
                                return format!(
                                    "({{ {} _else_tmp = {}; if ({}) {{ {}; }} _else_tmp; }})",
                                    left_type, left, check_expr, right
                                );
                            } else {
                                let right = self.generate_expression(&binop.right, is_main);
                                return format!(
                                    "({{ {} _else_tmp = {}; {} ? ({}) : _else_tmp; }})",
                                    left_type, left, check_expr, right
                                );
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
                        let left_expr = self.generate_expression(&binop.left, is_main);

                        // Special handling for comparisons with 'none'
                        // We need to infer the Option type from the other operand
                        if let parser::Expression::Ident(ident) = &*binop.right {
                            if ident.name == "none" {
                                // Infer Option type from left operand
                                let left_type = self.infer_expr_type(&binop.left);
                                if left_type.starts_with("Option_") {
                                    // Generate tag comparison instead of struct comparison
                                    let tag_name = format!("{}_tag_none", left_type);
                                    return match binop.op {
                                        parser::Operator::Eq => {
                                            format!("({}.tag == {})", left_expr, tag_name)
                                        }
                                        parser::Operator::Ne => {
                                            format!("({}.tag != {})", left_expr, tag_name)
                                        }
                                        _ => format!(
                                            "({} {} ({}){{.tag = {}}})",
                                            left_expr, op_str, left_type, tag_name
                                        ),
                                    };
                                }
                            }
                        }

                        // Special handling for list equality comparisons
                        // C doesn't support operator overloading, so we use a helper function
                        // We must check BEFORE generating the right expression so we can
                        // set the expected element type for list literal generation
                        if matches!(binop.op, parser::Operator::Eq | parser::Operator::Ne) {
                            let left_type = self.infer_expr_type(&binop.left);
                            if left_type.starts_with("List_") {
                                let elem_type = left_type
                                    .strip_prefix("List_")
                                    .unwrap_or("void")
                                    .to_string();

                                // Set expected element type before generating right expression
                                self.expected_list_elem_type = Some(elem_type.clone());
                                let right = self.generate_expression(&binop.right, is_main);
                                self.expected_list_elem_type = None;

                                self.list_eq_needed.insert(elem_type.clone());
                                let eq_call = format!(
                                    "blitz_list_eq_{}({}, {})",
                                    elem_type, left_expr, right
                                );
                                return if matches!(binop.op, parser::Operator::Ne) {
                                    format!("(!{})", eq_call)
                                } else {
                                    eq_call
                                };
                            }

                            // Special handling for string equality comparisons
                            // C doesn't support == for strings, so we use strcmp
                            if left_type == "char*" || left_type == "String" {
                                let right = self.generate_expression(&binop.right, is_main);
                                // strcmp returns 0 when strings are equal
                                return if matches!(binop.op, parser::Operator::Eq) {
                                    format!("(strcmp({}, {}) == 0)", left_expr, right)
                                } else {
                                    format!("(strcmp({}, {}) != 0)", left_expr, right)
                                };
                            }
                        }

                        let right = self.generate_expression(&binop.right, is_main);

                        // Generate with parentheses for proper precedence
                        format!("({} {} {})", left_expr, op_str, right)
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

                // Special handling for _assert_cmp_failed() - generated by assert for comparisons
                // Arguments: condition_str, left_val, left_str, right_val, right_str
                // We handle this BEFORE generic arg generation because:
                // 1. We need to set expected_list_elem_type for list literal args
                // 2. We don't want the auto-unwrap heuristic to apply
                if func_name == "_assert_cmp_failed" && call.args.len() == 5 {
                    // Generate condition_str, left_str, right_str (string args - straightforward)
                    let condition_str = self.generate_expression(&call.args[0].init, is_main);
                    let left_str = self.generate_expression(&call.args[2].init, is_main);
                    let right_str = self.generate_expression(&call.args[4].init, is_main);

                    // Infer types of left and right value expressions
                    let left_type = self.infer_expr_type(&call.args[1].init);
                    let right_type_raw = self.infer_expr_type(&call.args[3].init);

                    // For list comparison assertions, set expected_list_elem_type
                    // so the right-side list literal generates with the correct element type
                    if left_type.starts_with("List_") {
                        let elem_type = left_type.strip_prefix("List_").unwrap().to_string();
                        self.expected_list_elem_type = Some(elem_type);
                    }

                    // Generate left and right value expressions
                    let left_val = self.generate_expression(&call.args[1].init, is_main);
                    let right_val = self.generate_expression(&call.args[3].init, is_main);

                    self.expected_list_elem_type = None;

                    // Determine actual C types for display, accounting for any transformations.
                    // The C type of the generated expression may differ from the Blitz-level type
                    // (e.g., Option_T becomes T* when auto-unwrapped).
                    let left_c_type = left_type.clone();
                    let mut right_c_type = right_type_raw.clone();

                    // For list comparisons, the right side type inference may be wrong
                    // (e.g., inferring List_Assignment instead of List_TokenKind).
                    // Since both sides of a comparison must be the same type, use the
                    // left type for the right side when both are lists and types differ.
                    if left_c_type.starts_with("List_")
                        && right_c_type.starts_with("List_")
                        && left_c_type != right_c_type
                    {
                        right_c_type = left_c_type.clone();
                    }

                    // Get display expressions that convert values to char*
                    let left_display = self.get_display_expr(&left_c_type, &left_val);
                    let right_display = if right_val == "none" || right_val == "none.value" {
                        "\"none\"".to_string()
                    } else {
                        self.get_display_expr(&right_c_type, &right_val)
                    };

                    return format!(
                        "blitz_assert_cmp_failed({}, {}, {}, {}, {})",
                        condition_str, left_str, left_display, right_str, right_display
                    );
                }

                // Try to find the function signature to guide argument generation
                // Use the signature that matches the argument count for overloaded functions
                // CLONE the signature to avoid borrowing self while calling generate_expression later
                let arg_count = call.args.len();
                let signature = self
                    .function_signatures
                    .get(func_name)
                    .and_then(|sigs| {
                        // First, try to find a signature with matching arg count
                        sigs.iter()
                            .find(|(params, _)| params.len() == arg_count)
                            .or_else(|| sigs.first()) // Fallback to first if no exact match
                    })
                    .cloned();

                // Look up mutability flags for the callee's parameters
                let mut_flags: Option<Vec<bool>> = self
                    .function_mut_params
                    .get(func_name)
                    .and_then(|overloads| {
                        overloads
                            .iter()
                            .find(|(params, _)| params.len() == arg_count)
                            .or_else(|| overloads.first())
                    })
                    .map(|(_, flags)| flags.clone());

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
                                } else if !param_type.starts_with("Option") {
                                    // Param is not an Option type - check if arg IS an Option and unwrap
                                    let arg_type = self.infer_expr_type(&arg.init);
                                    if arg_type.starts_with("Option_") {
                                        if !arg_code.ends_with(".value") {
                                            arg_code = format!("{}.value", arg_code);
                                        }
                                    }
                                }

                                // Union-to-struct extraction for function call arguments:
                                // If the arg's type is a tagged union (e.g., Expression*) but the
                                // param expects a variant of that union (e.g., Ident), extract
                                // with arg->data.as_Variant. Only for simple variable references.
                                if let parser::Expression::Ident(ident) = &*arg.init {
                                    let arg_type = self.infer_expr_type(&arg.init);
                                    let arg_type_base = arg_type.trim_end_matches('*');
                                    if self.tagged_union_types.contains(arg_type_base) {
                                        // The arg is a tagged union — check if param_type is
                                        // one of its variants
                                        if let Some(unions) = self.variant_to_union.get(param_type.as_str()) {
                                            if unions.contains(arg_type_base) && param_type != arg_type_base {
                                                debug_println!(self,
                                                    "DEBUG Function arg: extracting variant {} from union {} (var={})",
                                                    param_type, arg_type_base, ident.name
                                                );
                                                arg_code = format!("{}->data.as_{}", arg_code, param_type);
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                            // No signature found - use heuristic: if arg is Option and function
                            // doesn't seem to be Option-aware, unwrap it
                            let arg_type = self.infer_expr_type(&arg.init);
                            if arg_type.starts_with("Option_") {
                                // Check if the function name suggests it expects Option
                                // (heuristic: functions starting with Option, some, none, etc.)
                                let expects_option = func_name.starts_with("Option")
                                    || func_name.contains("_Option")
                                    || func_name == "some"
                                    || func_name == "none"
                                    || func_name == "unwrap";
                                if !expects_option && !arg_code.ends_with(".value") {
                                    arg_code = format!("{}.value", arg_code);
                                }
                            }
                        }
                        // If this argument corresponds to a `mut` value-type parameter,
                        // pass it by address so the callee can modify it through a pointer
                        if let Some(ref flags) = mut_flags {
                            if i < flags.len() && flags[i] {
                                // Check that the arg's C type is a value type
                                let arg_type = self.infer_expr_type(&arg.init);
                                let c_type = self.map_type_name(&arg_type);
                                if self.is_c_value_type(&c_type) {
                                    // Need to take address of the arg.
                                    // If the arg is a member access (e.g., reg->mods), wrap with &(...)
                                    // If the arg is a dereferenced mut-pointer param (*name), just pass the raw pointer name
                                    if arg_code.starts_with("(*") && arg_code.ends_with(')') {
                                        // This is a dereferenced mut-pointer param like (*mods)
                                        // Just pass the raw pointer: mods
                                        arg_code = arg_code[2..arg_code.len()-1].to_string();
                                    } else {
                                        arg_code = format!("&({})", arg_code);
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
                        "print" if args.len() == 1 => {
                            // obj.print() - print method on various types
                            // Check if the receiver is a list type that needs special handling
                            let receiver_type = self.infer_expr_type(&call.args[0].init);
                            if receiver_type.starts_with("List_") {
                                // For List types, print a placeholder since we can't easily iterate
                                // in an expression context
                                return format!("printf(\"<{}>\\n\")", receiver_type);
                            }
                            // For other types, use the print macro
                            return format!("print({})", args[0]);
                        }
                        "precedence" if args.len() == 1 => {
                            // op.precedence() where op is TokenKind but precedence() expects Operator*
                            // Check if the receiver is TokenKind
                            let receiver_type = self.infer_expr_type(&call.args[0].init);
                            if receiver_type == "TokenKind" {
                                return format!("precedence(TokenKind_to_Operator({}))", args[0]);
                            }
                            // Otherwise, pass through normally
                            format!("precedence({})", args[0])
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

                    // Special handling for print() - handle types not covered by _Generic macro
                    if func_name == "print" && args.len() == 1 {
                        let arg_type = self.infer_expr_type(&call.args[0].init);
                        // Check if this is a type not handled by the print macro
                        if arg_type == "TokenKind" {
                            return format!("printf(\"TokenKind(%d)\\n\", (int)({}))", args[0]);
                        } else if arg_type == "Statement" || arg_type == "Statement*" {
                            return format!("printf(\"<Statement>\\n\")");
                        } else if arg_type.starts_with("List_") && arg_type != "List_Rune" {
                            return format!("printf(\"<{}>\\n\")", arg_type);
                        } else if self.tagged_union_types.contains(&arg_type)
                            || self
                                .tagged_union_types
                                .contains(&arg_type.trim_end_matches('*').to_string())
                        {
                            return format!("printf(\"<{}>\\n\")", arg_type);
                        }
                        // Otherwise, let the print macro handle it
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

                // Handle type name collisions between struct and enum (e.g., Assignment struct and
                // Assignment enum). If this is a struct constructor (has field arguments), we need
                // to use the correct C name from the registry.
                //
                // The registry maps original Blitz names to C names, handling collisions.
                // For constructors with named fields, we always want the struct version.
                if !ctor.args.is_empty() {
                    // This constructor has field arguments, so it must be a struct
                    // Look up the struct's C name from the registry using module context
                    let module = ctor
                        .r#type
                        .module
                        .as_deref()
                        .unwrap_or(&self.current_module);
                    if let Some(struct_c_name) = self
                        .type_name_registry
                        .get_struct_c_name_for_module(&ctor.r#type.name, module)
                        .or_else(|| self.type_name_registry.get_struct_c_name(&ctor.r#type.name))
                    {
                        if struct_c_name != type_name {
                            debug_println!(self, 
                                "DEBUG Constructor: resolving struct '{}' to C name '{}' (collision detected)",
                                type_name, struct_c_name
                            );
                            type_name = struct_c_name;
                        }
                    }
                }

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

                    // Look up the expected field type from struct_field_types
                    let expected_field_type = self
                        .struct_field_types
                        .get(&type_name)
                        .and_then(|fields| fields.get(field_name))
                        .cloned();

                    // Validate that the field actually exists on this struct
                    if expected_field_type.is_none() {
                        if let Some(known_fields) = self.struct_field_types.get(&type_name) {
                            let available: Vec<&String> = known_fields.keys().collect();
                            self.codegen_errors.push(format!(
                                "struct '{}' has no field '{}' (available fields: {})",
                                type_name,
                                field_name,
                                if available.is_empty() {
                                    "<none>".to_string()
                                } else {
                                    let mut sorted: Vec<&str> =
                                        available.iter().map(|s| s.as_str()).collect();
                                    sorted.sort();
                                    sorted.join(", ")
                                }
                            ));
                        }
                    }

                    // Check if this is an empty list - we need to determine the correct type
                    let mut field_value = if let parser::Expression::List(list) = &*arg.init {
                        if list.elems.is_empty() {
                            // Empty list - try to infer the correct List type from the struct field
                            // First, check if we have the expected field type from struct definition
                            let list_type = if let Some(ref eft) = expected_field_type {
                                // Expected field type is from struct_field_types, e.g. "List_Expression"
                                // Resolve inner type names using module-aware lookup
                                if eft.starts_with("List(") || eft.starts_with("List_") {
                                    self.resolve_field_type_to_c(eft, &type_name)
                                } else {
                                    "List_Int".to_string()
                                }
                            } else {
                                // Fallback to hardcoded mappings for common patterns
                                match (type_name.as_str(), field_name.as_str()) {
                                    ("Block", "statements") | (_, "statements") => {
                                        "List_Statement".to_string()
                                    }
                                    ("SwitchCase", "body") | (_, "body") => {
                                        "List_Statement".to_string()
                                    }
                                    ("Call", "args") => "List_CallArg".to_string(),
                                    ("Fn", "args") | (_, "args") => "List_Arg".to_string(),
                                    ("Struct", "fields") | (_, "fields") => {
                                        "List_Field".to_string()
                                    }
                                    ("Union", "cases") | (_, "cases") => "List_Case".to_string(),
                                    (_, "elems") => format!(
                                        "List_{}",
                                        self.resolve_c_name_for("Expression", &self.current_module)
                                    ),
                                    _ => "List_Int".to_string(), // Default fallback
                                }
                            };
                            format!("({}){{.data = NULL, .len = 0, .cap = 0}}", list_type)
                        } else {
                            self.generate_expression(&arg.init, is_main)
                        }
                    } else if let parser::Expression::Ident(ident) = &*arg.init {
                        // Special handling for 'none' - needs the field type to know which Option type
                        if ident.name == "none" {
                            if let Some(ref eft) = expected_field_type {
                                // The field type should be Option_X, generate the none constructor
                                if eft.starts_with("Option_") || eft.starts_with("Option(") {
                                    // Extract the inner type for the C Option name
                                    let option_type = if eft.starts_with("Option(") {
                                        // Parse Option(Ident) -> Option_Ident
                                        let inner = eft
                                            .strip_prefix("Option(")
                                            .and_then(|s| s.strip_suffix(")"))
                                            .unwrap_or("Unknown");
                                        format!("Option_{}", inner)
                                    } else {
                                        eft.clone()
                                    };
                                    format!("({}){{.tag = {}_tag_none}}", option_type, option_type)
                                } else {
                                    // Field isn't Option, use generic none (will likely error)
                                    "none".to_string()
                                }
                            } else {
                                // No field type info, try infer from field name conventions
                                // Common Option field patterns in Blitz compiler
                                match field_name.as_str() {
                                    "label" => {
                                        "(Option_Ident){.tag = Option_Ident_tag_none}".to_string()
                                    }
                                    "type" | "ty" => {
                                        "(Option_Type){.tag = Option_Type_tag_none}".to_string()
                                    }
                                    "else" | "r#else" => {
                                        "(Option_Block){.tag = Option_Block_tag_none}".to_string()
                                    }
                                    _ => "none".to_string(), // fallback
                                }
                            }
                        } else {
                            // Regular identifier expression handling
                            // For identifier expressions, use the expected field type as a hint
                            // This helps qualify enum variants correctly (e.g., `add` -> `BinaryOperator_add`)
                            let var_name = self.get_variable_name(&ident.name);

                            // Check if this is a known variable by looking in variable_types
                            let var_type = self.variable_types.get(&ident.name).cloned();

                            debug_println!(self, 
                                "DEBUG Constructor field: type={}, field={}, ident={}, var_type={:?}, expected={:?}",
                                type_name, field_name, ident.name, var_type, expected_field_type
                            );

                            if let Some(vt) = &var_type {
                                // It's a known variable - check if we need to convert TokenKind to operator types
                                if let Some(eft) = &expected_field_type {
                                    if vt == "TokenKind" && eft == "BinaryOperator" {
                                        format!("TokenKind_to_BinaryOperator({})", var_name)
                                    } else if vt == "TokenKind" && eft == "UnaryOperator" {
                                        format!("TokenKind_to_UnaryOperator({})", var_name)
                                    } else {
                                        // Check if the variable is typed as a union but the field
                                        // expects a bare struct that's a variant of that union.
                                        // e.g., var_type = "SwitchLabel*" but field expects "Ident"
                                        let vt_base = vt.trim_end_matches('*');
                                        if vt_base != eft.as_str()
                                            && self.tagged_union_types.contains(vt_base)
                                        {
                                            // The variable is a tagged union — check if the expected
                                            // field type is one of its variants
                                            if let Some(variants) =
                                                self.variant_to_union.get(eft.as_str())
                                            {
                                                if variants.contains(vt_base) {
                                                    // Extract the inner variant value from the union
                                                    debug_println!(self,
                                                        "DEBUG Constructor field: extracting variant {} from union {} (var={})",
                                                        eft, vt_base, var_name
                                                    );
                                                    format!("{}->data.as_{}", var_name, eft)
                                                } else {
                                                    var_name
                                                }
                                            } else {
                                                var_name
                                            }
                                        } else {
                                            var_name
                                        }
                                    }
                                } else {
                                    var_name
                                }
                            } else if var_name == ident.name {
                                // Not a known variable - try to qualify as enum variant with type hint
                                let qualified = self.qualify_identifier_with_hint(
                                    &ident.name,
                                    expected_field_type.as_deref(),
                                );
                                // If the identifier wasn't resolved as an enum variant
                                // (qualify_identifier_with_hint returned it unchanged),
                                // check if it's a label-only tagged union variant and
                                // generate the proper constructor.
                                if qualified == ident.name {
                                    // Check if this is a label-only tagged union variant
                                    // e.g., `builtin` in TypeDefKind union
                                    let mut found_tagged_union: Option<String> = None;
                                    for (union_name, labels) in &self.tagged_union_labels {
                                        if labels.contains(&ident.name) {
                                            found_tagged_union = Some(union_name.clone());
                                            break;
                                        }
                                    }
                                    if let Some(union_name) = found_tagged_union {
                                        // Generate tagged union constructor for label-only variant
                                        // e.g., builtin -> memcpy(malloc(sizeof(TypeDefKind)), &((TypeDefKind){.tag = TypeDefKind_tag_builtin}), sizeof(TypeDefKind))
                                        format!(
                                            "memcpy(malloc(sizeof({union})), &(({union}){{.tag = {union}_tag_{label}}}), sizeof({union}))",
                                            union = union_name,
                                            label = ident.name
                                        )
                                    } else {
                                        let is_constructor_type =
                                            self.seen_types.contains(&ident.name)
                                                || self.tagged_union_types.contains(&ident.name);
                                        if !is_constructor_type {
                                            self.codegen_errors.push(format!(
                                                "undefined variable '{}' used in constructor field '{}' of struct '{}'",
                                                ident.name, field_name, type_name
                                            ));
                                        }
                                        qualified
                                    }
                                } else {
                                    qualified
                                }
                            } else {
                                // Mapped variable name
                                var_name
                            }
                        }
                    } else {
                        // Set expected_expr_type from the field's declared type so that
                        // nested constructors know whether to produce a bare struct or
                        // wrap in a tagged union.
                        let old_expected = self.expected_expr_type.take();
                        if let Some(ref eft) = expected_field_type {
                            self.expected_expr_type = Some(eft.clone());
                        }
                        let result = self.generate_expression(&arg.init, is_main);
                        self.expected_expr_type = old_expected;
                        result
                    };

                    // Auto-unwrap Option types for struct fields that expect pointers
                    // If the field value has Option type but the field likely expects a pointer,
                    // append .value to unwrap
                    // BUT: If the target field ALSO expects an Option, don't unwrap!
                    let field_type = self.infer_expr_type(&arg.init);
                    if field_type.starts_with("Option_") && !field_value.ends_with(".value") {
                        // Check if the expected field type is also an Option - if so, don't unwrap
                        let expected_is_option = expected_field_type
                            .as_ref()
                            .map(|t| t.starts_with("Option"))
                            .unwrap_or(false);

                        if !expected_is_option {
                            // Check if the inner type is a struct (would be a pointer in C)
                            let inner_type =
                                field_type.strip_prefix("Option_").unwrap_or(&field_type);
                            let inner_is_struct = self.seen_types.contains(inner_type)
                                && !self.enum_types.contains(inner_type);
                            let inner_is_tagged_union =
                                self.tagged_union_types.contains(inner_type);
                            let inner_is_list = inner_type.starts_with("List_");

                            // Unwrap Option types in constructor fields when:
                            // - Inner type is a struct (pointer in C)
                            // - Inner type is a tagged union
                            // - Inner type is a List type
                            if inner_is_struct || inner_is_tagged_union || inner_is_list {
                                // Generate a safe unwrap that panics if none
                                // Pattern: ({ OptionType _tmp = val; if (_tmp.tag == ..._tag_none) panic("Unwrap failed"); _tmp.value; })
                                field_value = format!(
                                    "({{ {} _unwrap_tmp = {}; if (_unwrap_tmp.tag == {}_tag_none) panic(\"Implicit unwrap failed on '{}' for field '{}'\"); _unwrap_tmp.value; }})",
                                    field_type, field_value, field_type, field_value, field_name
                                );
                            }
                        }
                    }

                    // Auto-wrap values INTO Option types when expected field is Option but value isn't
                    // e.g., field expects Option_Ident but we have Ident*
                    if let Some(ref eft) = expected_field_type {
                        if eft.starts_with("Option_") || eft.starts_with("Option(") {
                            // Check if the value is NOT already an Option type
                            if !field_type.starts_with("Option_") {
                                let option_type = if eft.starts_with("Option(") {
                                    let inner = eft
                                        .strip_prefix("Option(")
                                        .and_then(|s| s.strip_suffix(")"))
                                        .unwrap_or("Unknown");
                                    format!("Option_{}", inner)
                                } else {
                                    eft.clone()
                                };
                                // Wrap in Option with some tag
                                field_value = format!(
                                    "({}){{.tag = {}_tag_some, .value = {}}}",
                                    option_type, option_type, field_value
                                );
                            }
                        }
                    }

                    field_inits.push(format!(".{} = {}", field_name, field_value));
                }

                // Check if this is a variant of a tagged union (like Lit_Bool -> Expression)
                // BUT: if the context directly expects the bare struct type (not a union),
                // we should NOT wrap it in the parent union.
                //
                // Contexts that tell us to skip wrapping:
                // 1. expected_expr_type is set and matches our type (from outer constructor field)
                // 2. Return type is Option(X) where X matches our type
                // 3. Return type directly matches our type (bare struct return)
                // 4. No context at all (let binding without type annotation) AND the type
                //    is used as a struct field type somewhere — prefer bare struct
                let should_skip_union_wrapping = if let Some(ref expected) = self.expected_expr_type
                {
                    // The outer constructor's field expects a specific type.
                    // If that type matches our constructor type, produce a bare struct.
                    // e.g., TypeDef.name is "Ident" and we're constructing Ident(...)
                    if expected == &type_name {
                        debug_println!(self,
                            "DEBUG Constructor: skipping union wrap because expected_expr_type '{}' matches type_name={}",
                            expected, type_name
                        );
                        true
                    } else {
                        false
                    }
                } else if let Some(ref return_type) = self.current_return_type {
                    // If return type is Option(X), check if X matches our type_name
                    if return_type.name == "Option" && !return_type.params.is_empty() {
                        let inner_type = &return_type.params[0];
                        let inner_monomorphized = self.type_name_for_instance(inner_type);
                        if inner_monomorphized == type_name
                            || inner_monomorphized.as_str() == type_name.as_str()
                        {
                            debug_println!(self, 
                                "DEBUG Constructor: skipping union wrap because return type Option({}) matches type_name={}",
                                inner_monomorphized, type_name
                            );
                            true
                        } else {
                            false
                        }
                    } else if return_type.name == type_name {
                        // Return type directly matches (e.g., fn lower() -> TypeDef returns TypeDef)
                        debug_println!(self,
                            "DEBUG Constructor: skipping union wrap because return type '{}' matches type_name={}",
                            return_type.name, type_name
                        );
                        true
                    } else {
                        // Return type is a different type — check if it's a union that contains our type
                        // If so, we DO need wrapping (should_skip = false)
                        // If not, it's unrelated context, no reason to wrap
                        false
                    }
                } else {
                    false
                };

                // Note: a variant can belong to multiple unions, we pick the first one
                // Also check the _1 suffixed version for types with collision (e.g., Assignment struct
                // might be registered as Assignment_1 in the union due to collision with Assignment enum)
                let variant_lookup_name = if self.variant_to_union.contains_key(&type_name) {
                    type_name.clone()
                } else {
                    let suffixed = format!("{}_1", type_name);
                    if self.variant_to_union.contains_key(&suffixed) {
                        suffixed
                    } else {
                        type_name.clone() // Keep original, will fail the lookup below
                    }
                };

                if !should_skip_union_wrapping {
                    if let Some(union_set) = self.variant_to_union.get(&variant_lookup_name) {
                        let union_name = union_set.iter().next().cloned().unwrap();
                        // This is a tagged union variant - wrap in the union struct
                        debug_println!(self, 
                            "DEBUG Constructor: type_name={} (lookup={}) is variant of union {}, generating union wrapper",
                            type_name, variant_lookup_name, union_name
                        );

                        // Determine if this variant's field in the union is a pointer type
                        // Non-parameterized types (like Index, Mut) that are registered structs become pointers
                        // Parameterized types (like Lit_Bool) are stored as values
                        let variant_is_pointer = self.seen_types.contains(&type_name)
                            && !self.enum_types.contains(&type_name);

                        let variant_literal =
                            format!("({}){{{}}}", type_name, field_inits.join(", "));

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

                        // Use variant_lookup_name for tags/fields since that's how it's registered in the union
                        let union_literal = format!(
                            "(({}){{.tag = {}_tag_{}, .data.as_{} = {}}})",
                            union_name,
                            union_name,
                            variant_lookup_name,
                            variant_lookup_name,
                            variant_assignment
                        );

                        return format!(
                            "memcpy(malloc(sizeof({})), &{}, sizeof({}))",
                            union_name, union_literal, union_name
                        );
                    }
                }

                // Check if we need to return a pointer to this struct
                // This happens when:
                // 1. The struct type is known and not an enum, OR
                // 2. The return type is Option(X) and we're returning X - Option.value is always a pointer
                let needs_pointer = (self.seen_types.contains(&type_name)
                    && !self.enum_types.contains(&type_name))
                    || should_skip_union_wrapping; // When returning into Option.value, we need a pointer

                debug_println!(self, 
                    "DEBUG Constructor: type_name={}, seen_types.contains={}, enum_types.contains={}, needs_pointer={}, should_skip_union_wrapping={}",
                    type_name,
                    self.seen_types.contains(&type_name),
                    self.enum_types.contains(&type_name),
                    needs_pointer,
                    should_skip_union_wrapping
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

                // Special case: parser method calls without parentheses
                // In Blitz, `parser.mut.parse_def` or `parser.mut.parse_ident` are method calls, not field accesses
                // Parser doesn't have these as fields, so they must be calls
                if member_name.starts_with("parse_") {
                    let parent_type = self.infer_expr_type(&member.parent);
                    let base_type = parent_type.trim_end_matches('*');
                    if base_type == "Parser" {
                        return format!("{}({})", member_name, parent_code);
                    }
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

                    if base_type == "Expression"
                        || base_type == "ast_Expression"
                        || base_type == "Statement"
                        || base_type == "ast_Statement"
                    {
                        // Use helper function: Expression_span(expr)
                        // If parent_type is Option, we need to unwrap/access value first with a safety check
                        if parent_type.starts_with("Option_") {
                            // Generate a safe unwrap that panics if none
                            return format!(
                                "{}_span(({{ {} _unwrap_tmp = {}; if (_unwrap_tmp.tag == {}_tag_none) panic(\"Implicit unwrap failed accessing .span\"); _unwrap_tmp.value; }}))",
                                base_type, parent_type, parent_code, parent_type
                            );
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

                    // Generate a safe unwrap that panics if none
                    let safe_unwrap = format!(
                        "({{ {} _unwrap_tmp = {}; if (_unwrap_tmp.tag == {}_tag_none) panic(\"Implicit unwrap failed accessing .{}\"); _unwrap_tmp.value; }})",
                        parent_type, parent_code, parent_type, member_name
                    );

                    if parent_type.contains("List_") || parent_type.contains("Range") {
                        format!("{}.{}", safe_unwrap, member_name)
                    } else {
                        // Assume pointer for other user types
                        format!("{}->{}", safe_unwrap, member_name)
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
                // For plain arrays/strings, we use direct indexing

                let target_expr = self.generate_expression(&idx.target, is_main);
                let index_expr = self.generate_expression(&idx.index, is_main);

                // Use type inference to determine if the target is a List type
                let target_type = self.infer_expr_type(&idx.target);

                // List types need .data accessor, strings/char* use direct indexing
                if target_type.starts_with("List_") {
                    format!("{}.data[{}]", target_expr, index_expr)
                } else {
                    // String, char*, arrays use direct indexing
                    format!("{}[{}]", target_expr, index_expr)
                }
            }
            parser::Expression::Assignment(assign) => {
                // Generate assignment: lval = rhs
                let lval_code = match &assign.left {
                    parser::Lval::Ident(ident) => {
                        let var_name = self.get_variable_name(&ident.name);
                        if self.mut_pointer_params.contains_key(&var_name) {
                            // Mut value-type param: assignment goes through pointer
                            format!("*{}", var_name)
                        } else {
                            var_name
                        }
                    }
                    parser::Lval::Member(member) => {
                        // Use the same logic as Expression::Member for consistency
                        let parent_code = self.generate_expression(&member.parent, is_main);
                        let member_name = &member.member;

                        // Infer the parent type to handle Option types
                        let parent_type = self.infer_expr_type(&member.parent);

                        // Special case: .mut is a Blitz mutability marker
                        if member_name == "mut" {
                            parent_code
                        } else if parent_type.starts_with("Option_") {
                            // Option types need .value to access inner type's members
                            // Since we're assigning, the inner type must be a pointer
                            format!("{}.value->{}", parent_code, member_name)
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
                        // For List types, use .data accessor (same as Expression::Index)
                        let target_type = self.infer_expr_type(&index.target);
                        if target_type.starts_with("List_") {
                            format!("{}.data[{}]", target, idx)
                        } else {
                            format!("{}[{}]", target, idx)
                        }
                    }
                };

                // Get the lval type from variable_types
                let lval_var_name = match &assign.left {
                    parser::Lval::Ident(ident) => Some(self.get_variable_name(&ident.name)),
                    _ => None,
                };
                let mut lval_type = lval_var_name
                    .as_ref()
                    .and_then(|name| self.variable_types.get(name))
                    .cloned();

                // For member assignments (e.g., ty.module = name), infer the field type
                // from the parent struct's field type information
                if lval_type.is_none() {
                    if let parser::Lval::Member(member) = &assign.left {
                        let parent_type = self.infer_expr_type(&member.parent);
                        let parent_base = parent_type.trim_end_matches('*');
                        if let Some(fields) = self.struct_field_types.get(parent_base) {
                            if let Some(field_type) = fields.get(&member.member) {
                                // Resolve the field type to a proper C type
                                let resolved =
                                    self.resolve_field_type_to_c(field_type, parent_base);
                                lval_type = Some(resolved);
                            }
                        }
                    }
                }

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

                    // Check if lval is Option_X and rhs is X* (or X) — wrap rhs in Option
                    if lval_t.starts_with("Option_") && !rhs_type.starts_with("Option_") {
                        let option_inner = &lval_t[7..]; // e.g., "Ident"
                        let rhs_base = rhs_type.trim_end_matches('*');
                        if rhs_base == option_inner {
                            return format!(
                                "{} = ({}){{.tag = {}_tag_some, .value = {}}}",
                                lval_code, lval_t, lval_t, rhs
                            );
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
                            // Create a unique loop label for break statements to target
                            let loop_label = format!("_loop_exit_{}", self.loop_label_counter);
                            self.loop_label_counter += 1;
                            self.loop_label_stack.push(loop_label.clone());

                            // Generate body statements
                            let mut body_code = String::new();
                            for stmt in &for_loop.body {
                                let stmt_code = self.generate_statement(stmt, is_main);
                                body_code.push_str("        ");
                                body_code.push_str(&stmt_code);
                                body_code.push_str("\n");
                            }

                            // Pop the loop label
                            self.loop_label_stack.pop();

                            // Generate C for-loop with proper iterator variable declaration
                            return format!(
                                "for (int64_t {} = {}; {} < {}; {}++) {{\n{}    }}\n    {}:;",
                                elem_name,
                                begin,
                                elem_name,
                                until,
                                elem_name,
                                body_code,
                                loop_label
                            );
                        }
                    }
                }

                // Check if the iterator is a List type - generate index-based for loop
                let iter_type = self.infer_expr_type(&for_loop.iter);
                if iter_type.starts_with("List_") {
                    let iter_code = self.generate_expression(&for_loop.iter, is_main);

                    // Extract the element type from List_ElementType
                    let elem_type = &iter_type[5..]; // Remove "List_" prefix

                    // Determine if the element type is a struct/union (needs pointer) or value type (enum/primitive)
                    // Check if the element type is in our seen_types (structs) or tagged_union_types
                    let is_struct_type = self.seen_types.contains(elem_type)
                        || self.tagged_union_types.contains(elem_type);
                    let is_enum_type = self.enum_types.contains(elem_type);

                    // For structs and tagged unions, elements are pointers (List has Type**)
                    // For enums and primitives, elements are values (List has Type*)
                    let c_elem_type = if is_struct_type && !is_enum_type {
                        format!("{}*", elem_type)
                    } else {
                        self.normalize_c_type(elem_type)
                    };

                    // Create a unique loop label for break statements to target
                    let loop_label = format!("_loop_exit_{}", self.loop_label_counter);
                    self.loop_label_counter += 1;
                    self.loop_label_stack.push(loop_label.clone());

                    // Register the element variable type for use in the body
                    self.variable_types
                        .insert(elem_name.clone(), c_elem_type.clone());

                    // Check if this is a for-expression (returns a list of values)
                    // A for-expression has a single expression statement as its body
                    let is_for_expression = for_loop.body.len() == 1
                        && matches!(&for_loop.body[0], parser::Statement::Expression(_));

                    if is_for_expression {
                        // This is a for-expression that collects values into a list
                        if let parser::Statement::Expression(body_expr) = &for_loop.body[0] {
                            // Infer the result element type from the body expression
                            let result_elem_type = self.infer_expr_type(body_expr);

                            // Skip void expressions (side-effect-only for loops)
                            if result_elem_type != "void" {
                                // Strip pointer suffix for list type naming.
                                // infer_expr_type returns e.g. "TypeRef*" for struct types,
                                // but the list type name should be "List_TypeRef" (without '*').
                                let base_elem_type =
                                    result_elem_type.trim_end_matches('*').to_string();
                                let result_list_type = format!("List_{}", base_elem_type);

                                // Generate the body expression
                                let body_code = self.generate_expression(body_expr, is_main);

                                // Register that we need the list_append function for the base element type
                                self.register_list_append_needed(base_elem_type.clone());

                                // Pop the loop label
                                self.loop_label_stack.pop();

                                // Generate a for-expression that collects values:
                                // ({ List_T _result = {0}; for (...) { _result = blitz_list_append_T(_result, expr); } _result; })
                                return format!(
                                    "({{ {} _result = {{0}}; for (int64_t _idx_{} = 0; _idx_{} < ({}).len; _idx_{}++) {{ {} {} = ({}).data[_idx_{}]; _result = blitz_list_append_{}(_result, {}); }} _result; }})",
                                    result_list_type,
                                    elem_name, elem_name, iter_code, elem_name,
                                    c_elem_type, elem_name, iter_code, elem_name,
                                    base_elem_type, body_code
                                );
                            }
                        }
                    }

                    // Generate body statements (for statement-style for loops)
                    let mut body_code = String::new();
                    for stmt in &for_loop.body {
                        let stmt_code = self.generate_statement(stmt, is_main);
                        body_code.push_str("            ");
                        body_code.push_str(&stmt_code);
                        body_code.push_str("\n");
                    }

                    // Pop the loop label
                    self.loop_label_stack.pop();

                    // Generate C for-loop over list elements
                    // Pattern: for (int64_t _i = 0; _i < list.len; _i++) { ElemType elem = list.data[_i]; ... }
                    return format!(
                        "for (int64_t _idx_{} = 0; _idx_{} < ({}).len; _idx_{}++) {{\n        {} {} = ({}).data[_idx_{}];\n{}}}\n    {}:;",
                        elem_name, elem_name, iter_code, elem_name,
                        c_elem_type, elem_name, iter_code, elem_name,
                        body_code, loop_label
                    );
                }

                // Check if the iterator is a struct with a list field (structural iteration)
                // e.g., `for ast |item|` where Ast has `items: List(Definition)`
                let iter_type_base = iter_type.trim_end_matches('*');
                let struct_list_field: Option<(String, String)> =
                    if let Some(fields) = self.struct_field_types.get(iter_type_base) {
                        fields
                            .iter()
                            .find(|(_, ftype)| ftype.starts_with("List_"))
                            .map(|(k, v)| (k.clone(), v.clone()))
                    } else {
                        None
                    };
                if let Some((field_name, field_type)) = struct_list_field {
                    let elem_type_name = field_type.strip_prefix("List_").unwrap();

                    // Determine if the element type is a struct/union (needs pointer) or value type
                    let is_struct_type = self.seen_types.contains(elem_type_name)
                        || self.tagged_union_types.contains(elem_type_name);
                    let is_enum_type = self.enum_types.contains(elem_type_name);

                    let c_elem_type = if is_struct_type && !is_enum_type {
                        format!("{}*", elem_type_name)
                    } else {
                        self.normalize_c_type(elem_type_name)
                    };

                    let iter_code = self.generate_expression(&for_loop.iter, is_main);

                    // Create a unique loop label for break statements to target
                    let loop_label = format!("_loop_exit_{}", self.loop_label_counter);
                    self.loop_label_counter += 1;
                    self.loop_label_stack.push(loop_label.clone());

                    // Register the element variable type for use in the body
                    self.variable_types
                        .insert(elem_name.clone(), c_elem_type.clone());

                    // Generate body statements
                    let mut body_code = String::new();
                    for stmt in &for_loop.body {
                        let stmt_code = self.generate_statement(stmt, is_main);
                        body_code.push_str("            ");
                        body_code.push_str(&stmt_code);
                        body_code.push_str("\n");
                    }

                    // Pop the loop label
                    self.loop_label_stack.pop();

                    // Access the list field through the struct pointer: iter->field.data[i]
                    let list_access = if iter_type.ends_with('*') {
                        format!("({}->{})", iter_code, field_name)
                    } else {
                        format!("({}.{})", iter_code, field_name)
                    };

                    return format!(
                        "for (int64_t _idx_{} = 0; _idx_{} < {}.len; _idx_{}++) {{\n        {} {} = {}.data[_idx_{}];\n{}}}\n    {}:;",
                        elem_name, elem_name, list_access, elem_name,
                        c_elem_type, elem_name, list_access, elem_name,
                        body_code, loop_label
                    );
                }

                // Fallback: generate a TODO comment for unknown iteration types
                // DO NOT emit the loop body since the iterator variable is not declared
                let iter_code = self.generate_expression(&for_loop.iter, is_main);

                format!(
                    "// TODO: For loop over unknown type '{}': for {} in {} {{ ... }}",
                    iter_type, elem_name, iter_code
                )
            }
            parser::Expression::While(while_loop) => {
                // Generate while loop: while (condition) { body }
                let cond = self.generate_expression(&while_loop.cond, is_main);

                // Create a unique loop label for break statements to target
                let loop_label = format!("_loop_exit_{}", self.loop_label_counter);
                self.loop_label_counter += 1;
                self.loop_label_stack.push(loop_label.clone());

                // Generate body statements
                let mut body_code = String::new();
                for stmt in &while_loop.body {
                    let stmt_code = self.generate_statement(stmt, is_main);
                    body_code.push_str("        ");
                    body_code.push_str(&stmt_code);
                    body_code.push_str("\n");
                }

                // Pop the loop label
                self.loop_label_stack.pop();

                // If there's a break that needs to escape a switch, add the label after the loop
                format!(
                    "while ({}) {{\n{}    }}\n    {}:;",
                    cond, body_code, loop_label
                )
            }
            parser::Expression::If(if_expr) => {
                // Standalone if expression (no else)
                // Check if this is used as a statement (body ends with control flow) or as an expression
                let cond = self.generate_expression(&if_expr.cond, is_main);

                // Check if body ends with control flow (return, break, continue)
                let ends_with_control_flow = if let Some(last_stmt) = if_expr.body.last() {
                    match last_stmt {
                        parser::Statement::Expression(expr) => {
                            let is_noreturn = matches!(expr, parser::Expression::Call(call) 
                                if matches!(call.name.as_str(), "panic" | "todo" | "unreachable"));
                            matches!(
                                expr,
                                parser::Expression::Return(_)
                                    | parser::Expression::Continue
                                    | parser::Expression::Break
                            ) || is_noreturn
                        }
                        _ => false,
                    }
                } else {
                    false
                };

                if ends_with_control_flow {
                    // This is a statement-style if (control flow in body)
                    // Generate simple if statement
                    let mut body_code = String::new();
                    for stmt in &if_expr.body {
                        let stmt_code = self.generate_statement(stmt, is_main);
                        body_code.push_str("        ");
                        body_code.push_str(&stmt_code);
                        body_code.push_str("\n");
                    }
                    format!("if ({}) {{\n{}    }}", cond, body_code)
                } else {
                    // This is an expression-style if - returns Option<T>
                    // Pre-register variable types from declarations in the if body
                    // so that infer_expr_type can resolve them when inferring the
                    // type of the last expression (e.g., `let ident = ...; ident`).
                    for stmt in &if_expr.body {
                        if let parser::Statement::Declaration(decl) = stmt {
                            if let Some(init_expr) = &decl.init {
                                let inferred = self.infer_expr_type(init_expr);
                                let var_name = self.compute_mangled_name(&decl.name);
                                // Insert under both original and mangled names so
                                // infer_expr_type can find the type regardless of
                                // whether it looks up the original or mangled name.
                                self.variable_types
                                    .insert(decl.name.clone(), inferred.clone());
                                if var_name != decl.name {
                                    self.variable_types.insert(var_name, inferred);
                                }
                            }
                        }
                    }

                    // Infer the result type from the body
                    let body_type = if let Some(parser::Statement::Expression(last_expr)) =
                        if_expr.body.last()
                    {
                        self.infer_expr_type(last_expr)
                    } else {
                        "int64_t".to_string()
                    };

                    // Map to proper C type name
                    let c_type = self.normalize_c_type(&body_type);

                    // Wrap in Option type for the result
                    let option_type = if c_type.starts_with("Option_") {
                        c_type.clone()
                    } else {
                        format!("Option_{}", c_type.trim_end_matches('*'))
                    };

                    // Generate body with wrapping in Option (for standalone if)
                    let body_code = self.generate_if_branch_body_with_option_wrap(
                        &if_expr.body,
                        is_main,
                        &option_type,
                    );

                    // Generate statement expression:
                    // ({ Option_T _if_result; if (cond) { ... _if_result = some(val); } else { _if_result = none; } _if_result; })
                    format!(
                        "({{ {} _if_result; if ({}) {{ {} }} else {{ _if_result = ({}){{.tag = {}_tag_none}}; }} _if_result; }})",
                        option_type, cond, body_code, option_type, option_type
                    )
                }
            }
            parser::Expression::Switch(switch_expr) => self.generate_switch(switch_expr, is_main),
            parser::Expression::List(list) => {
                // Generate list literal: List(T) with malloc'd data array
                // For empty lists: []
                // For initialized lists: [a, b, c]

                if list.elems.is_empty() {
                    // Empty list: Try to infer element type from context
                    // Check function return type first
                    let list_type = if let Some(ref ret_type) = self.current_return_type {
                        let mapped = self.map_type(ret_type);
                        if mapped.starts_with("List_") {
                            mapped
                        } else {
                            "List_Int".to_string()
                        }
                    } else {
                        "List_Int".to_string()
                    };
                    format!("({}){{.data = NULL, .len = 0, .cap = 0}}", list_type)
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
                        // Check if all elements are string literals
                        let all_strings = list
                            .elems
                            .iter()
                            .all(|e| matches!(e, parser::Expression::String(_)));

                        if all_strings {
                            // All strings - use char**
                            code.push_str(&format!(
                                "        char** _tmp = malloc(sizeof(char*) * {});\n",
                                len
                            ));
                            for (i, elem) in list.elems.iter().enumerate() {
                                let elem_code = self.generate_expression(elem, is_main);
                                code.push_str(&format!("        _tmp[{}] = {};\n", i, elem_code));
                            }
                            code.push_str(&format!(
                                "        (List_String){{.data = _tmp, .len = {}, .cap = {}}};\n",
                                len, len
                            ));
                        } else {
                            // Check if all elements are identifiers (possibly enum variants)
                            let all_idents = list
                                .elems
                                .iter()
                                .all(|e| matches!(e, parser::Expression::Ident(_)));

                            if all_idents {
                                // Try to use expected element type from context first (e.g., list comparison)
                                // This ensures we use the correct enum type when multiple enums share variant names
                                let enum_type: Option<String> =
                                    if let Some(ref expected) = self.expected_list_elem_type {
                                        Some(expected.clone())
                                    } else if let parser::Expression::Ident(first_ident) =
                                        &list.elems[0]
                                    {
                                        // Fall back to inferring from first element
                                        let mut found_type: Option<String> = None;
                                        for (enum_name, variants) in &self.enum_variants {
                                            if variants.contains(&first_ident.name) {
                                                found_type = Some(enum_name.clone());
                                                break;
                                            }
                                        }
                                        found_type
                                    } else {
                                        None
                                    };

                                if let Some(enum_name) = enum_type {
                                    // Generate array of enum values
                                    code.push_str(&format!(
                                        "            {}* _tmp = malloc(sizeof({}) * {});\n",
                                        enum_name, enum_name, len
                                    ));
                                    for (i, elem) in list.elems.iter().enumerate() {
                                        let elem_code = self.generate_expression(elem, is_main);
                                        code.push_str(&format!(
                                            "            _tmp[{}] = {};\n",
                                            i, elem_code
                                        ));
                                    }
                                    code.push_str(&format!(
                                    "            (List_{}){{.data = _tmp, .len = {}, .cap = {}}};\n",
                                    enum_name, len, len
                                ));
                                } else {
                                    // Not an enum variant - check if it's a local variable
                                    // and infer the list element type from variable_types
                                    let var_type = if let parser::Expression::Ident(first_ident) =
                                        &list.elems[0]
                                    {
                                        self.variable_types.get(&first_ident.name).cloned().or_else(
                                            || {
                                                let mangled =
                                                    self.get_variable_name(&first_ident.name);
                                                self.variable_types.get(&mangled).cloned()
                                            },
                                        )
                                    } else {
                                        None
                                    };

                                    if let Some(ref vt) = var_type {
                                        // We know the element type from variable_types
                                        // Determine the base type name for the list (strip pointer suffix)
                                        let base_type = vt.trim_end_matches('*');
                                        let _is_pointer = vt.ends_with('*');
                                        let elem_c_type = vt.as_str();
                                        let list_type = format!("List_{}", base_type);

                                        code.push_str(&format!(
                                            "        {}* _tmp = malloc(sizeof({}) * {});\n",
                                            elem_c_type, elem_c_type, len
                                        ));
                                        for (i, elem) in list.elems.iter().enumerate() {
                                            let elem_code = self.generate_expression(elem, is_main);
                                            code.push_str(&format!(
                                                "        _tmp[{}] = {};\n",
                                                i, elem_code
                                            ));
                                        }
                                        code.push_str(&format!(
                                            "        ({}){{.data = _tmp, .len = {}, .cap = {}}};\n",
                                            list_type, len, len
                                        ));

                                        // Ensure we have a list append function for this element type
                                        self.list_append_needed.insert(base_type.to_string());
                                    } else {
                                        // Fall back to stub
                                        code.push_str("            /* TODO: non-integer list literal requires type inference */\n");
                                        code.push_str(&format!("            void* _tmp = NULL;\n"));
                                        code.push_str("            (void*){{}};\n");
                                    }
                                }
                            } else {
                                // Check if all elements are constructors
                                let all_constructors = list
                                    .elems
                                    .iter()
                                    .all(|e| matches!(e, parser::Expression::Constructor(_)));

                                if all_constructors && !list.elems.is_empty() {
                                    // Infer element type from the first constructor
                                    if let parser::Expression::Constructor(first_ctor) =
                                        &list.elems[0]
                                    {
                                        let elem_type =
                                            self.type_name_for_instance(&first_ctor.r#type);
                                        let list_type = format!("List_{}", elem_type);

                                        // Generate array of constructor results
                                        code.push_str(&format!(
                                            "        {}** _tmp = malloc(sizeof({}*) * {});\n",
                                            elem_type, elem_type, len
                                        ));
                                        for (i, elem) in list.elems.iter().enumerate() {
                                            let elem_code = self.generate_expression(elem, is_main);
                                            code.push_str(&format!(
                                                "        _tmp[{}] = {};\n",
                                                i, elem_code
                                            ));
                                        }
                                        code.push_str(&format!(
                                            "        ({}){{.data = _tmp, .len = {}, .cap = {}}};\n",
                                            list_type, len, len
                                        ));
                                    } else {
                                        // Fallback - shouldn't happen
                                        code.push_str("        /* TODO: constructor list type inference failed */\n");
                                        code.push_str(&format!("        void* _tmp = NULL;\n"));
                                        code.push_str("        (void*){{}};\n");
                                    }
                                } else {
                                    // Mixed/complex types - use infer_expr_type on the first element
                                    // to determine the list element type
                                    if !list.elems.is_empty() {
                                        let elem_type = self.infer_expr_type(&list.elems[0]);
                                        let is_pointer = elem_type.ends_with('*');
                                        let base_type = elem_type.trim_end_matches('*');
                                        let list_type = format!("List_{}", base_type);
                                        let c_elem_type = &elem_type;

                                        code.push_str(&format!(
                                            "        {}* _tmp = malloc(sizeof({}) * {});\n",
                                            c_elem_type, c_elem_type, len
                                        ));
                                        for (i, elem) in list.elems.iter().enumerate() {
                                            let elem_code = self.generate_expression(elem, is_main);
                                            code.push_str(&format!(
                                                "        _tmp[{}] = {};\n",
                                                i, elem_code
                                            ));
                                        }
                                        code.push_str(&format!(
                                            "        ({}){{.data = _tmp, .len = {}, .cap = {}}};\n",
                                            list_type, len, len
                                        ));

                                        // Ensure we have a list append function for this element type
                                        self.list_append_needed.insert(base_type.to_string());
                                    } else {
                                        code.push_str("        /* TODO: non-integer list literal requires type inference */\n");
                                        code.push_str(&format!("        void* _tmp = NULL;\n"));
                                        code.push_str("        (void*){{}};\n");
                                    }
                                }
                            }
                        } // Close the `if all_strings { ... } else { ... }` block
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
            parser::Expression::Break => {
                // If we're inside a switch (in_switch_depth > 0) that's inside a loop,
                // we need to use goto to break out of the loop, not just the switch
                if self.in_switch_depth > 0 && !self.loop_label_stack.is_empty() {
                    let loop_label = self.loop_label_stack.last().unwrap();
                    format!("goto {}", loop_label)
                } else {
                    "break".to_string()
                }
            }
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
            debug_println!(
                self,
                "DEBUG infer_switch_result_type: all Option types: {:?}",
                option_types
            );
            // Find the intersection of all parent unions
            let mut common_unions: Option<HashSet<String>> = None;
            for inner in &option_types {
                debug_println!(
                    self,
                    "DEBUG: Looking up variant '{}' in variant_to_union",
                    inner
                );
                if let Some(parents) = self.variant_to_union.get(*inner) {
                    debug_println!(self, "DEBUG: Found parents '{:?}'", parents);
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
                    debug_println!(self, "DEBUG: No parent found for '{}'", inner);
                    common_unions = Some(HashSet::new()); // No common parent
                }
            }
            if let Some(common) = common_unions {
                if common.len() == 1 {
                    let parent = common.iter().next().unwrap();
                    debug_println!(self, "DEBUG: Unified to Option_{}", parent);
                    return format!("Option_{}", parent);
                } else if common.len() > 1 {
                    // Multiple common parents - prefer the ast Expression union if it's there
                    let ast_expr_c_name = self.resolve_c_name_for("Expression", "ast");
                    if common.contains(&ast_expr_c_name) {
                        debug_println!(
                            self,
                            "DEBUG: Unified to Option_{} (preferred)",
                            ast_expr_c_name
                        );
                        return format!("Option_{}", ast_expr_c_name);
                    }
                    // Otherwise, just pick one
                    let parent = common.iter().next().unwrap();
                    debug_println!(self, "DEBUG: Unified to Option_{} (arbitrary)", parent);
                    return format!("Option_{}", parent);
                }
            }
        }

        // Check for MIXED Option and non-Option types that are all variants of the same union
        // e.g., Option_For + Return* should unify to Option_Expression
        if !option_types.is_empty() && option_types.len() < collected_types.len() {
            debug_println!(
                self,
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
                    // Prefer the ast Expression union if available
                    let ast_expr_c_name = self.resolve_c_name_for("Expression", "ast");
                    let parent = if common.contains(&ast_expr_c_name) {
                        ast_expr_c_name
                    } else {
                        common.iter().next().unwrap().clone()
                    };
                    debug_println!(self, "DEBUG: Mixed types unified to Option_{}", parent);
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
            } else if common.len() > 1 {
                let ast_expr_c_name = self.resolve_c_name_for("Expression", "ast");
                if common.contains(&ast_expr_c_name) {
                    return format!("{}*", ast_expr_c_name);
                }
            }
        }

        // Check if any collected type is a pointer to a variant of a tagged union
        for t in &collected_types {
            if let Some(base) = t.strip_suffix('*') {
                if let Some(union_set) = self.variant_to_union.get(base) {
                    let union_name = union_set.iter().next().cloned().unwrap();
                    return format!("{}*", union_name);
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

        // Handle noreturn/void functions (panic, todo, err) being used in expression context
        // These functions never return, so we wrap them in a statement expression with a dummy value
        if from_type == "void" && to_type != "void" {
            // Check if expr_code is a call to a noreturn function
            if expr_code.starts_with("panic(")
                || expr_code.starts_with("todo(")
                || expr_code.starts_with("err(")
            {
                // Wrap in statement expression: ({ panic("msg"); (T){0}; })
                // Since panic/todo/err are noreturn or void, the dummy value is never reached
                // For Option types, use proper none initialization
                // For scalar types, use {0} not {{0}}
                let dummy_value = if to_type.starts_with("Option_") {
                    format!("({}){{.tag = {}_tag_none}}", to_type, to_type)
                } else if to_type == "int64_t"
                    || to_type == "double"
                    || to_type == "bool"
                    || to_type == "uint32_t"
                    || to_type == "char"
                {
                    // Scalar types - use plain 0
                    "0".to_string()
                } else {
                    // Struct types - use compound literal with zero init
                    format!("({}){{0}}", to_type)
                };
                return format!("({{ {}; {}; }})", expr_code, dummy_value);
            }
            // For other void expressions, just return as-is (will be a type error)
        }

        // Handle Option_X to Option_Y conversions where X is a variant of Y
        if from_type.starts_with("Option_") && to_type.starts_with("Option_") {
            let from_inner = &from_type[7..]; // e.g., "Lit_String"
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

                // Handle nested unions: e.g., Lit_String -> Literal -> Expression
                // Check if any parent of from_inner is itself a variant of to_inner
                for parent in parents.iter() {
                    if let Some(grandparents) = self.variant_to_union.get(parent) {
                        if grandparents.contains(to_inner) {
                            // Need double wrapping:
                            // Lit_String -> Literal -> Expression
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
                                "*_conv_tmp.value" // dereference for value types stored by value
                            } else {
                                "_conv_tmp.value"
                            };

                            // First wrap in middle union (e.g., Literal), then in outer union (e.g., Expression)
                            // Pattern: Lit_String value -> Literal{.tag=Literal_tag_Lit_String, .data.as_Lit_String=value}
                            //       -> Expression{.tag=Expression_tag_Literal, .data.as_Literal=&middle}
                            return format!(
                                "({{ {} _conv_tmp = {}; \
                                (_conv_tmp.tag == {}_tag_some) \
                                ? ({}){{.tag = {}_tag_some, .value = memcpy(malloc(sizeof({})), &(({}){{.tag = {}_tag_{}, .data.as_{} = memcpy(malloc(sizeof({})), &(({}){{.tag = {}_tag_{}, .data.as_{} = {}}}), sizeof({}))}}), sizeof({}))}} \
                                : ({}){{.tag = {}_tag_none}}; }})",
                                from_type, expr_code,
                                from_type,
                                to_type, to_type, to_inner, to_inner, to_inner, parent, parent,
                                parent, parent, parent, from_inner, from_inner, value_access, parent,
                                to_inner,
                                to_type, to_type
                            );
                        }
                    }
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
            // Also handle enum types: X to Option_X where X is an enum
            if from_base == to_inner || from_type == to_inner {
                debug_println!(
                    self,
                    "DEBUG convert_expr_to_type: wrapping '{}' (type {}) in Option_{}",
                    expr_code,
                    from_type,
                    to_inner
                );
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

                // Check for nested union conversion: from_base -> middle -> to_inner
                // e.g., Lit_Int -> Literal -> SwitchLabel
                for middle in parents.iter() {
                    if let Some(grandparents) = self.variant_to_union.get(middle) {
                        if grandparents.contains(to_inner) {
                            // Double wrap: from_base in middle, then middle in to_inner, then wrap in Option
                            // Pattern: Lit_Int* -> Literal{Lit_Int} -> SwitchLabel{Literal} -> Option_SwitchLabel

                            // Some types are stored by value in the inner union (not as pointers)
                            // For Literal union, Lit_String, Lit_Rune, Lit_Float, Lit_Int are stored by value
                            // So we need to dereference the pointer from the expression
                            let value_types =
                                ["Lit_Bool", "Lit_String", "Lit_Rune", "Lit_Float", "Lit_Int"];
                            let inner_value =
                                if value_types.contains(&from_base) && from_type.ends_with('*') {
                                    format!("*{}", expr_code) // dereference pointer for value types
                                } else {
                                    expr_code.to_string()
                                };

                            return format!(
                                "({}){{.tag = {}_tag_some, .value = memcpy(malloc(sizeof({})), &(({}){{.tag = {}_tag_{}, .data.as_{} = memcpy(malloc(sizeof({})), &(({}){{.tag = {}_tag_{}, .data.as_{} = {}}}), sizeof({}))}}), sizeof({}))}}",
                                to_type, to_type, to_inner, to_inner, to_inner, middle, middle,
                                middle, middle, middle, from_base, from_base, inner_value, middle,
                                to_inner
                            );
                        }
                    }
                }
            }
        }

        // No conversion found, return as-is
        expr_code.to_string()
    }

    /// Check if a switch is used purely as a statement (for side effects, not for a value)
    /// A switch is a pure statement if all cases end with control flow or have empty bodies
    #[allow(dead_code)]
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

    /// Generate an if expression as a pure statement (no _if_result wrapper)
    /// Used when an if without else is used as a statement, not an expression
    fn generate_if_as_pure_statement(&mut self, if_expr: &parser::If, is_main: bool) -> String {
        let cond = self.generate_expression(&if_expr.cond, is_main);

        let mut body_code = String::new();
        for stmt in &if_expr.body {
            let stmt_code = self.generate_statement(stmt, is_main);
            body_code.push_str("        ");
            body_code.push_str(&stmt_code);
            body_code.push_str("\n");
        }

        format!("if ({}) {{\n{}    }}", cond, body_code)
    }

    /// Generate a for loop as a pure statement (no result collection)
    /// This is used when the for loop is the entire statement, not part of a larger expression
    fn generate_for_as_pure_statement(&mut self, for_loop: &parser::For, is_main: bool) -> String {
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
                    // Create a unique loop label for break statements to target
                    let loop_label = format!("_loop_exit_{}", self.loop_label_counter);
                    self.loop_label_counter += 1;
                    self.loop_label_stack.push(loop_label.clone());

                    // Generate body statements
                    let mut body_code = String::new();
                    for stmt in &for_loop.body {
                        let stmt_code = self.generate_statement(stmt, is_main);
                        body_code.push_str("        ");
                        body_code.push_str(&stmt_code);
                        body_code.push_str("\n");
                    }

                    // Pop the loop label
                    self.loop_label_stack.pop();

                    // Generate C for-loop with proper iterator variable declaration
                    return format!(
                        "for (int64_t {} = {}; {} < {}; {}++) {{\n{}    }}\n    {}:;",
                        elem_name, begin, elem_name, until, elem_name, body_code, loop_label
                    );
                }
            }
        }

        // Check if the iterator is a List type - generate index-based for loop
        let iter_type = self.infer_expr_type(&for_loop.iter);
        if iter_type.starts_with("List_") {
            let iter_code = self.generate_expression(&for_loop.iter, is_main);

            // Extract the element type from List_ElementType
            let elem_type = &iter_type[5..]; // Remove "List_" prefix

            // Determine if the element type is a struct/union (needs pointer) or value type (enum/primitive)
            let is_struct_type =
                self.seen_types.contains(elem_type) || self.tagged_union_types.contains(elem_type);
            let is_enum_type = self.enum_types.contains(elem_type);

            // For structs and tagged unions, elements are pointers (List has Type**)
            // For enums and primitives, elements are values (List has Type*)
            let c_elem_type = if is_struct_type && !is_enum_type {
                format!("{}*", elem_type)
            } else {
                self.normalize_c_type(elem_type)
            };

            // Create a unique loop label for break statements to target
            let loop_label = format!("_loop_exit_{}", self.loop_label_counter);
            self.loop_label_counter += 1;
            self.loop_label_stack.push(loop_label.clone());

            // Register the element variable type for use in the body
            self.variable_types
                .insert(elem_name.clone(), c_elem_type.clone());

            // Generate body statements (always treat as statement, no result collection)
            let mut body_code = String::new();
            for stmt in &for_loop.body {
                let stmt_code = self.generate_statement(stmt, is_main);
                body_code.push_str("            ");
                body_code.push_str(&stmt_code);
                body_code.push_str("\n");
            }

            // Pop the loop label
            self.loop_label_stack.pop();

            // Generate C for-loop over list elements
            return format!(
                "for (int64_t _idx_{} = 0; _idx_{} < ({}).len; _idx_{}++) {{\n        {} {} = ({}).data[_idx_{}];\n{}}}\n    {}:;",
                elem_name, elem_name, iter_code, elem_name,
                c_elem_type, elem_name, iter_code, elem_name,
                body_code, loop_label
            );
        }

        // Check if the iterator is a struct with a list field (structural iteration)
        // e.g., `for ast |item|` where Ast has `items: List(Definition)`
        let iter_type_base = iter_type.trim_end_matches('*');
        let struct_list_field: Option<(String, String)> =
            if let Some(fields) = self.struct_field_types.get(iter_type_base) {
                fields
                    .iter()
                    .find(|(_, ftype)| ftype.starts_with("List_"))
                    .map(|(k, v)| (k.clone(), v.clone()))
            } else {
                None
            };
        if let Some((field_name, field_type)) = struct_list_field {
            let elem_type_name = field_type.strip_prefix("List_").unwrap();

            // Determine if the element type is a struct/union (needs pointer) or value type
            let is_struct_type = self.seen_types.contains(elem_type_name)
                || self.tagged_union_types.contains(elem_type_name);
            let is_enum_type = self.enum_types.contains(elem_type_name);

            let c_elem_type = if is_struct_type && !is_enum_type {
                format!("{}*", elem_type_name)
            } else {
                self.normalize_c_type(elem_type_name)
            };

            let iter_code = self.generate_expression(&for_loop.iter, is_main);

            // Create a unique loop label for break statements to target
            let loop_label = format!("_loop_exit_{}", self.loop_label_counter);
            self.loop_label_counter += 1;
            self.loop_label_stack.push(loop_label.clone());

            // Register the element variable type for use in the body
            self.variable_types
                .insert(elem_name.clone(), c_elem_type.clone());

            // Generate body statements
            let mut body_code = String::new();
            for stmt in &for_loop.body {
                let stmt_code = self.generate_statement(stmt, is_main);
                body_code.push_str("            ");
                body_code.push_str(&stmt_code);
                body_code.push_str("\n");
            }

            // Pop the loop label
            self.loop_label_stack.pop();

            // Access the list field through the struct pointer: iter->field.data[i]
            let list_access = if iter_type.ends_with('*') {
                format!("({}->{})", iter_code, field_name)
            } else {
                format!("({}.{})", iter_code, field_name)
            };

            return format!(
                "for (int64_t _idx_{} = 0; _idx_{} < {}.len; _idx_{}++) {{\n        {} {} = {}.data[_idx_{}];\n{}}}\n    {}:;",
                elem_name, elem_name, list_access, elem_name,
                c_elem_type, elem_name, list_access, elem_name,
                body_code, loop_label
            );
        }

        // Fallback: generate a TODO comment for unknown iteration types
        let iter_code = self.generate_expression(&for_loop.iter, is_main);
        format!(
            "// TODO: For loop over unknown type '{}': for {} in {} {{ ... }}",
            iter_type, elem_name, iter_code
        )
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

        // Check if condition type is a tagged union (struct with tag + data union)
        // Tagged unions like Operator* can't be used with C switch
        let cond_base_type = cond_type.trim_end_matches('*').trim();
        let cond_is_tagged_union = self.tagged_union_types.contains(cond_base_type);

        if is_option_switch || cond_is_option || cond_is_tagged_union {
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
        // Track that we're inside a switch for break semantics
        self.in_switch_depth += 1;

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
                        '\0' => "\\0".to_string(),
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

        // Done with this switch
        self.in_switch_depth -= 1;

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
        let cond_base_type = cond_type.trim_end_matches('*').trim();

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
                            '\0' => "\\0".to_string(),
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
                            // Check if we're switching on a tagged union (e.g., Operator*)
                            if self.tagged_union_types.contains(cond_base_type) {
                                // For tagged unions, we need to check the tag and the inner data
                                let variant_name = &ident.name;
                                let mut found_condition: Option<String> = None;

                                // Check BinaryOperator first
                                if let Some(variants) = self.enum_variants.get("BinaryOperator") {
                                    if variants.contains(variant_name.as_str()) {
                                        found_condition = Some(format!(
                                            "{}->tag == {}_tag_BinaryOperator && {}->data.as_BinaryOperator == BinaryOperator_{}",
                                            cond, cond_base_type, cond, variant_name
                                        ));
                                    }
                                }

                                // Check UnaryOperator if not found
                                if found_condition.is_none() {
                                    if let Some(variants) = self.enum_variants.get("UnaryOperator")
                                    {
                                        if variants.contains(variant_name.as_str()) {
                                            found_condition = Some(format!(
                                                "{}->tag == {}_tag_UnaryOperator && {}->data.as_UnaryOperator == UnaryOperator_{}",
                                                cond, cond_base_type, cond, variant_name
                                            ));
                                        }
                                    }
                                }

                                // Fallback: check tagged_union_labels
                                if found_condition.is_none() {
                                    if let Some(labels) =
                                        self.tagged_union_labels.get(cond_base_type)
                                    {
                                        if labels.contains(variant_name.as_str()) {
                                            found_condition = Some(format!(
                                                "{}->tag == {}_tag_{}",
                                                cond, cond_base_type, variant_name
                                            ));
                                        }
                                    }
                                }

                                found_condition.unwrap_or_else(|| {
                                    let qualified = self.qualify_identifier(&ident.name);
                                    format!("{} == {}", cond, qualified)
                                })
                            } else {
                                let qualified = self.qualify_identifier(&ident.name);
                                format!("{} == {}", cond, qualified)
                            }
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
                            // Use cond_base_type for the tag enum, and ty.name for the variant
                            // e.g., switch on Definition* with case Fn -> item->tag == Definition_tag_Fn
                            format!("{}->tag == {}_tag_{}", cond, cond_base_type, ty.name)
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

            // For Type case labels on tagged unions, shadow the variable with the inner typed value
            // e.g., switch item { Fn { ... } } -> Fn* item = _outer_item->data.as_Fn; inside the Fn case
            // We use a nested scope and rename the outer variable to avoid conflicts
            let mut shadow_var: Option<(String, String)> = None; // (var_name, old_type)
            let mut created_it_binding = false; // whether we created an 'it' variable binding
            let mut it_old_type: Option<String> = None; // old type for 'it' if we shadow it

            if let parser::SwitchLabel::Type(ty) = &case.label {
                if self.tagged_union_types.contains(cond_base_type) {
                    // The condition might be a complex expression (like defs.data[0])
                    // We need to use a safe variable name for the temp and shadow
                    let safe_var_name = if cond.chars().all(|c| c.is_alphanumeric() || c == '_') {
                        cond.to_string()
                    } else {
                        // Generate a unique temp name for complex expressions
                        format!("_switch_val_{}", self.loop_label_counter)
                    };
                    self.loop_label_counter += 1;

                    let variant_type = format!("{}*", ty.name);
                    let old_type = self.variable_types.get(&safe_var_name).cloned();
                    shadow_var = Some((safe_var_name.clone(), old_type.unwrap_or_default()));
                    self.variable_types
                        .insert(safe_var_name.clone(), variant_type.clone());

                    // Also create 'it' binding for implicit access to the variant data
                    it_old_type = self.variable_types.get("it").cloned();
                    created_it_binding = true;
                    self.variable_types
                        .insert("it".to_string(), variant_type.clone());

                    // Generate the shadowing assignment using a temp for the outer value
                    // First save the outer pointer, then create the inner typed value
                    code.push_str(&format!(
                        "        {}* _outer_{} = {};\n",
                        cond_base_type, safe_var_name, cond
                    ));
                    code.push_str(&format!(
                        "        {}* {} = _outer_{}->data.as_{};\n",
                        ty.name, safe_var_name, safe_var_name, ty.name
                    ));
                    code.push_str(&format!("        {}* it = {};\n", ty.name, safe_var_name));
                }
            } else if let parser::SwitchLabel::Ident(ident) = &case.label {
                // For Ident labels on tagged unions (e.g., StructDef in `switch type_def.kind`),
                // generate variant data extraction and 'it' binding
                if self.tagged_union_types.contains(cond_base_type)
                    && ident.name != "else"
                    && ident.name != "some"
                    && ident.name != "none"
                {
                    let variant_name = &ident.name;
                    // Check if this ident is actually a variant of the tagged union
                    let is_variant = self
                        .tagged_union_labels
                        .get(cond_base_type)
                        .map_or(false, |labels| labels.contains(variant_name.as_str()));

                    if is_variant {
                        let safe_var_name = if cond.chars().all(|c| c.is_alphanumeric() || c == '_')
                        {
                            cond.to_string()
                        } else {
                            format!("_switch_val_{}", self.loop_label_counter)
                        };
                        self.loop_label_counter += 1;

                        let variant_type = format!("{}*", variant_name);
                        let old_type = self.variable_types.get(&safe_var_name).cloned();
                        shadow_var = Some((safe_var_name.clone(), old_type.unwrap_or_default()));
                        self.variable_types
                            .insert(safe_var_name.clone(), variant_type.clone());

                        // Create 'it' binding for implicit variant data access
                        it_old_type = self.variable_types.get("it").cloned();
                        created_it_binding = true;
                        self.variable_types
                            .insert("it".to_string(), variant_type.clone());

                        code.push_str(&format!(
                            "        {}* _outer_{} = {};\n",
                            cond_base_type, safe_var_name, cond
                        ));
                        code.push_str(&format!(
                            "        {}* {} = _outer_{}->data.as_{};\n",
                            variant_name, safe_var_name, safe_var_name, variant_name
                        ));
                        code.push_str(&format!(
                            "        {}* it = {};\n",
                            variant_name, safe_var_name
                        ));
                    }
                }
            }

            // Generate case body
            for stmt in &case.body {
                let stmt_code = self.generate_statement(stmt, is_main);
                code.push_str("        ");
                code.push_str(&stmt_code);
                code.push_str("\n");
            }

            // Restore the old type if we shadowed
            if let Some((var_name, old_type)) = shadow_var {
                if old_type.is_empty() {
                    self.variable_types.remove(&var_name);
                } else {
                    self.variable_types.insert(var_name, old_type);
                }
            }

            // Restore the old 'it' type if we created the binding
            if created_it_binding {
                if let Some(old_it) = it_old_type {
                    self.variable_types.insert("it".to_string(), old_it);
                } else {
                    self.variable_types.remove("it");
                }
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

        // Check if condition type is a tagged union (struct with tag + data union)
        // Tagged unions like Operator* can't be used with C switch
        let cond_base_type = cond_type.trim_end_matches('*').trim();
        let cond_is_tagged_union = self.tagged_union_types.contains(cond_base_type);

        if can_use_c_switch && !is_option_switch && !cond_is_option && !cond_is_tagged_union {
            self.generate_c_switch(&cond, &cond_type, &switch_expr.cases, is_main)
        } else {
            self.generate_if_else_chain(&cond, &cond_type, &switch_expr.cases, is_main)
        }
    }

    fn generate_c_switch(
        &mut self,
        cond: &str,
        cond_type: &str,
        cases: &[parser::SwitchCase],
        is_main: bool,
    ) -> String {
        // Track that we're inside a switch for break semantics
        self.in_switch_depth += 1;

        // Infer the result type from the first non-control-flow case body
        let mut result_type = self.infer_switch_result_type(cases);

        // If we have a function return type, use it when:
        // 1. The inferred type is a simple enum type that might be wrong due to name collision
        // 2. The function return type is an Option of that enum or a compatible type
        if let Some(ref ret_ty) = self.current_return_type {
            let mapped_return = self.map_type(ret_ty);
            debug_println!(
                self,
                "DEBUG generate_c_switch: inferred result_type='{}', function return='{}'",
                result_type,
                mapped_return
            );

            // Check if the inferred type is an enum that's a variant inside an Option return type
            if mapped_return.starts_with("Option_") {
                let inner = &mapped_return[7..]; // e.g., "ast_Expression"
                let result_base = result_type.trim_end_matches('*');
                // If the inferred type matches the inner type of the Option, use the Option type
                if result_base == inner {
                    debug_println!(
                        self,
                        "DEBUG generate_c_switch: using function return type '{}' instead of '{}'",
                        mapped_return,
                        result_type
                    );
                    result_type = mapped_return;
                } else if self.enum_types.contains(&result_type) && self.enum_types.contains(inner)
                {
                    // Both are enums - check if they share variant names (collision case)
                    // In this case, prefer the function's return type
                    if let (Some(inferred_variants), Some(return_variants)) = (
                        self.enum_variants.get(&result_type),
                        self.enum_variants.get(inner),
                    ) {
                        // Check if the variants we're returning are actually from the return type's enum
                        let has_collision = inferred_variants
                            .intersection(return_variants)
                            .next()
                            .is_some();
                        if has_collision {
                            debug_println!(self, 
                                "DEBUG generate_c_switch: enum variant collision detected, using function return type '{}' instead of '{}'",
                                mapped_return, result_type
                            );
                            result_type = mapped_return;
                        }
                    }
                }
            }
        }

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
                        '\0' => "\\0".to_string(),
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
                        // Enum variant - qualify it with the condition type as hint
                        // This ensures that when switching on TokenKind, we use TokenKind_xxx prefixes
                        debug_println!(
                            self,
                            "DEBUG generate_c_switch: ident='{}', cond_type='{}'",
                            ident.name,
                            cond_type
                        );
                        let final_label = if ident.name == "none" {
                            // Handle Option none variant
                            if cond_type.starts_with("Option_") || cond_type.contains("Option") {
                                format!("{}_tag_none", cond_type.replace("*", ""))
                            } else {
                                "default".to_string()
                            }
                        } else if ident.name == "some" {
                            // Handle Option some variant
                            if cond_type.starts_with("Option_") || cond_type.contains("Option") {
                                format!("{}_tag_some", cond_type.replace("*", ""))
                            } else {
                                "default".to_string()
                            }
                        } else {
                            // Use the condition type as a hint for qualification
                            self.qualify_identifier_with_hint(&ident.name, Some(cond_type))
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

            // Track if we generated an early return (to skip break)
            let mut generated_early_return = false;

            if !case.body.is_empty() {
                let last_idx = case.body.len() - 1;
                for (idx, stmt) in case.body.iter().enumerate() {
                    if idx == last_idx {
                        // Last statement - assign to _switch_result if it's an expression
                        match stmt {
                            parser::Statement::Expression(expr) => {
                                // Check if this is a `none` that should cause early return
                                // When the enclosing function returns Option_T and a switch case
                                // contains just `none`, it should return from the function, not assign
                                if let parser::Expression::Ident(ident) = expr {
                                    if ident.name == "none" {
                                        debug_println!(self, 
                                            "DEBUG: Found 'none' in switch case, current_return_type={:?}",
                                            self.current_return_type
                                        );
                                        if let Some(ref ret_ty) = self.current_return_type {
                                            let mapped_ret = self.map_type(ret_ty);
                                            debug_println!(
                                                self,
                                                "DEBUG: mapped_ret='{}', starts_with Option_={}",
                                                mapped_ret,
                                                mapped_ret.starts_with("Option_")
                                            );
                                            if mapped_ret.starts_with("Option_") {
                                                // Generate early return with none
                                                code.push_str(&format!(
                                                    "            return ({}){{.tag = {}_tag_none}};\n",
                                                    mapped_ret, mapped_ret
                                                ));
                                                code.push_str("        }\n");
                                                generated_early_return = true;
                                                break; // Exit the statement loop
                                            }
                                        }
                                    }
                                }

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
                                    let mut expr_type = self.infer_expr_type(expr);

                                    // When the result type is Option_X and the expression is an identifier,
                                    // check if that identifier is a variant in enum X. If so, use X as the type
                                    // instead of whatever infer_expr_type returns (which might find a different enum first).
                                    if result_type.starts_with("Option_") {
                                        let expected_inner = &result_type[7..]; // e.g., "Assignment_1"
                                        debug_println!(self, 
                                            "DEBUG switch Option: result_type={}, expected_inner={}, expr_type={}, expr_code={}",
                                            result_type, expected_inner, expr_type, expr_code
                                        );
                                        if let parser::Expression::Ident(ident) = expr {
                                            debug_println!(self, 
                                                "DEBUG switch Option ident: name={}, enum_variants.get({})={:?}",
                                                ident.name, expected_inner, self.enum_variants.get(expected_inner)
                                            );
                                            if let Some(variants) =
                                                self.enum_variants.get(expected_inner)
                                            {
                                                if variants.contains(&ident.name) {
                                                    debug_println!(self, "DEBUG switch Option: found variant, setting expr_type to {}", expected_inner);
                                                    expr_type = expected_inner.to_string();
                                                }
                                            }
                                        }
                                    }

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

            // Skip break and closing brace if we already generated an early return
            if generated_early_return {
                continue;
            }

            if needs_break {
                code.push_str("            break;\n");
            }
            code.push_str("        }\n");
        }

        code.push_str("    } _switch_result; })");

        // Done with this switch
        self.in_switch_depth -= 1;

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
                            '\0' => "\\0".to_string(),
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
                            // Check if we're switching on a tagged union (e.g., Operator*)
                            let cond_base_type = cond_type.trim_end_matches('*').trim();
                            if self.tagged_union_types.contains(cond_base_type) {
                                // For tagged unions, we need to check the tag and the inner data
                                // Find which inner enum contains this variant
                                let variant_name = &ident.name;

                                // Helper to generate the condition for a tagged union variant
                                let mut found_condition: Option<String> = None;

                                // Check BinaryOperator first
                                if let Some(variants) = self.enum_variants.get("BinaryOperator") {
                                    if variants.contains(variant_name.as_str()) {
                                        found_condition = Some(format!(
                                            "{}->tag == {}_tag_BinaryOperator && {}->data.as_BinaryOperator == BinaryOperator_{}",
                                            cond, cond_base_type, cond, variant_name
                                        ));
                                    }
                                }

                                // Check UnaryOperator if not found
                                if found_condition.is_none() {
                                    if let Some(variants) = self.enum_variants.get("UnaryOperator")
                                    {
                                        if variants.contains(variant_name.as_str()) {
                                            found_condition = Some(format!(
                                                "{}->tag == {}_tag_UnaryOperator && {}->data.as_UnaryOperator == UnaryOperator_{}",
                                                cond, cond_base_type, cond, variant_name
                                            ));
                                        }
                                    }
                                }

                                // Fallback: try to find in any enum that's a variant of this tagged union
                                if found_condition.is_none() {
                                    if let Some(labels) =
                                        self.tagged_union_labels.get(cond_base_type)
                                    {
                                        if labels.contains(variant_name.as_str()) {
                                            // This is a label-only variant of the tagged union itself
                                            found_condition = Some(format!(
                                                "{}->tag == {}_tag_{}",
                                                cond, cond_base_type, variant_name
                                            ));
                                        }
                                    }
                                }

                                // Use the found condition or fall back to regular comparison
                                found_condition.unwrap_or_else(|| {
                                    let qualified = self.qualify_identifier(&ident.name);
                                    format!("{} == {}", cond, qualified)
                                })
                            } else {
                                // Regular enum variant comparison - qualify the identifier
                                let qualified = self.qualify_identifier(&ident.name);
                                format!("{} == {}", cond, qualified)
                            }
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

            // Close the inner brace for the case body
            // Note: NO break statement - this is an if-else chain, not a C switch
            code.push_str("        }\n");
            // Close the if/else-if outer brace
            code.push_str("    }");
        }

        // Final closing: just the statement expression wrapper
        code.push_str(" _switch_result; })");
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

        // Check if condition type is a tagged union (struct with tag + data union)
        // Tagged unions like Operator* can't be used with C switch
        let cond_base_type = cond_type.trim_end_matches('*').trim();
        let cond_is_tagged_union = self.tagged_union_types.contains(cond_base_type);

        // Check if we can use a C switch (only for simple cases without Option or Type patterns)
        let can_use_c_switch = !is_option_switch
            && !cond_is_option
            && !cond_is_tagged_union
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
        let (actual_cond, _needs_temp) = if cond_is_option {
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
        // Track that we're inside a switch for break semantics
        self.in_switch_depth += 1;

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
                        '\0' => "\\0".to_string(),
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
                        // Use cond_type as hint for enum variant disambiguation in switch-as-statement
                        let qualified =
                            self.qualify_identifier_with_hint(&ident.name, Some(cond_type));
                        code.push_str(&format!("        case {}:\n", qualified));
                    }
                }
                parser::SwitchLabel::Else(_) => {
                    code.push_str("        default:\n");
                }
                _ => {}
            }

            code.push_str("        {\n");

            // Track if we generated an early return (to skip break)
            let mut generated_early_return = false;

            // Generate case body with assignment
            if !case.body.is_empty() {
                let last_idx = case.body.len() - 1;

                for (idx, stmt) in case.body.iter().enumerate() {
                    if idx == last_idx {
                        match stmt {
                            parser::Statement::Expression(expr) => {
                                // Check if this is a `none` that should cause early return
                                // When the enclosing function returns Option_T and a switch case
                                // contains just `none`, it should return from the function, not assign
                                if let parser::Expression::Ident(ident) = expr {
                                    if ident.name == "none" {
                                        if let Some(ref ret_ty) = self.current_return_type {
                                            let mapped_ret = self.map_type(ret_ty);
                                            if mapped_ret.starts_with("Option_") {
                                                // Generate early return with none
                                                code.push_str(&format!(
                                                    "            return ({}){{.tag = {}_tag_none}};\n",
                                                    mapped_ret, mapped_ret
                                                ));
                                                code.push_str("        }\n");
                                                generated_early_return = true;
                                                break; // Exit the statement loop
                                            }
                                        }
                                    }
                                }

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
                                    let (expr_code, mut expr_type) =
                                        if let parser::Expression::Ident(ident) = expr {
                                            // First check if this is a label-only tagged union variant
                                            let mut found_label_variant: Option<(String, String)> =
                                                None;
                                            for (union_name, labels) in &self.tagged_union_labels {
                                                if labels.contains(&ident.name) {
                                                    debug_println!(self, 
                                                    "DEBUG generate_c_switch_as_statement: found label-only variant '{}' in union '{}'",
                                                    ident.name, union_name
                                                );
                                                    // Generate constructor for label-only variant
                                                    let constructor = format!(
                                                    "memcpy(malloc(sizeof({union})), &(({union}){{.tag = {union}_tag_{label}}}), sizeof({union}))",
                                                    union = union_name,
                                                    label = ident.name
                                                );
                                                    // The type is a pointer to the union
                                                    let ptr_type = format!("{}*", union_name);
                                                    found_label_variant =
                                                        Some((constructor, ptr_type));
                                                    break;
                                                }
                                            }

                                            if let Some((constructor, label_type)) =
                                                found_label_variant
                                            {
                                                (constructor, label_type)
                                            } else {
                                                // Use var_type as hint for enum variant disambiguation
                                                let var_name_local =
                                                    self.get_variable_name(&ident.name);
                                                let code = if var_name_local == ident.name {
                                                    // Try to qualify as enum variant with type hint
                                                    self.qualify_identifier_with_hint(
                                                        &ident.name,
                                                        Some(var_type),
                                                    )
                                                } else {
                                                    var_name_local
                                                };
                                                (code, self.infer_expr_type(expr))
                                            }
                                        } else {
                                            (
                                                self.generate_expression(expr, is_main),
                                                self.infer_expr_type(expr),
                                            )
                                        };

                                    // When assigning to an Option type and the expression is an identifier,
                                    // check if it's a variant of the Option's inner type
                                    if var_type.starts_with("Option_") {
                                        let expected_inner = &var_type[7..]; // e.g., "Assignment_1"
                                        if let parser::Expression::Ident(ident) = expr {
                                            if let Some(variants) =
                                                self.enum_variants.get(expected_inner)
                                            {
                                                if variants.contains(&ident.name) {
                                                    debug_println!(self, 
                                                        "DEBUG generate_c_switch_as_statement: correcting expr_type from '{}' to '{}' for ident '{}'",
                                                        expr_type, expected_inner, ident.name
                                                    );
                                                    expr_type = expected_inner.to_string();
                                                }
                                            }
                                        }
                                    }

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

            // Skip break and closing brace if we already generated an early return
            if generated_early_return {
                continue;
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

        // Done with this switch
        self.in_switch_depth -= 1;

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
                            '\0' => "\\0".to_string(),
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
                            // Check if we're switching on a tagged union (e.g., Operator*)
                            let cond_base_type = cond_type.trim_end_matches('*').trim();
                            if self.tagged_union_types.contains(cond_base_type) {
                                // For tagged unions, we need to check the tag and the inner data
                                let variant_name = &ident.name;
                                let mut found_condition: Option<String> = None;

                                // Check BinaryOperator first
                                if let Some(variants) = self.enum_variants.get("BinaryOperator") {
                                    if variants.contains(variant_name.as_str()) {
                                        found_condition = Some(format!(
                                            "{}->tag == {}_tag_BinaryOperator && {}->data.as_BinaryOperator == BinaryOperator_{}",
                                            cond, cond_base_type, cond, variant_name
                                        ));
                                    }
                                }

                                // Check UnaryOperator if not found
                                if found_condition.is_none() {
                                    if let Some(variants) = self.enum_variants.get("UnaryOperator")
                                    {
                                        if variants.contains(variant_name.as_str()) {
                                            found_condition = Some(format!(
                                                "{}->tag == {}_tag_UnaryOperator && {}->data.as_UnaryOperator == UnaryOperator_{}",
                                                cond, cond_base_type, cond, variant_name
                                            ));
                                        }
                                    }
                                }

                                // Fallback: check tagged_union_labels
                                if found_condition.is_none() {
                                    if let Some(labels) =
                                        self.tagged_union_labels.get(cond_base_type)
                                    {
                                        if labels.contains(variant_name.as_str()) {
                                            found_condition = Some(format!(
                                                "{}->tag == {}_tag_{}",
                                                cond, cond_base_type, variant_name
                                            ));
                                        }
                                    }
                                }

                                found_condition.unwrap_or_else(|| {
                                    let qualified = self.qualify_identifier(&ident.name);
                                    format!("{} == {}", cond, qualified)
                                })
                            } else {
                                // Qualify enum variants
                                let qualified = self.qualify_identifier(&ident.name);
                                format!("{} == {}", cond, qualified)
                            }
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
            if !case.body.is_empty() {
                let last_idx = case.body.len() - 1;

                for (idx, stmt) in case.body.iter().enumerate() {
                    if idx == last_idx {
                        debug_println!(
                            self,
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

                                    debug_println!(self, "DEBUG if_else_chain_as_statement: var_name='{}', var_type='{}', expr_type='{}'", var_name, var_type, expr_type);

                                    // Check if we need to unwrap an Option type
                                    let should_unwrap = expr_type.starts_with("Option_")
                                        && !var_type.starts_with("Option_");

                                    if expr_type.starts_with("Option_")
                                        && expr_type.contains("Expression")
                                    {
                                        debug_println!(self, "DEBUG: should_unwrap={}, expr_code first 200 chars: {}", should_unwrap, &expr_code[..std::cmp::min(200, expr_code.len())]);
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
                // For non-parameterized types, just return the resolved C name without pointer
                if ty.params.is_empty() {
                    self.resolve_c_name_for_type(ty)
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
                    // Look up the C name for this type using the registry
                    // This handles collision resolution (e.g., Assignment struct vs Assignment enum)
                    // Use module-aware lookup when module qualifier is available

                    let module = ty.module.as_deref().unwrap_or(&self.current_module);

                    // Try module-aware lookups first (struct, then tagged union)
                    if let Some(c_name) = self
                        .type_name_registry
                        .get_struct_c_name_for_module(&ty.name, module)
                    {
                        return format!("{}*", c_name);
                    }
                    if let Some(c_name) = self
                        .type_name_registry
                        .get_tagged_union_c_name_for_module(&ty.name, module)
                    {
                        return format!("{}*", c_name);
                    }

                    // If we have an explicit module qualifier and module-aware lookup failed,
                    // don't fall back to name-only lookup (which could find wrong module's type).
                    // Instead, try the general module-aware lookup for enums etc.
                    if let Some(c_name) = self
                        .type_name_registry
                        .get_c_name_for_module(&ty.name, module)
                    {
                        // Check if this is a pointer type (seen_types minus enum_types)
                        if self.seen_types.contains(&c_name) && !self.enum_types.contains(&c_name) {
                            return format!("{}*", c_name);
                        }
                        return c_name;
                    }

                    // Fall back to name-only lookups (for types not in any specific module)
                    if let Some(struct_c_name) = self.type_name_registry.get_struct_c_name(&ty.name)
                    {
                        return format!("{}*", struct_c_name);
                    }
                    if let Some(union_c_name) =
                        self.type_name_registry.get_tagged_union_c_name(&ty.name)
                    {
                        return format!("{}*", union_c_name);
                    }

                    // For enums or unregistered types, use the resolved name
                    // (enums are value types, not pointers)
                    self.resolve_c_name_for_type(ty)
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
        debug_println!(
            self,
            "DEBUG: forward_decls length = {} bytes",
            self.forward_decls.len()
        );
        debug_println!(self, "DEBUG: header length = {} bytes", self.header.len());
        debug_println!(
            self,
            "DEBUG: impl_code length = {} bytes",
            self.impl_code.len()
        );
        debug_println!(
            self,
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
// Note: Complex types (TokenKind, Operator, etc.) are handled via code generation
// to print appropriate placeholders. This macro handles basic built-in types.
// The default case handles struct pointers and other unknown types.
#define print(x) _Generic((x), \
    char*: print_str, \
    const char*: print_str, \
    int64_t: print_int, \
    int: print_int_from_int, \
    bool: print_bool, \
    double: print_float, \
    List_Rune: print_list_rune, \
    default: print_ptr \
)(x)

void print_str(const char* str);
void print_int(int64_t val);
void print_int_from_int(int val);
void print_bool(bool val);
void print_float(double val);
void print_list_rune(List_Rune list);
void print_ptr(const void* ptr);

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

// String append rune - creates a new string by appending a Unicode codepoint to a string
char* blitz_string_append_rune(char* s, uint32_t rune);

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

        // Add TokenKind to BinaryOperator/UnaryOperator conversion functions
        // These are needed because pattern matching on TokenKind with Operator type
        // produces TokenKind values that need to be converted for use in BinaryOp/UnaryOp structs
        full_header.push_str("// TokenKind to Operator conversion functions\n");
        full_header
            .push_str("static inline BinaryOperator TokenKind_to_BinaryOperator(TokenKind tk) {\n");
        full_header.push_str("    switch (tk) {\n");
        full_header.push_str("        case TokenKind_add: return BinaryOperator_add;\n");
        full_header.push_str("        case TokenKind_sub: return BinaryOperator_sub;\n");
        full_header.push_str("        case TokenKind_mul: return BinaryOperator_mul;\n");
        full_header.push_str("        case TokenKind_div: return BinaryOperator_div;\n");
        full_header.push_str("        case TokenKind_rem: return BinaryOperator_rem;\n");
        full_header.push_str("        case TokenKind_concat: return BinaryOperator_concat;\n");
        full_header.push_str("        case TokenKind_dot: return BinaryOperator_dot;\n");
        full_header.push_str("        case TokenKind_eq: return BinaryOperator_eq;\n");
        full_header.push_str("        case TokenKind_ne: return BinaryOperator_ne;\n");
        full_header.push_str("        case TokenKind_gt: return BinaryOperator_gt;\n");
        full_header.push_str("        case TokenKind_ge: return BinaryOperator_ge;\n");
        full_header.push_str("        case TokenKind_lt: return BinaryOperator_lt;\n");
        full_header.push_str("        case TokenKind_le: return BinaryOperator_le;\n");
        full_header.push_str("        case TokenKind_and_: return BinaryOperator_and_;\n");
        full_header.push_str("        case TokenKind_or_: return BinaryOperator_or_;\n");
        full_header.push_str("        case TokenKind_else_: return BinaryOperator_else_;\n");
        full_header.push_str("        default: return BinaryOperator_add; // fallback\n");
        full_header.push_str("    }\n");
        full_header.push_str("}\n\n");
        full_header
            .push_str("static inline UnaryOperator TokenKind_to_UnaryOperator(TokenKind tk) {\n");
        full_header.push_str("    switch (tk) {\n");
        full_header.push_str("        case TokenKind_not: return UnaryOperator_not;\n");
        full_header.push_str(
            "        case TokenKind_sub: return UnaryOperator_neg; // '-' prefix is negation\n",
        );
        full_header.push_str("        default: return UnaryOperator_not; // fallback\n");
        full_header.push_str("    }\n");
        full_header.push_str("}\n\n");

        // Note: TokenKind_to_Operator function is added after struct definitions
        // because it needs the full Operator struct definition (not just forward declaration)

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
                    // Use pointers for struct types to avoid incomplete type errors
                    // But use values for primitives and List_X types (List_X is a small struct)
                    // Note: "List_" alone is an AST node type, not a generic List - use pointer for it
                    let is_generic_list = param_type.starts_with("List_") && param_type != "List_";
                    let c_type = if self.is_primitive_type(param_type) || is_generic_list {
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
                let is_generic_list = inner_type.starts_with("List_") && inner_type != "List_";
                let c_type = if self.is_primitive_type(inner_type) || is_generic_list {
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

        // Add list append function declarations
        if !self.list_append_needed.is_empty() {
            full_header.push_str("// List append function declarations\n");
            for elem_type in &self.list_append_needed {
                let list_type = format!("List_{}", elem_type);
                // For primitive types, element is the value itself (T)
                // For struct types, element is a pointer (T*)
                let is_primitive = self.is_primitive_type(elem_type);
                let elem_c_type = if is_primitive {
                    elem_type.clone()
                } else {
                    format!("{}*", elem_type)
                };
                full_header.push_str(&format!(
                    "{} blitz_list_append_{}({} list, {} elem);\n",
                    list_type, elem_type, list_type, elem_c_type
                ));
            }
            full_header.push_str("\n");
        }

        // Add list concat function declarations
        if !self.list_concat_needed.is_empty() {
            full_header.push_str("// List concat function declarations\n");
            for elem_type in &self.list_concat_needed {
                let list_type = format!("List_{}", elem_type);
                full_header.push_str(&format!(
                    "{} blitz_list_concat_{}({} a, {} b);\n",
                    list_type, elem_type, list_type, list_type
                ));
            }
            full_header.push_str("\n");
        }

        // Add list equality function declarations
        if !self.list_eq_needed.is_empty() {
            full_header.push_str("// List equality function declarations\n");
            for elem_type in &self.list_eq_needed {
                let list_type = format!("List_{}", elem_type);
                full_header.push_str(&format!(
                    "bool blitz_list_eq_{}({} a, {} b);\n",
                    elem_type, list_type, list_type
                ));
            }
            full_header.push_str("\n");
        }

        // Add type definitions
        full_header.push_str(&self.header);

        // Add TokenKind_to_Operator function AFTER struct definitions
        // This function needs the full Operator struct definition (not just forward declaration)
        full_header.push_str("\n// TokenKind to Operator* conversion function\n");
        full_header.push_str("static inline Operator* TokenKind_to_Operator(TokenKind tk) {\n");
        full_header.push_str("    Operator* op = malloc(sizeof(Operator));\n");
        full_header.push_str("    // Check if it's a unary operator\n");
        full_header.push_str("    if (tk == TokenKind_not) {\n");
        full_header.push_str("        op->tag = Operator_tag_UnaryOperator;\n");
        full_header.push_str("        op->data.as_UnaryOperator = UnaryOperator_not;\n");
        full_header.push_str("    } else {\n");
        full_header.push_str("        // Default: treat as binary operator\n");
        full_header.push_str("        op->tag = Operator_tag_BinaryOperator;\n");
        full_header
            .push_str("        op->data.as_BinaryOperator = TokenKind_to_BinaryOperator(tk);\n");
        full_header.push_str("    }\n");
        full_header.push_str("    return op;\n");
        full_header.push_str("}\n");

        // Add assertion helper and display function declarations
        if !self.display_needed.is_empty() {
            full_header.push_str("\n// Display function declarations for assertion output\n");
            full_header.push_str("void blitz_assert_cmp_failed(const char* cond_str, const char* left_str, const char* left_val, const char* right_str, const char* right_val) __attribute__((noreturn));\n");
            // Sort for deterministic output
            let mut display_types: Vec<_> = self.display_needed.iter().cloned().collect();
            display_types.sort();
            for type_name in &display_types {
                let c_param_type = self.display_param_type(type_name);
                full_header.push_str(&format!(
                    "char* blitz_display_{}({} value);\n",
                    type_name, c_param_type
                ));
            }
            full_header.push_str("\n");
        }

        full_header.push_str("\n#endif // BLITZ_H\n");

        debug_println!(
            self,
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

        if self.include_tests && !self.test_definitions.is_empty() {
            full_impl.push_str("#include <setjmp.h>\n");
            full_impl.push_str("#include <signal.h>\n\n");
            full_impl.push_str("static jmp_buf blitz_test_panic_buf;\n");
            full_impl.push_str("static char* blitz_test_panic_msg = NULL;\n");
            full_impl.push_str("static int blitz_in_test = 0;\n\n");
            // Signal-safe jump buffer for recovering from SIGSEGV/SIGBUS/SIGABRT in tests
            full_impl.push_str("static sigjmp_buf blitz_test_signal_buf;\n");
            full_impl.push_str("static volatile sig_atomic_t blitz_test_signal_caught = 0;\n\n");
            full_impl.push_str("static void blitz_test_signal_handler(int sig) {\n");
            full_impl.push_str("    blitz_test_signal_caught = sig;\n");
            full_impl.push_str("    siglongjmp(blitz_test_signal_buf, sig);\n");
            full_impl.push_str("}\n\n");
        }

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

        // blitz_string_append_rune implementation - appends a Unicode codepoint to a string
        full_impl.push_str("char* blitz_string_append_rune(char* s, uint32_t rune) {\n");
        full_impl.push_str("    size_t len_s = s ? strlen(s) : 0;\n");
        full_impl.push_str("    // Determine how many bytes the UTF-8 encoding needs\n");
        full_impl.push_str("    int rune_len;\n");
        full_impl.push_str("    if (rune < 0x80) rune_len = 1;\n");
        full_impl.push_str("    else if (rune < 0x800) rune_len = 2;\n");
        full_impl.push_str("    else if (rune < 0x10000) rune_len = 3;\n");
        full_impl.push_str("    else rune_len = 4;\n");
        full_impl.push_str("    char* result = (char*)malloc(len_s + rune_len + 1);\n");
        full_impl.push_str("    if (s) memcpy(result, s, len_s);\n");
        full_impl.push_str("    // Encode the rune as UTF-8\n");
        full_impl.push_str("    char* p = result + len_s;\n");
        full_impl.push_str("    if (rune < 0x80) {\n");
        full_impl.push_str("        *p++ = (char)rune;\n");
        full_impl.push_str("    } else if (rune < 0x800) {\n");
        full_impl.push_str("        *p++ = (char)(0xC0 | (rune >> 6));\n");
        full_impl.push_str("        *p++ = (char)(0x80 | (rune & 0x3F));\n");
        full_impl.push_str("    } else if (rune < 0x10000) {\n");
        full_impl.push_str("        *p++ = (char)(0xE0 | (rune >> 12));\n");
        full_impl.push_str("        *p++ = (char)(0x80 | ((rune >> 6) & 0x3F));\n");
        full_impl.push_str("        *p++ = (char)(0x80 | (rune & 0x3F));\n");
        full_impl.push_str("    } else {\n");
        full_impl.push_str("        *p++ = (char)(0xF0 | (rune >> 18));\n");
        full_impl.push_str("        *p++ = (char)(0x80 | ((rune >> 12) & 0x3F));\n");
        full_impl.push_str("        *p++ = (char)(0x80 | ((rune >> 6) & 0x3F));\n");
        full_impl.push_str("        *p++ = (char)(0x80 | (rune & 0x3F));\n");
        full_impl.push_str("    }\n");
        full_impl.push_str("    *p = '\\0';\n");
        full_impl.push_str("    return result;\n");
        full_impl.push_str("}\n\n");

        // todo implementation
        full_impl.push_str("void todo(char* msg) {\n");
        full_impl
            .push_str("    fprintf(stderr, \"TODO: %s\\n\", msg ? msg : \"not implemented\");\n");
        full_impl.push_str("    abort();\n");
        full_impl.push_str("}\n\n");

        // panic implementation
        full_impl.push_str("void panic(const char* message) {\n");
        if self.include_tests && !self.test_definitions.is_empty() {
            full_impl.push_str("    if (blitz_in_test) {\n");
            full_impl.push_str("        blitz_test_panic_msg = strdup(message);\n");
            full_impl.push_str("        longjmp(blitz_test_panic_buf, 1);\n");
            full_impl.push_str("    }\n");
        }
        full_impl.push_str("    fprintf(stderr, \"PANIC: %s\\n\", message);\n");
        full_impl.push_str("    abort();\n");
        full_impl.push_str("}\n\n");

        // print function implementations
        full_impl.push_str("void print_str(const char* str) {\n");
        full_impl.push_str("    printf(\"%s\\n\", str ? str : \"(null)\");\n");
        full_impl.push_str("}\n\n");

        full_impl.push_str("void print_int(int64_t val) {\n");
        full_impl.push_str("    printf(\"%lld\\n\", (long long)val);\n");
        full_impl.push_str("}\n\n");

        full_impl.push_str("void print_bool(bool val) {\n");
        full_impl.push_str("    printf(\"%s\\n\", val ? \"true\" : \"false\");\n");
        full_impl.push_str("}\n\n");

        full_impl.push_str("void print_float(double val) {\n");
        full_impl.push_str("    printf(\"%f\\n\", val);\n");
        full_impl.push_str("}\n\n");

        full_impl.push_str("void print_list_rune(List_Rune list) {\n");
        full_impl.push_str("    printf(\"List_Rune[len=%zu]\\n\", list.len);\n");
        full_impl.push_str("}\n\n");

        full_impl.push_str("void print_int_from_int(int val) {\n");
        full_impl.push_str("    printf(\"%d\\n\", val);\n");
        full_impl.push_str("}\n\n");

        full_impl.push_str("void print_ptr(const void* ptr) {\n");
        full_impl.push_str("    printf(\"<ptr %p>\\n\", ptr);\n");
        full_impl.push_str("}\n\n");

        // blitz_time implementation
        full_impl.push_str("#include <sys/time.h>\n");
        full_impl.push_str("int64_t blitz_time(void) {\n");
        full_impl.push_str("    struct timeval tv;\n");
        full_impl.push_str("    gettimeofday(&tv, NULL);\n");
        full_impl.push_str("    return (int64_t)tv.tv_sec * 1000 + (int64_t)tv.tv_usec / 1000;\n");
        full_impl.push_str("}\n\n");

        // blitz_read implementation
        full_impl.push_str("Option_String blitz_read(char* path) {\n");
        full_impl.push_str("    FILE* f = fopen(path, \"r\");\n");
        full_impl.push_str("    if (!f) {\n");
        full_impl.push_str("        return (Option_String){.tag = Option_String_tag_none};\n");
        full_impl.push_str("    }\n");
        full_impl.push_str("    fseek(f, 0, SEEK_END);\n");
        full_impl.push_str("    long size = ftell(f);\n");
        full_impl.push_str("    fseek(f, 0, SEEK_SET);\n");
        full_impl.push_str("    char* buf = (char*)malloc(size + 1);\n");
        full_impl.push_str("    fread(buf, 1, size, f);\n");
        full_impl.push_str("    buf[size] = '\\0';\n");
        full_impl.push_str("    fclose(f);\n");
        full_impl
            .push_str("    return (Option_String){.tag = Option_String_tag_some, .value = buf};\n");
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
                // - List_X types (except "List_") use value type
                // - non-primitives use pointer type
                let is_generic_list = inner_type.starts_with("List_") && inner_type != "List_";
                let c_type = if self.is_primitive_type(inner_type) || is_generic_list {
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

        // Generate list append function implementations
        if !self.list_append_needed.is_empty() {
            full_impl.push_str("// List append function implementations\n");
            full_impl.push_str(
                "// These functions append an element to a list, returning a new list\n\n",
            );

            for elem_type in &self.list_append_needed {
                let list_type = format!("List_{}", elem_type);
                // For primitive types, element is the value itself (T)
                // For struct types, element is a pointer (T*)
                let is_primitive = self.is_primitive_type(elem_type);
                let elem_c_type = if is_primitive {
                    elem_type.clone()
                } else {
                    format!("{}*", elem_type)
                };

                full_impl.push_str(&format!(
                    "{} blitz_list_append_{}({} list, {} elem) {{\n",
                    list_type, elem_type, list_type, elem_c_type
                ));
                full_impl.push_str("    if (list.len >= list.cap) {\n");
                full_impl.push_str("        size_t new_cap = list.cap == 0 ? 4 : list.cap * 2;\n");
                full_impl.push_str(&format!(
                    "        {}* new_data = realloc(list.data, sizeof({}) * new_cap);\n",
                    elem_c_type, elem_c_type
                ));
                full_impl.push_str("        list.data = new_data;\n");
                full_impl.push_str("        list.cap = new_cap;\n");
                full_impl.push_str("    }\n");
                full_impl.push_str("    list.data[list.len++] = elem;\n");
                full_impl.push_str("    return list;\n");
                full_impl.push_str("}\n\n");
            }
        }

        // Generate list concat function implementations
        if !self.list_concat_needed.is_empty() {
            full_impl.push_str("// List concat function implementations\n");
            full_impl
                .push_str("// These functions concatenate two lists, returning a new list\n\n");

            for elem_type in &self.list_concat_needed {
                let list_type = format!("List_{}", elem_type);
                // For primitive types, element is the value itself (T)
                // For struct types, element is a pointer (T*)
                let is_primitive = self.is_primitive_type(elem_type);
                let elem_c_type = if is_primitive {
                    elem_type.clone()
                } else {
                    format!("{}*", elem_type)
                };

                full_impl.push_str(&format!(
                    "{} blitz_list_concat_{}({} a, {} b) {{\n",
                    list_type, elem_type, list_type, list_type
                ));
                full_impl.push_str("    size_t new_len = a.len + b.len;\n");
                full_impl.push_str("    size_t new_cap = new_len;\n");
                full_impl.push_str(&format!(
                    "    {}* new_data = malloc(sizeof({}) * new_cap);\n",
                    elem_c_type, elem_c_type
                ));
                full_impl.push_str(&format!(
                    "    memcpy(new_data, a.data, sizeof({}) * a.len);\n",
                    elem_c_type
                ));
                full_impl.push_str(&format!(
                    "    memcpy(new_data + a.len, b.data, sizeof({}) * b.len);\n",
                    elem_c_type
                ));
                full_impl.push_str(&format!(
                    "    return ({}){{.data = new_data, .len = new_len, .cap = new_cap}};\n",
                    list_type
                ));
                full_impl.push_str("}\n\n");
            }
        }

        // Generate list equality function implementations
        if !self.list_eq_needed.is_empty() {
            full_impl.push_str("// List equality function implementations\n");
            full_impl.push_str("// These functions compare two lists for equality\n\n");

            for elem_type in &self.list_eq_needed {
                let list_type = format!("List_{}", elem_type);
                // For primitive types, element is the value itself (T)
                // For struct types, element is a pointer (T*)
                let is_primitive = self.is_primitive_type(elem_type);

                full_impl.push_str(&format!(
                    "bool blitz_list_eq_{}({} a, {} b) {{\n",
                    elem_type, list_type, list_type
                ));
                full_impl.push_str("    if (a.len != b.len) return false;\n");
                full_impl.push_str("    for (size_t i = 0; i < a.len; i++) {\n");

                if elem_type == "String" || elem_type == "char*" {
                    // For strings, use strcmp (must check before is_primitive since String is considered primitive)
                    full_impl
                        .push_str("        if (strcmp(a.data[i], b.data[i]) != 0) return false;\n");
                } else if is_primitive {
                    // For primitives, direct comparison
                    full_impl.push_str("        if (a.data[i] != b.data[i]) return false;\n");
                } else if elem_type == "char" || elem_type.ends_with("*") {
                    // For strings/pointers, use strcmp or pointer comparison
                    if elem_type == "char" {
                        full_impl.push_str(
                            "        if (strcmp(a.data[i], b.data[i]) != 0) return false;\n",
                        );
                    } else {
                        full_impl.push_str("        if (a.data[i] != b.data[i]) return false;\n");
                    }
                } else {
                    // For enum types (like TokenKind), direct comparison works
                    // For struct types that are stored as pointers, compare pointers
                    // TODO: For proper deep equality, we'd need per-type eq functions
                    full_impl.push_str("        if (a.data[i] != b.data[i]) return false;\n");
                }

                full_impl.push_str("    }\n");
                full_impl.push_str("    return true;\n");
                full_impl.push_str("}\n\n");
            }
        }

        // Generate display function implementations for assertion output
        if !self.display_needed.is_empty() {
            full_impl.push_str("// Display function implementations for assertion output\n\n");

            // blitz_assert_cmp_failed helper
            full_impl.push_str("void blitz_assert_cmp_failed(const char* cond_str, const char* left_str, const char* left_val, const char* right_str, const char* right_val) {\n");
            full_impl.push_str("    int _len = snprintf(NULL, 0, \"Assertion '%s' failed\\n  left:  %s ('%s')\\n  right: %s ('%s')\\n\", cond_str, left_val, left_str, right_val, right_str);\n");
            full_impl.push_str("    char* _msg = (char*)malloc((size_t)_len + 1);\n");
            full_impl.push_str("    snprintf(_msg, (size_t)_len + 1, \"Assertion '%s' failed\\n  left:  %s ('%s')\\n  right: %s ('%s')\\n\", cond_str, left_val, left_str, right_val, right_str);\n");
            full_impl.push_str("    panic(_msg);\n");
            full_impl.push_str("}\n\n");

            let mut display_types: Vec<_> = self.display_needed.iter().cloned().collect();
            display_types.sort();

            for type_name in &display_types {
                let c_param_type = self.display_param_type(type_name);
                full_impl.push_str(&format!(
                    "char* blitz_display_{}({} value) {{\n",
                    type_name, c_param_type
                ));

                if type_name.starts_with("List_") {
                    // List display: "[elem1, elem2, ...]"
                    let elem_type = type_name.strip_prefix("List_").unwrap();
                    let is_primitive_elem = self.is_primitive_type(elem_type);
                    let elem_c_type = if is_primitive_elem {
                        elem_type.to_string()
                    } else {
                        format!("{}*", elem_type)
                    };

                    full_impl.push_str("    // Calculate total length needed\n");
                    full_impl.push_str("    size_t total_len = 2; // []\n");
                    full_impl.push_str("    char** elem_strs = NULL;\n");
                    full_impl.push_str("    if (value.len > 0) {\n");
                    full_impl.push_str(
                        "        elem_strs = (char**)malloc(sizeof(char*) * value.len);\n",
                    );
                    full_impl.push_str("        for (size_t i = 0; i < value.len; i++) {\n");
                    full_impl.push_str(&format!(
                        "            {} elem = value.data[i];\n",
                        elem_c_type
                    ));
                    let elem_display = Self::display_elem_expr(elem_type, "elem");
                    full_impl.push_str(&format!("            elem_strs[i] = {};\n", elem_display));
                    full_impl.push_str("            total_len += strlen(elem_strs[i]);\n");
                    full_impl.push_str("            if (i > 0) total_len += 2; // \", \"\n");
                    full_impl.push_str("        }\n");
                    full_impl.push_str("    }\n");
                    full_impl.push_str("    char* result = (char*)malloc(total_len + 1);\n");
                    full_impl.push_str("    char* p = result;\n");
                    full_impl.push_str("    *p++ = '[';\n");
                    full_impl.push_str("    for (size_t i = 0; i < value.len; i++) {\n");
                    full_impl.push_str("        if (i > 0) { *p++ = ','; *p++ = ' '; }\n");
                    full_impl.push_str("        size_t slen = strlen(elem_strs[i]);\n");
                    full_impl.push_str("        memcpy(p, elem_strs[i], slen);\n");
                    full_impl.push_str("        p += slen;\n");
                    full_impl.push_str("    }\n");
                    full_impl.push_str("    *p++ = ']';\n");
                    full_impl.push_str("    *p = '\\0';\n");
                    full_impl.push_str("    if (elem_strs) free(elem_strs);\n");
                    full_impl.push_str("    return result;\n");
                } else if type_name.starts_with("Option_") {
                    // Option display: "some(<value>)" or "none"
                    let inner_type = type_name.strip_prefix("Option_").unwrap();
                    full_impl.push_str(&format!(
                        "    if (value.tag == {}_tag_none) {{\n",
                        type_name
                    ));
                    full_impl.push_str("        return \"none\";\n");
                    full_impl.push_str("    }\n");
                    let inner_display = Self::display_elem_expr(inner_type, "value.value");
                    full_impl.push_str(&format!("    char* inner = {};\n", inner_display));
                    full_impl.push_str("    int len = snprintf(NULL, 0, \"some(%s)\", inner);\n");
                    full_impl.push_str("    char* result = (char*)malloc((size_t)len + 1);\n");
                    full_impl
                        .push_str("    snprintf(result, (size_t)len + 1, \"some(%s)\", inner);\n");
                    full_impl.push_str("    return result;\n");
                } else if self.enum_types.contains(type_name.as_str()) {
                    // Enum display: show variant name
                    if let Some(variants) = self.enum_variants.get(type_name.as_str()) {
                        let mut sorted_variants: Vec<_> = variants.iter().cloned().collect();
                        sorted_variants.sort();
                        for variant in &sorted_variants {
                            full_impl.push_str(&format!(
                                "    if (value == {}_{}) return \"{}\";\n",
                                type_name, variant, variant
                            ));
                        }
                    }
                    full_impl.push_str(&format!(
                        "    char* buf = (char*)malloc(64); snprintf(buf, 64, \"{}(%d)\", (int)value); return buf;\n",
                        type_name
                    ));
                } else if self.tagged_union_types.contains(type_name.as_str()) {
                    // Tagged union display: show tag name
                    full_impl.push_str("    if (!value) return \"(null)\";\n");
                    if let Some(variants) = self.enum_variants.get(type_name.as_str()) {
                        let mut sorted_variants: Vec<_> = variants.iter().cloned().collect();
                        sorted_variants.sort();
                        for variant in &sorted_variants {
                            full_impl.push_str(&format!(
                                "    if (value->tag == {}_tag_{}) return \"{}\";\n",
                                type_name, variant, variant
                            ));
                        }
                    }
                    full_impl.push_str(&format!(
                        "    char* buf = (char*)malloc(64); snprintf(buf, 64, \"{}(tag=%d)\", (int)value->tag); return buf;\n",
                        type_name
                    ));
                } else {
                    // Unknown struct - show type name and pointer
                    full_impl.push_str(&format!(
                        "    char* buf = (char*)malloc(64); snprintf(buf, 64, \"<{} %p>\", (void*)value); return buf;\n",
                        type_name
                    ));
                }

                full_impl.push_str("}\n\n");
            }
        }

        full_impl.push_str(&self.impl_code);

        // Generate test runner main if in test mode
        if self.include_tests && !self.test_definitions.is_empty() {
            full_impl.push_str(self.generate_test_runner_main().as_str());
        }

        fs::write(&impl_path, full_impl)
            .map_err(|e| format!("Failed to write {}: {}", impl_path.display(), e))?;

        println!("Generated C files in {}", self.output_dir.display());
        println!("  - blitz_types.h");
        println!("  - blitz.h");
        println!("  - blitz.c");
        if self.include_tests && !self.test_definitions.is_empty() {
            println!(
                "  - {} test function(s) included",
                self.test_definitions.len()
            );
        }

        Ok(())
    }

    /// Generate a test runner main function that runs all tests with setjmp/longjmp for panic recovery
    fn generate_test_runner_main(&self) -> String {
        let mut code = String::new();

        // Add test runner banner and timing helper
        code.push_str(
            "\n// ============================================================================\n",
        );
        code.push_str("// Test runner infrastructure\n");
        code.push_str(
            "// ============================================================================\n\n",
        );
        code.push_str("#include <sys/time.h>\n\n");

        // Helper function to get time in milliseconds
        code.push_str("// Get current time in milliseconds\n");
        code.push_str("double blitz_get_time_ms(void) {\n");
        code.push_str("    struct timeval tv;\n");
        code.push_str("    gettimeofday(&tv, NULL);\n");
        code.push_str("    return (tv.tv_sec * 1000.0) + (tv.tv_usec / 1000.0);\n");
        code.push_str("}\n\n");

        // Generate main function
        code.push_str("int main(int argc, char** argv) {\n");
        code.push_str(
            "    // Disable stdout buffering so output is visible even if a test crashes\n",
        );
        code.push_str("    setbuf(stdout, NULL);\n");
        code.push_str("    int passed = 0;\n");
        code.push_str("    int failed = 0;\n");
        code.push_str("    double total_start_time = blitz_get_time_ms();\n\n");

        code.push_str(&format!("    printf(\"--- TEST OUTPUT ---\\n\\n\");\n"));
        code.push_str(&format!(
            "    printf(\"Running {} test(s)...\\n\\n\", {});\n\n",
            self.test_definitions.len(),
            self.test_definitions.len()
        ));

        // Generate test invocations
        for test in &self.test_definitions {
            // Generate safe function name (same as in generate_test_function)
            let safe_name: String = test
                .name
                .chars()
                .map(|c| if c.is_alphanumeric() { c } else { '_' })
                .collect();
            let c_func_name = format!("blitz_test_{}", safe_name);

            // Escape the test name for C string literal
            let escaped_name = test.name.replace("\\", "\\\\").replace("\"", "\\\"");

            code.push_str("    // Test: ");
            code.push_str(&test.name);
            code.push_str("\n");
            code.push_str("    {\n");
            code.push_str("        blitz_in_test = 1;\n");
            code.push_str("        blitz_test_signal_caught = 0;\n");
            code.push_str("        double test_start_time = blitz_get_time_ms();\n");
            // Install signal handlers for crash recovery
            code.push_str("        struct sigaction sa, old_segv, old_bus, old_abrt;\n");
            code.push_str("        sa.sa_handler = blitz_test_signal_handler;\n");
            code.push_str("        sigemptyset(&sa.sa_mask);\n");
            code.push_str("        sa.sa_flags = 0;\n");
            code.push_str("        sigaction(SIGSEGV, &sa, &old_segv);\n");
            code.push_str("        sigaction(SIGBUS, &sa, &old_bus);\n");
            code.push_str("        sigaction(SIGABRT, &sa, &old_abrt);\n");
            // Outer: signal recovery (SIGSEGV, SIGBUS, SIGABRT)
            code.push_str("        int sig = sigsetjmp(blitz_test_signal_buf, 1);\n");
            code.push_str("        if (sig != 0) {\n");
            code.push_str("            const char* sig_name = sig == SIGSEGV ? \"SIGSEGV\" : sig == SIGBUS ? \"SIGBUS\" : sig == SIGABRT ? \"SIGABRT\" : \"SIGNAL\";\n");
            code.push_str("            printf(\"\\x1b[95mEXIT\\x1b[0m ... \\\"%s\\\"\\n\", \"");
            code.push_str(&escaped_name);
            code.push_str("\");\n");
            code.push_str(
                "            printf(\"  -> \\x1b[91m%s (signal %d)\\x1b[0m\\n\", sig_name, sig);\n",
            );
            code.push_str("            failed++;\n");
            // Inner: panic recovery (blitz panic() calls)
            code.push_str("        } else if (setjmp(blitz_test_panic_buf) == 0) {\n");
            code.push_str(&format!("            {}();\n", c_func_name));
            code.push_str(
                "            double test_duration = blitz_get_time_ms() - test_start_time;\n",
            );
            code.push_str("            if (test_duration > 100.0) {\n");
            code.push_str(
                "                printf(\"\\x1b[93mSLOW\\x1b[0m ... \\\"%s\\\" -- %.2fms\\n\", \"",
            );
            code.push_str(&escaped_name);
            code.push_str("\", test_duration);\n");
            code.push_str("            } else {\n");
            code.push_str("                printf(\"\\x1b[92mPASS\\x1b[0m ... \\\"%s\\\"\\n\", \"");
            code.push_str(&escaped_name);
            code.push_str("\");\n");
            code.push_str("            }\n");
            code.push_str("            passed++;\n");
            code.push_str("        } else {\n");
            code.push_str("            printf(\"\\x1b[91mFAIL\\x1b[0m ... \\\"%s\\\"\\n\", \"");
            code.push_str(&escaped_name);
            code.push_str("\");\n");
            code.push_str("            if (blitz_test_panic_msg) {\n");
            code.push_str(
                "                printf(\"  -> \\x1b[91m%s\\x1b[0m\\n\", blitz_test_panic_msg);\n",
            );
            code.push_str("                free(blitz_test_panic_msg);\n");
            code.push_str("                blitz_test_panic_msg = NULL;\n");
            code.push_str("            }\n");
            code.push_str("            failed++;\n");
            code.push_str("        }\n");
            // Restore original signal handlers
            code.push_str("        sigaction(SIGSEGV, &old_segv, NULL);\n");
            code.push_str("        sigaction(SIGBUS, &old_bus, NULL);\n");
            code.push_str("        sigaction(SIGABRT, &old_abrt, NULL);\n");
            code.push_str("        blitz_in_test = 0;\n");
            code.push_str("    }\n\n");
        }

        // Summary with total execution time
        code.push_str("    double total_duration = blitz_get_time_ms() - total_start_time;\n");
        code.push_str("    printf(\"\\n\");\n");
        code.push_str("    if (failed == 0) {\n");
        code.push_str("        printf(\"test result: \\x1b[92mok\\x1b[0m. %d passed; %d failed; finished in %.2fms\\n\\n\", passed, failed, total_duration);\n");
        code.push_str("    } else {\n");
        code.push_str("        printf(\"test result: \\x1b[91mFAILED\\x1b[0m. %d passed; %d failed; finished in %.2fms\\n\\n\", passed, failed, total_duration);\n");
        code.push_str("    }\n\n");
        code.push_str("    return failed > 0 ? 1 : 0;\n");
        code.push_str("}\n");

        code
    }

    /// Generate if-else expression as a statement with returns in each branch
    /// Used when an if-else is the final expression of a function
    fn generate_if_else_with_returns(
        &mut self,
        left: &parser::Expression,
        right: &parser::Expression,
        ret_type: &str,
        is_main: bool,
    ) -> String {
        // Left should be an If expression
        if let parser::Expression::If(if_expr) = left {
            let cond = self.generate_expression(&if_expr.cond, is_main);

            // Generate then-body with return
            let mut then_code = String::new();
            let then_len = if_expr.body.len();
            for (i, stmt) in if_expr.body.iter().enumerate() {
                let is_last = i == then_len - 1;
                if is_last {
                    // Last statement should be returned
                    match stmt {
                        parser::Statement::Expression(expr) => {
                            let mut expr_code = self.generate_expression(expr, is_main);
                            // Auto-wrap if needed
                            if ret_type.starts_with("Option_") {
                                let expr_type = self.infer_expr_type(expr);
                                if !expr_type.starts_with("Option_") && expr_code != "none" {
                                    // Check for special 'none' identifier
                                    if let parser::Expression::Ident(ident) = expr {
                                        if ident.name == "none" {
                                            expr_code = format!(
                                                "({}){{.tag = {}_tag_none}}",
                                                ret_type, ret_type
                                            );
                                        } else {
                                            expr_code = format!(
                                                "({}){{.tag = {}_tag_some, .value = {}}}",
                                                ret_type, ret_type, expr_code
                                            );
                                        }
                                    } else {
                                        expr_code = format!(
                                            "({}){{.tag = {}_tag_some, .value = {}}}",
                                            ret_type, ret_type, expr_code
                                        );
                                    }
                                }
                            }
                            then_code.push_str(&format!("        return {};\n", expr_code));
                        }
                        _ => {
                            let stmt_code = self.generate_statement(stmt, is_main);
                            then_code.push_str(&format!("        {}\n", stmt_code));
                        }
                    }
                } else {
                    let stmt_code = self.generate_statement(stmt, is_main);
                    then_code.push_str(&format!("        {}\n", stmt_code));
                }
            }

            // Generate else-body with return
            let else_code = match right {
                parser::Expression::Block(stmts) => {
                    let mut code = String::new();
                    let stmts_len = stmts.len();
                    for (i, stmt) in stmts.iter().enumerate() {
                        let is_last = i == stmts_len - 1;
                        if is_last {
                            match stmt {
                                parser::Statement::Expression(expr) => {
                                    let mut expr_code = self.generate_expression(expr, is_main);
                                    if ret_type.starts_with("Option_") {
                                        let expr_type = self.infer_expr_type(expr);
                                        if !expr_type.starts_with("Option_") && expr_code != "none"
                                        {
                                            if let parser::Expression::Ident(ident) = expr {
                                                if ident.name == "none" {
                                                    expr_code = format!(
                                                        "({}){{.tag = {}_tag_none}}",
                                                        ret_type, ret_type
                                                    );
                                                } else {
                                                    expr_code = format!(
                                                        "({}){{.tag = {}_tag_some, .value = {}}}",
                                                        ret_type, ret_type, expr_code
                                                    );
                                                }
                                            } else {
                                                expr_code = format!(
                                                    "({}){{.tag = {}_tag_some, .value = {}}}",
                                                    ret_type, ret_type, expr_code
                                                );
                                            }
                                        }
                                    }
                                    code.push_str(&format!("        return {};\n", expr_code));
                                }
                                _ => {
                                    let stmt_code = self.generate_statement(stmt, is_main);
                                    code.push_str(&format!("        {}\n", stmt_code));
                                }
                            }
                        } else {
                            let stmt_code = self.generate_statement(stmt, is_main);
                            code.push_str(&format!("        {}\n", stmt_code));
                        }
                    }
                    code
                }
                parser::Expression::BinaryOp(binop)
                    if matches!(binop.op, parser::Operator::Else)
                        && matches!(&*binop.left, parser::Expression::If(_)) =>
                {
                    // Nested if-else (elif) - recursively generate
                    self.generate_if_else_with_returns(&binop.left, &binop.right, ret_type, is_main)
                }
                _ => {
                    // Single expression else body
                    let mut expr_code = self.generate_expression(right, is_main);
                    if ret_type.starts_with("Option_") {
                        let expr_type = self.infer_expr_type(right);
                        if !expr_type.starts_with("Option_") && expr_code != "none" {
                            if let parser::Expression::Ident(ident) = right {
                                if ident.name == "none" {
                                    expr_code =
                                        format!("({}){{.tag = {}_tag_none}}", ret_type, ret_type);
                                } else {
                                    expr_code = format!(
                                        "({}){{.tag = {}_tag_some, .value = {}}}",
                                        ret_type, ret_type, expr_code
                                    );
                                }
                            } else {
                                expr_code = format!(
                                    "({}){{.tag = {}_tag_some, .value = {}}}",
                                    ret_type, ret_type, expr_code
                                );
                            }
                        }
                    }
                    format!("        return {};\n", expr_code)
                }
            };

            // Check if this is elif or else
            let is_elif = matches!(right, parser::Expression::BinaryOp(b) 
                if matches!(b.op, parser::Operator::Else) && matches!(&*b.left, parser::Expression::If(_)));

            if is_elif {
                format!("if ({}) {{\n{}    }} else {}", cond, then_code, else_code)
            } else {
                format!(
                    "if ({}) {{\n{}    }} else {{\n{}    }}",
                    cond, then_code, else_code
                )
            }
        } else {
            // Not an if expression - shouldn't happen but handle gracefully
            let expr_code = self.generate_expression(left, is_main);
            format!("return {};", expr_code)
        }
    }
}
