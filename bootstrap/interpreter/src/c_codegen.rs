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
    /// Current function return type (for context-sensitive code generation)
    current_return_type: Option<Type>,
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

        // Generate parameter list
        let mut params = Vec::new();
        for arg in &func.args {
            let param_type = self.map_type(&arg.r#type);
            // Note: mutability is handled in Blitz semantics, not at C level
            // In C, all parameters are passed by value (or pointer for structs)
            params.push(format!("{} {}", param_type, arg.name));
        }
        let params_str = if params.is_empty() {
            "void".to_string()
        } else {
            params.join(", ")
        };

        // Generate function signature
        self.impl_code.push_str(&format!(
            "{} {}({}) {{\n",
            return_type, func.name, params_str
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
                let var_name = &decl.name;

                // If type is empty or "_", try to infer from initialization expression
                if c_type.is_empty() || c_type == "_" {
                    if let Some(init_expr) = &decl.init {
                        c_type = match init_expr {
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
                            _ => {
                                // For complex expressions, default to int64_t as a fallback
                                // In a real compiler, we'd do proper type inference
                                "int64_t".to_string()
                            }
                        };
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
                            var_name,
                        );
                    }

                    let init_code = self.generate_expression(init_expr, is_main);
                    format!("{} {} = {};", c_type, var_name, init_code)
                } else {
                    // Declaration without initialization
                    format!("{} {};", c_type, var_name)
                }
                // Note: is_mut is semantic only in Blitz, not represented in C
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
                ident.name.clone()
            }
            parser::Expression::BinaryOp(binop) => {
                // Map Blitz operators to C operators
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
                    _ => "/* unsupported_op */",
                };

                // Recursively generate left and right expressions
                let left = self.generate_expression(&binop.left, is_main);
                let right = self.generate_expression(&binop.right, is_main);

                // Generate with parentheses for proper precedence
                format!("({} {} {})", left, op_str, right)
            }
            parser::Expression::UnaryOp(unary_op) => {
                // Map Blitz unary operators to C operators
                let op_str = match unary_op.op {
                    parser::Operator::Not => "!",
                    parser::Operator::Neg => "-",
                    _ => "/* unsupported_unary_op */",
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
                    // For UFCS, the receiver is the first argument
                    // Format as: func_name(receiver, arg1, arg2, ...)
                    format!("{}({})", func_name, args.join(", "))
                } else {
                    // Regular function call
                    format!("{}({})", func_name, args.join(", "))
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

                // Format as compound literal: (TypeName){.field1 = val1, .field2 = val2}
                format!("({}){{{}}}", type_name, field_inits.join(", "))
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
                    parser::Lval::Ident(ident) => ident.name.clone(),
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
                // For loops iterate over ranges: for i in 0..10 { body }
                // We need to convert this to a C for loop
                // The iter expression should be a Range or a list
                // For simplicity, we'll handle Range cases and convert to C for loop

                let iter_code = self.generate_expression(&for_loop.iter, is_main);
                let elem_name = &for_loop.elem;

                // Generate body statements
                let mut body_code = String::new();
                for stmt in &for_loop.body {
                    let stmt_code = self.generate_statement(stmt, is_main);
                    body_code.push_str("        ");
                    body_code.push_str(&stmt_code);
                    body_code.push_str("\n");
                }

                // For now, assume iter is a Range-like expression (begin..end)
                // In the AST, ranges are typically BinaryOp with Range operator
                // We'll generate a C for loop: for (int i = begin; i < end; i++)
                // This is a simplification - full implementation would need range analysis
                format!(
                    "// For loop: for {} in {}\n    {{\n        /* TODO: proper range iteration */\n{}    }}",
                    elem_name, iter_code, body_code
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
                        // Enum variant - use it directly as a constant
                        code.push_str(&format!("        case {}:\n", ident.name));
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
                        // Enum variant comparison
                        format!("{} == {}", cond, ident.name)
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
