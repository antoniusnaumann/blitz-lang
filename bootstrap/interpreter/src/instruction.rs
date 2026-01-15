use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use parser::{BinaryOp, Expression, Operator, Statement};

use crate::registry::{Body, Registry, Value};

pub fn run(st: Statement, vars: &mut HashMap<String, Value>, reg: &Registry) -> Value {
    run_internal(st, vars, reg)
}

/// Top-level run that checks for escaped control flow
pub fn run_checked(st: Statement, vars: &mut HashMap<String, Value>, reg: &Registry) -> Value {
    let result = run_internal(st, vars, reg);

    // Check if control flow escaped inappropriately
    if let Value::Union(label, _) = &result {
        match label.as_str() {
            "__return__" => panic!("Return statement outside of function"),
            "__break__" => panic!("Break statement outside of loop"),
            "__continue__" => panic!("Continue statement outside of loop"),
            _ => {}
        }
    }

    result
}

fn run_internal(st: Statement, vars: &mut HashMap<String, Value>, reg: &Registry) -> Value {
    match st {
        Statement::Declaration(declaration) => {
            let init = declaration
                .init
                .map(|e| run(e.into(), vars, reg))
                .unwrap_or(Value::Void);

            // Propagate control flow sentinels
            if let Value::Union(label, _) = &init {
                if label == "__return__" || label == "__break__" || label == "__continue__" {
                    return init;
                }
            }

            _ = vars.insert(declaration.name.clone(), init).is_none_or(|_| {
                panic!(
                    "Illegal shadowing of {}\n\n{:?}",
                    declaration.name,
                    vars.keys(),
                )
            });
            Value::Void
        }
        Statement::Expression(expression) => match expression {
            Expression::Constructor(c) => {
                let ty = reg
                    .select_type(&c.r#type.name)
                    .expect("Type for constructor does not exist");

                let mut fields = HashMap::new();
                for init_arg in c.args {
                    let val = run(init_arg.init.deref().clone().into(), vars, reg);
                    fields.insert(init_arg.label.name, val);
                }

                let result = Value::Struct(fields);
                assert!(result.matches(ty), "{:#?} does not match {:#?}", result, ty);
                result
            }
            Expression::Call(call) => {
                let funcs = reg
                    .func(&call.name)
                    .unwrap_or_else(|| panic!("Did not find func {}", call.name));

                // Track which arguments are simple identifiers so we can propagate changes back
                let arg_idents: Vec<Option<String>> = call
                    .args
                    .iter()
                    .map(|call_arg| {
                        if let Expression::Ident(ident) = call_arg.init.as_ref() {
                            Some(ident.name.clone())
                        } else {
                            None
                        }
                    })
                    .collect();

                // Evaluate all argument expressions
                let arg_vals: Vec<_> = call
                    .args
                    .iter()
                    .map(|call_arg| run(call_arg.init.as_ref().clone().into(), vars, reg))
                    .collect();

                // Check if we have named arguments
                let has_any_labels = call.args.iter().any(|arg| arg.label.is_some());

                // For named arguments, we need to select the function differently
                // For now, we'll just take the first function if there's only one option
                // In the future, proper overload resolution with named args would be needed
                let func = if has_any_labels && funcs.len() == 1 {
                    &funcs[0]
                } else {
                    reg.select_func(funcs, &arg_vals, None, None)
                };

                let mut args = HashMap::new();
                let params = &func.params;

                // Validate argument count
                if call.args.len() > params.len() {
                    panic!(
                        "Function '{}' expects {} arguments, but {} were provided",
                        call.name,
                        params.len(),
                        call.args.len()
                    );
                }

                // Match arguments to parameters by name if labels are present, otherwise by position
                let has_any_labels = call.args.iter().any(|arg| arg.label.is_some());

                if has_any_labels {
                    // Named argument matching (with support for mixed positional/named)
                    let mut positional_idx = 0;
                    for (call_arg_idx, call_arg) in call.args.iter().enumerate() {
                        if let Some(label) = &call_arg.label {
                            // Named argument - match by parameter name
                            if !params.iter().any(|p| p.name == label.name) {
                                panic!(
                                    "Parameter '{}' not found in function '{}'",
                                    label.name, call.name
                                );
                            }
                            args.insert(label.name.clone(), arg_vals[call_arg_idx].clone());
                        } else {
                            // Positional argument - match to the next unfilled positional parameter
                            if positional_idx >= params.len() {
                                panic!(
                                    "Too many positional arguments for function '{}'",
                                    call.name
                                );
                            }
                            // Find the next parameter that hasn't been filled by a named argument
                            while positional_idx < params.len()
                                && args.contains_key(&params[positional_idx].name)
                            {
                                positional_idx += 1;
                            }
                            if positional_idx >= params.len() {
                                panic!(
                                    "Positional argument has no available parameter in function '{}'",
                                    call.name
                                );
                            }
                            args.insert(
                                params[positional_idx].name.clone(),
                                arg_vals[call_arg_idx].clone(),
                            );
                            positional_idx += 1;
                        }
                    }
                } else {
                    // Positional argument matching (original behavior)
                    for (param, arg) in params.iter().zip(arg_vals.iter()) {
                        args.insert(param.name.clone(), arg.clone());
                    }
                }

                let result = match &func.body {
                    Body::Builtin(executable) => executable(&mut args),
                    Body::Defined(statements) => {
                        let mut result = Value::Void;
                        for s in statements {
                            result = run(s.clone(), &mut args, reg);
                            // Check for return sentinel
                            if let Value::Union(label, val) = &result {
                                if label == "__return__" {
                                    // Extract the value but don't return yet - we need to propagate mutable params
                                    result = *val.clone();
                                    break;
                                }
                            }
                        }
                        result
                    }
                };

                // Propagate changes back to the original variables only if the parameter is mutable
                for (param, maybe_var_name) in params.iter().zip(arg_idents) {
                    if param.mutable {
                        if let Some(var_name) = maybe_var_name {
                            if let Some(updated_value) = args.get(&param.name) {
                                vars.insert(var_name, updated_value.clone());
                            }
                        }
                    }
                }

                result
            }
            Expression::Member(member) => {
                match run(member.parent.deref().clone().into(), vars, reg) {
                    Value::Struct(parent) => parent
                        .get(&member.member)
                        .unwrap_or_else(|| panic!("No entry for {}", member.member))
                        .clone(),
                    other => {
                        panic!("Member operator on non-struct: Queried {member:?} on {other:?}")
                    }
                }
            }
            Expression::Index(index) => {
                let target = run(index.target.deref().clone().into(), vars, reg);
                let index_val = run(index.index.deref().clone().into(), vars, reg);

                match (target, index_val) {
                    (Value::List(list), Value::Int(idx)) => {
                        let idx = idx as usize;
                        if idx < list.len() {
                            list[idx].clone()
                        } else {
                            panic!("Index out of bounds: {} >= {}", idx, list.len())
                        }
                    }
                    (Value::String(s), Value::Int(idx)) => {
                        let idx = idx as usize;
                        let chars: Vec<char> = s.chars().collect();
                        if idx < chars.len() {
                            Value::Rune(chars[idx])
                        } else {
                            panic!("Index out of bounds: {} >= {}", idx, chars.len())
                        }
                    }
                    (target, index) => panic!("Cannot index {:?} with {:?}", target, index),
                }
            }
            Expression::Ident(ident) => vars.get(&ident.name).cloned().unwrap_or_else(|| {
                // panic!("ERROR: Did not find '{}'. Have {:#?}", &ident.name, vars)
                // TODO: check if this union label actually exists
                Value::Union(ident.name, Value::Void.into())
            }),
            Expression::Assignment(assignment) => {
                let rhs = run(assignment.right.deref().clone().into(), vars, reg);

                // Helper function to recursively get mutable reference to the value
                fn get_mut_value<'a>(
                    expr: &Expression,
                    vars: &'a mut HashMap<String, Value>,
                    reg: &Registry,
                ) -> &'a mut Value {
                    match expr {
                        Expression::Ident(ident) => {
                            vars.get_mut(&ident.name).expect("Variable not found")
                        }
                        Expression::Index(nested_index) => {
                            let nested_index_val =
                                run(nested_index.index.deref().clone().into(), vars, reg);
                            let target = get_mut_value(&nested_index.target, vars, reg);

                            match (target, nested_index_val) {
                                (Value::List(list), Value::Int(idx)) => {
                                    let idx = idx as usize;
                                    if idx < list.len() {
                                        &mut list[idx]
                                    } else {
                                        panic!("Index out of bounds: {} >= {}", idx, list.len())
                                    }
                                }
                                (target, index) => {
                                    panic!("Cannot index {:?} with {:?}", target, index)
                                }
                            }
                        }
                        Expression::Member(member) => {
                            let target = get_mut_value(&member.parent, vars, reg);
                            match target {
                                Value::Struct(fields) => {
                                    fields.get_mut(&member.member).expect("Field not found")
                                }
                                _ => panic!("Member access on non-struct"),
                            }
                        }
                        _ => panic!("Invalid target for lvalue"),
                    }
                }

                match assignment.left {
                    parser::Lval::Member(member) => {
                        let target = get_mut_value(&member.parent, vars, reg);
                        match target {
                            Value::Struct(fields) => {
                                *fields.get_mut(&member.member).unwrap() = rhs;
                                Value::Void
                            }
                            _ => panic!("Invalid type for member access: member {:#?}", member),
                        }
                    }
                    parser::Lval::Ident(ident) => {
                        *vars.get_mut(&ident.name).unwrap() = rhs;
                        Value::Void
                    }
                    parser::Lval::Index(index) => {
                        let index_val = run(index.index.deref().clone().into(), vars, reg);
                        let target = get_mut_value(&index.target, vars, reg);

                        match (target, index_val) {
                            (Value::List(list), Value::Int(idx)) => {
                                let idx = idx as usize;
                                if idx < list.len() {
                                    list[idx] = rhs;
                                } else {
                                    panic!("Index out of bounds: {} >= {}", idx, list.len())
                                }
                            }
                            (target, index) => {
                                panic!("Cannot index assign {:?} with {:?}", target, index)
                            }
                        }
                        Value::Void
                    }
                }
            }
            Expression::For(for_loop) => {
                let iter = run(for_loop.iter.deref().clone().into(), vars, reg);
                match iter {
                    Value::List(values) => {
                        let mut results = Vec::new();
                        let mut should_break = false;
                        for val in values {
                            vars.insert(for_loop.elem.clone(), val);
                            let mut result = Value::Void;
                            for s in &for_loop.body {
                                result = run(s.clone(), vars, reg);
                                // Check for control flow sentinels
                                if let Value::Union(label, _) = &result {
                                    if label == "__break__" {
                                        should_break = true;
                                        break;
                                    } else if label == "__continue__" {
                                        break; // Continue to next iteration
                                    } else if label == "__return__" {
                                        // Propagate return up
                                        return result;
                                    }
                                }
                            }
                            if should_break {
                                break;
                            }
                            results.push(result)
                        }
                        Value::List(results)
                    }
                    val => panic!("Cannot iterate over {:#?}", val),
                }
            }
            Expression::While(while_) => {
                let mut results = Vec::new();
                let vars_before_loop: HashSet<_> = vars.keys().cloned().collect();

                loop {
                    let cond = run(while_.cond.deref().clone().into(), vars, reg);
                    match cond {
                        Value::Bool(val) => {
                            if !val {
                                break;
                            }
                            let mut result = Value::Void;
                            let mut should_break = false;
                            for s in &while_.body {
                                result = run(s.clone(), vars, reg);
                                // Check for control flow sentinels
                                if let Value::Union(label, _) = &result {
                                    if label == "__break__" {
                                        should_break = true;
                                        break;
                                    } else if label == "__continue__" {
                                        break; // Continue to next iteration
                                    } else if label == "__return__" {
                                        // Propagate return up
                                        return result;
                                    }
                                }
                            }
                            if should_break {
                                break;
                            }
                            results.push(result)
                        }
                        _ => panic!("While needs boolean as condition"),
                    }

                    // Discard all vars that were created this iteration
                    vars.retain(|k, _| vars_before_loop.contains(k));
                }
                Value::List(results)
            }
            Expression::If(if_) => {
                let cond = run(if_.cond.deref().clone().into(), vars, reg);
                match cond {
                    Value::Bool(val) => {
                        if val {
                            let mut result = Value::Void;
                            for s in if_.body {
                                result = run(s.clone(), vars, reg);
                                // Propagate control flow sentinels
                                if let Value::Union(label, _) = &result {
                                    if label == "__return__"
                                        || label == "__break__"
                                        || label == "__continue__"
                                    {
                                        return result;
                                    }
                                }
                            }
                            Value::Union("some".into(), result.into())
                        } else {
                            Value::Union("none".into(), Value::Void.into())
                        }
                    }
                    _ => panic!("If statement needs boolean as condition"),
                }
            }
            Expression::BinaryOp(bin) if bin.op == Operator::Else => {
                let lhs = run(bin.left.deref().clone().into(), vars, reg);
                if lhs == Value::Union("none".into(), Value::Void.into()) {
                    run(bin.right.deref().clone().into(), vars, reg)
                } else {
                    if let Value::Union(label, val) = &lhs
                        && label == "some"
                    {
                        val.deref().clone()
                    } else {
                        lhs
                    }
                }
            }
            Expression::Switch(switch) => {
                let cond_value = run(switch.cond.deref().clone().into(), vars, reg);

                let mut result = Value::Void;
                let mut matched = false;

                for case in switch.cases {
                    let matches = match &case.label {
                        parser::SwitchLabel::Type(ty) => cond_value.matches(
                            reg.select_type(&ty.name)
                                .expect("Type in switch label must exist"),
                        ),
                        parser::SwitchLabel::Ident(ident) => {
                            // Union label matching
                            if let Value::Union(label, _) = &cond_value {
                                ident.name == *label
                            } else {
                                false
                            }
                        }
                        parser::SwitchLabel::StringLit(lit) => {
                            // String literal matching
                            if let Value::String(s) = &cond_value {
                                lit.value == *s
                            } else {
                                false
                            }
                        }
                        parser::SwitchLabel::CharLit(lit) => {
                            // Rune literal matching
                            if let Value::Rune(c) = &cond_value {
                                lit.value == *c
                            } else {
                                false
                            }
                        }
                        parser::SwitchLabel::NumberLit(lit) => {
                            // Number literal matching
                            match &cond_value {
                                Value::Int(i) => lit.value == *i as f64,
                                Value::Float(f) => lit.value == *f,
                                _ => false,
                            }
                        }
                        parser::SwitchLabel::Discard(_) => {
                            // Discard pattern matches everything
                            true
                        }
                    };

                    if matches {
                        matched = true;
                        // For union types, bind the value to the label name
                        if let Value::Union(label, val) = cond_value.clone() {
                            let case_label = match &case.label {
                                parser::SwitchLabel::Type(ty) => ty.name.clone(),
                                parser::SwitchLabel::Ident(ident) => ident.name.clone(),
                                _ => label,
                            };
                            vars.insert(case_label, *val);
                        }

                        result = run(
                            Statement::Expression(Expression::Block(case.body)),
                            vars,
                            reg,
                        );
                        break;
                    }
                }

                if !matched {
                    panic!("No matching case in switch for value: {:?}", cond_value);
                }

                result
            }
            Expression::List(list) => {
                let mut results = Vec::new();
                for elem in list.elems {
                    results.push(run(elem.into(), vars, reg));
                }
                Value::List(results)
            }
            Expression::Group(group) => run(group.expr.deref().clone().into(), vars, reg),
            Expression::BinaryOp(binary_op) => run_bin_op(binary_op, vars, reg),
            Expression::UnaryOp(unary_op) => run_un_op(unary_op, vars, reg),
            Expression::Block(statements) => {
                let mut result = Value::Void;
                for s in statements {
                    // TODO: proper scoping
                    result = run(s, vars, reg);
                    // Propagate control flow sentinels
                    if let Value::Union(label, _) = &result {
                        if label == "__return__" || label == "__break__" || label == "__continue__"
                        {
                            return result;
                        }
                    }
                }
                result
            }
            Expression::Return(expression) => {
                // Return a special Union value that represents a return
                // This will be caught by function boundaries
                let value = run(expression.deref().clone().into(), vars, reg);
                Value::Union("__return__".into(), Box::new(value))
            }
            Expression::Continue => {
                // Return a special Union value for continue
                Value::Union("__continue__".into(), Box::new(Value::Void))
            }
            Expression::Break => {
                // Return a special Union value for break
                Value::Union("__break__".into(), Box::new(Value::Void))
            }
            Expression::String(s) => Value::String(s),
            Expression::Number(num) => {
                if num.round() == num {
                    Value::Int(num as isize)
                } else {
                    Value::Float(num)
                }
            }
            Expression::Rune(ch) => Value::Rune(ch),
            Expression::BoolLit(b) => Value::Bool(b.value),
        },
    }
}

fn run_bin_op(binary_op: BinaryOp, vars: &mut HashMap<String, Value>, reg: &Registry) -> Value {
    let lhs = run(binary_op.left.deref().clone().into(), vars, reg);
    let rhs = run(binary_op.right.deref().clone().into(), vars, reg);

    bin_op(binary_op.op, vars, reg, &lhs, &rhs)
}

fn bin_op(
    binary_op: Operator,
    vars: &mut HashMap<String, Value>,
    reg: &Registry,
    lhs: &Value,
    rhs: &Value,
) -> Value {
    match (lhs, rhs, binary_op) {
        // Int arithmetic operations
        (Value::Int(lhs), Value::Int(rhs), Operator::Add) => Value::Int(lhs + rhs),
        (Value::Int(lhs), Value::Int(rhs), Operator::Sub) => Value::Int(lhs - rhs),
        (Value::Int(lhs), Value::Int(rhs), Operator::Mul) => Value::Int(lhs * rhs),
        (Value::Int(lhs), Value::Int(rhs), Operator::Div) => Value::Int(lhs / rhs),
        (Value::Int(lhs), Value::Int(rhs), Operator::Rem) => Value::Int(lhs % rhs),

        // Int comparison operations
        (Value::Int(lhs), Value::Int(rhs), Operator::Eq) => Value::Bool(lhs == rhs),
        (Value::Int(lhs), Value::Int(rhs), Operator::Ne) => Value::Bool(lhs != rhs),
        (Value::Int(lhs), Value::Int(rhs), Operator::Gt) => Value::Bool(lhs > rhs),
        (Value::Int(lhs), Value::Int(rhs), Operator::Ge) => Value::Bool(lhs >= rhs),
        (Value::Int(lhs), Value::Int(rhs), Operator::Lt) => Value::Bool(lhs < rhs),
        (Value::Int(lhs), Value::Int(rhs), Operator::Le) => Value::Bool(lhs <= rhs),

        // Float arithmetic operations
        (Value::Float(lhs), Value::Float(rhs), Operator::Add) => Value::Float(lhs + rhs),
        (Value::Float(lhs), Value::Float(rhs), Operator::Sub) => Value::Float(lhs - rhs),
        (Value::Float(lhs), Value::Float(rhs), Operator::Mul) => Value::Float(lhs * rhs),
        (Value::Float(lhs), Value::Float(rhs), Operator::Div) => Value::Float(lhs / rhs),
        (Value::Float(lhs), Value::Float(rhs), Operator::Rem) => Value::Float(lhs % rhs),

        // Float comparison operations
        (Value::Float(lhs), Value::Float(rhs), Operator::Eq) => Value::Bool(lhs == rhs),
        (Value::Float(lhs), Value::Float(rhs), Operator::Ne) => Value::Bool(lhs != rhs),
        (Value::Float(lhs), Value::Float(rhs), Operator::Gt) => Value::Bool(lhs > rhs),
        (Value::Float(lhs), Value::Float(rhs), Operator::Ge) => Value::Bool(lhs >= rhs),
        (Value::Float(lhs), Value::Float(rhs), Operator::Lt) => Value::Bool(lhs < rhs),
        (Value::Float(lhs), Value::Float(rhs), Operator::Le) => Value::Bool(lhs <= rhs),

        // Rune comparison operations
        (Value::Rune(lhs), Value::Rune(rhs), Operator::Eq) => Value::Bool(lhs == rhs),
        (Value::Rune(lhs), Value::Rune(rhs), Operator::Ne) => Value::Bool(lhs != rhs),
        (Value::Rune(lhs), Value::Rune(rhs), Operator::Gt) => Value::Bool(lhs > rhs),
        (Value::Rune(lhs), Value::Rune(rhs), Operator::Ge) => Value::Bool(lhs >= rhs),
        (Value::Rune(lhs), Value::Rune(rhs), Operator::Lt) => Value::Bool(lhs < rhs),
        (Value::Rune(lhs), Value::Rune(rhs), Operator::Le) => Value::Bool(lhs <= rhs),

        // String operations
        (Value::String(_lhs), Value::String(_rhs), Operator::Add) => {
            todo!("String concat")
        }
        (Value::String(lhs), Value::String(rhs), Operator::Concat) => {
            Value::String(format!("{}{}", lhs, rhs))
        }
        (Value::String(lhs), Value::Rune(rhs), Operator::Concat) => {
            Value::String(format!("{}{}", lhs, rhs))
        }
        (Value::String(lhs), Value::String(rhs), Operator::Eq) => Value::Bool(lhs == rhs),
        (Value::String(lhs), Value::String(rhs), Operator::Ne) => Value::Bool(lhs != rhs),

        // Bool operations
        (Value::Bool(lhs), Value::Bool(rhs), Operator::And) => Value::Bool(*lhs && *rhs),
        (Value::Bool(lhs), Value::Bool(rhs), Operator::Or) => Value::Bool(*lhs || *rhs),
        (Value::Bool(lhs), Value::Bool(rhs), Operator::Eq) => Value::Bool(lhs == rhs),
        (Value::Bool(lhs), Value::Bool(rhs), Operator::Ne) => Value::Bool(lhs != rhs),

        // List operations
        (Value::List(lhs), Value::List(rhs), Operator::Concat) => {
            let mut result = lhs.clone();
            result.extend(rhs.clone());
            Value::List(result)
        }
        (Value::List(lhs), rhs, Operator::Concat) => {
            let mut result = lhs.clone();
            result.push(rhs.clone());
            Value::List(result)
        }

        (Value::List(lhs), Value::List(rhs), Operator::Eq) => Value::Bool(lhs == rhs),
        (Value::List(lhs), Value::List(rhs), Operator::Ne) => Value::Bool(lhs != rhs),

        (Value::Union(lhs_label, lhs_value), Value::Union(rhs_label, rhs_value), Operator::Eq) => {
            Value::Bool(lhs_label == rhs_label && lhs_value == rhs_value)
        }
        (Value::Union(lhs_label, lhs_value), Value::Union(rhs_label, rhs_value), Operator::Ne) => {
            Value::Bool(lhs_label != rhs_label || lhs_value != rhs_value)
        }

        (Value::Struct(lhs), Value::Struct(rhs), op @ (Operator::Ne | Operator::Eq)) => {
            if lhs.len() != rhs.len() {
                Value::Bool(if op == Operator::Ne { true } else { false })
            } else {
                let mut same = true;
                for (name, field) in lhs {
                    if let Some(other) = rhs.get(name) {
                        let Value::Bool(eq) = bin_op(Operator::Eq, vars, reg, &field, other) else {
                            unreachable!()
                        };
                        if !eq {
                            same = false;
                            break;
                        }
                    } else {
                        same = false;
                        break;
                    }
                }
                Value::Bool(same)
            }
        }

        (Value::Union(_, u), s @ Value::Struct(_), op @ (Operator::Ne | Operator::Eq))
        | (s @ Value::Struct(_), Value::Union(_, u), op @ (Operator::Ne | Operator::Eq)) => {
            match u.clone().deref() {
                inner @ Value::Struct(_) => bin_op(op, vars, reg, inner, s),
                _ => Value::Bool(if op == Operator::Ne { true } else { false }),
            }
        }

        (lhs, rhs, op) => panic!(
            "Invalid combination value for operator {:#?} : {:#?}, {:#?}",
            op, lhs, rhs
        ),
    }
}

fn run_un_op(
    unary_op: parser::UnaryOp,
    vars: &mut HashMap<String, Value>,
    reg: &Registry,
) -> Value {
    let expr = run(unary_op.expr.deref().clone().into(), vars, reg);

    match (expr, unary_op.op) {
        // Negation for numbers
        (Value::Int(val), Operator::Neg) => Value::Int(-val),
        (Value::Float(val), Operator::Neg) => Value::Float(-val),

        // Logical not for booleans
        (Value::Bool(val), Operator::Not) => Value::Bool(!val),

        (expr, op) => panic!(
            "Invalid combination for unary operator {:#?} : {:#?}",
            op, expr
        ),
    }
}
