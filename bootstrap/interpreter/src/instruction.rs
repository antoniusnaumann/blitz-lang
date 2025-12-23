use std::{collections::HashMap, ops::Deref};

use parser::{BinaryOp, Expression, Operator, Statement};

use crate::registry::{Body, Registry, Value};

pub fn run(st: Statement, vars: &mut HashMap<String, Value>, reg: &Registry) -> Value {
    match st {
        Statement::Declaration(declaration) => {
            let init = declaration
                .init
                .map(|e| run(e.into(), vars, reg))
                .unwrap_or(Value::None);
            _ = vars
                .insert(declaration.name.clone(), init)
                .is_none_or(|_| panic!("Illegal shadowing of {}", declaration.name));
            Value::None
        }
        Statement::Expression(expression) => match expression {
            Expression::Call(call) => {
                let func = reg
                    .func(&call.name)
                    .unwrap_or_else(|| panic!("Did not find func {}", call.name));
                let mut args = HashMap::new();
                let params = &func.params;
                for ((name, _ty), arg) in params.iter().zip(call.args) {
                    let expr = run(arg.into(), vars, reg);
                    args.insert(name.clone(), expr);
                }

                match &func.body {
                    Body::Builtin(executable) => executable(args),
                    Body::Defined(statements) => {
                        let mut result = Value::None;
                        for s in statements {
                            result = run(s.clone(), &mut args, reg);
                        }

                        result
                    }
                }
            }
            Expression::Member(member) => {
                let Value::Struct(parent) = run(member.parent.deref().clone().into(), vars, reg)
                else {
                    panic!("Member operator on non-struct")
                };
                parent[&member.member].clone()
            }
            Expression::Ident(ident) => vars
                .get(&ident.name)
                .unwrap_or_else(|| {
                    panic!("ERROR: Did not find '{}'. Have {:#?}", &ident.name, vars)
                })
                .clone(),
            Expression::Assignment(assignment) => {
                let rhs = run(assignment.right.deref().clone().into(), vars, reg);
                match assignment.left {
                    parser::Lval::Member(member) => {
                        let ident = match &*member.parent {
                            Expression::Member(member) => todo!("member chains as lvalue"),
                            Expression::Ident(ident) => ident.name.clone(),
                            _ => {
                                panic!("Invalid parent for LValue. must be an ident")
                            }
                        };
                        match vars.get_mut(&ident).unwrap() {
                            Value::Struct(fields) => {
                                *fields.get_mut(&ident).unwrap() = rhs;
                                Value::None
                            }
                            _ => panic!("Invalid type for member access: member {:#?}", member),
                        }
                    }
                    parser::Lval::Ident(ident) => {
                        *vars.get_mut(&ident.name).unwrap() = rhs;
                        Value::None
                    }
                }
            }
            Expression::For(for_loop) => {
                let iter = run(for_loop.iter.deref().clone().into(), vars, reg);
                match iter {
                    Value::List(values) => {
                        let mut results = Vec::new();
                        for val in values {
                            vars.insert(for_loop.elem.clone(), val);
                            let mut result = Value::None;
                            for s in &for_loop.body {
                                result = run(s.clone(), vars, reg);
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

                loop {
                    let cond = run(while_.cond.deref().clone().into(), vars, reg);
                    match cond {
                        Value::Bool(val) => {
                            if !val {
                                break;
                            }
                            let mut result = Value::None;
                            for s in &while_.body {
                                result = run(s.clone(), vars, reg);
                            }
                            results.push(result)
                        }
                        _ => panic!("While needs boolean as condition"),
                    }
                }
                Value::List(results)
            }
            Expression::If(if_) => {
                let cond = run(if_.cond.deref().clone().into(), vars, reg);
                match cond {
                    Value::Bool(val) => {
                        if val {
                            let mut result = Value::None;
                            for s in if_.body {
                                result = run(s.clone(), vars, reg);
                            }
                            result
                        } else {
                            Value::None
                        }
                    }
                    _ => panic!("If statement needs boolean as condition"),
                }
            }
            Expression::BinaryOp(bin) if bin.op == Operator::Else => {
                let lhs = run(bin.left.deref().clone().into(), vars, reg);
                if matches!(lhs, Value::None) {
                    run(bin.right.deref().clone().into(), vars, reg)
                } else {
                    lhs
                }
            }
            Expression::Switch(switch) => {
                let Value::Union(label, _val) = run(switch.cond.deref().clone().into(), vars, reg)
                else {
                    panic!("Can only switch on union")
                };
                let mut val = Value::None;
                for case in switch.cases {
                    let case_label = match case.label {
                        parser::SwitchLabel::Type(ty) => ty.name,
                        parser::SwitchLabel::Ident(ident) => ident.name,
                    };
                    if case_label == label {
                        val = run(
                            Statement::Expression(Expression::Block(case.body)),
                            vars,
                            reg,
                        );
                        break;
                    }
                }
                val
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
                let mut result = Value::None;
                for s in statements {
                    // TODO: proper scoping
                    result = run(s, vars, reg);
                }
                result
            }
            Expression::Return(expression) => todo!(),
            Expression::Continue => todo!(),
            Expression::Break => todo!(),
            Expression::String(s) => Value::String(s),
            Expression::Number(num) => {
                if num.round() == num {
                    Value::Int(num as isize)
                } else {
                    Value::Float(num)
                }
            }
        },
    }
}

fn run_bin_op(binary_op: BinaryOp, vars: &mut HashMap<String, Value>, reg: &Registry) -> Value {
    let lhs = run(binary_op.left.deref().clone().into(), vars, reg);
    let rhs = run(binary_op.right.deref().clone().into(), vars, reg);

    match (lhs, rhs, binary_op.op) {
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

        // String operations
        (Value::String(lhs), Value::String(rhs), Operator::Add) => {
            panic!("Cannot use '+' on strings, use '++' to concat strings instead")
        }
        (Value::String(lhs), Value::String(rhs), Operator::Concat) => {
            Value::String(format!("{}{}", lhs, rhs))
        }
        (Value::String(lhs), Value::String(rhs), Operator::Eq) => Value::Bool(lhs == rhs),
        (Value::String(lhs), Value::String(rhs), Operator::Ne) => Value::Bool(lhs != rhs),

        // Bool operations
        (Value::Bool(lhs), Value::Bool(rhs), Operator::And) => Value::Bool(lhs && rhs),
        (Value::Bool(lhs), Value::Bool(rhs), Operator::Or) => Value::Bool(lhs || rhs),
        (Value::Bool(lhs), Value::Bool(rhs), Operator::Eq) => Value::Bool(lhs == rhs),
        (Value::Bool(lhs), Value::Bool(rhs), Operator::Ne) => Value::Bool(lhs != rhs),

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
