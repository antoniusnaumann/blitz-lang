use std::collections::HashMap;

use parser::{Alias, Definition, Fn, Statement, Struct, Union};

#[derive(Default)]
pub struct Registry {
    types: HashMap<String, Type>,
    funcs: HashMap<String, Func>,
}

pub enum Type {
    String,
    Int,
    Float,
    Bool,
    Struct(HashMap<String, Type>),
    Union(HashMap<String, Type>),
    List(Box<Type>),
    Any,
    Void,
}

pub struct Func {
    pub params: Vec<(String, String)>,
    pub result: String,
    pub body: Body,
}

pub enum Body {
    Builtin(Box<fn(HashMap<String, Value>) -> Value>),
    Defined(Vec<Statement>),
}

#[derive(Clone, Debug)]
pub enum Value {
    String(String),
    Int(isize),
    Float(f64),
    Bool(bool),
    Struct(HashMap<String, Value>),
    Union(String, Box<Value>),
    List(Vec<Value>),
    None,
}

enum AstType {
    Struct(Struct),
    Union(Union),
}

impl Registry {
    pub fn func(&self, name: &str) -> Option<&Func> {
        self.funcs.get(name)
    }
}

impl From<Vec<Definition>> for Registry {
    fn from(value: Vec<Definition>) -> Self {
        let mut reg = Registry::default();
        let mut tys = HashMap::new();

        for def in &value {
            match def {
                Definition::Struct(ty) => {
                    tys.insert(ty.name.clone(), AstType::Struct(ty.clone()));
                }
                Definition::Union(union) => {
                    tys.insert(union.name.clone(), AstType::Union(union.clone()));
                }
                _ => {}
            }
        }

        for def in value {
            match def {
                Definition::Fn(func) => {
                    reg.funcs.insert(func.name.clone(), func.into());
                }
                Definition::Struct(ty) => {
                    let conv = resolve_type(ty.name.clone(), &tys);
                    reg.types.insert(ty.name, conv);
                }
                Definition::Union(union) => {
                    let conv = resolve_type(union.name.clone(), &tys);
                    reg.types.insert(union.name, conv);
                }
                Definition::Alias(alias) => todo!(),
                Definition::Actor(actor) => todo!(),
                Definition::Test(test) => todo!(),
            }
        }

        reg
    }
}

fn resolve_type(name: String, tys: &HashMap<String, AstType>) -> Type {
    match name.as_str() {
        "Int" => Type::Int,
        "Float" => Type::Float,
        "String" => Type::String,
        "Bool" => Type::Bool,
        "List" => Type::List(Box::new(Type::Any)),
        _ => {
            let def = &tys[&name];

            match def {
                AstType::Struct(ty) => {
                    let mut fields = HashMap::new();
                    for field in &ty.fields {
                        fields.insert(
                            field.name.clone(),
                            resolve_type(field.r#type.name.clone(), tys),
                        );
                    }

                    Type::Struct(fields)
                }
                AstType::Union(union) => {
                    let mut cases = HashMap::new();
                    for case in &union.cases {
                        cases.insert(
                            case.label
                                .clone()
                                .or(case.r#type.clone().map(|t| t.name))
                                .unwrap(),
                            case.r#type
                                .clone()
                                .map(|t| resolve_type(t.name, tys))
                                .unwrap_or(Type::Void),
                        );
                    }

                    Type::Union(cases)
                }
            }
        }
    }
}

impl From<Fn> for Func {
    fn from(value: Fn) -> Self {
        Func {
            params: value
                .args
                .iter()
                .map(|arg| (arg.name.clone(), arg.r#type.name.clone()))
                .collect(),
            result: value.r#type.map(|t| t.name).unwrap(),
            body: Body::Defined(value.body),
        }
    }
}
