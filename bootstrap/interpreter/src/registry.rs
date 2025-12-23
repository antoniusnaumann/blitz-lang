use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use parser::{Ast, Definition, Fn, Span, Statement, Struct, Union};

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

    pub fn insert(&mut self, name: String, func: Func) {
        self.funcs.insert(name, func);
    }
}

impl From<Vec<Ast>> for Registry {
    fn from(value: Vec<Ast>) -> Self {
        let mut reg = Registry::default();
        let mut tys = HashMap::new();

        for ast in value {
            for def in &ast.defs {
                match def {
                    Definition::Struct(ty) => {
                        tys.insert(ty.sig.name.clone(), AstType::Struct(ty.clone()));
                    }
                    Definition::Union(union) => {
                        tys.insert(union.sig.name.clone(), AstType::Union(union.clone()));
                    }
                    _ => {}
                }
            }

            for def in &ast.defs {
                insert_def(&mut reg, &tys, &ast, def);
            }
        }

        reg
    }
}

fn insert_def(reg: &mut Registry, tys: &HashMap<String, AstType>, ast: &Ast, def: &Definition) {
    match def {
        Definition::Pub(p) => {
            insert_def(reg, tys, ast, p.item.deref());
        }
        Definition::Fn(func) => {
            reg.funcs.insert(func.name.clone(), func.clone().into());
        }
        Definition::Struct(ty) => {
            let conv = resolve_type(
                ty.sig.name.clone(),
                tys,
                &HashSet::from_iter(ty.sig.params.iter().map(|p| p.name.clone())),
                &ast.source,
                ty.span.clone(),
            );
            reg.types.insert(ty.sig.name.clone(), conv);
        }
        Definition::Union(union) => {
            let conv = resolve_type(
                union.sig.name.clone(),
                tys,
                &HashSet::from_iter(union.sig.params.iter().map(|p| p.name.clone())),
                &ast.source,
                union.span.clone(),
            );
            reg.types.insert(union.sig.name.clone(), conv);
        }
        Definition::Alias(alias) => todo!(),
        Definition::Actor(actor) => todo!(),
        Definition::Test(test) => todo!(),
    }
}

fn resolve_type(
    name: String,
    tys: &HashMap<String, AstType>,
    generics: &HashSet<String>,
    source: &str,
    span: Span,
) -> Type {
    match name.as_str() {
        "Int" => Type::Int,
        "Float" => Type::Float,
        "String" => Type::String,
        "Bool" => Type::Bool,
        "List" => Type::List(Box::new(Type::Any)),
        _ => match &tys.get(&name) {
            Some(def) => match def {
                AstType::Struct(ty) => {
                    let mut fields = HashMap::new();
                    for field in &ty.fields {
                        fields.insert(
                            field.name.clone(),
                            resolve_type(
                                field.r#type.name.clone(),
                                tys,
                                generics,
                                source,
                                field.r#type.span.clone(),
                            ),
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
                                .map(|t| resolve_type(t.name, tys, generics, source, t.span))
                                .unwrap_or(Type::Void),
                        );
                    }

                    Type::Union(cases)
                }
            },
            None => {
                _ = &dbg!(generics).get(&name).unwrap_or_else(|| {
                    let location = span.report(source);
                    panic!("No type named {name} \n\n{location}")
                });
                // TODO: proper type checking
                Type::Any
            }
        },
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
            result: value.r#type.map(|t| t.name).unwrap_or("Void".into()),
            body: Body::Defined(value.body),
        }
    }
}
