use std::collections::HashMap;

use crate::{Body, Func, Registry, Value};

pub trait Builtin {
    fn add_builtins(&mut self);
}

impl Builtin for Registry {
    fn add_builtins(&mut self) {
        let func = Func {
            params: vec![("s".into(), "T".into())],
            result: "Void".into(),
            body: Body::Builtin(Box::new(print)),
        };
        self.insert("print".into(), func);
    }
}

fn print(values: HashMap<String, Value>) -> Value {
    match &values["s"] {
        Value::String(s) => println!("{s}"),
        Value::Int(i) => println!("{i}"),
        Value::Float(f) => println!("{f}"),
        Value::Bool(b) => println!("{b}"),
        Value::Struct(hash_map) => todo!(),
        Value::Union(_, value) => todo!(),
        Value::List(values) => todo!(),
        Value::None => println!("None"),
    }

    Value::None
}
