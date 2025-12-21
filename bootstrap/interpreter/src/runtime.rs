use std::{collections::HashMap, path::PathBuf, sync::OnceLock};

use crate::{Body, Func, Registry, Value};

pub static ROOT: OnceLock<PathBuf> = OnceLock::new();

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

        let func = Func {
            params: vec![("path".into(), "String".into())],
            result: "String".into(),
            body: Body::Builtin(Box::new(read)),
        };
        self.insert("read".into(), func);
    }
}

fn print(values: HashMap<String, Value>) -> Value {
    fn print_val(value: &Value) {
        match value {
            Value::String(s) => println!("{s}"),
            Value::Int(i) => println!("{i}"),
            Value::Float(f) => println!("{f}"),
            Value::Bool(b) => println!("{b}"),
            Value::Struct(hash_map) => todo!(),
            Value::Union(s, value) if *s == "err" => panic!("Tried to print error: {:#?}", value),
            Value::Union(_, value) => print_val(value),
            Value::List(values) => todo!(),
            Value::None => println!("None"),
        }
    }
    print_val(&values["s"]);

    Value::None
}

fn read(values: HashMap<String, Value>) -> Value {
    let path = &values["path"];
    let content = match path {
        Value::String(s) => {
            let path = ROOT.get().unwrap().join(s);
            std::fs::read_to_string(&path)
        }
        val => panic!("'read' requires a String as path, got: {:#?}", val),
    };

    match content {
        Ok(s) => Value::Union("ok".into(), Box::new(Value::String(s.into()))),
        Err(err) => Value::Union("err".into(), Box::new(Value::String(err.to_string()))),
    }
}
