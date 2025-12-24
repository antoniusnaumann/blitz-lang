use std::{collections::HashMap, path::PathBuf, process::exit, sync::OnceLock};

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
        self.insert_func("print".into(), func);

        let func = Func {
            params: vec![("path".into(), "String".into())],
            result: "String".into(),
            body: Body::Builtin(Box::new(read)),
        };
        self.insert_func("read".into(), func);

        let func = Func {
            params: vec![("msg".into(), "String".into())],
            result: "Never".into(),
            body: Body::Builtin(Box::new(panic)),
        };
        self.insert_func("panic".into(), func);
    }
}

fn as_str(value: &Value) -> String {
    match value {
        Value::String(s) => format!("{s}"),
        Value::Int(i) => format!("{i}"),
        Value::Float(f) => format!("{f}"),
        Value::Bool(b) => format!("{b}"),
        Value::Struct(hash_map) => todo!(),
        Value::Union(s, value) if s == "err" => panic!("Tried to print error: {:#?}", value),
        Value::Union(_, value) => as_str(value),
        Value::List(values) => todo!(),
        Value::None => format!("None"),
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

fn panic(values: HashMap<String, Value>) -> Value {
    eprintln!("\x1b[91m{}", as_str(&values["msg"]));
    exit(1)
}
