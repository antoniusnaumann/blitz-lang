use std::{collections::HashMap, path::PathBuf, sync::OnceLock};

use crate::{Body, Func, Param, Registry, Value};

pub static ROOT: OnceLock<PathBuf> = OnceLock::new();

pub struct UserPanic(pub String);
fn user_panic(msg: String) -> ! {
    std::panic::panic_any(UserPanic(msg))
}

macro_rules! make_builtin {
    ($name:ident ( $($param:ident),* $(,)? ) $body:block) => {
        fn $name(values: &mut HashMap<String, Value>) -> Value {
            $(let $param = &values[stringify!($param)];)*
            $body
        }
    };
}

macro_rules! register_builtin {
    ($registry:expr, $func:ident, [$( ($param:expr, $param_type:expr, $mutable:expr) ),* $(,)?], $result:expr) => {
        let func = Func {
            params: vec![$(Param {
                name: $param.into(),
                ty: $param_type.into(),
                mutable: $mutable,
            }),*],
            result: $result.into(),
            body: Body::Builtin(Box::new($func)),
        };
        $registry.insert_func(stringify!($func).into(), func);
    };
}

pub trait Builtin {
    fn add_builtins(&mut self);
}

impl Builtin for Registry {
    fn add_builtins(&mut self) {
        register_builtin!(self, print, [("s", "T", false)], "Void");
        register_builtin!(self, debug, [("s", "T", false)], "T");
        register_builtin!(self, read, [("path", "String", false)], "String");
        register_builtin!(self, panic, [("msg", "String", false)], "Never");
        register_builtin!(self, todo, [("msg", "String", false)], "Never");
        register_builtin!(self, chars, [("s", "String", false)], "List(Rune)");
        register_builtin!(self, len, [("arr", "List", false)], "Int");
    }
}

fn as_str(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Int(i) => format!("{i}"),
        Value::Float(f) => format!("{f}"),
        Value::Bool(b) => format!("{b}"),
        Value::Rune(c) => format!("{c}"),
        Value::Struct(_hash_map) => todo!(),
        Value::Union(label, value) => format!("{label}: {}", as_str(value)),
        Value::List(values) => {
            let mut result = String::from("[");
            for (i, val) in values.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                result.push_str(&match val {
                    Value::String(s) => format!("\"{s}\""),
                    Value::Int(i) => format!("{i}"),
                    Value::Float(f) => format!("{f}"),
                    Value::Bool(b) => format!("{b}"),
                    Value::Rune(c) => format!("'{c}'"),
                    Value::Struct(_) => "struct".to_string(),
                    Value::Union(label, v) => format!("{label}: {}", as_str(v)),
                    Value::List(_) => "[...]".to_string(),
                    Value::Void => "Void".to_string(),
                });
            }
            result.push(']');
            result
        }
        Value::Void => format!("Void"),
    }
}

make_builtin!(print(s) {
    fn print_val(value: &Value) {
        match value {
            Value::String(s) => println!("{s}"),
            Value::Int(i) => println!("{i}"),
            Value::Float(f) => println!("{f}"),
            Value::Bool(b) => println!("{b}"),
            Value::Rune(c) => println!("{c}"),
            Value::Struct(_hash_map) => println!("struct"),
            Value::Union(label, value) => { print!("{label}: "); print_val(value) },
            Value::List(values) => {
                print!("[");
                for (i, val) in values.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    match val {
                        Value::String(s) => print!("\"{s}\""),
                        Value::Int(i) => print!("{i}"),
                        Value::Float(f) => print!("{f}"),
                        Value::Bool(b) => print!("{b}"),
                        Value::Rune(c) => print!("'{c}'"),
                        Value::Struct(_) => print!("struct"),
                        Value::Union(label, value) => { print!("{label}: "); print_val(value) },
                        Value::List(_) => print!("[...]"), // Nested lists shown as [...]
                        Value::Void => print!("Void"),
                    }
                }
                println!("]");
            }
            Value::Void => println!("Void"),
        }
    }
    print_val(s);

    Value::Void
});

make_builtin!(debug(s) {
    dbg!(s).clone()
});

make_builtin!(read(path) {
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
});

make_builtin!(panic(msg) {
    let msg_str = as_str(msg);
    user_panic(format!("\x1b[91m{}", msg_str));
});

make_builtin!(todo(msg) {
    user_panic(format!("\x1b[96m{}", as_str(msg)));
});

make_builtin!(chars(s) {
    match s {
        Value::String(s) => Value::List(s.chars().map(|c| Value::Rune(c)).collect()),
        _ => panic!("'chars' requires String as args")
    }
});

make_builtin!(len(arr) {
    match arr {
        Value::List(list) => Value::Int(list.len().try_into().unwrap()),
        _ => panic!("'len' requires List")
    }
});
