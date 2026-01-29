use std::{
    collections::HashMap,
    path::PathBuf,
    sync::OnceLock,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{Body, Func, Param, Registry, Value};

pub static ROOT: OnceLock<PathBuf> = OnceLock::new();
pub static DEBUG: OnceLock<bool> = OnceLock::new();

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
        register_builtin!(
            self,
            substring,
            [
                ("source", "List", false),
                ("start", "Int", false),
                ("until", "Int", false)
            ],
            "String"
        );
        register_builtin!(self, time, [], "Int");
    }
}

fn as_str(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Int(i) => format!("{i}"),
        Value::Float(f) => format!("{f}"),
        Value::Bool(b) => format!("{b}"),
        Value::Rune(c) => format!("{c}"),
        Value::Struct(fields) => format!(
            "{{{}}}",
            fields.keys().cloned().collect::<Vec<_>>().join(", ")
        ),
        Value::Union(label, value) => format!("{label}: {}", as_str(value)),
        Value::List(values) => {
            let mut result = String::from("[");
            for (i, val) in values.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                result.push_str(&as_str(val));
            }
            result.push_str("]");
            result
        }
        Value::RcList(values) => {
            let mut result = String::from("[");
            for (i, val) in values.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                result.push_str(&as_str(val));
            }
            result.push_str("]");
            result
        }
        Value::Void => format!("Void"),
    }
}

make_builtin!(print(s) {
    println!("{}", as_str(s));

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
    user_panic(format!("\x1b[91m{}\x1b[0m", msg_str));
});

make_builtin!(todo(msg) {
    user_panic(format!("\x1b[96m{}\x1b[0m", as_str(msg)));
});

make_builtin!(chars(s) {
    match s {
        Value::String(s) => Value::RcList(s.chars().map(|c| Value::Rune(c)).collect::<Vec<_>>().into()),
        _ => panic!("'chars' requires String as args")
    }
});

make_builtin!(len(arr) {
    match arr {
        Value::List(list) => Value::Int(list.len().try_into().unwrap()),
        Value::RcList(list) => Value::Int(list.len().try_into().unwrap()),
        _ => panic!("'len' requires List")
    }
});

make_builtin!(substring(source, start, until) {
    match (source, start, until) {
        (Value::RcList(list), Value::Int(start), Value::Int(until)) => {
            let start = *start as usize;
            let until = *until as usize;
            let string: String = list[start..until].iter().map(|v| match v { Value::Rune(ch) =>  ch, _ => panic!("Needs char")}).collect();
            Value::String(string)
        },
        _ => panic!("Wrong args for substring")
    }
});

make_builtin!(time() {
    Value::Int(SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis() as isize)
});
