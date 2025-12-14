use crate::{Case, Definition, Field, Type};

pub trait Print {
    fn print(&self) -> String;
}

impl Print for Definition {
    fn print(&self) -> String {
        match self {
            Definition::Fn(f) => format!(
                "fn {} ({}) {}",
                f.name,
                f.args.print(),
                f.r#type.as_ref().map_or(String::new(), |t| t.print())
            ),
            Definition::Struct(s) => format!("struct {} {{\n {}}}", s.name, s.fields.print()),
            Definition::Union(u) => format!("union {} {{\n {}}}", u.name, u.cases.print()),
            Definition::Alias(a) => todo!(),
            Definition::Actor(a) => todo!(),
            Definition::Test(t) => todo!(),
        }
    }
}

impl<T: Print> Print for Vec<T> {
    fn print(&self) -> String {
        let mut s = String::new();
        for f in self {
            s.push('\t');
            s.push_str(&f.print());
            s.push('\n');
        }

        s
    }
}

impl Print for Field {
    fn print(&self) -> String {
        format!("{} {}", self.name, self.r#type.print())
    }
}

impl Print for Case {
    fn print(&self) -> String {
        match self {
            Case {
                label: Some(label),
                r#type: Some(ty),
            } => format!("{label}: {}", ty.print()),
            Case {
                label: Some(label),
                r#type: None,
            } => label.clone(),
            Case {
                label: None,
                r#type: Some(ty),
            } => ty.print(),
            _ => unreachable!(),
        }
    }
}

impl Print for Type {
    fn print(&self) -> String {
        format!(
            "{}({})",
            self.name,
            self.args
                .iter()
                .map(Type::print)
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
