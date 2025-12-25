use crate::{Case, Definition, Expression, Field, Operator, Statement, Type};

pub trait Print {
    fn print(&self) -> String;
}

impl Print for Definition {
    fn print(&self) -> String {
        match self {
            Definition::Pub(p) => format!("pub {}", p.item.print()),
            Definition::Fn(f) => {
                let mut s = format!(
                    "fn {}({}) {}",
                    f.name,
                    f.args.print(),
                    f.r#type.as_ref().map_or(String::new(), |t| t.print())
                )
                .trim()
                .to_owned();
                if !f.body.is_empty() {
                    s.push_str(" {\n");
                    for stmt in &f.body {
                        s.push_str("  ");
                        s.push_str(&stmt.print());
                        s.push('\n');
                    }
                    s.push('}');
                }
                s
            }
            Definition::Struct(s) => {
                format!("struct {} {{\n {}}}", s.sig.print(), s.fields.print())
            }
            Definition::Union(u) => format!("union {} {{\n {}}}", u.sig.print(), u.cases.print()),
            Definition::Alias(_a) => todo!(),
            Definition::Actor(_a) => todo!(),
            Definition::Test(_t) => todo!(),
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
            self.params
                .iter()
                .map(Type::print)
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

impl Print for Expression {
    fn print(&self) -> String {
        match self {
            Expression::Constructor(c) => {
                format!(
                    "{}({})",
                    c.r#type.name,
                    c.args
                        .iter()
                        .map(|arg| format!("{}: {}", arg.label.name, arg.init.print()))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Expression::Call(c) => {
                if c.ufcs {
                    format!(
                        "{}.{}({})",
                        c.args[0].print(),
                        c.name,
                        c.args[1..]
                            .iter()
                            .map(|e| e.print())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                } else {
                    format!(
                        "{}({})",
                        c.name,
                        c.args
                            .iter()
                            .map(|e| e.print())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            Expression::Member(m) => format!("{}.{}", m.parent.print(), m.member),
            Expression::Index(i) => format!("{}[{}]", i.target.print(), i.index.print()),
            Expression::Ident(i) => i.name.clone(),
            Expression::Assignment(_) => "<assignment>".into(),
            Expression::BinaryOp(b) => {
                format!("({} {} {})", b.left.print(), b.op.print(), b.right.print())
            }
            Expression::UnaryOp(u) => format!("({}{})", u.op.print(), u.expr.print()),
            Expression::For(_) => "<for>".into(),
            Expression::While(_) => "<while>".into(),
            Expression::If(_) => "<if>".into(),
            Expression::Switch(_) => "<switch>".into(),
            Expression::List(l) => format!(
                "[{}]",
                l.elems
                    .iter()
                    .map(|e| e.print())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expression::Group(g) => format!("({})", g.expr.print()),
            Expression::Block(stmts) => {
                if stmts.is_empty() {
                    "{}".into()
                } else {
                    let mut s = String::from("{ ");
                    s.push_str(
                        &stmts
                            .iter()
                            .map(|stmt| stmt.print())
                            .collect::<Vec<_>>()
                            .join("; "),
                    );
                    s.push_str(" }");
                    s
                }
            }
            Expression::Return(e) => format!("return {}", e.print()),
            Expression::Continue => "continue".into(),
            Expression::Break => "break".into(),
            Expression::String(s) => s.clone(),
            Expression::Number(num) => format!("{num}"),
            Expression::Char(ch) => format!("'{}'", ch),
        }
    }
}

impl Print for Operator {
    fn print(&self) -> String {
        match self {
            Operator::Add => "+".into(),
            Operator::Sub => "-".into(),
            Operator::Div => "/".into(),
            Operator::Mul => "*".into(),
            Operator::Rem => "%".into(),
            Operator::Concat => "++".into(),
            Operator::Member => ".".into(),
            Operator::Eq => "==".into(),
            Operator::Ne => "!=".into(),
            Operator::Gt => ">".into(),
            Operator::Ge => ">=".into(),
            Operator::Lt => "<".into(),
            Operator::Le => "<=".into(),
            Operator::Not => "!".into(),
            Operator::Neg => "-".into(),
            Operator::And => "and".into(),
            Operator::Or => "or".into(),
            Operator::Else => "else".into(),
        }
    }
}

impl Print for Statement {
    fn print(&self) -> String {
        match self {
            Statement::Declaration(d) => format!(
                "{} {}: {}{}",
                if d.is_mut { "mut" } else { "let" },
                d.name,
                d.r#type.print(),
                d.init
                    .as_ref()
                    .map_or(String::new(), |e| format!(" = {}", e.print()))
            ),
            Statement::Expression(e) => e.print(),
        }
    }
}
