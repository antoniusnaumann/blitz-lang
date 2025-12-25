mod lexer;
mod parser;
mod precedence;
mod print;

use std::{
    cmp::{max, min},
    hash::Hash,
    ops::RangeInclusive,
};

pub use lexer::*;
pub use parser::*;
pub use print::*;

type_macro::blitz_types!();

pub struct Ast {
    pub defs: Vec<Definition>,
    pub source: String,
}

impl From<Span> for RangeInclusive<usize> {
    fn from(value: Span) -> Self {
        value.start..=value.end
    }
}

impl Span {
    fn merge(&self, other: &Span) -> Span {
        Self {
            start: min(self.start, other.start),
            end: max(self.end, other.end),
        }
    }

    pub fn start_pos(&self, source: &str) -> Pos {
        let mut pos = Pos { line: 1, col: 1 };
        for (i, ch) in source.char_indices() {
            if i >= self.start {
                break;
            }
            if ch == '\n' {
                pos.col = 1;
                pos.line += 1;
            } else {
                pos.col += 1;
            }
        }

        return pos;
    }

    pub fn end_pos(&self, source: &str) -> Pos {
        let mut pos = Pos { line: 1, col: 1 };
        for (i, ch) in source.char_indices() {
            if i >= self.end {
                break;
            }
            if ch == '\n' {
                pos.col = 1;
                pos.line += 1;
            } else {
                pos.col += 1;
            }
        }

        return pos;
    }

    pub fn report(&self, source: &str) -> String {
        let start = self.start_pos(source);
        let end = self.end_pos(source);
        if start.line != end.line {
            return (&source[RangeInclusive::from(self.clone())]).to_owned();
        }
        let line = start.line;

        let snippet = source.lines().skip(line - 1).next().unwrap();
        let marker = format!("{snippet}     ")
            .char_indices()
            .map(|(i, ch)| {
                let i = i + 1;
                if i >= start.col && i <= end.col {
                    '^'
                } else if ch != '\t' {
                    ' '
                } else {
                    ch
                }
            })
            .collect::<String>();

        format!(
            "{} | {}\n{} | {}",
            line,
            snippet,
            line, // quick hack for aligning line numbers
            marker
        )
    }
}

impl Pos {}

impl Definition {
    pub fn span(&self) -> Span {
        match self {
            Definition::Pub(p) => p.item.span(),
            Definition::Fn(f) => f.span.clone(),
            Definition::Struct(s) => s.span.clone(),
            Definition::Union(union) => union.span.clone(),
            Definition::Alias(_alias) => todo!(),
            Definition::Actor(_actor) => todo!(),
            Definition::Test(_test) => todo!(),
        }
    }
}

impl Lval {
    pub fn span(&self) -> Span {
        match self {
            Lval::Member(member) => member.span.clone(),
            Lval::Index(index) => index.span.clone(),
            Lval::Ident(ident) => ident.span.clone(),
        }
    }
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Constructor(c) => c.span.clone(),
            Expression::Call(c) => c.span.clone(),
            Expression::Member(m) => m.span.clone(),
            Expression::Index(i) => i.span.clone(),
            Expression::Ident(i) => i.span.clone(),
            Expression::Assignment(a) => a.left.span(),
            Expression::BinaryOp(b) => b.span.clone(),
            Expression::UnaryOp(u) => u.span.clone(),
            Expression::For(f) => f.span.clone(),
            Expression::While(w) => w.span.clone(),
            Expression::If(i) => i.span.clone(),
            Expression::Switch(s) => s.span.clone(),
            Expression::List(l) => l.span.clone(),
            Expression::Group(g) => g.span.clone(),
            Expression::Block(stmts) => {
                if stmts.is_empty() {
                    Span { start: 0, end: 0 }
                } else {
                    let first = stmts.first().unwrap().span();
                    let last = stmts.last().unwrap().span();
                    first.merge(&last)
                }
            }
            Expression::Return(e) => e.span(),
            // TODO: add spans here
            Expression::Continue => Span { start: 0, end: 0 },
            Expression::Break => Span { start: 0, end: 0 },
            Expression::String(_s) => Span { start: 0, end: 0 },
            Expression::Number(_num) => Span { start: 0, end: 0 },
            Expression::Char(_ch) => Span { start: 0, end: 0 },
        }
    }
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::Declaration(d) => d.span.clone(),
            Statement::Expression(e) => e.span(),
        }
    }
}

impl Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.params.hash(state);
        self.span.hash(state);
    }
}

impl Eq for Type {}

impl Hash for Span {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.end.hash(state);
    }
}
