mod lexer;
mod parser;
mod precedence;
mod print;

use std::{
    cmp::{max, min},
    ops::RangeInclusive,
};

pub use lexer::*;
pub use parser::*;
pub use print::*;

type_macro::blitz_types!();

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

    fn to_pos(&self, source: &str) -> Pos {
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
}

impl Definition {
    pub fn span(&self) -> Span {
        match self {
            Definition::Fn(f) => f.span.clone(),
            Definition::Struct(s) => s.span.clone(),
            Definition::Union(union) => union.span.clone(),
            Definition::Alias(alias) => todo!(),
            Definition::Actor(actor) => todo!(),
            Definition::Test(test) => todo!(),
        }
    }
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Call(c) => c.span.clone(),
            Expression::Member(m) => m.span.clone(),
            Expression::Ident(i) => i.span.clone(),
            Expression::Assignment(_) => todo!("Assignment span"),
            Expression::BinaryOp(b) => b.span.clone(),
            Expression::UnaryOp(u) => u.span.clone(),
            Expression::For(f) => f.span.clone(),
            Expression::While(w) => w.span.clone(),
            Expression::If(i) => i.span.clone(),
            Expression::Switch(_) => todo!("Switch span"),
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
            Expression::String(s) => Span { start: 0, end: 0 },
            Expression::Number(num) => Span { start: 0, end: 0 },
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
