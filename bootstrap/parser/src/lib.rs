mod lexer;
mod parser;
mod print;

use std::{
    cmp::{max, min},
    ops::RangeInclusive,
};

pub use lexer::*;
pub use parser::*;
pub use print::*;

type_macro::anti_types!();

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
            Definition::Fn(f) => todo!(),
            Definition::Struct(s) => s.span.clone(),
            Definition::Union(union) => union.span.clone(),
            Definition::Alias(alias) => todo!(),
            Definition::Actor(actor) => todo!(),
            Definition::Test(test) => todo!(),
        }
    }
}
