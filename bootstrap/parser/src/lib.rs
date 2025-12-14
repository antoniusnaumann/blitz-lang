mod lexer;
mod parser;

use std::{
    cmp::{max, min},
    ops::RangeInclusive,
};

pub use lexer::*;
pub use parser::*;

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
}

impl Definition {
    pub fn span(&self) -> Span {
        match self {
            Definition::Fn(f) => todo!(),
            Definition::Struct(s) => s.span.clone(),
            Definition::Union(union) => todo!(),
            Definition::Alias(alias) => todo!(),
            Definition::Actor(actor) => todo!(),
            Definition::Test(test) => todo!(),
        }
    }
}
