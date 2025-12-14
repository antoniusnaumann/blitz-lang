use std::{iter::Peekable, ops::RangeInclusive};

use crate::{Case, Definition, Field, Fn, Lexer, PeekKind, Span, Struct, Token, TokenKind, Union};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    source: &'a str,
    pos: Span,
}

impl<'a> Parser<'a> {
    pub fn from(lexer: Lexer<'a>) -> Self {
        Self {
            source: lexer.source,
            lexer: lexer.peekable(),
            pos: Span { start: 0, end: 0 },
        }
    }

    pub fn new(source: &'a str) -> Self {
        Self {
            source: source,
            lexer: crate::Lexer::new(source).peekable(),
            pos: Span { start: 0, end: 0 },
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Token {
        if kind != TokenKind::Newline {
            self.skip_newlines();
        }
        let next = self.lexer.next();
        let got = next.as_ref().map(|t| t.kind.clone());
        assert_eq!(got, Some(kind), "\nAt {}:", next.unwrap().span.start,);
        let result = next.unwrap();
        self.pos = result.span.clone();

        result
    }

    fn one_of(&mut self, kinds: &[TokenKind]) -> Token {
        if kinds.contains(&TokenKind::Newline) {
            self.skip_newlines();
        }
        let next = self.lexer.next();
        let got = next.as_ref().map(|t| t.kind.clone());
        assert!(
            kinds.contains(&got.unwrap()),
            "\nAt {}:",
            next.unwrap().span.start,
        );
        let result = next.unwrap();
        self.pos = result.span.clone();

        result
    }

    fn consume(&mut self, kind: TokenKind) -> Option<Token> {
        if kind != TokenKind::Newline {
            self.skip_newlines();
        }
        let next = self.lexer.next();
        if next.as_ref().map(|t| t.kind.clone()) == Some(kind) {
            let result = next.unwrap();
            self.pos = result.span.clone();

            Some(result)
        } else {
            None
        }
    }

    fn consume_ident(&mut self) -> Option<&'a str> {
        let Token { kind: _, span } = self.consume(TokenKind::Ident)?;
        Some(&self.source[RangeInclusive::from(span)])
    }

    fn skip_newlines(&mut self) {
        while self
            .lexer
            .next_if(|token| token.kind == TokenKind::Newline || token.kind == TokenKind::Comment)
            .is_some()
        {}
    }

    fn expect_ident(&mut self) -> &'a str {
        let Token { kind: _, span } = self.one_of(&[
            TokenKind::Ident,
            TokenKind::Union,
            TokenKind::Struct,
            TokenKind::Alias,
            TokenKind::Actor,
            TokenKind::Fn,
        ]);
        &self.source[RangeInclusive::from(span)]
    }

    fn expect_type(&mut self) -> &'a str {
        let Token { kind: _, span } = self.expect(TokenKind::Type);
        &self.source[RangeInclusive::from(span)]
    }

    fn parse_struct(&mut self) -> Struct {
        let start = self.expect(TokenKind::Struct).span;
        let name = self.expect_type();

        if self.lexer.kind() != TokenKind::Lbrace {
            return Struct {
                name: name.into(),
                fields: Vec::new(),
                span: start.merge(&self.pos),
            };
        }

        self.expect(TokenKind::Lbrace);
        let mut fields = Vec::new();
        while self.lexer.kind() != TokenKind::Rbrace {
            self.skip_newlines();
            fields.push(self.parse_field());
            self.skip_newlines();
        }
        self.expect(TokenKind::Rbrace);

        Struct {
            name: name.into(),
            fields,
            span: start.merge(&self.pos),
        }
    }

    fn parse_field(&mut self) -> Field {
        let name = self.expect_ident().into();
        let ty = self.expect_type().into();

        Field { name, r#type: ty }
    }

    fn parse_union(&mut self) -> Union {
        let start = self.expect(TokenKind::Union).span;
        let name = self.expect_type().into();

        if self.lexer.kind() != TokenKind::Lbrace {
            return Union {
                name,
                cases: Vec::new(),
                span: start.merge(&self.pos),
            };
        }

        self.expect(TokenKind::Lbrace);
        let mut cases = Vec::new();
        while self.lexer.kind() != TokenKind::Rbrace {
            self.skip_newlines();
            cases.push(self.parse_case());
            self.skip_newlines();
        }
        self.expect(TokenKind::Rbrace);

        Union {
            name,
            cases,
            span: start.merge(&self.pos),
        }
    }

    fn parse_case(&mut self) -> Case {
        let label = self.consume_ident().map(|s| s.into());
        let ty = match self.lexer.kind() {
            _ if label.is_none() => Some(self.expect_type().into()),
            TokenKind::Colon => Some(self.expect_type().into()),
            _ => None,
        };

        Case { label, r#type: ty }
    }

    fn parse_fn(&mut self) -> Fn {
        todo!()
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Definition;

    fn next(&mut self) -> Option<Self::Item> {
        use TokenKind as Tk;
        self.skip_newlines();
        let Some(Token { kind, span: _ }) = self.lexer.peek() else {
            return None;
        };
        match kind {
            Tk::Struct => Some(self.parse_struct().into()),
            Tk::Union => Some(self.parse_union().into()),
            Tk::Fn => Some(self.parse_fn().into()),
            Tk::Actor => todo!("actor"),
            Tk::Test => todo!("test"),
            Tk::Alias => todo!("alias"),
            Tk::Pub => todo!("pub"),
            Tk::Eof => None,
            kind => panic!(
                "At {}:\nIllegal token at toplevel: {:#?}",
                self.pos.start, kind
            ),
        }
    }
}
