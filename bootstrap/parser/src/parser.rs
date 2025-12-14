use std::{iter::Peekable, ops::RangeInclusive};

use crate::{
    Case, Definition, Field, Fn, Lexer, PeekKind, Span, Statement, Struct, Token, TokenKind, Type,
    Union,
};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    source: &'a str,
    span: Span,
}

impl<'a> Parser<'a> {
    pub fn from(lexer: Lexer<'a>) -> Self {
        Self {
            source: lexer.source,
            lexer: lexer.peekable(),
            span: Span { start: 0, end: 0 },
        }
    }

    pub fn new(source: &'a str) -> Self {
        Self {
            source: source,
            lexer: crate::Lexer::new(source).peekable(),
            span: Span { start: 0, end: 0 },
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Token {
        if kind != TokenKind::Newline {
            self.skip_newlines();
        }
        let next = self.lexer.next().unwrap();
        let got = next.kind.clone();
        let pos = next.span.to_pos(self.source);
        assert_eq!(
            got, kind,
            "\nAt {}:{}  (left: got, right: expected)",
            pos.line, pos.col
        );
        self.span = next.span.clone();

        next
    }

    fn has(&mut self, kind: TokenKind) -> bool {
        if kind != TokenKind::Newline {
            self.skip_newlines();
        }
        self.lexer.kind() == kind
    }

    // fn one_of(&mut self, kinds: &[TokenKind]) -> Token {
    //     if kinds.contains(&TokenKind::Newline) {
    //         self.skip_newlines();
    //     }
    //     let next = self.lexer.next().unwrap();
    //     let pos = next.span.to_pos(self.source);
    //     let got = next.kind.clone();
    //     assert!(
    //         kinds.contains(&got),
    //         "\nAt {}:{}, expected one of {:#?}",
    //         pos.line,
    //         pos.col,
    //         kinds
    //     );
    //     self.span = next.span.clone();

    //     next
    // }

    fn consume(&mut self, kind: TokenKind) -> Option<Token> {
        if kind != TokenKind::Newline {
            self.skip_newlines();
        }
        let next = self.lexer.peek();
        if next.as_ref().map(|t| t.kind.clone()) == Some(kind.clone()) {
            let result = self.expect(kind);
            self.span = result.span.clone();

            Some(result)
        } else {
            None
        }
    }

    fn consume_ident(&mut self) -> Option<&'a str> {
        let Token { kind: _, span } = self.consume(TokenKind::Ident)?;
        Some(&self.source[RangeInclusive::from(span)])
    }

    fn consume_type(&mut self) -> Option<Type> {
        let Token { kind: _, span } = self.consume(TokenKind::Type)?;
        let name = String::from(&self.source[RangeInclusive::from(span)]);
        let args = self.parse_type_args();

        Some(Type { name, args })
    }

    fn skip_newlines(&mut self) {
        while self
            .lexer
            .next_if(|token| token.kind == TokenKind::Newline || token.kind == TokenKind::Comment)
            .is_some()
        {}
    }

    fn expect_ident(&mut self) -> &'a str {
        let Token { kind: _, span } = self.expect(TokenKind::Ident);
        &self.source[RangeInclusive::from(span)]
    }

    fn expect_type(&mut self) -> Type {
        let Token { kind: _, span } = self.expect(TokenKind::Type);
        let name = String::from(&self.source[RangeInclusive::from(span)]);
        let args = self.parse_type_args();

        Type { name, args }
    }

    fn parse_type_args(&mut self) -> Vec<Type> {
        let mut args = Vec::new();
        if self.has(TokenKind::Lparen) {
            self.expect(TokenKind::Lparen);

            while !self.has(TokenKind::Rparen) {
                args.push(self.expect_type());
                self.consume(TokenKind::Comma);
            }
            self.expect(TokenKind::Rparen);
        }
        args
    }

    fn parse_struct(&mut self) -> Struct {
        let start = self.expect(TokenKind::Struct).span;
        // TODO: handle type parameters in signature
        let name = self.expect_type().name;

        if !self.has(TokenKind::Lbrace) {
            return Struct {
                name,
                fields: Vec::new(),
                span: start.merge(&self.span),
            };
        }

        self.expect(TokenKind::Lbrace);
        let mut fields = Vec::new();
        self.skip_newlines();
        while !self.has(TokenKind::Rbrace) {
            fields.push(self.parse_field());
        }
        self.expect(TokenKind::Rbrace);

        Struct {
            name: name.into(),
            fields,
            span: start.merge(&self.span),
        }
    }

    fn parse_field(&mut self) -> Field {
        let name = self.expect_ident().into();
        let ty = self.expect_type();
        self.consume(TokenKind::Comma);

        Field { name, r#type: ty }
    }

    fn parse_union(&mut self) -> Union {
        let start = self.expect(TokenKind::Union).span;
        // TODO: handle type parameters in signature
        let name = self.expect_type().name;

        if !self.has(TokenKind::Lbrace) {
            return Union {
                name,
                cases: Vec::new(),
                span: start.merge(&self.span),
            };
        }

        self.expect(TokenKind::Lbrace);
        let mut cases = Vec::new();
        while !self.has(TokenKind::Rbrace) {
            cases.push(self.parse_case());
        }
        self.expect(TokenKind::Rbrace);

        Union {
            name,
            cases,
            span: start.merge(&self.span),
        }
    }

    fn parse_case(&mut self) -> Case {
        let label = self.consume_ident().map(|s| s.into());
        let ty = match self.lexer.kind() {
            _ if label.is_none() => Some(self.expect_type().into()),
            TokenKind::Colon => {
                self.expect(TokenKind::Colon);
                Some(self.expect_type().into())
            }
            _ => None,
        };

        Case { label, r#type: ty }
    }

    fn parse_fn(&mut self) -> Fn {
        let start = self.expect(TokenKind::Fn).span;
        let name = self.expect_ident().into();

        self.expect(TokenKind::Lparen);
        // TODO: also collect type params
        let mut args = Vec::new();
        self.skip_newlines();
        while !self.has(TokenKind::Rparen) {
            args.push(self.parse_field());
        }
        self.expect(TokenKind::Rparen);

        let ty = self.consume_type();

        let body = if self.has(TokenKind::Lbrace) {
            self.parse_body()
        } else {
            Vec::new()
        };

        Fn {
            name,
            args,
            body,
            r#type: ty,
            span: start.merge(&self.span),
        }
    }

    fn parse_body(&mut self) -> Vec<Statement> {
        self.expect(TokenKind::Lbrace);

        let mut statements = Vec::new();
        while !self.has(TokenKind::Rbrace) {
            statements.push(self.parse_statement());
        }
        self.expect(TokenKind::Rbrace);

        statements
    }

    fn parse_statement(&mut self) -> Statement {
        self.skip_newlines();
        match self.lexer.kind() {
            TokenKind::Let | TokenKind::Mut => self.parse_assignment(),
            _ => self.parse_expression().into(),
        }
    }

    fn parse_expression(&mut self) -> _ {
        todo!()
    }

    fn parse_assignment(&mut self) -> Statement {
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
                self.span.start, kind
            ),
        }
    }
}
