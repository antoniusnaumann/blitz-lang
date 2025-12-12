use std::{iter::Peekable, str::CharIndices};

use crate::{Span, Token, TokenKind};

type Source<'a> = Peekable<CharIndices<'a>>;

struct Lexer<'a> {
    source: Source<'a>,
    origin: &'a str,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source: source.char_indices().peekable(),
            origin: source,
        }
    }

    fn next(&mut self) -> Token {
        let (start, ch) = self.peek_char();
        let mut end = start;
        use TokenKind::*;
        let kind = match ch {
            'A'..='Z' => {
                end = self.skip_ident();
                Type
            }
            'a'..='z' => {
                end = self.skip_ident();
                match &self.origin[start..=end] {
                    "struct" => Struct,
                    "actor" => Actor,
                    "union" => Union,
                    "alias" => Alias,
                    "fn" => Fn,
                    "pub" => Pub,
                    "let" => Let,
                    "mut" => Mut,
                    "for" => For,
                    "while" => While,
                    "if" => If,
                    "else" => Else,
                    "switch" => Switch,
                    "test" => Test,
                    _ => Var,
                }
            }
            '+' => self.up(Add),
            '-' => self.up(Sub),
            '*' => self.up(Mul),
            '/' => self.up(Div),
            '=' => {
                _ = self.source.next();
                let (pos, ch) = self.peek_char();
                end = pos;
                match ch {
                    '=' => self.up(Eq),
                    _ => Assign,
                }
            }
            '>' => {
                _ = self.source.next();
                let (pos, ch) = self.peek_char();
                end = pos;
                match ch {
                    '=' => self.up(Ge),
                    _ => Gt,
                }
            }
            '<' => {
                _ = self.source.next();
                let (pos, ch) = self.peek_char();
                end = pos;
                match ch {
                    '=' => self.up(Le),
                    _ => Lt,
                }
            }
            '!' => {
                _ = self.source.next();
                let (pos, ch) = self.peek_char();
                end = pos;
                match ch {
                    '=' => self.up(Ne),
                    _ => Not,
                }
            }
            '{' => self.up(Lbrace),
            '}' => self.up(Rbrace),
            '[' => self.up(Lbracket),
            ']' => self.up(Rbracket),
            '(' => self.up(Lparen),
            ')' => self.up(Rparen),
            ',' => self.up(Comma),
            '.' => self.up(Dot),
            ';' => self.up(Semicolon),
            '\n' => self.up(Newline),
            '\0' => Eof,
            _ => self.up(Error),
        };

        let span = Span { start, end };

        Token { kind, span }
    }

    fn skip_ident(&mut self) -> usize {
        let mut end = 0;
        loop {
            let (curr, ch) = self.peek_char();
            match ch {
                'A'..='Z' | 'a'..='z' => {
                    end = curr;
                    _ = self.next();
                }
                _ => return end,
            }
        }
    }

    fn peek_char(&mut self) -> (usize, char) {
        match self.source.peek() {
            Some((p, c)) => (*p, *c),
            None => (0, '\0'),
        }
    }

    /// Returns the argument and consumes the current character
    fn up(&mut self, t: TokenKind) -> TokenKind {
        _ = self.source.next();
        t
    }
}
