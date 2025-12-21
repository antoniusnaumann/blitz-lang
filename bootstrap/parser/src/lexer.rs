use std::{iter::Peekable, ops::RangeInclusive, str::CharIndices};

use crate::{Span, Token, TokenKind};

pub type CharIter<'a> = Peekable<CharIndices<'a>>;

pub struct Lexer<'a> {
    chars: CharIter<'a>,
    pos: Span,
    pub(crate) source: &'a str,
}

pub(crate) trait PeekKind {
    fn kind(&mut self) -> TokenKind;
}

impl PeekKind for Peekable<Lexer<'_>> {
    fn kind(&mut self) -> TokenKind {
        self.peek()
            .map(|t| t.kind.clone())
            .unwrap_or(TokenKind::Eof)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token {
                span: _,
                kind: TokenKind::Eof,
            } => None,
            t => Some(t),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.char_indices().peekable(),
            source,
            pos: Span { start: 0, end: 0 },
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let (start, ch) = self.peek_char();
        let mut end = start;
        use TokenKind::*;
        let kind = match ch {
            'A'..='Z' => {
                end = self.skip_ident();
                Type
            }
            'a'..='z' | '_' => {
                end = self.skip_ident();
                match &self.source[start..=end] {
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
                    "and" => And,
                    "or" => Or,
                    "break" => Break,
                    "continue" => Continue,
                    "return" => Return,
                    _ => Ident,
                }
            }
            '"' => {
                end = self.skip_string();
                Str
            }
            '0'..='9' => {
                end = self.skip_num();
                Num
            }
            '+' => self.up(Add),
            '-' => self.up(Sub),
            '*' => self.up(Mul),
            '/' => {
                _ = self.chars.next();
                let (pos, ch) = self.peek_char();
                end = pos;
                match ch {
                    '/' => {
                        self.consume_comment();
                        let (pos, _) = self.peek_char();
                        end = pos;
                        Comment
                    }
                    _ => Div,
                }
            }
            '%' => self.up(Rem),
            '=' => {
                _ = self.chars.next();
                let (pos, ch) = self.peek_char();
                end = pos;
                match ch {
                    '=' => self.up(Eq),
                    _ => Assign,
                }
            }
            '>' => {
                _ = self.chars.next();
                let (pos, ch) = self.peek_char();
                end = pos;
                match ch {
                    '=' => self.up(Ge),
                    _ => Gt,
                }
            }
            '<' => {
                _ = self.chars.next();
                let (pos, ch) = self.peek_char();
                end = pos;
                match ch {
                    '=' => self.up(Le),
                    _ => Lt,
                }
            }
            '!' => {
                _ = self.chars.next();
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
            ':' => self.up(Colon),
            ';' => self.up(Semicolon),
            '|' => self.up(Pipe),
            '\n' => self.up(Newline),
            '\0' => self.up(Eof),

            ch => {
                eprintln!("Got illegal char {ch}");
                self.up(Error)
            }
        };

        let span = Span { start, end };
        self.pos = span.clone();

        Token { kind, span }
    }

    fn skip_ident(&mut self) -> usize {
        let mut end = 0;
        loop {
            let (curr, ch) = self.peek_char();
            match ch {
                'A'..='Z' | 'a'..='z' | '_' => {
                    end = curr;
                    _ = self.chars.next();
                }
                _ => return end,
            }
        }
    }

    fn skip_string(&mut self) -> usize {
        let (_, ch) = self.chars.next().unwrap();
        assert!(ch == '"', "Strings need to start with \"");
        // TODO: Handle format strings
        let mut escape = false;
        loop {
            let (curr, ch) = self.peek_char();
            match ch {
                '\\' if !escape => {
                    escape = true;
                    _ = self.chars.next();
                }
                '\\' if escape => {
                    escape = false;
                    _ = self.chars.next();
                }
                '"' if !escape => {
                    _ = self.chars.next();
                    return curr;
                }
                ch if escape => {
                    panic!("ERROR: Illegal escape sequence: \\{ch}")
                }
                _ => {
                    _ = self.chars.next();
                }
            }
        }
    }

    fn skip_num(&mut self) -> usize {
        let mut end = 0;
        let mut has_dot = false;

        loop {
            let (curr, ch) = self.peek_char();
            match ch {
                // TODO: lookahead to handle method calls, maybe upgrade in lexer if num ends with dot
                '.' if !has_dot => {
                    has_dot = true;
                    end = curr;
                    _ = self.chars.next();
                }
                '0'..='9' | '_' => {
                    end = curr;
                    _ = self.chars.next();
                }
                _ => return end,
            }
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let (_, ch) = self.peek_char();
            if !ch.is_whitespace() || ch == '\n' || ch == '\0' {
                break;
            }
            _ = self.chars.next();
        }
    }

    fn consume_comment(&mut self) {
        loop {
            let (_, ch) = self.peek_char();
            if ch == '\n' || ch == '\0' {
                break;
            }
            _ = self.chars.next();
        }
    }

    fn peek_char(&mut self) -> (usize, char) {
        match self.chars.peek() {
            Some((p, c)) => (*p, *c),
            None => (0, '\0'),
        }
    }

    /// Returns the argument and consumes the current character
    fn up(&mut self, t: TokenKind) -> TokenKind {
        _ = self.chars.next();
        t
    }
}
