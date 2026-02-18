use std::{
    iter::Peekable,
    ops::{Not, RangeInclusive},
};

use crate::*;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    source: &'a str,
    span: Span,
    #[allow(dead_code)]
    has_pub: bool,
    /// A token that was consumed but needs to be "put back" for the next read.
    pending_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn from(lexer: Lexer<'a>) -> Self {
        Self {
            source: lexer.source,
            lexer: lexer.peekable(),
            span: Span { start: 0, end: 0 },
            has_pub: false,
            pending_token: None,
        }
    }

    pub fn new(source: &'a str) -> Self {
        Self {
            source: source,
            lexer: crate::Lexer::new(source).peekable(),
            span: Span { start: 0, end: 0 },
            has_pub: false,
            pending_token: None,
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Token {
        if kind != TokenKind::Newline {
            self.skip_newlines();
        }
        let next = if let Some(pending) = self.pending_token.take() {
            pending
        } else {
            self.lexer.next().unwrap_or_else(|| {
                panic!(
                    "Expected {:#?}, but got nothing.\n\n{}",
                    kind,
                    self.report_span(self.span.clone())
                )
            })
        };
        let got = next.kind.clone();
        assert_eq!(
            got,
            kind,
            "\n\n{}\n\n(left: got, right: expected)",
            self.report_span(next.span)
        );
        self.span = next.span.clone();

        next
    }

    fn has(&mut self, kind: TokenKind) -> bool {
        if kind != TokenKind::Newline {
            self.skip_newlines();
        }
        if let Some(pending) = &self.pending_token {
            return pending.kind == kind;
        }
        self.lexer.kind() == kind
    }

    fn one_of(&mut self, kinds: &[TokenKind]) -> Token {
        if kinds.contains(&TokenKind::Newline) {
            self.skip_newlines();
        }
        let next = if let Some(pending) = self.pending_token.take() {
            pending
        } else {
            self.lexer.next().unwrap()
        };
        let got = next.kind.clone();
        assert!(
            kinds.contains(&got),
            "Expected one of {:#?}\n\n{}",
            kinds,
            self.report_span(next.span)
        );
        self.span = next.span.clone();

        next
    }

    fn consume(&mut self, kind: TokenKind) -> Option<Token> {
        if kind != TokenKind::Newline {
            self.skip_newlines();
        }
        let next_kind = if let Some(pending) = &self.pending_token {
            Some(pending.kind.clone())
        } else {
            self.lexer.peek().map(|t| t.kind.clone())
        };
        if next_kind == Some(kind.clone()) {
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
        // Check for qualified type: ident::Type
        let module = self.consume_module_prefix();

        let Token { kind: _, span } = self.consume(TokenKind::Type)?;
        let name = String::from(&self.source[RangeInclusive::from(span.clone())]);
        let params = self.parse_type_params();

        Some(Type {
            name,
            params,
            module,
            span,
        })
    }

    fn skip_newlines(&mut self) {
        // Don't skip newlines past a pending token
        if self.pending_token.is_some() {
            return;
        }
        while self
            .lexer
            .next_if(|token| token.kind == TokenKind::Newline || token.kind == TokenKind::Comment)
            .is_some()
        {}
    }

    /// Try to consume a module prefix of the form `ident::`.
    /// If the next tokens are `ident` followed by `::`, consume both and return Some(module_name).
    /// Otherwise return None without consuming anything (may leave a pending token).
    fn consume_module_prefix(&mut self) -> Option<String> {
        self.skip_newlines();

        // Check if next token is Ident
        let peeked_kind = if let Some(pending) = &self.pending_token {
            pending.kind.clone()
        } else {
            self.lexer.peek().map(|t| t.kind.clone())?
        };

        if peeked_kind != TokenKind::Ident {
            return None;
        }

        // Consume the ident token
        let ident_token = if let Some(pending) = self.pending_token.take() {
            pending
        } else {
            self.lexer.next()?
        };
        let ident_span = ident_token.span.clone();

        // Check if next token is Path (::)
        if self.lexer.peek().map(|t| t.kind.clone()) == Some(TokenKind::Path) {
            // Consume the :: token
            self.lexer.next();
            let module_name = self.source[RangeInclusive::from(ident_span)].to_string();
            Some(module_name)
        } else {
            // Not a qualified name - put the ident back as a pending token
            self.pending_token = Some(ident_token);
            None
        }
    }

    fn expect_ident(&mut self) -> Ident {
        let Token { kind: _, span } = self.expect(TokenKind::Ident);
        let name = self.source[RangeInclusive::from(span.clone())].into();

        Ident { name, span }
    }

    fn expect_type(&mut self) -> Type {
        // Check for qualified type: ident::Type
        let module = self.consume_module_prefix();

        let Token { kind: _, span } = self.expect(TokenKind::Type);
        let name = String::from(&self.source[RangeInclusive::from(span.clone())]);
        let params = self.parse_type_params();

        Type {
            name,
            params,
            module,
            span,
        }
    }

    fn parse_type_params(&mut self) -> Vec<Type> {
        let mut args = Vec::new();
        // TODO: Handle nested type args
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
        let sig = self.expect_type();

        if !self.has(TokenKind::Lbrace) {
            return Struct {
                sig,
                fields: Vec::new(),
                span: start.merge(&self.span),
            };
        }

        self.expect(TokenKind::Lbrace);
        let mut fields = Vec::new();
        self.skip_newlines();
        while !self.has(TokenKind::Rbrace) {
            fields.push(self.parse_field(false));
        }
        self.expect(TokenKind::Rbrace);

        Struct {
            sig,
            fields,
            span: start.merge(&self.span),
        }
    }

    fn parse_field(&mut self, mutable: bool) -> Field {
        let name = self.expect_ident().name.into();
        let ty = self.expect_type();
        self.consume(TokenKind::Comma);

        Field {
            name,
            r#type: ty,
            mutable,
        }
    }

    fn parse_union(&mut self) -> Union {
        let start = self.expect(TokenKind::Union).span;
        let sig = self.expect_type();

        if !self.has(TokenKind::Lbrace) {
            return Union {
                sig,
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
            sig,
            cases,
            span: start.merge(&self.span),
        }
    }

    fn parse_case(&mut self) -> Case {
        let label = match self.lexer.peek().unwrap().kind {
            TokenKind::False => {
                self.lexer.next();
                "false_".to_owned().into()
            }
            TokenKind::True => {
                self.lexer.next();
                "true_".to_owned().into()
            }
            _ => self.consume_ident().map(|s| s.into()),
        };
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
        let name = self.expect_ident().name.into();

        self.expect(TokenKind::Lparen);
        // TODO: also collect type params
        let mut args = Vec::new();
        self.skip_newlines();
        while !self.has(TokenKind::Rparen) {
            let is_mut = self.consume(TokenKind::Mut).is_some();
            args.push(self.parse_field(is_mut));
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

    fn parse_test(&mut self) -> Test {
        let start = self.expect(TokenKind::Test).span;
        let name = self.parse_string_lit();

        let body = self.parse_body();

        Test {
            name,
            body,
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
            TokenKind::Let | TokenKind::Var => self.parse_declaration().into(),
            _ => self.parse_expression().into(),
        }
    }

    fn token_to_infix_op(&self, kind: &TokenKind) -> Option<Operator> {
        match kind {
            TokenKind::Add => Some(Operator::Add),
            TokenKind::Sub => Some(Operator::Sub),
            TokenKind::Mul => Some(Operator::Mul),
            TokenKind::Div => Some(Operator::Div),
            TokenKind::Rem => Some(Operator::Rem),
            TokenKind::Concat => Some(Operator::Concat),
            TokenKind::Eq => Some(Operator::Eq),
            TokenKind::Ne => Some(Operator::Ne),
            TokenKind::Lt => Some(Operator::Lt),
            TokenKind::Le => Some(Operator::Le),
            TokenKind::Gt => Some(Operator::Gt),
            TokenKind::Ge => Some(Operator::Ge),
            TokenKind::And => Some(Operator::And),
            TokenKind::Or => Some(Operator::Or),
            TokenKind::Dot => Some(Operator::Member),
            TokenKind::Else => Some(Operator::Else),
            _ => None,
        }
    }

    fn token_to_prefix_op(&self, kind: &TokenKind) -> Option<Operator> {
        match kind {
            TokenKind::Not => Some(Operator::Not),
            TokenKind::Sub => Some(Operator::Neg),
            TokenKind::Try => Some(Operator::Try),
            _ => None,
        }
    }

    fn parse_precedence(&mut self, mut lhs: Expression, min_prec: u8) -> Expression {
        self.skip_newlines();

        loop {
            let peek = self.lexer.peek();
            if peek.is_none() {
                break;
            }

            let token_kind = peek.unwrap().kind.clone();

            // Special handling for index expressions (postfix [])
            if token_kind == TokenKind::Lbracket {
                let _bracket = self.lexer.next().unwrap();
                self.skip_newlines();
                let index = self.parse_expression_bp(0);
                let end_bracket = self.expect(TokenKind::Rbracket);
                let span = lhs.span().merge(&end_bracket.span);

                lhs = Index {
                    target: Box::new(lhs),
                    index: Box::new(index),
                    span,
                }
                .into();
                continue;
            }

            let op = match self.token_to_infix_op(&token_kind) {
                Some(op) => op,
                None => break,
            };

            let (left_bp, right_bp) = op.precedence();
            if left_bp < min_prec {
                break;
            }

            // Consume the operator token
            let _op_token = self.lexer.next().unwrap();
            self.skip_newlines();

            // Special handling for member access (needs identifier, not full expression)
            if matches!(op, Operator::Member) {
                // TODO: quick and dirty solution to parse and ignore the .mut modifier
                if self.consume(TokenKind::Mut).is_some() {
                    self.expect(TokenKind::Dot);
                }

                // Special handling for .try: desugar to "expr else return none"
                if self.consume(TokenKind::Try).is_some() {
                    let span = lhs.span().merge(&self.span);
                    let none_expr = Expression::Ident(Ident {
                        name: "none".into(),
                        span: span.clone(),
                    });
                    let return_none = Expression::Return(Box::new(none_expr));

                    lhs = BinaryOp {
                        op: Operator::Else,
                        left: Box::new(lhs),
                        right: Box::new(return_none),
                        span,
                    }
                    .into();
                    continue;
                }

                let member = self.expect_ident().name.into();
                let span = lhs.span().merge(&self.span);

                // Check if this is a method call
                if self.has(TokenKind::Lparen) {
                    lhs = self.parse_member_call(lhs, member).into();
                } else {
                    lhs = Member {
                        parent: Box::new(lhs),
                        member,
                        span,
                    }
                    .into();
                }
            } else {
                // Parse the right-hand side with the right binding power
                let rhs = self.parse_expression_bp(right_bp);
                let span = lhs.span().merge(&rhs.span());

                lhs = BinaryOp {
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    span,
                }
                .into();
            }
        }

        lhs
    }

    fn parse_expression_bp(&mut self, min_prec: u8) -> Expression {
        self.skip_newlines();
        let token = self.lexer.peek().unwrap().clone();

        // Handle prefix operators
        if let Some(op) = self.token_to_prefix_op(&token.kind) {
            let op_token = self.expect(token.kind);
            let (_, right_bp) = op.precedence();
            let expr = self.parse_expression_bp(right_bp);
            let span = op_token.span.merge(&expr.span());

            // Special handling for 'try': desugar to "expr else return none"
            let unary = if matches!(op, Operator::Try) {
                let none_expr = Expression::Ident(Ident {
                    name: "none".into(),
                    span: span.clone(),
                });
                let return_none = Expression::Return(Box::new(none_expr));

                BinaryOp {
                    op: Operator::Else,
                    left: Box::new(expr),
                    right: Box::new(return_none),
                    span,
                }
                .into()
            } else {
                UnaryOp {
                    op,
                    expr: Box::new(expr),
                    span,
                }
                .into()
            };

            return self.parse_precedence(unary, min_prec);
        }

        // Handle primary expressions (atoms)
        let lhs = match &token.kind {
            TokenKind::For => self.parse_for().into(),
            TokenKind::While => self.parse_while().into(),
            TokenKind::If => self.parse_if().into(),
            TokenKind::Switch => self.parse_switch().into(),
            TokenKind::Return => {
                self.lexer.next(); // Consume the return token
                if self.lexer.peek().is_some_and(|t| {
                    [
                        TokenKind::Var,
                        TokenKind::Let,
                        TokenKind::Semicolon,
                        TokenKind::Rbrace,
                        TokenKind::Rbracket,
                        TokenKind::Rparen,
                        TokenKind::Newline,
                    ]
                    .contains(&t.kind)
                    .not()
                }) {
                    Expression::Return(self.parse_expression_bp(0).into())
                } else {
                    Expression::Return(Expression::Block(Vec::new()).into())
                }
            }
            TokenKind::Continue => {
                self.lexer.next(); // Consume the continue token
                Expression::Continue
            }
            TokenKind::Break => {
                self.lexer.next(); // Consume the break token
                Expression::Break
            }
            TokenKind::Assert => {
                self.lexer.next(); // Consume the assert token
                let condition = self.parse_expression_bp(0);

                // Convert the condition to a string for the error message
                let condition_str = condition.print();

                // Check if this is a comparison binary operation - if so, we can show left/right values
                let is_comparison = matches!(
                    &condition,
                    Expression::BinaryOp(binop) if matches!(
                        binop.op,
                        Operator::Eq | Operator::Ne | Operator::Lt | Operator::Le | Operator::Gt | Operator::Ge
                    )
                );

                if is_comparison {
                    // Extract left and right from the comparison
                    if let Expression::BinaryOp(binop) = &condition {
                        let left_str = binop.left.print();
                        let right_str = binop.right.print();

                        // Create: if !(condition) { _assert_cmp_failed("condition", left, "left_str", right, "right_str") }
                        let negated_condition = Expression::UnaryOp(crate::UnaryOp {
                            op: Operator::Not,
                            expr: Box::new(condition.clone()),
                            span: self.span.clone(),
                        });

                        // Generate a call to _assert_cmp_failed with the values
                        let assert_call = Expression::Call(crate::Call {
                            name: "_assert_cmp_failed".into(),
                            module: None,
                            args: vec![
                                CallArg {
                                    label: None,
                                    init: Box::new(Expression::String(condition_str)),
                                },
                                CallArg {
                                    label: None,
                                    init: binop.left.clone(),
                                },
                                CallArg {
                                    label: None,
                                    init: Box::new(Expression::String(left_str)),
                                },
                                CallArg {
                                    label: None,
                                    init: binop.right.clone(),
                                },
                                CallArg {
                                    label: None,
                                    init: Box::new(Expression::String(right_str)),
                                },
                            ],
                            ufcs: false,
                            span: self.span.clone(),
                        });

                        return Expression::If(crate::If {
                            cond: Box::new(negated_condition),
                            body: vec![assert_call.into()],
                            span: self.span.clone(),
                        });
                    }
                }

                // For non-comparison assertions, use the simple panic message
                let negated_condition = Expression::UnaryOp(crate::UnaryOp {
                    op: Operator::Not,
                    expr: Box::new(condition),
                    span: self.span.clone(),
                });

                let panic_message = format!("Assertion '{}' failed", condition_str);
                let panic_call = Expression::Call(crate::Call {
                    name: "panic".into(),
                    module: None,
                    args: vec![CallArg {
                        label: None,
                        init: Box::new(Expression::String(panic_message)),
                    }],
                    ufcs: false,
                    span: self.span.clone(),
                });

                Expression::If(crate::If {
                    cond: Box::new(negated_condition),
                    body: vec![panic_call.into()],
                    span: self.span.clone(),
                })
            }
            TokenKind::Lbrace => self.parse_block().into(),
            TokenKind::Ident => {
                let span = token.span;
                let name = self.expect_ident().name;

                // Check if this is a qualified constructor: ident::Type(...)
                if self.has(TokenKind::Path) {
                    // This is a module-qualified name
                    self.expect(TokenKind::Path);
                    // After ::, we expect a Type token for a constructor
                    if self.has(TokenKind::Type) {
                        // Put the module name into pending state and parse as constructor
                        let module = Some(name.clone());
                        let type_token = self.expect(TokenKind::Type);
                        let type_name = String::from(
                            &self.source[RangeInclusive::from(type_token.span.clone())],
                        );

                        if self.has(TokenKind::Lparen) {
                            // module::Type(...)  - qualified constructor
                            let mut args = Vec::new();
                            let type_params = Vec::new();
                            self.expect(TokenKind::Lparen);
                            while !self.has(TokenKind::Rparen) {
                                let label = self.expect_ident();
                                self.expect(TokenKind::Colon);

                                let init =
                                    if self.has(TokenKind::Comma) || self.has(TokenKind::Rparen) {
                                        Box::new(Expression::Ident(Ident {
                                            name: label.name.clone(),
                                            span: label.span.clone(),
                                        }))
                                    } else {
                                        self.parse_expression().into()
                                    };

                                self.consume(TokenKind::Comma);
                                args.push(ConstructorArg { label, init });
                            }
                            self.expect(TokenKind::Rparen);

                            let r#type = Type {
                                name: type_name,
                                params: type_params,
                                module,
                                span: span.clone(),
                            };
                            Constructor {
                                r#type,
                                args,
                                span: span.merge(&self.span),
                            }
                            .into()
                        } else {
                            // module::Type without parens - treat as a type reference
                            // This shouldn't normally happen in expression context,
                            // but we'll return it as a constructor with no args for now
                            panic!(
                                "Expected '(' after qualified type name {}::{}\n\n{}",
                                name,
                                type_name,
                                self.report_span(span)
                            );
                        }
                    } else if self.has(TokenKind::Ident) {
                        // module::func(...) - qualified function call
                        let module = Some(name.clone());
                        let func_ident = self.expect_ident();
                        let func_name = func_ident.name;

                        if self.has(TokenKind::Lparen) {
                            let mut call = self.parse_call(&func_name);
                            call.module = module;
                            Expression::Call(call)
                        } else {
                            panic!(
                                "Expected '(' after qualified function name {}::{}\n\n{}",
                                name,
                                func_name,
                                self.report_span(span)
                            );
                        }
                    } else {
                        panic!(
                            "Expected type name after '{}::'\n\n{}",
                            name,
                            self.report_span(span)
                        );
                    }
                } else if self.has(TokenKind::Lparen) {
                    self.parse_call(&name).into()
                } else {
                    Ident {
                        name: name.into(),
                        span,
                    }
                    .into()
                }
            }
            TokenKind::Type => self.parse_constructor().into(),
            TokenKind::Lbracket => self.parse_list().into(),
            TokenKind::Lparen => self.parse_group().into(),
            TokenKind::Str => self.parse_string_lit().into(),
            TokenKind::Ch => self.parse_char_lit().into(),
            TokenKind::Num => self.parse_num_lit().into(),
            TokenKind::True => self.parse_bool_lit(true).into(),
            TokenKind::False => self.parse_bool_lit(false).into(),
            kind => {
                panic!(
                    "Illegal token kind {:#?} for expression!\n\n{}",
                    kind,
                    self.span.report(self.source)
                )
            }
        };

        let expr = self.parse_precedence(lhs, min_prec);
        if let Some(op) = self.consume_assignment() {
            let left = match expr {
                Expression::Ident(ident) => Lval::Ident(ident),
                Expression::Member(member) => Lval::Member(member),
                Expression::Index(index) => Lval::Index(index),
                e => panic!("Assignment on invalid lval {:#?}", e),
            };

            let right = self.parse_expression();

            // Desugar compound assignments: a += b becomes a = a + b
            let right = if let Some(binary_op) = op {
                // Reconstruct the left side as an expression
                let left_expr = match &left {
                    Lval::Ident(ident) => Expression::Ident(ident.clone()),
                    Lval::Member(member) => Expression::Member(member.clone()),
                    Lval::Index(index) => Expression::Index(index.clone()),
                };

                let span = left_expr.span().merge(&right.span());
                Expression::BinaryOp(BinaryOp {
                    op: binary_op,
                    left: Box::new(left_expr),
                    right: Box::new(right),
                    span,
                })
            } else {
                right
            };

            Assignment {
                left,
                right: Box::new(right),
            }
            .into()
        } else {
            expr
        }
    }

    // Returns Some(operator) for compound assignments (+=, -=, etc.) or None for simple assignment (=)
    fn consume_assignment(&mut self) -> Option<Option<Operator>> {
        if self.consume(TokenKind::Add_assign).is_some() {
            Some(Some(Operator::Add))
        } else if self.consume(TokenKind::Sub_assign).is_some() {
            Some(Some(Operator::Sub))
        } else if self.consume(TokenKind::Mul_assign).is_some() {
            Some(Some(Operator::Mul))
        } else if self.consume(TokenKind::Div_assign).is_some() {
            Some(Some(Operator::Div))
        } else if self.consume(TokenKind::Rem_assign).is_some() {
            Some(Some(Operator::Rem))
        } else if self.consume(TokenKind::Concat_assign).is_some() {
            Some(Some(Operator::Concat))
        } else if self.consume(TokenKind::Assign).is_some() {
            Some(None)
        } else {
            None
        }
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_expression_bp(0)
    }

    fn parse_member_call(&mut self, parent: Expression, method: String) -> Call {
        self.expect(TokenKind::Lparen);

        let mut args = vec![CallArg {
            label: None,
            init: Box::new(parent.clone()),
        }];
        while !self.has(TokenKind::Rparen) {
            let _is_mut = self.consume(TokenKind::Mut).is_some();
            args.push(CallArg {
                label: None,
                init: Box::new(self.parse_expression()),
            });
            self.consume(TokenKind::Comma);
        }
        let end_span = self.expect(TokenKind::Rparen).span;

        Call {
            name: method,
            module: None,
            args,
            ufcs: true,
            span: parent.span().merge(&end_span),
        }
    }

    fn parse_for(&mut self) -> For {
        let Token { kind: _, span } = self.expect(TokenKind::For);
        let iter = self.parse_expression().into();

        // TODO: implement implicit elements with $0
        self.expect(TokenKind::Pipe);
        let elem = self.expect_ident().name.into();
        self.expect(TokenKind::Pipe);

        let body = self.parse_body();

        For {
            iter,
            elem,
            body,
            span: span.merge(&self.span),
        }
    }

    fn parse_while(&mut self) -> While {
        let Token { kind: _, span } = self.expect(TokenKind::While);
        let cond = self.parse_expression().into();

        // TODO: while loops should also work with unpacking optionals I think, so check for pipes
        let body = self.parse_body();

        While {
            cond,
            body,
            span: span.merge(&self.span),
        }
    }

    fn parse_if(&mut self) -> If {
        let Token { kind: _, span } = self.expect(TokenKind::If);
        let cond = self.parse_expression().into();

        // TODO: while loops should also work with unpacking optionals I think, so check for pipes

        let body = self.parse_body();

        If {
            cond,
            body,
            span: span.merge(&self.span),
        }
    }

    fn parse_switch(&mut self) -> Switch {
        let Token { kind: _, span } = self.expect(TokenKind::Switch);
        let cond = self.parse_expression().into();
        self.expect(TokenKind::Lbrace);

        let mut cases = Vec::new();
        while !self.has(TokenKind::Rbrace) {
            let Token {
                kind,
                span: case_span,
            } = self.lexer.peek().unwrap().clone();

            let label = match kind {
                TokenKind::Ident => {
                    // Check if this is a qualified type: ident::Type
                    let ident_span = case_span.clone();
                    let ident_name: String = self.expect_ident().name;

                    if self.has(TokenKind::Path) {
                        // Qualified type in switch label: module::Type
                        self.expect(TokenKind::Path);
                        let ty = self.expect_type();
                        Type {
                            name: ty.name,
                            params: ty.params,
                            module: Some(ident_name),
                            span: ident_span.merge(&ty.span),
                        }
                        .into()
                    } else {
                        // Plain ident
                        Ident {
                            name: ident_name,
                            span: ident_span,
                        }
                        .into()
                    }
                }
                TokenKind::Type => self.expect_type().into(),
                TokenKind::Else => {
                    self.lexer.next(); // Consume 'else' token
                    Else {}.into()
                }
                TokenKind::Str => StringLit {
                    value: self.parse_string_lit(),
                }
                .into(),
                TokenKind::Ch => CharLit {
                    value: self.parse_char_lit(),
                }
                .into(),
                TokenKind::Num => NumberLit {
                    value: self.parse_num_lit().into(),
                }
                .into(),
                _ => panic!(
                    "Unexpected token in switch case: {:#?}\n\n{}",
                    kind,
                    case_span.report(self.source)
                ),
            };

            let body = self.parse_body();

            cases.push(SwitchCase {
                label,
                body,
                span: case_span.merge(&self.span),
            });
        }
        self.expect(TokenKind::Rbrace);

        Switch {
            cond,
            cases,
            span: span.merge(&self.span),
        }
    }

    fn parse_list(&mut self) -> List {
        let Token { kind: _, span } = self.expect(TokenKind::Lbracket);

        let mut elems = Vec::new();
        while !self.has(TokenKind::Rbracket) {
            elems.push(self.parse_expression());
            self.consume(TokenKind::Comma);
        }
        self.expect(TokenKind::Rbracket);

        List {
            elems,
            span: span.merge(&self.span),
        }
    }

    fn parse_group(&mut self) -> Group {
        let Token { kind: _, span } = self.expect(TokenKind::Lparen);
        let expr = self.parse_expression().into();
        self.expect(TokenKind::Rparen);

        Group {
            expr,
            span: span.merge(&self.span),
        }
    }

    fn parse_string_lit(&mut self) -> String {
        let Token { kind: _, span } = self.expect(TokenKind::Str);

        self.source[(span.start + 1)..(span.end)].into()
    }

    fn parse_char_lit(&mut self) -> Rune {
        let Token { kind: _, span } = self.expect(TokenKind::Ch);

        let chars: Vec<_> = self.source[(span.start + 1)..(span.end)].chars().collect();
        assert!(!chars.is_empty(), "char cannot be empty");

        if chars.len() == 1 {
            chars[0]
        } else if chars.len() == 2 && chars[0] == '\\' {
            match chars[1] {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '0' => '\0',
                '\\' => '\\',
                '\'' => '\'',
                ch => panic!("Invalid escape sequence \\{ch}"),
            }
        } else {
            panic!(
                "invalid char {}",
                &self.source[(span.start + 1)..(span.end)]
            )
        }
    }

    fn parse_num_lit(&mut self) -> Float {
        let Token { kind: _, span } = self.expect(TokenKind::Num);

        self.source[RangeInclusive::from(span)].parse().unwrap()
    }

    fn parse_bool_lit(&mut self, value: bool) -> BoolLit {
        let Token { kind: _, span } = if value {
            self.expect(TokenKind::True)
        } else {
            self.expect(TokenKind::False)
        };

        BoolLit { value, span }
    }

    fn parse_block(&mut self) -> Expression {
        Expression::Block(self.parse_body())
    }

    fn parse_call(&mut self, ident: &str) -> Call {
        let Token { kind: _, span } = self.expect(TokenKind::Lparen);

        let mut args = Vec::new();
        while !self.has(TokenKind::Rparen) {
            let _is_mut = self.consume(TokenKind::Mut).is_some();

            // Parse the argument expression
            let expr = self.parse_expression();

            // Check if this is a named argument (expression must be just an Ident followed by colon)
            if let Expression::Ident(ident_expr) = &expr {
                if self.consume(TokenKind::Colon).is_some() {
                    // This is a named argument or shorthand
                    let label = ident_expr.clone();

                    // Check for shorthand syntax (comma or rparen after colon)
                    if self.has(TokenKind::Comma) || self.has(TokenKind::Rparen) {
                        // Shorthand: foo(bar:) means foo(bar: bar)
                        let init = Box::new(Expression::Ident(ident_expr.clone()));
                        args.push(CallArg {
                            label: Some(label),
                            init,
                        });
                    } else {
                        // Full named argument: foo(bar: expr)
                        let init = Box::new(self.parse_expression());
                        args.push(CallArg {
                            label: Some(label),
                            init,
                        });
                    }
                } else {
                    // Not followed by colon, treat as positional
                    args.push(CallArg {
                        label: None,
                        init: Box::new(expr),
                    });
                }
            } else {
                // Not an Ident, must be positional
                args.push(CallArg {
                    label: None,
                    init: Box::new(expr),
                });
            }

            self.consume(TokenKind::Comma);
        }
        self.expect(TokenKind::Rparen);

        Call {
            name: ident.into(),
            module: None,
            args,
            ufcs: false,
            span: span.merge(&self.span),
        }
    }

    fn parse_constructor(&mut self) -> Constructor {
        // Module prefix was already consumed and stored in module_prefix if present
        let module = self.consume_module_prefix();
        let Token { kind: _, span } = self.expect(TokenKind::Type);
        let name = String::from(&self.source[RangeInclusive::from(span.clone())]);

        let mut args = Vec::new();
        let type_params = Vec::new();
        self.expect(TokenKind::Lparen);
        // TODO: parse type args (args before semicolon)
        while !self.has(TokenKind::Rparen) {
            let label = self.expect_ident();
            self.expect(TokenKind::Colon);

            // Check for shorthand syntax (no expression after colon)
            let init = if self.has(TokenKind::Comma) || self.has(TokenKind::Rparen) {
                // Shorthand: Type(field:) means Type(field: field)
                Box::new(Expression::Ident(Ident {
                    name: label.name.clone(),
                    span: label.span.clone(),
                }))
            } else {
                // Full syntax: Type(field: expr)
                self.parse_expression().into()
            };

            self.consume(TokenKind::Comma);
            args.push(ConstructorArg { label, init })
        }
        self.expect(TokenKind::Rparen);

        let r#type = Type {
            name,
            params: type_params,
            module,
            span: span.clone(),
        };
        Constructor {
            r#type,
            args,
            span: span.merge(&self.span),
        }
    }

    fn parse_declaration(&mut self) -> Declaration {
        let Token { kind, span } = self.one_of(&[TokenKind::Let, TokenKind::Var]);
        let name = self.expect_ident().name.into();

        // TODO: type inference
        let r#type = if let Some(_token) = self.consume(TokenKind::Colon) {
            self.expect_type()
        } else {
            Type {
                name: "_".into(),
                params: Vec::new(),
                module: None,
                span: span.clone(),
            }
        };

        let init = if let Some(_) = self.consume(TokenKind::Assign) {
            Some(self.parse_expression())
        } else {
            None
        };

        Declaration {
            name,
            r#type,
            is_mut: kind == TokenKind::Var,
            span: span.merge(&self.span),
            init,
        }
    }

    fn report_span(&self, span: Span) -> String {
        span.report(&self.source)
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Definition;

    fn next(&mut self) -> Option<Self::Item> {
        use TokenKind as Tk;
        self.skip_newlines();
        let is_pub = self.consume(TokenKind::Pub).is_some();
        let Some(Token { kind, span: _ }) = self.lexer.peek() else {
            return None;
        };

        let item: Option<Definition> = match kind {
            Tk::Struct => Some(self.parse_struct().into()),
            Tk::Union => Some(self.parse_union().into()),
            Tk::Fn => Some(self.parse_fn().into()),
            Tk::Test => Some(self.parse_test().into()),
            Tk::Actor => todo!("actor"),
            Tk::Alias => todo!("alias"),
            Tk::Eof => None,
            kind => panic!(
                "At {}:\nIllegal token at toplevel: {:#?}",
                self.span.start, kind
            ),
        };

        if is_pub {
            Some(Definition::Pub(Pub {
                item: item.unwrap().into(),
            }))
        } else {
            item
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_expr(source: &str) -> Expression {
        let mut parser = Parser::new(source);
        parser.parse_expression()
    }

    #[test]
    fn test_simple_binary_op() {
        let expr = parse_expr("a + b");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Add));
                assert!(matches!(*op.left, Expression::Ident(_)));
                assert!(matches!(*op.right, Expression::Ident(_)));
            }
            _ => panic!("Expected BinaryOp, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_precedence_mul_over_add() {
        // a + b * c should parse as a + (b * c)
        let expr = parse_expr("a + b * c");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Add));
                assert!(matches!(*op.left, Expression::Ident(_)));

                // Right side should be b * c
                match *op.right {
                    Expression::BinaryOp(right_op) => {
                        assert!(matches!(right_op.op, Operator::Mul));
                    }
                    _ => panic!("Expected right side to be multiplication"),
                }
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_left_associativity() {
        // a + b + c should parse as (a + b) + c
        let expr = parse_expr("a + b + c");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Add));

                // Left side should be a + b
                match *op.left {
                    Expression::BinaryOp(left_op) => {
                        assert!(matches!(left_op.op, Operator::Add));
                    }
                    _ => panic!("Expected left side to be addition"),
                }

                assert!(matches!(*op.right, Expression::Ident(_)));
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_comparison_precedence() {
        // a == b or c != d should parse as (a == b) or (c != d)
        let expr = parse_expr("a == b or c != d");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Or));

                // Both sides should be comparisons
                match *op.left {
                    Expression::BinaryOp(left_op) => {
                        assert!(matches!(left_op.op, Operator::Eq));
                    }
                    _ => panic!("Expected left side to be equality comparison"),
                }

                match *op.right {
                    Expression::BinaryOp(right_op) => {
                        assert!(matches!(right_op.op, Operator::Ne));
                    }
                    _ => panic!("Expected right side to be inequality comparison"),
                }
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_and_over_or() {
        // a or b and c should parse as a or (b and c)
        let expr = parse_expr("a or b and c");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Or));
                assert!(matches!(*op.left, Expression::Ident(_)));

                match *op.right {
                    Expression::BinaryOp(right_op) => {
                        assert!(matches!(right_op.op, Operator::And));
                    }
                    _ => panic!("Expected right side to be 'and'"),
                }
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_unary_prefix() {
        let expr = parse_expr("!a");

        match expr {
            Expression::UnaryOp(op) => {
                assert!(matches!(op.op, Operator::Not));
                assert!(matches!(*op.expr, Expression::Ident(_)));
            }
            _ => panic!("Expected UnaryOp"),
        }
    }

    #[test]
    fn test_unary_negation() {
        let expr = parse_expr("-a");

        match expr {
            Expression::UnaryOp(op) => {
                assert!(matches!(op.op, Operator::Neg));
                assert!(matches!(*op.expr, Expression::Ident(_)));
            }
            _ => panic!("Expected UnaryOp"),
        }
    }

    #[test]
    fn test_unary_with_binary() {
        // -a + b should parse as (-a) + b
        let expr = parse_expr("-a + b");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Add));

                match *op.left {
                    Expression::UnaryOp(unary) => {
                        assert!(matches!(unary.op, Operator::Neg));
                    }
                    _ => panic!("Expected left side to be unary negation"),
                }

                assert!(matches!(*op.right, Expression::Ident(_)));
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_parentheses_override_precedence() {
        // (a + b) * c should parse with addition inside
        let expr = parse_expr("(a + b) * c");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Mul));

                // Left side should be a group containing addition
                match *op.left {
                    Expression::Group(group) => match *group.expr {
                        Expression::BinaryOp(inner_op) => {
                            assert!(matches!(inner_op.op, Operator::Add));
                        }
                        _ => panic!("Expected addition inside group"),
                    },
                    _ => panic!("Expected left side to be a group"),
                }

                assert!(matches!(*op.right, Expression::Ident(_)));
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_member_access() {
        let expr = parse_expr("obj.field");

        match expr {
            Expression::Member(m) => {
                assert!(matches!(*m.parent, Expression::Ident(_)));
                assert_eq!(m.member, "field");
            }
            _ => panic!("Expected Member, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_chained_member_access() {
        // a.b.c should parse as (a.b).c
        let expr = parse_expr("a.b.c");

        match expr {
            Expression::Member(m) => {
                assert_eq!(m.member, "c");

                match *m.parent {
                    Expression::Member(inner) => {
                        assert_eq!(inner.member, "b");
                        assert!(matches!(*inner.parent, Expression::Ident(_)));
                    }
                    _ => panic!("Expected parent to be Member"),
                }
            }
            _ => panic!("Expected Member"),
        }
    }

    #[test]
    fn test_method_call_ufcs() {
        // obj.method() should parse as a Call with obj as first arg
        let expr = parse_expr("obj.method()");

        match expr {
            Expression::Call(c) => {
                assert_eq!(c.name, "method");
                assert!(c.ufcs);
                assert_eq!(c.args.len(), 1);
                assert!(matches!(c.args[0].init.as_ref(), Expression::Ident(_)));
            }
            _ => panic!("Expected Call, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_method_call_with_args() {
        // obj.method(a, b) should parse with obj, a, b as args
        let expr = parse_expr("obj.method(a, b)");

        match expr {
            Expression::Call(c) => {
                assert_eq!(c.name, "method");
                assert!(c.ufcs);
                assert_eq!(c.args.len(), 3);
                assert!(matches!(c.args[0].init.as_ref(), Expression::Ident(_))); // obj
                assert!(matches!(c.args[1].init.as_ref(), Expression::Ident(_))); // a
                assert!(matches!(c.args[2].init.as_ref(), Expression::Ident(_)));
                // b
            }
            _ => panic!("Expected Call"),
        }
    }

    #[test]
    fn test_regular_function_call() {
        let expr = parse_expr("foo(a, b)");

        match expr {
            Expression::Call(c) => {
                assert_eq!(c.name, "foo");
                assert!(!c.ufcs);
                assert_eq!(c.args.len(), 2);
            }
            _ => panic!("Expected Call"),
        }
    }

    #[test]
    fn test_member_binds_tighter_than_add() {
        // a.b + c should parse as (a.b) + c
        let expr = parse_expr("a.b + c");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Add));
                assert!(matches!(*op.left, Expression::Member(_)));
                assert!(matches!(*op.right, Expression::Ident(_)));
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_complex_expression() {
        // a.b() + c * d.e should parse as (a.b()) + (c * (d.e))
        let expr = parse_expr("a.b() + c * d.e");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Add));

                // Left: a.b()
                assert!(matches!(*op.left, Expression::Call(_)));

                // Right: c * d.e
                match *op.right {
                    Expression::BinaryOp(mul_op) => {
                        assert!(matches!(mul_op.op, Operator::Mul));
                        assert!(matches!(*mul_op.left, Expression::Ident(_)));
                        assert!(matches!(*mul_op.right, Expression::Member(_)));
                    }
                    _ => panic!("Expected multiplication on right side"),
                }
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_list_literal() {
        let expr = parse_expr("[a, b, c]");

        match expr {
            Expression::List(l) => {
                assert_eq!(l.elems.len(), 3);
                for elem in l.elems {
                    assert!(matches!(elem, Expression::Ident(_)));
                }
            }
            _ => panic!("Expected List"),
        }
    }

    #[test]
    fn test_index_expression() {
        let expr = parse_expr("arr[5]");

        match expr {
            Expression::Index(i) => {
                assert!(matches!(*i.target, Expression::Ident(_)));
                assert!(matches!(*i.index, Expression::Number(_)));
            }
            _ => panic!("Expected Index"),
        }
    }

    #[test]
    fn test_chained_index() {
        let expr = parse_expr("matrix[i][j]");

        match expr {
            Expression::Index(outer) => {
                assert!(matches!(*outer.target, Expression::Index(_)));
                assert!(matches!(*outer.index, Expression::Ident(_)));
            }
            _ => panic!("Expected chained Index"),
        }
    }

    #[test]
    fn test_index_with_member() {
        let expr = parse_expr("obj.arr[0]");

        match expr {
            Expression::Index(i) => {
                assert!(matches!(*i.target, Expression::Member(_)));
                assert!(matches!(*i.index, Expression::Number(_)));
            }
            _ => panic!("Expected Index with Member"),
        }
    }

    #[test]
    fn test_precedence_table() {
        // Test that all precedence levels are ordered correctly
        assert!(Operator::Else.precedence().0 < Operator::Or.precedence().0);
        assert!(Operator::Or.precedence().0 < Operator::And.precedence().0);
        assert!(Operator::And.precedence().0 < Operator::Eq.precedence().0);
        assert!(Operator::Eq.precedence().0 < Operator::Concat.precedence().0);
        assert!(Operator::Concat.precedence().0 < Operator::Add.precedence().0);
        assert!(Operator::Add.precedence().0 < Operator::Mul.precedence().0);
        assert!(Operator::Mul.precedence().0 < Operator::Not.precedence().1);
        assert!(Operator::Not.precedence().1 < Operator::Member.precedence().0);
    }

    #[test]
    fn test_else_operator_precedence() {
        // maybe else default should parse as one expression
        let expr = parse_expr("a else b");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Else));
                assert!(matches!(*op.left, Expression::Ident(_)));
                assert!(matches!(*op.right, Expression::Ident(_)));
            }
            _ => panic!("Expected BinaryOp with else"),
        }
    }

    #[test]
    fn test_else_has_lowest_precedence() {
        // a + b else c should parse as (a + b) else c
        let expr = parse_expr("a + b else c");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Else));

                match *op.left {
                    Expression::BinaryOp(add_op) => {
                        assert!(matches!(add_op.op, Operator::Add));
                    }
                    _ => panic!("Expected addition on left side"),
                }

                assert!(matches!(*op.right, Expression::Ident(_)));
            }
            _ => panic!("Expected BinaryOp"),
        }
    }

    #[test]
    fn test_block_expression() {
        let expr = parse_expr("{ a }");

        match expr {
            Expression::Block(stmts) => {
                assert_eq!(stmts.len(), 1);
                match &stmts[0] {
                    Statement::Expression(e) => {
                        assert!(matches!(e, Expression::Ident(_)));
                    }
                    _ => panic!("Expected expression statement"),
                }
            }
            _ => panic!("Expected Block, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_empty_block() {
        let expr = parse_expr("{}");

        match expr {
            Expression::Block(stmts) => {
                assert_eq!(stmts.len(), 0);
            }
            _ => panic!("Expected Block"),
        }
    }

    #[test]
    fn test_else_with_block() {
        // maybe else { default } should parse as else with a block
        let expr = parse_expr("a else { b }");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Else));
                assert!(matches!(*op.left, Expression::Ident(_)));

                match *op.right {
                    Expression::Block(stmts) => {
                        assert_eq!(stmts.len(), 1);
                    }
                    _ => panic!("Expected block on right side of else"),
                }
            }
            _ => panic!("Expected BinaryOp with else"),
        }
    }

    #[test]
    fn test_block_with_multiple_statements() {
        let expr = parse_expr("{ a b c }");

        match expr {
            Expression::Block(stmts) => {
                assert_eq!(stmts.len(), 3);
                for stmt in stmts {
                    match stmt {
                        Statement::Expression(e) => {
                            assert!(matches!(e, Expression::Ident(_)));
                        }
                        _ => panic!("Expected expression statement"),
                    }
                }
            }
            _ => panic!("Expected Block"),
        }
    }

    #[test]
    fn test_boolean_literal_true() {
        let expr = parse_expr("true");

        match expr {
            Expression::BoolLit(b) => {
                assert_eq!(b.value, true);
            }
            _ => panic!("Expected BoolLit, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_boolean_literal_false() {
        let expr = parse_expr("false");

        match expr {
            Expression::BoolLit(b) => {
                assert_eq!(b.value, false);
            }
            _ => panic!("Expected BoolLit, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_boolean_in_if_condition() {
        let expr = parse_expr("if true { x }");

        match expr {
            Expression::If(if_expr) => {
                assert!(matches!(*if_expr.cond, Expression::BoolLit(_)));
                match *if_expr.cond {
                    Expression::BoolLit(b) => assert_eq!(b.value, true),
                    _ => panic!("Expected BoolLit in condition"),
                }
            }
            _ => panic!("Expected If expression"),
        }
    }

    #[test]
    fn test_else_if_right_associative() {
        // if a {} else if b {} else {} should parse as: if a {} else (if b {} else {})
        // This ensures proper right-associativity
        let expr = parse_expr("if a { x } else if b { y } else { z }");

        match expr {
            Expression::BinaryOp(outer_else) => {
                assert!(matches!(outer_else.op, Operator::Else));

                // Left side should be: if a { x }
                assert!(matches!(*outer_else.left, Expression::If(_)));

                // Right side should be: (if b { y }) else { z }
                match *outer_else.right {
                    Expression::BinaryOp(inner_else) => {
                        assert!(matches!(inner_else.op, Operator::Else));
                        // Inner left should be: if b { y }
                        assert!(matches!(*inner_else.left, Expression::If(_)));
                        // Inner right should be: { z }
                        assert!(matches!(*inner_else.right, Expression::Block(_)));
                    }
                    _ => panic!("Expected nested else operator for right-associativity"),
                }
            }
            _ => panic!("Expected else operator, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_multiple_else_if_right_associative() {
        // Test three-level nesting: if a {} else if b {} else if c {} else {}
        let expr = parse_expr("if a { x } else if b { y } else if c { z } else { w }");

        match expr {
            Expression::BinaryOp(op1) => {
                assert!(matches!(op1.op, Operator::Else));
                assert!(matches!(*op1.left, Expression::If(_)));

                // First right side: if b {} else if c {} else {}
                match *op1.right {
                    Expression::BinaryOp(op2) => {
                        assert!(matches!(op2.op, Operator::Else));
                        assert!(matches!(*op2.left, Expression::If(_)));

                        // Second right side: if c {} else {}
                        match *op2.right {
                            Expression::BinaryOp(op3) => {
                                assert!(matches!(op3.op, Operator::Else));
                                assert!(matches!(*op3.left, Expression::If(_)));
                                assert!(matches!(*op3.right, Expression::Block(_)));
                            }
                            _ => panic!("Expected third nested else operator"),
                        }
                    }
                    _ => panic!("Expected second nested else operator"),
                }
            }
            _ => panic!("Expected else operator"),
        }
    }

    #[test]
    fn test_else_operator_is_right_associative() {
        // Test raw else chaining: a else b else c should parse as a else (b else c)
        let expr = parse_expr("a else b else c");

        match expr {
            Expression::BinaryOp(outer_else) => {
                assert!(matches!(outer_else.op, Operator::Else));
                assert!(matches!(*outer_else.left, Expression::Ident(_)));

                // Right side should be: b else c (another else operator)
                match *outer_else.right {
                    Expression::BinaryOp(inner_else) => {
                        assert!(matches!(inner_else.op, Operator::Else));
                        assert!(matches!(*inner_else.left, Expression::Ident(_)));
                        assert!(matches!(*inner_else.right, Expression::Ident(_)));
                    }
                    _ => panic!(
                        "Expected nested else for right-associativity, got {:?}",
                        outer_else.right.print()
                    ),
                }
            }
            _ => panic!("Expected else operator"),
        }
    }

    #[test]
    fn test_concat_operator() {
        let expr = parse_expr("a ++ b");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Concat));
                assert!(matches!(*op.left, Expression::Ident(_)));
                assert!(matches!(*op.right, Expression::Ident(_)));
            }
            _ => panic!("Expected BinaryOp with concat"),
        }
    }

    #[test]
    fn test_concat_is_left_associative() {
        // a ++ b ++ c should parse as (a ++ b) ++ c
        let expr = parse_expr("a ++ b ++ c");

        match expr {
            Expression::BinaryOp(outer) => {
                assert!(matches!(outer.op, Operator::Concat));

                // Left side should be: a ++ b
                match *outer.left {
                    Expression::BinaryOp(inner) => {
                        assert!(matches!(inner.op, Operator::Concat));
                        assert!(matches!(*inner.left, Expression::Ident(_)));
                        assert!(matches!(*inner.right, Expression::Ident(_)));
                    }
                    _ => panic!("Expected nested concat for left-associativity"),
                }

                // Right side should be: c
                assert!(matches!(*outer.right, Expression::Ident(_)));
            }
            _ => panic!("Expected concat operator"),
        }
    }

    #[test]
    fn test_concat_precedence_over_comparison() {
        // a ++ b == c should parse as (a ++ b) == c
        let expr = parse_expr("a ++ b == c");

        match expr {
            Expression::BinaryOp(eq_op) => {
                assert!(matches!(eq_op.op, Operator::Eq));

                // Left side should be: a ++ b
                match *eq_op.left {
                    Expression::BinaryOp(concat_op) => {
                        assert!(matches!(concat_op.op, Operator::Concat));
                    }
                    _ => panic!("Expected concat on left side"),
                }

                assert!(matches!(*eq_op.right, Expression::Ident(_)));
            }
            _ => panic!("Expected comparison operator"),
        }
    }

    #[test]
    fn test_add_precedence_over_concat() {
        // a + b ++ c should parse as (a + b) ++ c
        let expr = parse_expr("a + b ++ c");

        match expr {
            Expression::BinaryOp(concat_op) => {
                assert!(matches!(concat_op.op, Operator::Concat));

                // Left side should be: a + b
                match *concat_op.left {
                    Expression::BinaryOp(add_op) => {
                        assert!(matches!(add_op.op, Operator::Add));
                    }
                    _ => panic!("Expected add on left side"),
                }

                assert!(matches!(*concat_op.right, Expression::Ident(_)));
            }
            _ => panic!("Expected concat operator"),
        }
    }

    #[test]
    fn test_concat_with_string_literals() {
        let expr = parse_expr("\"hello\" ++ \"world\"");

        match expr {
            Expression::BinaryOp(op) => {
                assert!(matches!(op.op, Operator::Concat));
                assert!(matches!(*op.left, Expression::String(_)));
                assert!(matches!(*op.right, Expression::String(_)));
            }
            _ => panic!("Expected BinaryOp with concat"),
        }
    }

    #[test]
    fn test_named_arguments() {
        // foo(a: 1, b: 2) should parse with named arguments
        let expr = parse_expr("foo(a: 1, b: 2)");

        match expr {
            Expression::Call(c) => {
                assert_eq!(c.name, "foo");
                assert_eq!(c.args.len(), 2);

                // First argument: a: 1
                assert!(c.args[0].label.is_some());
                assert_eq!(c.args[0].label.as_ref().unwrap().name, "a");
                assert!(matches!(c.args[0].init.as_ref(), Expression::Number(_)));

                // Second argument: b: 2
                assert!(c.args[1].label.is_some());
                assert_eq!(c.args[1].label.as_ref().unwrap().name, "b");
                assert!(matches!(c.args[1].init.as_ref(), Expression::Number(_)));
            }
            _ => panic!("Expected Call, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_shorthand_arguments() {
        // foo(bar:, baz:) should parse as foo(bar: bar, baz: baz)
        let expr = parse_expr("foo(bar:, baz:)");

        match expr {
            Expression::Call(c) => {
                assert_eq!(c.name, "foo");
                assert_eq!(c.args.len(), 2);

                // First argument: bar: (shorthand for bar: bar)
                assert!(c.args[0].label.is_some());
                assert_eq!(c.args[0].label.as_ref().unwrap().name, "bar");
                match c.args[0].init.as_ref() {
                    Expression::Ident(ident) => assert_eq!(ident.name, "bar"),
                    _ => panic!("Expected Ident in shorthand init"),
                }

                // Second argument: baz: (shorthand for baz: baz)
                assert!(c.args[1].label.is_some());
                assert_eq!(c.args[1].label.as_ref().unwrap().name, "baz");
                match c.args[1].init.as_ref() {
                    Expression::Ident(ident) => assert_eq!(ident.name, "baz"),
                    _ => panic!("Expected Ident in shorthand init"),
                }
            }
            _ => panic!("Expected Call, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_mixed_positional_and_named() {
        // foo(1, b: 2) should parse with mixed arguments
        let expr = parse_expr("foo(1, b: 2)");

        match expr {
            Expression::Call(c) => {
                assert_eq!(c.name, "foo");
                assert_eq!(c.args.len(), 2);

                // First argument: positional
                assert!(c.args[0].label.is_none());
                assert!(matches!(c.args[0].init.as_ref(), Expression::Number(_)));

                // Second argument: named
                assert!(c.args[1].label.is_some());
                assert_eq!(c.args[1].label.as_ref().unwrap().name, "b");
                assert!(matches!(c.args[1].init.as_ref(), Expression::Number(_)));
            }
            _ => panic!("Expected Call, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_constructor_shorthand() {
        // Person(name:, age:) should parse with shorthand
        let expr = parse_expr("Person(name:, age:)");

        match expr {
            Expression::Constructor(c) => {
                assert_eq!(c.r#type.name, "Person");
                assert_eq!(c.args.len(), 2);

                // First argument: name: (shorthand)
                assert_eq!(c.args[0].label.name, "name");
                match c.args[0].init.as_ref() {
                    Expression::Ident(ident) => assert_eq!(ident.name, "name"),
                    _ => panic!("Expected Ident in constructor shorthand"),
                }

                // Second argument: age: (shorthand)
                assert_eq!(c.args[1].label.name, "age");
                match c.args[1].init.as_ref() {
                    Expression::Ident(ident) => assert_eq!(ident.name, "age"),
                    _ => panic!("Expected Ident in constructor shorthand"),
                }
            }
            _ => panic!("Expected Constructor, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_add_assign() {
        // a += b should parse as a = a + b
        let expr = parse_expr("a += b");

        match expr {
            Expression::Assignment(assignment) => {
                // Check that left side is 'a'
                match &assignment.left {
                    Lval::Ident(ident) => assert_eq!(ident.name, "a"),
                    _ => panic!("Expected Ident on left side"),
                }

                // Check that right side is a + b
                match assignment.right.as_ref() {
                    Expression::BinaryOp(op) => {
                        assert!(matches!(op.op, Operator::Add));
                        match op.left.as_ref() {
                            Expression::Ident(ident) => assert_eq!(ident.name, "a"),
                            _ => panic!("Expected Ident 'a' on left of addition"),
                        }
                        match op.right.as_ref() {
                            Expression::Ident(ident) => assert_eq!(ident.name, "b"),
                            _ => panic!("Expected Ident 'b' on right of addition"),
                        }
                    }
                    _ => panic!("Expected BinaryOp on right side"),
                }
            }
            _ => panic!("Expected Assignment, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_sub_assign() {
        // a -= b should parse as a = a - b
        let expr = parse_expr("a -= b");

        match expr {
            Expression::Assignment(assignment) => match assignment.right.as_ref() {
                Expression::BinaryOp(op) => {
                    assert!(matches!(op.op, Operator::Sub));
                }
                _ => panic!("Expected BinaryOp on right side"),
            },
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_mul_assign() {
        // a *= b should parse as a = a * b
        let expr = parse_expr("a *= b");

        match expr {
            Expression::Assignment(assignment) => match assignment.right.as_ref() {
                Expression::BinaryOp(op) => {
                    assert!(matches!(op.op, Operator::Mul));
                }
                _ => panic!("Expected BinaryOp on right side"),
            },
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_div_assign() {
        // a /= b should parse as a = a / b
        let expr = parse_expr("a /= b");

        match expr {
            Expression::Assignment(assignment) => match assignment.right.as_ref() {
                Expression::BinaryOp(op) => {
                    assert!(matches!(op.op, Operator::Div));
                }
                _ => panic!("Expected BinaryOp on right side"),
            },
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_rem_assign() {
        // a %= b should parse as a = a % b
        let expr = parse_expr("a %= b");

        match expr {
            Expression::Assignment(assignment) => match assignment.right.as_ref() {
                Expression::BinaryOp(op) => {
                    assert!(matches!(op.op, Operator::Rem));
                }
                _ => panic!("Expected BinaryOp on right side"),
            },
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_concat_assign() {
        // a ++= b should parse as a = a ++ b
        let expr = parse_expr("a ++= b");

        match expr {
            Expression::Assignment(assignment) => match assignment.right.as_ref() {
                Expression::BinaryOp(op) => {
                    assert!(matches!(op.op, Operator::Concat));
                }
                _ => panic!("Expected BinaryOp on right side"),
            },
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_compound_assignment_with_complex_rhs() {
        // a += b * c should parse as a = a + (b * c)
        let expr = parse_expr("a += b * c");

        match expr {
            Expression::Assignment(assignment) => {
                match assignment.right.as_ref() {
                    Expression::BinaryOp(add_op) => {
                        assert!(matches!(add_op.op, Operator::Add));

                        // Left side should be 'a'
                        match add_op.left.as_ref() {
                            Expression::Ident(ident) => assert_eq!(ident.name, "a"),
                            _ => panic!("Expected 'a' on left"),
                        }

                        // Right side should be b * c
                        match add_op.right.as_ref() {
                            Expression::BinaryOp(mul_op) => {
                                assert!(matches!(mul_op.op, Operator::Mul));
                            }
                            _ => panic!("Expected multiplication on right"),
                        }
                    }
                    _ => panic!("Expected BinaryOp on right side"),
                }
            }
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_compound_assignment_with_member() {
        // obj.field += 5 should parse as obj.field = obj.field + 5
        let expr = parse_expr("obj.field += 5");

        match expr {
            Expression::Assignment(assignment) => {
                // Left side should be obj.field
                match &assignment.left {
                    Lval::Member(member) => {
                        assert_eq!(member.member, "field");
                    }
                    _ => panic!("Expected Member on left side"),
                }

                // Right side should be obj.field + 5
                match assignment.right.as_ref() {
                    Expression::BinaryOp(op) => {
                        assert!(matches!(op.op, Operator::Add));
                        match op.left.as_ref() {
                            Expression::Member(member) => {
                                assert_eq!(member.member, "field");
                            }
                            _ => panic!("Expected Member on left of addition"),
                        }
                    }
                    _ => panic!("Expected BinaryOp on right side"),
                }
            }
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_compound_assignment_with_index() {
        // arr[0] += 1 should parse as arr[0] = arr[0] + 1
        let expr = parse_expr("arr[0] += 1");

        match expr {
            Expression::Assignment(assignment) => {
                // Left side should be arr[0]
                match &assignment.left {
                    Lval::Index(_) => {}
                    _ => panic!("Expected Index on left side"),
                }

                // Right side should be arr[0] + 1
                match assignment.right.as_ref() {
                    Expression::BinaryOp(op) => {
                        assert!(matches!(op.op, Operator::Add));
                        match op.left.as_ref() {
                            Expression::Index(_) => {}
                            _ => panic!("Expected Index on left of addition"),
                        }
                    }
                    _ => panic!("Expected BinaryOp on right side"),
                }
            }
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn test_simple_assignment() {
        // a = b should still parse correctly (no desugaring)
        let expr = parse_expr("a = b");

        match expr {
            Expression::Assignment(assignment) => {
                // Left side should be 'a'
                match &assignment.left {
                    Lval::Ident(ident) => assert_eq!(ident.name, "a"),
                    _ => panic!("Expected Ident on left side"),
                }

                // Right side should just be 'b' (no BinaryOp)
                match assignment.right.as_ref() {
                    Expression::Ident(ident) => assert_eq!(ident.name, "b"),
                    _ => panic!("Expected simple Ident 'b' on right side, not a BinaryOp"),
                }
            }
            _ => panic!("Expected Assignment"),
        }
    }

    // --- Qualified name tests ---

    fn parse_type(source: &str) -> Type {
        let mut parser = Parser::new(source);
        parser.expect_type()
    }

    #[test]
    fn test_qualified_type() {
        // hir::Type should parse with module = Some("hir")
        let ty = parse_type("hir::Type");
        assert_eq!(ty.name, "Type");
        assert_eq!(ty.module, Some("hir".to_string()));
    }

    #[test]
    fn test_unqualified_type() {
        // Type should parse with module = None
        let ty = parse_type("Type");
        assert_eq!(ty.name, "Type");
        assert_eq!(ty.module, None);
    }

    #[test]
    fn test_qualified_type_with_params() {
        // hir::Option(String) should parse with module and params
        let ty = parse_type("hir::Option(String)");
        assert_eq!(ty.name, "Option");
        assert_eq!(ty.module, Some("hir".to_string()));
        assert_eq!(ty.params.len(), 1);
        assert_eq!(ty.params[0].name, "String");
    }

    #[test]
    fn test_qualified_constructor() {
        // hir::Node(name: x) should parse as a constructor with module
        let expr = parse_expr("hir::Node(name: x)");

        match expr {
            Expression::Constructor(c) => {
                assert_eq!(c.r#type.name, "Node");
                assert_eq!(c.r#type.module, Some("hir".to_string()));
                assert_eq!(c.args.len(), 1);
                assert_eq!(c.args[0].label.name, "name");
            }
            _ => panic!("Expected Constructor, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_qualified_constructor_shorthand() {
        // hir::Node(name:) should parse with shorthand
        let expr = parse_expr("hir::Node(name:)");

        match expr {
            Expression::Constructor(c) => {
                assert_eq!(c.r#type.name, "Node");
                assert_eq!(c.r#type.module, Some("hir".to_string()));
                assert_eq!(c.args.len(), 1);
                assert_eq!(c.args[0].label.name, "name");
                match c.args[0].init.as_ref() {
                    Expression::Ident(ident) => assert_eq!(ident.name, "name"),
                    _ => panic!("Expected Ident in constructor shorthand"),
                }
            }
            _ => panic!("Expected Constructor, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_unqualified_constructor_still_works() {
        // Person(name: x) should still work without module
        let expr = parse_expr("Person(name: x)");

        match expr {
            Expression::Constructor(c) => {
                assert_eq!(c.r#type.name, "Person");
                assert_eq!(c.r#type.module, None);
                assert_eq!(c.args.len(), 1);
            }
            _ => panic!("Expected Constructor, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_unqualified_ident_still_works() {
        // Plain ident should still parse as ident (not confused by :: logic)
        let expr = parse_expr("foo");

        match expr {
            Expression::Ident(ident) => {
                assert_eq!(ident.name, "foo");
            }
            _ => panic!("Expected Ident, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_unqualified_call_still_works() {
        // foo(a, b) should still parse as a function call
        let expr = parse_expr("foo(a, b)");

        match expr {
            Expression::Call(call) => {
                assert_eq!(call.name, "foo");
                assert_eq!(call.module, None);
                assert_eq!(call.args.len(), 2);
            }
            _ => panic!("Expected Call, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_qualified_function_call() {
        // ast::empty_span() should parse as a qualified function call
        let expr = parse_expr("ast::empty_span()");

        match expr {
            Expression::Call(call) => {
                assert_eq!(call.name, "empty_span");
                assert_eq!(call.module, Some("ast".to_string()));
                assert_eq!(call.args.len(), 0);
                assert!(!call.ufcs);
            }
            _ => panic!("Expected Call, got {:?}", expr.print()),
        }
    }

    #[test]
    fn test_qualified_function_call_with_args() {
        // math::add(x, y) should parse as a qualified function call with args
        let expr = parse_expr("math::add(x, y)");

        match expr {
            Expression::Call(call) => {
                assert_eq!(call.name, "add");
                assert_eq!(call.module, Some("math".to_string()));
                assert_eq!(call.args.len(), 2);
            }
            _ => panic!("Expected Call, got {:?}", expr.print()),
        }
    }
}
