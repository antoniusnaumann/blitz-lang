use std::{iter::Peekable, ops::RangeInclusive};

use crate::*;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    source: &'a str,
    span: Span,
    has_pub: bool,
}

impl<'a> Parser<'a> {
    pub fn from(lexer: Lexer<'a>) -> Self {
        Self {
            source: lexer.source,
            lexer: lexer.peekable(),
            span: Span { start: 0, end: 0 },
            has_pub: false,
        }
    }

    pub fn new(source: &'a str) -> Self {
        Self {
            source: source,
            lexer: crate::Lexer::new(source).peekable(),
            span: Span { start: 0, end: 0 },
            has_pub: false,
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Token {
        if kind != TokenKind::Newline {
            self.skip_newlines();
        }
        let next = self.lexer.next().unwrap_or_else(|| {
            panic!(
                "Expected {:#?}, but got nothing.\n\n{}",
                kind,
                self.report_span(self.span.clone())
            )
        });
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
        self.lexer.kind() == kind
    }

    fn one_of(&mut self, kinds: &[TokenKind]) -> Token {
        if kinds.contains(&TokenKind::Newline) {
            self.skip_newlines();
        }
        let next = self.lexer.next().unwrap();
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
        let name = String::from(&self.source[RangeInclusive::from(span.clone())]);
        let params = self.parse_type_params();

        Some(Type { name, params, span })
    }

    fn skip_newlines(&mut self) {
        while self
            .lexer
            .next_if(|token| token.kind == TokenKind::Newline || token.kind == TokenKind::Comment)
            .is_some()
        {}
    }

    fn expect_ident(&mut self) -> Ident {
        let Token { kind: _, span } = self.expect(TokenKind::Ident);
        let name = self.source[RangeInclusive::from(span.clone())].into();

        Ident { name, span }
    }

    fn expect_type(&mut self) -> Type {
        let Token { kind: _, span } = self.expect(TokenKind::Type);
        let name = String::from(&self.source[RangeInclusive::from(span.clone())]);
        let params = self.parse_type_params();

        Type { name, params, span }
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
            fields.push(self.parse_field());
        }
        self.expect(TokenKind::Rbrace);

        Struct {
            sig,
            fields,
            span: start.merge(&self.span),
        }
    }

    fn parse_field(&mut self) -> Field {
        let name = self.expect_ident().name.into();
        let ty = self.expect_type();
        self.consume(TokenKind::Comma);

        Field { name, r#type: ty }
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
        let name = self.expect_ident().name.into();

        self.expect(TokenKind::Lparen);
        // TODO: also collect type params
        let mut args = Vec::new();
        self.skip_newlines();
        while !self.has(TokenKind::Rparen) {
            let is_mut = self.consume(TokenKind::Mut).is_some();
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
            TokenKind::Let | TokenKind::Mut => self.parse_declaration().into(),
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

            let unary = UnaryOp {
                op,
                expr: Box::new(expr),
                span,
            }
            .into();

            return self.parse_precedence(unary, min_prec);
        }

        // Handle primary expressions (atoms)
        let lhs = match &token.kind {
            TokenKind::For => self.parse_for().into(),
            TokenKind::While => self.parse_while().into(),
            TokenKind::If => self.parse_if().into(),
            TokenKind::Switch => self.parse_switch().into(),
            TokenKind::Return => Expression::Return(self.parse_expression_bp(0).into()),
            TokenKind::Continue => Expression::Continue,
            TokenKind::Break => Expression::Break,
            TokenKind::Lbrace => self.parse_block().into(),
            TokenKind::Ident => {
                let span = token.span;
                let name = self.expect_ident().name;

                if self.has(TokenKind::Lparen) {
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
            TokenKind::Num => self.parse_num_lit().into(),
            kind => {
                panic!(
                    "Illegal token kind {:#?} for expression!\n\n{}",
                    kind,
                    self.span.report(self.source)
                )
            }
        };

        let expr = self.parse_precedence(lhs, min_prec);
        if let Some(assignment) = self.consume_assignment() {
            let left = match expr {
                Expression::Ident(ident) => Lval::Ident(ident),
                Expression::Member(member) => Lval::Member(member),
                e => panic!("Assignment on invalid lval {:#?}", e),
            };

            let right = self.parse_expression().into();
            Assignment { left, right }.into()
        } else {
            expr
        }
    }

    // TODO: introduce assignment operator union
    fn consume_assignment(&mut self) -> Option<()> {
        self.consume(TokenKind::Assign).map(|_| ())
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_expression_bp(0)
    }

    fn parse_member_call(&mut self, parent: Expression, method: String) -> Call {
        self.expect(TokenKind::Lparen);

        let mut args = vec![parent.clone()];
        while !self.has(TokenKind::Rparen) {
            let is_mut = self.consume(TokenKind::Mut).is_some();
            args.push(self.parse_expression());
            self.consume(TokenKind::Comma);
        }
        let end_span = self.expect(TokenKind::Rparen).span;

        Call {
            name: method,
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
                TokenKind::Ident => self.expect_ident().into(),
                TokenKind::Type => self.expect_type().into(),
                _ => todo!("Support other tokens than ident and type for switch"),
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

    fn parse_num_lit(&mut self) -> Float {
        let Token { kind: _, span } = self.expect(TokenKind::Num);

        self.source[RangeInclusive::from(span)].parse().unwrap()
    }

    fn parse_block(&mut self) -> Expression {
        Expression::Block(self.parse_body())
    }

    fn parse_call(&mut self, ident: &str) -> Call {
        let Token { kind: _, span } = self.expect(TokenKind::Lparen);

        let mut args = Vec::new();
        while !self.has(TokenKind::Rparen) {
            let is_mut = self.consume(TokenKind::Mut).is_some();
            args.push(self.parse_expression());
            self.consume(TokenKind::Comma);
        }
        self.expect(TokenKind::Rparen);

        Call {
            name: ident.into(),
            args,
            ufcs: false,
            span: span.merge(&self.span),
        }
    }

    fn parse_constructor(&mut self) -> Constructor {
        let Token { kind: _, span } = self.expect(TokenKind::Type);
        let name = String::from(&self.source[RangeInclusive::from(span.clone())]);

        let mut args = Vec::new();
        let mut type_params = Vec::new();
        self.expect(TokenKind::Lparen);
        // TODO: parse type args (args before semicolon)
        while !self.has(TokenKind::Rparen) {
            let label = self.expect_ident();
            self.expect(TokenKind::Colon);
            let init = self.parse_expression().into();
            self.consume(TokenKind::Comma);
            args.push(ConstructorArg { label, init })
        }
        self.expect(TokenKind::Rparen);

        let r#type = Type {
            name,
            params: type_params,
            span: span.clone(),
        };
        Constructor {
            r#type,
            args,
            span: span.merge(&self.span),
        }
    }

    fn parse_declaration(&mut self) -> Declaration {
        let Token { kind, span } = self.one_of(&[TokenKind::Let, TokenKind::Mut]);
        let name = self.expect_ident().name.into();

        // TODO: type inference
        let r#type = if let Some(token) = self.consume(TokenKind::Colon) {
            self.expect_type()
        } else {
            Type {
                name: "_".into(),
                params: Vec::new(),
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
            is_mut: kind == TokenKind::Mut,
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
            Tk::Actor => todo!("actor"),
            Tk::Test => todo!("test"),
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
                assert!(matches!(c.args[0], Expression::Ident(_)));
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
                assert!(matches!(c.args[0], Expression::Ident(_))); // obj
                assert!(matches!(c.args[1], Expression::Ident(_))); // a
                assert!(matches!(c.args[2], Expression::Ident(_))); // b
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
}
