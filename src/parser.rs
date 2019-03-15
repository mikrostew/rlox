use std::iter::Peekable;
use std::slice::Iter;

use crate::ast::{BinaryOp, Expr, Literal, UnaryOp};
use crate::error::Reporter;
use crate::token::{Position, Token, TokenKind};

pub struct Parser {
    // reporter that implements this trait
    err_reporter: Box<Reporter>,
    // how many errors the scanner encountered
    num_errors: u64,
    // the token stream from the scanner
    tokens: Vec<Token>,
}

// $a  → $b ( ( $match_ops ) $b )* ;
macro_rules! binary_expr_parser {
    ( $a:ident, $b:ident, $match_ops:ident ) => {
        fn $a(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
            let mut expr = self.$b(tokens)?;
            while let Some(operator) = Parser::$match_ops(tokens) {
                let right = self.$b(tokens)?;
                expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
            }
            Ok(expr)
        }
    }
}

// if the next token is one of the $kind list, return the matching $opkind::$kind
// (including the position, to track for errors)
macro_rules! match_op {
    ( $a:ident, $opkind:ident, $($kind:ident),* ) => {
        fn $a(tokens: &mut Peekable<Iter<Token>>) -> Option<$opkind> {
            if let Some(token) = tokens.peek() {
                match token.kind {
                    $(
                        TokenKind::$kind => tokens.next().map(|t| $opkind::$kind(t.position.clone())),
                    )*
                    _ => None,
                }
            } else {
                None
            }
        }
    };
}

impl Parser {
    pub fn new<R: Reporter + 'static>(tokens: Vec<Token>, err_reporter: R) -> Self {
        Parser {
            tokens,
            err_reporter: Box::new(err_reporter),
            num_errors: 0,
        }
    }

    pub fn parse(mut self) -> Result<Expr, String> {
        let mut tokens = self.tokens.iter().peekable();

        // TODO: right now this only does expressions (that will change later...)
        match self.expression(&mut tokens) {
            Ok(expr) => Ok(expr),
            Err(_) => {
                // TODO: this should be calculated somewhere else?
                self.num_errors += 1;
                Err(format!("parser encountered {} error(s)", self.num_errors))
            }
        }
    }

    // TODO: add:
    //  * comma-separated expressions
    //  * ternary operator: `a ? b : c`
    //  * error productions for binary operators without a left-hand operand
    //    (e.g. `<= 7`, or `== 4`)
    // see http://www.craftinginterpreters.com/parsing-expressions.html#challenges

    // expression     → equality ;
    fn expression(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        // TODO: error handling here?
        self.equality(tokens)
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    binary_expr_parser!(equality, comparison, match_equality_op);
    // comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
    binary_expr_parser!(comparison, addition, match_comparison_op);
    // addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
    binary_expr_parser!(addition, multiplication, match_addition_op);
    // multiplication → unary ( ( "/" | "*" ) unary )* ;
    binary_expr_parser!(multiplication, unary, match_multiplication_op);

    // unary          → ( "!" | "-" ) unary | primary ;
    fn unary(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        if let Some(operator) = Parser::match_unary_op(tokens) {
            let right = self.unary(tokens)?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }

        self.primary(tokens)
    }

    // primary        → NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")" ;
    fn primary(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        if let Some(token) = tokens.next() {
            Ok(match &token.kind {
                TokenKind::False => Expr::Literal(Literal::Bool(false)),
                TokenKind::True => Expr::Literal(Literal::Bool(true)),
                TokenKind::Nil => Expr::Literal(Literal::Nil),
                TokenKind::Number(n) => Expr::Literal(Literal::Number(*n)),
                TokenKind::String(s) => Expr::Literal(Literal::String(s.to_string())),
                TokenKind::LeftParen => {
                    let expr = self.expression(tokens)?;
                    self.consume_or_err(
                        tokens,
                        TokenKind::RightParen,
                        "missing closing `)`",
                        &token,
                    )?;
                    Expr::Grouping(Box::new(expr))
                }
                _ => {
                    let report_string = self.err_reporter.report(
                        &format!("expected literal or `(`, found `{}`", token),
                        &format!("unexpected"),
                        &token.position,
                        token.length,
                    );
                    return Err(report_string);
                }
            })
        } else {
            // TODO: this shouldn't happen, so maybe don't have this case?
            let report_string = self.err_reporter.report(
                "expected literal or `(`, found nothing",
                "expected literal",
                &Position::new(),
                1,
            );
            return Err(report_string);
        }
    }

    // if the next token is '!=' or '==', return it
    match_op!(match_equality_op, BinaryOp, BangEqual, EqualEqual);
    // if the next token is '>', '>=', '<' or '<=', return it
    match_op!(
        match_comparison_op,
        BinaryOp,
        Greater,
        GreaterEqual,
        Less,
        LessEqual
    );
    // if the next token is '+' or '-', return it
    match_op!(match_addition_op, BinaryOp, Plus, Minus);
    // if the next token is '*' or '/', return it
    match_op!(match_multiplication_op, BinaryOp, Star, Slash);
    // if the next token is '!' or '-', return it
    match_op!(match_unary_op, UnaryOp, Bang, Minus);

    fn consume_or_err(
        &self,
        tokens: &mut Peekable<Iter<Token>>,
        kind: TokenKind,
        err: &str,
        err_token: &Token,
    ) -> Result<Token, String> {
        if let Some(&token) = tokens.peek() {
            if token.kind == kind {
                // consume the token (already peeked so unwrap is ok)
                return Ok(tokens
                    .next()
                    .cloned()
                    // TODO: parser should not panic (this should be an error)
                    .expect("could not consume token after peeking"));
            }
        }
        let report_string =
            self.err_reporter
                .report(err, err, &err_token.position, err_token.length);
        Err(report_string)
    }

    // synchronize the parser to the beginning of a statement
    fn synchronize(&self, tokens: &mut Peekable<Iter<Token>>) {
        // go thru tokens until we find a semicolon, or the start of a statement

        while let Some(&token) = tokens.peek() {
            match token.kind {
                // for ; consume that and assume we are synchronized on the next statement
                TokenKind::Semicolon => {
                    tokens.next();
                    return;
                }

                // for the beginning of a statement, don't consume the token
                TokenKind::Class
                | TokenKind::For
                | TokenKind::Fun
                | TokenKind::If
                | TokenKind::Print
                | TokenKind::Return
                | TokenKind::Var
                | TokenKind::While => return,

                // anything else keep going
                _ => (),
            }
        }
    }
}
