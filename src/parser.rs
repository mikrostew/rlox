use std::iter::Peekable;
use std::slice::Iter;

use crate::ast::{Expr, Literal};
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

impl Parser {
    pub fn new<R: Reporter + 'static>(tokens: Vec<Token>, err_reporter: R) -> Self {
        Parser {
            tokens,
            err_reporter: Box::new(err_reporter),
            num_errors: 0,
        }
    }

    pub fn parse(self) -> Result<Expr, String> {
        let mut tokens = self.tokens.iter().peekable();

        // TODO: right now this only does expressions (that will change later...)
        match self.expression(&mut tokens) {
            Ok(expr) => Ok(expr),
            Err(_) => Err(format!("Parser encountered {} errors", self.num_errors)),
        }
    }

    // expression     → equality ;
    fn expression(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        // TODO: error handling here?
        self.equality(tokens)
    }

    // TODO: can possibly macro-ize a lot of these, they're very similar

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        let mut expr = self.comparison(tokens)?;

        while let Some(operator) = Parser::match_equality_op(tokens) {
            let right = self.comparison(tokens)?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
    fn comparison(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        let mut expr = self.addition(tokens)?;

        while let Some(operator) = Parser::match_comparison_op(tokens) {
            let right = self.addition(tokens)?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
    fn addition(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        let mut expr = self.multiplication(tokens)?;

        while let Some(operator) = Parser::match_addition_op(tokens) {
            let right = self.multiplication(tokens)?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // multiplication → unary ( ( "/" | "*" ) unary )* ;
    fn multiplication(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        let mut expr = self.unary(tokens)?;

        while let Some(operator) = Parser::match_multiplication_op(tokens) {
            let right = self.unary(tokens)?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

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
                    self.consume_or_err(tokens, TokenKind::RightParen, "Missing closing ')'")?;
                    Expr::Grouping(Box::new(expr))
                }
                _ => {
                    let report_string = self.err_reporter.report(
                        &format!(
                            "unexpected token: expected literal or '(', found `{}`",
                            token
                        ),
                        &format!("unexpected token"),
                        &token.position,
                        1,
                    );
                    return Err(report_string);
                }
            })
        } else {
            let report_string = self.err_reporter.report(
                // TODO: better error messages
                "ran out of tokens in primary()",
                "ran out of tokens",
                // TODO: report the position where we started parsing the expression
                &Position::new(),
                1,
            );
            return Err(report_string);
        }
    }

    // TODO: can probably macro-ize these, they're so similar

    // if the next token is '!=' or '==', return it
    fn match_equality_op(tokens: &mut Peekable<Iter<Token>>) -> Option<Token> {
        if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::BangEqual | TokenKind::EqualEqual => tokens.next().cloned(),
                _ => None,
            }
        } else {
            None
        }
    }

    // if the next token is '>', '>=', '<' or '<=', return it
    fn match_comparison_op(tokens: &mut Peekable<Iter<Token>>) -> Option<Token> {
        if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::Less
                | TokenKind::LessEqual => tokens.next().cloned(),
                _ => None,
            }
        } else {
            None
        }
    }

    // if the next token is '+' or '-', return it
    fn match_addition_op(tokens: &mut Peekable<Iter<Token>>) -> Option<Token> {
        if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::Plus | TokenKind::Minus => tokens.next().cloned(),
                _ => None,
            }
        } else {
            None
        }
    }

    // if the next token is '*' or '/', return it
    fn match_multiplication_op(tokens: &mut Peekable<Iter<Token>>) -> Option<Token> {
        if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::Slash | TokenKind::Star => tokens.next().cloned(),
                _ => None,
            }
        } else {
            None
        }
    }

    // if the next token is '!' or '-', return it
    fn match_unary_op(tokens: &mut Peekable<Iter<Token>>) -> Option<Token> {
        if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::Bang | TokenKind::Minus => tokens.next().cloned(),
                _ => None,
            }
        } else {
            None
        }
    }

    fn consume_or_err(
        &self,
        tokens: &mut Peekable<Iter<Token>>,
        kind: TokenKind,
        err: &str,
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
        // TODO: position is end of file/source?
        let report_string = self.err_reporter.report(err, err, &Position::new(), 1);
        Err(report_string)
    }
}
