use std::iter::Peekable;
use std::slice::Iter;

use crate::ast::{Expr, Literal};
use crate::token::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens }
    }

    pub fn parse(self) -> Result<Expr, String> {
        let mut tokens = self.tokens.iter().peekable();

        // TODO: right now this only does expressions (that will change later...)
        self.expression(&mut tokens)
    }

    // expression     → equality ;
    fn expression(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
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
                _ => return Err(format!("Expected primary, got {}", token)),
            })
        } else {
            Err("TODO: Ran out of tokens in primary()".to_string())
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
        if let Some(token) = tokens.peek() {
            if token.kind == kind {
                // consume the token
                return tokens
                    .next()
                    .cloned()
                    .ok_or("consume() could not get next token for some reason".to_string());
            }
        }
        // TODO: need to have custom error things, with the token, maybe position, stuff like that
        Err(err.to_string())
    }
}
