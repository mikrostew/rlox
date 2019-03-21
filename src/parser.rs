use std::iter::Peekable;
use std::slice::Iter;

use crate::ast::{BinaryOp, Expr, Literal, Stmt, UnaryOp};
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

// $a → $b ( ( $match_ops ) $b )* ;
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

// Parser for this grammar:
//
// program        → declaration* EOF ;
//
// declaration    → var_decl | statement ;
//
// var_decl       → "var" IDENTIFIER ( "=" expression )? ";" ;
//
// statement      → expr_stmt | print_stmt ;
//
// expr_stmt      → expression ";" ;
// print_stmt      → "print" expression ";" ;
//
// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
// addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
// multiplication → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary | primary ;
// primary        → NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")" | IDENTIFIER ;
impl Parser {
    pub fn new<R: Reporter + 'static>(tokens: Vec<Token>, err_reporter: R) -> Self {
        Parser {
            tokens,
            err_reporter: Box::new(err_reporter),
            num_errors: 0,
        }
    }

    pub fn parse(mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();
        let mut tokens = self.tokens.iter().peekable();

        // program   → statement* EOF ;
        loop {
            match self.statement(&mut tokens) {
                Ok(stmt) => match stmt {
                    Some(s) => statements.push(s),
                    None => break,
                },
                Err(_) => {
                    // TODO: need to do error recovery here?
                    self.num_errors += 1;
                }
            }
        }

        if self.num_errors > 0 {
            Err(format!("parser encountered {} error(s)", self.num_errors))
        } else {
            Ok(statements)
        }
    }

    // TODO: add:
    //  * comma-separated expressions
    //  * ternary operator: `a ? b : c`
    //  * error productions for binary operators without a left-hand operand
    //    (e.g. `<= 7`, or `== 4`)
    // see http://www.craftinginterpreters.com/parsing-expressions.html#challenges

    // statement → expr_stmt | print_stmt ;
    fn statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Option<Stmt>, String> {
        if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::Print => {
                    // consume the token and parse the print statement
                    tokens.next();
                    Ok(Some(self.print_statement(tokens)?))
                }
                // TODO: going to add the other ones
                // once we hit EOF, that's the end of the statements
                TokenKind::Eof => Ok(None),
                _ => {
                    // doesn't match anything else, so assume expression
                    Ok(Some(self.expression_statement(tokens)?))
                }
            }
        } else {
            // no more tokens, no more statements
            Ok(None)
        }
    }

    // print_stmt → "print" expression ";" ;
    fn print_statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        let value = self.expression(tokens)?;
        // TODO: this should really be the position of the expression itself
        let todo_token = Token::new(TokenKind::Nil, "nil", &Position::new());
        self.consume_or_err(
            tokens,
            TokenKind::Semicolon,
            "expected `;` at end of statement",
            &todo_token,
        )?;
        Ok(Stmt::Print(Box::new(value)))
    }

    // expr_stmt → expression ";" ;
    fn expression_statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        let expr = self.expression(tokens)?;
        // TODO: see same thing above
        let todo_token = Token::new(TokenKind::Nil, "nil", &Position::new());
        self.consume_or_err(
            tokens,
            TokenKind::Semicolon,
            "expected `;` at end of statement",
            &todo_token,
        )?;
        Ok(Stmt::Expression(Box::new(expr)))
    }

    // expression → equality ;
    fn expression(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        // TODO: error handling here?
        self.equality(tokens)
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    binary_expr_parser!(equality, comparison, match_equality_op);

    // comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
    binary_expr_parser!(comparison, addition, match_comparison_op);

    // addition → multiplication ( ( "-" | "+" ) multiplication )* ;
    binary_expr_parser!(addition, multiplication, match_addition_op);

    // multiplication → unary ( ( "/" | "*" ) unary )* ;
    binary_expr_parser!(multiplication, unary, match_multiplication_op);

    // unary → ( "!" | "-" ) unary | primary ;
    fn unary(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        if let Some(operator) = Parser::match_unary_op(tokens) {
            let right = self.unary(tokens)?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }

        self.primary(tokens)
    }

    // primary → NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")" ;
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

    // if the next token is '!=' or '==', return the equivalent BinaryOp
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

    // if the next token is '+' or '-', return the equivalent BinaryOp
    match_op!(match_addition_op, BinaryOp, Plus, Minus);

    // if the next token is '*' or '/', return the equivalent BinaryOp
    match_op!(match_multiplication_op, BinaryOp, Star, Slash);

    // if the next token is '!' or '-', return the equivalent UnaryOp
    match_op!(match_unary_op, UnaryOp, Bang, Minus);

    // TODO: this should take a Position instead of an err_token Token
    // (and position should include length)
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
