use std::iter::Peekable;
use std::slice::Iter;

use crate::ast::{BinaryOp, Expr, Literal, LogicalOp, Stmt, UnaryOp};
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

// $fn_name → $next_fn ( ( $match_ops ) $next_fn )* ;
macro_rules! binary_expr_parser {
    ( $expr_kind:ident, $fn_name:ident, $next_fn:ident, $match_ops:ident ) => {
        fn $fn_name(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
            let mut expr = self.$next_fn(tokens)?;
            while let Some(operator) = Parser::$match_ops(tokens) {
                let right = self.$next_fn(tokens)?;
                expr = Expr::$expr_kind(Box::new(expr), operator, Box::new(right));
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
// statement      → expr_stmt | if_stmt | print_stmt | block ;
//
// expr_stmt      → expression ";" ;
// if_stmt        → "if" "(" expression ")" statement ( "else" statement )? ;
// print_stmt     → "print" expression ";" ;
// block          → "{" declaration* "}" ;
//
// expression     → assignment ;
// assignment     → IDENTIFIER "=" assignment | logic_or ;
// logic_or       → logic_and ( "or" logic_and )* ;
// logic_and      → equality ( "and" equality )* ;
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

        // program → declaration* EOF ;
        loop {
            match self.declaration(&mut tokens) {
                Ok(stmt) => match stmt {
                    Some(s) => statements.push(s),
                    None => break,
                },
                Err(_) => {
                    self.num_errors += 1;
                    self.synchronize(&mut tokens);
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

    // declaration → var_decl | statement ;
    fn declaration(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Option<Stmt>, String> {
        if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::Var => {
                    // consume the token and parse the var declaration
                    tokens.next();
                    Ok(Some(self.var_declaration(tokens)?))
                }
                // TODO: going to add the other ones
                // once we hit EOF, that's the end of the statements
                TokenKind::Eof => Ok(None),
                // doesn't match anything else, so assume statement
                _ => self.statement(tokens),
            }
        } else {
            // no more tokens, no more statements
            Ok(None)
        }
    }

    // var_decl → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn var_declaration(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        // TODO: this should really be the position of the expression/declaration itself
        let todo_token = Token::new(TokenKind::Nil, "nil", &Position::new());
        let name = self.consume_or_err(
            tokens,
            TokenKind::Identifier("".to_string()),
            "expected variable name",
            &todo_token,
        )?;

        // assume nil unless an expression is given
        let mut initializer = Expr::Literal(Literal::Nil);

        // TODO: there should be a match() function (like in the book) to do these 3 lines
        if let Some(token) = tokens.peek() {
            if token.kind == TokenKind::Equal {
                tokens.next();
                initializer = self.expression(tokens)?;
            }
        }

        // TODO: this should really be the position of the expression/declaration itself
        let todo_token = Token::new(TokenKind::Nil, "nil", &Position::new());
        self.consume_or_err(
            tokens,
            TokenKind::Semicolon,
            "expected `;` after variable declaration",
            &todo_token,
        )?;

        Ok(Stmt::Var(name.to_string(), Box::new(initializer)))
    }

    // statement → expr_stmt | if_stmt | print_stmt | block ;
    fn statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Option<Stmt>, String> {
        if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::If => {
                    // consume the token and parse the print statement
                    tokens.next();
                    Ok(Some(self.if_statement(tokens)?))
                }
                TokenKind::Print => {
                    // consume the token and parse the print statement
                    tokens.next();
                    Ok(Some(self.print_statement(tokens)?))
                }
                TokenKind::LeftBrace => {
                    // consume the token and parse the print statement
                    tokens.next();
                    Ok(Some(self.block_statement(tokens)?))
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

    // if_stmt → "if" "(" expression ")" statement ( "else" statement )? ;
    fn if_statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        // TODO: this should be the actual position
        let todo_token = Token::new(TokenKind::Nil, "nil", &Position::new());
        self.consume_or_err(
            tokens,
            TokenKind::LeftParen,
            "expected `(` after `if`",
            &todo_token,
        )?;

        let condition = self.expression(tokens)?;

        // TODO: this should be the actual position
        let todo_token = Token::new(TokenKind::Nil, "nil", &Position::new());
        self.consume_or_err(
            tokens,
            TokenKind::RightParen,
            "expected `)` after if condition",
            &todo_token,
        )?;

        let then_branch = self
            .statement(tokens)?
            .ok_or("TODO: expected statement after `if ( )`")?;

        // this will attach a dangling `else` to the closest `if`
        let else_branch = if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::Else => {
                    // consume the else and parse the statement
                    tokens.next();
                    Some(
                        self.statement(tokens)?
                            .ok_or("TODO: expected statement after `else`")?,
                    )
                }
                _ => None,
            }
        } else {
            // no more tokens
            None
        };

        Ok(Stmt::If(
            Box::new(condition),
            Box::new(then_branch),
            else_branch.map(|eb| Box::new(eb)),
        ))
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

    // block → "{" declaration* "}" ;
    fn block_statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        let mut statements = Vec::new();

        // loop through the block of statements until we hit the "}"
        loop {
            if let Some(token) = tokens.peek() {
                match token.kind {
                    // exit once we find the right brace
                    TokenKind::RightBrace => break,
                    // otherwise parse the statements
                    _ => match self.declaration(tokens)? {
                        Some(stmt) => statements.push(stmt),
                        None => break,
                    },
                }
            } else {
                // ran out of tokens
                break;
            }
        }

        // TODO: this should be at the correct position
        let todo_token = Token::new(TokenKind::Nil, "nil", &Position::new());
        self.consume_or_err(
            tokens,
            TokenKind::RightBrace,
            "expected `}` after block",
            &todo_token,
        )?;

        Ok(Stmt::Block(statements))
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

    // expression → assignment ;
    fn expression(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        self.assignment(tokens)
    }

    // assignment → IDENTIFIER "=" assignment | logic_or ;
    fn assignment(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        // parse the l-value (left hand side of assignment, or just an expression),
        // at this point for assignment this will just be an identifier
        let l_value = self.logic_or(tokens)?;

        // TODO: there should be a match() function (like in the book) to do these 3 lines
        if let Some(token) = tokens.peek() {
            if token.kind == TokenKind::Equal {
                // if it's an equal, consume it but save for possible err reporting
                let equal = tokens.next().unwrap();

                // parse the r-value (right hand side of assignment)
                let r_value = self.assignment(tokens)?;

                // check left hand side is valid assignment target
                match l_value {
                    Expr::Variable(name) => {
                        return Ok(Expr::Assign(name.clone(), Box::new(r_value)));
                    }
                    _ => {
                        let report_string = self.err_reporter.report(
                            "invalid assignment target",
                            "for this assignment",
                            &equal.position,
                            equal.length,
                        );
                        return Err(report_string);
                    }
                }
            }
        }

        // not an assignment, just return the expression
        Ok(l_value)
    }

    // logic_or → logic_and ( "or" logic_and )* ;
    binary_expr_parser!(Logical, logic_or, logic_and, match_or_op);

    // logic_and → equality ( "and" equality )* ;
    binary_expr_parser!(Logical, logic_and, equality, match_and_op);

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    binary_expr_parser!(Binary, equality, comparison, match_equality_op);

    // comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
    binary_expr_parser!(Binary, comparison, addition, match_comparison_op);

    // addition → multiplication ( ( "-" | "+" ) multiplication )* ;
    binary_expr_parser!(Binary, addition, multiplication, match_addition_op);

    // multiplication → unary ( ( "/" | "*" ) unary )* ;
    binary_expr_parser!(Binary, multiplication, unary, match_multiplication_op);

    // unary → ( "!" | "-" ) unary | primary ;
    fn unary(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        if let Some(operator) = Parser::match_unary_op(tokens) {
            let right = self.unary(tokens)?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }

        self.primary(tokens)
    }

    // primary → NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")" | IDENTIFIER ;
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
                TokenKind::Identifier(s) => Expr::Variable(s.to_string()),
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

    // if the next token is 'or', return the equivalent LogicalOp
    match_op!(match_or_op, LogicalOp, Or);

    // if the next token is 'and', return the equivalent LogicalOp
    match_op!(match_and_op, LogicalOp, And);

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

    // consume the specified token and return it, or report the input error and return that
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
            match kind {
                // TODO
                // TokenKind::String(_) => {
                // }
                // TokenKind::Number(_) => {
                // }
                TokenKind::Identifier(_) => {
                    if let TokenKind::Identifier(_s) = &token.kind {
                        // consume and return the token (already peeked so expect should be ok)
                        return Ok(tokens
                            .next()
                            .cloned()
                            .expect("could not consume token after peeking"));
                    }
                }
                _ => {
                    if token.kind == kind {
                        // consume and return the token (already peeked so expect should be ok)
                        return Ok(tokens
                            .next()
                            .cloned()
                            // TODO: parser should not panic (this should be an error)
                            .expect("could not consume token after peeking"));
                    }
                }
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

                // anything else consume the token and keep going
                _ => {
                    tokens.next();
                }
            }
        }
    }
}
