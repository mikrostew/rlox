use std::fmt;
use std::iter::Peekable;
use std::slice::Iter;

use crate::ast::{BinaryOp, Expr, Identifier, Literal, LogicalOp, Stmt, UnaryOp};
use crate::error::Reporter;
use crate::token::{Position, Token, TokenKind};

pub struct Parser {
    // file that is being parsed (None if it's the REPL)
    file: Option<String>,
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
                let expr_pos = Position::from_positions(expr.position(), right.position());
                expr = Expr::$expr_kind(expr_pos, Box::new(expr), operator, Box::new(right));
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

enum FunKind {
    Function,
}

impl fmt::Display for FunKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunKind::Function => write!(f, "function"),
        }
    }
}

// Parser for this grammar:
//
// program        → declaration* EOF ;
//
// declaration    → fun_decl
//                | var_decl
//                | statement ;
//
// fun_decl       → "fun" function ;
//
// function       → IDENTIFIER "(" parameters? ")" block ;
//
// parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
//
// var_decl       → "var" IDENTIFIER ( "=" expression )? ";" ;
//
// statement      → expr_stmt
//                | for_stmt
//                | if_stmt
//                | print_stmt
//                | return_stmt
//                | while_stmt
//                | block ;
//
// expr_stmt      → expression ";" ;
//
// for_stmt        → "for" "("
//                      ( var_decl | expr_stmt | ";" )
//                      expression? ";"
//                      expression?
//                  ")" statement ;
//
// if_stmt        → "if" "(" expression ")" statement ( "else" statement )? ;
//
// print_stmt     → "print" expression ";" ;
//
// return_stmt    → "return" expression? ";" ;
//
// while_stmt     → "while" "(" expression ")" statement ;
//
// block          → "{" declaration* "}" ;
//
// expression     → assignment ;
//
// assignment     → IDENTIFIER "=" assignment | logic_or ;
//
// logic_or       → logic_and ( "or" logic_and )* ;
//
// logic_and      → equality ( "and" equality )* ;
//
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//
// comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
//
// addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
//
// multiplication → unary ( ( "/" | "*" ) unary )* ;
//
// unary          → ( "!" | "-" ) unary | call ;
//
// call           → primary ( "(" arguments? ")" )* ;
//
// primary        → NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")" | IDENTIFIER ;
//
// arguments      → expression ( "," expression )* ;
impl Parser {
    pub fn new<R: Reporter + 'static>(
        file: Option<String>,
        tokens: Vec<Token>,
        err_reporter: R,
    ) -> Self {
        Parser {
            file,
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

    // TODO: Add support for break statements
    // (http://www.craftinginterpreters.com/control-flow.html#challenges)
    // The syntax is a `break` keyword followed by a semicolon.
    // It should be a syntax error to have a break statement appear outside of any enclosing loop.
    // At runtime, a break statement causes execution to jump to the end of the nearest enclosing loop and proceeds from there.
    // Note that the break may be nested inside other blocks and if statements that also need to be exited.

    // declaration → fun_decl | var_decl | statement ;
    fn declaration(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Option<Stmt>, String> {
        if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::Fun => {
                    // consume the token and parse the function declaration
                    tokens.next();
                    // TODO: "function" should be an enum
                    Ok(Some(self.function(FunKind::Function, tokens)?))
                }
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

    // function → IDENTIFIER "(" parameters? ")" block ;
    fn function(&self, kind: FunKind, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        let name = self.consume_or_err(
            tokens,
            TokenKind::Identifier("".to_string()),
            &format!("expected {} name", kind),
        )?;

        self.consume_or_err(
            tokens,
            TokenKind::OpenParen,
            &format!("expected `(` after {} name", kind),
        )?;

        let mut parameters = Vec::new();

        // parse the parameters
        if let Some(token) = tokens.peek() {
            match token.kind {
                // if there are no params, skip parsing them
                TokenKind::CloseParen => (),
                // otherwise keep parsing the comma-separated params
                _ => {
                    loop {
                        let param = self.consume_or_err(
                            tokens,
                            TokenKind::Identifier("".to_string()),
                            &format!("expected parameter name"),
                        )?;

                        // only supports 8 arguments max
                        if parameters.len() >= 8 {
                            // just report the error - don't need to stop parsing & synchronize

                            // TODO: and, this should call some kind of self.error() function,
                            // which can set an error flag (this will not actually stop the parser, oops)
                            self.err_reporter.report(
                                "cannot have more than 8 parameters",
                                "this param",
                                &param.position, // TODO: add position to this
                            );
                        }

                        parameters.push(Identifier::new(param.to_string(), param.position));
                        if let Some(token) = tokens.peek() {
                            match token.kind {
                                TokenKind::Comma => {
                                    // consume the comma token and continue parsing params
                                    tokens.next();
                                }
                                // otherwise break out of the loop
                                _ => break,
                            }
                        }
                    }
                }
            }
        }

        self.consume_or_err(
            tokens,
            TokenKind::CloseParen,
            &format!("expected `)` after {} parameters", kind),
        )?;

        // parse the function body

        self.consume_or_err(
            tokens,
            TokenKind::OpenBrace,
            &format!("expected `{{` before {} body", kind),
        )?;

        let body = self.block_statement(tokens)?;

        Ok(Stmt::Function(
            Identifier::new(name.to_string(), name.position),
            parameters,
            Box::new(body),
        ))
    }

    // var_decl → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn var_declaration(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        let name = self.consume_or_err(
            tokens,
            TokenKind::Identifier("".to_string()),
            "expected variable name",
        )?;

        // assume nil unless an expression is given
        let mut initializer = Expr::Literal(name.position.clone(), Literal::Nil);

        // TODO: there should be a match() function (like in the book) to do these 3 lines
        if let Some(token) = tokens.peek() {
            if token.kind == TokenKind::Equal {
                tokens.next();
                initializer = self.expression(tokens)?;
            }
        }

        self.consume_or_err(
            tokens,
            TokenKind::Semicolon,
            "expected `;` after variable declaration",
        )?;

        Ok(Stmt::Var(
            Identifier::new(name.to_string(), name.position),
            Box::new(initializer),
        ))
    }

    // statement → expr_stmt | for_stmt | if_stmt | print_stmt | while_stmt | block ;
    fn statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Option<Stmt>, String> {
        if let Some(token) = tokens.peek() {
            match token.kind {
                // TODO: use a macro or something here, these are so similar
                TokenKind::For => {
                    // consume the token and parse the print statement
                    tokens.next();
                    Ok(Some(self.for_statement(tokens)?))
                }
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
                TokenKind::Return => {
                    // save the position of the return keyword, for err reporting
                    let ret_pos = token.position.clone();
                    // consume the token and parse the return statement
                    tokens.next();
                    Ok(Some(self.return_statement(ret_pos, tokens)?))
                }
                TokenKind::While => {
                    // consume the token and parse the while statement
                    tokens.next();
                    Ok(Some(self.while_statement(tokens)?))
                }
                TokenKind::OpenBrace => {
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

    // for_stmt → "for" "("
    //               ( var_decl | expr_stmt | ";" )
    //               expression? ";"
    //               expression?
    //            ")" statement ;
    fn for_statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        let open_paren =
            self.consume_or_err(tokens, TokenKind::OpenParen, "expected `(` after `for`")?;

        let initializer = if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::Semicolon => {
                    // consume the token, and don't create an initializer
                    tokens.next();
                    None
                }
                TokenKind::Var => self.declaration(tokens)?,
                _ => Some(self.expression_statement(tokens)?),
            }
        } else {
            // no more tokens, so no initializer
            None
        };

        let condition = if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::Semicolon => None,
                _ => Some(self.expression(tokens)?),
            }
        } else {
            // no more tokens, no condition
            None
        };
        self.consume_or_err(
            tokens,
            TokenKind::Semicolon,
            "expected `;` after loop condition",
        )?;

        let increment = if let Some(token) = tokens.peek() {
            match token.kind {
                TokenKind::CloseParen => None,
                _ => Some(self.expression(tokens)?),
            }
        } else {
            // no more tokens, no increment
            None
        };
        let close_paren = self.consume_or_err(
            tokens,
            TokenKind::CloseParen,
            "expected `)` after `for` clauses",
        )?;
        let for_pos = Position::from_positions(open_paren.position, close_paren.position);

        // optional loop body (if nothing, default to an empty block)
        let body = self.statement(tokens)?.unwrap_or(Stmt::Block(Vec::new()));

        // now build the for loop using a while loop

        let loop_body = match increment {
            // if there is an increment, it executes after the body in each loop iteration
            Some(inc) => Stmt::Block(vec![body, Stmt::Expression(Box::new(inc))]),
            None => body,
        };

        let while_body = match condition {
            // build the loop using a primitive while loop
            // if there is a condition, use it, otherwise no condition == true
            Some(cond) => Stmt::While(Box::new(cond), Box::new(loop_body)),
            None => Stmt::While(
                Box::new(Expr::Literal(for_pos, Literal::Bool(true))),
                Box::new(loop_body),
            ),
        };

        // if there is an initializer, run that before the whole loop
        let full_loop = if let Some(init) = initializer {
            Stmt::Block(vec![init, while_body])
        } else {
            // no initializer
            while_body
        };

        Ok(full_loop)
    }

    // if_stmt → "if" "(" expression ")" statement ( "else" statement )? ;
    fn if_statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        self.consume_or_err(tokens, TokenKind::OpenParen, "expected `(` after `if`")?;

        let condition = self.expression(tokens)?;

        self.consume_or_err(
            tokens,
            TokenKind::CloseParen,
            "expected `)` after if condition",
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
        self.consume_or_err(
            tokens,
            TokenKind::Semicolon,
            "expected `;` at end of statement",
        )?;
        Ok(Stmt::Print(Box::new(value)))
    }

    // return_stmt → "return" expression? ";" ;
    fn return_statement(
        &self,
        ret_pos: Position,
        tokens: &mut Peekable<Iter<Token>>,
    ) -> Result<Stmt, String> {
        let mut value = Expr::Literal(ret_pos, Literal::Nil);

        if let Some(token) = tokens.peek() {
            match token.kind {
                // if there is no return expression, leave the value as nil
                TokenKind::Semicolon => (),
                _ => {
                    // for anything else, parse the expression
                    value = self.expression(tokens)?;
                }
            }
        }

        self.consume_or_err(
            tokens,
            TokenKind::Semicolon,
            "expected `;` after return value",
        )?;

        Ok(Stmt::Return(Box::new(value)))
    }

    // while_stmt → "while" "(" expression ")" statement ;
    fn while_statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        self.consume_or_err(tokens, TokenKind::OpenParen, "expected `(` after while")?;

        let condition = self.expression(tokens)?;

        self.consume_or_err(
            tokens,
            TokenKind::CloseParen,
            "expected `)` after condition",
        )?;

        let body = match self.statement(tokens)? {
            Some(stmt) => stmt,
            None => {
                // TODO: report error instead of just returning it
                return Err("no body for `while` loop".to_string());
            }
        };

        Ok(Stmt::While(Box::new(condition), Box::new(body)))
    }

    // block → "{" declaration* "}" ;
    fn block_statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        let mut statements = Vec::new();

        // loop through the block of statements until we hit the "}"
        loop {
            if let Some(token) = tokens.peek() {
                match token.kind {
                    // exit once we find the right brace
                    TokenKind::CloseBrace => break,
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

        self.consume_or_err(tokens, TokenKind::CloseBrace, "expected `}` after block")?;

        Ok(Stmt::Block(statements))
    }

    // expr_stmt → expression ";" ;
    fn expression_statement(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, String> {
        let expr = self.expression(tokens)?;
        self.consume_or_err(
            tokens,
            TokenKind::Semicolon,
            "expected `;` at end of statement",
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
                    Expr::Variable(pos, name, _dist) => {
                        let expr_pos = Position::from_positions(pos, r_value.position());
                        return Ok(Expr::Assign(
                            expr_pos,
                            name.clone(),
                            Box::new(r_value),
                            None,
                        ));
                    }
                    _ => {
                        let report_string = self.err_reporter.report(
                            "invalid assignment target",
                            "for this assignment",
                            &equal.position,
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

    // unary → ( "!" | "-" ) unary | call ;
    fn unary(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        if let Some(operator) = Parser::match_unary_op(tokens) {
            let right = self.unary(tokens)?;
            return Ok(Expr::Unary(right.position(), operator, Box::new(right)));
        }

        // if it's not a unary, bubble up to the call syntax
        self.call(tokens)
    }

    // call → primary ( "(" arguments? ")" )* ;
    fn call(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        // call starts with a primary expression (left operand)
        let mut expr = self.primary(tokens)?;

        // handle any function calls
        loop {
            if let Some(token) = tokens.peek() {
                match token.kind {
                    TokenKind::OpenParen => {
                        // consume the open paren and finish the call
                        tokens.next();
                        expr = self.finish_call(expr, tokens)?;
                    }
                    // TODO: going to handle properties on objects
                    _ => break,
                }
            } else {
                break;
            }
        }

        // return the function calls, or just the primary expr if no function calls
        Ok(expr)
    }

    fn finish_call(
        &self,
        callee: Expr,
        tokens: &mut Peekable<Iter<Token>>,
    ) -> Result<Expr, String> {
        let mut args = Vec::new();

        if let Some(token) = tokens.peek() {
            match token.kind {
                // if there are no args, skip parsing them
                TokenKind::CloseParen => (),
                // otherwise keep parsing the comma-separated args
                _ => {
                    loop {
                        let arg = self.expression(tokens)?;
                        // only supports 8 arguments max
                        if args.len() >= 8 {
                            // just report the error - don't need to stop parsing & synchronize

                            // TODO: this should call some kind of self.error() function,
                            // which can set an error count (currently this will not actually stop after parsing, oops)
                            self.err_reporter.report(
                                "cannot have more than 8 arguments",
                                "this argument",
                                &arg.position(),
                            );
                        }
                        args.push(Box::new(arg));
                        if let Some(token) = tokens.peek() {
                            match token.kind {
                                TokenKind::Comma => {
                                    // consume the comma token and continue parsing args
                                    tokens.next();
                                }
                                // otherwise break out of the loop
                                _ => break,
                            }
                        }
                    }
                }
            }
        }

        let close_paren = self.consume_or_err(
            tokens,
            TokenKind::CloseParen,
            "expected `)` after arguments",
        )?;

        // TODO: need to construct a position from one token to another (based on their positions)
        let expr_pos = Position::from_positions(callee.position(), close_paren.position);
        Ok(Expr::Call(expr_pos, Box::new(callee), args))
    }

    // primary → NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")" | IDENTIFIER ;
    fn primary(&self, tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        if let Some(token) = tokens.next() {
            Ok(match &token.kind {
                TokenKind::False => Expr::Literal(token.position.clone(), Literal::Bool(false)),
                TokenKind::True => Expr::Literal(token.position.clone(), Literal::Bool(true)),
                TokenKind::Nil => Expr::Literal(token.position.clone(), Literal::Nil),
                TokenKind::Number(n) => Expr::Literal(token.position.clone(), Literal::Number(*n)),
                TokenKind::String(s) => {
                    Expr::Literal(token.position.clone(), Literal::String(s.to_string()))
                }
                TokenKind::OpenParen => {
                    let expr = self.expression(tokens)?;
                    self.consume_or_err(tokens, TokenKind::CloseParen, "missing closing `)`")?;
                    Expr::Grouping(expr.position(), Box::new(expr))
                }
                TokenKind::Identifier(s) => {
                    Expr::Variable(token.position.clone(), s.to_string(), None)
                }
                _ => {
                    let report_string = self.err_reporter.report(
                        &format!("expected literal or `(`, found `{}`", token),
                        &format!("unexpected"),
                        &token.position,
                    );
                    return Err(report_string);
                }
            })
        } else {
            // TODO: this shouldn't happen, so maybe don't have this case?
            let report_string = self.err_reporter.report(
                "expected literal or `(`, found nothing",
                "expected literal",
                &Position::new(self.file.clone()),
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
    ) -> Result<Token, String> {
        // position of the error for reporting
        let error_pos;

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
                    // otherwise, it's the wrong thing - use that position
                    error_pos = token.position.clone();
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
                    // otherwise, it's the wrong thing - use that position
                    error_pos = token.position.clone();
                }
            }
        } else {
            // TODO: else, no tokens, how to report the EOF position?
            error_pos = Position::new(self.file.clone());
        }

        let report_string = self.err_reporter.report(err, err, &error_pos);
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
