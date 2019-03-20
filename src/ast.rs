use std::fmt;

use crate::token::Position;

// AST for this grammar:
//
// program    → statement* EOF ;
// statement  → exprStmt | printStmt ;
// exprStmt   → expression ";" ;
// printStmt  → "print" expression ";" ;
//
// expression → literal | unary | binary | grouping ;
//
// literal    → NUMBER | STRING | "true" | "false" | "nil" ;
// grouping   → "(" expression ")" ;
// unary      → ( "-" | "!" ) expression ;
// binary     → expression operator expression ;
// operator   → "==" | "!=" | "<" | "<=" | ">" | ">=" | "+"  | "-"  | "*" | "/" ;

// for some ideas on visitor pattern in Rust, see:
// https://github.com/rust-unofficial/patterns/blob/master/patterns/visitor.md

pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &impl Visitor<T>) -> Result<T, String> {
        visitor.visit_stmt(self)
    }
}

// TODO: all of these should implement some kind of "Position" trait, so that they have a
// .position() method which gives the line, col, and length (for error reporting niceness)
pub enum Expr {
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Literal),
    Unary(UnaryOp, Box<Expr>),
}

impl Expr {
    pub fn accept<T>(&self, visitor: &impl Visitor<T>) -> Result<T, String> {
        visitor.visit_expr(self)
    }
}

// so that the matching for this can be restricted
#[derive(PartialEq)]
pub enum UnaryOp {
    Minus(Position),
    Bang(Position),
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match self {
            UnaryOp::Minus(_) => "-",
            UnaryOp::Bang(_) => "!",
        };
        write!(f, "{}", op)
    }
}

// so that the matching for this can be restricted
pub enum BinaryOp {
    BangEqual(Position),
    EqualEqual(Position),
    Greater(Position),
    GreaterEqual(Position),
    Less(Position),
    LessEqual(Position),
    Plus(Position),
    Minus(Position),
    Star(Position),
    Slash(Position),
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match self {
            BinaryOp::BangEqual(_) => "!=",
            BinaryOp::EqualEqual(_) => "==",
            BinaryOp::Greater(_) => ">",
            BinaryOp::GreaterEqual(_) => ">=",
            BinaryOp::Less(_) => "<",
            BinaryOp::LessEqual(_) => "<=",
            BinaryOp::Plus(_) => "+",
            BinaryOp::Minus(_) => "-",
            BinaryOp::Star(_) => "*",
            BinaryOp::Slash(_) => "/",
        };
        write!(f, "{}", op)
    }
}

pub enum Literal {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
}

impl Literal {
    pub fn accept<T>(&self, visitor: &impl Visitor<T>) -> Result<T, String> {
        visitor.visit_literal(self)
    }
}

pub trait Visitor<T> {
    fn visit_stmt(&self, e: &Stmt) -> Result<T, String>;
    fn visit_expr(&self, e: &Expr) -> Result<T, String>;
    fn visit_literal(&self, l: &Literal) -> Result<T, String>;
}

// actually use the visitor pattern to sort-of pretty-print the AST
pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter {}
    }

    pub fn print(&self, statements: &Vec<Stmt>) -> Result<String, String> {
        let mut ret_string = String::new();
        for s in statements {
            ret_string += &s.accept(self)?;
        }
        Ok(ret_string)
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_stmt(&self, stmt: &Stmt) -> Result<String, String> {
        Ok(match stmt {
            Stmt::Print(ref expr) => format!("print {};", expr.accept(self)?),
            Stmt::Expression(ref expr) => format!("{};", expr.accept(self)?),
        })
    }

    fn visit_expr(&self, e: &Expr) -> Result<String, String> {
        Ok(match e {
            Expr::Binary(ref expr1, token, ref expr2) => format!(
                "({} {} {})",
                expr1.accept(self)?,
                token,
                expr2.accept(self)?
            ),
            Expr::Grouping(ref expr) => format!("({})", expr.accept(self)?),
            Expr::Literal(lit) => lit.accept(self)?,
            Expr::Unary(op, ref expr) => format!("({} {})", op, expr.accept(self)?),
        })
    }

    fn visit_literal(&self, l: &Literal) -> Result<String, String> {
        Ok(match l {
            Literal::Bool(b) => b.to_string(),
            Literal::Nil => "nil".to_string(),
            Literal::Number(num) => format!("{}", num),
            Literal::String(s) => s.clone(),
        })
    }
}
