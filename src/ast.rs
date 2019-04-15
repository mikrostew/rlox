use std::fmt;
use std::rc::Rc;

use crate::environment::Environment;
use crate::token::Position;

// AST for the lox language

// for some ideas on visitor pattern in Rust, see:
// https://github.com/rust-unofficial/patterns/blob/master/patterns/visitor.md

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Expression(Box<Expr>),
    Function(String, Vec<String>, Box<Stmt>), // name, params, body
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>), // if expr then stmt (else stmt)?
    Print(Box<Expr>),
    Var(String, Box<Expr>),      // name, initializer
    While(Box<Expr>, Box<Stmt>), // condition, body
}

impl Stmt {
    pub fn accept<T>(
        &self,
        visitor: &mut impl Visitor<T>,
        env: &Rc<Environment>,
    ) -> Result<T, String> {
        visitor.visit_stmt(self, env)
    }
}

// TODO: all of these should implement some kind of "Position" trait, so that they have a
// .position() method which gives the line, col, and length (for error reporting niceness)
// (so, maybe these should be structs...)
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign(String, Box<Expr>), // name, value
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Call(Box<Expr>, Vec<Box<Expr>>), // callee (thing being called), arguments
    Grouping(Box<Expr>),
    Literal(Literal),
    Logical(Box<Expr>, LogicalOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Variable(String), // name
}

impl Expr {
    pub fn accept<T>(
        &self,
        visitor: &mut impl Visitor<T>,
        env: &Rc<Environment>,
    ) -> Result<T, String> {
        visitor.visit_expr(self, env)
    }
}

// so that the matching for this can be restricted
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum LogicalOp {
    And(Position),
    Or(Position),
}

impl fmt::Display for LogicalOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match self {
            LogicalOp::And(_) => "&&",
            LogicalOp::Or(_) => "||",
        };
        write!(f, "{}", op)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
}

impl Literal {
    pub fn accept<T>(&self, visitor: &impl Visitor<T>, env: &Rc<Environment>) -> Result<T, String> {
        visitor.visit_literal(self, env)
    }
}

pub trait Visitor<T> {
    fn visit_stmt(&mut self, e: &Stmt, env: &Rc<Environment>) -> Result<T, String>;
    fn visit_expr(&mut self, e: &Expr, env: &Rc<Environment>) -> Result<T, String>;
    fn visit_literal(&self, l: &Literal, env: &Rc<Environment>) -> Result<T, String>;
}

// actually use the visitor pattern to sort-of pretty-print the AST
pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter {}
    }

    pub fn print(&mut self, statements: &Vec<Stmt>) -> Result<String, String> {
        let mut ret_string = String::new();
        let environment = Environment::new(None);
        for s in statements {
            ret_string += &s.accept(self, &environment)?;
        }
        Ok(ret_string)
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_stmt(&mut self, stmt: &Stmt, env: &Rc<Environment>) -> Result<String, String> {
        Ok(match stmt {
            Stmt::Block(statements) => {
                if statements.len() == 0 {
                    "{}".to_string()
                } else {
                    let mut formatted_stmts = String::new();
                    for statement in statements {
                        formatted_stmts.push_str(&statement.accept(self, env)?);
                    }
                    format!("{{\n{}}}\n", formatted_stmts)
                }
            }
            Stmt::Expression(ref expr) => format!("{};\n", expr.accept(self, env)?),
            Stmt::Function(name, params, body) => {
                let formatted_params = params.join(", ");
                let formatted_body = body.accept(self, env)?;

                format!("<fn {}>({}) {}", name, formatted_params, formatted_body)
            }
            Stmt::If(ref if_expr, ref then_stmt, ref opt_else_stmt) => {
                let mut if_stmt = String::new();
                if_stmt.push_str("if ");
                if_stmt.push_str(&if_expr.accept(self, env)?);
                if_stmt.push_str("\nthen:\n");
                if_stmt.push_str(&then_stmt.accept(self, env)?);
                if let Some(ref else_stmt) = opt_else_stmt {
                    if_stmt.push_str("else:\n");
                    if_stmt.push_str(&else_stmt.accept(self, env)?);
                } else {
                    if_stmt.push_str("(no else)\n");
                }
                if_stmt
            }
            Stmt::Print(ref expr) => format!("print {};\n", expr.accept(self, env)?),
            Stmt::Var(name, ref expr) => format!("var {} = {};\n", name, expr.accept(self, env)?),
            Stmt::While(ref condition, ref body) => format!(
                "while ( {} ) {}",
                condition.accept(self, env)?,
                body.accept(self, env)?
            ),
        })
    }

    fn visit_expr(&mut self, e: &Expr, env: &Rc<Environment>) -> Result<String, String> {
        Ok(match e {
            Expr::Assign(var_name, ref expr) => {
                format!("{} = {}", var_name, expr.accept(self, env)?,)
            }
            Expr::Binary(ref expr1, token, ref expr2) => format!(
                "({} {} {})",
                expr1.accept(self, env)?,
                token,
                expr2.accept(self, env)?
            ),
            Expr::Call(ref callee_expr, args) => {
                // TODO: join a vec of strings with commas
                let arg_string = args
                    .into_iter()
                    .map(|a| match a.accept(self, env) {
                        Ok(s) => s,
                        Err(e) => format!("Error: {}", e),
                    })
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{}({})", callee_expr.accept(self, env)?, arg_string)
            }
            Expr::Grouping(ref expr) => format!("({})", expr.accept(self, env)?),
            Expr::Literal(lit) => lit.accept(self, env)?,
            Expr::Logical(ref expr1, op, ref expr2) => format!(
                "({} {} {})",
                expr1.accept(self, env)?,
                op,
                expr2.accept(self, env)?
            ),
            Expr::Unary(op, ref expr) => format!("({} {})", op, expr.accept(self, env)?),
            Expr::Variable(name) => format!("{}", name),
        })
    }

    fn visit_literal(&self, l: &Literal, _env: &Rc<Environment>) -> Result<String, String> {
        Ok(match l {
            Literal::Bool(b) => b.to_string(),
            Literal::Nil => "nil".to_string(),
            Literal::Number(num) => format!("{}", num),
            Literal::String(s) => format!("\"{}\"", s),
        })
    }
}
