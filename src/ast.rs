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
    Return(Box<Expr>),
    Var(String, Box<Expr>),      // name, initializer
    While(Box<Expr>, Box<Stmt>), // condition, body
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign(Position, String, Box<Expr>, Option<usize>), // pos, name, value, resolved var distance
    Binary(Position, Box<Expr>, BinaryOp, Box<Expr>),
    Call(Position, Box<Expr>, Vec<Box<Expr>>), // pos, callee (thing being called), arguments
    Grouping(Position, Box<Expr>),
    Literal(Position, Literal),
    Logical(Position, Box<Expr>, LogicalOp, Box<Expr>),
    Unary(Position, UnaryOp, Box<Expr>),
    Variable(Position, String, Option<usize>), // pos, name, resolved var distance
}

impl Expr {
    pub fn position(&self) -> Position {
        match self {
            Expr::Assign(p, _, _, _) => p,
            Expr::Binary(p, _, _, _) => p,
            Expr::Call(p, _, _) => p,
            Expr::Grouping(p, _) => p,
            Expr::Literal(p, _) => p,
            Expr::Logical(p, _, _, _) => p,
            Expr::Unary(p, _, _) => p,
            Expr::Variable(p, _, _) => p,
        }
        .clone()
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

pub trait Visitor<T> {
    // to support errors or return statements that unwind the call stack
    type Error;

    fn visit_stmt(&mut self, s: &Stmt, env: &Rc<Environment>) -> Result<T, Self::Error>;
    fn visit_expr(&mut self, e: &Expr, env: &Rc<Environment>) -> Result<T, Self::Error>;
    fn visit_literal(&self, l: &Literal, env: &Rc<Environment>) -> Result<T, Self::Error>;
}

// for things that need to modify the AST when visiting
pub trait VisitorMut<T> {
    // to support errors or return statements that unwind the call stack
    type Error;

    fn visit_stmt(&mut self, s: &mut Stmt, env: &Rc<Environment>) -> Result<T, Self::Error>;
    fn visit_expr(&mut self, e: &mut Expr, env: &Rc<Environment>) -> Result<T, Self::Error>;
    fn visit_literal(&self, l: &Literal, env: &Rc<Environment>) -> Result<T, Self::Error>;
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
            ret_string += &self.visit_stmt(s, &environment)?;
        }
        Ok(ret_string)
    }
}

impl Visitor<String> for AstPrinter {
    type Error = String;

    fn visit_stmt(&mut self, s: &Stmt, env: &Rc<Environment>) -> Result<String, String> {
        Ok(match s {
            Stmt::Block(statements) => {
                if statements.len() == 0 {
                    "{}".to_string()
                } else {
                    let mut formatted_stmts = String::new();
                    for statement in statements {
                        formatted_stmts.push_str(&self.visit_stmt(statement, env)?);
                    }
                    format!("{{\n{}}}\n", formatted_stmts)
                }
            }
            Stmt::Expression(ref expr) => format!("{};\n", self.visit_expr(expr, env)?),
            Stmt::Function(name, params, body) => {
                let formatted_params = params.join(", ");
                let formatted_body = self.visit_stmt(body, env)?;

                format!("<fn {}>({}) {}", name, formatted_params, formatted_body)
            }
            Stmt::If(ref if_expr, ref then_stmt, ref opt_else_stmt) => {
                let mut if_stmt = String::new();
                if_stmt.push_str("if ");
                if_stmt.push_str(&self.visit_expr(if_expr, env)?);
                if_stmt.push_str("\nthen:\n");
                if_stmt.push_str(&self.visit_stmt(then_stmt, env)?);
                if let Some(ref else_stmt) = opt_else_stmt {
                    if_stmt.push_str("else:\n");
                    if_stmt.push_str(&self.visit_stmt(else_stmt, env)?);
                } else {
                    if_stmt.push_str("(no else)\n");
                }
                if_stmt
            }
            Stmt::Print(ref expr) => format!("print {};\n", self.visit_expr(expr, env)?),
            Stmt::Return(ref expr) => format!("return {};\n", self.visit_expr(expr, env)?),
            Stmt::Var(name, ref expr) => {
                format!("var {} = {};\n", name, self.visit_expr(expr, env)?)
            }
            Stmt::While(ref condition, ref body) => format!(
                "while ( {} ) {}",
                self.visit_expr(condition, env)?,
                self.visit_stmt(body, env)?
            ),
        })
    }

    fn visit_expr(&mut self, e: &Expr, env: &Rc<Environment>) -> Result<String, String> {
        Ok(match e {
            Expr::Assign(_pos, var_name, ref expr, opt_dist) => format!(
                "{} <dist {:?}> = {}",
                var_name,
                opt_dist,
                self.visit_expr(expr, env)?
            ),
            Expr::Binary(_pos, ref expr1, token, ref expr2) => format!(
                "({} {} {})",
                self.visit_expr(expr1, env)?,
                token,
                self.visit_expr(expr2, env)?
            ),
            Expr::Call(_pos, ref callee_expr, args) => {
                // TODO: join a vec of strings with commas
                let arg_string = args
                    .into_iter()
                    .map(|a| match self.visit_expr(a, env) {
                        Ok(s) => s,
                        Err(e) => format!("Error: {}", e),
                    })
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{}({})", self.visit_expr(callee_expr, env)?, arg_string)
            }
            Expr::Grouping(_pos, ref expr) => format!("({})", self.visit_expr(expr, env)?),
            Expr::Literal(_pos, lit) => self.visit_literal(lit, env)?,
            Expr::Logical(_pos, ref expr1, op, ref expr2) => format!(
                "({} {} {})",
                self.visit_expr(expr1, env)?,
                op,
                self.visit_expr(expr2, env)?
            ),
            Expr::Unary(_pos, op, ref expr) => format!("({} {})", op, self.visit_expr(expr, env)?),
            Expr::Variable(_pos, name, opt_dist) => format!("{}<dist {:?}>", name, opt_dist),
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
