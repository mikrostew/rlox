use std::fmt;

use crate::ast::{BinaryOp, Expr, Literal, UnaryOp, Visitor};

// because lox is dynamically typed, we need something to store any possible types
#[derive(PartialEq)]
enum Object {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
}

impl Object {
    pub fn is_truthy(self) -> bool {
        match self {
            // like Ruby, only nil and false are falsey
            Object::Bool(b) => b,
            Object::Nil => false,
            // anything else is truthy
            _ => true,
        }
    }

    pub fn as_number(self) -> Result<f64, String> {
        match self {
            Object::Number(n) => Ok(n),
            _ => Err("Can't convert this stuff to a number".to_string()),
        }
    }

    pub fn as_string(self) -> Result<String, String> {
        match self {
            Object::String(s) => Ok(s),
            _ => Err("Can't convert this stuff to a string".to_string()),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Bool(b) => write!(f, "{}", b),
            Object::Nil => write!(f, "nil"),
            Object::Number(n) => write!(f, "{}", n),
            Object::String(s) => write!(f, "{}", s),
        }
    }
}

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    pub fn interpret(&self, expr: Expr) -> Result<(), String> {
        let value = self.evaluate(&Box::new(expr))?;
        println!("{}", value);
        Ok(())
    }

    fn evaluate(&self, expr: &Box<Expr>) -> Result<Object, String> {
        expr.accept(self)
    }

    fn error(&self, error_msg: &str) -> Result<String, String> {
        println!("Runtime Error: {}", error_msg);
        // TODO: actually report the error
        Err(error_msg.to_string())
    }
}

fn add_or_concat(left: Object, right: Object) -> Result<Object, String> {
    // decide based on the left-hand operand
    Ok(match left {
        Object::Number(n) => Object::Number(n + right.as_number()?),
        Object::String(s) => Object::String(s + &right.as_string()?),
        _ => return Err("can't add or concat this stuff".to_string()),
    })
}

impl Visitor<Object> for Interpreter {
    fn visit_expr(&self, e: &Expr) -> Result<Object, String> {
        match e {
            Expr::Binary(ref expr1, op, ref expr2) => {
                let left = self.evaluate(expr1)?;
                let right = self.evaluate(expr2)?;

                Ok(match op {
                    // equality
                    BinaryOp::BangEqual(_) => Object::Bool(left != right),
                    BinaryOp::EqualEqual(_) => Object::Bool(left == right),
                    // comparison
                    BinaryOp::Greater(_) => Object::Bool(left.as_number()? > right.as_number()?),
                    BinaryOp::GreaterEqual(_) => {
                        Object::Bool(left.as_number()? >= right.as_number()?)
                    }
                    BinaryOp::Less(_) => Object::Bool(left.as_number()? < right.as_number()?),
                    BinaryOp::LessEqual(_) => Object::Bool(left.as_number()? <= right.as_number()?),
                    // arithmetic or concat
                    BinaryOp::Plus(_) => add_or_concat(left, right)?,
                    // arithmetic
                    BinaryOp::Minus(_) => Object::Number(left.as_number()? - right.as_number()?),
                    BinaryOp::Star(_) => Object::Number(left.as_number()? * right.as_number()?),
                    BinaryOp::Slash(_) => Object::Number(left.as_number()? / right.as_number()?),
                })
            }
            // for a group, evaluate the inner expression
            Expr::Grouping(ref expr) => self.evaluate(expr),
            // for a literal, visit the literal
            Expr::Literal(lit) => lit.accept(self),
            Expr::Unary(op, ref expr) => {
                let right = self.evaluate(expr)?;
                Ok(match op {
                    UnaryOp::Minus(_) => Object::Number(-right.as_number()?),
                    UnaryOp::Bang(_) => Object::Bool(!right.is_truthy()),
                })
            }
        }
    }

    fn visit_literal(&self, l: &Literal) -> Result<Object, String> {
        Ok(match l {
            // just convert the literal to an object
            Literal::Bool(b) => Object::Bool(*b),
            Literal::Nil => Object::Nil,
            Literal::Number(n) => Object::Number(*n),
            Literal::String(s) => Object::String(s.to_string()),
        })
    }
}
