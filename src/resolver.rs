use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{Expr, Identifier, Literal, Stmt, VisitorMut};
use crate::environment::Environment;
use crate::error::Reporter;
use crate::token::Position;

#[derive(Clone, PartialEq)]
pub enum FunctionKind {
    None,
    Function,
    // TODO: add more kinds supposedly...
}

pub struct Resolver {
    // track what things are currently in scope, for local block scopes
    // (global scope is not tracked)
    scopes: Vec<HashMap<String, bool>>,
    // track if we are currently in a function, and if so what kind
    current_fn: FunctionKind,
    // for reporting errors found during this stage
    err_reporter: Box<Reporter>,
    // keep track of errors encountered
    num_errors: u64,
}

impl Resolver {
    pub fn new<R: Reporter + 'static>(err_reporter: R) -> Self {
        Resolver {
            scopes: Vec::new(),
            // start out at the top level
            current_fn: FunctionKind::None,
            err_reporter: Box::new(err_reporter),
            num_errors: 0,
        }
    }

    pub fn resolve(&mut self, statements: &mut Vec<Stmt>) -> Result<(), String> {
        let environment = Environment::new(None);

        for s in statements {
            // visit all the statements, and catch any errors
            match self.visit_stmt(s, &environment) {
                Ok(_) => (),
                Err(_) => {
                    self.num_errors += 1;
                }
            }
        }

        if self.num_errors > 0 {
            Err(format!("resolver encountered {} error(s)", self.num_errors))
        } else {
            Ok(())
        }
    }

    // report an error
    pub fn error(&mut self, pos: Position, msg: &str) {
        self.err_reporter.report(msg, "here", &pos);
        self.num_errors += 1;
    }

    // start a new scope
    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    // exit the current scope
    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    // declare a variable in the current scope
    pub fn declare(&mut self, ident: &Identifier) {
        // try to access the top element of the stack
        match self.scopes.last_mut() {
            // if empty, do nothing (don't worry about global vars)
            None => (),
            Some(scope) => {
                // check if this has already been declared
                if scope.contains_key(&ident.name.to_string()) {
                    // report the error, but don't return it
                    self.error(
                        ident.pos.clone(),
                        &format!("variable `{}` re-declared in local scope", ident.name),
                    );
                } else {
                    // mark that the var exists, but is not yet initialized
                    scope.insert(ident.name.to_string(), false);
                }
            }
        }
    }

    // define a variable in the current scope
    pub fn define(&mut self, ident: &Identifier) {
        // try to access the top element of the stack
        match self.scopes.last_mut() {
            // if empty, do nothing (don't worry about global vars)
            None => (),
            Some(scope) => {
                // mark that the var exists, and is now initialized
                scope.insert(ident.name.to_string(), true);
            }
        }
    }

    // figure out where the var will resolve, and
    // store that in the interpreter
    pub fn resolve_local(&mut self, name: &str, resolved_dist: &mut Option<usize>) {
        // start at the innermost scope and work outwards
        for (dist, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                // NOTE:
                // For the book this info is stored in a HashMap in the interpreter,
                // like HashMap<Expr, u64>,
                // which I tried, but then `Eq` and `Hash` have to be derived for all kinds
                // of things, and `f64` doesn't implement `Eq`, and I don't want to manually
                // implement it, not to mention `Hash` (which I didn't try).
                //
                // So, where should I store this info?
                // From the book: "One obvious place is right in the syntax tree node itself."
                // (the book does not take that approach, because "it would require mucking
                // around with our syntax tree generator")
                //
                // I'm not using their generator anyway, so that's where I'm going to store
                // this info - in the AST node itself.
                *resolved_dist = Some(dist);
                return;
            }
        }
        // not found, assume it's global
    }

    pub fn resolve_function(
        &mut self,
        params: &Vec<Identifier>,
        body: &mut Stmt,
        env: &Rc<Environment>,
        kind: FunctionKind,
    ) -> Result<(), String> {
        // use the call stack to save the enclosing function kind,
        // then set the current one
        let enclosing_fn = self.current_fn.clone();
        self.current_fn = kind;

        // create a new scope for the function body
        self.begin_scope();
        // bind vars for each of the function parameters
        for param in params {
            self.declare(param);
            self.define(param);
        }
        self.visit_stmt(body, env)?;
        self.end_scope();
        // back to whatever function may be enclosing this one
        self.current_fn = enclosing_fn;
        Ok(())
    }
}

// mut because the resolver needs to modify Expr with resolved distance
impl VisitorMut<()> for Resolver {
    type Error = String;

    fn visit_stmt(&mut self, s: &mut Stmt, env: &Rc<Environment>) -> Result<(), String> {
        match s {
            Stmt::Block(statements) => {
                // blocks create the local scopes for statements
                self.begin_scope();
                for stmt in statements {
                    // just have to resolve each statement in turn
                    self.visit_stmt(stmt, env)?;
                }
                self.end_scope();
            }
            Stmt::Expression(ref mut expr) => {
                self.visit_expr(expr, env)?; // resolve the parts
            }
            Stmt::Function(name, params, ref mut body) => {
                // functions bind var names and create a local scope
                // first, handle the binding of the function name
                // (eagerly, so the function can recursively refer to itself)
                self.declare(name);
                self.define(name);

                // then handle the function body
                self.resolve_function(params, body, env, FunctionKind::Function)?;
            }
            Stmt::If(ref mut if_expr, ref mut then_stmt, ref mut opt_else_stmt) => {
                // resolve the condition and both branches
                self.visit_expr(if_expr, env)?;
                self.visit_stmt(then_stmt, env)?;
                if let Some(s) = opt_else_stmt {
                    self.visit_stmt(s, env)?;
                }
            }
            Stmt::Print(ref mut expr) => {
                self.visit_expr(expr, env)?; // resolve the parts
            }
            Stmt::Return(ref mut expr) => {
                // check that we are actually in a function
                // TODO: this should probably use the position of the Stmt
                // (BUT, there is not Position for Stmt, so have to implement that...)
                if self.current_fn == FunctionKind::None {
                    self.error(expr.position().clone(), "cannot return from top-level code");
                }
                self.visit_expr(expr, env)?; // resolve the parts
            }
            Stmt::Var(name, ref mut expr) => {
                // this adds a new entry to the innermost scope
                // variable binding is split into 2 steps - declaring and defining
                self.declare(name);
                self.visit_expr(expr, env)?;
                self.define(name);
            }
            Stmt::While(ref mut condition_expr, ref mut body) => {
                // resolve the condition and body
                self.visit_expr(condition_expr, env)?;
                self.visit_stmt(body, env)?;
            }
        }
        Ok(())
    }
    fn visit_expr(&mut self, e: &mut Expr, env: &Rc<Environment>) -> Result<(), String> {
        match e {
            Expr::Assign(_pos, var_name, ref mut expr, ref mut resolved_vars) => {
                // resolve the expr first in case it also contains other vars
                self.visit_expr(expr, env)?;
                // then resolve the var being assigned to
                self.resolve_local(var_name, resolved_vars);
            }
            Expr::Binary(_pos, ref mut expr1, _op, ref mut expr2) => {
                // resolve both operands
                self.visit_expr(expr1, env)?;
                self.visit_expr(expr2, env)?;
            }
            Expr::Call(_pos, ref mut callee_expr, args) => {
                // resolve the thing being called
                self.visit_expr(callee_expr, env)?;
                // then walk the arg list and resolve those
                for arg in args {
                    self.visit_expr(arg, env)?;
                }
            }
            Expr::Grouping(_pos, ref mut expr) => {
                self.visit_expr(expr, env)?; // resolve the parts
            }
            Expr::Literal(_pos, _lit) => {
                // nothing to do - literals don't mention vars, and don't have subexpressions
            }
            Expr::Logical(_pos, ref mut expr1, _op, ref mut expr2) => {
                // resolve body operands
                self.visit_expr(expr1, env)?;
                self.visit_expr(expr2, env)?;
            }
            Expr::Unary(_pos, _op, ref mut expr) => {
                self.visit_expr(expr, env)?; // resolve the operand
            }
            Expr::Variable(pos, name, ref mut resolved_vars) => {
                // have to check the scope maps to resolve var expressions
                match self.scopes.last() {
                    None => (),
                    Some(scope) => {
                        // check if the var is referring to itself in its initializer
                        if scope.get(name) == Some(&false) {
                            self.error(pos.clone(), "cannot read local var in its initializer");
                        }
                    }
                }
                // actually resolve the var
                self.resolve_local(name, resolved_vars);
            }
        }
        Ok(())
    }
    fn visit_literal(&self, _l: &Literal, _env: &Rc<Environment>) -> Result<(), String> {
        // nothing to do for these - not going to actually call the visit method above
        Ok(())
    }
}
