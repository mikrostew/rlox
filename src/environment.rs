use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::error::LoxErr;
use crate::interpreter::Object;

#[derive(PartialEq)]
pub struct Environment {
    // any values defined locally in this environment
    values: RefCell<HashMap<String, Object>>,
    // the Environment enclosing this one (for shadowed vars)
    enclosing: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<Environment>>) -> Rc<Self> {
        Rc::new(Environment {
            values: RefCell::new(HashMap::new()),
            enclosing,
        })
    }

    pub fn assign(&self, name: &String, value: Object) -> Result<(), LoxErr> {
        if self.values.borrow().contains_key(name) {
            self.values.borrow_mut().insert(name.to_string(), value);
            Ok(())
        } else {
            match self.enclosing {
                Some(ref rc_env) => rc_env.assign(name, value),
                None => Err(LoxErr::Error(format!("Undefined variable `{}`", name))),
            }
        }
    }

    pub fn define(&self, name: &String, value: Object) {
        self.values.borrow_mut().insert(name.to_string(), value);
    }

    pub fn get(&self, name: &String) -> Result<Object, LoxErr> {
        match self.values.borrow().get(&name.to_string()) {
            // if it exists locally, return that
            Some(value) => Ok(value.clone()),
            None => {
                // otherwise check the enclosing environment for shadowed var
                match &self.enclosing {
                    Some(env) => env.get(name),
                    None => Err(LoxErr::Error(format!("Undefined variable `{}`", name))),
                }
            }
        }
    }

    // get the var in the enclosing env which is the input distance from this one
    pub fn get_at(&self, dist: &usize, name: &str) -> Result<Object, LoxErr> {
        // the book uses an ancestor() method, but I can't figure out how to make that work,
        // (the borrow checker has defeated me)
        // so whatever, just doing it with more duplication of code like so
        let mut env = self;
        for _i in 0..*dist {
            match env.enclosing {
                Some(ref e) => {
                    env = e;
                }
                None => {
                    return Err(LoxErr::Error(
                        "oh no! error in static analysis!!".to_string(),
                    ));
                }
            }
        }
        env.get(&name.to_string())
    }

    // assign the var in the enclosing env which is the input distance from this one
    pub fn assign_at(&self, dist: &usize, name: &str, value: Object) -> Result<(), LoxErr> {
        // (same note as get_at())
        let mut env = self;
        for _i in 0..*dist {
            match env.enclosing {
                Some(ref e) => {
                    env = e;
                }
                None => {
                    return Err(LoxErr::Error(
                        "oh no! error in static analysis!!".to_string(),
                    ));
                }
            }
        }
        env.assign(&name.to_string(), value)
    }
}
