use std::collections::HashMap;

use crate::interpreter::Object;

pub struct Environment {
    // any values defined locally in this environment
    values: HashMap<String, Object>,
    // the Environment enclosing this one (for shadowed vars)
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn enclosed_by(mut self, enclosing: Environment) -> Self {
        self.enclosing = Some(Box::new(enclosing));
        self
    }

    pub fn assign(&mut self, name: &String, value: Object) -> Result<(), String> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else {
            match self.enclosing {
                Some(ref mut env) => env.assign(name, value),
                None => Err(format!("Undefined variable `{}`", name)),
            }
        }
    }

    pub fn define(&mut self, name: &String, value: Object) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &String) -> Result<Object, String> {
        match self.values.get(&name.to_string()) {
            // if it exists locally, return that
            Some(value) => Ok(value.clone()),
            None => {
                // otherwise check the enclosing environment for shadowed var
                match &self.enclosing {
                    Some(env) => env.get(name),
                    None => Err(format!("Undefined variable `{}`", name)),
                }
            }
        }
    }
}
