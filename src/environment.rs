use std::collections::HashMap;

use crate::interpreter::Object;

pub struct Environment {
    values: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &String, value: Object) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &String) -> Result<Object, String> {
        self.values
            .get(&name.to_string())
            .cloned()
            .ok_or(format!("Undefined variable `{}`", name))
    }
}
