pub use crate::evaluator::object::object::Object;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum VariableValue {
    Object(Object),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    // * mutableかどうかなどはvalueの方でもつ
    variables: HashMap<String, VariableValue>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn get(&self, identifier: String) -> &VariableValue {
        if self.variables.contains_key(&identifier) {
            return self.variables.get(&identifier).unwrap();
        };

        let res = match &self.outer {
            Some(env) => env.get(identifier),
            None => panic!("{} is not defined", identifier),
        };

        res
    }

    pub fn set(&mut self, identifier: String, value: VariableValue) -> &VariableValue {
        self.variables.insert(identifier.clone(), value);
        self.variables.get(&identifier).unwrap()
    }

    // TODO wasted memory
    pub fn extend(&self) -> Environment {
        Environment {
            variables: HashMap::new(),
            // TODO waste memory
            outer: Some(Box::new(self.clone())),
        }
    }

    pub fn new() -> Environment {
        Environment {
            variables: HashMap::new(),
            outer: None,
        }
    }
}
