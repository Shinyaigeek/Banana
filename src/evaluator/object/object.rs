pub use crate::evaluator::object::bool::Bool;
pub use crate::evaluator::object::integer::Integer;
#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(Integer),
    Bool(Bool),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(int) => int.value.to_string(),
            Object::Bool(bool) => bool.value.to_string(),
            _ => panic!(""),
        }
    }
}
