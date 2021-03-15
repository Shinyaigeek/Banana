pub use crate::evaluator::object::bool::Bool;
pub use crate::evaluator::object::integer::Integer;
pub use crate::evaluator::object::null::Null;
#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(Integer),
    Bool(Bool),
    Null(Null),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(int) => int.value.to_string(),
            Object::Bool(bool) => bool.value.to_string(),
            Object::Null(null) => "null;".to_string(),
            _ => panic!(""),
        }
    }
}
