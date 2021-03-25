pub use crate::evaluator::object::bool::Bool;
pub use crate::evaluator::object::float::Float;
pub use crate::evaluator::object::function::Function;
pub use crate::evaluator::object::integer::Integer;
pub use crate::evaluator::object::null::Null;
#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(Integer),
    Float(Float),
    Bool(Bool),
    Null(Null),
    Function(Function),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(int) => int.value.to_string(),
            Object::Float(float) => float.value.to_string(),
            Object::Bool(bool) => bool.value.to_string(),
            Object::Null(null) => "null;".to_string(),
            Object::Function(function) => "[object object]".to_string(),
            _ => panic!(""),
        }
    }
}
