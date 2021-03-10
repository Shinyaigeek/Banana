pub use crate::evaluator::object::integer::Integer;
#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(Integer),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(int) => int.value.to_string(),
            _ => panic!(""),
        }
    }
}
