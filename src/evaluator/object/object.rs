pub use crate::evaluator::object::integer::Integer;
#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(Integer),
}
