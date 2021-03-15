pub use crate::evaluator::object::object::Object;
use std::collections::HashMap;

pub enum VariableValue {
    Object(Object),
}
