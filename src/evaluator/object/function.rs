use crate::evaluator::variable::variable::Environment;
use crate::parser::parser::Statement;

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub arguments: Vec<String>,
    pub body: Vec<Statement>,
}
