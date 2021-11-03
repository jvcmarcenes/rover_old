
use crate::{Result, parser::{block::Block, expression::{Expression, ExpressionType, Literal}, statement::{Statement, StatementType}}};

use super::*;

#[derive(Debug, Clone)]
pub struct Function {
	pub params: Vec<String>,
	pub block: Block,
}

impl PartialEq for Function {
	fn eq(&self, _: &Self) -> bool { false }
}

impl Function {
	pub fn new(params: Vec<String>, block: Block) -> Self { Self { params, block } }
	pub fn return_static(lit: Literal, pos: SourcePos) -> Self {
		Self::new(
			Vec::new(),
			Block::new(vec![Statement::new(StatementType::Return { expr: Box::new(Expression::new(ExpressionType::ValueLiteral { value: lit }, pos)) }, pos)])
		)
	}
}

impl Into<Value> for Function {
	fn into(self) -> Value {
		Value::new(ValueData::Function(self))
	}
}

impl Value {
	pub fn to_function(&mut self, pos: SourcePos) -> Result<Function> {
		if let ValueData::Function(f) = self.value.clone() { Ok(f.to_owned()) }
		else { Error::create("Expected a function".to_string(), pos) }
	}
}
