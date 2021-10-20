
use std::fmt::Display;

use crate::{*, lexer::tokens::Literal, parser::expression::*};

use super::Interpreter;


#[derive(Debug, PartialEq, Clone)]
pub enum Value {
	Str(String),
	Num(f32),
	Bool(bool),
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Str(s) => write!(f, "{}", s),
			Value::Num(n) => write!(f, "{}", n),
			Value::Bool(b) => write!(f, "{}", b),
		}
	}
}

impl Interpreter {

	pub fn evaluate(&mut self, expr: Expression) -> Result<Value> {
		match expr.expr_type {
			ExpressionType::Literal { value } => self.evaluate_literal(value),
			ExpressionType::VariableReference { name } => self.evaluate_variable_reference(name, expr.pos),
			_ => Error::create(String::from("Error trying to parse expression"), expr.pos)
		}
	}

	fn evaluate_literal(&mut self, lit: Literal) -> Result<Value> {
		let v = match lit {
			Literal::Str(s) => Value::Str(s),
			Literal::Num(n) => Value::Num(n),
			Literal::Bool(b) => Value::Bool(b),
		};
		Ok(v)
	}

	fn evaluate_variable_reference(&mut self, name: String, pos: SourcePos) -> Result<Value> {
		match self.symbol_table.get(&name) {
			Some(v) => Ok(v.clone()),
			None => Error::create(format!("variable {} is not defined", name), pos),
		}
	}

}