
use std::fmt::Display;
use text_io::read;

use crate::{*, lexer::tokens::Literal, parser::expression::*};

use super::{Interpreter, value::Value};

fn unwrap_or_error(res: std::result::Result<Value, String>, pos: SourcePos) -> Result<Value> {
	match res {
		Ok(v) => Ok(v),
		Err(e) => Error::create(e, pos),
	}
}

impl Interpreter {

	pub fn evaluate(&mut self, expr: Box<Expression>) -> Result<Value> {
		match expr.expr_type {
			ExpressionType::Literal { value } => self.evaluate_literal(value),
			ExpressionType::VariableReference { name } => self.evaluate_variable_reference(name, expr.pos),
			ExpressionType::Group { expr } => self.evaluate(expr),
			ExpressionType::BinaryOperation { op, left_expr, right_expr } => self.evaluate_bin_operation(op, left_expr, right_expr),
			ExpressionType::Read => self.evaluate_read(),
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

	fn evaluate_bin_operation(&mut self, op: BinaryOperator, left_expr: Box<Expression>, right_expr: Box<Expression>) -> Result<Value> {
		let lhs = self.evaluate(left_expr.clone())?;
		let rhs = self.evaluate(right_expr.clone())?;
		let pos = right_expr.pos;
		match op {
			BinaryOperator::Add => unwrap_or_error(lhs + rhs, pos),
			BinaryOperator::Sub => unwrap_or_error(lhs - rhs, pos),
			BinaryOperator::Mul => unwrap_or_error(lhs * rhs, pos),
			BinaryOperator::Div => unwrap_or_error(lhs / rhs, pos),
			BinaryOperator::Mod => unwrap_or_error(lhs % rhs, pos),
			BinaryOperator::Equals    => Ok(Value::Bool(lhs == rhs)),
			BinaryOperator::NotEquals => Ok(Value::Bool(lhs != rhs)),
			BinaryOperator::Greater     => Ok(Value::Bool(lhs > rhs)),
			BinaryOperator::GreaterOrEq => Ok(Value::Bool(lhs >= rhs)),
			BinaryOperator::Lesser      => Ok(Value::Bool(lhs < rhs)),
			BinaryOperator::LesserOrEq  => Ok(Value::Bool(lhs <= rhs)),
			_ => return Error::create(format!("Invalid operation"), right_expr.pos),
		}
	}

	fn evaluate_read(&mut self) -> Result<Value> {
		let line: String = read!("{}\r\n"); // I believe this won't work on other platforms
		Ok(Value::Str(line))
	}

}