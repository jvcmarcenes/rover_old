
use std::fmt::Display;
use text_io::read;

use crate::{*, lexer::tokens::Literal, parser::expression::*};

use super::{Interpreter, value::Value};

impl Interpreter {

	pub fn evaluate(&mut self, expr: Box<Expression>) -> Result<Value> {
		match expr.expr_type {
			ExpressionType::BinaryOperation { op, left_expr, right_expr } => self.evaluate_bin_operation(op, left_expr, right_expr),
			ExpressionType::Literal { value } => self.evaluate_literal(value),
			ExpressionType::VariableReference { name } => self.evaluate_variable_reference(name, expr.pos),
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
		let left_value = self.evaluate(left_expr.clone())?;
		let right_value = self.evaluate(right_expr.clone())?;
		let res = match op {
			BinaryOperator::Add => {
				match left_value + right_value {
					Ok(v) => v,
					Err(e) => return Error::create(e, right_expr.pos),
				}
			}
			_ => return Error::create(format!("Invalid operation"), right_expr.pos),
		};
		Ok(res)
	}

	fn evaluate_read(&mut self) -> Result<Value> {
		let line: String = read!("{}\r\n"); // I believe this won't work on other platforms
		Ok(Value::Str(line))
	}

}