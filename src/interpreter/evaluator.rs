
use text_io::try_read;

use crate::{*, lexer::tokens::Literal, parser::expression::*};

use super::{Interpreter, value::Value};

fn unwrap_or_error(res: std::result::Result<Value, String>, pos: SourcePos) -> Result<Value> {
	match res {
		Ok(v) => Ok(v),
		Err(e) => Error::create(e, pos),
	}
}

impl Interpreter {

	pub fn evaluate(&mut self, expr: &Box<Expression>) -> Result<Value> {
		match expr.to_owned().expr_type {
			ExpressionType::ValueLiteral { value } => self.evaluate_literal(value),
			ExpressionType::VariableReference { name } => self.evaluate_variable_reference(name, expr.pos),
			ExpressionType::Group { expr } => self.evaluate(&expr),
			ExpressionType::BinaryOperation { op, left_expr, right_expr } => self.evaluate_bin_operation(op, &left_expr, &right_expr),
			ExpressionType::UnaryOperation { op, expr } => self.evaluate_un_operation(op, &expr),
			ExpressionType::Read => self.evaluate_read(expr.pos),
			ExpressionType::ReadNum => self.evaluate_readnum(expr.pos),
			ExpressionType::List { expressions } => self.evaluate_list(expressions),
			ExpressionType::ArrayReference { head_expr, index_expr } => self.evaluate_array_reference(&head_expr, &index_expr),
		}
	}

	fn evaluate_to_num(&mut self, expr: &Box<Expression>) -> Result<f32> { self.evaluate(expr)?.to_num(expr.pos) }
	fn evaluate_to_list(&mut self, expr: &Box<Expression>) -> Result<Vec<Value>> { self.evaluate(expr)?.to_list(expr.pos) }

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

	fn evaluate_array_reference(&mut self, head_expr: &Box<Expression>, index_expr: &Box<Expression>) -> Result<Value> {
		let head = self.evaluate_to_list(head_expr)?;
		let index = self.evaluate_to_num(index_expr)?;
		if let Some(val) = head.get(index as usize) {
			Ok(val.clone())
		} else {
			Error::create("Unable to index array".to_string(), index_expr.pos)
		}
	}

	fn evaluate_list(&mut self, expr_vec: Vec<Expression>) -> Result<Value> {
		let mut values: Vec<Value> = Vec::new();
		for expr in expr_vec {
			values.push(self.evaluate(&Box::new(expr))?);
		}
		Ok(Value::List(values))
	}

	fn evaluate_un_operation(&mut self, op: UnaryOperator, expr: &Box<Expression>) -> Result<Value> {
		let val = self.evaluate(expr)?;
		match op {
			UnaryOperator::NumNegation => {
				if let Value::Num(n) = val { Ok(Value::Num(-n)) }
				else { Error::create("Invalid operator for type".to_string(), expr.pos) }
			},
			UnaryOperator::BoolNegation => {
				if let Value::Bool(b) = val { Ok(Value::Bool(!b)) }
				else { Error::create("Invalid operator for type".to_string(), expr.pos) }
			}
		}
	}

	fn evaluate_bin_operation(&mut self, op: BinaryOperator, left_expr: &Box<Expression>, right_expr: &Box<Expression>) -> Result<Value> {
		let lhs = self.evaluate(left_expr)?;
		let rhs = self.evaluate(right_expr)?;
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
			BinaryOperator::And => unwrap_or_error(Value::bool_op(|a, b| a && b, lhs, rhs), pos),
			BinaryOperator::Or  => unwrap_or_error(Value::bool_op(|a, b| a || b, lhs, rhs), pos),
		}
	}

	fn evaluate_read(&mut self, pos: SourcePos) -> Result<Value> {
		let str_res: std::result::Result<String, text_io::Error> = try_read!("{}\r\n"); // I believe this won't work on other platforms
		match str_res {
			Ok(str) => Ok(Value::Str(str)),
			Err(_) => Error::create("Invalid console input".to_string(), pos),
		}
	}

	fn evaluate_readnum(&mut self, pos: SourcePos) -> Result<Value> {
		let num_res: std::result::Result<f32, text_io::Error> = try_read!();
		match num_res {
			Ok(num) => Ok(Value::Num(num)),
			Err(_) => Error::create("Invalid console input, expected a number".to_string(), pos),
		}
	}

}