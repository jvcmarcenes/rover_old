
use std::collections::HashMap;

use text_io::try_read;

use crate::{*, parser::expression::*, parser::expression::Literal};

use super::{Interpreter, value::{Value, function::Function}};

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
			ExpressionType::VariableReference { name } => self.evaluate_variable_reference(&name, expr.pos),
			ExpressionType::Group { expr } => self.evaluate(&expr),
			ExpressionType::BinaryOperation { op, left_expr, right_expr } => self.evaluate_bin_operation(op, &left_expr, &right_expr),
			ExpressionType::UnaryOperation { op, expr } => self.evaluate_un_operation(op, &expr),
			ExpressionType::Read => self.evaluate_read(expr.pos),
			ExpressionType::ReadNum => self.evaluate_readnum(expr.pos),
			ExpressionType::List { expressions } => self.evaluate_list_literal(expressions),
			ExpressionType::IndexAccess { head_expr, index_expr } => self.evaluate_index_access(&head_expr, &index_expr),
			ExpressionType::Map { table } => self.evaluate_map_literal(table),
			ExpressionType::PropertyAccess { head_expr, prop } => self.evaluate_property_access(&head_expr, &prop),
			ExpressionType::StringTemplate { expressions } => self.evaluate_string_template(expressions),
			ExpressionType::FunctionDef { params, block } => self.evaluate_function_def(params, block),
			ExpressionType::FunctionCall { head_expr, args_expr } => self.evaluate_function_call(&head_expr, args_expr),
			ExpressionType::Random => Ok(Value::Num(rand::random())),
		}
	}

	pub fn evaluate_to_num(&mut self, expr: &Box<Expression>) -> Result<f32> { self.evaluate(expr)?.to_num(expr.pos) }
	pub fn evaluate_to_list(&mut self, expr: &Box<Expression>) -> Result<Vec<Value>> { self.evaluate(expr)?.to_list(expr.pos) }
	pub fn evaluate_to_map(&mut self, expr: &Box<Expression>) -> Result<HashMap<String, Value>> { self.evaluate(expr)?.to_map(expr.pos) }
	pub fn evaluate_to_function(&mut self, expr: &Box<Expression>) -> Result<Function> { self.evaluate(expr)?.to_function(expr.pos) }

	fn evaluate_literal(&mut self, lit: Literal) -> Result<Value> {
		let v = match lit {
			Literal::Void => Value::Void,
			Literal::Str(s) => Value::Str(s),
			Literal::Num(n) => Value::Num(n),
			Literal::Bool(b) => Value::Bool(b),
		};
		Ok(v)
	}

	fn evaluate_variable_reference(&mut self, name: &str, pos: SourcePos) -> Result<Value> {
		match self.symbol_table.get(name) {
			Some(v) => Ok(v.clone()),
			None => Error::create(format!("variable {} is not defined", name), pos),
		}
	}

	fn evaluate_index_access(&mut self, head_expr: &Box<Expression>, index_expr: &Box<Expression>) -> Result<Value> {
		let head = self.evaluate_to_list(head_expr)?;
		let mut index = self.evaluate_to_num(index_expr)?;
		if index < 0.0 { index += head.len() as f32; }
		if index < 0.0 || index as usize >= head.len() {
			Error::create("Index out of bounds".to_string(), index_expr.pos)
		} else {
			Ok(head.get(index as usize).unwrap().clone())
		}
	}

	fn evaluate_list_literal(&mut self, expr_vec: Vec<Expression>) -> Result<Value> {
		let mut values: Vec<Value> = Vec::new();
		for expr in expr_vec {
			values.push(self.evaluate(&Box::new(expr))?);
		}
		Ok(Value::List(values))
	}

	fn evaluate_property_access(&mut self, head_expr: &Box<Expression>, prop: &str) -> Result<Value> {
		let head = self.evaluate_to_map(head_expr)?;
		match head.get(prop) {
			Some(val) => Ok(val.clone()),
			None => Error::create(format!("Property {} does not exist on map", prop), head_expr.pos),
		}
	}

	fn evaluate_map_literal(&mut self, expr_table: HashMap<String, Expression>) -> Result<Value> {
		let mut value_table: HashMap<String, Value> = HashMap::new();
		for (name, expr) in expr_table.iter() {
			value_table.insert(name.clone(), self.evaluate(&Box::new(expr.clone()))?);
		}
		Ok(Value::Map(value_table))
	}

	fn evaluate_string_template(&mut self, expressions: Vec<Expression>) -> Result<Value> {
		let mut values: Vec<(Value, SourcePos)> = Vec::new() ;
		for expr in expressions.iter() {
			values.push((self.evaluate(&Box::new(expr.clone()))?, expr.pos));
		}
		let mut res: Value = Value::Str("".to_string());
		for (value, pos) in values.iter() {
			res = unwrap_or_error(res + value.clone(), *pos)?;
		}
		Ok(res)
	}

	fn evaluate_function_def(&mut self, params: Vec<String>, block: Block) -> Result<Value> {
		Ok(Value::Function(Function::new(params, block)))
	}

	pub fn evaluate_function_call(&mut self, head_expr: &Box<Expression>, args_expr: Vec<Expression>) -> Result<Value> {
		let head = self.evaluate_to_function(head_expr)?;

		if head.params.len() != args_expr.len() {
			return Error::create(format!("Expected {} arguments, received {}", head.params.len(), args_expr.len()), head_expr.pos);
		}

		let mut symbol_table: HashMap<String, Value> = HashMap::new();

		for (key, expr) in head.params.iter().zip(args_expr.iter()) {
			let value = self.evaluate(&Box::new(expr.clone()))?;
			symbol_table.insert(key.clone(), value);
		}

		self.symbol_table.push(symbol_table);
		let ret = match self.run_block(&head.block)? {
			Message::Return(value) => value,
			_ => Value::Void,
		};
		self.symbol_table.pop()?;

		Ok(ret)

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
		let num_res: std::result::Result<f32, text_io::Error> = try_read!("{}\r\n"); // I believe this won't work on other platforms
		match num_res {
			Ok(num) => Ok(Value::Num(num)),
			Err(_) => Error::create("Invalid console input, expected a number".to_string(), pos),
		}
	}

}