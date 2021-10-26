
use std::collections::HashMap;

use text_io::try_read;

use crate::{*, parser::expression::*, parser::expression::Literal};

use super::{Interpreter, value::{Value, ValueObject, function::Function}};

pub fn unwrap_or_error(res: std::result::Result<ValueObject, String>, pos: SourcePos) -> Result<ValueObject> {
	match res {
		Ok(v) => Ok(v),
		Err(e) => Error::create(e, pos),
	}
}

impl Interpreter {

	pub fn evaluate(&mut self, expr: &Box<Expression>) -> Result<ValueObject> {
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
			ExpressionType::Random => Ok(Value::Num(rand::random()).into()),
		}
	}

	pub fn evaluate_to_num(&mut self, expr: &Box<Expression>) -> Result<f32> { self.evaluate(expr)?.to_num(expr.pos) }
	pub fn evaluate_to_list(&mut self, expr: &Box<Expression>) -> Result<Vec<ValueObject>> { self.evaluate(expr)?.to_list(expr.pos) }
	pub fn evaluate_to_map(&mut self, expr: &Box<Expression>) -> Result<HashMap<String, ValueObject>> { self.evaluate(expr)?.to_map(expr.pos) }
	pub fn evaluate_to_function(&mut self, expr: &Box<Expression>) -> Result<Function> { self.evaluate(expr)?.to_function(expr.pos) }

	fn evaluate_literal(&mut self, lit: Literal) -> Result<ValueObject> {
		let value = match lit {
			Literal::Void => Value::Void,
			Literal::Str(s) => Value::Str(s),
			Literal::Num(n) => Value::Num(n),
			Literal::Bool(b) => Value::Bool(b),
		};
		Ok(value.into())
	}

	fn evaluate_variable_reference(&mut self, name: &str, pos: SourcePos) -> Result<ValueObject> {
		match self.symbol_table.get(name) {
			Some(value) => Ok(value.clone()),
			None => Error::create(format!("variable {} is not defined", name), pos),
		}
	}

	fn evaluate_index_access(&mut self, head_expr: &Box<Expression>, index_expr: &Box<Expression>) -> Result<ValueObject> {
		
		let acc = self.evaluate(index_expr)?;
		match acc.value {
			Value::Str(prop) => self.evaluate_property_access(head_expr, &prop),
			Value::Num(mut index) => {
				let head_obj = self.evaluate(head_expr)?;
				let head = match head_obj.value {
					Value::List(list) => list,
					Value::Str(s) => s.chars().into_iter().map(|c| Value::Str(c.to_string()).into()).collect(),
					_ => return Error::create(format!("Cannot index {}", head_obj), head_expr.pos),
				};
				if index < 0.0 { index += head.len() as f32; }
				if index < 0.0 || index as usize >= head.len() {
					Error::create("Index out of bounds".to_string(), index_expr.pos)
				} else {
					Ok(head.get(index as usize).unwrap().clone())
				}
			}
			_ => Error::create(format!("Expected a number or string, found {}", acc), index_expr.pos),
		}
	}

	fn evaluate_list_literal(&mut self, expr_vec: Vec<Expression>) -> Result<ValueObject> {
		let mut values: Vec<ValueObject> = Vec::new();
		for expr in expr_vec {
			values.push(self.evaluate(&Box::new(expr))?);
		}
		Ok(Value::List(values).into())
	}

	fn evaluate_property_access(&mut self, head_expr: &Box<Expression>, prop: &str) -> Result<ValueObject> {
		let err = Error::create(format!("Property {} does not exist for type", prop), head_expr.pos);
		let pos = head_expr.pos;

		let head = self.evaluate(head_expr)?;
		let res = match head.value {
			Value::List(list) => {
				match prop {
					"size" => Value::Num(list.len() as f32).into(),
					_ => return err,
				}
			}
			Value::Str(s) => {
				match prop {
					"size" => Value::Num(s.len() as f32).into(),
					"num" => match s.parse() {
						Ok(n) => Function::return_static(Literal::Num(n), pos).into(),
						Err(_) => return Error::create("Could not cast string to num".to_string(), pos)
					}
					"is_num" => Function::return_static(Literal::Bool(s.parse::<f32>().is_ok()), pos).into(), 
					_ => return err,
				}
			}
			Value::Map(map) => {
				match map.get(prop) {
					Some(val) => val.clone(),
					None => return err
				}
			}
			_ => return err,
		};

		Ok(res)
	}

	fn evaluate_map_literal(&mut self, expr_table: HashMap<String, Expression>) -> Result<ValueObject> {
		let mut value_table: HashMap<String, ValueObject> = HashMap::new();
		for (name, expr) in expr_table.iter() {
			value_table.insert(name.clone(), self.evaluate(&Box::new(expr.clone()))?);
		}
		Ok(Value::Map(value_table).into())
	}

	fn evaluate_string_template(&mut self, expressions: Vec<Expression>) -> Result<ValueObject> {
		let mut values: Vec<(ValueObject, SourcePos)> = Vec::new() ;
		for expr in expressions.iter() {
			values.push((self.evaluate(&Box::new(expr.clone()))?, expr.pos));
		}
		let mut res: ValueObject = ValueObject::new(Value::Str("".to_string()));
		for (value, pos) in values.iter() {
			res = unwrap_or_error(res + value.clone(), *pos)?;
		}
		Ok(res)
	}

	fn evaluate_function_def(&mut self, params: Vec<String>, block: Block) -> Result<ValueObject> {
		Ok(Value::Function(Function::new(params, block)).into())
	}

	pub fn evaluate_function_call(&mut self, head_expr: &Box<Expression>, args_expr: Vec<Expression>) -> Result<ValueObject> {
		let head = self.evaluate_to_function(head_expr)?;

		if head.params.len() != args_expr.len() {
			return Error::create(format!("Expected {} arguments, received {}", head.params.len(), args_expr.len()), head_expr.pos);
		}

		let mut symbol_table: HashMap<String, ValueObject> = HashMap::new();

		for (key, expr) in head.params.iter().zip(args_expr.iter()) {
			let value = self.evaluate(&Box::new(expr.clone()))?;
			symbol_table.insert(key.clone(), value);
		}

		self.symbol_table.push(symbol_table);
		let ret = match self.run_block(&head.block)? {
			Message::Return(value) => value,
			_ => Value::Void.into(),
		};
		self.symbol_table.pop()?;

		Ok(ret)

	}

	fn evaluate_un_operation(&mut self, op: UnaryOperator, expr: &Box<Expression>) -> Result<ValueObject> {
		let val = self.evaluate(expr)?;
		match op {
			UnaryOperator::NumNegation => {
				if let Value::Num(n) = val.value.clone() { Ok(Value::Num(-n).into()) }
				else { Error::create("Invalid operator for type".to_string(), expr.pos) }
			},
			UnaryOperator::BoolNegation => {
				if let Value::Bool(b) = val.value.clone() { Ok(Value::Bool(!b).into()) }
				else { Error::create("Invalid operator for type".to_string(), expr.pos) }
			}
		}
	}

	fn evaluate_bin_operation(&mut self, op: BinaryOperator, left_expr: &Box<Expression>, right_expr: &Box<Expression>) -> Result<ValueObject> {
		let lhs = self.evaluate(left_expr)?;
		let rhs = self.evaluate(right_expr)?;
		let pos = right_expr.pos;
		match op {
			BinaryOperator::Add => unwrap_or_error(lhs + rhs, pos),
			BinaryOperator::Sub => unwrap_or_error(lhs - rhs, pos),
			BinaryOperator::Mul => unwrap_or_error(lhs * rhs, pos),
			BinaryOperator::Div => unwrap_or_error(lhs / rhs, pos),
			BinaryOperator::Mod => unwrap_or_error(lhs % rhs, pos),
			BinaryOperator::Equals    => Ok(Value::Bool(lhs == rhs).into()),
			BinaryOperator::NotEquals => Ok(Value::Bool(lhs != rhs).into()),
			BinaryOperator::Greater     => Ok(Value::Bool(lhs > rhs).into()),
			BinaryOperator::GreaterOrEq => Ok(Value::Bool(lhs >= rhs).into()),
			BinaryOperator::Lesser      => Ok(Value::Bool(lhs < rhs).into()),
			BinaryOperator::LesserOrEq  => Ok(Value::Bool(lhs <= rhs).into()),
			BinaryOperator::And => unwrap_or_error(ValueObject::bool_op(|a, b| a && b, lhs, rhs), pos),
			BinaryOperator::Or  => unwrap_or_error(ValueObject::bool_op(|a, b| a || b, lhs, rhs), pos),
		}
	}

	fn evaluate_read(&mut self, pos: SourcePos) -> Result<ValueObject> {
		let str_res: std::result::Result<String, text_io::Error> = try_read!("{}\r\n"); // I believe this won't work on other platforms
		match str_res {
			Ok(str) => Ok(Value::Str(str).into()),
			Err(_) => Error::create("Invalid console input".to_string(), pos),
		}
	}

	fn evaluate_readnum(&mut self, pos: SourcePos) -> Result<ValueObject> {
		let num_res: std::result::Result<f32, text_io::Error> = try_read!("{}\r\n"); // I believe this won't work on other platforms
		match num_res {
			Ok(num) => Ok(Value::Num(num).into()),
			Err(_) => Error::create("Invalid console input, expected a number".to_string(), pos),
		}
	}

}