
use std::io::Write as _;

use crate::{Error, Result, SourcePos, interpreter::value::ValueData, parser::{block::Block, expression::{Expression, ExpressionType}, statement::{AssignmentOperator, Statement, StatementType}}};

use super::{Interpreter, Message, evaluator::unwrap_or_error};

impl Interpreter {

	pub fn execute_statement(&mut self, statement: Statement) -> Result<Message> {
		let pos = statement.pos;
		match statement.statement_type {
			StatementType::Write { expr } => self.execute_write(&expr),
			StatementType::Writeline { expr } => self.execute_writeline(&expr),
			StatementType::Declaration { id, expr } => self.execute_declaration(id, &expr),
			StatementType::Assignment { op, path, expr } => self.execute_assigment(pos, op, path, &expr),
			StatementType::If { condition, then_block, else_block } => self.execute_if_statement(&condition, then_block, else_block),
			StatementType::Loop { block } => self.execute_loop_statement(block),
			StatementType::Break => Ok(Message::Break),
			StatementType::Continue => Ok(Message::Continue),
			StatementType::Return { expr } => self.execute_return_statement(&expr),
			StatementType::FunctionCall { head_expr, args_expr } => {
				self.evaluate_function_call(&head_expr, args_expr)?;
				Ok(Message::None)
			}
		}
	}
	
	fn execute_write(&mut self, expr: &Box<Expression>) -> Result<Message> {
		let value = self.evaluate(expr)?;
		print!("{}", value);
		std::io::stdout().flush().unwrap();
		Ok(Message::None)
	}

	fn execute_writeline(&mut self, expr: &Box<Expression>) -> Result<Message> {
		let value = self.evaluate(expr)?;
		println!("{}", value);
		Ok(Message::None)
	}

	fn execute_declaration(&mut self, id: String, expr: &Box<Expression>) -> Result<Message> {
		let value = self.evaluate(expr)?;
		self.symbol_table.insert(id, value);
		Ok(Message::None)
	}

	fn execute_assigment(&mut self, pos: SourcePos, op: AssignmentOperator, path: Box<Expression>, expr: &Box<Expression>) -> Result<Message> {
		let mut head = path;
		let mut value = match op {
			AssignmentOperator::Equals => self.evaluate(expr)?,
			AssignmentOperator::Increment => {
				let base = self.evaluate(&head)?;
				let val = self.evaluate(expr)?;
				unwrap_or_error(base + val, pos)?
			}
			AssignmentOperator::Decrement => {
				let base = self.evaluate(&head)?;
				let val = self.evaluate(expr)?;
				unwrap_or_error(base - val, pos)?
			}
		};

		loop {
			match head.expr_type {
				ExpressionType::VariableReference { name } => {
					self.symbol_table.replace(name, value, pos)?;
					return Ok(Message::None);
				}
				ExpressionType::PropertyAccess { head_expr, prop } => {
					head = head_expr.clone();
					let mut map = self.evaluate_to_map(&head_expr)?;
					map.insert(prop, value);
					value = ValueData::Map(map).into();
				}
				ExpressionType::IndexAccess { head_expr, index_expr } => {
					head = head_expr.clone();
					let root = self.evaluate(&head_expr)?;
					match root.data {
						ValueData::List(mut list) => {
							let index = self.evaluate_to_num(&index_expr)? as usize;
							list.remove(index);
							list.insert(index, value);
							value = ValueData::List(list).into();
						}
						ValueData::Str(mut str) => {
							let index = self.evaluate_to_num(&index_expr)? as usize;
							str.remove(index);
							str.insert_str(index, &value.to_string());
							value = ValueData::Str(str).into();
						}
						ValueData::Map(mut map) => {
							let prop = self.evaluate_to_str(&index_expr)?;
							map.remove(&prop);
							map.insert(prop, value);
							value = ValueData::Map(map).into();
						}
						_ => return Error::create("Cannot index value".to_string(), head_expr.pos),
					}
				}
				_ => return Error::create(format!("Expected Identifier, found {:?}", head.expr_type), head.pos),
			}
		}
	}

	fn execute_if_statement(&mut self, condition: &Box<Expression>, then_block: Block, else_block: Block) -> Result<Message> {
		let value = self.evaluate(condition)?.to_bool(condition.pos)?;

		if value {
			self.run_block(&then_block)
		} else {
			self.run_block(&else_block)
		}
	}

	fn execute_loop_statement(&mut self, block: Block) -> Result<Message> {
		loop {
			match self.run_block(&block)? {
				Message::Break => return Ok(Message::None),
				msg @ Message::Return(_) => return Ok(msg),
				_ => continue,
			}
		}
	}

	fn execute_return_statement(&mut self, expr: &Box<Expression>) -> Result<Message> {
		let value = self.evaluate(expr)?;
		Ok(Message::Return(value))
	}

}