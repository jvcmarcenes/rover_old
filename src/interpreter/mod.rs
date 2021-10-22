
pub mod evaluator;
pub mod value;

use std::collections::HashMap;
use std::vec::IntoIter;

use crate::*;
use crate::parser::{expression::*, block::*, statement::*};

use self::value::*;

pub struct Interpreter {
	symbol_table: HashMap<String, Value>,
	program: Block,
}

pub enum Message {
	None, Break, Continue
}

impl Interpreter {

	pub fn new(program: Block) -> Self {
		Self {
			symbol_table: HashMap::new(),
			program,
		}
	}

	pub fn run_block(&mut self, block: &Block) -> Result<Message> {
		let mut iter = block.iter();

		loop {
			if let Some(statement) = iter.next() {
				let res = match statement.statement_type {
					StatementType::Write { expr } => self.execute_write(&expr),
					StatementType::Writeline { expr } => self.execute_writeline(&expr),
					StatementType::Assignment { name, path, expr } => self.execute_assigment(name, statement.pos, path, &expr),
					StatementType::If { condition, then_block, else_block } => self.execute_if_statement(&condition, then_block, else_block),
					StatementType::Loop { block } => {
						loop {
							match self.run_block(&block)? {
								Message::Break => break,
								_ => continue,
							}
						}
						Ok(Message::None)
					}
					StatementType::Break => Ok(Message::Break),
					StatementType::Continue => Ok(Message::Continue),
				};
				match res {
					Ok(Message::None) => continue,
					Ok(msg) => return Ok(msg),
					Err(e) => return Err(e),
				}
			} else {
				return Ok(Message::None)
			}
		}
	}

	pub fn run(&mut self) -> Result<Message> {
		self.run_block(&self.program.clone())		
	}

	fn execute_write(&mut self, expr: &Box<Expression>) -> Result<Message> {
		let value = self.evaluate(expr)?;
		print!("{}", value);
		Ok(Message::None)
	}

	fn execute_writeline(&mut self, expr: &Box<Expression>) -> Result<Message> {
		let value = self.evaluate(expr)?;
		print!("{}\n", value);
		Ok(Message::None)
	}

	fn execute_assigment(&mut self, name: String, pos: SourcePos, path: Vec<Expression>, expr: &Box<Expression>) -> Result<Message> {

		fn calculate_new_value(base: Value, base_pos: SourcePos, path: &mut IntoIter<(Value, SourcePos)>, value: Value) -> Result<Value> {
			match path.next() {
				Some((path_v, path_v_pos)) => {

					match path_v {
						Value::Str(name) => {
							let mut base = base.to_map(base_pos)?;
							if let Some(next_base) = base.get(&name) {
								let value = calculate_new_value(next_base.clone(), path_v_pos, path, value)?;
								base.insert(name, value);
								Ok(Value::Map(base))
							} else {
								return Error::create(format!("Property {} is not defined", name), base_pos);
							}
						}
						Value::Num(mut num) => {
							let mut base = base.to_list(base_pos)?;
							if num < 0.0 { num += base.len() as f32 }
							if num < 0.0 || num >= base.len() as f32 {
								return Error::create(format!("Index {} out of bounds", num), base_pos);
							}
							if let Some(next_base) = base.get(num as usize) {
								let value = calculate_new_value(next_base.clone(), path_v_pos, path, value)?;
								base.remove(num as usize);
								base.insert(num as usize, value);
								Ok(Value::List(base))
							} else {
								return Error::create(format!("Index {} out of bounds", num), base_pos);
							}
						}
						_ => return Error::create("".to_string(), base_pos),
					}

				}
				None => Ok(value),
			}
		}

		let base = match self.symbol_table.get(&name) {
			Some(val) => val.clone(),
			None if path.len() == 0 => Value::Num(0.0),
			_ => return Error::create(format!("Identifier {} is not defined", name), pos),
		};
		let value = self.evaluate(expr)?;

		let mut values: Vec<(Value, SourcePos)> = Vec::new();
		for expr in path.iter() {
			let val = self.evaluate(&Box::new(expr.clone()))?;
			values.push((val, expr.pos));
		}

		self.symbol_table.insert(name, calculate_new_value(base, pos, &mut values.into_iter(), value)?);
		Ok(Message::None)
	}

	fn execute_if_statement(&mut self, condition: &Box<Expression>, then_block: Block, else_block: Block) -> Result<Message> {
		let value = self.evaluate(condition)?.to_bool(condition.pos)?;

		if value {
			self.run_block(&then_block)
		} else {
			self.run_block(&else_block)
		}
	}

}