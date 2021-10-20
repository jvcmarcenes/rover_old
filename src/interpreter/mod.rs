
pub mod evaluator;
pub mod value;

use std::collections::HashMap;

use crate::*;
use crate::parser::{expression::*, block::*, statement::*};

use self::value::*;

pub struct Interpreter {
	symbol_table: HashMap<String, Value>,
	program: Block,
}

impl Interpreter {

	pub fn new(program: Block) -> Self {
		Self {
			symbol_table: HashMap::new(),
			program,
		}
	}

	pub fn run_block(&mut self, block: Block) -> Result<()> {
		let mut iter = block.iter();

		loop {
			if let Some(statement) = iter.next() {
				let res = match statement {
					Statement::Write { expr } => self.execute_write(&expr),
					Statement::Writeline { expr } => self.execute_writeline(&expr),
					Statement::Assignment { name, expr } => self.execute_assigment(name, &expr),
					Statement::IfStatement { condition, then_block, else_block } => self.execute_if_statement(&condition, then_block, else_block),
				};
				match res {
					Ok(_) => continue,
					Err(e) => return Err(e),
				}
			} else {
				return Ok(())
			}
		}
	}

	pub fn run(&mut self) -> Result<()> {
		self.run_block(self.program.clone())		
	}

	fn execute_write(&mut self, expr: &Box<Expression>) -> Result<()> {
		let value = self.evaluate(expr)?;
		print!("{}", value);
		Ok(())
	}

	fn execute_writeline(&mut self, expr: &Box<Expression>) -> Result<()> {
		let value = self.evaluate(expr)?;
		println!("{}", value);
		Ok(())
	}

	fn execute_assigment(&mut self, name: String, expr: &Box<Expression>) -> Result<()> {
		let value = self.evaluate(expr)?;
		self.symbol_table.insert(name, value);
		Ok(())
	}

	fn execute_if_statement(&mut self, condition: &Box<Expression>, then_block: Block, else_block: Block) -> Result<()> {
		let value = self.evaluate(condition)?.to_bool(condition.pos)?;

		if value {
			self.run_block(then_block)
		} else {
			self.run_block(else_block)
		}
	}

}