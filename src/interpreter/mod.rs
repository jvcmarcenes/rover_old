
pub mod symbol;
pub mod value;
pub mod evaluator;
pub mod executor;

use std::collections::HashMap;

use crate::*;
use crate::parser::block::*;

use self::{value::*, symbol::*};

pub type ValueTable = HashMap<u32, Value>;

#[derive(Debug)]
pub struct Interpreter {
	value_table: ValueTable,
	symbol_table: SymbolTable,
	program: Block,
	dir: String,
}

pub enum Message {
	None,
	Break,
	Continue,
	Return(Value),
}

impl Interpreter {

	pub fn new(program: Block, dir: &str) -> Self {
		Self {
			value_table: ValueTable::new(),
			symbol_table: SymbolTable::new(),
			program,
			dir: dir.to_string(),
		}
	}

	pub fn run_block(&mut self, block: &Block) -> Result<Message> {
		let mut iter = block.iter();

		loop {
			if let Some(statement) = iter.next() {
				match self.execute_statement(statement)? {
					Message::None => continue,
					msg => return Ok(msg),
				}
			} else {
				return Ok(Message::None)
			}
		}
	}

	pub fn run(&mut self) -> Result<()> {
		self.run_block(&self.program.clone())?;
		Ok(())		
	}

}