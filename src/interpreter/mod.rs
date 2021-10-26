
pub mod evaluator;
pub mod value;
pub mod executor;

use std::collections::HashMap;

use crate::*;
use crate::parser::block::*;

use self::value::*;

#[derive(Debug, Clone)]
pub struct SymbolTable {
	map: HashMap<String, ValueObject>,
	parent: Vec<HashMap<String, ValueObject>>,
}

impl SymbolTable {
	fn new() -> Self {
		Self {
			map: HashMap::new(),
			parent: Vec::new(),
		}
	}

	pub fn insert(&mut self, key: String, value: ValueObject) {
		self.map.insert(key, value);
	}

	pub fn get(&self, key: &str) -> Option<&ValueObject> {
		match self.map.get(key) {
			Some(value) => Some(value),
			None => {
				for map in self.parent.iter() {
					match map.get(key) {
						Some(value) => return Some(value),
						None => continue,
					}
				}
				None
			}
		}
	}

	pub fn push(&mut self, map: HashMap<String, ValueObject>) {
		self.parent.push(self.map.clone());
		self.map = map;
	}

	pub fn push_new(&mut self) { self.push(HashMap::new()); }

	pub fn pop(&mut self) -> Result<()> {
		match self.parent.pop() {
			Some(map) => self.map = map,
			None => return Error::create("Tried to pop a Symbol Table that had no parent".to_string(), SourcePos::default()),
		}
		Ok(())
	}
}

#[derive(Debug)]
pub struct Interpreter {
	symbol_table: SymbolTable,
	program: Block,
}

pub enum Message {
	None,
	Break,
	Continue,
	Return(ValueObject),
}

impl Interpreter {

	pub fn new(program: Block) -> Self {
		Self {
			symbol_table: SymbolTable::new(),
			program,
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

	pub fn run(&mut self) -> Result<Message> {
		self.run_block(&self.program.clone())		
	}

}