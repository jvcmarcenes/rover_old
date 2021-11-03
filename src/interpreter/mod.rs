
pub mod evaluator;
pub mod value;
pub mod executor;

use std::collections::HashMap;

use crate::*;
use crate::parser::block::*;

use self::value::*;

#[derive(Debug, Clone)]
pub struct SymbolTable {
	map: HashMap<String, Value>,
	parent: Vec<HashMap<String, Value>>,
}

impl SymbolTable {
	fn new() -> Self {
		Self {
			map: HashMap::new(),
			parent: Vec::new(),
		}
	}

	pub fn insert(&mut self, key: String, value: Value) {
		self.map.insert(key, value);
	}

	pub fn replace(&mut self, key: String, value: Value, pos: SourcePos) -> Result<()> {
		if self.map.contains_key(&key) {
			self.map.insert(key, value);
			Ok(())
		} else {
			for map in self.parent.iter_mut() {
				if map.contains_key(&key) {
					map.insert(key, value);
					return Ok(())
				}
			}
			Error::create(format!("Identifier {:?}, not found", key), pos)
		}
	}

	pub fn get(&self, key: &str) -> Option<&Value> {
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

	pub fn push(&mut self, map: HashMap<String, Value>) {
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