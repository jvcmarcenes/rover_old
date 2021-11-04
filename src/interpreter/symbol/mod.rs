
use crate::*;
use std::collections::HashMap;
use super::value::Value;

#[derive(Debug, Clone)]
pub struct SymbolTable {
	map: HashMap<String, Value>,
	parent: Vec<HashMap<String, Value>>,
}

impl SymbolTable {
	pub fn new() -> Self {
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
