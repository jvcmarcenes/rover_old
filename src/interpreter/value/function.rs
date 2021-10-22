
use crate::{Result, parser::block::Block};

use super::*;

#[derive(Debug, Clone)]
pub struct Function {
	pub params: Vec<String>,
	pub block: Block,
}

impl PartialEq for Function {
	fn eq(&self, _: &Self) -> bool { false }
}

impl Function {
	pub fn new(params: Vec<String>, block: Block) -> Self { Self { params, block } }
}

impl Value {
	pub fn to_function(&mut self, pos: SourcePos) -> Result<Function> {
		if let Self::Function(f) = self { Ok(f.to_owned()) }
		else { Error::create("Expected a function".to_string(), pos) }
	}
}
