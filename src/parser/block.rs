
use std::{iter::Peekable, vec::IntoIter};

use crate::{*, lexer::tokens::*};
use super::{Parser, statement::Statement};

#[derive(Debug, Clone)]
pub struct Block {
	pub statements: Vec<Statement>,
}

impl Block {
	pub fn new(statements: Vec<Statement>) -> Self { Self { statements } }
	pub fn iter(&self) -> Peekable<IntoIter<Statement>> { self.clone().statements.into_iter().peekable() }
}

impl Default for Block {
	fn default() -> Self {
		Self::new(Vec::new())
	}
}

impl Parser {

	pub fn skip_new_lines(&mut self) {
		loop {
			match self.tokens.peek() {
				Some(token) if token.token_type == TokenType::EOL => { self.tokens.next(); },
				_ => break,
			}
		}
	}

	pub fn parse_program(&mut self) -> Result<Block> {
		let mut statements: Vec<Statement> = Vec::new();

		loop {
			self.skip_new_lines();
			match self.tokens.peek() {
				None => return Ok(Block::new(statements)),
				_ => statements.push(self.parse_statement()?)
			}
		}
	}

	pub fn parse_block(&mut self) -> Result<Block> {
		self.skip_new_lines();
		
		self.expect_symbol(Symbol::OpenBracket)?;

		let mut statements: Vec<Statement> = Vec::new();

		loop {
			self.skip_new_lines();
			match self.tokens.peek() {
				None => return Error::create("Statement Block was not closed".to_string(), SourcePos::new(0, 0)),
				Some(token) if token.token_type == TokenType::Symbol(Symbol::CloseBracket) => {
					self.tokens.next();
					return Ok(Block::new(statements));
				}
				_ => statements.push(self.parse_statement()?),
			}
		}
	}
}