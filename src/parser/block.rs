
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
	pub fn parse_program(&mut self) -> Result<Block> {
		let mut statements: Vec<Statement> = Vec::new();

		loop {
			match self.tokens.peek() {
				None => return Ok(Block::new(statements)),
				Some(Token { token_type: TokenType::EOL, pos: _ }) => {
					self.tokens.next();
					continue;
				}
				_ => statements.push(self.parse_statement()?)
			}
		}
	}

	pub fn parse_block(&mut self) -> Result<Block> {
		self.expect_symbol(Symbol::OpenBracket)?;

		let mut statements: Vec<Statement> = Vec::new();

		loop {
			match self.tokens.peek() {
				None => return Error::create("Statement Block was not closed".to_string(), SourcePos::new(0, 0)),
				Some(token) if token.token_type == TokenType::Symbol(Symbol::CloseBracket) => {
					self.tokens.next();
					return Ok(Block::new(statements));
				}
				Some(token) if token.token_type == TokenType::EOL => {
					self.tokens.next();
					continue;
				},
				_ => statements.push(self.parse_statement()?),
			}
		}
	}
}