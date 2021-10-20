
use std::{iter::Peekable, vec::IntoIter};

use crate::{*, lexer::tokens::*};
use super::{Parser, statement::Statement};

type StatementIter = Peekable<IntoIter<Statement>>;

#[derive(Debug)]
pub struct Program {
	pub statements: StatementIter
}

impl Parser {
	pub fn parse_program(&mut self) -> Result<Program> {
		let mut statements: Vec<Statement> = Vec::new();

		loop {
			match self.tokens.peek() {
				None => break,
				Some(Token { token_type: TokenType::EOL, pos: _ }) => {
					// println!("Skiping EOl");
					self.tokens.next();
					continue;
				}
				_ => statements.push(self.parse_statement()?)
			}
		}

		Ok(Program { statements: statements.into_iter().peekable() })
	}
}