
use crate::*;
use crate::parser::{*, expression::*};

#[derive(Debug)]
pub enum Statement {
	Writeline { expr: Box<Expression> },
	Assignment { name: String, expr: Box<Expression> },
}

impl Parser {
	pub fn parse_statement(&mut self) -> Result<Statement> {
		if let Some(token) = self.tokens.next() {
			let res = match token.token_type {
				TokenType::Identifier(name) => self.parse_assigment_statement(name),
				TokenType::Keyword(keyword) => match keyword {
					Keyword::Writeline => self.parse_writeline_statement(),
					_ => Error::create(format!("Expected statement found {:?}", token.token_type), token.pos)
				}
				_ => Error::create(format!("Expected statement, found {:?}", token.token_type), token.pos)
			};
			match res {
				Ok(_) => {
					match self.expect(TokenType::EOL) {
						Ok(_) => res,
						Err(e) => Err(e),
					}
				},
				_ => res
			}
		} else { Error::create(String::from("Reached EOF"), SourcePos { line: 0, column: 0 }) }
	}

	fn parse_writeline_statement(&mut self) -> Result<Statement> {
		let expr = self.parse_expression()?;
		Ok(Statement::Writeline { expr: Box::new(expr) })
	}

	fn parse_assigment_statement(&mut self, name: String) -> Result<Statement> {
		match self.expect(TokenType::Symbol(Symbol::Equals)) {
			Ok(_) => {
				let expr = Box::new(self.parse_expression()?);
				Ok(Statement::Assignment { name, expr })
			}
			Err(e) => Err(e),
		}
	}
}