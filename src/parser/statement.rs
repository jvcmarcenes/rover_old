
use crate::*;
use crate::parser::{*, expression::*, block::*};

#[derive(Debug, Clone)]
pub enum Statement {
	Write { expr: Box<Expression> },
	Writeline { expr: Box<Expression> },
	Assignment { name: String, expr: Box<Expression> },
	IfStatement { condition: Box<Expression>, then_block: Block, else_block: Block }
}

impl Parser {

	pub fn parse_statement(&mut self) -> Result<Statement> {
		if let Some(token) = self.tokens.next() {
			let res = match token.token_type {
				TokenType::Identifier(name) => self.parse_assigment_statement(name),
				TokenType::Keyword(keyword) => match keyword {
					Keyword::Write => self.parse_write_statement(),
					Keyword::Writeline => self.parse_writeline_statement(),
					Keyword::If => self.parse_if_statement(),
					_ => Error::create(format!("Expected statement found {:?}", token.token_type), token.pos)
				}
				_ => Error::create(format!("Expected statement, found {:?}", token.token_type), token.pos)
			};
			if res.is_ok() { self.expect(TokenType::EOL)?; }
			res
		} else { Error::create(String::from("Reached EOF"), SourcePos { line: 0, column: 0 }) }
	}

	fn parse_write_statement(&mut self) -> Result<Statement> {
		let expr = self.parse_expression()?;
		Ok(Statement::Write { expr: Box::new(expr) })
	}

	fn parse_writeline_statement(&mut self) -> Result<Statement> {
		let expr = self.parse_expression()?;
		Ok(Statement::Writeline { expr: Box::new(expr) })
	}

	fn parse_assigment_statement(&mut self, name: String) -> Result<Statement> {
		self.expect_symbol(Symbol::Equals)?;
		let expr = Box::new(self.parse_expression()?);
		Ok(Statement::Assignment { name, expr })
	}

	fn parse_if_statement(&mut self) -> Result<Statement> {
		let condition = self.parse_expression()?;
		let then_block = self.parse_block()?;
		let mut else_block: Block = Block::default();

		self.skip_new_lines();
		
		if let Some(token) = self.tokens.peek() {
			if token.token_type == TokenType::Keyword(Keyword::Else) {
				self.tokens.next();
				else_block = match self.tokens.peek() {
					Some(token) if token.token_type == TokenType::Keyword(Keyword::If) => {
						self.tokens.next();
						Block::new(vec![self.parse_if_statement()?])	
					}
					_ => self.parse_block()?,
				}
			}
		}
		Ok(Statement::IfStatement {
			condition: Box::new(condition),
			then_block,
			else_block,
		})
	}
}