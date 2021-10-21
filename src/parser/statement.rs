
use crate::*;
use crate::parser::{*, expression::*, block::*};

#[derive(Debug, Clone)]
pub enum StatementType {
	Write { expr: Box<Expression> },
	Writeline { expr: Box<Expression> },
	Assignment { name: String, path: Vec<Expression>, expr: Box<Expression> },
	IfStatement { condition: Box<Expression>, then_block: Block, else_block: Block }
}

#[derive(Debug, Clone)]
pub struct Statement {
	pub statement_type: StatementType,
	pub pos: SourcePos,
}

impl Statement {
	fn new(statement_type: StatementType, pos: SourcePos) -> Self { Self { statement_type, pos } }
	fn create(statement_type: StatementType, pos: SourcePos) -> Result<Self> { Ok(Self::new(statement_type, pos)) }
}

impl Parser {

	pub fn parse_statement(&mut self) -> Result<Statement> {
		if let Some(token) = self.tokens.next() {
			let res = match token.token_type {
				TokenType::Identifier(name) => self.parse_assigment_statement(&name, token.pos),
				TokenType::Keyword(keyword) => match keyword {
					Keyword::Write => self.parse_write_statement(token.pos),
					Keyword::Writeline => self.parse_writeline_statement(token.pos),
					Keyword::If => self.parse_if_statement(token.pos),
					_ => Error::create(format!("Expected statement found {:?}", token.token_type), token.pos)
				}
				_ => Error::create(format!("Expected statement, found {:?}", token.token_type), token.pos)
			};
			if res.is_ok() { self.expect(TokenType::EOL)?; }
			res
		} else { Error::create(String::from("Reached EOF"), SourcePos::new(0, 0)) }
	}

	fn parse_write_statement(&mut self, pos: SourcePos) -> Result<Statement> {
		let expr = self.parse_expression()?;
		Statement::create(StatementType::Write { expr: Box::new(expr) }, pos)
	}

	fn parse_writeline_statement(&mut self, pos: SourcePos) -> Result<Statement> {
		let expr = self.parse_expression()?;
		Statement::create(StatementType::Writeline { expr: Box::new(expr) }, pos)
	}

	fn parse_assigment_statement(&mut self, name: &str, pos: SourcePos) -> Result<Statement> {
		let mut path: Vec<Expression> = Vec::new();
		loop {
			if let Some(token) = self.tokens.next() {
				match token.token_type {
					TokenType::Symbol(Symbol::Equals) => break,
					TokenType::Symbol(Symbol::OpenSqr) => {
						let index_expr = self.parse_expression()?;
						self.expect_symbol(Symbol::CloseSqr)?;
						path.push(index_expr);
					}
					TokenType::Symbol(Symbol::Period) => {
						if let Some(prop_token) = self.tokens.next() {
							match prop_token.token_type {
								TokenType::Identifier(name) => path.push(Expression { expr_type: ExpressionType::ValueLiteral { value: Literal::Str(name) }, pos: prop_token.pos }),
								_ => return Error::create(format!("Expected identifier, found {:?}", prop_token.token_type), prop_token.pos),
							}
						} else { return Error::create(format!("Expected identifier, found {:?}", token.token_type), token.pos); };
					}
					_ => return Error::create(format!("Expected Equals or Variable reference, found {:?}", token.token_type), token.pos),
				}
			} else { break; }
		}
		let expr = Box::new(self.parse_expression()?);
		Statement::create(StatementType::Assignment { name: name.to_string(), path, expr }, pos)
	}

	fn parse_if_statement(&mut self, pos: SourcePos) -> Result<Statement> {
		let condition = self.parse_expression()?;
		let then_block = self.parse_block()?;
		let mut else_block: Block = Block::default();

		self.skip_new_lines();

		if let Some(token) = self.tokens.peek() {
			if token.token_type == TokenType::Keyword(Keyword::Else) {
				self.tokens.next();
				else_block = match self.tokens.peek() {
					Some(token) if token.token_type == TokenType::Keyword(Keyword::If) => {
						let pos = token.pos;
						self.tokens.next();
						Block::new(vec![self.parse_if_statement(pos)?])	
					}
					_ => self.parse_block()?,
				}
			}
		}
		Statement::create(StatementType::IfStatement {
			condition: Box::new(condition),
			then_block,
			else_block,
		}, pos)
	}
}