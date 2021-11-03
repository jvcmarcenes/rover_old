
use crate::*;
use crate::parser::{*, expression::*, block::*};

#[derive(Debug, Clone)]
pub enum AssignmentOperator {
	Equals, Increment, Decrement
}

#[derive(Debug, Clone)]
pub enum StatementType {
	Write { expr: Box<Expression> },
	Writeline { expr: Box<Expression> },
	Declaration { id: String, expr: Box<Expression> },
	Assignment { op: AssignmentOperator, path: Box<Expression>, expr: Box<Expression> },
	If { condition: Box<Expression>, then_block: Block, else_block: Block },
	Loop { block: Block }, Break, Continue,
	FunctionCall { head_expr: Box<Expression>, args_expr: Vec<Expression> },
	Return { expr: Box<Expression> }
}

#[derive(Debug, Clone)]
pub struct Statement {
	pub statement_type: StatementType,
	pub pos: SourcePos,
}

impl Statement {
	pub fn new(statement_type: StatementType, pos: SourcePos) -> Self { Self { statement_type, pos } }
	fn create(statement_type: StatementType, pos: SourcePos) -> Result<Self> { Ok(Self::new(statement_type, pos)) }
}

impl Parser {

	pub fn parse_statement(&mut self) -> Result<Statement> {
		if let Some(token) = self.tokens.peek() {
			let pos = token.pos;
			let statement = match token.token_type {
				TokenType::Identifier(_) | TokenType::Symbol(Symbol::OpenPar) => {
					let expr = self.parse_expression()?;
					
					match expr.expr_type {
						ExpressionType::FunctionCall { head_expr, args_expr } => {
							Statement::new(StatementType::FunctionCall { head_expr, args_expr }, expr.pos)
						}
						_ => self.parse_assigment_statement(Box::new(expr))?,
					}
				}
				TokenType::Keyword(keyword) => match keyword {
					Keyword::Write => self.parse_write_statement()?,
					Keyword::Writeline => self.parse_writeline_statement()?,
					Keyword::Let => self.parse_declaration_statement()?,
					Keyword::If => self.parse_if_statement()?,
					Keyword::Loop => self.parse_loop_statement()?,
					Keyword::Break => if self.in_loop {
						let Token { token_type: _, pos } = self.tokens.next().unwrap();
						self.expect_eol()?;
						Statement::create(StatementType::Break, pos)?
					} else { return Error::create("Break statements are only allowed inside loops".to_string(), pos) },
					Keyword::Continue => if self.in_loop { 
						let Token { token_type: _, pos } = self.tokens.next().unwrap();
						self.expect_eol()?;
						Statement::create(StatementType::Continue, pos)?
					} else { return Error::create("Continue statements are only allowed inside loops".to_string(), pos) },
					Keyword::Return => if self.in_function { 
						let Token { token_type: _, pos } = self.tokens.next().unwrap();
						let expr = self.parse_expression_or_void()?;
						self.expect_eol()?;
						Statement::create(StatementType::Return { expr: Box::new(expr) }, pos)?
					} else { return Error::create("Return statements are only allowed inside functions".to_string(), pos) },
					_ => return Error::create(format!("Expected statement found {:?}", token.token_type), pos)
				}
				_ => return Error::create(format!("Expected statement, found {:?}", token.token_type), pos)
			};
			Ok(statement)
		} else {
			Error::create(String::from("Expected statement, foud EOF"), SourcePos::new(0, 0))
		}
	}

	fn parse_write_statement(&mut self) -> Result<Statement> {
		let Token { token_type: _, pos } = self.tokens.next().unwrap();
		let expr = self.parse_expression()?;
		self.expect_eol()?;
		Statement::create(StatementType::Write { expr: Box::new(expr) }, pos)
	}
	
	fn parse_writeline_statement(&mut self) -> Result<Statement> {
		let Token { token_type: _, pos } = self.tokens.next().unwrap();
		let expr = self.parse_expression_or_void()?;
		self.expect_eol()?;
		Statement::create(StatementType::Writeline { expr: Box::new(expr) }, pos)
	}

	fn parse_declaration_statement(&mut self) -> Result<Statement> {
		let Token { token_type: _, pos } = self.tokens.next().unwrap();
		let name = match self.tokens.next() {
			Some(Token { token_type: TokenType::Identifier(name), pos: _ }) => name,
			_ => return Error::create("Expected identifier".to_string(), pos),
		};
		let expr = if let Some(_) = self.optional_symbol(Symbol::Equals) {
			self.parse_expression()?
		} else { 
			Expression::new(ExpressionType::ValueLiteral { value: expression::Literal::Void }, pos)
		};
		Statement::create(StatementType::Declaration { id: name, expr: Box::new(expr) }, pos)
	}

	fn parse_assigment_statement(&mut self, path: Box<Expression>) -> Result<Statement> {
		let Token { token_type, pos } = self.expect_any_symbol(vec![Symbol::Equals, Symbol::PlusEquals, Symbol::MinusEquals])?;
		let op = match token_type {
			TokenType::Symbol(Symbol::Equals) => AssignmentOperator::Equals,
			TokenType::Symbol(Symbol::PlusEquals) => AssignmentOperator::Increment,
			TokenType::Symbol(Symbol::MinusEquals) => AssignmentOperator::Decrement,
			_ => panic!("expect_any should guarantee that this never panics."),
		};
		let expr = Box::new(self.parse_expression()?);
		self.expect_eol()?;
		Statement::create(StatementType::Assignment { op, path, expr }, pos)
	}

	fn parse_if_statement(&mut self) -> Result<Statement> {
		let Token { token_type: _, pos } = self.tokens.next().unwrap();
		let condition = self.parse_expression()?;
		let then_block = self.parse_block()?;
		let mut else_block: Block = Block::default();

		self.skip_new_lines();

		if let Some(token) = self.tokens.peek() {
			if token.token_type == TokenType::Keyword(Keyword::Else) {
				self.tokens.next();
				else_block = match self.tokens.peek() {
					Some(token) if token.token_type == TokenType::Keyword(Keyword::If) => {
						Block::new(vec![self.parse_if_statement()?])	
					}
					_ => self.parse_block()?,
				}
			}
		}
		
		Statement::create(StatementType::If {
			condition: Box::new(condition),
			then_block,
			else_block,
		}, pos)
	}

	fn parse_loop_statement(&mut self) -> Result<Statement> {
		let Token { token_type: _, pos } = self.tokens.next().unwrap();

		let root_loop = if self.in_loop { false } else { self.in_loop = true; true };
		let block = self.parse_block()?;
		if root_loop { self.in_loop = false }

		Statement::create(
			StatementType::Loop { block },
			pos,
		)
	}

}