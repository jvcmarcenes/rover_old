
use crate::*;
use crate::lexer::tokens::*;
use crate::parser::Parser;

use self::BinaryOperator::*;

#[derive(Debug, Clone)]
pub enum BinaryOperator {
	Add, Sub, Mul, Div, Mod,
	And, Or,
	Equals, NotEquals, Greater, GreaterOrEq, Lesser, LesserOrEq,
}

fn get_bin_op_for_symbol(s: &Symbol) -> Option<BinaryOperator> {
	let op = match s {
		Symbol::Plus => Add,
		Symbol::Minus => Sub,
		Symbol::Asterisk => Mul,
		Symbol::Slash => Div,
		Symbol::Percent => Mod,
		Symbol::DoubleAmper => And,
		Symbol::DoubleBar => Or,
		Symbol::DoubleEquals => Equals,
		Symbol::ExclamEquals => NotEquals,
		Symbol::CloseAng => Greater,
		Symbol::CloseAngEquals => GreaterOrEq,
		Symbol::OpenAng => Lesser,
		Symbol::OpenAngEquals => LesserOrEq,
		_ => return None,
	};
	Some(op)
}

fn get_bin_op_for_token(t: &Token) -> Result<BinaryOperator> {
	match t.token_type {
		TokenType::Symbol(symbol) if is_binary_symbol(&symbol) => {
			if let Some(op) = get_bin_op_for_symbol(&symbol) {
				Ok(op)
			} else {
				Error::create(format!("Expected binary operator, found {:?}", symbol), t.pos)
			}
		}
		_ => Error::create(format!("Expected binary operator, found {:?}", t), t.pos)
	}
}

fn get_precedence(op: &BinaryOperator) -> i32 {
	match op {
		And | Or => 1,
		Equals | NotEquals | Greater | GreaterOrEq | Lesser | LesserOrEq => 2,
		Add | Sub | Mod => 3,
		Mul | Div => 4,
	}
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
	NumNegation, BoolNegation
}

fn get_un_op_for_symbol(s: &Symbol) -> Option<UnaryOperator> {
	let op = match s {
		Symbol::Minus => UnaryOperator::NumNegation,
		Symbol::Exclam => UnaryOperator::BoolNegation,
		_ => return None,
	};
	Some(op)
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
	Literal { value: Literal },
	Group { expr: Box<Expression> },
	VariableReference { name: String },
	BinaryOperation {
		op: BinaryOperator,
		left_expr: Box<Expression>,
		right_expr: Box<Expression>,
	},
	UnaryOperation {
		op: UnaryOperator,
		expr: Box<Expression>
	},
	Read,
}

#[derive(Debug, Clone)]
pub struct Expression {
	pub expr_type: ExpressionType,
	pub pos: SourcePos,
}

impl Expression {
	pub fn new(expr_type: ExpressionType, pos: SourcePos) -> Self { Self { expr_type, pos } }
	pub fn create(expr_type: ExpressionType, pos: SourcePos) -> Result<Self> { Ok(Self::new(expr_type, pos)) }
}

impl Parser {
	pub fn parse_expression(&mut self) -> Result<Expression> {
		let left_expr = self.parse_expression_no_binary()?;
		// println!("{:?}", left_expr);
		self.parse_binary_r_expression(0, left_expr)		
	}

	fn parse_expression_no_binary(&mut self) -> Result<Expression> {
		if let Some(token) = self.tokens.next() {
			let res = match token.token_type {
				TokenType::Literal(lit) => self.parse_literal_expression(lit),
				TokenType::Identifier(name) => {
					self.parse_variable_reference_expression(name)	
				}
				TokenType::Keyword(keyword) => {
					match keyword {
						Keyword::Read => Ok(ExpressionType::Read),
						_ => Error::create(format!("Expected expression, found {:?}", keyword), token.pos)
					}
				}
				TokenType::Symbol(Symbol::OpenPar) => self.parse_grouped_expression(),
				TokenType::Symbol(symbol) if is_unary_symbol(&symbol) => self.parse_unary_expression(&symbol, token.pos),
				_ => Error::create(String::from("Unable to parse expression"), token.pos)
			};
			match res {
				Ok(expr_type) => Expression::create(expr_type, token.pos),
				Err(e) => Err(e),
			}
		} else {
			Error::create(String::from("Tried to parse expression at eof"), SourcePos { line: 0, column: 0 })
		}
	}

	fn parse_literal_expression(&mut self, lit: Literal) -> Result<ExpressionType> {
		Ok(ExpressionType::Literal { value: lit })
	}

	fn parse_variable_reference_expression(&mut self, name: String) -> Result<ExpressionType> {
		Ok(ExpressionType::VariableReference { name })
	}

	fn parse_grouped_expression(&mut self) -> Result<ExpressionType> {
		let expr = Box::new(self.parse_expression()?);
		match self.expect(TokenType::Symbol(Symbol::ClosePar)) {
			Ok(_) => Ok(ExpressionType::Group { expr }),
			Err(e) => Err(e),
		}
	}

	fn parse_unary_expression(&mut self, symbol: &Symbol, pos: SourcePos) -> Result<ExpressionType> {
		if let Some(op) = get_un_op_for_symbol(&symbol) {
			let expr = Box::new(self.parse_expression_no_binary()?);
			Ok(ExpressionType::UnaryOperation { op, expr })
		} else {
			Error::create(format!("Expected a unary operator, found {:?}", *symbol), pos)
		}
	}

	fn parse_binary_r_expression(&mut self, prec: i32, left_expr: Expression) -> Result<Expression> {
		let mut left_expr = left_expr;

		loop {
			match self.tokens.peek() {
				Some(token) => {
					match get_bin_op_for_token(&token) {
						Ok(op) => {
							let current_prec = get_precedence(&op);
							if current_prec < prec { return Ok(left_expr) }

							let token = self.tokens.next().unwrap();
							let mut right_expr = self.parse_expression_no_binary()?;

							match self.tokens.peek() {
								Some(next_token) => {
									match get_bin_op_for_token(&next_token) {
										Ok(next_op) => {
											let next_prec = get_precedence(&next_op);
											if next_prec > current_prec {
												right_expr = self.parse_binary_r_expression(current_prec + 1, right_expr)?;
											}
										}
										Err(_) => ()
									}
								}
								None => ()
							}
							left_expr = Expression::new(ExpressionType::BinaryOperation { op, left_expr: Box::new(left_expr), right_expr: Box::new(right_expr) }, token.pos);
						}
						Err(_) => return Ok(left_expr)
					}
				}
				None => return Ok(left_expr),
			}
		}
	}
}