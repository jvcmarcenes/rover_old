
use std::collections::HashMap;

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
pub enum Literal {
	Void,
	Str(String),
	Num(f64),
	Bool(bool),
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
	ValueLiteral { value: Literal },
	StringTemplate { expressions: Vec<Expression> },
	List { expressions: Vec<Expression> },
	Map { table: HashMap<String, Expression> },
	FunctionDef { params: Vec<String>, block: Block },
	Group { expr: Box<Expression> },
	VariableReference { name: String },
	IndexAccess { head_expr: Box<Expression>, index_expr: Box<Expression> },
	PropertyAccess { head_expr: Box<Expression>, prop: String },
	FunctionCall { head_expr: Box<Expression>, args_expr: Vec<Expression> },
	BinaryOperation {
		op: BinaryOperator,
		left_expr: Box<Expression>,
		right_expr: Box<Expression>,
	},
	UnaryOperation {
		op: UnaryOperator,
		expr: Box<Expression>
	},
	Read, ReadNum, Random,
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
	pub fn parse_expression_or_void(&mut self) -> Result<Expression> {
		match self.tokens.peek() {
			Some(token) if [TokenType::EOL, TokenType::Symbol(Symbol::CloseBracket)].contains(&token.token_type) => {
				let pos = token.pos;
				Expression::create(ExpressionType::ValueLiteral { value: Literal::Void }, pos) 
			}
			_ => self.parse_expression(),
		}
	}

	pub fn can_parse_expression(&self) -> bool {
		let mut dummy_parser = Parser::new(self.tokens.clone());
		dummy_parser.parse_expression().is_ok()
	}

	pub fn parse_expression(&mut self) -> Result<Expression> {
		let left_expr = self.parse_expression_no_binary()?;
		self.parse_binary_r_expression(0, left_expr)
	}

	fn parse_index_access(&mut self, head: Expression) -> Result<Expression> {
		self.tokens.next();
		let index_expr = self.parse_expression()?;
		self.expect_symbol(Symbol::CloseSqr)?;
		let pos = index_expr.pos;
		Expression::create(
			ExpressionType::IndexAccess {
				head_expr: Box::new(head), 
				index_expr: Box::new(index_expr)
			},
			pos,
		)
	}

	fn parse_property_access(&mut self, head: Expression) -> Result<Expression> {
		self.tokens.next();
		match self.tokens.next() {
			Some(Token { token_type: TokenType::Identifier(name), pos }) => {
				Expression::create(
					ExpressionType::PropertyAccess {
						head_expr: Box::new(head),
						prop: name,
					},
					pos,
				)
			}
			Some(token) => Error::create(format!("Expected identifier, found {:?}", token.token_type), token.pos),
			None => Error::create("Expected identifier, found EOF".to_string(), SourcePos::new(0, 0)),
		}
	}

	pub fn parse_function_call(&mut self, head: Expression) -> Result<(Box<Expression>, Vec<Expression>)> {
		let Token { token_type: _, pos } = self.tokens.next().unwrap();
		let mut args: Vec<Expression> = Vec::new();
		
		self.skip_new_lines();
		loop {
			match self.tokens.peek() {
				Some(token) if token.token_type == TokenType::Symbol(Symbol::ClosePar) => {
					self.tokens.next();
					return Ok((Box::new(head), args))
				}
				Some(_) if args.len() == 0 => args.push(self.parse_expression()?),
				Some(_) => {
					self.expect_symbol(Symbol::Comma)?;
					self.skip_new_lines();
					match self.tokens.peek() {
						Some(token) if token.token_type == TokenType::Symbol(Symbol::ClosePar) => continue,
						Some(_) => args.push(self.parse_expression()?),
						None => return Error::create("Expected expression, found EOF".to_string(), pos),
					}
				}
				None => return Error::create("Expected expression, found EOF".to_string(), pos),
			}
		}
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
						Keyword::ReadNum => Ok(ExpressionType::ReadNum),
						Keyword::Function => self.parse_function_definition(),
						Keyword::Random => Ok(ExpressionType::Random),
						_ => Error::create(format!("Expected expression, found {:?}", keyword), token.pos)
					}
				}
				TokenType::Symbol(symbol) => {
					match symbol {
						Symbol::OpenPar => self.parse_grouped_expression(),
						Symbol::OpenSqr => self.parse_list_literal(),
						Symbol::OpenBracket => self.parse_map_literal(),
						symbol if is_unary_symbol(&symbol) => self.parse_unary_expression(&symbol, token.pos),
						_ => Error::create(format!("Expected expression, found {:?}", symbol), token.pos)
					}
				}
				TokenType::Template(tokens) => self.parse_template_string(tokens),
				_ => Error::create(String::from("Unable to parse expression"), token.pos)
			};

			match res {
				Ok(expr_type) => {
					let mut expr = Expression::new(expr_type, token.pos);
					loop {
						if let Some(token) = self.tokens.peek() {
							let pos = token.pos;
							match token.token_type {
								TokenType::Symbol(Symbol::OpenSqr) => expr = self.parse_index_access(expr)?,
								TokenType::Symbol(Symbol::Period) => expr = self.parse_property_access(expr)?,
								TokenType::Symbol(Symbol::OpenPar) => {
									let (head_expr, args_expr) = self.parse_function_call(expr)?;
									expr = Expression::new(ExpressionType::FunctionCall { head_expr, args_expr }, pos);
								},
								_ => break,
							}
						} else { break; }
					}
					Ok(expr)
				},
				Err(e) => Err(e),
			}

		} else {
			Error::create(String::from("Tried to parse expression at eof"), SourcePos { line: 0, column: 0 })
		}
	}

	fn parse_literal_expression(&mut self, lit: tokens::Literal) -> Result<ExpressionType> {
		match lit {
			tokens::Literal::Str(s) => Ok(ExpressionType::ValueLiteral { value: Literal::Str(s) }),
			tokens::Literal::Num(n) => Ok(ExpressionType::ValueLiteral { value: Literal::Num(n) }),
			tokens::Literal::Bool(b) => Ok(ExpressionType::ValueLiteral { value: Literal::Bool(b) }),
		}
	}

	fn parse_variable_reference_expression(&mut self, name: String) -> Result<ExpressionType> {
		Ok(ExpressionType::VariableReference { name })
	}

	fn parse_grouped_expression(&mut self) -> Result<ExpressionType> {
		let expr = Box::new(self.parse_expression()?);
		self.expect_symbol(Symbol::ClosePar)?; 
		Ok(ExpressionType::Group { expr })
	}

	fn parse_list_literal(&mut self) -> Result<ExpressionType> {
		self.skip_new_lines();
		let mut expressions: Vec<Expression> = Vec::new();

		loop {
			match self.tokens.peek() {
				Some(token) if token.token_type == TokenType::Symbol(Symbol::CloseSqr) => {
					self.tokens.next();
					return Ok(ExpressionType::List { expressions });
				}
				Some(_) if expressions.len() == 0 => expressions.push(self.parse_expression()?),
				Some(_) => {
					self.expect_any(vec![TokenType::EOL, TokenType::Symbol(Symbol::Comma)])?;
					self.skip_new_lines();
					match self.tokens.peek() {
						Some(token) if token.token_type == TokenType::Symbol(Symbol::CloseSqr) => continue,
						Some(_) => expressions.push(self.parse_expression()?),
						None => return Error::create("Expected expression, found EOF".to_string(), SourcePos::new(0, 0)),
					}
				}
				None => return Error::create("Expected expression, found EOF".to_string(), SourcePos::new(0, 0)),
			}
		}
	}

	fn parse_map_literal(&mut self) -> Result<ExpressionType> {
		self.skip_new_lines();
		let mut table: HashMap<String, Expression> = HashMap::new();

		loop {
			match self.tokens.peek() {
				Some(token) if token.token_type == TokenType::Symbol(Symbol::CloseBracket) => {
					self.tokens.next();
					return Ok(ExpressionType::Map { table })
				}
				Some(token) if table.len() == 0 => {
					if let TokenType::Identifier(name) = token.clone().token_type {
						self.tokens.next();
						self.expect_symbol(Symbol::Equals)?;
						let expr = self.parse_expression()?;
						table.insert(name, expr);
					} else { return Error::create(format!("Expected identifier, found {:?}", token.token_type), token.pos) }
				}
				Some(_) => {
					self.expect_any(vec![TokenType::EOL, TokenType::Symbol(Symbol::Comma)])?;
					self.skip_new_lines();
					if let Some(token) = self.tokens.peek() {
						match token.token_type.clone() {
							TokenType::Symbol(Symbol::CloseBracket) => continue,
							TokenType::Identifier(name) => {
								self.tokens.next();
								self.expect_symbol(Symbol::Equals)?;
								let expr = self.parse_expression()?;
								table.insert(name, expr);
							}
							_ => return Error::create(format!("Expected identifier, found {:?}", token.token_type), token.pos),
						}
					} else {
						return Error::create("Expected identifier, found EOF".to_string(), SourcePos::new(0, 0));
					}
				}
				None => return Error::create("Expected expression, found EOF".to_string(), SourcePos::new(0, 0)),
			}
		}
	}

	fn parse_function_definition(&mut self) -> Result<ExpressionType> {
		self.expect_symbol(Symbol::OpenPar)?;
		self.skip_new_lines();

		let mut params: Vec<String> = Vec::new();

		loop {
			match self.tokens.peek() {
				Some(token) if token.token_type == TokenType::Symbol(Symbol::ClosePar) => {
					self.tokens.next();
					let root = if self.in_function { false } else { self.in_function = true; true };
					let block = self.parse_block()?;
					if root { self.in_function = false; }
					return Ok(ExpressionType::FunctionDef { params, block });
				}
				Some(Token { token_type: TokenType::Identifier(name), pos: _ }) if params.len() == 0 => {
					params.push(name.to_string());
					self.tokens.next();
				}
				Some(_) => {
					self.expect_symbol(Symbol::Comma)?;
					self.skip_new_lines();
					match self.tokens.peek() {
						Some(token) if token.token_type == TokenType::Symbol(Symbol::ClosePar) => continue,
						Some(Token { token_type: TokenType::Identifier(name), pos: _ }) => {
							params.push(name.to_string());
							self.tokens.next();
						}
						Some(token) => return Error::create(format!("Expexted Identifier, found {:?}", token), token.pos),
						None => return Error::create("Expected expression, found EOF".to_string(), SourcePos::new(0, 0)),
					}
				}
				None => return Error::create("Expected expression, found EOF".to_string(), SourcePos::new(0, 0)),
			}
		}
	}

	fn parse_template_string(&mut self, tokens: Vec<Token>) -> Result<ExpressionType> {
		let mut expressions: Vec<Expression> = Vec::new();
		
		let mut parser = Parser::new(tokens.into_iter().peekable());

		loop {
			if let Some(token) = parser.tokens.peek() {
				match token.token_type {
					TokenType::Symbol(Symbol::HashtagOpenBracket) => {
						parser.tokens.next();
						expressions.push(parser.parse_expression()?);
						parser.expect_symbol(Symbol::CloseBracket)?;
					}
					_ => expressions.push(parser.parse_expression()?),
				}
			} else {
				return Ok(ExpressionType::StringTemplate { expressions })
			}
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
