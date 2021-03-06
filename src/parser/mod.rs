
pub mod expression;
pub mod statement;
pub mod block;

use crate::*;
use crate::lexer::tokens::*;

use std::{iter::Peekable, vec::IntoIter};

type TokenIter = Peekable<IntoIter<Token>>;

pub struct Parser {
	tokens: TokenIter,
	in_loop: bool,
	in_function: bool,
}

impl Parser {
	pub fn new(tokens: TokenIter) -> Self {
		// println!("{:#?}", tokens);
		Parser { 
			tokens: tokens.filter(|token| token.token_type != TokenType::Comment).collect::<Vec<Token>>().into_iter().peekable(),
			in_loop: false,
			in_function: false,
		}
	}

	pub fn expect(&mut self, expected_type: TokenType) -> Result<Token> {
		match self.tokens.peek() {
			Some(token) if token.token_type == expected_type => Ok(self.tokens.next().unwrap()),
			Some(token) => Error::create(format!("Expected {:?}, found {:?}", expected_type, token.token_type), token.pos),
			None => Error::create(format!("Expected {:?}, found EOF", expected_type), SourcePos::new(0, 0)),
		}
	}

	pub fn expect_pred(&mut self, expected: &str, pred: fn(&TokenType) -> bool) -> Result<Token> {
		match self.tokens.peek() {
			Some(Token { token_type, pos: _ }) if pred(token_type) => Ok(self.tokens.next().unwrap()),
			Some(token) => Error::create(format!("Expected {:?}, found {:?}", expected, token.token_type), token.pos),
			None => Error::create(format!("Expected {:?}, found EOF", expected), SourcePos::new(0, 0)),
		}
	}

	pub fn expect_any(&mut self, expected_types: Vec<TokenType>) -> Result<Token> {
		match self.tokens.peek() {
			Some(token) if expected_types.contains(&token.token_type) => Ok(self.tokens.next().unwrap()),
			Some(token) => Error::create(format!("Expected any of {:?}, found {:?}", expected_types, token.token_type), token.pos),
			_ => Error::create(format!("Expected any of {:?}, found EOF", expected_types), SourcePos::new(0, 0)),
		}
	}

	pub fn expect_symbol(&mut self, expected_symbol: Symbol) -> Result<Token> { self.expect(TokenType::Symbol(expected_symbol)) }

	pub fn expect_eol(&mut self) -> Result<()> { 
		match self.tokens.peek() {
			Some(token) if token.token_type == TokenType::Symbol(Symbol::CloseBracket) => Ok(()),
			_ => self.expect(TokenType::EOL).map(|_|())
		}
	}

	pub fn expect_any_symbol(&mut self, expected_symbols: Vec<Symbol>) -> Result<Token> {
		self.expect_any(expected_symbols.iter().map(|&symbol| TokenType::Symbol(symbol)).collect())	
	}

	pub fn optional(&mut self, optional_token: TokenType) -> Option<Token> {
		match self.tokens.peek() {
			Some(token) if token.token_type == optional_token => self.tokens.next(),
			_ => None
		}
	}

	pub fn optional_symbol(&mut self, optional_symbol: Symbol) -> Option<Token> {
		self.optional(TokenType::Symbol(optional_symbol))
	}

	pub fn optional_eol(&mut self) -> Option<Token> {
		if let token @ Some(_) = self.optional_symbol(Symbol::SemiColon) { return token }
		if let token @ Some(_) = self.optional(TokenType::EOL) { return token }
		None
	} 
}
