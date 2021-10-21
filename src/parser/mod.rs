
pub mod expression;
pub mod statement;
pub mod block;

use crate::*;
use crate::lexer::tokens::*;

use std::{iter::Peekable, vec::IntoIter};

type TokenIter = Peekable<IntoIter<Token>>;

pub struct Parser {
	tokens: TokenIter,
}

impl Parser {
	pub fn new(tokens: TokenIter) -> Self {
		// println!("{:#?}", tokens);
		Parser { tokens: tokens.filter(|token| token.token_type != TokenType::Comment).collect::<Vec<Token>>().into_iter().peekable() }
	}

	pub fn expect(&mut self, expected_type: TokenType) -> Result<()> {
		match self.tokens.peek() {
			Some(token) => {
				if token.token_type == expected_type {
					self.tokens.next();
					Ok(())
				} else { Error::create(format!("Expected {:?}, found {:?}", expected_type, token.token_type), token.pos) }
			}
			_ => Error::create(format!("Expected {:?}, found EOF", expected_type), SourcePos { line: 0, column: 0 })
		}
	}

	pub fn expect_any(&mut self, expected_types: Vec<TokenType>) -> Result<()> {
		match self.tokens.peek() {
			Some(token) => {
				if expected_types.contains(&token.token_type) {
					self.tokens.next();
					Ok(())
				} else { Error::create(format!("Expected {:?}, found {:?}", expected_types, token.token_type), token.pos) }
			}
			_ => Error::create(format!("Expected {:?}, found EOF", expected_types), SourcePos { line: 0, column: 0 })
		}
	}

	pub fn expect_symbol(&mut self, expected_symbol: Symbol) -> Result<()> {
		self.expect(TokenType::Symbol(expected_symbol))
	}

	pub fn expect_any_symbol(&mut self, expected_symbols: Vec<Symbol>) -> Result<()> {
		self.expect_any(expected_symbols.iter().map(|&symbol| TokenType::Symbol(symbol)).collect())	
	}
}
