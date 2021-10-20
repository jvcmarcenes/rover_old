
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

	pub fn expect_symbol(&mut self, expected_symbol: Symbol) -> Result<()> {
		self.expect(TokenType::Symbol(expected_symbol))
	}
}
