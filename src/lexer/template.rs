use std::{iter::Peekable, vec::IntoIter};

use crate::{Error, Result, SourcePos, lexer::Lexer};

use super::tokens::{Literal, Symbol, Token, TokenType};

pub struct TemplateLexer {
	raw_data: Peekable<IntoIter<char>>,
	pos: SourcePos,
}

impl TemplateLexer {
	pub fn new(text: &str, pos: SourcePos) -> Self {
		Self {
			raw_data: text.chars().collect::<Vec<_>>().into_iter().peekable(),
			pos
		}
	}

	fn next_data(&mut self) -> Option<char> {
		self.pos.column += 1;
		self.raw_data.next()
	}

	pub fn run(&mut self) -> Result<Token> {
		let mut tokens: Vec<Token> = Vec::new();

		loop {
			match self.next_data() {
				None => return Token::create(TokenType::Template(tokens), self.pos),
				Some(c) if c == '#' => {
					match self.raw_data.peek() {
						Some(peek) if *peek == '{' => {
							self.next_data();
							let mut value = String::new();
							
							loop {
								match self.raw_data.peek() {
									Some(c) if *c != '}' => {
										value.push(*c);
										self.next_data();
									}
									Some(_) => break,
									None => return Error::create("Expected }".to_string(), self.pos),
								}
							}

							self.next_data();

							tokens.push(Token::new(TokenType::Symbol(Symbol::HashtagOpenBracket), self.pos));
		
							let lexer = Lexer::from_text(&value, self.pos);
							for token_res in lexer.into_iter() {
								match token_res {
									Ok(token) => tokens.push(token),
									Err(e) => return Err(e),
								}
							}

							tokens.push(Token::new(TokenType::Symbol(Symbol::CloseBracket), self.pos))
						}
						_ => {
							let mut value = c.to_string();
							loop {
								match self.raw_data.peek() {
									Some(c) if *c != '#' => {
										value.push(*c);
										self.next_data();
									}
									_ => break,
								}
							}
							tokens.push(Token::new(TokenType::Literal(Literal::Str(value)), self.pos))
						}
					}
				}
				Some(c) => {
					let mut value = c.to_string();
					loop {
						match self.raw_data.peek() {
							Some(c) if *c != '#' => {
								value.push(*c);
								self.next_data();
							}
							_ => break,
						}
					}
					tokens.push(Token::new(TokenType::Literal(Literal::Str(value)), self.pos))
				},
			}
		}

	}
}
