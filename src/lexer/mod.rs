
pub mod tokens;

use std::{fs, io, iter::Peekable, vec::IntoIter};
// use log::trace;

use crate::lexer::tokens::*;
use crate::{SourcePos, Result, Error};

pub struct Lexer {
	raw_data: Peekable<IntoIter<char>>,
	pos: SourcePos,
}

impl Lexer {
	pub fn from_text(text: &str) -> Self {
		let text = format!("{}\r\n\r\n", text); // inserts extra line skips at the end of the file
		// println!("{:?}", text);
		Lexer {
			raw_data: text.chars().collect::<Vec<_>>().into_iter().peekable(),
			pos: SourcePos { line: 1, column: 1 }
		}
	}

	pub fn from_file(file_path: &str) -> io::Result<Self> {
		Ok(Self::from_text(&fs::read_to_string(file_path)?))
	}

	fn next_data(&mut self) -> Option<char> {
		let next = self.raw_data.next();
		self.pos.column += 1;

		if let Some(c) = next {
			if c == '\n' {
				self.pos.line += 1;
				self.pos.column = 1;
			}
		}

		next
	}

	fn get_next_char_while(&mut self, raw_token: &mut String, cond: fn(char) -> bool) {
		loop {
			match self.raw_data.peek() {
				Some(c) if cond(*c) => {
					raw_token.push(*c);
					self.next_data();
				}
				_ => break
			}
		}
	}

	fn is_identifier(c: char) -> bool {
		c.is_ascii_alphanumeric() || c == '_'
	}
}

impl Iterator for Lexer {
	type Item = Result<Token>;

	fn next(&mut self) -> Option<Self::Item> {
		let token: Result<Token>;

		let first_char: char;
		let pos = self.pos;

		loop {
			match self.next_data() {
				Some(c) if c.is_whitespace() => {
					// self.pos.column += 1;
					if c == '\n' {
						return Some(Token::create(TokenType::EOL, pos));
					}
				},
				Some(c) => {
					first_char = c;
					break;
				},
				None => return None,
			}
		}

		if Self::is_identifier(first_char) && !first_char.is_numeric() {
			let mut name = first_char.to_string();
			self.get_next_char_while(&mut name, Self::is_identifier);
			token = if let Some(keyword) = Keyword::get(&name) {
				let t = match keyword {
					Keyword::True => TokenType::Literal(Literal::Bool(true)),
					Keyword::False => TokenType::Literal(Literal::Bool(false)),
					_ => TokenType::Keyword(keyword),
				};
				Token::create(t, pos)
			} else {
				Token::create(TokenType::Identifier(name), pos)
			}
		} else if first_char.is_numeric() {
			let mut value = first_char.to_string();
			self.get_next_char_while(&mut value, |c| c.is_numeric() || c == '.');
			token = match value.parse::<f32>() {
				Ok(n) => Token::create(TokenType::Literal(Literal::Num(n)), pos),
				Err(_) => Error::create(format!("Numeral literal {} is invalid", value), pos),
			}
		} else if first_char == '"' {
			let mut value = String::new();
			self.get_next_char_while(&mut value, |c| c != '"');
			self.next_data();
			token = Token::create(TokenType::Literal(Literal::Str(value)), pos);
		} else {
			let mut raw = first_char.to_string();
			loop {
				if let Some(peek) = self.raw_data.peek() {
					raw.push(*peek);
					if Symbol::get(&raw).is_none() {
						raw.pop();
						break;
					}
					self.raw_data.next();
				} else { break }
			}
			token = if let Some(symbol) = Symbol::get(&raw) {
				match symbol {
					Symbol::Hashtag => {
						self.get_next_char_while(&mut String::new(), |c| c != '\n');
						Token::create(TokenType::Comment, pos)
					}
					_ => Token::create(TokenType::Symbol(symbol), pos)
				}
			} else {
				Error::create(format!("Unknow token: {}", raw), pos)
			}
		}

		Some(token)
	}
}
