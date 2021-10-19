
pub mod tokens;

use std::{fs, io, iter::Peekable, vec::IntoIter};
// use log::trace;

use crate::lexer::tokens::*;

type Result<T> = std::result::Result<T, String>;

pub struct Lexer {
	raw_data: Peekable<IntoIter<char>>,
	pos: SourcePos,
}

impl Lexer {
	pub fn from_text(text: &str) -> Self {
		// println!("{:?}", text);
		Lexer {
			raw_data: text.chars().collect::<Vec<_>>().into_iter().peekable(),
			pos: SourcePos { line: 0, column: 0 }
		}
	}

	pub fn from_file(file_path: &str) -> io::Result<Self> {
		Ok(Self::from_text(&fs::read_to_string(file_path)?))
	}

	fn get_next_char_while(&mut self, raw_token: &mut String, cond: fn(char) -> bool) {
		loop {
			match self.raw_data.peek() {
				Some(c) if cond(*c) => {
					raw_token.push(*c);
					self.raw_data.next();
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

		self.pos.column += 1;

		loop {
			match self.raw_data.next() {
				Some(c) if c.is_whitespace() => {
					if c == '\n' || (c == '\r' && self.raw_data.next().unwrap() == '\n') {
						self.pos.line += 1;
						self.pos.column = 0;
						return Some(Ok(Token::new(TokenType::EOL, self.pos)));
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
			token = if let Some(keyword) = get_keyword(&name) {
				Ok(Token::new(TokenType::Keyword(keyword), self.pos))
			} else {
				Ok(Token::new(TokenType::Identifier(name), self.pos))
			}
		} else if first_char.is_numeric() {
			let mut value = first_char.to_string();
			self.get_next_char_while(&mut value, |c| c.is_numeric() || c == '.');
			token = match value.parse::<f32>() {
				Ok(n) => Ok(Token::new(TokenType::Literal(Literal::Num(n)), self.pos)),
				Err(_) => Err(format!("Numeral literal {} is invalid", value)),
			}
		} else if first_char == '"' {
			let mut value = String::new();
			self.get_next_char_while(&mut value, |c| c != '"');
			self.raw_data.next();
			token = Ok(Token::new(TokenType::Literal(Literal::Str(value)), self.pos));
		} else {
			let mut raw = first_char.to_string();
			loop {
				if let Some(peek) = self.raw_data.peek() {
					if is_valid_symbol(*peek) {
						raw.push(*peek);
					} else { break }
				} else { break }
			}
			token = if let Some(symbol) = get_symbol(&raw) {
				match symbol {
					Symbol::Hashtag => {
						self.get_next_char_while(&mut String::new(), |c| c != '\n');
						Ok(Token::new(TokenType::Comment, self.pos))
					}
					_ => Ok(Token::new(TokenType::Symbol(symbol), self.pos))
				}
			} else {
				Err(format!("Unknow token: {}", raw))
			}
		}

		Some(token)
	}
}
