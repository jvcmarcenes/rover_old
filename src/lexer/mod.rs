
pub mod tokens;
pub mod template;

use std::ops::Add;
use std::{fs, io, iter::Peekable, vec::IntoIter};

use crate::lexer::tokens::*;
use crate::{SourcePos, Result, Error};

use self::template::TemplateLexer;

pub struct Lexer {
	raw_data: Peekable<IntoIter<char>>,
	pos: SourcePos,
}

impl Lexer {

	pub fn from_text(text: &str, pos: SourcePos) -> Self {
		// println!("{:?}", text);
		Lexer {
			raw_data: text.chars().collect::<Vec<_>>().into_iter().peekable(),
			pos
		}
	}

	pub fn from_file(file_path: &str) -> io::Result<Self> {
		let text = fs::read_to_string(file_path)?;
		Ok(Self::from_text(&format!("{}\r\n\r\n", text), SourcePos::new(1, 1)))
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
		} else if first_char == '\'' {
			let mut value = String::new();
			self.get_next_char_while(&mut value, |c| c != '\'');
			self.next_data();

			let mut template_lexer = TemplateLexer::new(&value, self.pos);
			token = template_lexer.run();
		} else {
			let mut raw = first_char.to_string();
			loop {
				if let Some(peek) = self.raw_data.peek() {
					raw.push(*peek);
					if Symbol::get(&raw).is_none() {
						raw.pop();
						break;
					}
					self.next_data();
				} else { break }
			}
			token = if let Some(symbol) = Symbol::get(&raw) {
				match symbol {
					Symbol::Hashtag => {
						self.get_next_char_while(&mut String::new(), |c| c != '\n');
						Token::create(TokenType::Comment, pos)
					}
					Symbol::OpenParHashtag => {
						loop {
							if let (Some(c1), Some(c2)) = (self.next_data(), self.raw_data.peek()) {
								let s = c1.to_string().add(&c2.to_string());
								if s == "#)" {
									self.next_data();
									break;
								}
							} else { return Some(Error::create("Multiline comment not closed".to_string(), pos)) }
						}
						Token::create(TokenType::Comment, pos)
					}
					Symbol::SemiColon => Token::create(TokenType::EOL, pos),
					_ => Token::create(TokenType::Symbol(symbol), pos)
				}
			} else {
				Error::create(format!("Unknow token: {}", raw), pos)
			}
		}

		Some(token)
	}
}
