
use crate::lexer::*;
use crate::SourcePos;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
	Num(f32),
	Str(String),
	Bool(bool),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Symbol {
	OpenPar, ClosePar, OpenBracket, CloseBracket, OpenSqr, CloseSqr, OpenAng, CloseAng,
	Period, Comma, SemiColon,
	Equals, PlusEquals, MinusEquals,
	Plus, Minus, Asterisk, Slash, Percent,
	DoubleEquals, ExclamEquals, OpenAngEquals, CloseAngEquals,
	DoubleAmper, DoubleBar, Exclam,
	SingleQuote, DoubleQuotes,
	Hashtag, HashtagOpenBracket
}

impl Symbol {
	// const VALID_SYMBOLS: [char; 25] = [
	// 	'#', '!', '%', '+', '-', '/', '*', '=', '{', '}', '(', ')', '[', ']', ':', '.', ',', ';', '&', '|', '\'', '"', '<', '>', '?'
	// ];

	// pub fn is_valid(c: char) -> bool {
	// 	Self::VALID_SYMBOLS.contains(&c)
	// }

	pub fn get(s: &str) -> Option<Symbol> {
		let symbol = match s {
			"(" => Symbol::OpenPar,
			")" => Symbol::ClosePar,
			"{" => Symbol::OpenBracket,
			"}" => Symbol::CloseBracket,
			"[" => Symbol::OpenSqr,
			"]" => Symbol::CloseSqr,
			"<" => Symbol::OpenAng,
			">" => Symbol::CloseAng,
			"." => Symbol::Period,
			"," => Symbol::Comma,
			";" => Symbol::SemiColon,
			"=" => Symbol::Equals,
			"+=" => Symbol::PlusEquals,
			"-=" => Symbol::MinusEquals,
			"+" => Symbol::Plus,
			"-" => Symbol::Minus,
			"*" => Symbol::Asterisk,
			"/" => Symbol::Slash,
			"%" => Symbol::Percent,
			"==" => Symbol::DoubleEquals,
			"!=" => Symbol::ExclamEquals,
			"<=" => Symbol::OpenAngEquals,
			">=" => Symbol::CloseAngEquals,
			"&&" => Symbol::DoubleAmper,
			"||" => Symbol::DoubleBar,
			"!" => Symbol::Exclam,
			"'" => Symbol::SingleQuote,
			"\"" => Symbol::DoubleQuotes,
			"#" => Symbol::Hashtag,
			"#{" => Symbol::HashtagOpenBracket,
			_ => return None,
		};
		Some(symbol)
	}
}

const UNARY_OPERATORS: &[Symbol] = &[Symbol::Minus, Symbol::Exclam];
pub fn is_unary_symbol(s: &Symbol) -> bool {
	UNARY_OPERATORS.contains(s)
}

const BINARY_OPERATORS: &[Symbol] = &[
	Symbol::Plus, Symbol::Minus, Symbol::Asterisk, Symbol::Slash, Symbol::Percent, Symbol::DoubleAmper, Symbol::DoubleBar,
	Symbol::DoubleEquals, Symbol::ExclamEquals, Symbol::OpenAng, Symbol::CloseAng, Symbol::OpenAngEquals, Symbol::CloseAngEquals
];
pub fn is_binary_symbol(s: &Symbol) -> bool {
	BINARY_OPERATORS.contains(s)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Keyword {
	True, False,
	Write, Writeline, Read
}

impl Keyword {
	pub fn get(s: &str) -> Option<Self> {
		let keyword = match s {
			"true" => Keyword::True,
			"false" => Keyword::False,
			"write" => Keyword::Write,
			"writeline" => Keyword::Writeline,
			"read" => Keyword::Read,
			_ => return None,
		};
		Some(keyword)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
	Identifier(String),
	Literal(Literal),
	Symbol(Symbol),
	Keyword(Keyword),
	Comment,
	EOL,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
	pub token_type: TokenType,
	pub pos: SourcePos
}

impl Token {
	pub fn new(token_type: TokenType, pos: SourcePos) -> Self {
		Token { token_type, pos }
	}

	pub fn create(token_type: TokenType, pos: SourcePos) -> Result<Token> {
		Ok(Self::new(token_type, pos))
	}
}
