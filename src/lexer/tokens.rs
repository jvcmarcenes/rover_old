
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
	Num(f32),
	Str(String),
	// Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
	LeftPar, RightPar, LeftBracket, RightBracket, LeftSqr, RightSqr, LeftAng, RightAng,
	Period, Comma, SemiColon, Colon,
	Equals, PlusEquals, MinusEquals,
	Plus, Minus, Asterisk, Slash, Percent,
	DoubleEquals, ExclamEquals, LeftAngEquals, RightAngEquals,
	DoubleAmper, DoubleBar, Exclam,
	SingleQuote, DoubleQuotes,
	Hashtag, HashtagLeftBracket
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
	Writeline
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SourcePos {
	pub line: i32,
	pub column: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
	pub token_type: TokenType,
	pub src: SourcePos
}

impl Token {
	pub fn new(token_type: TokenType, src: SourcePos) -> Self {
		Token { token_type, src }
	}
}

pub fn get_keyword(s: &str) -> Option<Keyword> {
	match s {
		"writeline" => Some(Keyword::Writeline),
		_ => None
	}
}

const valid_symbols: &[char] = &[
	'#', '!', '%', '+', '-', '/', '*', '=', '{', '}', '(', ')', '[', ']', ':', '.', ',', ';', '&', '|', '\'', '"', '<', '>', '?'
];

pub fn is_valid_symbol(c: char) -> bool {
	valid_symbols.contains(&c)
}

pub fn get_symbol(s: &str) -> Option<Symbol> {
	match s {
		"#" => Some(Symbol::Hashtag),
		"\"" => Some(Symbol::DoubleQuotes),
		_ => None,
	}
}