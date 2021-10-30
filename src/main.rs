
#![allow(dead_code)]

mod lexer;
mod parser;
mod interpreter;

use crate::{
	lexer::*,
	parser::{*, block::*}, 
	interpreter::*
};

use structopt::StructOpt;
use std::{path::PathBuf, process};

#[derive(Debug, Copy, Clone, PartialEq, Default)]
pub struct SourcePos {
	pub line: i32,
	pub column: i32,
}

impl SourcePos {
	fn new (line: i32, column: i32) -> Self { Self { line, column } }
}

#[derive(Debug, Clone)]
pub struct Error {
	message: String,
	pos: SourcePos,
}

impl Error {
	pub fn new(message: String, pos: SourcePos) -> Self { Self { message, pos } }
	pub fn create<T>(message: String, pos: SourcePos) -> Result<T> { Err(Self::new(message, pos)) }
}

pub type Result<T> = std::result::Result<T, Error>;

macro_rules! unwrap_or_exit {
	($f:expr, $origin:tt, $path:expr) => {
		match $f {
			Ok(a) => a,
			Err(e) => {
				eprintln!("{} {}: {}", 
					ansi_term::Colour::Red.paint(format!("{} error", $origin)),
					format!("[{}:{}:{}]", $path, e.pos.line, e.pos.column),
					e.message
				);
				process::exit(0);
			}
		}
	};
}

fn get_ast(path: &str) -> Block {

	let lexer = Lexer::from_file(path).unwrap_or_else(|e| {
		eprintln!("{}: {}", ansi_term::Color::Red.paint("system error"), e.to_string());
		process::exit(0);
	});

	let tokens = lexer.map(|token| unwrap_or_exit!(token, "lexing", path)).collect::<Vec<_>>();

	// for token in tokens.clone() { println!("{:?}", token); }

	let mut parser = Parser::new(tokens.into_iter().peekable());
	unwrap_or_exit!(parser.parse_program(), "parsing", path)
}

#[derive(StructOpt)]
struct Rover {
	#[structopt(parse(from_os_str))]
	path: PathBuf,
}

fn main() {
	ansi_term::enable_ansi_support().unwrap();

	let args = Rover::from_args();
	let path = args.path.into_os_string().into_string().unwrap();

	let program = get_ast(&path);

	// println!("{:#?}", program);

	let result = Interpreter::new(program).run();
	
	unwrap_or_exit!(result, "runtime", path);
}
