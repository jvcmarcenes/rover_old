
mod lexer;
mod parser;
mod interpreter;

use parser::block::*;

use crate::{lexer::*, parser::*, interpreter::*};

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
	pub fn new(message: String, pos: SourcePos) -> Self {
		Self { message, pos }
	}
	pub fn create<T>(message: String, pos: SourcePos) -> Result<T> {
		Err(Self::new(message, pos))
	}
}

pub type Result<T> = std::result::Result<T, Error>;

macro_rules! unwrap_or_exit {
	($f:expr, $origin:tt, $path:expr) => {
		match $f {
			Ok(a) => a,
			Err(e) => {
				println!("{} Error [{}:{}:{}]: {}", $origin, $path, e.pos.line, e.pos.column, e.message);
				process::exit(0);
			}
		}
	};
}

fn get_ast(path: &str) -> Block {
	let lexer = Lexer::from_file(path).unwrap();
	let tokens = lexer.map(|token| unwrap_or_exit!(token, "Lexing", path)).collect::<Vec<_>>();

	// for token in tokens.clone() { println!("{:?}", token); }

	let mut parser = Parser::new(tokens.into_iter().peekable());
	unwrap_or_exit!(parser.parse_program(), "Parsing", path)
}

#[derive(StructOpt)]
struct Cli {
	#[structopt(parse(from_os_str))]
	path: PathBuf,
}

fn main() {
	let args = Cli::from_args();
	let path = &args.path.into_os_string().into_string().unwrap();

	let program = get_ast(path);

	// println!("{:#?}", program);

	let result = Interpreter::new(program).run();
	
	unwrap_or_exit!(result, "Execution", path);
}
