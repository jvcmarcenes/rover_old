
mod lexer;

use crate::lexer::*;

fn main() {
	let lexer = Lexer::from_file("./test.mars").unwrap();
	for token in lexer {
		match token {
			Ok(t) => println!("{:?}", t),
			Err(e) => println!("Error! {}", e),
		}
	}
}
