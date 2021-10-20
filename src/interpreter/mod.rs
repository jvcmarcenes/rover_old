
pub mod evaluator;
pub mod value;

use std::collections::HashMap;

use crate::*;
use crate::parser::{expression::*, program::*, statement::*};

use self::value::*;

pub struct Interpreter {
	symbol_table: HashMap<String, Value>,
	program: Program,
}

impl Interpreter {

	pub fn new(program: Program) -> Self {
		Self {
			symbol_table: HashMap::new(),
			program,
		}
	}

	pub fn run(&mut self) -> Result<()> {
		loop {
			if let Some(statement) = self.program.statements.next() {
				match statement {
					Statement::Write { expr } => self.execute_write(*expr.clone()),
					Statement::Writeline { expr } => self.execute_writeline(*expr.clone()),
					Statement::Assignment { name, expr } => self.execute_assigment(name, *expr.clone())
				}
			} else {
				return Ok(())
			}
		}
	}

	fn execute_write(&mut self, expr: Expression) {
		let value = self.evaluate(Box::new(expr)).unwrap();
		print!("{}", value);
	}

	fn execute_writeline(&mut self, expr: Expression) {
		let value = self.evaluate(Box::new(expr)).unwrap();
		println!("{}", value);
	}

	fn execute_assigment(&mut self, name: String, expr: Expression) {
		let value = self.evaluate(Box::new(expr)).unwrap();
		self.symbol_table.insert(name, value);
	}

}