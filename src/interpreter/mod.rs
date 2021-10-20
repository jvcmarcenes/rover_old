
pub mod evaluator;

use std::collections::HashMap;

use crate::*;
use crate::parser::{expression::*, program::*, statement::*};

use self::evaluator::Value;

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
					Statement::Writeline { expr } => self.execute_writeline(*expr.clone()),
					Statement::Assignment { name, expr } => self.execute_assigment(name, *expr.clone())
				}
			} else {
				return Ok(())
			}
		}
	}

	fn execute_writeline(&mut self, expr: Expression) {
		let value = self.evaluate(expr).unwrap();
		println!("{}", value);
	}

	fn execute_assigment(&mut self, name: String, expr: Expression) {
		let value = self.evaluate(expr).unwrap();
		self.symbol_table.insert(name, value);
	}

}