
use std::{fmt::Display, ops::Add, primitive, result::Result};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
	Str(String),
	Num(f32),
	Bool(bool),
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Str(s) => write!(f, "{}", s),
			Value::Num(n) => write!(f, "{}", n),
			Value::Bool(b) => write!(f, "{}", b),
		}
	}
}

impl Add for Value {
	type Output = Result<Self, String>;

	fn add(self, rhs: Self) -> Self::Output {
		let res = match self {
			Self::Str(s) => Value::Str(format!("{}{}", s, rhs)),
			Self::Num(n) => {
				match rhs {
					Self::Num(rn) => Value::Num(n + rn),
					_ => return Err(String::from("Invalid operator for types"))
				}
			},
			Self::Bool(_) => return Err(String::from("Invalid operator for boolean type")),
		};
		Ok(res)
	}
}