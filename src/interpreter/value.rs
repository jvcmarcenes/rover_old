
use std::{fmt::Display, ops::{Add, Div, Mul, Rem, Sub}, result::Result};

use crate::{SourcePos, Error};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
	Str(String),
	Num(f32),
	Bool(bool),
}

impl Value {
	pub fn to_bool(&self, pos: SourcePos) -> crate::Result<bool> {
		if let Self::Bool(b) = self { Ok(*b) }
		else { Error::create("Expected a boolean value".to_string(), pos) }
	}

	#[allow(dead_code)]
	pub fn to_num(&self, pos: SourcePos) -> crate::Result<f32> {
		if let Self::Num(n) = self { Ok(*n) }
		else { Error::create("Expected a numeric value".to_string(), pos) }
	}

	pub fn math_op(f: fn(f32, f32) -> f32, lhs: Self, rhs: Self) -> Result<Value, String> {
		if let (Self::Num(ln), Self::Num(rn)) = (lhs, rhs) {
			Ok(Value::Num(f(ln, rn)))
		} else {
			Err("Invalid operator for types".to_string())
		}
	}
	
	pub fn bool_op(f: fn(bool, bool) -> bool, lhs: Self, rhs: Self) -> Result<Value, String> {
		if let (Self::Bool(lb), Self::Bool(rb)) = (lhs, rhs) {
			Ok(Value::Bool(f(lb, rb)))
		} else {
			Err("Invalid operator for types".to_string())
		}
	}
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
		match self {
			Self::Str(_) => Ok(Self::Str(format!("{}{}", self, rhs))),
			Self::Num(_) => {
				match rhs {
					Self::Str(_) => Ok(Self::Str(format!("{}{}", self, rhs))),
					_ => Self::math_op(|a, b| a + b, self, rhs)
				}
			}
			Self::Bool(_) => Err(String::from("Invalid operator for boolean type")),
		}
	}
}

impl Sub for Value {
	type Output = Result<Self, String>;

	fn sub(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a - b, self, rhs) }
}

impl Mul for Value {
	type Output = Result<Self, String>;

	fn mul(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a * b, self, rhs) }
}

impl Div for Value {
	type Output = Result<Value, String>;

	fn div(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a / b, self, rhs) }
}

impl Rem for Value {
	type Output = Result<Value, String>;

	fn rem(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a % b, self, rhs) }
}

impl PartialOrd for Value {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		match (self, other) {
			(Self::Str(l0), Self::Str(r0)) => l0.partial_cmp(r0),
			(Self::Num(l0), Self::Num(r0)) => l0.partial_cmp(r0),
			(Self::Bool(l0), Self::Bool(r0)) => l0.partial_cmp(r0),
			_ => None,
		}
	}
}
