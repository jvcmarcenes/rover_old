
pub mod function;

use std::{collections::HashMap, fmt::Display, ops::{Add, Div, Mul, Rem, Sub}, result::Result};

use crate::{SourcePos, Error};

use self::function::*;

#[derive(Debug, PartialEq, Clone)]
pub enum ValueData {
	Void,
	Str(String),
	Num(f64),
	Bool(bool),
	List(Vec<Value>),
	Map(HashMap<String, Value>),
	Function(Function),
}

impl Into<Value> for ValueData {
	fn into(self) -> Value {
		Value {
			data: self,
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct Value {
	pub data: ValueData,
}

impl Value {
	pub fn new(value: ValueData) -> Self { Self { data: value } }

	pub fn to_bool(&self, pos: SourcePos) -> crate::Result<bool> {
		if let ValueData::Bool(b) = self.data { Ok(b) }
		else { Error::create("Expected a boolean value".to_string(), pos) }
	}

	pub fn to_num(&self, pos: SourcePos) -> crate::Result<f64> {
		if let ValueData::Num(n) = self.data { Ok(n) }
		else { Error::create("Expected a numeric value".to_string(), pos) }
	}

	pub fn to_list(&self, pos: SourcePos) -> crate::Result<Vec<Self>> {
		if let ValueData::List(list) = self.data.clone() { Ok(list) }
		else { Error::create("Expected a list".to_string(), pos) }
	}

	pub fn to_map(&self, pos: SourcePos) -> crate::Result<HashMap<String, Value>> {
		if let ValueData::Map(table) = self.data.clone() { Ok(table) }
		else { Error::create("Expected a map".to_string(), pos) }
	}

	pub fn math_op(f: fn(f64, f64) -> f64, lhs: &Self, rhs: &Self) -> Result<Value, String> {
		if let (ValueData::Num(ln), ValueData::Num(rn)) = (lhs.data.clone(), rhs.data.clone()) {
			Ok(ValueData::Num(f(ln, rn)).into())
		} else {
			Err("Invalid operator for types".to_string())
		}
	}
	
	pub fn bool_op(f: fn(bool, bool) -> bool, lhs: Self, rhs: Self) -> Result<Value, String> {
		if let (ValueData::Bool(lb), ValueData::Bool(rb)) = (lhs.data, rhs.data) {
			Ok(ValueData::Bool(f(lb, rb)).into())
		} else {
			Err("Invalid operator for types".to_string())
		}
	}
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self.data.clone() {
			ValueData::Void => write!(f, ""),
			ValueData::Str(s) => write!(f, "{}", s),
			ValueData::Num(n) => write!(f, "{}", n),
			ValueData::Bool(b) => write!(f, "{}", b),
			ValueData::List(list) => {
				write!(f, "[")?;
				let mut i = 0;
				loop {
					if i >= list.len() { break; }
					write!(f, "{}", list[i])?;
					if i + 1 < list.len() { write!(f, ", ")?; }
					i += 1;
				}
				write!(f, "]")?;
				Ok(())
			},
			ValueData::Map(map) => {
				fn print_map(level: &str, map: &HashMap<String, Value>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
					write!(f, "{{\n")?;
					for (key, value_obj) in map.iter() {
						match value_obj.data.clone() {
							ValueData::Map(in_map) => {
								write!(f, "  {}{} = ", level, key)?;
								print_map(&format!("  {}", level), &in_map, f)?;
							}
							ValueData::Function(_) => (),
							_ => write!(f, "  {}{} = {}\n", level, key, value_obj)?,
						}
					}
					write!(f, "{}}}\n", level)?;
					Ok(())
				}
				print_map("", &map, f)
			},
			ValueData::Function(_) => panic!("Can't write out a function"),
		}
	}
}

impl Add for Value {
	type Output = Result<Self, String>;

	fn add(self, rhs: Self) -> Self::Output {
		match (self.data.clone(), rhs.data.clone()) {
			(ValueData::List(list), _) => {
				let mut res = list.clone();
				res.push(rhs);
				Ok(Value::new(ValueData::List(res)))
			}
			(ValueData::Str(_), _) | (_, ValueData::Str(_)) => Ok(Value::new(ValueData::Str(format!("{}{}", self, rhs)))),
			(ValueData::Num(_), ValueData::Num(_)) => Self::math_op(|a, b| a + b, &self, &rhs),
			_ => Err("Invalid operator for type".to_string()),
		}
	}
}

impl Sub for Value {
	type Output = Result<Self, String>;

	fn sub(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a - b, &self, &rhs) }
}

impl Mul for Value {
	type Output = Result<Self, String>;

	fn mul(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a * b, &self, &rhs) }
}

impl Div for Value {
	type Output = Result<Value, String>;

	fn div(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a / b, &self, &rhs) }
}

impl Rem for Value {
	type Output = Result<Value, String>;

	fn rem(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a % b, &self, &rhs) }
}

impl PartialOrd for Value {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		match (self.data.clone(), other.data.clone()) {
			(ValueData::Str(l0), ValueData::Str(r0)) => l0.partial_cmp(&r0),
			(ValueData::Num(l0), ValueData::Num(r0)) => l0.partial_cmp(&r0),
			(ValueData::Bool(l0), ValueData::Bool(r0)) => l0.partial_cmp(&r0),
			_ => None,
		}
	}
}
