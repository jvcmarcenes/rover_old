
pub mod function;
pub mod component;

use std::{collections::HashMap, fmt::Display, ops::{Add, Div, Mul, Rem, Sub}, result::Result};

use crate::{SourcePos, Error};

use self::{function::*, component::*};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
	Void,
	Str(String),
	Num(f32),
	Bool(bool),
	List(Vec<ValueObject>),
	Map(HashMap<String, ValueObject>),
	Function(Function),
}

impl Into<ValueObject> for Value {
	fn into(self) -> ValueObject {
		ValueObject {
			value: self,
			components: Vec::new()
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct ValueObject {
	pub value: Value,
	components: Vec<Component>,
}

impl ValueObject {
	pub fn new(value: Value) -> Self { Self { value, components: Vec::new() } }

	pub fn to_bool(&self, pos: SourcePos) -> crate::Result<bool> {
		if let Value::Bool(b) = self.value { Ok(b) }
		else { Error::create("Expected a boolean value".to_string(), pos) }
	}

	pub fn to_num(&self, pos: SourcePos) -> crate::Result<f32> {
		if let Value::Num(n) = self.value { Ok(n) }
		else { Error::create("Expected a numeric value".to_string(), pos) }
	}

	pub fn to_list(&self, pos: SourcePos) -> crate::Result<Vec<Self>> {
		if let Value::List(list) = self.value.clone() { Ok(list) }
		else { Error::create("Expected a list".to_string(), pos) }
	}

	pub fn to_map(&self, pos: SourcePos) -> crate::Result<HashMap<String, ValueObject>> {
		if let Value::Map(table) = self.value.clone() { Ok(table) }
		else { Error::create("Expected a map".to_string(), pos) }
	}

	pub fn math_op(f: fn(f32, f32) -> f32, lhs: &Self, rhs: &Self) -> Result<ValueObject, String> {
		if let (Value::Num(ln), Value::Num(rn)) = (lhs.value.clone(), rhs.value.clone()) {
			Ok(Value::Num(f(ln, rn)).into())
		} else {
			Err("Invalid operator for types".to_string())
		}
	}
	
	pub fn bool_op(f: fn(bool, bool) -> bool, lhs: Self, rhs: Self) -> Result<ValueObject, String> {
		if let (Value::Bool(lb), Value::Bool(rb)) = (lhs.value, rhs.value) {
			Ok(Value::Bool(f(lb, rb)).into())
		} else {
			Err("Invalid operator for types".to_string())
		}
	}
}

impl Display for ValueObject {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self.value.clone() {
			Value::Void => write!(f, ""),
			Value::Str(s) => write!(f, "{}", s),
			Value::Num(n) => write!(f, "{}", n),
			Value::Bool(b) => write!(f, "{}", b),
			Value::List(list) => {
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
			Value::Map(map) => {
				fn print_map(level: &str, map: &HashMap<String, ValueObject>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
					write!(f, "{{\n")?;
					for (key, value_obj) in map.iter() {
						if let Value::Map(in_map) = value_obj.value.clone() {
							write!(f, "  {}{} = ", level, key)?;
							print_map(&format!("  {}", level), &in_map, f)?;
						} else {
							write!(f, "  {}{} = {}\n", level, key, value_obj)?;
						}
					}
					write!(f, "{}}}\n", level)?;
					Ok(())
				}
				print_map("", &map, f)
			},
			Value::Function(_) => panic!("Can't write out a function")
		}
	}
}

impl Add for ValueObject {
	type Output = Result<Self, String>;

	fn add(self, rhs: Self) -> Self::Output {
		match (self.value.clone(), rhs.value.clone()) {
			(Value::List(list), _) => {
				let mut res = list.clone();
				res.push(rhs);
				Ok(ValueObject::new(Value::List(res)))
			}
			(Value::Str(_), _) | (_, Value::Str(_)) => Ok(ValueObject::new(Value::Str(format!("{}{}", self, rhs)))),
			(Value::Num(_), Value::Num(_)) => Self::math_op(|a, b| a + b, &self, &rhs),
			_ => Err("Invalid operator for type".to_string()),
		}
	}
}

impl Sub for ValueObject {
	type Output = Result<Self, String>;

	fn sub(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a - b, &self, &rhs) }
}

impl Mul for ValueObject {
	type Output = Result<Self, String>;

	fn mul(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a * b, &self, &rhs) }
}

impl Div for ValueObject {
	type Output = Result<ValueObject, String>;

	fn div(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a / b, &self, &rhs) }
}

impl Rem for ValueObject {
	type Output = Result<ValueObject, String>;

	fn rem(self, rhs: Self) -> Self::Output { Self::math_op(|a, b| a % b, &self, &rhs) }
}

impl PartialOrd for ValueObject {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		match (self.value.clone(), other.value.clone()) {
			(Value::Str(l0), Value::Str(r0)) => l0.partial_cmp(&r0),
			(Value::Num(l0), Value::Num(r0)) => l0.partial_cmp(&r0),
			(Value::Bool(l0), Value::Bool(r0)) => l0.partial_cmp(&r0),
			_ => None,
		}
	}
}
