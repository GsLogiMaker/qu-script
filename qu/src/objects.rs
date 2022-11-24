
//! Defines all types and objects used by Qu.


//use qu_derive::qu_methods;
use std::{any::Any, ops::{Add, AddAssign}, fmt::Debug};

use qu_derive::qu_methods;

use crate::{
	QuVm,
	Qu,
	QuMsg,
	vm::{
		QuExtFn,
		QuMemId,
		QuExtVoidFn,
		QuRegId,
		QuAny,
		RegisterValue,
	},
	QuRegisterValue,
};


pub type QuExtFnData = (String, QuFnPtr, Vec<&'static str>, &'static str);
pub type QuExtVoidFnData = (String, QuFnVoidPtr, Vec<&'static str>);


/// Defines all the types supported by Qu.
#[derive(Debug, Default, Clone)]
pub enum QuType {
	#[default] Void,
	Int,
	Bool,
	String,
	Tuple(Vec<QuType>),
	Array,
	Dictionary,
	Object(usize),
} impl From<u8> for QuType {

	fn from(f:u8) -> Self { 
		match f {
			0 => QuType::Void,
			1 => QuType::Int,
			2 => QuType::Bool,
			3 => QuType::String,
			4 => panic!(),//QuType::Tuple(Vec::default()),
			5 => QuType::Array,
			6 => QuType::Dictionary,
			7 => QuType::Object(0),
			_ => panic!(),
		}
	}

}


/// Defines all the types supported by Qu.
#[derive(Debug, Default, Clone)]
pub enum QuValue {
	#[default] Void,
	Int(isize),
	Bool(bool),
	Object(usize),
}


/// Defines a block of code.
/// 
/// Often used for Qu functions.
#[derive(Debug, Default, Clone)]
pub struct QuCodeObject {
	/// Where the [`QuCodeObject`] starts. The [`QuCodeObject`] ends at the
	/// `END` VM instruction.
	pub start_index:usize,
} impl QuCodeObject {

	/// Constructs a new [`QuCodeObject`].
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::QuCodeObject;
	/// 
	/// let codeobj = QuCodeObject::new(0);
	/// ```
	pub fn new(start_index:usize) -> Self {
		return Self{start_index};
	}

}


/// Defines a Qu function.
#[derive(Debug, Default, Clone)]
pub struct QuFnObject {
	/// The input parameters of the function.
	pub parameters:Vec<QuType>,
	/// The function body.
	pub body:QuCodeObject,
	/// The variable type that this function returns.
	pub return_type:QuType,

} impl QuFnObject {

	pub fn new(parameters:Vec<QuType>, body:QuCodeObject, return_type:QuType
	) -> Self {
		Self {
			parameters,
			body,
			return_type,
		}
	}

}


pub struct QuFnPtr(pub QuExtFn);
impl Debug for QuFnPtr {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("QuFnPtr").finish()
    }

}
pub struct QuFnVoidPtr(pub QuExtVoidFn);
impl Debug for QuFnVoidPtr {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("QuFnVoidPtr").finish()
    }

}


#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct QuInt(pub i32);
#[qu_methods]
impl QuInt {

	pub fn new(value:i32) -> Self {Self(value)}


	pub fn boing(&self, b:f32) {}


	pub fn stats(&self, b:i32) -> i32 {self.0+b}

	
	fn test(&self) {
		//Self::quwrappernew(vm, args);
		//Self::quwrapperboing(vm, source_id, args);
		//Self::quwrapperstats(vm, args);
		
	}


} impl Add for QuInt {

	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {QuInt(self.0 + rhs.0)}

} impl AddAssign for QuInt {

	fn add_assign(&mut self, rhs: Self) {*self += rhs}

} impl QuAny for QuInt {
}


#[derive(Debug, Default, Clone, Copy)]
pub struct QuVoid();
impl QuRegisterStruct for QuVoid {

	fn get_name() -> &'static str where Self: Sized {
		"Void"
	}

} impl QuAny for QuVoid {
} impl From<QuInt> for String {

	fn from(quint:QuInt) -> Self {
		format!("{}", quint.0)
	}

} impl From<&QuInt> for String {

	fn from(quint:&QuInt) -> Self {
		format!("{}", quint.0)
	}

}


pub trait QuRegisterStruct {

	/// Returns functions to be called by the [`QuVm`].
	fn register_fns() -> Vec<QuExtFnData> where Self: Sized {
		Vec::default()
	}


	/// Returns functions to be called by the [`QuVm`].
	fn register_void_fns() -> Vec<QuExtVoidFnData> where Self: Sized {
		Vec::default()
	}


	/// Returns functions to be called by the [`QuVm`].
	fn register_static_fns() -> Vec<QuExtFnData> where Self: Sized {
		Vec::default()
	}


	/// Returns the name that identifies the struct being registered.
	fn get_name() -> &'static str where Self: Sized;

}


#[cfg(test)]
mod test_objects {
    use crate::{QuInt, QuRegisterStruct};


	#[test]
	fn register_fns() {
		dbg!(QuInt::get_name());
		let fns = QuInt::register_fns();
		let static_fns = QuInt::register_static_fns();
		let void_fns = QuInt::register_void_fns();
		dbg!(fns);
		dbg!(static_fns);
		dbg!(void_fns);
		//dbg!(QuInt::register_static_fns());
		//dbg!(QuInt::register_void_fns());
		//dbg!(QuInt::get_name());
	}

}
