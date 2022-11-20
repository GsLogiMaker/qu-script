
//! Defines all types and objects used by Qu.

use std::{any::Any, ops::{Add, AddAssign}};

use crate::{
	QuVm,
	Qu,
	QuMsg,
	vm::{
		QuExtFn,
		QuMemId,
		QuVoidExtFn,
		QuRegId,
		QuAny,
		RegisterValue,
	},
	QuRegisterValue,
};


pub type QuExtFnData = (String, QuExtFn, Vec<usize>, usize);
pub type QuVoidFnForm = (String, QuVoidExtFn, Vec<usize>);
type QuMethodRegistration = (String, &'static dyn Fn(&mut QuVm));


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


#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct QuInt(pub i32);
impl QuInt {

	fn new(value:i32) -> Self {Self(value)}


	fn quwrapper_add(vm:&mut QuVm, obj_id:QuRegId, parameters:Vec<QuRegId>
	) -> Result<RegisterValue, QuMsg> {
		if parameters.len() != 1 {
			return Err(QuMsg::general("incorrect parameters quantity"));
		}
		let l = vm.reg_get_as::<QuInt>(obj_id)?;
		let r = vm.reg_get_as::<QuInt>(parameters[0])?;

		let output = *l+*r;
		Ok(Box::new(output))
	}


	fn quwrapper_lesser(vm:&mut QuVm, obj_id:QuRegId, parameters:Vec<QuRegId>
	) -> Result<RegisterValue, QuMsg> {
		if parameters.len() != 1 {
			return Err(QuMsg::general("incorrect parameters quantity"));
		}
		let l = vm.reg_get_as::<QuInt>(obj_id)?;
		let r = vm.reg_get_as::<QuInt>(parameters[0])?;

		let output = *l<*r;
		vm.is_zero = output;
		if output {
			Ok(Box::new(QuInt(1)))
		} else {
			Ok(Box::new(QuInt(0)))
		}
	}


//	fn quwrapper_to_string(vm:&mut QuVm, obj_id:QuRegId, parameters:Vec<QuRegId>
//	) -> Result<RegisterValue, QuMsg> {
//		if parameters.len() != 0 {
//			return Err(QuMsg::general("incorrect parameters quantity"));
//		}
//		let s = vm.reg_get_as::<QuInt>(obj_id)?;
//
//		Ok(Box::new(Into::<String>::into(s)))
//	}

} impl Add for QuInt {

	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {QuInt(self.0 + rhs.0)}

//} impl PartialOrd for QuInt {
//
//    fn partial_cmp(&self, other: &Self) -> Option<Self> {
//        Some(self.cmp(other))
//    }

} impl AddAssign for QuInt {

	fn add_assign(&mut self, rhs: Self) {*self += rhs}

} impl QuRegisterStruct for QuInt {
	
	fn register_fns() -> Vec<QuExtFnData> {
		return vec![
			("__add__".to_owned(), &Self::quwrapper_add, vec![0], 0),
			("__lesser__".to_owned(), &Self::quwrapper_lesser, vec![0], 0),
//			("__to_string__".to_owned(), &Self::quwrapper_to_string, vec![0], 0),
		];
	}


	fn register_void_fns() -> Vec<QuVoidFnForm> {
		return vec![];
	}


	fn register_methods() -> Vec<QuMethodRegistration> {
		return vec![];
	}


	fn get_name() -> String {
		"Int".to_owned()	
	}


//	fn debug_string(a:&dyn Any) -> String {
//		format!("{:?}", a.downcast_ref::<Self>())
//	}

} impl QuAny for QuInt {
}



#[derive(Debug, Default, Clone, Copy)]
pub struct QuVoid();
impl QuRegisterStruct for QuVoid {

	fn get_name() -> String where Self: Sized {
		"Void".to_owned()
	}

} impl QuAny for QuVoid {}


pub trait QuRegisterStruct {

	/// Returns functions to be called by the [`QuVm`].
	fn register_fns() -> Vec<QuExtFnData> where Self: Sized {
		Vec::default()
	}


	/// Returns functions to be called by the [`QuVm`].
	fn register_void_fns() -> Vec<QuVoidFnForm> where Self: Sized {
		Vec::default()
	}


	/// Returns functions to be called by the [`QuVm`].
	fn register_methods() -> Vec<QuMethodRegistration> where Self: Sized {
		Vec::default()
	}


	/// Returns the name that identifies the struct being registered.
	fn get_name() -> String where Self: Sized;


//	fn debug_string(a:&dyn Any) -> String {
//		format!("<debug_string not implemented>")
//	}

}


impl From<QuInt> for String {

	fn from(quint:QuInt) -> Self {
		format!("{}", quint.0)
	}

}


impl From<&QuInt> for String {

	fn from(quint:&QuInt) -> Self {
		format!("{}", quint.0)
	}

}


#[cfg(test)]
mod test_objects {

}