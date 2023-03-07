
//! Defines all types and objects used by Qu.

use std::{any::Any, ops::{Add, AddAssign, Sub, SubAssign}};

use crate::{QuVm, parser::QuOperator, import::QuStructId};
use crate::Qu;
use crate::QuMsg;
use crate::vm::QuExtFn;
use crate::vm::QuMemId;
use crate::vm::QuVoidExtFn;
use crate::vm::QuStackId;
use crate::vm::QuAny;
use crate::vm::StackValue;


pub type QuExtFnData = (String, QuExtFn, Vec<usize>, usize);
pub type QuVoidFnForm = (String, QuVoidExtFn, Vec<usize>);
type QuMethodRegistration = (String, &'static dyn Fn(&mut QuVm));


pub struct ExternalFunction {
	pub name: String,
	pub pointer: QuExtFn,
	pub parameters: Vec<QuStructId>,
	pub return_type: QuStructId,
} impl ExternalFunction {

	pub fn new(
		name:&str,
		pointer:QuExtFn,
		parameters:&[QuStructId],
		return_type:QuStructId,
	) -> Self{
		Self {
			name: name.into(),
			pointer,
			parameters: parameters.into(),
			return_type,
		}
	}


	pub fn call(
		&self, vm:&mut QuVm, parameters: &Vec<QuStackId>, output_id: QuStackId,
	) -> Result<(), QuMsg> {
		(self.pointer)(vm, parameters, output_id)
	}

}


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
pub struct QuInt(pub isize);
impl QuInt {

	fn new(value:isize) -> Self {Self(value)}


	fn quwrapper__add__(
		vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
	) -> Result<(), QuMsg> {
		assert_eq!(parameters.len(), 2);
		let l = vm.reg_get::<QuInt>(parameters[0])?;
		let r = vm.reg_get::<QuInt>(parameters[1])?;
		vm.reg_set(output_id, *l+*r);
		Ok(())
	}


	fn quwrapper__copy__(
		vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
	) -> Result<(), QuMsg> {
		assert_eq!(parameters.len(), 1);
		let s = vm.reg_get::<QuInt>(parameters[0])?;
		vm.reg_set(output_id, *s);
		Ok(())
	}


	fn quwrapper__greater__(
		vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
	) -> Result<(), QuMsg> {
		assert_eq!(parameters.len(), 2);
		let l = vm.reg_get::<QuInt>(parameters[0])?;
		let r = vm.reg_get::<QuInt>(parameters[1])?;

		let output = *l>*r;
		if output {
			vm.reg_set(output_id, QuInt(1));
		} else {
			vm.reg_set(output_id, QuInt(0));
		}
		vm.hold_is_zero = output;
		Ok(())
	}


	fn quwrapper__lesser__(
		vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
	) -> Result<(), QuMsg> {
		assert_eq!(parameters.len(), 2);
		let l = vm.reg_get::<QuInt>(parameters[0])?;
		let r = vm.reg_get::<QuInt>(parameters[1])?;

		let output = *l<*r;
		vm.hold_is_zero = output;
		if output {
			vm.reg_set(output_id, QuInt(1));
		} else {
			vm.reg_set(output_id, QuInt(0));
		}
		Ok(())
	}


	fn quwrapper__subtract__(
		vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
	) -> Result<(), QuMsg> {
		assert_eq!(parameters.len(), 2);
		let l = vm.reg_get::<QuInt>(parameters[0])?;
		let r = vm.reg_get::<QuInt>(parameters[1])?;
		vm.reg_set(output_id, *l-*r);
		Ok(())
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

} impl AddAssign for QuInt {

	fn add_assign(&mut self, rhs: Self) {*self += rhs}

} impl Sub for QuInt {

	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {QuInt(self.0 - rhs.0)}

} impl SubAssign for QuInt {

	fn sub_assign(&mut self, rhs: Self) {*self -= rhs}

} impl QuRegisterStruct for QuInt {
	
	fn register_fns() -> Vec<ExternalFunction> {
		return vec![
			ExternalFunction::new(
				QuOperator::Add.get_function_name(),
				&Self::quwrapper__add__,
				&[1.into(), 1.into()],
				1.into(),
			),
			ExternalFunction::new(
				QuOperator::Sub.get_function_name(),
				&Self::quwrapper__subtract__,
				&[1.into(), 1.into()],
				1.into(),
			),
			ExternalFunction::new(
				QuOperator::Less.get_function_name(),
				&Self::quwrapper__lesser__,
				&[1.into(), 1.into()],
				1.into(),
			),
			ExternalFunction::new(
				QuOperator::Great.get_function_name(),
				&Self::quwrapper__greater__,
				&[1.into(), 1.into()],
				1.into(),
			),
			ExternalFunction::new(
				"copy",
				&Self::quwrapper__copy__,
				&[1.into(), 1.into()],
				1.into(),
			),
		];
	}


	fn register_void_fns() -> Vec<QuVoidFnForm> {
		return vec![];
	}


	fn register_methods() -> Vec<QuMethodRegistration> {
		return vec![];
	}


	fn get_name() -> &'static str {
		"int"
	}


//	fn debug_string(a:&dyn Any) -> String {
//		format!("{:?}", a.downcast_ref::<Self>())
//	}

} impl QuAny for QuInt {
}



#[derive(Debug, Default, Clone, Copy)]
pub struct QuVoid();
impl QuVoid {

	fn quwrapper__copy__(
		vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
	) -> Result<(), QuMsg> {
		Ok(())
	}

} impl QuRegisterStruct for QuVoid {

	fn register_fns() -> Vec<ExternalFunction> {
		vec![
			ExternalFunction::new(
				"copy",
				&Self::quwrapper__copy__,
				&[0.into(), 0.into()],
				0.into(),
			),
		]
	}


	fn get_name() -> &'static str {
		"void"
	}

} impl QuAny for QuVoid {}


pub trait QuRegisterStruct {

	/// Returns functions to be called by the [`QuVm`].
	fn register_fns() -> Vec<ExternalFunction> where Self: Sized {
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
	fn get_name() -> &'static str;


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