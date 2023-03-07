
//! Defines all types and objects used by Qu.

use std::{any::Any, ops::{Add, AddAssign, Sub, SubAssign}};

use crate::{QuVm, parser::QuOperator, import::QuStructId};
use crate::Qu;
use crate::QuMsg;
use crate::vm::QuExtFn;
use crate::vm::QuMemId;
use crate::vm::QuVoidExtFn;
use crate::vm::QuStackId;
use crate::vm::StackValue;


pub type QuExtFnData = (String, QuExtFn, Vec<usize>, usize);
pub type QuVoidFnForm = (String, QuVoidExtFn, Vec<usize>);
type QuMethodRegistration = (String, &'static dyn Fn(&mut QuVm));


pub struct ExternalFunction {
	pub name: String,
	pub pointer: QuExtFn,
	pub parameters: &'static[&'static str],
	pub return_type: &'static str,
} impl ExternalFunction {

	pub fn new(
		name: &str,
		pointer: QuExtFn,
		parameters: &'static[&'static str],
		return_type: &'static str,
	) -> Self{
		Self {
			name: name.into(),
			pointer,
			parameters: parameters,
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


impl QuRegisterStruct for i32 {
	
	fn register_fns() -> Vec<ExternalFunction> {

		fn add(
			vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
		) -> Result<(), QuMsg> {
			assert_eq!(parameters.len(), 2);
			let l = vm.reg_get::<i32>(parameters[0])?;
			let r = vm.reg_get::<i32>(parameters[1])?;
			vm.reg_set(output_id, *l+*r);
			Ok(())
		}
	
		fn copy(
			vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
		) -> Result<(), QuMsg> {
			assert_eq!(parameters.len(), 1);
			let s = vm.reg_get::<i32>(parameters[0])?;
			vm.reg_set(output_id, *s);
			Ok(())
		}
	
		fn great(
			vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
		) -> Result<(), QuMsg> {
			assert_eq!(parameters.len(), 2);
			let l = vm.reg_get::<i32>(parameters[0])?;
			let r = vm.reg_get::<i32>(parameters[1])?;
	
			let output = *l>*r;
			if output {
				vm.reg_set(output_id, 1i32);
			} else {
				vm.reg_set(output_id, 0i32);
			}
			vm.hold_is_zero = output;
			Ok(())
		}

		fn less(
			vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
		) -> Result<(), QuMsg> {
			assert_eq!(parameters.len(), 2);
			let l = vm.reg_get::<i32>(parameters[0])?;
			let r = vm.reg_get::<i32>(parameters[1])?;
	
			let output = *l<*r;
			vm.hold_is_zero = output;
			if output {
				vm.reg_set(output_id, 1i32);
			} else {
				vm.reg_set(output_id, 0i32);
			}
			Ok(())
		}
	
		fn sub(
			vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
		) -> Result<(), QuMsg> {
			assert_eq!(parameters.len(), 2);
			let l = vm.reg_get::<i32>(parameters[0])?;
			let r = vm.reg_get::<i32>(parameters[1])?;
			vm.reg_set(output_id, *l-*r);
			Ok(())
		}

		use QuOperator::Add as OpAdd;
		use QuOperator::Sub as OpSub;
		use QuOperator::Less as OpLes;
		use QuOperator::Great as OpGrt;

		let int:&&'static str = &Self::name();
		return vec![
			ExternalFunction::new(
				OpAdd.name(),
				&add,
				&["int", "int"],
				Self::name(),
			),
			ExternalFunction::new(
				OpSub.name(),
				&sub,
				&["int", "int"],
				Self::name(),
			),
			ExternalFunction::new(
				OpLes.name(),
				&less,
				&["int", "int"],
				Self::name(),
			),
			ExternalFunction::new(
				OpGrt.name(),
				&great,
				&["int", "int"],
				Self::name(),
			),
			ExternalFunction::new(
				"copy",
				&copy,
				&["int", "int"],
				Self::name(),
			),
			ExternalFunction::new(
				"other",
				&|vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId| -> Result<(), QuMsg> {
					Ok(())
				},
				&["int", "int"],
				Self::name(),
			),
		];
	}


	fn name() -> &'static str {
		"int"
	}

}



#[derive(Debug, Default, Clone, Copy)]
pub struct QuVoid();impl QuRegisterStruct for QuVoid {

	fn register_fns() -> Vec<ExternalFunction> {

		fn copy(
			vm:&mut QuVm, parameters:&[QuStackId], output_id: QuStackId,
		) -> Result<(), QuMsg> {
			Ok(())
		}

		vec![
			ExternalFunction::new(
				"copy",
				&copy,
				&["void"],
				QuVoid::name(),
			),
		]
	}


	fn name() -> &'static str {
		"void"
	}

}


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
	fn name() -> &'static str;


//	fn debug_string(a:&dyn Any) -> String {
//		format!("<debug_string not implemented>")
//	}

}


#[cfg(test)]
mod test_objects {

}