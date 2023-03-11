
//! Defines all types and objects used by Qu.


use crate::QuVm;
use crate::QuMsg;
use crate::vm::QuExtFn;
use crate::vm::QuVoidExtFn;
use crate::vm::QuStackId;
use crate::vm::QuAny;
use crate::vm::StackValue;


/// Data for an external function.
pub type QuExtFnData = (String, QuExtFn, Vec<usize>, usize);

/// Data for a void external function.
pub type QuVoidFnForm = (String, QuVoidExtFn, Vec<usize>);

type QuMethodRegistration = (String, &'static dyn Fn(&mut QuVm));


#[allow(non_snake_case)]
fn int_quwrapper__add__(vm:&mut QuVm, parameters:&Vec<QuStackId>
) -> Result<StackValue, QuMsg> {
	if parameters.len() != 2 {
		return Err(QuMsg::general("incorrect parameters quantity"));
	}
	let l = vm.reg_get_as::<isize>(parameters[0])?;
	let r = vm.reg_get_as::<isize>(parameters[1])?;

	let output = *l+*r;
	Ok(Box::new(output))
}


#[allow(non_snake_case)]
fn int_quwrapper__copy__(vm:&mut QuVm, parameters:&Vec<QuStackId>
) -> Result<StackValue, QuMsg> {
	if parameters.len() != 1 {
		return Err(QuMsg::general("incorrect parameters quantity"));
	}
	let s = vm.reg_get_as::<isize>(parameters[0])?;
	Ok(Box::new(*s))
}


#[allow(non_snake_case)]
fn int_quwrapper__greater__(vm:&mut QuVm, parameters:&Vec<QuStackId>
) -> Result<StackValue, QuMsg> {
	if parameters.len() != 2 {
		return Err(QuMsg::general("incorrect parameters quantity"));
	}
	let l = vm.reg_get_as::<isize>(parameters[0])?;
	let r = vm.reg_get_as::<isize>(parameters[1])?;

	let output = *l>*r;
	vm.hold_is_true = output;
	if output {
		Ok(Box::new(1isize))
	} else {
		Ok(Box::new(0isize))
	}
}


#[allow(non_snake_case)]
fn int_quwrapper__lesser__(vm:&mut QuVm, parameters:&Vec<QuStackId>
) -> Result<StackValue, QuMsg> {
	if parameters.len() != 2 {
		return Err(QuMsg::general("incorrect parameters quantity"));
	}
	let l = vm.reg_get_as::<isize>(parameters[0])?;
	let r = vm.reg_get_as::<isize>(parameters[1])?;

	let output = *l<*r;
	vm.hold_is_true = output;
	if output {
		Ok(Box::new(1isize))
	} else {
		Ok(Box::new(0isize))
	}
}


#[allow(non_snake_case)]
fn int_quwrapper__subtract__(vm:&mut QuVm, parameters:&Vec<QuStackId>
) -> Result<StackValue, QuMsg> {
	if parameters.len() != 2 {
		return Err(QuMsg::general("incorrect parameters quantity"));
	}
	let l = vm.reg_get_as::<isize>(parameters[0])?;
	let r = vm.reg_get_as::<isize>(parameters[1])?;

	let output = *l-*r;
	Ok(Box::new(output))
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
	pub parameters:Vec<String>,
	/// The function body.
	pub body:QuCodeObject,
	/// The variable type that this function returns.
	pub return_type:String,

} impl QuFnObject {

	/// Constructs a new [`QuFnObject`].
	pub fn new(parameters:Vec<String>, body:QuCodeObject, return_type:String
	) -> Self {
		Self {
			parameters,
			body,
			return_type,
		}
	}

}


#[derive(Debug, Default, Clone, Copy)]
/// A Qu wrapper defining a void object.
pub struct QuVoid();
impl QuRegisterStruct for QuVoid {

	fn get_name() -> String where Self: Sized {
		"Void".to_owned()
	}

} impl QuAny for QuVoid {}


/// Allows a struct to be imported into the Qu language.
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


impl QuRegisterStruct for isize {
	
	fn register_fns() -> Vec<QuExtFnData> {
		return vec![
			(
				"__add__".into(),
				&int_quwrapper__add__,
				vec![0, 0],
				0
			), (
				"__subtract__".into(),
				&int_quwrapper__subtract__,
				vec![0, 0],
				0
			), (
				"__lesser__".into(),
				&int_quwrapper__lesser__,
				vec![0, 0],
				0
			), (
				"__greater__".into(),
				&int_quwrapper__greater__,
				vec![0, 0],
				0
			), (
				"__copy__".into(),
				&int_quwrapper__copy__,
				vec![0, 0],
				0
			),
		];
	}


	fn register_void_fns() -> Vec<QuVoidFnForm> {
		return vec![];
	}


	fn register_methods() -> Vec<QuMethodRegistration> {
		return vec![];
	}


	fn get_name() -> String {
		"int".to_owned()	
	}

}


#[cfg(test)]
mod test_objects {

}