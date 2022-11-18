
//! Defines all types and objects used by Qu.

use std::any::Any;

use crate::{QuVm, Qu, QuMsg, vm::{QuRegisteredFunction, QuMemId}};


pub type QuFunctionRegistration = (String, QuRegisteredFunction);
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

	/// Constructs a [`QuFnObject`].
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::QuCodeObject;
	/// use qu::QuFnObject;
	/// use qu::QuType;
	/// 
	/// let func = QuFnObject::new(
	/// 	vec![],
	/// 	QuCodeObject::new(0),
	/// 	QuType::Void
	/// );
	/// ```
	pub fn new(parameters:Vec<QuType>, body:QuCodeObject, return_type:QuType
	) -> Self {
		return Self{
			parameters,
			body,
			return_type,
		}
	}


	fn change_type(&mut self, to:QuType) {
		println!("old type: {:?}", self.return_type);
		self.return_type = to;
		println!("new type: {:?}", self.return_type);
	}


	fn quwrapper_change_type(
		vm:&mut QuVm, obj_id:QuMemId, parameters:Vec<&dyn Any>
	) -> Result<(), QuMsg>{
		let obj = vm.get_mem_mut_by_id::<Self>(obj_id)?;
		let to_type = parameters[0]
			.downcast_ref::<QuType>().unwrap();

		obj.change_type(to_type.clone());

		return Ok(());
	}


	fn say_hello(vm:&mut QuVm) {
		println!("hello")
	}

} impl QuRegister for QuFnObject {
	
	fn register_functions() -> Vec<QuFunctionRegistration> {
		return vec![
			("change_type".to_owned(), &Self::quwrapper_change_type)
		];
	}


	fn register_methods() -> Vec<QuMethodRegistration> {
		return vec![
			("say_hello".to_owned(), &Self::say_hello)
		];
	}


	fn get_name() -> String {
		"FnObject".to_owned()	
	}

}


pub trait QuRegister {

	/// Returns functions to be called by the [`QuVm`].
	fn register_functions() -> Vec<QuFunctionRegistration>;


	/// Returns functions to be called by the [`QuVm`].
	fn register_methods() -> Vec<QuMethodRegistration>;


	/// Returns the name that identifies the struct being registered.
	fn get_name() -> String;

}


#[cfg(test)]
mod test_objects {

}