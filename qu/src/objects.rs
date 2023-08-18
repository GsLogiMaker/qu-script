
//! Defines all types and objects used by Qu.


use crate::QuVm;
use crate::QuMsg;
use crate::compiler::Definitions;
use crate::compiler::ModuleId;
use crate::import::ClassId;
use crate::import::ArgsAPI;
use crate::import::ExternalFunctionPointer;
use crate::import::ModuleBuilder;
use crate::import::Registerer;
use crate::vm::QuExtFn;
use crate::vm::QuVoidExtFn;
use crate::vm::QuStackId;
use std::fmt::Debug;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;

pub const FUNDAMENTALS_MODULE:&str = "__fundamentals__";

macro_rules! qufn {
	($module_builder:ident, $api:ident, $name:ident($($param:ident),*) $return:ident $block:block) => {
		$module_builder.add_function(
			stringify!($name).into(),
			&[$($param),*],
			$return,
			&|$api| $block
		)?
	};
}

/// A method for registering the __fundamentals__ module in Qu.
/// 
/// # Examples
/// 
/// ```
/// # fn main(){example().unwrap()}
/// # fn example() -> Result<(), qu::QuMsg> {
/// use qu::Qu;
/// use qu::objects::fundamentals_module;
/// 
/// let mut qu = Qu::new();
/// // TODO: Maybe add a Qu constructor without fundamentals already added.
/// //qu.register(&fundamentals_module)?;
/// # return Ok(());
/// # }
/// ```
pub fn fundamentals_module(registerer: &mut Registerer) -> Result<(), QuMsg> {
	registerer.add_module(
		FUNDAMENTALS_MODULE.into(),
		&|m| {
			let bool = m.add_class::<Bool>()?;
			let class = m.add_class::<Class>()?;
			let int = m.add_class::<i32>()?;
			let module = m.add_class::<Module>()?;
			let void = m.add_class::<QuVoid>()?;

			// bool functions
			{
				qufn!(m, api, and(bool) bool {
					api.set(api.get::<Bool>(0)?.and(api.get::<Bool>(1)?));
					Ok(())
				});
				qufn!(m, api, or(bool) bool {
					api.set(api.get::<Bool>(0)?.or(api.get::<Bool>(1)?));
					Ok(())
				});
				qufn!(m, api, eq(bool) bool {
					api.set::<Bool>((api.get::<Bool>(0)? == api.get::<Bool>(1)?).into());
					Ok(())
				});
				qufn!(m, api, neq(bool) bool {
					api.set::<Bool>((api.get::<Bool>(0)? != api.get::<Bool>(1)?).into());
					Ok(())
				});
				qufn!(m, api, copy(bool) bool {
					api.set::<Bool>(*api.get::<Bool>(0)?);
					Ok(())
				});
			}

			// class functions
			{
				qufn!(m, api, copy(class) class {
					api.set(*api.get::<Class>(0)?);
					Ok(())
				});
			}

			// int functions
			{
				qufn!(m, api, add(int, int) int {
					api.set(<i32 as Add>::add(
						*api.get::<i32>(0)?,
						*api.get::<i32>(1)?
					));
					Ok(())
				});
				qufn!(m, api, sub(int, int) int {
					api.set(<i32 as Sub>::sub(
						*api.get::<i32>(0)?,
						*api.get::<i32>(1)?
					));
					Ok(())
				});
				qufn!(m, api, mul(int, int) int {
					api.set(<i32 as Mul>::mul(
						*api.get::<i32>(0)?,
						*api.get::<i32>(1)?
					));
					Ok(())
				});
				qufn!(m, api, div(int, int) int {
					api.set(<i32 as Div>::div(
						*api.get::<i32>(0)?,
						*api.get::<i32>(1)?
					));
					Ok(())
				});
				qufn!(m, api, lesser(int, int) bool {
					let output = api.get::<i32>(0)? < api.get::<i32>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output.into());
					Ok(())
				});
				qufn!(m, api, lessereq(int, int) bool {
					let output = api.get::<i32>(0)? <= api.get::<i32>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output.into());
					Ok(())
				});
				qufn!(m, api, greater(int, int) bool {
					let output = api.get::<i32>(0)? > api.get::<i32>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output.into());
					Ok(())
				});
				qufn!(m, api, greatereq(int, int) bool {
					let output = api.get::<i32>(0)? >= api.get::<i32>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output.into());
					Ok(())
				});
				qufn!(m, api, eq(int, int) bool {
					let output = api.get::<i32>(0)? == api.get::<i32>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output.into());
					Ok(())
				});
				qufn!(m, api, neq(int, int) bool {
					let output = api.get::<i32>(0)? != api.get::<i32>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output.into());
					Ok(())
				});
				qufn!(m, api, copy(int) int {
					api.set(*api.get::<i32>(0)?);
					Ok(())
				});
			}
			
			// module functions
			{
				qufn!(m, api, copy(module) module {
					api.set(*api.get::<Module>(0)?);
					Ok(())
				});
			}

			// void functions
			{
				qufn!(m, api, copy(void) void {
					Ok(())
				});
			}

			Ok(())
		}
	)?;
	Ok(())
}

pub fn math_module(registerer: &mut Registerer) -> Result<(), QuMsg> {
	registerer.add_module(
		"math".into(),
		&|m| {
			let vector2 = m.add_class::<Vector2>()?;
			let fundamentals = m.get_module(FUNDAMENTALS_MODULE)?;
			let int = fundamentals.get_class_id("int")?;
			m.add_function(
				"foo".into(),
				&[int],
				int,
				&|api:&mut ArgsAPI| {
					let first:i32 = *api.get(0)?;
					api.set(first);
					Ok(())
				}
			)?;
			Ok(())
		}
	)?;
	Ok(())
}


/// Data for an external function.
pub type QuExtFnData = (String, QuExtFn, Vec<usize>, usize);

/// Data for a void external function.
pub type QuVoidFnForm = (String, QuVoidExtFn, Vec<usize>);


#[derive(Debug, Default, Clone, Copy)]
pub struct Class {
	id: ClassId,
} impl QuRegisterStruct for Class {
	fn name() -> &'static str {"__Class__"}
}


#[derive(Clone)]
pub struct ExternalFunction {
	pub name: String,
	pub pointer: QuExtFn,
	pub parameters: Vec<ClassId>,
	pub return_type: ClassId,
} impl ExternalFunction {
	pub fn new(
		name: &str,
		pointer: QuExtFn,
		parameters: Vec<ClassId>,
		return_type: ClassId,
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
} impl Debug for ExternalFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExternalFunction")
			.field("name", &self.name)
			.field("parameters", &self.parameters)
			.field("return_type", &self.return_type)
			.finish()
    }
} impl Default for ExternalFunction {
    fn default() -> Self {
        Self {
			pointer: &|_, _, _| {Ok(())},
			..Default::default()
		}
    }
}


#[derive(Clone)]
pub struct ExternalFunctionDefinition {
	pub name: String,
	pub pointer: &'static ExternalFunctionPointer,
	pub parameters: Vec<ClassId>,
	pub return_type: ClassId,
} impl ExternalFunctionDefinition {
	pub fn new(
		name: &str,
		pointer: &'static ExternalFunctionPointer,
		parameters: Vec<ClassId>,
		return_type: ClassId,
	) -> Self{
		Self {
			name: name.into(),
			pointer,
			parameters: parameters,
			return_type,
		}
	}
} impl Debug for ExternalFunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExternalFunctionDefinition")
			.field("name", &self.name)
			.field("parameters", &self.parameters)
			.field("return_type", &self.return_type)
			.finish()
    }
} impl Default for ExternalFunctionDefinition {
    fn default() -> Self {
        Self {
			name: Default::default(),
			pointer: &|_| {Ok(())},
			parameters: Default::default(),
			return_type: Default::default(),
		}
    }
}



#[derive(Debug, Default, Clone)]
pub struct FunctionPointer {
	pub pc_target: usize,
}


#[derive(Debug, Default, Clone, Copy)]
pub struct Module {
	id: ModuleId,
} impl QuRegisterStruct for Module {
	fn name() -> &'static str {"__Module__"}
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bool (bool, bool, bool, bool);
impl Bool {
	pub fn and(&self, other: &Self) -> Bool {
		(self.0 && other.0).into()
	}
	
	pub fn or(&self, other: &Self) -> Bool {
		(self.0 && other.0).into()
	}
} impl Into<bool> for Bool {
    fn into(self) -> bool {
        self.0
    }
} impl QuRegisterStruct for Bool {
	fn register_fns(
		definitions: &mut Definitions
	) -> Vec<ExternalFunctionDefinition> {vec![]}

	fn name() -> &'static str {"bool"}
}

impl Into<Bool> for bool {
    fn into(self) -> Bool {
        Bool(self, false, false, false)
    }
}

impl QuRegisterStruct for i32 {
	fn name() -> &'static str {"int"}
}



#[derive(Debug, Default, Clone, Copy)]
/// Represents a void type in Qu.
pub struct QuVoid();
impl QuRegisterStruct for QuVoid {
	fn name() -> &'static str {"void"}
}


/// A trait for registering structs into the Qu programming language.
pub trait QuRegisterStruct {
	/// Returns functions that are callable by [`QuVm`].
	fn register_fns(
		definitions: &mut Definitions
	) -> Vec<ExternalFunctionDefinition> where Self: Sized {
		Vec::default()
	}


	/// Returns the name that identifies the struct being registered.
	fn name() -> &'static str;
}


#[derive(Copy, Clone, Debug, Default)]
pub struct Vector2 {
	x: i32,
	y: i32,
} impl QuRegisterStruct for Vector2 {
	fn name() -> &'static str {"Vector2"}
}


#[cfg(test)]
mod test_objects {

}