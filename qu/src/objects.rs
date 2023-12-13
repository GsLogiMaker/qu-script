
//! Defines all types and objects used by Qu.

use once_cell::sync::Lazy;

use crate::QuMsg;
use crate::Uuid;
use crate::compiler::CONSTRUCTOR_NAME;
use crate::compiler::Definitions;
use crate::compiler::ModuleId;
use crate::compiler::REGISTERED_BANK;
use crate::import::ClassId;
use crate::import::ArgsAPI;
use crate::import::ExternalFunctionPointer;
use crate::import::Registerer;
use crate::import::RegistererLayer;
use crate::vm::QuExtFn;
use crate::vm::QuVoidExtFn;
use std::any::TypeId;
use std::any::type_name;
use std::fmt::Debug;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;

pub(crate) const FUNDAMENTALS_MODULE:&str = "__fundamentals__";

macro_rules! qufn {
	($module_builder:ident, $api:ident, $name:ident($($param:ident),*) $return:ident $block:block) => {
		$module_builder.add_function(
			stringify!($name),
			&[$($param),*],
			$return,
			&|$api| $block
		)?
	};
	($module_builder:ident, $api:ident, $for_class:ident.$name:ident($($param:ident),*) $return:ident $block:block) => {
		$module_builder.add_class_static_function(
			$for_class,
			stringify!($name),
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
		FUNDAMENTALS_MODULE,
		&|m| {
			let void = m.add_class::<QuVoid>()?;
			let int = m.add_class::<i32>()?;
			let bool = m.add_class::<bool>()?;
			let class = m.add_class::<Class>()?;
			let module = m.add_class::<Module>()?;

			m.add_constant("PI", 3)?;

			// void functions
			{
				qufn!(m, _api, copy(void) void {
					Ok(())
				});
			}

			// int functions
			{
				m.add_class_static_function(int, CONSTRUCTOR_NAME,
					&[],
					int,
					&|api| {
						api.set(0);
						Ok(())
					}
				)?;
				m.add_class_static_function(int, CONSTRUCTOR_NAME,
					&[int],
					int,
					&|api| {
						api.set(*api.get::<i32>(0)?);
						Ok(())
					}
				)?;
				m.add_class_static_function(int, CONSTRUCTOR_NAME,
					&[bool],
					int,
					&|api| {
						api.set(*api.get::<bool>(0)? as i32);
						Ok(())
					}
				)?;
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
					api.set::<bool>(output.into());
					Ok(())
				});
				qufn!(m, api, lessereq(int, int) bool {
					let output = api.get::<i32>(0)? <= api.get::<i32>(1)?;
					api.set_hold(output);
					api.set::<bool>(output.into());
					Ok(())
				});
				qufn!(m, api, greater(int, int) bool {
					let output = api.get::<i32>(0)? > api.get::<i32>(1)?;
					api.set_hold(output);
					api.set::<bool>(output.into());
					Ok(())
				});
				qufn!(m, api, greatereq(int, int) bool {
					let output = api.get::<i32>(0)? >= api.get::<i32>(1)?;
					api.set_hold(output);
					api.set::<bool>(output.into());
					Ok(())
				});
				qufn!(m, api, eq(int, int) bool {
					let output = api.get::<i32>(0)? == api.get::<i32>(1)?;
					api.set_hold(output);
					api.set::<bool>(output.into());
					Ok(())
				});
				qufn!(m, api, neq(int, int) bool {
					let output = api.get::<i32>(0)? != api.get::<i32>(1)?;
					api.set_hold(output);
					api.set::<bool>(output.into());
					Ok(())
				});
				qufn!(m, api, copy(int) int {
					api.set(*api.get::<i32>(0)?);
					Ok(())
				});
			}

			// bool functions
			{
				m.add_class_static_function(bool, CONSTRUCTOR_NAME,
					&[],
					bool,
					&|api| {
						api.set(bool::from(false));
						Ok(())
					}
				)?;
				m.add_class_static_function(bool, CONSTRUCTOR_NAME,
					&[bool],
					bool,
					&|api| {
						api.set(*api.get::<bool>(0)?);
						Ok(())
					}
				)?;
				m.add_class_static_function(bool, CONSTRUCTOR_NAME,
					&[int],
					bool,
					&|api| {
						api.set(bool::from(*api.get::<i32>(0)? != 0));
						Ok(())
					}
				)?;
				qufn!(m, api, and(bool) bool {
					api.set(*api.get::<bool>(0)? && *api.get::<bool>(1)?);
					Ok(())
				});
				qufn!(m, api, or(bool) bool {
					api.set(*api.get::<bool>(0)? || *api.get::<bool>(1)?);
					Ok(())
				});
				qufn!(m, api, eq(bool) bool {
					api.set::<bool>((api.get::<bool>(0)? == api.get::<bool>(1)?).into());
					Ok(())
				});
				qufn!(m, api, neq(bool) bool {
					api.set::<bool>((api.get::<bool>(0)? != api.get::<bool>(1)?).into());
					Ok(())
				});
				qufn!(m, api, copy(bool) bool {
					api.set::<bool>(*api.get::<bool>(0)?);
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

			// module functions
			{
				qufn!(m, api, copy(module) module {
					api.set(*api.get::<Module>(0)?);
					Ok(())
				});
			}

			

			Ok(())
		}
	)?;
	Ok(())
}

/// A method for registering the math module in Qu.
pub fn math_module(registerer: &mut Registerer) -> Result<(), QuMsg> {
	registerer.add_module(
		"math",
		&|m| {
			let fundamentals = m
				.get_module(FUNDAMENTALS_MODULE)?;
			let int = fundamentals.get_class_id("int")?;

			m.add_function(
				"foo",
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


/// A reference to a Qu class.
#[derive(Debug, Default, Clone, Copy)]
pub struct Class {
	_id: ClassId,
} impl QuRegisterStruct for Class {
	fn name() -> &'static str {"__Class__"}
}


/// Defines information about an external function that can be sued by Qu.
#[derive(Clone)]
pub struct ExternalFunction {
	/// The name of the function.
	/// 
	/// This is the name used when calling this function from a Qu script.
	pub name: String,
	/// The pointer to the function.
	pub pointer: &'static ExternalFunctionPointer,
	/// The arguments this function takes.
	pub parameters: Vec<ClassId>,
	/// The return type of the function.
	pub return_type: ClassId,
} impl ExternalFunction {
	/// Instantiates a new [`ExternalFunctionDefinition`].
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
} impl Debug for ExternalFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExternalFunctionDefinition")
			.field("name", &self.name)
			.field("parameters", &self.parameters)
			.field("return_type", &self.return_type)
			.finish()
    }
} impl Default for ExternalFunction {
    fn default() -> Self {
        Self {
			name: Default::default(),
			pointer: &|_| {Ok(())},
			parameters: Default::default(),
			return_type: Default::default(),
		}
    }
}


/// A reference to a Qu module.
#[derive(Debug, Default, Clone, Copy)]
pub struct Module { // TODO: Lock modules to the vm they came from.
	_id: ModuleId,
} impl QuRegisterStruct for Module {
	fn name() -> &'static str {"__Module__"}
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

/// A wrapper for Qu booleans.
// #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// pub struct bool (bool, bool, bool, bool);
// impl bool {
// 	/// Performas the boolean *and* operation between this and another `[bool]`.
// 	pub fn and(&self, other: &Self) -> bool {
// 		(self.0 && other.0).into()
// 	}
	
// 	/// Performas the boolean *or* operation between this and another `[bool]`.
// 	pub fn or(&self, other: &Self) -> bool {
// 		(self.0 && other.0).into()
// 	}
// } impl From<bool> for bool {
//     fn from(value: bool) -> Self {
    //     Self (
	// 		value,
	// 		false,
	// 		false,
	// 		false
	// 	)
    // }
// }
impl QuRegisterStruct for bool {
	fn register_fns(
		_definitions: &mut Definitions
	) -> Vec<ExternalFunction> {vec![]}

	// fn name() -> &'static str {"bool"}
}

// impl From<bool> for bool {
//     fn from(value: bool) -> Self {
//         value.0
//     }
// }

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
	/// The name of the object inferred from the source type.
	const NAME:Lazy<&'static str> = Lazy::new(||{
		let path = type_name::<Self>().split("::");
		path.collect::<Vec<&str>>().last().unwrap()
	});

	/// Gets the [`ClassId`] of this struct from the Qu instance with `uuid`.
	fn get_id(uuid: &Uuid) -> Option<ClassId> where Self: 'static{
		REGISTERED_BANK
			.read()
			.unwrap()
			.get(&uuid)?
			.get(&TypeId::of::<Self>())
			.map(|x|{*x})
	}

	/// Returns functions that are callable by [`QuVm`].
	fn register_fns(
		_definitions: &mut Definitions
	) -> Vec<ExternalFunction> where Self: Sized {
		Vec::default()
	}

	/// Returns the name that identifies the struct being registered.
	fn name() -> &'static str {*Self::NAME}
}

#[cfg(test)]
mod test_objects {

}