
//! Defines all types and objects used by Qu.

use once_cell::sync::Lazy;

use crate::QuMsg;
use crate::Uuid;
use crate::compiler::CONSTRUCTOR_NAME;
use crate::compiler::ModuleId;
use crate::compiler::REGISTERED_BANK;
use crate::import::ClassId;
use crate::import::ArgsAPI;
use crate::import::Registerer;
use crate::import::RegistererLayer;
use std::any::TypeId;
use std::any::type_name;
use std::fmt::Debug;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;

pub(crate) const FUNDAMENTALS_MODULE:&str = "__fundamentals__";

/// Qu's boolean type
pub type Bool = bool;
/// Qu's float type
pub type Float = f32;
/// Qu's int type
pub type Int = i32;

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
			let void = m.add_class::<Void>()?;
			let bool = m.add_class::<Bool>()?;
			let float = m.add_class::<Float>()?;
			let int = m.add_class::<Int>()?;
			let class = m.add_class::<Class>()?;
			let module = m.add_class::<Module>()?;

			// Traits
			let add = m.add_trait::<QuAdd>()?;
			{
				qufn!(m, _api, add(add, add) add {
					Ok(())
				});
			}

			// Constants
			m.add_constant("PI", 3)?;

			// void functions
			{
				qufn!(m, _api, copy(void) void {
					Ok(())
				});
			}

			// float functions
			{
				m.implement(add, float)?;
				m.implement_function(
					add,
					float,
					"add",
					&[float, float],
					float,
					&|api| {
						api.set::<Float>(<Float as Add>::add(
							*api.get::<Float>(0)?,
							*api.get::<Float>(1)?
						));
						Ok(())
					},
				)?;

				m.add_class_static_function(float, CONSTRUCTOR_NAME,
					&[],
					float,
					&|api| {
						api.set::<Float>(0f32);
						Ok(())
					}
				)?;
				m.add_class_static_function(float, CONSTRUCTOR_NAME,
					&[float],
					float,
					&|api| {
						api.set::<Float>(*api.get::<Float>(0)?);
						Ok(())
					}
				)?;
				m.add_class_static_function(float, CONSTRUCTOR_NAME,
					&[int],
					float,
					&|api| {
						api.set::<Float>(*api.get::<Int>(0)? as Float);
						Ok(())
					}
				)?;
				m.add_class_static_function(float, CONSTRUCTOR_NAME,
					&[bool],
					float,
					&|api| {
						api.set::<Float>(*api.get::<Bool>(0)? as Int as Float);
						Ok(())
					}
				)?;
				// qufn!(m, api, add(float, float) float {
				// 	api.set::<Float>(<Float as Add>::add(
				// 		*api.get::<Float>(0)?,
				// 		*api.get::<Float>(1)?
				// 	));
				// 	Ok(())
				// });
				qufn!(m, api, sub(float, float) float {
					api.set::<Float>(<Float as Sub>::sub(
						*api.get::<Float>(0)?,
						*api.get::<Float>(1)?
					));
					Ok(())
				});
				qufn!(m, api, mul(float, float) float {
					api.set::<Float>(<Float as Mul>::mul(
						*api.get::<Float>(0)?,
						*api.get::<Float>(1)?
					));
					Ok(())
				});
				qufn!(m, api, div(float, float) float {
					api.set::<Float>(<Float as Div>::div(
						*api.get::<Float>(0)?,
						*api.get::<Float>(1)?
					));
					Ok(())
				});
				qufn!(m, api, lesser(float, float) bool {
					let output = api.get::<Float>(0)? < api.get::<Float>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, lessereq(float, float) bool {
					let output = api.get::<Float>(0)? <= api.get::<Float>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, greater(float, float) bool {
					let output = api.get::<Float>(0)? > api.get::<Float>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, greatereq(float, float) bool {
					let output = api.get::<Float>(0)? >= api.get::<Float>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, eq(float, float) bool {
					let output = api.get::<Float>(0)? == api.get::<Float>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, neq(float, float) bool {
					let output = api.get::<Float>(0)? != api.get::<Float>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, copy(float) float {
					api.set::<Float>(*api.get::<Float>(0)?);
					Ok(())
				});
			}

			// int functions
			{
				m.add_class_static_function(int, CONSTRUCTOR_NAME,
					&[],
					int,
					&|api| {
						api.set::<Int>(0);
						Ok(())
					}
				)?;
				m.add_class_static_function(int, CONSTRUCTOR_NAME,
					&[int],
					int,
					&|api| {
						api.set::<Int>(*api.get::<Int>(0)?);
						Ok(())
					}
				)?;
				m.add_class_static_function(int, CONSTRUCTOR_NAME,
					&[bool],
					int,
					&|api| {
						api.set::<Int>(*api.get::<Bool>(0)? as Int);
						Ok(())
					}
				)?;
				m.add_class_static_function(int, CONSTRUCTOR_NAME,
					&[float],
					int,
					&|api| {
						api.set::<Int>(*api.get::<f32>(0)? as Int);
						Ok(())
					}
				)?;
				qufn!(m, api, add(int, int) int {
					api.set::<Int>(<Int as Add>::add(
						*api.get::<Int>(0)?,
						*api.get::<Int>(1)?
					));
					Ok(())
				});
				qufn!(m, api, sub(int, int) int {
					api.set::<Int>(<Int as Sub>::sub(
						*api.get::<Int>(0)?,
						*api.get::<Int>(1)?
					));
					Ok(())
				});
				qufn!(m, api, mul(int, int) int {
					api.set::<Int>(<Int as Mul>::mul(
						*api.get::<Int>(0)?,
						*api.get::<Int>(1)?
					));
					Ok(())
				});
				qufn!(m, api, div(int, int) int {
					api.set::<Int>(<Int as Div>::div(
						*api.get::<Int>(0)?,
						*api.get::<Int>(1)?
					));
					Ok(())
				});
				qufn!(m, api, lesser(int, int) bool {
					let output = api.get::<Int>(0)? < api.get::<Int>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, lessereq(int, int) bool {
					let output = api.get::<Int>(0)? <= api.get::<Int>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, greater(int, int) bool {
					let output = api.get::<Int>(0)? > api.get::<Int>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, greatereq(int, int) bool {
					let output = api.get::<Int>(0)? >= api.get::<Int>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, eq(int, int) bool {
					let output = api.get::<Int>(0)? == api.get::<Int>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, neq(int, int) bool {
					let output = api.get::<Int>(0)? != api.get::<Int>(1)?;
					api.set_hold(output);
					api.set::<Bool>(output);
					Ok(())
				});
				qufn!(m, api, copy(int) int {
					api.set::<Int>(*api.get::<Int>(0)?);
					Ok(())
				});
			}

			// bool functions
			{
				m.add_class_static_function(bool, CONSTRUCTOR_NAME,
					&[],
					bool,
					&|api| {
						api.set::<Bool>(false);
						Ok(())
					}
				)?;
				m.add_class_static_function(bool, CONSTRUCTOR_NAME,
					&[bool],
					bool,
					&|api| {
						api.set::<Bool>(*api.get::<Bool>(0)?);
						Ok(())
					}
				)?;
				m.add_class_static_function(bool, CONSTRUCTOR_NAME,
					&[int],
					bool,
					&|api| {
						api.set::<Bool>(*api.get::<Int>(0)? != 0);
						Ok(())
					}
				)?;
				m.add_class_static_function(bool, CONSTRUCTOR_NAME,
					&[float],
					bool,
					&|api| {
						api.set::<Bool>(*api.get::<Float>(0)? != 0f32);
						Ok(())
					}
				)?;
				qufn!(m, api, and(bool) bool {
					api.set::<Bool>(*api.get::<Bool>(0)? && *api.get::<Bool>(1)?);
					Ok(())
				});
				qufn!(m, api, or(bool) bool {
					api.set::<Bool>(*api.get::<Bool>(0)? || *api.get::<Bool>(1)?);
					Ok(())
				});
				qufn!(m, api, eq(bool) bool {
					api.set::<Bool>(api.get::<Bool>(0)? == api.get::<Bool>(1)?);
					Ok(())
				});
				qufn!(m, api, neq(bool) bool {
					api.set::<Bool>(api.get::<Bool>(0)? != api.get::<Bool>(1)?);
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
					api.set::<Class>(*api.get::<Class>(0)?);
					Ok(())
				});
			}

			// module functions
			{
				qufn!(m, api, copy(module) module {
					api.set::<Module>(*api.get::<Module>(0)?);
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
			// TODO: Improve module access api
			let int = fundamentals.common.get_class_id("int")?;

			m.add_function(
				"foo",
				&[int],
				int,
				&|api:&mut ArgsAPI| {
					let first:Int = *api.get(0)?;
					api.set::<>(first);
					Ok(())
				}
			)?;
			Ok(())
		}
	)?;
	Ok(())
}


/// The Add trait for Qu
pub struct QuAdd {}
impl Register for QuAdd {
	fn name() -> &'static str {"Add"}
}


/// A reference to a Qu class.
#[derive(Debug, Default, Clone, Copy)]
pub struct Class {
	_id: ClassId,
} impl Register for Class {
	fn name() -> &'static str {"__Class__"}
}


/// A reference to a Qu module.
#[derive(Debug, Default, Clone, Copy)]
pub struct Module { // TODO: Lock modules to the vm they came from.
	_id: ModuleId,
} impl Register for Module {
	fn name() -> &'static str {"__Module__"}
}


impl Register for Bool {
	fn name() -> &'static str {"bool"}
}
impl Register for Int {
	fn name() -> &'static str {"int"}
}
impl Register for Float {
	fn name() -> &'static str {"float"}
}


#[derive(Debug, Default, Clone, Copy)]
/// Represents a void type in Qu.
pub struct Void();
impl Register for Void {
	fn name() -> &'static str {"void"}
}


/// A trait for registering structs into the Qu programming language.
pub trait Register {
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

	/// Returns the name that identifies the struct being registered.
	fn name() -> &'static str {*Self::NAME}
}

#[cfg(test)]
mod test_objects {

}