
//! Defines all types and objects used by Qu.

use duplicate::duplicate;
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

			// Define traits
			duplicate!(
				[
					[ident[add] Type [QuAdd]]
					[ident[sub] Type [QuSub]]
					[ident[mul] Type [QuMul]]
					[ident[div] Type [QuDiv]]
					[ident[pow] Type [QuPow]]
					[ident[modulous] Type [QuMod]]
					[ident[lesser] Type [QuLesser]]
					[ident[greater] Type [QuGreater]]
					[ident[equal] Type [QuEqual]]
					[ident[not_equal] Type [QuNotEqual]]
				]
				let ident = m.add_trait::<Type>()?;
				qufn!(m, _api, ident(ident, ident) ident {Ok(())});
			);

			// Constants
			m.add_constant("PI", 3)?;

			// void functions
			{
				qufn!(m, _api, copy(void) void {
					Ok(())
				});
			}
			
			{ // float
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
				qufn!(m, api, copy(float) float {
					api.set::<Float>(*api.get::<Float>(0)?);
					Ok(())
				});
			}

			{ // int
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
				qufn!(m, api, copy(int) int {
					api.set::<Int>(*api.get::<Int>(0)?);
					Ok(())
				});
			}
			
			{ // bool
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

			{ // class
				qufn!(m, api, copy(class) class {
					api.set::<Class>(*api.get::<Class>(0)?);
					Ok(())
				});
			}

			{ // module
				qufn!(m, api, copy(module) module {
					api.set::<Module>(*api.get::<Module>(0)?);
					Ok(())
				});
			}

			// Implement trait functions traits in classes
			duplicate!(
				[
					[class_id [float] OpType [Float]]
					[class_id [int] OpType [Int]]
				]
				duplicate!(
					[
						[trait_id [equal] fn_name ["equal"] op [==] Ret [Bool] ret_id [bool]]
						[trait_id [not_equal] fn_name ["not_equal"] op [!=] Ret [Bool] ret_id [bool]]
						[trait_id [greater] fn_name ["greater"] op [>] Ret [Bool] ret_id [bool]]
						[trait_id [lesser] fn_name ["lesser"] op [<] Ret [Bool] ret_id [bool]]
					]
					m.implement(trait_id, class_id)?;
					m.implement_function(
						trait_id,
						class_id,
						fn_name,
						&[class_id, class_id],
						ret_id,
						&|api| {
							let value = *api.get::<OpType>(0)?
								op *api.get::<OpType>(1)?;
							api.set_hold(value);
							api.set::<Ret>(value);
							Ok(())
						},
					)?;
				)
			);
			duplicate!(
				[
					[class_id [float] OpType [Float]]
					[class_id [int] OpType [Int]]
				]
				duplicate!(
					[
						[trait_id [add] fn_name ["add"] op [+] Ret [OpType] ret_id [class_id]]
						[trait_id [sub] fn_name ["sub"] op [-] Ret [OpType] ret_id [class_id]]
						[trait_id [mul] fn_name ["mul"] op [*] Ret [OpType] ret_id [class_id]]
						[trait_id [div] fn_name ["div"] op [/] Ret [OpType] ret_id [class_id]]
					]
					m.implement(trait_id, class_id)?;
					m.implement_function(
						trait_id,
						class_id,
						fn_name,
						&[class_id, class_id],
						ret_id,
						&|api| {
							api.set::<Ret>(
								*api.get::<OpType>(0)? op *api.get::<OpType>(1)?
							);
							Ok(())
						},
					)?;
				)
			);
			
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


duplicate!(
	[
		[ClassName ["Add"] InternalName [QuAdd]]
		[ClassName ["Sub"] InternalName [QuSub]]
		[ClassName ["Mul"] InternalName [QuMul]]
		[ClassName ["Div"] InternalName [QuDiv]]
		[ClassName ["Pow"] InternalName [QuPow]]
		[ClassName ["Mod"] InternalName [QuMod]]
		[ClassName ["Greater"] InternalName [QuGreater]]
		[ClassName ["Lesser"] InternalName [QuLesser]]
		[ClassName ["Equal"] InternalName [QuEqual]]
		[ClassName ["NotEqual"] InternalName [QuNotEqual]]
	]
	/// The ClassName trait for Qu
	pub struct InternalName {}
	impl Register for InternalName {
		fn name() -> &'static str {ClassName}
	}
);


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