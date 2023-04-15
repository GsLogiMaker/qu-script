
//! Defines all types and objects used by Qu.


use crate::QuVm;
use crate::QuMsg;
use crate::compiler::FunctionGroupId;
use crate::compiler::ModuleId;
use crate::import::ClassId;
use crate::vm::QuExtFn;
use crate::vm::QuStackId;
use crate::vm::VmStackPointer;
use std::fmt::Debug;


macro_rules! qufn {
	($name:ident($($param:ident),*) $return:ident $block:expr) => {
		{
			ExternalFunction::new(
				stringify!($name),
				&$block,
				[$(<$param as QuRegisterStruct>::name()),*].into(),
				<$return as QuRegisterStruct>::name(),
			)
		}
	};
}


/// Data for an external function.
pub type QuExtFnData = (String, QuExtFn, Vec<usize>, usize);


#[derive(Debug, Default, Clone, Copy)]
pub struct Class {
	id: ClassId,
} impl QuRegisterStruct for Class {
	fn new(
		vm: &mut QuVm,
		args: &[VmStackPointer],
		output: VmStackPointer,
	) -> Result<(), QuMsg> where Self: Sized {
		Err(format!("Type 'class' cannot be instantiated.").into())
	}


	fn register_functions() -> Vec<ExternalFunction> where Self: Sized {
		vec![
			qufn!(copy(Self) Self |vm, args, return_id| {
				vm.write(return_id, *vm.read::<Class>(args[0])?);
				Ok(())
			}),
		]
	}


	fn name() -> &'static str {"__Class__"}
}


#[derive(Clone)]
pub struct ExternalFunction {
	pub name: String,
	pub pointer: QuExtFn,
	pub parameters: Vec<&'static str>,
	pub return_type: &'static str,
} impl ExternalFunction {
	pub fn new(
		name: &str,
		pointer: QuExtFn,
		parameters: Vec<&'static str>,
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
		&self,
		vm:&mut QuVm,
		parameters: &Vec<VmStackPointer>,
		output_id: VmStackPointer,
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


#[derive(Debug, Default, Clone, Copy)]
pub struct FunctionGroup {
	id: FunctionGroupId,
} impl QuRegisterStruct for FunctionGroup {
	fn new(
		vm: &mut QuVm,
		args: &[VmStackPointer],
		output: VmStackPointer,
	) -> Result<(), QuMsg> where Self: Sized {
		Err(format!("Type 'FunctionGroup' cannot be instantiated.").into())
	}


	fn register_functions() -> Vec<ExternalFunction> where Self: Sized {
		vec![
		]
	}


	fn name() -> &'static str {"__FunctionGroup__"}
}


#[derive(Debug, Default, Clone)]
pub struct FunctionPointer {
	pub pc_target: usize,
}


#[derive(Debug, Default, Clone, Copy)]
pub struct Module {
	id: ModuleId,
} impl QuRegisterStruct for Module {
	fn new(
		vm: &mut QuVm,
		args: &[VmStackPointer],
		output: VmStackPointer,
	) -> Result<(), QuMsg> where Self: Sized {
		Err(format!("Type 'module' cannot be instantiated.").into())
	}


	fn register_functions() -> Vec<ExternalFunction> where Self: Sized {
		vec![
			qufn!(copy(Self) Self |vm, args, return_id| {
				vm.write(return_id, *vm.read::<Module>(args[0])?);
				Ok(())
			}),
		]
	}


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


impl QuRegisterStruct for bool {
	fn new(
		vm: &mut QuVm,
		args: &[VmStackPointer],
		output: VmStackPointer,
	) -> Result<(), QuMsg> where Self: Sized {
		vm.write(output, true);
		Ok(())
	}
	
	fn register_functions() -> Vec<ExternalFunction> {
		return vec![
			qufn!(and(Self, Self) Self |vm, args, return_id| {
				vm.write(return_id, *vm.read::<Self>(args[0])? && *vm.read::<Self>(args[1])?);
				Ok(())
			}),
			qufn!(or(Self, Self) Self |vm, args, return_id| {
				vm.write(return_id, *vm.read::<Self>(args[0])? || *vm.read::<Self>(args[1])?);
				Ok(())
			}),
			qufn!(eq(Self, Self) Self |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? == vm.read::<i32>(args[1])?;
				vm.hold_is_zero = output;
				if output {
					vm.write(return_id, 1i32);
				} else {
					vm.write(return_id, 0i32);
				}
				Ok(())
			}),
			qufn!(neq(Self, Self) Self |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? != vm.read::<i32>(args[1])?;
				vm.hold_is_zero = output;
				if output {
					vm.write(return_id, 1i32);
				} else {
					vm.write(return_id, 0i32);
				}
				Ok(())
			}),
			qufn!(copy(Self) Self |vm, args, return_id| {
				vm.write(return_id, *vm.read::<bool>(args[0])?);
				Ok(())
			}),
		];
	}


	fn name() -> &'static str {"bool"}
}


impl QuRegisterStruct for i32 {
	fn new(
		vm: &mut QuVm,
		args: &[VmStackPointer],
		output: VmStackPointer,
	) -> Result<(), QuMsg> where Self: Sized {
		vm.write(output, 0);
		Ok(())
	}

	
	fn register_functions() -> Vec<ExternalFunction> {
		return vec![
			ExternalFunction::new(
				":new",
				&|vm, args, return_id| {
					let result = *vm.read::<i32>(args[0])?;
					vm.write(return_id, result);
					Ok(())
				},
				vec![Self::name()],
				Self::name(),
			),
			ExternalFunction::new(
				":new",
				&|vm, args, return_id| {
					let result = if *vm.read::<bool>(args[0])? {
						1
					} else {
						0
					};
					vm.write(return_id, result);
					Ok(())
				},
				vec![<bool as QuRegisterStruct>::name()],
				Self::name(),
			),

			qufn!(add(i32, i32) i32 |vm, args, return_id| {
				vm.write(return_id, vm.read::<i32>(args[0])? + vm.read::<i32>(args[1])?);
				Ok(())
			}),
			qufn!(sub(i32, i32) i32 |vm, args, return_id| {
				vm.write(return_id, vm.read::<i32>(args[0])? - vm.read::<i32>(args[1])?);
				Ok(())
			}),
			qufn!(mul(i32, i32) i32 |vm, args, return_id| {
				vm.write(return_id, vm.read::<i32>(args[0])? * vm.read::<i32>(args[1])?);
				Ok(())
			}),
			qufn!(div(i32, i32) i32 |vm, args, return_id| {
				vm.write(return_id, vm.read::<i32>(args[0])? / vm.read::<i32>(args[1])?);
				Ok(())
			}),
			qufn!(lesser(i32, i32) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? < vm.read::<i32>(args[1])?;
				vm.hold_is_zero = output;
				vm.write(return_id, output);
				Ok(())
			}),
			qufn!(lessereq(i32, i32) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? <= vm.read::<i32>(args[1])?;
				vm.hold_is_zero = output;
				vm.write(return_id, output);
				Ok(())
			}),
			qufn!(greater(i32, i32) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? > vm.read::<i32>(args[1])?;
				vm.hold_is_zero = output;
				vm.write(return_id, output);
				Ok(())
			}),
			qufn!(greatereq(i32, i32) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? >= vm.read::<i32>(args[1])?;
				vm.hold_is_zero = output;
				vm.write(return_id, output);
				Ok(())
			}),
			qufn!(eq(i32, i32) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? == vm.read::<i32>(args[1])?;
				vm.hold_is_zero = output;
				vm.write(return_id, output);
				Ok(())
			}),
			qufn!(neq(i32, i32) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? != vm.read::<i32>(args[1])?;
				vm.hold_is_zero = output;
				vm.write(return_id, output);
				Ok(())
			}),
			qufn!(copy(i32) i32 |vm, args, return_id| {
				vm.write(return_id, *vm.read::<i32>(args[0])?);
				Ok(())
			}),
		];
	}


	fn name() -> &'static str {"int"}
}



#[derive(Debug, Default, Clone, Copy)]
/// Represents a void type in Qu.
pub struct QuVoid();
impl QuRegisterStruct for QuVoid {
	fn new(
		vm: &mut QuVm,
		args: &[VmStackPointer],
		output: VmStackPointer,
	) -> Result<(), QuMsg> where Self: Sized {
		Err(format!("Type 'void' cannot be instantiated.").into())
	}


	fn name() -> &'static str {"void"}
}


/// A trait for registering structs into the Qu programming language.
pub trait QuRegisterStruct : Debug {
	/// A wrapper function to create a new instance.
	fn new(
		vm: &mut QuVm,
		args: &[VmStackPointer],
		output: VmStackPointer,
	) -> Result<(), QuMsg> where Self: Sized;


	/// Returns functions that are callable by [`QuVm`].
	fn register_functions() -> Vec<ExternalFunction> where Self: Sized {
		Vec::default()
	}


	/// Returns functions that are callable by [`QuVm`].
	fn private_register_functions(
	) -> Vec<ExternalFunction> where Self: 'static+ Sized {
		let mut registered = vec![
			ExternalFunction::new(
				":new",
				&Self::new,
				vec![],
				Self::name(),
			)
		];

		registered.extend(Self::register_functions());

		registered
	}


	/// Returns the name that identifies the struct being registered.
	fn name() -> &'static str where Self: Sized;
}


#[derive(Clone, Copy, Debug, Default)]
pub struct Vector2 {
	x: i32,
	y: i32,
} impl QuRegisterStruct for Vector2 {
	fn new(
		vm: &mut QuVm,
		args: &[VmStackPointer],
		output: VmStackPointer,
	) -> Result<(), QuMsg> where Self: Sized {
		vm.write(output, Vector2::default());
		Ok(())
	}


	fn register_functions() -> Vec<ExternalFunction> where Self: Sized {
		vec![
			qufn!(foo(i32) i32 |vm, args, return_id| {
				let value = *vm.read::<i32>(args[0])?;
				vm.write(
					return_id,
					value,
				);
				Ok(())
			}),
		]
	}


	fn name() -> &'static str {"Vector2"}
}


#[cfg(test)]
mod test_objects {

}