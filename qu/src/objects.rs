
//! Defines all types and objects used by Qu.


use crate::QuVm;
use crate::QuMsg;
use crate::compiler::Definitions;
use crate::compiler::ModuleId;
use crate::import::ClassId;
use crate::vm::FUNDAMENTALS_MODULE;
use crate::vm::QuExtFn;
use crate::vm::QuVoidExtFn;
use crate::vm::QuStackId;
use std::fmt::Debug;


macro_rules! qufn {
	($name:ident($($param:ident),*) $return:ident $block:expr) => {
		{
			ExternalFunctionDefinition::new(
				stringify!($name),
				&$block,
				[$($param),*].into(),
				$return,
			)
		}
	};
}


/// Data for an external function.
pub type QuExtFnData = (String, QuExtFn, Vec<usize>, usize);

/// Data for a void external function.
pub type QuVoidFnForm = (String, QuVoidExtFn, Vec<usize>);


#[derive(Debug, Default, Clone, Copy)]
pub struct Class {
	id: ClassId,
} impl QuRegisterStruct for Class {
	fn register_fns(
		definitions: &mut Definitions
	) -> Vec<ExternalFunctionDefinition> {
		let literals = definitions
			.get_module_by_name(FUNDAMENTALS_MODULE)
			.unwrap();
		let class = literals
			.get_class_id("__Class__")
			.unwrap();

		vec![
			qufn!(copy(class) class |vm, args, return_id| {
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
	pub pointer: QuExtFn,
	pub parameters: Vec<ClassId>,
	pub return_type: ClassId,
} impl ExternalFunctionDefinition {
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
			pointer: &|_, _, _| {Ok(())},
			..Default::default()
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
	fn register_fns(
		definitions: &mut Definitions
	) -> Vec<ExternalFunctionDefinition> {
		let literals = definitions
			.get_module_by_name(FUNDAMENTALS_MODULE)
			.unwrap();
		let module = literals
			.get_class_id("__Module__")
			.unwrap();

		vec![
			qufn!(copy(module) module |vm, args, return_id| {
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
	) -> Vec<ExternalFunctionDefinition> {
		let literals = definitions
			.get_module_by_name(FUNDAMENTALS_MODULE)
			.unwrap();
		let bool = literals
			.get_class_id("bool")
			.unwrap();

		return vec![
			qufn!(and(bool, bool) bool |vm, args, return_id| {
				vm.write(
					return_id,
					vm.read::<Bool>(args[0])?.and(
						vm.read::<Bool>(args[1])?
					)
				);
				Ok(())
			}),
			qufn!(or(bool, bool) bool |vm, args, return_id| {
				vm.write(
					return_id,
					vm.read::<Bool>(args[0])?.or(vm.read::<Bool>(args[1])?)
				);
				Ok(())
			}),
			qufn!(eq(bool, bool) bool |vm, args, return_id| {
				let output = vm.read::<Bool>(args[0])? == vm.read::<Bool>(args[1])?;
				vm.hold_is_true = output;
				if output {
					vm.write(return_id, 1i32);
				} else {
					vm.write(return_id, 0i32);
				}
				Ok(())
			}),
			qufn!(neq(bool, bool) bool |vm, args, return_id| {
				let output = vm.read::<Bool>(args[0])? != vm.read::<Bool>(args[1])?;
				vm.hold_is_true = output;
				if output {
					vm.write(return_id, 1i32);
				} else {
					vm.write(return_id, 0i32);
				}
				Ok(())
			}),
			qufn!(copy(bool) bool |vm, args, return_id| {
				vm.write(return_id, *vm.read::<Bool>(args[0])?);
				Ok(())
			}),
		];
	}

	fn name() -> &'static str {"bool"}
}

impl Into<Bool> for bool {
    fn into(self) -> Bool {
        Bool(self, false, false, false)
    }
}

impl QuRegisterStruct for i32 {
	
	fn register_fns(
		definitions: &mut Definitions
	) -> Vec<ExternalFunctionDefinition> {
		let literals = definitions
			.get_module_by_name(FUNDAMENTALS_MODULE)
			.unwrap();
		let int = literals
			.get_class_id("int")
			.unwrap();
		let bool = literals
			.get_class_id("bool")
			.unwrap();

		return vec![
			qufn!(add(int, int) int |vm, args, return_id| {
				vm.write(return_id, vm.read::<i32>(args[0])? + vm.read::<i32>(args[1])?);
				Ok(())
			}),
			qufn!(sub(int, int) int |vm, args, return_id| {
				vm.write(return_id, vm.read::<i32>(args[0])? - vm.read::<i32>(args[1])?);
				Ok(())
			}),
			qufn!(mul(int, int) int |vm, args, return_id| {
				vm.write(return_id, vm.read::<i32>(args[0])? * vm.read::<i32>(args[1])?);
				Ok(())
			}),
			qufn!(div(int, int) int |vm, args, return_id| {
				vm.write(return_id, vm.read::<i32>(args[0])? / vm.read::<i32>(args[1])?);
				Ok(())
			}),
			qufn!(lesser(int, int) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? < vm.read::<i32>(args[1])?;
				vm.hold_is_true = output;
				vm.write::<Bool>(
					return_id,
					output.into(),
				);
				Ok(())
			}),
			qufn!(lessereq(int, int) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? <= vm.read::<i32>(args[1])?;
				vm.hold_is_true = output;
				vm.write::<Bool>(
					return_id,
					output.into(),
				);
				Ok(())
			}),
			qufn!(greater(int, int) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? > vm.read::<i32>(args[1])?;
				vm.hold_is_true = output;
				vm.write::<Bool>(
					return_id,
					output.into(),
				);
				Ok(())
			}),
			qufn!(greatereq(int, int) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? >= vm.read::<i32>(args[1])?;
				vm.hold_is_true = output;
				vm.write::<Bool>(
					return_id,
					output.into(),
				);
				Ok(())
			}),
			qufn!(eq(int, int) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? == vm.read::<i32>(args[1])?;
				vm.hold_is_true = output;
				vm.write::<Bool>(
					return_id,
					output.into(),
				);
				Ok(())
			}),
			qufn!(neq(int, int) bool |vm, args, return_id| {
				let output = vm.read::<i32>(args[0])? != vm.read::<i32>(args[1])?;
				vm.hold_is_true = output;
				vm.write::<Bool>(
					return_id,
					output.into(),
				);
				Ok(())
			}),
			qufn!(copy(int) int |vm, args, return_id| {
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
	fn register_fns(
		definitions: &mut Definitions
	) -> Vec<ExternalFunctionDefinition> {
		let literals = definitions
			.get_module_by_name(FUNDAMENTALS_MODULE)
			.unwrap();
		let void = literals
			.get_class_id("void")
			.unwrap();

		vec![
			qufn!(copy(void) void |vm, args, return_id| {
				Ok(())
			}),
		]
	}


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


pub struct Vector2 {
	x: i32,
	y: i32,
} impl QuRegisterStruct for Vector2 {
	fn register_fns(
		definitions: &mut Definitions
	) -> Vec<ExternalFunctionDefinition> {
		let math = definitions
			.get_module_by_name("math")
			.unwrap();
		let fundamentals = definitions
			.get_module_by_name(FUNDAMENTALS_MODULE)
			.unwrap();
		let int = fundamentals
			.get_class_id("int")
			.unwrap();

		vec![
			qufn!( foo(int) int |vm, args, return_id| {
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