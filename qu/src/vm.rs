
use std::fmt::Debug;
use std::mem::size_of;

use crate::Bool;
use crate::Class;
use crate::Module;
use crate::QuMsg;
use crate::QuRegisterStruct;
use crate::QuVoid;
use crate::Vector2;
use crate::compiler::Definitions;
use crate::compiler::ExternalFunctionId;
use crate::compiler::FunctionId;
use crate::import::ClassId;

pub const FUNDAMENTALS_MODULE:&str = "__fundamentals__";
pub const MAIN_MODULE:&str = "__main__";

pub type QuExtFn = &'static dyn Fn(
	&mut QuVm,
	&[QuStackId],
	QuStackId,
	)->Result<(), QuMsg>;
pub type QuVoidExtFn = &'static dyn Fn(
	&mut QuVm,
	&Vec<QuStackId>,
	)->Result<(), QuMsg>;


#[derive(Clone, PartialEq)]
/// The low level operations of [`QuVm`].
pub enum QuOp {
	/// Calls a function defined by Qu.
	Call(FunctionId, QuStackId),
	/// Calls a function defined outside of Qu (like in Rust).
	CallExt(ExternalFunctionId, Vec<QuStackId>, QuStackId),
	// Fn name, fn body length. (DEPCRICATED)
	DefineFn(FunctionId, usize),
	End,
	/// Moves the program counter by the given [`isize`].
	JumpBy(isize),
	/// Moves the program counter by the given [`isize`] if the last expression
	/// was false.
	JumpByIfNot(isize),
	/// Loads an [`isize`] into the stack. Takes the value being loaded and the
	/// stack id where it will bestored.
	Value(isize, QuStackId),
	/// Specifies to the Vm what class can be retrieved from the API.
	Return(ClassId),
} impl Debug for QuOp {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Call(arg0, arg1) =>
				write!(f, "Call({:?}, {:?})", arg0, arg1),
			Self::CallExt(
				arg0,
				arg1,
				arg2,
			) =>
				write!(f, "CallExt({:?}, {:?}, {:?})", arg0, arg1, arg2,),
			Self::DefineFn(arg0, arg1) => 
				write!(f, "DefineFn({:?}, {:?})", arg0, arg1),
			Self::End =>
				write!(f, "End"),
			Self::JumpBy(arg0) =>
				write!(f, "JumpBy({:?})", arg0),
			Self::JumpByIfNot(arg0) =>
			write!(f, "JumpByIfNot({:?})", arg0),
			Self::Value(arg0, arg1) =>
				write!(f, "Value({:?}, {:?})", arg0, arg1),
			Self::Return(arg0) =>
				write!(f, "Return({:?})", arg0),
		}
	}
}


#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct QuConstId(pub usize);
impl From<usize> for QuConstId {

	fn from(v:usize) -> Self {
		Self(v)
	}

} impl From<u8> for QuConstId {

	fn from(v:u8) -> Self {
		Self(v as usize)
	}

} impl From<i32> for QuConstId {

	fn from(v:i32) -> Self {
		Self(v as usize)
	}

} impl From<u32> for QuConstId {

	fn from(v:u32) -> Self {
		Self(v as usize)
	}

}


impl From<QuConstId> for u8 {
	fn from(v:QuConstId) -> Self {
		v.0 as u8
	}
}
impl From<QuConstId> for usize {
	fn from(v:QuConstId) -> Self {
		v.0 as usize
	}
}
impl From<QuConstId> for i32 {
	fn from(v:QuConstId) -> Self {
		v.0 as i32
	}
}
impl From<QuConstId> for u32 {
	fn from(v:QuConstId) -> Self {
		v.0 as u32
	}
}


#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
/// An ID to a location on the Vm's stack.
pub struct QuStackId(pub usize, ClassId);
impl QuStackId {
	/// Constructs a [`QuStackId`]
	pub fn new(index: usize, struct_id: ClassId) -> Self {
		Self(index, struct_id)
	}

	/// Returns a readable [`String`] representation of the Id.
	fn readable(&self, definitions: &Definitions) -> String {
		format!(
			"{}:{}",
			self.0,
			definitions.get_class(self.1).unwrap().name,
		)
	}


	/// Returns an index in the [`QuVm`]s stack.
	pub fn index(&self) -> usize {
		self.0
	}


	/// Returns the Id of the class this Id is connected to.
	pub fn class_id(&self) -> ClassId {
		self.1
	}
}
impl From<usize> for QuStackId {
	fn from(v:usize) -> Self {
		Self(v, ClassId::default())
	}
} impl From<isize> for QuStackId {
	fn from(v:isize) -> Self {
		Self(v as usize, 0.into())
	}
} impl From<u8> for QuStackId {
	fn from(v:u8) -> Self {
		Self(v as usize, ClassId::default())
	}
} impl From<i32> for QuStackId {
	fn from(v:i32) -> Self {
		Self(v as usize, ClassId::default())
	}
}


impl From<QuStackId> for u8 {
	fn from(v:QuStackId) -> Self {
		v.0 as u8
	}
} impl From<QuStackId> for usize {
	fn from(v:QuStackId) -> Self {
		v.0 as usize
	}
} impl From<QuStackId> for isize {
	fn from(v:QuStackId) -> Self {
		v.0 as isize
	}
} impl From<QuStackId> for i32 {
	fn from(v:QuStackId) -> Self {
		v.0 as i32
	}
}


/// The virtual machine that runs Qu code.
/// 
/// This struct is not meant to be accessed directly (in most cases). See
/// [`qu::Qu`] for interfacing with Qu script.
#[derive(Default)]
pub struct QuVm {
	/// Holds the outputed value of the last executed operation.
	pub hold_is_true: bool,
	/// Holds the value returned from a Qu script.
	return_type: ClassId,
	/// Contains all the defined class, funcitons, and more for the Vm. 
	pub definitions: Definitions,
	/// Holds the Vm's memory.
	stack: VmStack,

} impl QuVm {

	/// Constructs a new [`QuVm`].
	/// 
	/// # Example
	/// 
	/// ```
	/// use qu::QuVm;;
	/// 
	/// let vm = QuVm::new();
	/// ```
	pub fn new() -> Self {
		let mut vm = QuVm { 
			hold_is_true: false,
			return_type: 0.into(),
			stack: VmStack::new(u8::MAX as usize),
			..Default::default()
		};
		vm.definitions.define_module(
			FUNDAMENTALS_MODULE.into(),
			&|mut b| {
				b.register_struct::<QuVoid>()?;
				b.register_struct::<i32>()?;
				b.register_struct::<Bool>()?;
				b.register_struct::<Class>()?;
				b.register_struct::<Module>()?;
				b.register_functions()?;
				Ok(())
			},
		).unwrap();

		vm.definitions.define_module(
			MAIN_MODULE.into(),
			&|mut b| {Ok(())},
		).unwrap();

		vm.definitions.define_module(
			"math".into(),
			&|mut b| {
				b.register_struct::<Vector2>()?;
				b.register_functions()?;
				Ok(())
			},
		).unwrap();

		vm
	}


	fn external_call_by_id(
		&mut self,
		fn_id: ExternalFunctionId,
		args: Vec<QuStackId>,
		output_id: QuStackId,
	) -> Result<(), QuMsg> {
		let external_func = self.definitions
			.get_external_function(fn_id)?;

		// Call the external function
		Ok((external_func.pointer)(
			self,
			&args,
			output_id,
		)?)
	}


	/// Returns the value returned by the last run Qu script.
	/// 
	/// # Examples
	/// 
	/// //TODO: Example
	pub fn return_value_id(&mut self) -> ClassId {
		let return_type = self.return_type;
		self.return_type = 0.into();
		return_type
	}


	#[inline]
	/// Returns *true* if *hold* is a *true* value.
	fn is_hold_true(&self) -> bool {
		return self.hold_is_true;
	}


	fn op_call_fn(
		&mut self,
		fn_id: usize,
		ouput: QuStackId,
	) -> Result<(), QuMsg> {
		*self.stack.offset_mut() += usize::from(ouput);
		// Assure registers is big enought to fit u8::MAX more values
		if (self.stack.offset + u8::MAX as usize) > self.stack.len() {
			self.stack.data.resize(
				self.stack.offset + u8::MAX as usize, 
				0
			);
		}
		let function = self.definitions.get_function(fn_id)?;
		self.loop_ops(function.code_block)?;
		*self.stack.offset_mut() -= usize::from(ouput);

		return Ok(());
	}


	/// Sets the start of the function with the given function_id to the
	/// current program counter.
	fn op_define_fn(&mut self, function_id: usize, length: usize) {
		todo!("Define functions with code_block rather than pc_start");
		// self.definitions.get_function_mut(function_id)
		// 	.unwrap()
		// 	.pc_start = self.pc;
		// self.pc += length;
	}


	fn op_jump_by(&mut self, mut pc:usize, by:isize) -> usize {
		// Add
		if by > 0 {
			pc += by as usize;
		// Subtract
		} else {
			pc -= by.abs() as usize;
		}
		pc
	}


	fn op_jump_by_if_not(&mut self, mut pc:usize, by:isize) -> usize{
		if !self.is_hold_true() {
			// Add
			if by > 0 {
				pc += by as usize;
			// Subtract
			} else {
				pc -= by.abs() as usize;
			}
		}
		pc
	}


	fn op_load_int(&mut self, value:isize, output:QuStackId) {
		// TODO: Make function signature i32
		self.write(output, value as i32);
	}


	#[inline]
	/// Gets a register value.
	pub fn read<T: QuRegisterStruct>(&self, at_reg:QuStackId) -> Result<&T, QuMsg> {
		let struct_data = self.definitions
			.get_class(at_reg.class_id())?;
		if size_of::<T>() != struct_data.size as usize {
			return Err(format!(
				"Can't get register as type '{}' because it's type '{}'.",
				<T as QuRegisterStruct>::name(),
				self.definitions.get_class(at_reg.class_id()).unwrap().name,
			).into());
		}

		let got = self.stack.read(at_reg);
		Ok(got)
	}


	#[inline]
	/// Gets a register value.
	pub fn reg_get_mut<T>(&mut self, at_reg:QuStackId) -> Result<&mut T, QuMsg> {
		let struct_data = self.definitions
			.get_class(at_reg.class_id())?;
		// TODO: Add check if casting to right struct
		assert_eq!(size_of::<T>(), struct_data.size as usize);

		let got = self.stack.read_mut(at_reg.into());
		Ok(got)
	}


	#[inline]
	/// Sets a register value.
	pub fn write<T>(&mut self, id:QuStackId, value:T) {
		return self.stack.write(id.into(), value);
	}


	/// Runs inputed bytecode in a loop.
	pub fn loop_ops(
		&mut self,
		code_block:usize,
	) -> Result<(), QuMsg>{
		let mut pc = 0;
		while pc != self.definitions.byte_code_blocks[code_block].len() {
			let op = &self.definitions.byte_code_blocks[code_block][pc];
			#[cfg(feature = "qu_print_vm_operations")] {
				match op {
					QuOp::Call(function_id, output) => {
						let params = &self.definitions
							.get_function(*function_id)?
							.identity.parameters;
						
						let mut args_string = String::from("");
						for arg in params {
							args_string.push_str(&format!(
								"{}, ",
								self.definitions
									.get_class(*arg)
									.unwrap().name,
								));
						}
						println!(
							"Call::{}({args_string}) -> {}",
							self.definitions
								.get_function(*function_id)
								.unwrap()
								.identity
								.name,
							output.readable(&self.definitions),
						);
					},
					QuOp::CallExt(function_id, args, output) => {
						let mut args_string = String::from("");
						for arg in args {
							args_string.push_str(&format!(
								"{}:{}, ",
								arg.0,
								self.definitions
									.get_class(arg.class_id())
									.unwrap().name,
								));
						}
						println!(
							"CallExt::{}({args_string}) -> {}",
							self.definitions
								.get_external_function(*function_id)
								.unwrap()
								.name,
							output.readable(&self.definitions),
						);
					},
					QuOp::End => {
						println!("End");
					},
					QuOp::DefineFn(function_id, length) => {
						println!(
							"DefineFn({}, {})",
							self.definitions
								.get_function(*function_id)
								.unwrap()
								.identity
								.name,
							length,
						);
					},
					QuOp::JumpByIfNot(by) => {
						println!(
							"JumpBy({})IfNot hold:{}",
							by,
							self.is_hold_true(),
						);
					},
					QuOp::JumpBy(by) => {
						println!(
							"JumpBy {by}",
						);
					},
					QuOp::Value(value, output) => {
						println!(
							"Value {} -> {}",
							value,
							output.readable(&self.definitions),
						);
					},
					QuOp::Return(return_type) => {
						println!(
							"Return:{}",
							self.definitions.get_class(*return_type).unwrap().name,
						);
					},
				};
			}
			match op {
				QuOp::Call(fn_id, ouput) => self.op_call_fn(*fn_id, *ouput)?,
				QuOp::CallExt(fn_id, args, output) => self.external_call_by_id(*fn_id, args.clone(), *output)?,
				QuOp::End => break,
				QuOp::DefineFn(fn_id, length) => self.op_define_fn(*fn_id, *length),
				QuOp::JumpByIfNot(by) => pc = self.op_jump_by_if_not(pc, *by),
				QuOp::JumpBy( by) => pc = self.op_jump_by(pc, *by),
				QuOp::Value(value, output) => self.op_load_int(*value, *output),
				QuOp::Return(return_type) => self.return_type = *return_type,
			};
			pc += 1;
		}
		return Ok(());
	}
}


#[derive(Debug, Default, Clone)]
struct VmStack {
	data: Vec<u8>,
	offset: usize,
} impl VmStack {
	pub fn new(size:usize) -> Self {
		let mut stack = Self {data: vec![], offset: 0};
		stack.data.resize(size, 0);
		stack
	}
} impl Stack for VmStack {
	type Indexer = QuStackId;


	fn data(&self) -> &Vec<u8> {
		&self.data
	}


	fn data_mut(&mut self) -> &mut Vec<u8> {
		&mut self.data
	}


	fn offset(&self) -> usize {
		self.offset
	}


	fn offset_mut(&mut self) -> &mut usize {
		&mut self.offset
	}
}


pub trait Stack {
	type Indexer: Into<usize>;

	fn data(&self) -> &Vec<u8>;


	fn data_mut(&mut self) -> &mut Vec<u8>;


	fn read<T>(&self, id: Self::Indexer) -> &T {
		unsafe { self.data()[self.offset() + id.into()..]
			.as_ptr()
			.cast::<T>()
			.as_ref()
			.unwrap()
		}
	}


	fn read_mut<T>(&mut self, id: Self::Indexer) -> &mut T {
		let index = self.offset() + id.into();
		unsafe { self.data_mut()[index..]
			.as_mut_ptr()
			.cast::<T>()
			.as_mut()
			.unwrap()
		}
	}


	fn len(&self) -> usize {
		self.data().len()
	}


	fn move_offset(&mut self, by:isize) {
		if by > 0 {
			*self.offset_mut() += by as usize;
		// Subtract
		} else {
			*self.offset_mut() -= by.abs() as usize;
		}
	}


	fn offset(&self) -> usize;


	fn offset_mut(&mut self) -> &mut usize;


	fn write<T>(&mut self, id: Self::Indexer, value: T) {
		let id = id.into();
		assert!(&self.offset() + id + &size_of::<T>() <= self.data().len());
		let index = self.offset() + id;
		let index_pointer = self.data_mut()
			.as_mut_slice()[index..]
			.as_mut_ptr();
		unsafe { *(index_pointer as *mut T) = value };
	}
}