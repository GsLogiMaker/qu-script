
use std::fmt::Debug;
use std::mem::size_of;
use std::path::Iter;
use std::rc::Rc;
use std::vec;

use crate::Class;
use crate::Module;
use crate::QuMsg;
use crate::QuRegisterStruct;
use crate::QuVoid;
use crate::Vector2;
use crate::compiler::Definitions;
use crate::compiler::ExternalFunctionId;
use crate::compiler::FunctionGroup;
use crate::compiler::FunctionId;
use crate::import::ClassId;
use crate::objects;

pub const BASE_MODULE:&str = "__base__";
pub const MAIN_MODULE:&str = "__main__";

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
	Int(i32),
	Float(f32),
	Bool(bool),
}


pub type QuExtFn = &'static dyn Fn(
	&mut QuVm,
	&[VmStackPointer],
	VmStackPointer,
	)->Result<(), QuMsg>;


#[derive(Clone, PartialEq)]
/// The low level operations of [`QuVm`].
pub enum QuOp {
	Call(FunctionId, VmStackPointer),
	CallExt(ExternalFunctionId, VmStackPointers, VmStackPointer),
	// Fn name, fn body length.
	DefineFn(FunctionId, usize),
	End,
	/// Moves the program counter by the given [`isize`].
	JumpBy(isize),
	/// Moves the program counter by the given [`isize`] if the last expression
	/// was false.
	JumpByIfNot(isize),
	Literal(Literal, VmStackPointer),
	/// Loads a value of type [`ClassId`] into the stack. Takes the value being
	/// loaded and the stack id where it will bestored.
	Value(ExternalFunctionId, VmStackPointer),
	/// Specifies to the Vm what class can be retrieved from the API.
	Return(ClassId),
} impl Debug for QuOp {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			QuOp::Call(arg0, arg1) =>
				write!(f, "Call({:?}, {:?})", arg0, arg1),
			QuOp::CallExt(
				arg0,
				arg1,
				arg2
			) =>
				write!(f, "CallExt({:?}, {:?}, {:?})", arg0, arg1, arg2),
			QuOp::DefineFn(arg0, arg1) => 
				write!(f, "DefineFn({:?}, {:?})", arg0, arg1),
			QuOp::End =>
				write!(f, "End"),
			QuOp::JumpBy(arg0) =>
				write!(f, "JumpBy({:?})", arg0),
			QuOp::JumpByIfNot(arg0) =>
			write!(f, "JumpByIfNot({:?})", arg0),
			QuOp::Value(arg0, arg1) =>
				write!(f, "Value({:?}, {:?})", arg0, arg1),
			QuOp::Return(arg0) =>
				write!(f, "Return({:?})", arg0),
    		QuOp::Literal(arg0, arg1) =>
				write!(f, "Literal({:?}, {:?})", arg0, arg1),
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
	pub hold_is_zero: bool,
	/// Holds the value returned from a Qu script.
	return_type: ClassId,
	/// Contains all the defined class, funcitons, and more for the Vm. 
	pub definitions: Definitions,
	/// Holds the Vm's memory.
	stack: VmStack,
	/// The program counter.
	pc: usize,

} impl QuVm {
	pub fn new() -> Self {
		let mut vm = QuVm { 
			hold_is_zero: false,
			return_type: 0.into(),
			stack: VmStack::new(u8::MAX as usize),
			//stack_offset: 0,
			pc: 0,
			..Default::default()
		};
		vm.definitions.define_module(
			BASE_MODULE.into(),
			&|mut b| {
				b.register_struct::<QuVoid>()?;
				b.register_struct::<i32>()?;
				b.register_struct::<bool>()?;
				b.register_struct::<Class>()?;
				b.register_struct::<Module>()?;
				b.register_struct::<objects::FunctionGroup>()?;
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
		parameters: &Vec<VmStackPointer>,
		output_id: VmStackPointer,
	) -> Result<(), QuMsg> {
		Ok((self.definitions.get_external_function(fn_id)?.pointer)(
			self,
			parameters,
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


	/// Ends the current frame.
	fn frame_end(&mut self) -> Result<(), QuMsg>{
		return Err(QuMsg::done());
	}


	/// Starts a new frame.
	fn frame_start(&mut self, at_code:usize) {
		self.pc = at_code;
	}


	#[inline]
	/// Returns *true* if *hold* is a *true* value.
	fn is_hold_true(&mut self) -> bool {
		return self.hold_is_zero;
	}


	fn next_op<'a>(&mut self, code:&'a [QuOp]) -> &'a QuOp {
		let val = &code[self.pc];
		self.pc += 1;
		return val;
	}


	fn op_call_fn(
		&mut self,
		fn_id: usize,
		ouput: VmStackPointer,
		code: &[QuOp],
	) -> Result<(), QuMsg> {
		*self.stack.offset_mut() += usize::from(ouput);
		// Assure registers is big enought to fit u8::MAX more values
		if (self.stack.offset + u8::MAX as usize) > self.stack.len() {
			self.stack.data.resize(
				self.stack.offset + u8::MAX as usize, 
				0
			);
		}

		let return_pc = self.pc;
		let function = self.definitions.get_function(fn_id)?;
		self.frame_start(function.pc_start);
		self.loop_ops(code)?;
		*self.stack.offset_mut() -= usize::from(ouput);
		self.pc = return_pc;

		return Ok(());
	}


	fn op_call_fn_ext(
		&mut self,
		func: ExternalFunctionId,
		args: &Vec<VmStackPointer>,
		output: VmStackPointer,
	) -> Result<(), QuMsg> {
		self.external_call_by_id(
			func,
			args,
			output,
		)?;

		Ok(())
	}


	/// Sets the start of the function with the given function_id to the
	/// current program counter.
	fn op_define_fn(&mut self, function_id: usize, length: usize) {
		self.definitions.get_function_mut(function_id)
			.unwrap()
			.pc_start = self.pc;
		self.pc += length;
	}


	fn op_jump_by(&mut self, by:isize) {
		// Add
		if by > 0 {
			self.pc += by as usize;
		// Subtract
		} else {
			self.pc -= by.abs() as usize;
		}
		
	}


	fn op_jump_by_if_not(&mut self, by:isize) {
		if !self.is_hold_true() {
			// Add
			if by > 0 {
				self.pc += by as usize;
			// Subtract
			} else {
				self.pc -= by.abs() as usize;
			}
		}
	}


	fn op_literal(&mut self, literal: Literal, output: VmStackPointer) {
		match literal {
			Literal::Int(value) => self.write(output, value),
			Literal::Float(value) => self.write(output, value),
			Literal::Bool(value) => self.write(output, value),
		}
	}


	fn op_load_class(
		&mut self,
		function_id: ExternalFunctionId,
		output: VmStackPointer,
	) -> Result<(), QuMsg> {
		self.external_call_by_id(
			function_id,
			&vec![],
			output,
		)?;
		Ok(())
	}


	#[inline]
	/// Gets a register value.
	pub fn read<T>(&self, at_reg:VmStackPointer) -> Result<&T, QuMsg> {
		let got = self.stack.read(at_reg);
		Ok(got)
	}


	#[inline]
	/// Sets a register value.
	pub fn write<T>(&mut self, id:VmStackPointer, value:T) {
		return self.stack.write(id, value);
	}


	/// Runs inputed bytecode in a loop.
	fn loop_ops(&mut self, code:&[QuOp]
	) -> Result<(), QuMsg>{
		while self.pc != code.len() {
			let result
				= self.do_next_op(code);
			if let Err(e) = result {
				if e.description == "Done" {
					return Ok(())
				} else {
					return Err(e);
				}
			}
		}
		return Ok(());
	}


	/// Runs the next command in the given bytecode.
	fn do_next_op(
		&mut self, instructions:&[QuOp]
	) -> Result<(), QuMsg> {
		let op = self.next_op(instructions);
		#[cfg(feature = "qu_print_vm_operations")] {
			match op {
				QuOp::Call(function_id, output) => {
					println!(
						"Call::{} -> {}",
						self.definitions
							.get_function(*function_id)
							.unwrap()
							.identity
							.name,
						output.readable(&self.definitions),
					);
				},
				QuOp::CallExt(function_id, args, output) => {
					let mut arg_string = "".to_owned();
					for arg in args.iter() {
						arg_string.extend(
							arg.readable(&self.definitions).chars()
						);
						arg_string.push(',');
						arg_string.push(' ');
					}
					println!(
						"CallExt::{}({}) -> {}",
						self.definitions
							.get_external_function(*function_id)
							.unwrap()
							.name,
						arg_string,
						output.readable(&self.definitions),
					);
				},
				QuOp::End => {
					println!(
						"End",
					);
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
    			QuOp::Literal(literal, pointer) => {
					println!("{:}& = {:?} [Literal]", pointer.offset, literal);
				},
			};
		}
		match op {
			QuOp::Call(fn_id, ouput) => self.op_call_fn(*fn_id, *ouput, instructions)?,
			QuOp::CallExt(fn_id, args, output) => self.op_call_fn_ext(*fn_id, args.into(), *output)?,
			QuOp::End => self.frame_end()?,
			QuOp::DefineFn(fn_id, length) => self.op_define_fn(*fn_id, *length),
			QuOp::JumpByIfNot(by) => self.op_jump_by_if_not(*by),
			QuOp::JumpBy(by) => self.op_jump_by(*by),
			QuOp::Value(function_id, output) => self.op_load_class(*function_id, *output)?,
			QuOp::Return(return_type) => self.return_type = *return_type,
    		QuOp::Literal(literal, output) => self.op_literal(*literal, *output),
		};
		return Ok(());
	}


	pub fn run_ops(&mut self, instructions:&[QuOp]
	) -> Result<(), QuMsg> {
		self.pc = 0;
		while self.pc != instructions.len() {
			match self.do_next_op(&instructions) {
				// No errors, continue running
				Ok(_) => {/*pass*/},
				// Encountered error; check if done and finish, otherwise
				// propogate error
				Err(msg) => {
					if msg.description == "Done" {
						break;
					}
					return Err(msg);
				},
			};
		}

		return Ok(());
	}
}


#[derive(Debug, Clone)]
pub struct VmRc {
	rc: Rc<dyn QuRegisterStruct>,
}


trait VmRcTrait: QuRegisterStruct + Debug {}


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
	type Indexer = VmStackPointer;


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


#[derive(Debug, Clone, Copy, PartialEq)]
/// Note: The safety of this pointer relies on the VM's memory nevery moving.
pub struct VmStackPointer {
	offset: usize,
} impl VmStackPointer {
	fn readable(&self, definitions: &Definitions) -> String {
		format!("{}", self.offset)
	}
} impl From<usize> for VmStackPointer {
	fn from(offset: usize) -> Self {
		VmStackPointer { offset }
	}
} impl From<QuStackId> for VmStackPointer {
	fn from(value: QuStackId) -> Self {
		VmStackPointer { offset: value.0 }
	}
}

impl From<VmStackPointer> for usize {
	fn from(value: VmStackPointer) -> Self {
		value.offset
	}
}


#[derive(Debug, Clone, PartialEq)]
pub struct VmStackPointers {
	pointers: Vec<VmStackPointer>,
} impl VmStackPointers {
	fn iter(&self) -> core::slice::Iter<VmStackPointer> {
		self.pointers.iter()
	}
} impl From<Vec<QuStackId>> for VmStackPointers {
    fn from(value: Vec<QuStackId>) -> Self {
        VmStackPointers {
			pointers: value.iter()
				.map(|x|{(*x).into()})
				.collect()
		}
    }
}

impl From<VmStackPointers> for Vec<VmStackPointer> {
	fn from(value: VmStackPointers) -> Self {
		value.pointers
	}
} impl<'a> From<&'a VmStackPointers> for &'a Vec<VmStackPointer> {
	fn from(value: &'a VmStackPointers) -> &'a Vec<VmStackPointer> {
		&value.pointers
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