
use std::any::Any;
use std::any::TypeId;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::mem::size_of;
use std::mem::replace;
use std::mem::take;
use std::ops::AddAssign;
use std::ops::Deref;
use std::ops::DerefMut;

use lazy_static::__Deref;

use crate::FunctionPointer;
use crate::QuExtFnData;
use crate::QuMsg;
use crate::QuRegisterStruct;
use crate::QuValue;
use crate::QuVoid;
use crate::compiler::Definitions;
use crate::compiler::ExternalFunctionId;
use crate::compiler::FunctionId;
use crate::compiler::FunctionMetadata;
use crate::compiler::ModuleBuilder;
use crate::import::QuFunctionId;
use crate::import::QuRegistered;
use crate::import::QuStruct;
use crate::import::ClassId;
use crate::objects::QuCodeObject;
use crate::objects::QuFnObject;
use crate::objects::QuType;

pub const BASE_MODULE:&str = "__base__";

pub type QuExtFn = &'static dyn Fn(
	&mut QuVm,
	&[QuStackId],
	QuStackId,
	)->Result<(), QuMsg>;
pub type QuVoidExtFn = &'static dyn Fn(
	&mut QuVm,
	&Vec<QuStackId>,
	)->Result<(), QuMsg>;
/// The [QuVm] registers type.
pub type StackValue = Box<dyn Any>;


#[derive(Clone, PartialEq)]
pub enum QuOp {
	Call(FunctionId, QuStackId),
	CallExt(ExternalFunctionId, Vec<QuStackId>, QuStackId),
	// Fn name, fn body length.
	DefineFn(FunctionId, usize),
	End,
	JumpBy(isize),
	JumpByIfNot(isize),
	Value(isize, QuStackId),
	Return(ClassId),
} impl Debug for QuOp {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Call(arg0, arg1) =>
				write!(f, "Call({:?}, {:?})", arg0, arg1),
			Self::CallExt(
				arg0,
				arg1,
				arg2
			) =>
				write!(f, "CallExt({:?}, {:?}, {:?})", arg0, arg1, arg2),
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


#[derive(Clone, Copy, Debug, Default)]
pub struct QuMemId {
	index:usize,
} impl QuMemId {

	fn new(index:usize) -> Self {
		return Self{
			index
		};
	}

}


#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct QuStackId(pub usize, ClassId);
impl QuStackId {
	pub fn new(index: usize, struct_id: ClassId) -> Self {
		Self(index, struct_id)
	}

	fn readable(&self, definitions: &Definitions) -> String {
		format!(
			"{}:{}",
			self.0,
			definitions.get_class(self.1).unwrap().name,
		)
	}


	pub fn index(&self) -> usize {
		self.0
	}


	pub fn class_id(&self) -> ClassId {
		self.1
	}
}
impl From<usize> for QuStackId {

	fn from(v:usize) -> Self {
		Self(v, ClassId::default())
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
}
impl From<QuStackId> for usize {
	fn from(v:QuStackId) -> Self {
		v.0 as usize
	}
}
impl From<QuStackId> for i32 {
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

	/// Holds defined constants.
	constants: Vec<Box<dyn Any>>,
	/// Maps constant names to their index in [`QuMemory::constants`].
	constant_map:HashMap<String, QuConstId>,
	/// Holds allocated data types.
	memory: Vec<Box<dyn Any>>,
	/// Maps variable names to their index in [`QuMemory::memory`].
	memory_map: HashMap<String, QuMemId>,
	/// Holds the value returned from a Qu script.
	return_type: ClassId,
	pub definitions: Definitions,

	/// The memory registers. Holds all primatives of the VM.
	stack: VmStack,
	/// The offset of reading and writing to registers.
	stack_offset: usize,
	/// The program counter.
	pc: usize,

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
			hold_is_zero: false,
			return_type: 0.into(),
			stack: VmStack::new(u8::MAX as usize),
			stack_offset: 0,
			pc: 0,
			..Default::default()
		};
		vm.definitions.define_module(
			BASE_MODULE.into(),
			&|mut b| {
				b.register_struct::<QuVoid>()?;
				b.register_struct::<i32>()?;
				b.register_struct::<bool>()?;
				Ok(())
			},
		).unwrap();
		vm.definitions.register_functions();
		vm
	}


	/// Defines a new constant in Qu.
	///
	/// # Examples
	/// 
	/// ```
	/// use qu::QuVm;
	/// use qu::QuMsg;
	/// 
	/// # fn main() {example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// let mut vm = QuVm::new();
	/// vm.define_const("BIRTHDAY".to_owned(), Box::new("c280800"));
	/// 
	/// let constant = vm.get_const::<&str>("BIRTHDAY")?;
	/// assert_eq!(*constant, "c280800");
	/// # return Ok(());
	/// # }
	/// ```
	pub fn define_const(&mut self, name:String, value:Box<dyn Any>) {
		assert_eq!(self.constants.len(), self.constant_map.len());
		let const_index = self.constants.len();

		self.constants.push(value);
		self.constant_map.insert(name.to_owned(), const_index.into());
	}


	/// Defines a variable in Qu memory.
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::QuVm;
	/// use qu::QuMsg;
	/// 
	/// # fn main() {example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// let mut vm = QuVm::new();
	/// vm.define_mem("name".to_owned(), Box::new("Dave"));
	/// 
	/// let constant = vm.get_mem::<&str>("name")?;
	/// assert_eq!(*constant, "Dave");
	/// # return Ok(());
	/// # }
	/// ```
	pub fn define_mem(&mut self, name:String, value:Box<dyn Any>) {
		assert_eq!(self.memory.len(), self.memory_map.len());
		let const_index = self.memory.len();

		self.memory.push(value);
		self.memory_map.insert(name.to_owned(), QuMemId::new(const_index));
	}


	fn external_call_by_id(
		&mut self,
		fn_id: ExternalFunctionId,
		parameters: &Vec<QuStackId>,
		output_id: QuStackId,
	) -> Result<(), QuMsg> {
		Ok((self.definitions.get_external_function(fn_id)?.pointer)(
			self,
			parameters,
			output_id,
		)?)
	}


	/// Returns a reference to a Qu constant.
	/// 
	/// # Errors
	/// 
	/// If no constant called `name` is found or if the constant can't be cast
	/// to `T` then an [`Err`] is returned.
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::QuVm;
	/// use qu::QuMsg;
	/// 
	/// # fn main() {example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// let mut vm = QuVm::new();
	/// vm.define_const("MEANING_OF_LIFE".into(), Box::new(42isize));
	/// 
	/// let constant = vm.get_const::<isize>("MEANING_OF_LIFE")?;
	/// assert_eq!(*constant, 42isize);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn get_const<'a, T:'a + 'static>(&'a self, name:&str
	) -> Result<&T, QuMsg> {
		let Some(index) = self.constant_map.get(&name.to_owned())
			else {return Err(QuMsg::general(
				&format!("No constant by name {name} found. TODO: Better msg.")
			))};
		
		let const_ref = self.get_const_by_id::<T>(*index)?;

		return Ok(const_ref);
	}


	/// Returns a reference to a Qu constant by its index.
	/// 
	/// # Errors
	/// 
	/// If `index` is out of range or the constant can't be cast to `T` then an
	/// [`Err`] is returned.
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::QuVm;
	/// use qu::QuMsg;
	/// 
	/// # fn main() {example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// let mut vm = QuVm::new();
	/// vm.define_const("BITS".into(), Box::new(128isize));
	/// 
	/// let BITS = vm.get_const_by_id::<isize>(0.into())?;
	/// assert_eq!(*BITS, 128isize);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn get_const_by_id<'a, T:'a + 'static>(&'a self, id:QuConstId
	) -> Result<&T, QuMsg> {
		let Some(any) = self.constants.get(id.0)
			else {return Err(QuMsg::general(
				"Can't access constant by id {id}. Index is out of range. TODO: Better msg"
			))};

		let Some(cast) = any.downcast_ref::<T>()
			else {return Err(QuMsg::general(
				&format!("Can't cast constant to specified T. TODO: Better msg.")
			))};

		return Ok(cast);
	}


	pub fn get_mem_id(&mut self, name:&str) -> Result<QuMemId, QuMsg> {
		let Some(id) = self.memory_map.get(name) else {
			return Err(QuMsg::general(
				&format!("qu has no memory location with name {name}")
			));
		};

		Ok(id.clone())
	}


	/// Returns a mutable reference to a variable in Qu memory.
	/// 
	/// # Errors
	/// 
	/// If no variable called `name` is found or if the variable can't be cast
	/// to `T` then an [`Err`] is returned.
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::QuVm;
	/// use qu::QuMsg;
	/// 
	/// # fn main() {example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// let mut vm = QuVm::new();
	/// vm.define_mem("count".into(), Box::new(100isize));
	/// 
	/// let count = vm.get_mem_mut::<isize>("count")?;
	/// assert_eq!(*count, 100isize);
	/// 
	/// *count += 5;
	/// let altered_count = vm.get_mem_mut::<isize>("count")?;
	/// assert_eq!(*altered_count, 105isize);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn get_mem_mut<'a, T:'a + 'static>(&'a mut self, name:&str
	) -> Result<&mut T, QuMsg> {
		let Some(index) = self.memory_map.get(&name.to_owned())
			else {return Err(QuMsg::general(
				&format!("No variable by name {name} found. TODO: Better msg.")
			))};

		return Ok(self.get_mem_mut_by_id(index.clone())?);
	}


	pub fn get_mem_mut_by_id<'a, T:'a + 'static>(&'a mut self, id:QuMemId
	) -> Result<&mut T, QuMsg> {
		self.get_mem_mut_by_index(id.index)
	}


	/// Returns a mutable reference to a variable in Qu memory by its index.
	/// 
	/// # Errors
	/// 
	/// If `index` is out of range or if the variable can't be cast to `T` then
	/// an [`Err`] is returned.
	fn get_mem_mut_by_index<'a, T:'a + 'static>(&'a mut self, index:usize
	) -> Result<&mut T, QuMsg> {
		let Some(any) = self.memory.get_mut(index)
		else {return Err(QuMsg::general(
			&format!("Can't get variable. Index {index} is out of range.")
		))};
		let Some(cast) = any.downcast_mut::<T>()
			else {return Err(QuMsg::general(
				&format!("Can't cast variable to specified T. TODO: Better msg.")
			))};

		return Ok(cast);
	}


	/// Returns a reference to a variable in Qu memory.
	/// 
	/// # Errors
	/// 
	/// If no variable called `name` is found or if the variable can't be
	/// cast to `T` then an [`Err`] is returned.
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::QuVm;
	/// use qu::QuMsg;
	/// 
	/// # fn main() {example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// let mut vm = QuVm::new();
	/// vm.define_mem("count".into(), Box::new(100isize));
	/// 
	/// let count = vm.get_mem::<isize>("count")?;
	/// assert_eq!(*count, 100isize);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn get_mem<'a, T:'a + 'static>(&'a self, name:&str
	) -> Result<&T, QuMsg> {
		let Some(index) = self.memory_map.get(&name.to_owned())
			else {return Err(QuMsg::general(
				&format!("No variable by name {name} found. TODO: Better msg.")
			))};

		self.get_mem_by_id(index.clone())
	}


	pub fn get_mem_by_id<'a, T:'a + 'static>(&'a self, id:QuMemId
	) -> Result<&T, QuMsg> {
		self.get_mem_by_index(id.index)
	}


	/// Returns a reference to a variable in Qu memory by its index.
	/// 
	/// # Errors
	/// 
	/// If `index` is out of range or if the variable can't be
	/// cast to `T` then an [`Err`] is returned.
	fn get_mem_by_index<'a, T:'a + 'static>(&'a self, index:usize
	) -> Result<&T, QuMsg> {
		let Some(any) = self.memory.get(index)
			else {return Err(QuMsg::general(
				&format!("Can't get variable. Index {index} is out of range.")
			))};

		let Some(cast) = self.memory[index].downcast_ref::<T>()
			else {return Err(QuMsg::general(
				&format!("Can't cast variable to specified T. TODO: Better msg.")
			))};

		return Ok(cast);
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
		// TODO: Implement register boolean evaluation
		unimplemented!();
		//return self.hold != 0;
	}


	fn next_op<'a>(&mut self, code:&'a [QuOp]) -> &'a QuOp {
		let val = &code[self.pc];
		self.pc += 1;
		return val;
	}


	fn op_call_fn(
		&mut self,
		fn_id: usize,
		ouput: QuStackId,
		code: &[QuOp],
	) -> Result<(), QuMsg> {
		*self.stack.offset_mut() += usize::from(ouput);
		// Assure registers is big enought to fit u8::MAX more values
		if (self.stack_offset + u8::MAX as usize) > self.stack.len() {
			self.stack.data.resize(
				self.stack_offset + u8::MAX as usize, 
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
		args: &Vec<QuStackId>,
		output: QuStackId,
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


	fn op_load_int(&mut self, value:isize, output:QuStackId) {
		// TODO: Make function signature i32
		self.write(output, value as i32);
	}


	#[inline]
	/// Gets a register value.
	pub fn read<T>(&self, at_reg:QuStackId) -> Result<&T, QuMsg> {
		let struct_data = self.definitions
			.get_class(at_reg.class_id())?;
		if size_of::<T>() != struct_data.size as usize {
			return Err("Can't get register. Mismatched types. TODO: Better msg".into());
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


	pub fn set_mem<'a, T:'a + 'static>(&'a mut self, name:&str, value:T
	) -> Result<(), QuMsg> {
		let Some(id) = self.memory_map.get(&name.to_owned())
			else {return Err(QuMsg::general(
				&format!("No variable by name {name} found. TODO: Better msg.")
			))};

		self.set_mem_by_id(id.clone(), value)?;

		return Ok(());
	}


	pub fn set_mem_by_id<'a, T:'a + 'static>(&'a mut self, id:QuMemId, value:T
	) -> Result<(), QuMsg> {
		self.set_mem_by_index(id.index, value)
	}


	fn set_mem_by_index<'a, T:'a + 'static>(&'a mut self, index:usize, value:T
	) -> Result<(), QuMsg> {
		let Some(any) = self.memory.get_mut(index)
		else {return Err(QuMsg::general(
			&format!("Can't get variable. Index {index} is out of range.")
		))};
		let Some(cast) = any.downcast_mut::<T>()
			else {return Err(QuMsg::general(
				&format!("Can't cast variable to specified T. TODO: Better msg.")
			))};
		
		*cast = value;

		return Ok(());
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
						output.as_string(&self.definitions),
					);
				},
				QuOp::CallExt(function_id, args, output) => {
					let mut arg_string = "".to_owned();
					for arg in args {
						arg_string.extend(
							arg.as_string(&self.definitions).chars()
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
						output.as_string(&self.definitions),
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
						output.as_string(&self.definitions),
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
			QuOp::Call(fn_id, ouput) => self.op_call_fn(*fn_id, *ouput, instructions)?,
			QuOp::CallExt(fn_id, args, output) => self.op_call_fn_ext(*fn_id, args, *output)?,
			QuOp::End => self.frame_end()?,
			QuOp::DefineFn(fn_id, length) => self.op_define_fn(*fn_id, *length),
			QuOp::JumpByIfNot(by) => self.op_jump_by_if_not(*by),
			QuOp::JumpBy(by) => self.op_jump_by(*by),
			QuOp::Value(value, output) => self.op_load_int(*value, *output),
			QuOp::Return(return_type) => self.return_type = *return_type,
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