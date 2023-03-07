
use std::any::Any;
use std::any::TypeId;
use std::collections::HashMap;
use std::fmt::Debug;
use std::mem::size_of;
use std::mem::replace;
use std::mem::take;
use std::ops::AddAssign;

use crate::QuExtFnData;
use crate::QuMsg;
use crate::QuRegisterStruct;
use crate::QuValue;
use crate::QuVoid;
use crate::import::QuFunctionId;
use crate::import::QuRegistered;
use crate::import::QuStruct;
use crate::import::QuStructId;
use crate::objects::QuCodeObject;
use crate::objects::QuFnObject;
use crate::objects::QuType;

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
	Call(QuConstId, QuStackId),
	CallExt(QuFunctionId, Vec<QuStackId>, QuStackId),
	// Fn name, fn body length.
	DefineFn(String, usize),
	End,
	JumpBy(isize),
	JumpByIfNot(isize),
	Value(isize, QuStackId),
	Return(QuStructId),
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


#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct QuStackId(pub usize, QuStructId);
impl QuStackId {

	pub fn new(index: usize, struct_id: QuStructId) -> Self {
		Self(index, struct_id)
	}

	pub fn index(&self) -> usize {
		self.0
	}


	pub fn struct_id(&self) -> QuStructId {
		self.1
	}

}
impl From<usize> for QuStackId {

	fn from(v:usize) -> Self {
		Self(v, QuStructId::default())
	}

} impl From<u8> for QuStackId {

	fn from(v:u8) -> Self {
		Self(v as usize, QuStructId::default())
	}

} impl From<i32> for QuStackId {

	fn from(v:i32) -> Self {
		Self(v as usize, QuStructId::default())
	}

}impl Copy for QuStackId {

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
pub struct QuVm {
	/// Holds the outputed value of the last executed operation.
	hold: StackValue,
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
	return_type: QuStructId,
	pub imports: QuRegistered,

	/// The memory registers. Holds all primatives of the VM.
	stack: Stack,
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
			hold: Box::new(QuVoid()),
			hold_is_zero: false,

			constants: Vec::default(),
			constant_map: HashMap::default(),
			memory: Vec::default(),
			memory_map: HashMap::default(),
			return_type: 0.into(),
			imports: QuRegistered::default(),

			stack: Stack::new(u8::MAX as usize),
			stack_offset: 0,
			pc: 0,
		};
		vm.imports.register_struct::<QuVoid>();
		vm.imports.register_struct::<i32>();
		vm.imports.register_fns();
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


	/// Defines a Qu function.
	fn define_fn(&mut self, name:String, code_start:usize) {
		self.define_const(
			name,
			Box::new(QuFnObject::new(
				vec![],
				QuCodeObject::new(code_start),
				QuType::Void,
			))
		);
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


	pub fn external_call<S:'static+Default+QuRegisterStruct>(
		&mut self,
		obj_name:&str,
		fn_name:&str,
		parameters:Vec<QuMemId>,
		imports:&QuRegistered,
	) -> Result<(), QuMsg> {

		let struct_name = <S as QuRegisterStruct>::name();
		let r_struct = imports.get_struct(&struct_name)?;

		let fn_id = imports.get_fn_id(fn_name, &struct_name)?;
		let mem_id = self.get_mem_id(obj_name)?.clone();

		// TODO: Bring named memory addresses into registers to be used in fn

//		self.external_call_by_id(
//			mem_id,
//			fn_id,
//			parameters,
//		)?;
		return Ok(());
	}


	fn external_call_by_id(
		&mut self,
		fn_id:QuFunctionId,
		parameters:&Vec<QuStackId>,
		output_id:QuStackId,
	) -> Result<(), QuMsg> {
		Ok((self.imports.get_fn_data_by_id(fn_id)?.pointer)(
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
				"Can't acsess constant by id {id}. Index is out of range. TODO: Better msg"
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
	pub fn return_value_id(&mut self) -> QuStructId {
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
		fn_id:QuConstId,
		ouput:QuStackId,
		code:&[QuOp],
	) -> Result<(), QuMsg> {
		self.stack.add_offset(usize::from(ouput));
		// Assure registers is big enought to fit u8::MAX more values
		if (self.stack_offset + u8::MAX as usize) > self.stack.len() {
			self.stack.data.resize(
				self.stack_offset + u8::MAX as usize, 
				0
			);
		}

		let return_pc = self.pc;
		let fn_obj:&QuFnObject = self.get_const_by_id(fn_id)?;
		self.frame_start(fn_obj.body.start_index);
		self.loop_ops(code)?;
		self.stack.subtract_offset(usize::from(ouput));
		self.pc = return_pc;

		return Ok(());
	}


	fn op_call_fn_ext(
		&mut self,
		func:QuFunctionId,
		args:&Vec<QuStackId>,
		output:QuStackId,
	) -> Result<(), QuMsg> {
		let fn_data
			= self.imports.get_fn_data_by_id(func)?;

		self.external_call_by_id(
			func,
			args,
			output,
		)?;

		Ok(())
	}


	/// Defines a function from next bytes in the code.
	fn op_define_fn(&mut self, name:String, length:usize) {
		self.define_fn(name, self.pc);
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
		self.reg_set(output, value as i32);
	}


	#[inline]
	/// Gets a register value.
	pub fn reg_get<T>(&self, at_reg:QuStackId) -> Result<&T, QuMsg> {
		let Ok(struct_data) = self.imports
			.get_struct_by_id(at_reg.struct_id())
		else {
			return Err("Class does not exist. From reg_get_mut TODO: Better msg".into());
		};
		if size_of::<T>() != struct_data.size as usize {
			return Err("Can't get register. Mismatched types. TODO: Better msg".into());
		}

		unsafe {
			let Some(got) = self.stack.get(at_reg)
				.as_ptr()
				.cast::<T>()
				.as_ref()
			else {
				return Err("Invalid pointer".into());
			};
			Ok(got)
		}
	}


	#[inline]
	/// Gets a register value.
	pub fn reg_get_mut<T>(&mut self, at_reg:QuStackId) -> Result<&mut T, QuMsg> {
		let Ok(struct_data) = self.imports
			.get_struct_by_id(at_reg.struct_id())
		else {
			return Err("Class does not exist. From reg_get_mut".into());
		};
		// TODO: Add check if casting to right struct
		assert_eq!(size_of::<T>(), struct_data.size as usize);

		unsafe {
			let Some(got) = self.stack.get_mut(at_reg)
				.as_mut_ptr()
				.cast::<T>()
				.as_mut()
			else {
				return Err("Invalid pointer".into());
			};
			Ok(got)
		}
	}


	#[inline]
	/// Sets a register value.
	pub fn reg_set<T>(&mut self, id:QuStackId, value:T) {
		return self.stack.set(id, value);
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
		match op {
			QuOp::Call(fn_id, ouput) => self.op_call_fn(*fn_id, *ouput, instructions)?,
			QuOp::CallExt(fn_id, args, output) => self.op_call_fn_ext(*fn_id, args, *output)?,
			QuOp::End => self.frame_end()?,
			QuOp::DefineFn(name, length) => self.op_define_fn(name.clone(), *length),
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
struct Stack {
	data: Vec<u8>,
	offset:usize,
} impl Stack {

	pub fn new(size:usize) -> Self {
		let mut stack = Self {data: vec![], offset: 0};
		stack.data.resize(size, 0);
		stack
	}


	fn add_offset(&mut self, by:usize) {
		self.offset += by;
	}


	fn get(&self, id:QuStackId) -> &[u8] {
		&self.data[self.offset + id.index()..]
	}


	fn get_mut(&mut self, id:QuStackId) -> &mut [u8] {
		&mut self.data[self.offset + id.index()..]
	}


	fn len(&self) -> usize {
		self.data.len()
	}


	fn move_offset(&mut self, by:isize) {
		if by > 0 {
			self.offset += by as usize;
		// Subtract
		} else {
			self.offset -= by.abs() as usize;
		}
	}


	fn set<T>(&mut self, id:QuStackId, value:T) {
		assert!(self.offset + id.index() + size_of::<T>() < self.data.len());
		let index_pointer = self.data
			.as_mut_slice()[self.offset + id.index()..]
			.as_mut_ptr();
		unsafe { *(index_pointer as *mut T) = value };
	}


	fn set_offset(&mut self, to:usize) {
		self.offset = to;
	}


	fn subtract_offset(&mut self, by:usize) {
		self.offset -= by;
	}

}