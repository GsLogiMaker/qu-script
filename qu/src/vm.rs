
use std::any::Any;
use std::collections::HashMap;
use std::fmt::Debug;
use std::mem::replace;
use std::mem::take;

use crate::QuMsg;
use crate::QuVoid;
use crate::import::QuExtFnId;
use crate::import::QuImports;
use crate::objects::QuCodeObject;
use crate::objects::QuFnObject;

pub type QuExtFn = &'static dyn Fn(
	&mut QuVm,
	&Vec<QuStackId>
	)->Result<StackValue, QuMsg>;
	
pub type QuVoidExtFn = &'static dyn Fn(
	&mut QuVm,
	&Vec<QuStackId>
	)->Result<(), QuMsg>;

/// The [QuVm] registers type.
pub type StackValue = Box<dyn Any>;


#[derive(Clone, PartialEq)]
/// The low level operations of [`QuVm`].
pub enum QuOp {
	/// Calls a Qu function. Takes a const ID to the function and a stack ID
	/// for the return value.
	Call(QuConstId, QuStackId),
	/// Calls an external function. Takes an external function Id, a [`Vec`] of
	/// stack IDs for arguments, and a stack ID for the return value.
	CallExt(QuExtFnId, Vec<QuStackId>, QuStackId),
	/// Defines a Qu function. Takes the name of the function and the length of
	/// the function in [`QuOp`]s.
	DefineFn(String, usize),
	/// Ends the current scope.
	End,
	/// Moves the program counter by the given [`isize`].
	JumpBy(isize),
	/// Moves the program counter by the given [`isize`] if the last expression
	/// was false.
	JumpByIfNot(isize),
	/// Loads an [`isize`] into the stack. Takes the value being loaded and the
	/// stack id where it will bestored.
	Value(isize, QuStackId),
	/// Moves the value at `0` in the stack to the return value hold.
	Return,
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
			Self::Return =>
				write!(f, "Return"),
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
/// The ID to a stack value. 
/// 
/// Used to index stack values in [`QuVm`].
/// 
/// # Example
/// 
/// ```
/// use qu::Qu;
/// use qu::QuStackId;
/// 
/// let mut qu = Qu::new();
/// 
/// qu.get_stack_value(QuStackId::from(0));
/// ```
pub struct QuStackId(pub usize);
impl From<usize> for QuStackId {

	fn from(v:usize) -> Self {
		Self(v)
	}

} impl From<isize> for QuStackId {

	fn from(v:isize) -> Self {
		Self(v as usize)
	}

} impl From<u8> for QuStackId {

	fn from(v:u8) -> Self {
		Self(v as usize)
	}

} impl From<i32> for QuStackId {

	fn from(v:i32) -> Self {
		Self(v as usize)
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
pub struct QuVm {
	/// Holds the outputed value of the last executed operation.
	pub hold_is_true:bool,

	/// Holds defined constants.
	constants:Vec<Box<dyn Any>>,
	/// Maps constant names to their index in [`QuMemory::constants`].
	constant_map:HashMap<String, QuConstId>,
	/// Holds allocated data types.
	memory:Vec<Box<dyn Any>>,
	/// Maps variable names to their index in [`QuMemory::memory`].
	memory_map:HashMap<String, QuMemId>,
	/// Holds the value returned from a Qu script.
	return_value:Option<StackValue>,

	/// The memory registers. Holds all primatives of the VM.
	stack:Vec<StackValue>,
	/// The offset of reading and writing to registers.
	stack_offset:usize,
	/// The program counter.
	pc:usize,

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

			constants: Vec::default(),
			constant_map: HashMap::default(),
			memory: Vec::default(),
			memory_map: HashMap::default(),
			return_value: None,

			stack:Vec::with_capacity(u8::MAX as usize),
			stack_offset: 0,
			pc: 0,
		};

		vm.stack.resize_with(u8::MAX as usize, ||{Box::new(QuVoid())});

		return vm;
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
				"void".into(),
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


	/// Calls an imported external function.
	fn external_call_by_id(
		&mut self,
		fn_id:QuExtFnId,
		parameters:&Vec<QuStackId>,
		imports:&QuImports,
	) -> Result<StackValue, QuMsg> {
		Ok((imports.get_fn_data_by_id(fn_id)?).1(self, parameters)?)
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


	/// Returns a memory location's ID from its name.
	/// 
	/// /// # Errors
	/// 
	/// If no memory location called `name` is found then an [`Err`] is
	/// returned.
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
	/// let mem_id = vm.get_mem_id("count")?;
	/// assert_eq!(vm.get_mem_by_id::<isize>(mem_id)?, &100isize);
	/// # return Ok(());
	/// # }
	/// ```
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


	/// Returns a mutable referance to a [`QuVm`] memory variable.
	/// 
	/// /// # Errors
	/// 
	/// If no variable by `id` is found or if the variable can't be cast
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
	/// let count_id = vm.get_mem_id("count")?;
	/// 
	/// let count = vm.get_mem_mut_by_id::<isize>(count_id)?;
	/// assert_eq!(count, &mut 100isize);
	/// # return Ok(());
	/// # }
	/// ```
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


	/// Returns a shared referance to a [`QuVm`] memory variable.
	/// 
	/// /// # Errors
	/// 
	/// If no variable by `id` is found or if the variable can't be cast
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
	/// let count_id = vm.get_mem_id("count")?;
	/// 
	/// let count = vm.get_mem_mut_by_id::<isize>(count_id)?;
	/// assert_eq!(count, &100isize);
	/// # return Ok(());
	/// # }
	/// ```
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

		let Some(cast) = any.downcast_ref::<T>()
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
	pub fn get_return_value(&mut self) -> Option<StackValue> {
		return take(&mut self.return_value);
	}


	/// Ends the current frame.
	fn frame_end(&mut self) -> Result<(), QuMsg>{
		return Err(QuMsg::general("Done"));
	}


	/// Starts a new frame.
	fn frame_start(&mut self, at_code:usize) {
		self.pc = at_code;
	}


	#[inline]
	/// Returns *true* if *hold* is a *true* value.
	fn is_hold_true(&mut self) -> bool {
		return self.hold_is_true;
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
		imports:&QuImports,
	) -> Result<(), QuMsg> {
		self.stack_offset += usize::from(ouput);
		// Assure registers is big enought to fit u8::MAX more values
		if (self.stack_offset + u8::MAX as usize) > self.stack.len() {
			self.stack.resize_with(
				self.stack_offset + u8::MAX as usize, 
				||{Box::new(QuVoid())}
			);
		}

		let return_pc = self.pc;
		let fn_obj:&QuFnObject = self.get_const_by_id(fn_id)?;
		self.frame_start(fn_obj.body.start_index);
		self.loop_ops(code, imports)?;
		self.stack_offset -= usize::from(ouput);
		self.pc = return_pc;

		return Ok(());
	}


	fn op_call_fn_ext(
		&mut self,
		func:QuExtFnId,
		args:&Vec<QuStackId>,
		output:QuStackId,
		imports:&QuImports,
	) -> Result<(), QuMsg> {
		let returned = self.external_call_by_id(
			func,
			args,
			imports,
		)?;

		self.reg_set(output, returned);

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
		self.reg_set(output, Box::new(value));
	}


	#[inline]
	/// Returns a shared referance to a stack value.
	pub fn reg_get(&self, at_reg:QuStackId) -> &StackValue {
		return &self.stack[self.stack_offset+at_reg.0];
	}


	/// Returns a shared referance to a stack value cast to type `T`.
	pub fn reg_get_as<'a, T:'a + 'static>(&self, at_reg:QuStackId
	) -> Result<&T, QuMsg> {
		let Some(r) = self.stack[self.stack_offset+at_reg.0]
			.downcast_ref::<T>() else {
			return Err(QuMsg::general(
				&format!("reg_get_as could not convert register at index {}", at_reg.0)
			))
		};
		Ok(r)
	}


	#[inline]
	/// Returns a mutable referance to a stack value.
	pub fn reg_get_mut(&mut self, at_reg:QuStackId) -> &mut StackValue {
		return &mut self.stack[self.stack_offset+at_reg.0];
	}


	/// Returns a mutable referance to a stack value cast to type `T`.
	pub fn reg_get_mut_as<'a, T:'a + 'static>(&mut self, at_reg:QuStackId
	) -> Result<&mut T, QuMsg> {
		let Some(r) = self.stack[self.stack_offset+at_reg.0]
			.downcast_mut::<T>() else {
			return Err(QuMsg::general(
				&format!("reg_get_mut_as could not convert register at index {}", at_reg.0)
			))
		};
		Ok(r)
	}


	#[inline]
	/// Sets a register value.
	pub fn reg_set(&mut self, at_reg:QuStackId, to:StackValue) {
		return self.stack[self.stack_offset+at_reg.0] = to;
	}


	/// Sets a memory location by its name.
	pub fn set_mem<'a, T:'a + 'static>(&'a mut self, name:&str, value:T
	) -> Result<(), QuMsg> {
		let Some(id) = self.memory_map.get(&name.to_owned())
			else {return Err(QuMsg::general(
				&format!("No variable by name {name} found. TODO: Better msg.")
			))};

		self.set_mem_by_id(id.clone(), value)?;

		return Ok(());
	}


	/// Sets a memory location by its ID.
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
	fn loop_ops(&mut self, code:&[QuOp], imports:&QuImports
	) -> Result<(), QuMsg>{
		while self.pc != code.len() {
			let result
				= self.do_next_op(code, imports);
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
	fn do_next_op(&mut self, instructions:&[QuOp], imports:&QuImports
	) -> Result<(), QuMsg> {
		let op = self.next_op(instructions);
		//println!("Doing op: {:?}", &op);
		match op {
			QuOp::Call(fn_id, ouput) => self.op_call_fn(*fn_id, *ouput, instructions, imports)?,
			QuOp::CallExt(fn_id, args, output) => self.op_call_fn_ext(*fn_id, args, *output, imports)?,
			QuOp::End => self.frame_end()?,
			QuOp::DefineFn(name, length) => self.op_define_fn(name.clone(), *length),
			QuOp::JumpByIfNot(by) => self.op_jump_by_if_not(*by),
			QuOp::JumpBy(by) => self.op_jump_by(*by),
			QuOp::Value(value, output) => self.op_load_int(*value, *output),
			QuOp::Return => self.return_value = Some(replace(self.reg_get_mut(0.into()), Box::new(QuVoid()))),
		};
		return Ok(());
	}


	/// Runs a [`Vec`] of instructions.
	pub fn run_ops(&mut self, instructions:&[QuOp], imports:&QuImports
	) -> Result<(), QuMsg> {
		self.pc = 0;
		while self.pc != instructions.len() {
			match self.do_next_op(&instructions, &imports) {
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


pub trait QuAny: Any+Clone {}


#[cfg(test)]
mod test_vm {
	use crate::QuVm;
	use crate::QuMsg;

	#[test]
	fn define_and_get_const() -> Result<(), QuMsg> {
		let mut vm = QuVm::new();

		vm.define_const(
			"MEANING_OF_LIFE".into(), Box::new(42isize));
		vm.define_const(
			"NAME".into(), Box::new("Qu"));

		let meaning = vm.get_const::<isize>("MEANING_OF_LIFE")?;
		assert_eq!(*meaning, 42isize);

		let name = vm.get_const::<&str>("NAME")?;
		assert_eq!(*name, "Qu");

		let wrong_type
			= vm.get_const::<&str>("MEANING_OF_LIFE");
		assert!(wrong_type.is_err());

		let not_defined
			= vm.get_const::<isize>("DIVIDE_BY_INF");
		assert!(not_defined.is_err());

		return Ok(());
	}


	#[test]
	fn define_and_get_mem() -> Result<(), QuMsg> {
		let mut vm = QuVm::new();

		vm.define_mem("health".to_owned(), Box::new(10.0f32));
		assert_eq!(*vm.get_mem::<f32>("health")?, 10.0f32);

		vm.define_mem("lives".to_owned(), Box::new(3isize));
		assert_eq!(*vm.get_mem::<isize>("lives")?, 3isize);

		let health = vm.get_mem_mut::<f32>("health")?;
		*health = 5.0;
		assert_eq!(*vm.get_mem::<f32>("health")?, 5.0f32);

		let lives = vm.get_mem_mut::<isize>("lives")?;
		*lives = 2;
		assert_eq!(*vm.get_mem::<isize>("lives")?, 2isize);

		let bad_type
			= vm.get_const::<&str>("health");
		assert!(bad_type.is_err());
		
		let not_defined
			= vm.get_const::<isize>("i_dont_exist");
		assert!(not_defined.is_err());

		return Ok(());
	}

}