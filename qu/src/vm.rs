
use std::any::Any;
use std::collections::HashMap;

use crate::QuMsg;
use crate::objects::QuCodeObject;
use crate::objects::QuFnObject;
use crate::objects::QuType;


/// Defines a [QuOpLibrary] struct.
/// 
/// Example:
/// ```
/// //FIXME:
/// //struct_QuOpLibrary!{
/// //	[0] end:END()
/// //	[1] add:ADD((1,), (1,), (1,))
/// //}
/// //let oplib = QuOpLibrary::new();
/// //assert_eq(oplib.end, 0);
/// //assert_eq(oplib.add, 1);
/// ```
/// 
/// Expands into:
/// ```
/// //FIXME:
/// //pub struct QuOpLibrary<'a> {
/// //	ops: Vec<QuOperation<'a>>,
/// //	end: u8,
/// //	add: u8,
/// //}
/// //impl<'a> QuOpLibrary<'a> {
/// //	fn new() -> Self {
/// //		return Self {
/// //			ops: vec![
/// //				QuOperation::new("end", "END", &[(1,)]),
/// //				QuOperation::new("add", "ADD", &[(1,), (1,), (1,)]),
/// //			],
/// //			end: 0,
/// //			add: 1,
/// //		};
/// //	}
/// //}
/// ```
macro_rules! struct_QuOpLibrary {

	( $(  [$idx:expr] $name:ident:$asm_keyword:ident( $($args:expr),* )  )+ ) => {
		/// A struct that holds all the metadata for [QuVm]'s operations. Meant
		/// to be used as a static variable.
		pub struct QuOpLibrary<'a> {
			/// The [Vec] of [QuOperations]. It holds the metadata.
			pub ops:Vec<QuOperation<'a>>,
			$(
				/// The index of &name in [ops].
				pub $name:u8,
			)+
		} impl<'a> QuOpLibrary<'a> {
			
			fn new() -> Self {
				return Self{
					ops:vec![
						$(
							QuOperation::new(stringify!($name), stringify!($asm_keyword), $idx, &[   $( $args, )*   ]),
						)+
					],
					
					$(
						$name:$idx,
					)+
				};
			}


			pub fn op_args_size(&self, op:u8) -> usize {
				let mut size = 0;
				for arg in self.ops[op as usize].args {
					size += arg.size(&vec![0 as u8], 0);
				}
				return size;
			}


			/// Converts a math or logic symbol to the id of the [QuOperation] that will
			/// perform it.
			pub fn op_id_from_symbol(&self, symbol:&str) -> u8 {
				return match symbol {
					"+" => self.add,
					"-" => self.sub,
					"*" => self.mul,
					"/" => self.div,
					"**" => self.pow,
					"%" => self.modulate,
					">" => self.greater,
					"<" => self.lesser,
					"==" => self.equal,
					"!=" => self.not_equal,
					_ => panic!("Unknown Qu VM operation symbol: {}", symbol),
				};
			}

		}
	};
}

/// Generates code for [QuVm] math operation functions.
macro_rules! vm_exc_math_all {
	( $(op $name:ident:$op:tt),* ) => {
		$(
			/// Excecutes a QuVm unsigned math operation
			fn $name(&mut self, code:&[u8]) {
				// TODO: Performance improvements by operating on value_hold
				// rather than reading a register
				let rg_left = self.next_u8(code) as usize;
				let rg_right = self.next_u8(code) as usize;
				let rg_output = self.next_u8(code) as usize;
				self.hold = (
					self.reg_get(rg_left)
					$op self.reg_get(rg_right)
				) as usize;
				self.reg_set(rg_output, self.hold);
			}
		)*
	};

	( $(fn $name:ident:$func:path),* ) => {
		$(
			/// Excecutes a QuVm unsigned function call
			fn $name(&mut self, code:&[u8]) {
				let rg_left = self.next_u8(code) as usize;
				let rg_right = self.next_u8(code) as usize;
				let rg_output = self.next_u8(code) as usize;
				self.hold = ($func(
					self.reg_get(rg_left),
					self.reg_get(rg_right) as u32)
				) as usize;
				self.reg_set(rg_output, self.hold);
			}
		)*
	};
}


lazy_static!{
	/// I don't know what this is documenting...
	pub static ref OPLIB:QuOpLibrary<'static> = QuOpLibrary::new();
}


/// A tuple of for specifying arguments for a [QuOperation].
type CommandArg = QuAsmTypes;
/// The [QuVm] registers type.
type RegisterValue = usize;


enum QuAsmTypes {
	UInt8,
	UInt16,
	Uint32,
	UInt64,
	Str,
} impl QuAsmTypes {
	fn size(&self, code:&[u8], at:usize) -> usize {
		return match self {
			Self::UInt8 => 1,
			Self::UInt16 => 2,
			Self::Uint32 => 4,
			Self::UInt64 => 8,
			Self::Str => code[at] as usize,
		};
	}
}


// Define the [QuOpLibrary] struct
struct_QuOpLibrary!{
	[  0] end:END()

	[  1] load_val_u8:LDU8(QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[  2] load_val_u16:LDU16(QuAsmTypes::UInt16, QuAsmTypes::UInt8)
	[  3] load_val_u32:LDU32(QuAsmTypes::Uint32, QuAsmTypes::UInt8)
	[  4] load_val_u64:LDU64(QuAsmTypes::UInt64, QuAsmTypes::UInt8)

	[  5] load_mem:LDM(QuAsmTypes::Uint32, QuAsmTypes::UInt8)
	[  6] store_mem:STM(QuAsmTypes::UInt8, QuAsmTypes::Uint32)
	[  7] copy_reg:CPY(QuAsmTypes::UInt8, QuAsmTypes::UInt8)

	[  8] add:ADD(QuAsmTypes::UInt8, QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[  9] sub:SUB(QuAsmTypes::UInt8, QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[ 10] mul:MUL(QuAsmTypes::UInt8, QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[ 11] div:DIV(QuAsmTypes::UInt8, QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[ 12] modulate:MOD(QuAsmTypes::UInt8, QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[ 13] pow:POW(QuAsmTypes::UInt8, QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[ 14] lesser:LES(QuAsmTypes::UInt8, QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[ 15] greater:GRT(QuAsmTypes::UInt8, QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[ 16] equal:EQ(QuAsmTypes::UInt8, QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[ 17] not_equal:NEQ(QuAsmTypes::UInt8, QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[ 18] not:NOT(QuAsmTypes::UInt8, QuAsmTypes::UInt8)

	[ 19] jump_to:JP(QuAsmTypes::Uint32)
	[ 20] jump_by:JB(QuAsmTypes::Uint32)
	[ 21] jump_to_if_not:JPIN(QuAsmTypes::Uint32)
	[ 22] jump_by_if_not:JBIN(QuAsmTypes::Uint32)

	[ 23] print:PRT(QuAsmTypes::UInt8)

	[ 24] call:CALL(QuAsmTypes::Uint32, QuAsmTypes::UInt8)

	[ 25] define_fn:DFFN(QuAsmTypes::Str, QuAsmTypes::Uint32)
	[ 26] define_const_str:DFCS(QuAsmTypes::Str)

	[ 27] parameter_add:PAD(QuAsmTypes::UInt8)
	[ 28] parameter_pop:PPO(QuAsmTypes::UInt8)
}


/// Contains metadata for a single Qu VM operation.
pub struct QuOperation<'a> {
	/// The name of this operation.
	name:String,
	/// The assembly keyword that represents this operation.
	asm_keyword:String,
	/// The arguments that this operation takes.
	args:&'a [CommandArg],
	/// The index that this operation is stored at in the op [Vec].
	id:u8,

} impl<'a> QuOperation<'a> {

	fn new(name:&str, asm_keyword:&str, id:u8, args:&'a [CommandArg]) -> Self {
		return Self{
			name:name.to_string(),
			asm_keyword:asm_keyword.to_string(),
			args:args,
			id:id,
		};
	}

}


/// The virtual machine that runs Qu code.
/// 
/// This struct is not meant to be accessed directly (in most cases). See
/// [`qu::Qu`] for interfacing with Qu script.
#[derive(Debug, Default)]
pub struct QuVm {
	/// Holds the outputed value of the last executed operation.
	hold:usize,

	/// Holds defined constants.
	constants:Vec<Box<dyn Any>>,
	/// Maps constant names to their index in [`QuMemory::constants`].
	constant_map:HashMap<String, usize>,
	/// Holds allocated data types.
	memory:Vec<Box<dyn Any>>,
	/// Maps variable names to their index in [`QuMemory::memory`].
	memory_map:HashMap<String, usize>,

	/// The memory registers. Holds all primatives of the VM.
	registers:Vec<RegisterValue>,
	/// The offset of reading and writing to registers.
	register_offset:usize,
	/// The program counter.
	pc:usize,
	/// Holds the parameters of the next function.
	parameters:Vec<RegisterValue>,

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
			hold: usize::default(),

			constants: Vec::default(),
			constant_map: HashMap::default(),
			memory: Vec::default(),
			memory_map: HashMap::default(),

			registers:Vec::with_capacity(u8::MAX as usize),
			register_offset: 0,
			pc: 0,
			parameters: Vec::default(),
		};

		vm.registers.resize(u8::MAX as usize, 0);

		return vm;
	}


	/// Converts byte code to human readable Qu assembly instructions.
	/// 
	/// # Example
	/// 
	/// ```
	/// use qu::QuVm;
	/// 
	/// let asm = QuVm::code_to_asm(&[8, 5, 6, 0], false);
	/// assert_eq!(asm, "\nADD 5 6 0".to_owned());
	/// ```
	pub fn code_to_asm(code:&[u8], include_line_columns:bool
	) -> String {
		let mut asm = String::new();
		
		let mut i = 0;
		while i < code.len() {
			let op_code = code[i];
			// HACK: Skip commands if they exceed the ops length.
			if op_code as usize >= OPLIB.ops.len() {
				i += 1;
				// Add error text
				asm.push_str(format!("\n{:.>8}-{:.<8} {}",
					i, i, "INVALID OPERATION").as_str());
				continue;
			}
			let op
					= &OPLIB.ops[op_code as usize];
			assert!(op.id == op_code);

			asm.push_str("\n");
			// Add line/index columns
			if include_line_columns {
				asm.push_str(
					format!("{:.>8}-{:.<8} ",
					i, i+op.args.len()).as_str()
				);
			}
			// Add code text
			asm.push_str(
				format!("{}",op.asm_keyword).as_str());

			// Add parameter text
			for asm_type in op.args.iter() {
				let size = asm_type.size(code, i+1);
				// Get value
				let val = match asm_type {
					QuAsmTypes::UInt8 => {
						let bytes = [code[i+1]];
						i += 1;
						format!("{}", u8::from_be_bytes(bytes))
					}
					QuAsmTypes::UInt16 => {
						let bytes = [code[i+1], code[i+2]];
						i += 2;
						format!("{}", u16::from_be_bytes(bytes))
					}
					QuAsmTypes::Uint32 => {
						let bytes = [
							code[i+1], code[i+2], code[i+3], code[i+4]];
						i += 4;
						format!("{}", u32::from_be_bytes(bytes))
					}
					QuAsmTypes::UInt64 => {
						let bytes = [
							code[i+1], code[i+2], code[i+3], code[i+4],
							code[i+5], code[i+6], code[i+7], code[i+8]];
						i += 8;
						format!("{}", u64::from_be_bytes(bytes))
					}
					QuAsmTypes::Str => {
						let mut val = "\"".to_string();
						for _ in 0..size+1 {
							val.push(code[i+1] as char);
							i += 1;
						}
						val.push('"');
						val
					}
				};
				asm.push_str(format!(" {}{}", "", val).as_str());
			}
			i += 1;
		}

		return asm;
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
		self.constant_map.insert(name.to_owned(), const_index);
	}


	/// Defines a Qu function.
	fn define_fn(&mut self, name:&str, code_start:usize) {
		self.define_const(
			name.to_owned(),
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
		self.memory_map.insert(name.to_owned(), const_index);
	}


	fn exc_copy_reg(&mut self, code:&[u8]) {
		let from_reg = self.next_u8(code) as usize;
		let to_reg = self.next_u8(code) as usize;
		self.reg_set(to_reg, self.reg_get(from_reg));
	}


	/// Reads the bytecode of a function call command and executes it.
	fn exc_call_fn(&mut self, code:&[u8]) -> Result<(), QuMsg> {
		let fn_id = self.next_u32(code) as usize;
		let frame_offset = self.next_u8(code) as usize;
		let fn_obj:&QuFnObject = self.get_const_by_index(fn_id)?;
		let code_start = fn_obj.body.start_index;

		self.register_offset += frame_offset;
		// Assure registers is big enought to fit u8::MAX more values
		if (self.register_offset + u8::MAX as usize) > self.registers.len() {
			self.registers.resize(
				self.register_offset + u8::MAX as usize, 0);
		}
		let return_pc = self.pc;

		self.frame_start(code_start);
		self.do_loop(code)?;

		self.register_offset -= frame_offset;
		self.pc = return_pc;

		return Ok(());
	}


	/// Defines a function from next bytes in the code.
	fn exc_define_fn(&mut self, code:&[u8]) {
		let name = self.next_ascii(code);
		let fn_length = self.next_u32(code) as usize;

		self.define_fn(&name, self.pc);

		self.pc += fn_length;
	}


	fn exc_jump_by(&mut self, code:&[u8]) {
		let val_by = self.next_u32(code) as i32;
		// Add
		if val_by > 0 {
			self.pc += val_by as usize;
		// Subtract
		} else {
			self.pc -= val_by.abs() as usize;
		}
		
	}


	fn exc_jump_by_if_not(&mut self, code:&[u8]) {
		let val_by = self.next_u32(code) as i32;
		if !self.is_hold_true() {
			// Add
			if val_by > 0 {
				self.pc += val_by as usize;
			// Subtract
			} else {
				self.pc -= val_by.abs() as usize;
			}
		}
	}


	fn exc_jump_to(&mut self, _:&[u8]) {
		unimplemented!()
	}


	fn exc_jump_to_if_not(&mut self, code:&[u8]) {
		let val_to = self.next_u32(code) as usize;
		if !self.is_hold_true() {
			self.pc = val_to as usize;
		}
	}


	fn exc_load_val_u8(&mut self, code:&[u8]) {
		self.hold = self.next_u8(code) as RegisterValue;
		let rg_to = self.next_u8(code) as usize;
		self.reg_set(rg_to, self.hold);
	}


	fn exc_load_val_u16(&mut self, code:&[u8]) {
		self.hold = self.next_u16(code) as RegisterValue;
		let rg_to = self.next_u8(code) as usize;
		self.reg_set(rg_to, self.hold);
	}


	fn exc_load_val_u32(&mut self, code:&[u8]) {
		self.hold = self.next_u32(code) as RegisterValue;
		let rg_to = self.next_u8(code) as usize;
		self.reg_set(rg_to, self.hold);
	}


	fn exc_load_val_u64(&mut self, code:&[u8]) {
		self.hold = self.next_u64(code) as RegisterValue;
		let rg_to = self.next_u8(code) as usize;
		self.reg_set(rg_to, self.hold);
	}


	fn exc_load_mem(&mut self, _:&[u8]) {
		unimplemented!();
	}


	/// Adds a register value as a parameter for a function.
	fn exc_parameter_add(&mut self, code:&[u8]) {
		let reg = self.next_u8(code) as RegisterValue;
		self.parameters.push(self.registers[reg]);
	}


	/// Pops a parameter value into a register.
	fn exc_parameter_pop(&mut self, code:&[u8]) -> Result<(), QuMsg> {
		let Some(x) = self.parameters.pop()
			else {return Err(QuMsg::general(
				"Could not pop parameter. Parameters is empty. TODO: Better msg"
			))};
		let to_reg = self.next_u8(code) as usize;
		self.registers[to_reg] = x;
		return Ok(());
	}


	fn exc_print(&mut self, code:&[u8]) {
		let read_from_reg = self.next_u8(code) as usize;
		let val = self.reg_get(read_from_reg);
		println!("Qu Print: {}", val);
	}


	fn exc_store_mem(&mut self, _:&[u8]) {
		unimplemented!();
	}


	vm_exc_math_all!{
		op exc_math_add: +,
		op exc_math_sub: -,
		op exc_math_mul: *,
		op exc_math_div: /,
		op exc_math_mod: %,
		op exc_logi_equal: ==,
		op exc_logi_greater: >,
		op exc_logi_lesser: <,
		op exc_logi_not_equal: !=
	}

	
	vm_exc_math_all!{
		fn exc_math_pow: usize::pow
	}


	fn exc_logi_not(&mut self, code:&[u8]) {
		let rg_left = self.next_u8(code) as usize;
		let rg_result = self.next_u8(code) as usize;
		let x = self.reg_get(rg_left);
		self.reg_set(rg_result,	(x*0 == x) as RegisterValue);
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
	/// vm.define_const("MEANING_OF_LIFE".to_owned(), Box::new(42));
	/// 
	/// let constant = vm.get_const::<i32>("MEANING_OF_LIFE")?;
	/// assert_eq!(*constant, 42);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn get_const<'a, T:'a + 'static>(&'a self, name:&str
	) -> Result<&T, QuMsg> {
		let Some(index) = self.constant_map.get(&name.to_owned())
			else {return Err(QuMsg::general(
				&format!("No constant by name {name} found. TODO: Better msg.")
			))};
		
		let const_ref = self.get_const_by_index::<T>(*index)?;

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
	/// vm.define_const("BITS".to_owned(), Box::new(128));
	/// 
	/// let BITS = vm.get_const_by_index::<i32>(0)?;
	/// assert_eq!(*BITS, 128);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn get_const_by_index<'a, T:'a + 'static>(&'a self, index:usize
	) -> Result<&T, QuMsg> {
		let Some(any) = self.constants.get(index)
			else {return Err(QuMsg::general(
				"Can't acsess constant by index {index}. Index is out of range. TODO: Better msg"
			))};

		let Some(cast) = any.downcast_ref::<T>()
			else {return Err(QuMsg::general(
				&format!("Can't cast constant to specified T. TODO: Better msg.")
			))};

		return Ok(cast);
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
	/// vm.define_mem("count".to_owned(), Box::new(100));
	/// 
	/// let count = vm.get_mem_mut::<i32>("count")?;
	/// assert_eq!(*count, 100);
	/// 
	/// *count += 5;
	/// let altered_count = vm.get_mem_mut::<i32>("count")?;
	/// assert_eq!(*altered_count, 105);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn get_mem_mut<'a, T:'a + 'static>(&'a mut self, name:&str
	) -> Result<&mut T, QuMsg> {
		let Some(index) = self.memory_map.get(&name.to_owned())
			else {return Err(QuMsg::general(
				&format!("No variable by name {name} found. TODO: Better msg.")
			))};
		
		let cast = self.get_mem_mut_by_index(*index)?
			else {return Err(QuMsg::general(
				&format!("Can't cast variable to specified T. TODO: Better msg.")
			))};

		return Ok(cast);
	}


	/// Returns a mutable reference to a variable in Qu memory by its index.
	/// 
	/// # Errors
	/// 
	/// If `index` is out of range or if the variable can't be cast to `T` then
	/// an [`Err`] is returned.
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
	/// vm.define_mem("apple_count".to_owned(), Box::new(25));
	/// 
	/// let apples = vm.get_mem_mut_by_index::<i32>(0)?;
	/// assert_eq!(*apples, 25);
	/// 
	/// *apples += 5;
	/// let altered_apples = vm.get_mem_mut_by_index::<i32>(0)?;
	/// assert_eq!(*altered_apples, 30);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn get_mem_mut_by_index<'a, T:'a + 'static>(&'a mut self, index:usize
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
	/// vm.define_mem("count".to_owned(), Box::new(100));
	/// 
	/// let count = vm.get_mem::<i32>("count")?;
	/// assert_eq!(*count, 100);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn get_mem<'a, T:'a + 'static>(&'a self, name:&str
	) -> Result<&T, QuMsg> {
		let Some(index) = self.memory_map.get(&name.to_owned())
			else {return Err(QuMsg::general(
				&format!("No variable by name {name} found. TODO: Better msg.")
			))};

		return Ok(self.get_mem_by_index(*index)?);
	}


	/// Returns a reference to a variable in Qu memory by its index.
	/// 
	/// # Errors
	/// 
	/// If `index` is out of range or if the variable can't be
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
	/// vm.define_mem("count".to_owned(), Box::new(100));
	/// 
	/// let count = vm.get_mem_by_index::<i32>(0)?;
	/// assert_eq!(*count, 100);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn get_mem_by_index<'a, T:'a + 'static>(&'a self, index:usize
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
		return self.hold != 0;
	}


	/// Reads a [String] from the next bytes in the code.
	fn next_ascii(&mut self, code:&[u8]) -> String {
		let str_size = self.next_u8(code);
		let mut str_vec = Vec::with_capacity(str_size as usize);
		for _ in 0..str_size {
			str_vec.push(self.next_u8(code));
		}

		return String::from_utf8(str_vec).expect("TODO: Handle this error");
	}


	/// Gets the next byte in the source code as a [`u8`].
	fn next_u8(&mut self, code:&[u8]) -> u8 {
		let val = code[self.pc];
		self.pc += 1;
		return val;
	}


	/// Gets the next 2 byte in the source code as a [`u16`].
	fn next_u16(&mut self, code:&[u8]) -> u16 {
		let bytes = [code[self.pc], code[self.pc+1]];
		self.pc += 2;
		return u16::from_be_bytes(bytes);
	}


	/// Gets the next 4 byte in the source code as a [`u32`].
	fn next_u32(&mut self, code:&[u8]) -> u32 {
		let bytes = [
			code[self.pc], code[self.pc+1],
			code[self.pc+2], code[self.pc+3],
		];
		self.pc += 4;
		return u32::from_be_bytes(bytes);
	}


	// Gets the next 8 byte in the source code as a [`u64`].
	fn next_u64(&mut self, code:&[u8]) -> u64 {
		let bytes = [
			code[self.pc], code[self.pc+1],
			code[self.pc+2], code[self.pc+3],
			code[self.pc+4], code[self.pc+5],
			code[self.pc+6], code[self.pc+7],
		];
		self.pc += 8;
		return u64::from_be_bytes(bytes);
	}


	#[inline]
	/// Gets a register value.
	fn reg_get(&self, at:usize) -> RegisterValue {
		return self.registers[self.register_offset+at];
	}


	#[inline]
	/// Sets a register value.
	fn reg_set(&mut self, at:usize, with:RegisterValue) {
		return self.registers[self.register_offset+at] = with;
	}


	/// Runs inputed bytecode in a loop.
	fn do_loop(&mut self, bytecode:&[u8]
	) -> Result<(), QuMsg>{
		while self.pc != bytecode.len() {
			let result = self.do_next(bytecode);
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
	fn do_next(&mut self, bytecode:&[u8]) -> Result<(), QuMsg> {
		let op = self.next_u8(bytecode);
		//println!("Doing op: {}", &OPLIB.ops[op as usize].name);
		match op {
			x if x == OPLIB.end as u8 => {self.frame_end()?},

			x if x == OPLIB.load_val_u8 => self.exc_load_val_u8(bytecode),
			x if x == OPLIB.load_val_u16 => self.exc_load_val_u16(bytecode),
			x if x == OPLIB.load_val_u32 => self.exc_load_val_u32(bytecode),
			x if x == OPLIB.load_val_u64 => self.exc_load_val_u64(bytecode),
			x if x == OPLIB.load_mem => self.exc_load_mem(bytecode),
			x if x == OPLIB.store_mem => self.exc_store_mem(bytecode),
			x if x == OPLIB.copy_reg => self.exc_copy_reg(bytecode),
			x if x == OPLIB.add => self.exc_math_add(bytecode),
			x if x == OPLIB.sub => self.exc_math_sub(bytecode),
			x if x == OPLIB.mul => self.exc_math_mul(bytecode),
			x if x == OPLIB.div => self.exc_math_div(bytecode),
			x if x == OPLIB.modulate => self.exc_math_mod(bytecode),
			x if x == OPLIB.pow => self.exc_math_pow(bytecode),
			x if x == OPLIB.lesser => self.exc_logi_lesser(bytecode),
			x if x == OPLIB.greater => self.exc_logi_greater(bytecode),
			x if x == OPLIB.equal => self.exc_logi_equal(bytecode),
			x if x == OPLIB.not_equal => self.exc_logi_not_equal(bytecode),
			x if x == OPLIB.not => self.exc_logi_not(bytecode),
			x if x == OPLIB.jump_to => self.exc_jump_to(bytecode),
			x if x == OPLIB.jump_by => self.exc_jump_by(bytecode),
			x if x == OPLIB.jump_to_if_not => self.exc_jump_to_if_not(bytecode),
			x if x == OPLIB.jump_by_if_not => self.exc_jump_by_if_not(bytecode),
			x if x == OPLIB.print => self.exc_print(bytecode),
			x if x == OPLIB.call => self.exc_call_fn(bytecode)?,
			x if x == OPLIB.define_fn => self.exc_define_fn(bytecode),
			x if x == OPLIB.parameter_add => self.exc_parameter_add(bytecode),
			x if x == OPLIB.parameter_pop => self.exc_parameter_pop(bytecode)?,

			x => { println!("{x}"); todo!(); }
		};
		return Ok(());
	}


	/// Runs `bytecode`.
	/// 
	/// # Errors
	/// 
	/// If `bytecode` does not pass the sanity checks or there is a runtime
	/// error then an [`Err`] is returend.
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
	/// vm.run_bytes(&vec![1, 5, 0])?;
	/// # return Ok(());
	/// # }
	/// ```
	pub fn run_bytes(&mut self, bytecode:&[u8]) -> Result<(), QuMsg> {
		while self.pc != bytecode.len() {
			match self.do_next(&bytecode) {
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


#[cfg(test)]
mod test_vm {
    use std::any::Any;

    use crate::{Qu, QuVm, QuMsg};

	#[test]
	fn define_and_get_const() -> Result<(), QuMsg> {
		let mut vm = QuVm::new();
		vm.define_const("MEANING_OF_LIFE".to_owned(), Box::new(42));
		vm.define_const("NAME".to_owned(), Box::new("Qu"));

		let meaning = vm.get_const::<i32>("MEANING_OF_LIFE")?;
		let name = vm.get_const::<&str>("NAME")?;
		let bad_type = vm.get_const::<&str>("MEANING_OF_LIFE");
		let not_defined = vm.get_const::<i32>("DIVIDE_BY_INF");
		
		assert_eq!(*meaning, 42);
		assert_eq!(*name, "Qu");
		assert!(bad_type.is_err());
		assert!(not_defined.is_err());

		return Ok(());
	}


	#[test]
	fn define_and_get_mem() -> Result<(), QuMsg> {
		let mut vm = QuVm::new();
		vm.define_mem("health".to_owned(), Box::new(10.0f32));
		vm.define_mem("lives".to_owned(), Box::new(3));

		assert_eq!(*vm.get_mem::<f32>("health")?, 10.0);
		assert_eq!(*vm.get_mem::<i32>("lives")?, 3);

		let health = vm.get_mem_mut::<f32>("health")?;
		*health = 5.0;

		let lives = vm.get_mem_mut::<i32>("lives")?;
		*lives = 2;

		assert_eq!(*vm.get_mem::<f32>("health")?, 5.0);
		assert_eq!(*vm.get_mem::<i32>("lives")?, 2);

		let bad_type = vm.get_const::<&str>("health");
		let not_defined = vm.get_const::<i32>("i_dont_exist");
		
		assert!(bad_type.is_err());
		assert!(not_defined.is_err());

		return Ok(());
	}


	#[test]
	fn test_runtime_object_storage() -> Result<(), QuMsg>{
		let mut v:Vec<Box<dyn Any>> = vec![];
		v.push(Box::new(5u8));
		v.push(Box::new("hello wolrd!"));

		let a = &v[0];
		let b = &v[1];

		let Some(a_int) = a.downcast_ref::<u8>()
			else {return Err(QuMsg::general("First is not int"))};
		let Some(b_str) = b.downcast_ref::<&str>()
			else {return Err(QuMsg::general("Second is not str"))};
		
		assert_eq!(a_int, &5u8);
		assert_eq!(b_str, &"hello wolrd!");

		return Ok(());
	}

}