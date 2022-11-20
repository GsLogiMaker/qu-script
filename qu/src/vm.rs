
use std::any::Any;
use std::any::TypeId;
use std::collections::HashMap;
use std::fmt::Debug;
use std::mem::take;

use crate::QuExtFnData;
use crate::QuInt;
use crate::QuMsg;
use crate::QuRegisterStruct;
use crate::QuValue;
use crate::QuVoid;
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


lazy_static!{
	/// I don't know what this is documenting...
	pub static ref OPLIB:QuOpLibrary<'static> = QuOpLibrary::new();
}


pub type QuExtFn = &'static dyn Fn(
	&mut QuVm,
	QuRegId,
	Vec<QuRegId>
	)->Result<RegisterValue, QuMsg>;
pub type QuVoidExtFn = &'static dyn Fn(
	&mut QuVm,
	QuRegId,
	Vec<QuRegId>
	)->Result<(), QuMsg>;
/// A tuple of for specifying arguments for a [QuOperation].
type CommandArg = QuAsmTypes;
/// The [QuVm] registers type.
pub type RegisterValue = Box<dyn Any>;


enum QuAsmTypes {
	UInt8,
	UInt16,
	UInt32,
	UInt64,
	Str,
} impl QuAsmTypes {
	fn size(&self, code:&[u8], at:usize) -> usize {
		return match self {
			Self::UInt8 => 1,
			Self::UInt16 => 2,
			Self::UInt32 => 4,
			Self::UInt64 => 8,
			Self::Str => code[at] as usize,
		};
	}
}


pub enum QuRegisterValue {
	Void,
	Int(isize),
	Bool(bool),
	Tuple(Box<(usize, Vec<QuRegisterValue>)>),
	/// Having a box point to a box is used to keep QuRegisterValue at 16 bytes
	Object(Box<Box<dyn Any>>),
}


// Define the [QuOpLibrary] struct
struct_QuOpLibrary!{
	[  0] end:END()

	[  1] load_val_u8:LDU8(QuAsmTypes::UInt8, QuAsmTypes::UInt8)
	[  2] load_val_u16:LDU16(QuAsmTypes::UInt16, QuAsmTypes::UInt8)
	[  3] load_val_u32:LDU32(QuAsmTypes::UInt32, QuAsmTypes::UInt8)
	[  4] load_val_u64:LDU64(QuAsmTypes::UInt64, QuAsmTypes::UInt8)

	[  5] load_mem:LDM(QuAsmTypes::UInt32, QuAsmTypes::UInt8)
	[  6] store_mem:STM(QuAsmTypes::UInt8, QuAsmTypes::UInt32)
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

	[ 19] jump_to:JP(QuAsmTypes::UInt32)
	[ 20] jump_by:JB(QuAsmTypes::UInt32)
	[ 21] jump_to_if_not:JPIN(QuAsmTypes::UInt32)
	[ 22] jump_by_if_not:JBIN(QuAsmTypes::UInt32)

	[ 23] print:PRT(QuAsmTypes::UInt8)

	[ 24] call:CALL(QuAsmTypes::UInt32, QuAsmTypes::UInt8)

	[ 25] define_fn:DFFN(QuAsmTypes::Str, QuAsmTypes::UInt32)
	[ 26] define_const_str:DFCS(QuAsmTypes::Str)

	[ 27] call_ext:CEXT(QuAsmTypes::UInt8, QuAsmTypes::UInt32, QuAsmTypes::UInt8 /* Any number of parameters */)

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



#[derive(Clone, Default)]
pub struct QuStruct {
	name:String,
	functions:Vec<QuExtFn>,
	functions_map:HashMap<String, QuFunctionId>,
} impl QuStruct {

	fn new(
		name:String,
	) -> Self {
		return Self {
			name,
			functions: Vec::default(),
			functions_map: HashMap::default(),
		};
	}

}


#[derive(Clone, Copy, Debug, Default)]
pub struct QuFunctionId {
	index:usize,
} impl QuFunctionId {

	fn new(index:usize) -> Self {
		Self {
			index,
		}
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


#[derive(Clone, Copy, Debug, Default)]
pub struct QuRegId(pub usize);


#[derive(Clone, Copy, Debug, Default)]
pub struct QuStructId {
	index:usize,
} impl QuStructId {

	fn new(index:usize) -> Self {
		return Self{
			index
		};
	}

}


/// The virtual machine that runs Qu code.
/// 
/// This struct is not meant to be accessed directly (in most cases). See
/// [`qu::Qu`] for interfacing with Qu script.
pub struct QuVm {
	/// Holds the outputed value of the last executed operation.
	hold:RegisterValue,
	pub is_zero:bool,

	/// Holds defined constants.
	constants:Vec<Box<dyn Any>>,
	/// Maps constant names to their index in [`QuMemory::constants`].
	constant_map:HashMap<String, usize>,
	/// Holds allocated data types.
	memory:Vec<Box<dyn Any>>,
	/// Maps variable names to their index in [`QuMemory::memory`].
	memory_map:HashMap<String, QuMemId>,
	/// Holds the value returned from a Qu script.
	return_value:Option<QuValue>,

	registered_fns:Vec<QuExtFnData>,
	registered_structs:Vec<QuStruct>,
	registered_structs_map:HashMap<String, QuStructId>,

	/// The memory registers. Holds all primatives of the VM.
	registers:Vec<RegisterValue>,
	/// The offset of reading and writing to registers.
	register_offset:usize,
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
			hold: Box::new(QuVoid()),
			is_zero: false,

			constants: Vec::default(),
			constant_map: HashMap::default(),
			memory: Vec::default(),
			memory_map: HashMap::default(),
			return_value: None,

			registered_fns: Vec::default(),
			registered_structs: Vec::default(),
			registered_structs_map: HashMap::default(),

			registers:Vec::with_capacity(u8::MAX as usize),
			register_offset: 0,
			pc: 0,
		};

		vm.registers.resize_with(u8::MAX as usize, ||{Box::new(QuVoid())});

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
					QuAsmTypes::UInt32 => {
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
						let mut val = "\"".to_owned();
						for _ in 1..size+1 {
							i += 1;
							val.push(code[i+1] as char);
						}
						i += 1;
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
		self.memory_map.insert(name.to_owned(), QuMemId::new(const_index));
	}


	/// Returns the value returned by the last run Qu script.
	/// 
	/// # Examples
	/// 
	/// //TODO: Example
	pub fn get_return_value(&mut self) -> Option<QuValue> {
		return take(&mut self.return_value);
	}


	fn exc_copy_reg(&mut self, code:&[u8]) {
		unimplemented!();
//		let from_reg = self.next_u8(code) as usize;
//		let to_reg = self.next_u8(code) as usize;
//		let to = (*self.reg_get(QuRegId(from_reg))).clone();
//		self.reg_set(QuRegId(to_reg), *to);
	}


	/// Reads the bytecode of a function call command and executes it.
	fn exc_call_fn(&mut self, code:&[u8]) -> Result<(), QuMsg> {
		let fn_id = self.next_u32(code) as usize;
		let output_reg = self.next_u8(code) as usize;
		let fn_obj:&QuFnObject = self.get_const_by_index(fn_id)?;
		let code_start = fn_obj.body.start_index;

		self.register_offset += output_reg;
		// Assure registers is big enought to fit u8::MAX more values
		if (self.register_offset + u8::MAX as usize) > self.registers.len() {
			self.registers.resize_with(
				self.register_offset + u8::MAX as usize, 
				||{Box::new(QuVoid())}
			);
		}
		let return_pc = self.pc;

		self.frame_start(code_start);
		self.do_loop(code)?;

		self.register_offset -= output_reg;
		self.pc = return_pc;

		return Ok(());
	}


	fn exc_call_fn_ext(&mut self, code:&[u8]) -> Result<(), QuMsg> {
		let on_reg = QuRegId(self.next_u8(code) as usize);
		let fn_id
			= QuFunctionId::new(self.next_u32(code) as usize);

		let Some(fn_data)
			= self.registered_fns.get(fn_id.index) else {
			return Err(QuMsg::general(
				&format!("No function by index {}", fn_id.index)
			))
		};

		let mut params = Vec::default();
		for _ in 0..fn_data.2.len() {
			params.push(QuRegId(self.next_u8(code) as usize));
		}

		let to_reg = QuRegId(self.next_u8(code) as usize);

		let returned = self.external_call_by_id(
			on_reg,
			fn_id,
			params,
		)?;

		self.reg_set(to_reg, returned);

		Ok(())
	}


	/// Defines a function from next bytes in the code.
	fn exc_define_fn(&mut self, code:&[u8]) {
		let name = self.next_ascii(code);
		let fn_start = self.next_u32(code) as usize;

		self.define_fn(&name, fn_start);
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
		let left = Box::new(QuInt(self.next_u8(code) as i32));
		let rg_to = self.next_u8(code) as usize;
		self.reg_set(QuRegId(rg_to), left);
	}


	fn exc_load_val_u16(&mut self, code:&[u8]) {
		let left = Box::new(QuInt(self.next_u16(code) as i32));
		let rg_to = self.next_u8(code) as usize;
		self.reg_set(QuRegId(rg_to), left);
	}


	fn exc_load_val_u32(&mut self, code:&[u8]) {
		let left = Box::new(QuInt(self.next_u32(code) as i32));
		let rg_to = self.next_u8(code) as usize;
		self.reg_set(QuRegId(rg_to), left);
	}


	fn exc_load_val_u64(&mut self, code:&[u8]) {
		let left = Box::new(QuInt(self.next_u64(code) as i32));
		let rg_to = self.next_u8(code) as usize;
		self.reg_set(QuRegId(rg_to), left);
	}


	fn exc_load_mem(&mut self, _:&[u8]) {
		unimplemented!();
	}


	fn exc_print(&mut self, code:&[u8]) {
		let read_from_reg = self.next_u8(code) as usize;
		let val = self.reg_get(QuRegId(read_from_reg));

		if (&**val).type_id() == TypeId::of::<QuInt>() {
			let ival = val.downcast_ref::<QuInt>().unwrap();
			println!("Qu Print: {:?}", ival.0);
		} else {
			println!("Qu Print: {:?}", val);
		}
	}


	fn exc_store_mem(&mut self, _:&[u8]) {
		unimplemented!();
	}


	pub fn external_call<S:'static+Default+QuRegisterStruct>(
		&mut self,
		obj_name:&str,
		fn_name:&str,
		parameters:Vec<QuMemId>,
	) -> Result<(), QuMsg> {

		let struct_name = <S as QuRegisterStruct>::get_name();
		let Some(struct_id)
			= self.registered_structs_map.get(&struct_name) else {
			return Err(QuMsg::general(
				&format!("No struct registered with name '{struct_name}'")
		))};

		let Some(r_struct)
		= self.registered_structs.get_mut(struct_id.index) else {
			return Err(QuMsg::general(
				&format!("No struct registered with id '{}'", struct_id.index)
		))};

		let fn_id = self.get_fn_id(fn_name, &struct_name)?;
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
		obj_id:QuRegId,
		fn_id:QuFunctionId,
		parameters:Vec<QuRegId>,
	) -> Result<RegisterValue, QuMsg> {
		Ok(self.get_fn_by_id(fn_id)?(self, obj_id, parameters)?)
	}


	/// Registers an external struct.
	pub fn external_struct_register<S:QuRegisterStruct>(&mut self) {
		let mut r_struct = QuStruct::new(
			<S as QuRegisterStruct>::get_name()
		);

		let fn_registrations
			= <S as QuRegisterStruct>::register_fns();
		for fn_data in fn_registrations {
			if r_struct.functions_map.contains_key(&fn_data.0) {
				panic!(
					"qu function with name {} already defined in struct {}",
					fn_data.0,
					<S as QuRegisterStruct>::get_name(),
				);
			}
			
			r_struct.functions_map.insert(
				fn_data.0.clone(),
				QuFunctionId::new(self.registered_fns.len()),
			);
			self.registered_fns.push(fn_data)
		}
		
		self.registered_structs_map.insert(
			<S as QuRegisterStruct>::get_name(),
			QuStructId::new(self.registered_structs.len()),
		);
		self.registered_structs.push(r_struct);

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


	/// Returns a function reference by its name.
	pub fn get_fn(&self, fn_name:&str, struct_name:&str
	) -> Result<QuExtFn, QuMsg> {
		let fn_id = self.get_fn_id(fn_name, struct_name)?;
		return Ok(self.get_fn_by_id(fn_id)?);
	}


	/// Returns a function reference by its ID.
	pub fn get_fn_by_id(&self, fn_id:QuFunctionId
	) -> Result<QuExtFn, QuMsg> {
		return Ok(self.get_fn_by_index(fn_id.index)?);
	}


	/// Returns a function reference by its index.
	fn get_fn_by_index(&self, fn_index:usize
	) -> Result<QuExtFn, QuMsg> {
		let Some(fn_ref)
			= self.registered_fns.get(fn_index) else {
			return Err(QuMsg::general(
				&format!("qu vm has no function with index {fn_index}")))
		};
		let fn_ref = fn_ref.1;

		return Ok(fn_ref);
	}


	/// Returns a function ID by its name.
	pub fn get_fn_id(&self, fn_name:&str, struct_name:&str
	) -> Result<QuFunctionId, QuMsg> {
		let s = self.get_struct(struct_name)?;

		let Some(id) = s.functions_map.get(fn_name) else {
			return Err(QuMsg::general(
				&format!("struct with name {} has no function with name {fn_name}", s.name)))
		};
		return Ok(id.clone());
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


	pub fn get_struct(&self, struct_name:&str
	) -> Result<&QuStruct, QuMsg> {
		let id = self.get_struct_id(struct_name)?;
		self.get_struct_by_id(id)
	}


	pub fn get_struct_by_id(&self, id:QuStructId
	) -> Result<&QuStruct, QuMsg> {
		self.get_struct_by_index(id.index)
	}


	fn get_struct_by_index(&self, index:usize
	) -> Result<&QuStruct, QuMsg> {
		let Some(s) = self.registered_structs.get(index) else {
			return Err(QuMsg::general(
				&format!("qu has no struct by index {index}")
			))
		};
	
		return Ok(s);
	}


	pub fn get_struct_id(&self, struct_name:&str) -> Result<QuStructId, QuMsg> {
		let Some(st) = self.registered_structs_map.get(struct_name) else {
			return Err(QuMsg::general(
				&format!("qu has no struct by the name {struct_name}")
			));
		};
		Ok(st.clone())
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
		return self.is_zero;
		// TODO: Implement register boolean evaluation
		unimplemented!();
		//return self.hold != 0;
	}


	/// Reads an array from the next bytes in the code.
	fn next_array(&mut self, code:&[u8]) -> String {
		let str_size = self.next_u8(code);
		let mut str_vec = Vec::with_capacity(str_size as usize);
		for _ in 0..str_size {
			str_vec.push(self.next_u8(code));
		}

		return String::from_utf8(str_vec).expect("TODO: Handle this error");
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
	fn reg_get(&self, at_reg:QuRegId) -> &RegisterValue {
		return &self.registers[self.register_offset+at_reg.0];
	}


	/// Gets a register value.
	pub fn reg_get_as<'a, T:'a + 'static>(&self, at_reg:QuRegId
	) -> Result<&T, QuMsg> {
		let Some(r) = self.registers[self.register_offset+at_reg.0]
			.downcast_ref::<T>() else {
			return Err(QuMsg::general(
				&format!("reg_get_as could not convert register at index {}", at_reg.0)
			))
		};
		Ok(r)
	}


	pub fn reg_get_mut_as<'a, T:'a + 'static>(&mut self, at_reg:QuRegId
	) -> Result<&mut T, QuMsg> {
		let Some(r) = self.registers[self.register_offset+at_reg.0]
			.downcast_mut::<T>() else {
			return Err(QuMsg::general(
				&format!("reg_get_mut_as could not convert register at index {}", at_reg.0)
			))
		};
		Ok(r)
	}


	#[inline]
	/// Sets a register value.
	pub fn reg_set(&mut self, at_reg:QuRegId, to:RegisterValue) {
		return self.registers[self.register_offset+at_reg.0] = to;
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
			x if x == OPLIB.jump_to => self.exc_jump_to(bytecode),
			x if x == OPLIB.jump_by => self.exc_jump_by(bytecode),
			x if x == OPLIB.jump_to_if_not => self.exc_jump_to_if_not(bytecode),
			x if x == OPLIB.jump_by_if_not => self.exc_jump_by_if_not(bytecode),
			x if x == OPLIB.print => self.exc_print(bytecode),
			x if x == OPLIB.call => self.exc_call_fn(bytecode)?,
			x if x == OPLIB.define_fn => self.exc_define_fn(bytecode),

			x if x == OPLIB.call_ext => self.exc_call_fn_ext(bytecode)?,

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


pub trait QuAny: Any+Debug+QuRegisterStruct {}


#[cfg(test)]
mod test_vm {
    use std::any::Any;
    use std::any::TypeId;

    use crate::vm::QuAny;
    use crate::{Qu, QuVm, QuMsg, QuFnObject, QuType, QuCompiler, QuInt, vm::QuRegId};

    use super::OPLIB;

	#[test]
	fn external_fn_calls() -> Result<(), QuMsg> {
		let mut vm = QuVm::new();
		vm.external_struct_register::<QuInt>();

		let mut code = vec![];

		// Defines a constant zero
		code.append(&mut vec![OPLIB.load_val_u8, 0, 20]);
			
		// var nterms = 9
		let p_nterms = 0;
		code.append(&mut vec![OPLIB.load_val_u8, 9, p_nterms]);
		// var n1 = 0
		let p_n1 = 1;
		code.append(&mut vec![OPLIB.load_val_u8, 0, p_n1]);
		// var n2 = 1
		let p_n2 = 2;
		code.append(&mut vec![OPLIB.load_val_u8, 1, p_n2]);
		// var count = 0
		let p_count = 3;
		code.append(&mut vec![OPLIB.load_val_u8, 0, p_count]);

		// while count < nterms:
		let while_body_len = (8*4)+3+5;
		code.append(&mut vec![OPLIB.call_ext, p_count, 0,0,0,1, 4, p_nterms]);
		code.append(&mut vec![OPLIB.jump_by_if_not, 0,0,0,while_body_len]);

			// var nth = n1 + n2
			code.append(&mut vec![OPLIB.call_ext, p_n1, 0,0,0,0, 4, p_n2]);
			// n1 = n2
			code.append(&mut vec![OPLIB.call_ext, p_n2, 0,0,0,0, p_n1, 20]);
			// n2 = nth
			code.append(&mut vec![OPLIB.call_ext, 4, 0,0,0,0, p_n2, 20]);
			// count = count + 1
			code.append(&mut vec![OPLIB.load_val_u8, 1, 5]);
			code.append(&mut vec![OPLIB.call_ext, p_count, 0,0,0,0, p_count, 5]);

		code.append(&mut vec![OPLIB.jump_by]);
		code.append(&mut (-((8+5)+(while_body_len as i8)) as i32).to_be_bytes().to_vec());

		vm.run_bytes(&code)?;

		assert_eq!(*vm.reg_get_mut_as::<QuInt>(QuRegId(1))?, QuInt(34));
		assert_eq!(*vm.reg_get_mut_as::<QuInt>(QuRegId(2))?, QuInt(55));
		assert_eq!(*vm.reg_get_mut_as::<QuInt>(QuRegId(3))?, QuInt(9));

		return Ok(());
	}


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
	fn register_external_structs() -> Result<(), QuMsg> {
		let mut vm = QuVm::new();
		vm.external_struct_register::<QuInt>();

		let code = QuFnObject::new(
			vec![],
			crate::QuCodeObject {start_index:0},
			crate::QuType::Void,
		);

		vm.define_mem("bob".to_owned(), Box::new(code));
		
		vm.external_call::<QuInt>(
			"bob",
			"change_type",
			vec![],
		)?;

//		let bob_id = vm.get_mem_id("bob")?;
//		let bob_change_type_id
//			= vm.get_fn_id("change_type", "FnObject")?;
//
//		vm.external_call_by_id(
//			bob_id,
//			bob_change_type_id,
//			vec![],
//		)?;

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