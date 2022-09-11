
use crate::QuFunc;
use crate::QuMsg;


/// Defines a [QuOpLibrary] struct
/// 
/// Example:
/// ```
/// struct_QuOpLibrary!{
/// 	[0] end:END()
/// 	[1] add:ADD((1,), (1,), (1,))
/// }
/// let oplib = QuOpLibrary::new();
/// assert_eq(oplib.end, 0);
/// assert_eq(oplib.add, 1);
/// ```
/// 
/// Expands into:
/// ```
/// pub struct QuOpLibrary<'a> {
/// 	ops: Vec<QuOperation<'a>>,
/// 	end: u8,
/// 	add: u8,
/// }
/// impl<'a> QuOpLibrary<'a> {
/// 	fn new() -> Self {
/// 		return Self {
/// 			ops: vec![
/// 				QuOperation::new("end", "END", &[(1,)]),
/// 				QuOperation::new("add", "ADD", &[(1,), (1,), (1,)]),
/// 			],
/// 			end: 0,
/// 			add: 1,
/// 		};
/// 	}
/// }
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

	[ 25] define_fn:DFFN(QuAsmTypes::Uint32, QuAsmTypes::Uint32)
	[ 26] define_const_str:DFCS(QuAsmTypes::Str)
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
/// [Qu] for interfacing with Qu script.
pub struct QuVm {
	/// A [Vec] of all defined [QuFunc]s.
	functions:Vec<QuFunc>,
	/// All defined constant [String]s.
	str_constants:Vec<String>,
	/// Holds the outputed value of the last executed operation.
	hold:usize,

	/// The memory registers. Holds all primatives of the VM.
	registers:Vec<RegisterValue>,
	/// The offset of reading and writing to registers.
	register_offset:usize,
	/// The program counter.
	pc:usize,

} impl QuVm {

	/// Instantiate a new [QuVm] struct.
	pub fn new() -> Self {
		let mut vm = QuVm { 
			functions: Vec::default(),
			str_constants: Vec::default(),
			hold: usize::default(),
			registers:Vec::with_capacity(u8::MAX as usize),
			register_offset: 0,
			pc: 0,
		};

		vm.registers.resize(u8::MAX as usize, 0);

		return vm;
	}


	/// Converts byte code to human readable Qu assembly instructions.
	/// Example:
	/// 
	/// ```
	/// use qu::QuVm;
	/// use qu::OPLIB;
	/// 
	/// // Bytecode representing an add operation which adds 5 and 6, then
	/// // stores the result in register 0.
	/// let asm = QuVm::code_to_asm(&[8, 5, 6, 0], false);
	/// 
	/// assert_eq!(asm, "\nADD 5 6 0".to_string());
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


	/// Defines a constant [String] in the VM.
	///
	/// Example:
	/// 
	/// ```
	/// use qu::QuVm;
	/// 
	/// let mut vm = QuVm::new();
	/// vm.define_const_string("Hello world!");
	/// vm.define_const_string("The second!");
	/// 
	/// assert_eq!(vm.get_const_string(0), "Hello world!");
	/// assert_eq!(vm.get_const_string(1), "The second!");
	/// ```
	#[inline]
	pub fn define_const_string(&mut self, value:&str) {
		self.str_constants.push(value.to_owned());
	}


	/// Defines a Qu function in the VM.
	fn define_fn(&mut self, name:&str, code_start:usize) {
		let id = self.functions.len();
		self.functions.push(QuFunc::new(&name, id, code_start));
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

		self.register_offset += frame_offset;
		// Assure registers is big enought to fit u8::MAX more values
		if (self.register_offset + u8::MAX as usize) > self.registers.len() {
			self.registers.resize(
				self.register_offset + u8::MAX as usize, 0);
		}
		let return_pc = self.pc;

		self.frame_start(self.functions[fn_id].code_start);
		self.do_loop(code)?;

		self.register_offset -= frame_offset;
		self.pc = return_pc;

		return Ok(());
	}


	fn exc_define_const_str(&mut self, code:&[u8]) {
		let string = self.next_ascii(code);
		self.define_const_string(&string);
	}


	/// Defines a function from next bytes in the code.
	fn exc_define_fn(&mut self, code:&[u8]) {
		let name_const_idx = self.next_u32(code) as usize;
		let fn_length = self.next_u32(code) as usize;

		let name = self.get_const_string(name_const_idx).to_owned();
		let code_start = self.pc;

		self.define_fn(&name, code_start);
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


	#[inline]
	/// Returns a previously defined constant &[str] by its index.
	pub fn get_const_string(&self, at:usize) -> &str {
		//!
		//! Example:
		//! 
		//! ```
		//! use qu::QuVm;
		//! 
		//! let mut vm = QuVm::new();
		//! vm.define_const_string("Hello world!");
		//! vm.define_const_string("The second!");
		//! 
		//! assert_eq!(vm.get_const_string(0), "Hello world!");
		//! assert_eq!(vm.get_const_string(1), "The second!");
		//! ```
		return self.str_constants[at].as_str();
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


	/// Gets the next byte in the source code as a u8 int.
	fn next_u8(&mut self, code:&[u8]) -> u8 {
		let val = code[self.pc];
		self.pc += 1;
		return val;
	}


	/// Gets the next 2 byte in the source code as a u16 int.
	fn next_u16(&mut self, code:&[u8]) -> u16 {
		let bytes = [code[self.pc], code[self.pc+1]];
		self.pc += 2;
		return u16::from_be_bytes(bytes);
	}


	/// Gets the next 4 byte in the source code as a u32 int.
	fn next_u32(&mut self, code:&[u8]) -> u32 {
		let bytes = [
			code[self.pc], code[self.pc+1],
			code[self.pc+2], code[self.pc+3],
		];
		self.pc += 4;
		return u32::from_be_bytes(bytes);
	}


	// Gets the next 8 byte in the source code as a u64 int.
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
				if e.description == "Done" {return Ok(())};
			}
		}
		return Ok(());
	}


	/// Runs the next command in the given bytecode.
	fn do_next(&mut self, bytecode:&[u8]) -> Result<(), QuMsg> {
		// TODO: Error handling for running operations
		let op = self.next_u8(bytecode);
//		println!("Doing op: {}", OPLIB.ops[op as usize].name);
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
			x if x == OPLIB.define_const_str => self.exc_define_const_str(bytecode),

			x => { println!("{x}"); todo!(); }
		};
		return Ok(());
	}


	/// Run the passed bytecode.
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