
/*
MIT License

Copyright (c) 2022 GsLogiMaker

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

//! TODO: Project level documentation.

#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
#![warn(rustdoc::broken_intra_doc_links)]


#[macro_use]
extern crate lazy_static;


mod compiler;
mod errors;
mod parser;
mod tokens;
mod vm;


#[cfg(test)]
mod tests {

	use crate::Qu;

	#[test]
	fn var_define() {
		let mut vm = Qu::new();
		let result = vm.run("vl a_variable = 1");
		
		assert!(result.is_ok());
	}

}


use tokens::{RULES, TOKEN_TYPE_NAME, tokenize, QuToken};
pub use errors::QuMsg;
pub use compiler::QuCompiler;
pub use parser::{QuParser, QuLeaf, QuLeafExpr};
pub use vm::QuVm;


/// The interface for the Qu programming language.
pub struct Qu {
	vm:QuVm,

} impl Qu {
	//!
	//! # Examples
	//! ```
	//! use qu::Qu;
	//! let mut qu = Qu::new();
	//! qu.run("
	//! 	fn add():
	//! 		vl left = 2
	//! 		vl right = 5
	//! 		print left + right
	//! 
	//! 	add()
	//! ").unwrap();
	//! ```
	
	/// Instantiates the [Qu] struct.
	pub fn new() -> Self {
		Qu {
			vm:QuVm::new(),
		}
	}


	/// Compiles Qu script into bytecode.
	pub fn compile(&self, code:&str) -> Result<Vec<u8>, QuMsg> {
		//!
		//! Examples:
		//! 
		//! ```
		//! use qu::Qu;
		//! use qu::OPLIB;
		//! 
		//! let qu = Qu::new();
		//! let bytecode = qu.compile("vl count = 0").unwrap();
		//! 
		//! assert_eq!(bytecode, vec![OPLIB.load_val_u8, 0, 0, OPLIB.end]);
		//! ```
		// Tokenize
		let code_str = code.to_string();
		let tokens = &mut tokenize(&code_str, RULES);

		// Parse
		let mut parser = QuParser::new(tokens);
		let leaf_block = parser.parse()?;

		// Compile
		let mut c = QuCompiler::new();
		return Ok(c.compile(&leaf_block)?);
	}


	/// Compiles Qu code to Qu assembly.
	/// 
	/// Example:
	/// 
	/// ```
	/// use qu::Qu;
	/// use qu::OPLIB;
	/// 
	/// let mut qu = Qu::new();
	/// let asm = qu.compile_to_asm("vl added = 5 + 6").unwrap();
	/// 
	/// assert_eq!(asm, "\nLDU8 6 1\nLDU8 5 0\nADD 0 1 0\nEND".to_string());
	/// ```
	/// 
	/// Errors:
	/// 
	/// Returns a [String] if compilation is successful, otherwise returns a
	/// [QuMsg] if invalid syntax was given.
	pub fn compile_to_asm(&mut self, code:&str) -> Result<String, QuMsg> {
		let code = self.compile(code)?;

		return Ok(QuVm::code_to_asm(&code, false));
	}


	/// Runs a [String] of Qu code.
	/// 
	/// Example:
	/// 
	/// ```
	/// use qu::Qu;
	/// 
	/// let mut qu = Qu::new();
	/// qu.run("vl count = 5 + 6").unwrap();
	/// ```
	/// 
	/// Errors:
	/// 
	/// Returns an empty [Ok] when ran successfully, otherwise returns a [QuMsg]
	/// if improper syntax was given, or if there was a runtime error raised.
	pub fn run(&mut self, script:&str) -> Result<(), QuMsg> {
		let code_res = self.compile(script);
		match code_res{
			Ok(code) => {
				let run_res = self.run_bytes(&code);
				if let Err(msg) = &run_res {
					msg.print(&script);
				}
				return run_res;
			}
			Err(msg) => {
				msg.print(&script);
				return Err(msg);
			}
		}
	}


	/// Runs Qu bytecode.
	/// 
	/// Example:
	/// 
	/// ```
	/// use qu::Qu;
	/// 
	/// let mut qu = Qu::new();
	/// let bytecode = qu.compile("vl count = 5 + 6").unwrap();
	/// qu.run_bytes(&bytecode).unwrap();
	/// ```
	/// 
	/// Errors:
	/// 
	/// Returns an empty [Ok] when ran successfully, otherwise returns a [QuMsg]
	/// if improper syntax was given, or if there was a runtime error raised.
	pub fn run_bytes(&mut self, bytes:&[u8]) -> Result<(), QuMsg> {
		return self.vm.run_bytes(bytes);
	}

}


/// A declared Qu function. Contains all the metadata for a defined function.
pub struct QuFunc {
	/// The name of this funciton.
	name:String,
	/// The index that is function is stored at in the function list.
	id:usize,
	/// The arguments accepted by this function.
	arg_list:Vec<u8>,
	/// The start index of this function's code.
	code_start:usize,

} impl QuFunc {

	fn new(name:&str, id:usize, code_start:usize) -> Self {
		return Self{
			name: name.to_owned(),
			id: id,
			arg_list: Vec::default(),
			code_start: code_start,
		}
	}

}


/// An object type (Ex: int, bool, String, Object).
pub struct QuType {
	name:String,
	size:usize,
} impl QuType {

	/// Makes a new [`QuType`].
	fn new(name:String, size:usize) -> QuType {
		return QuType{
			name,
			size,
		};
	}


	/// Instantiates a boolean [`QuType`].
	fn bool() -> QuType {
		// TODO:: Make "bool" string a constant
		return QuType::new("bool".to_string(), 1);
	}


	/// Instantiates an integer [`QuType`].
	fn int() -> QuType {
		// TODO: Make "int" string a constant
		return QuType::new("int".to_string(), 1);
	}


	/// Instantiates an unsigned integer [`QuType`].
	fn uint() -> QuType {
		// TODO:: Make "uint" string a constant
		return QuType::new("uint".to_string(), 1);
	}

}


/// Metadata for a Qu variable.
struct QuVar {
	/// The name of this variable.
	name:String,
	/// The static type of this variable.
	static_type:Option<usize>,
	/// The register to which this variable is saved to.
	register:u8,
} impl QuVar {
	
	/// Instantiate a [QuVar] struct.
	fn new(name:&str, static_type:Option<usize>, register:u8) -> Self {
		return Self{
			name: name.to_owned(),
			static_type: static_type,
			register: register,
		};
	}


	/// Instantiate a [QuVar] struct with default values.
	fn default() -> Self {
		return Self {
			name: String::default(),
			static_type: None,
			register: u8::default(),
		};
	}

}

