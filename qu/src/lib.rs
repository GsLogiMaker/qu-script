
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

//! The API to embed Qu into a project. TODO: Better documentation.

#![warn(missing_docs)]
#![warn(rustdoc::broken_intra_doc_links)]


#[macro_use]
extern crate lazy_static;


mod compiler;
mod errors;
pub mod objects;
mod parser;
mod tokens;
mod vm;


use tokens::{TOKEN_TYPE_NAME, QuToken};
pub use errors::QuMsg;
pub use compiler::QuCompiler;
pub use objects::*;
pub use parser::{QuParser, QuLeaf, QuLeafExpr};
pub use vm::QuVm;
pub use vm::QuRegisterValue;
pub use vm::OPLIB;
pub use vm::QuRegId;


/// The interface for the Qu programming language.
/// 
/// # Examples
/// ```
/// use qu::Qu;
/// use qu::QuMsg;
/// 
/// # fn main() {run_test().unwrap();}
/// # fn run_test() -> Result<(), QuMsg>{
/// let mut qu = Qu::new();
/// qu.run(r#"
/// 	fn add():
/// 		var left = 2
/// 		var right = 5
/// 		print left + right
/// 
/// 	add()
/// "#)?;
/// # return Ok(());
/// # }
/// ```
pub struct Qu {
	vm:QuVm,

} impl Qu {

	/// Instantiates a [`Qu`] struct.
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::Qu;
	/// 
	/// let qu = Qu::new();
	/// ```
	pub fn new() -> Self {
		Qu {
			vm:QuVm::new(),
		}
	}


	/// Compiles Qu script into a [`Vec<u8>`].
	///
	/// # Errors
	/// 
	/// If `code` contains improper Qu syntax then an [`Err`] is returned.
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::Qu;
	/// use qu::QuMsg;
	/// 
	/// # fn main(){example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// let qu = Qu::new();
	/// let bytecode = qu.compile("var count = 0")?;
	/// # return Ok(());
	/// # }
	/// ```
	pub fn compile(&self, code:&str) -> Result<Vec<u8>, QuMsg> {
		// Compile
		let mut c = QuCompiler::new();
		return Ok(c.compile(code)?);
	}


	/// Compiles Qu code to a [`String`] representing Qu assembly.
	/// 
	/// # Errors
	/// 
	/// If `code` contains improper Qu syntax then an [`Err`] is returned.
	/// 
	/// # Example
	/// 
	/// ```
	/// use qu::Qu;
	/// use qu::QuMsg;
	/// 
	/// # fn main() {example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// let mut qu = Qu::new();
	/// 
	/// let asm = qu.compile_to_asm("var added = 5 + 6", false)?;
	/// assert_eq!(
	/// 	asm,
	/// 	format!("{}{}{}{}",
	/// 		"\nLDU8 5 0",
	/// 		"\nLDU8 6 1",
	/// 		"\nADD 0 1 0",
	/// 		"\nEND",
	///	 	)
	/// );
	/// # return Ok(());
	/// # }
	/// ```
	pub fn compile_to_asm(&self, code:&str, include_line_columns:bool
	) -> Result<String, QuMsg> {
		let code = self.compile(code)?;

		return Ok(QuVm::code_to_asm(&code, include_line_columns));
	}


	/// Runs [`&str`] as Qu script.
	/// 
	/// # Errors
	/// 
	/// If `code` contains improper Qu syntax or if a Qu runtime error occurs
	/// then an [`Err`] is returned.
	/// 
	/// # Example
	/// 
	/// ```
	/// use qu::Qu;
	/// use qu::QuMsg;
	/// 
	/// # fn main(){example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// let mut qu = Qu::new();
	/// qu.run("var count = 5 + 6")?;
	/// # return Ok(());
	/// # }
	/// ```
	pub fn run(&mut self, script:&str) -> Result<(), QuMsg> {
		let code_res = self.compile(script);
		match code_res{
			Ok(code) => {
				let run_res = self.run_bytes(&code);
				return run_res;
			}
			Err(msg) => {
				return Err(msg);
			}
		}
	}


	/// Runs `&[u8]` as Qu bytecode.
	/// 
	/// # Errors
	/// 
	/// If `bytes` does not pass the sanity checks or a Qu runtime error
	/// occurs then an [`Err`] is returned.
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::Qu;
	/// use qu::QuMsg;
	/// 
	/// # fn main(){example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// let mut qu = Qu::new();
	/// 
	/// let bytecode = qu.compile("var count = 5 + 6")?;
	/// qu.run_bytes(&bytecode)?;
	/// # return Ok(());
	/// # }
	/// ```
	pub fn run_bytes(&mut self, bytes:&[u8]) -> Result<(), QuMsg> {
		return self.vm.run_bytes(bytes);
	}

}


/// A declared Qu function. Contains all the metadata for a defined function.
#[derive(Debug, Default)]
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


/// TODO: Remove this struct, it is replaced with an enum.
/// An object type (Ex: int, bool, String, Object).
pub struct QuType2 {
	name:String,
	size:usize,
} impl QuType2 {

	/// Makes a new [`QuType`].
	fn new(name:String, size:usize) -> QuType2 {
		return QuType2{
			name,
			size,
		};
	}


	/// Instantiates a boolean [`QuType`].
	fn bool() -> QuType2 {
		// TODO:: Make "bool" string a constant
		return QuType2::new("bool".to_string(), 1);
	}


	/// Instantiates an integer [`QuType`].
	fn int() -> QuType2 {
		// TODO: Make "int" string a constant
		return QuType2::new("int".to_string(), 1);
	}


	/// Instantiates an unsigned integer [`QuType`].
	fn uint() -> QuType2 {
		// TODO:: Make "uint" string a constant
		return QuType2::new("uint".to_string(), 1);
	}

}


/// Metadata for a Qu variable.
#[derive(Debug, Default, Clone)]
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

#[cfg(test)]
mod lib {
    use crate::{Qu, QuMsg};

	#[test]
	fn fibinachi() -> Result<(), QuMsg>{
		let mut qu = Qu::new();
		let script = r#"
			var nterms = 9
			var n1 = 0
			var n2 = 1
			var count = 0
		
			while count < nterms:
				var nth = n1 + n2
				n1 = n2
				n2 = nth
				count = count + 1
				print count
				print n1
		"#;

		qu.run(script)?;

		return Ok(());
	}


	#[test]
	fn recursive_fn_addinate() -> Result<(), QuMsg>{
		let mut qu = Qu::new();
		let script = r#"
			fn addinate(val):
				if val < 100:
					return addinate(val + val)
				return val

			print addinate(1)
		"#;
		// TODO: Actually check that the results are correct.

		println!("{}", qu.compile_to_asm(script, true)?);
		qu.run(script)?;

		return Ok(());
	}


	#[test]
	fn run_qu_example() -> Result<(), QuMsg>{
		let mut qu = Qu::new();
		qu.run(r#"
			fn add():
				var left = 2
				var right = 5
				print left + right
		
			add()
		"#)?;
		return Ok(());
	}


	#[test]
	fn run_qu_example_var_scoping1() -> Result<(), QuMsg>{
		let mut qu = Qu::new();
		qu.run(r#"
			fn add(a, b):
				print a + b
			print add(1, 2)
		"#)?;
		return Ok(());
	}


	#[test]
	#[should_panic]
	fn run_qu_example_var_scoping1_panic() {
		let mut qu = Qu::new();
		qu.run(r#"
			fn add():
				var left = 2
				var right = 5
				print left + right
			print left
			add()
		"#).unwrap();
	}


	#[test]
	fn run_qu_example_var_scoping2() {
		let mut qu = Qu::new();
		qu.run(r#"
			var counter = 1
			if counter:
				var value = 25
				print value
			print counter
		"#).unwrap();
	}


	#[test]
	#[should_panic]
	fn run_qu_example_var_scoping2_panic() {
		let mut qu = Qu::new();
		qu.run(r#"
			var counter = 1
			if counter:
				var value = 25
				print value
			print counter
			print value
		"#).unwrap();
	}

}