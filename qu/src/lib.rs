
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
mod import;
mod objects;
mod parser;
mod tokens;
mod vm;


use std::marker::PhantomData;

pub use import::QuFunctionId;
use import::QuRegistered;
use tokens::{TOKEN_TYPE_NAME, QuToken};
pub use errors::QuMsg;
pub use compiler::QuCompiler;
pub use objects::*;
pub use parser::QuParser;
pub use vm::QuOp;
pub use vm::QuVm;
pub use vm::QuStackId;
use vm::StackValue;


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
/// 		return left + right
/// 
/// 	add()
/// "#)?;
/// # return Ok(());
/// # }
/// ```
pub struct Qu<'a> {
	vm: QuVm,
	ph: PhantomData<&'a u8>,

} impl<'a> Qu<'a> {

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
			vm: QuVm::new(),
			ph: PhantomData {},
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
	pub fn compile(&self, code:&str) -> Result<Vec<QuOp>, QuMsg> {
		// Compile
		let mut c = QuCompiler::new();
		return Ok(c.compile(code, &self.vm.imports)?);
	}


	pub fn register_fns(&mut self) {
		self.vm.imports.register_fns()
	}


	fn register_fn(&mut self, fn_data:ExternalFunction) -> Result<(), QuMsg> {
		self.vm.imports.register_fn(fn_data)
	}


	pub fn register_struct<S:QuRegisterStruct+'static>(&mut self) {
		self.vm.imports.register_struct::<S>()
	}


	pub fn reg_get<'b, T:'a>(
		&self, at_reg:QuStackId
	) -> Result<&T, QuMsg> {
		self.vm.read::<T>(at_reg)
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
		let code = self.compile(script)?;
		self.run_ops(&code)
	}


	pub fn run_and_get<T:'a>(&mut self, script:&str) -> Result<&T, QuMsg> {
		let code = self.compile(script)?;
		self.run_ops(&code)?;
		let return_id = self.vm.return_value_id();
		self.reg_get(
			QuStackId::new(0, return_id)
		)
	}


	pub fn run_ops(&mut self, instructions:&[QuOp]) -> Result<(), QuMsg> {
		return self.vm.run_ops(instructions);
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
    use crate::Qu;

	#[test]
	fn fibinachi() {
		let mut qu = Qu::new();
		let script = r#"
			var nterms int = 9
			var n1 int = 0
			var n2 int = 1
			var count int = 0
		
			while count < nterms:
				var nth int = n1 + n2
				n1 = n2
				n2 = nth
				count = count + 1
			
			return n1
		"#;

		let n1 = *qu.run_and_get::<i32>(script).unwrap();
		assert_eq!(n1, 34);
	}


	#[test]
	fn non_returning_fn() {
		let mut qu = Qu::new();
		let script = r#"
			fn count(to):
				var i = 0
				while i < to:
					i = i + 1
			
			count(10)
		"#;

		qu.run(script).unwrap();
	}


	#[test]
	fn recursive_fn_addinate() {
		let mut qu = Qu::new();
		let script = r#"
			fn addinate(val):
				if val < 100:
					return addinate(val + val)
				return val

			return addinate(1)
		"#;

		dbg!(qu.compile(script).unwrap());
		let value:i32 = *qu.run_and_get(script).unwrap();
		assert_eq!(value, 128);
	}


	#[test]
	fn scoping1(){
		let script = r#"
			var l = 1
			var r = 2
			fn add(a, b):
				return a + b
			return add(l, r)
		"#;
		let mut qu = Qu::new();

		let val = *qu.run_and_get::<i32>(script).unwrap();
		assert_eq!(val, 3);
	}


	#[test]
	#[should_panic]
	fn scoping1_panic() {
		let mut qu = Qu::new();
		qu.run(r#"
			var l = 1
			var r = 2
			fn add(a, b):
				return a + b
			return a
		"#).unwrap();
	}


	#[test]
	fn scoping2() {
		let mut qu = Qu::new();
		qu.run(r#"
			var counter = 1
			if counter:
				var value = 25
			return counter
		"#).unwrap();
	}


	#[test]
	#[should_panic]
	fn scoping2_panic() {
		let mut qu = Qu::new();
		qu.run(r#"
			var counter = 1
			if counter:
				var value = 25
			return value
		"#).unwrap();
	}


	#[test]
	fn while_count_down() {
		let mut qu = Qu::new();
		let script = r#"
			var counter = 10
			while 0 < counter:
				counter = counter - 1
			return counter
		"#;

		let res:i32 = *qu.run_and_get(script).unwrap();
		assert_eq!(res, 0);
	}


	#[test]
	fn while_count_up() {
		let mut qu = Qu::new();
		let script = r#"
			var counter = 0
			while 10 > counter:
				counter = counter + 1
			return counter
		"#;

		let res:i32 = *qu.run_and_get(script).unwrap();
		assert_eq!(res, 10);
	}


	#[test]
	fn static_typing_return_from_run() {
		let mut qu = Qu::new();
		let script = r#"
			var counter int = 100
			return counter
		"#;

		let res:i32 = *qu.run_and_get(script).unwrap();
		assert_eq!(res, 100);
	}


	#[test]
	#[should_panic]
	fn static_typing_return_from_run_panic() {
		let mut qu = Qu::new();
		let script = r#"
			var counter void
			return counter
		"#;

		let res:i32 = *qu.run_and_get(script).unwrap();
	}


	#[test]
	#[should_panic]
	fn static_typing_return_from_run_panic_2() {
		let mut qu = Qu::new();
		let script = r#"
			var counter void = 1
			return counter
		"#;

		let res:i32 = *qu.run_and_get(script).unwrap();
	}

}