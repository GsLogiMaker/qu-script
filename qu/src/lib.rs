
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


extern crate lazy_static;


mod compiler;
mod errors;
mod import;
mod objects;
mod parser;
mod tokens;
mod vm;


pub use import::QuExtFnId;
use import::QuImports;
use tokens::{TOKEN_TYPE_NAME, QuToken};
pub use errors::QuMsg;
pub use compiler::QuCompiler;
pub use objects::*;
pub use parser::{QuParser, QuLeaf, QuLeafExpr};
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
pub struct Qu {
	vm:QuVm,
	imports:QuImports,

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
		let mut qu = Qu {
			vm: QuVm::new(),
			imports: QuImports::default(),
		};
		qu.import_struct::<isize>();
		qu.import_fns();
		qu
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
		return Ok(c.compile(code, &self.imports)?);
	}


	/// Imports the external functions of all imported structs.
	/// 
	/// # Warning
	/// 
	/// Likely to panic.
	pub fn import_fns(&mut self) {
		self.imports.register_fns()
	}


	/// Imports an external struct.
	/// 
	/// # Note
	/// 
	/// Does not register the methods of the struct. Call
	/// [`Qu::register_struct`] to register the methods of all registered
	/// structs.
	/// 
	/// # Example
	/// 
	/// ```
	/// use qu::Qu;
	/// use qu::QuRegisterStruct;
	/// 
	/// struct MyStruct();
	/// impl QuRegisterStruct for MyStruct {
	/// 	fn get_name() -> String {
	/// 		"MyStruct".into()
	/// 	}
	/// }
	/// 
	/// let mut qu = Qu::new();
	/// 
	/// qu.import_struct::<MyStruct>();
	/// ```
	pub fn import_struct<S:QuRegisterStruct+'static>(&mut self) {
		self.imports.register_struct::<S>()
	}


	/// Runs [`&str`] as Qu script.
	/// 
	/// # Errors
	/// 
	/// If `code` contains improper Qu syntax or a Qu runtime error occurs
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
	/// 
	/// qu.run("var count = 5 + 6")?;
	/// # return Ok(());
	/// # }
	/// ```
	pub fn run(&mut self, script:&str) -> Result<Option<StackValue>, QuMsg> {
		let code = self.compile(script)?;
		self.run_ops(&code)?;
		Ok(self.vm.get_return_value())
	}


	/// Runs a Qu script and returns the value returned by the script.
	/// 
	/// # Errors
	/// 
	/// If `code` contains improper Qu syntax, a Qu runtime error occurs,
	/// there is no return value, or the return value could not be cast to `T`
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
	/// 
	/// let val = qu.run_and_get::<isize>("return 5 + 6")?;
	/// assert_eq!(val, Box::new(11isize));
	/// # return Ok(());
	/// # }
	/// ```
	pub fn run_and_get<T:'static>(&mut self, script:&str
	) -> Result<Box<T>, QuMsg> {
		let code = self.compile(script)?;
		self.run_ops(&code)?;

		let Some(r) = self.vm.get_return_value() else {
			return Err("Nothing was returned".into())
		};
		let Ok(r) = r.downcast::<T>() else {
			return Err("Could not convert return type to `T`".into())
		};

		Ok(r)
	}


	/// Runs compiled Qu code.
	/// 
	/// # Errors
	/// 
	/// If `code` fails the sanity checks or a Qu runtime error occurs
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
	/// 
	/// let code = qu.compile("var count = 5 + 6")?;
	/// qu.run_ops(&code)?;
	/// # return Ok(());
	/// # }
	/// ```
	pub fn run_ops(&mut self, code:&[QuOp]) -> Result<(), QuMsg> {
		return self.vm.run_ops(code, &self.imports);
	}

}


#[cfg(test)]
mod lib {
    use crate::Qu;

	#[test]
	fn fibinachi() {
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
			
			return n1
		"#;

		let n1 = *qu.run_and_get::<isize>(script).unwrap();

		assert_eq!(n1, 34isize);
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
		let value:isize = *qu.run_and_get(script).unwrap();
		assert_eq!(value, 128isize);
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

		let val = *qu.run_and_get::<isize>(script).unwrap();
		assert_eq!(val, 3isize);
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

		let res:isize = *qu.run_and_get(script).unwrap();
		assert_eq!(res, 0isize);
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

		let res:isize = *qu.run_and_get(script).unwrap();
		assert_eq!(res, 10isize);
	}

}