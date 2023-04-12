
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

use std::marker::PhantomData;

use tokens::TOKEN_TYPE_NAME;
use tokens::QuToken;
pub use errors::QuMsg;
pub use compiler::QuCompiler;
pub use objects::*;
pub use parser::QuParser;
pub use vm::QuOp;
pub use vm::QuVm;
pub use vm::QuStackId;


/// The interface for the Qu programming language.
/// 
/// # Examples
/// ```
/// use qu::Qu;
/// 
/// # fn main() {run_test().unwrap();}
/// # fn run_test() -> Result<(), qu::QuMsg>{
/// let mut qu = Qu::new();
/// qu.run(r#"
/// 	fn adder() int:
/// 		var left int = 2
/// 		var right int = 5
/// 		return left + right
/// 
/// 	adder()
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
	/// 
	/// # fn main(){example().unwrap()}
	/// # fn example() -> Result<(), qu::QuMsg> {
	/// let mut qu = Qu::new();
	/// let bytecode = qu.compile("var count int = 0")?;
	/// # return Ok(());
	/// # }
	/// ```
	pub fn compile(&mut self, code:&str) -> Result<Vec<QuOp>, QuMsg> {
		// Compile
		let mut c = QuCompiler::new();
		let code = c.compile(code, &mut self.vm.definitions)?;
		return Ok(code);
	}


	pub fn register_fns(&mut self) -> Result<(), QuMsg>{
		self.vm.definitions.register_functions()
	}


	fn register_fn(&mut self, fn_data:ExternalFunction) -> Result<(), QuMsg> {
		self.vm.definitions.register_function(fn_data)
	}


	pub fn register_struct<S:QuRegisterStruct+'static>(&mut self) {
		unimplemented!()
	}


	pub fn read<'b, T:'a>(
		&self, at_reg:QuStackId
	) -> Result<&T, QuMsg> {
		self.vm.read::<T>(at_reg)
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
	pub fn run(&mut self, script:&str) -> Result<(), QuMsg> {
		let code = self.compile(script)?;
		self.run_ops(&code)
	}


	pub fn run_and_get<T:'a>(&mut self, script:&str) -> Result<&T, QuMsg> {
		self.run(script)?;
		let return_id = self.vm.return_value_id();
		self.read(
			QuStackId::new(0, return_id)
		)
	}


	fn run_ops(&mut self, instructions:&[QuOp]) -> Result<(), QuMsg> {
		return self.vm.run_ops(instructions);
	}

}


#[cfg(test)]
mod lib {
    use crate::{Qu, Module};

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
			fn count(to int) void:
				var i int = 0
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
			fn addinate(val int) int:
				if val < 100:
					return addinate(val + val)
				return val

			return addinate(1)
		"#;

		let value:i32 = *qu.run_and_get(script).unwrap();
		assert_eq!(value, 128);
	}


	#[test]
	fn scoping1(){
		let script = r#"
			var l int = 1
			var r int = 2

			fn adder(a int, b int) int:
				return a + b

			var last int = 3
			return adder(1,2) + last
		"#;
		let mut qu = Qu::new();

		let val = *qu.run_and_get::<i32>(script).unwrap();
		assert_eq!(val, 1+2+3);
	}


	#[test]
	#[should_panic]
	fn scoping1_panic() {
		let mut qu = Qu::new();
		qu.run(r#"
			var l int = 1
			var r int = 2
			fn add(a int, b int):
				return a + b
			return a
		"#).unwrap();
	}


	#[test]
	fn scoping2() {
		let mut qu = Qu::new();
		let result:i32 = *qu.run_and_get(r#"
			var counter int = 1
			if counter == 1:
				var value int = 25
				counter = counter + value

			var next int = 2
			counter = counter + next
			
			return counter
		"#).unwrap();
		assert_eq!(result, 1+25+2);
	}


	#[test]
	#[should_panic]
	fn scoping2_panic() {
		let mut qu = Qu::new();
		qu.run(r#"
			var counter int = 1
			if counter == 1:
				var value int = 25
			return value
		"#).unwrap();
	}


	#[test]
	fn while_count_down() {
		let mut qu = Qu::new();
		let script = r#"
			var counter int = 10
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
			var counter int = 0
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
		"#;

		qu.run(script).unwrap();
	}


	#[test]
	#[should_panic]
	fn assign_int_to_bool_panic() {
		let mut qu = Qu::new();
		let script = r#"
			var counter bool = 1
		"#;

		qu.run(script).unwrap();
	}


	#[test]
	fn function_identity_same_names() {
		let mut qu = Qu::new();
		let script = r#"
			fn grow(item int) int:
				return item * 2
			
			fn grow(item int, times int) int:
				return item * times
			
			return grow(2) + grow(3, 3)
		"#;

		let res:i32 = *qu.run_and_get(script).unwrap();
		assert_eq!(res, (2*2) + (3*3));
	}


	#[test]
	#[should_panic]
	fn function_identity_same_names_diff_returns_panic() {
		let mut qu = Qu::new();
		let script = r#"
			fn weird(item int) int:
				return item + 5
			
			fn weird(item int) bool:
				return item > 5
		"#;
		qu.run(script).unwrap();
	}


	#[test]
	fn function_similar_identity_to_add() {
		let mut qu = Qu::new();
		let script = r#"
			fn add(a int, b int, c int) int:
				return a + b + c
			
			return add(1, 2) + add(3, 4, 5)
		"#;
		let outcome:i32 = *qu.run_and_get(script).unwrap();
		assert_eq!(outcome, (1+2) + (3+4+5));
	}


	#[test]
	fn repeated_register_functions() {
		let mut qu = Qu::new();
		qu.register_fns().unwrap();
		qu.register_fns().unwrap();
	}


	#[test]
	fn repeated_runs() {
		let mut qu = Qu::new();
		qu.run("var a int = 10").unwrap();
		qu.run("var b int = 13").unwrap();
	}


	// TODO: implement accessing classes like variables
	//#[test]
	fn class_dot_notation() {
		let mut qu = Qu::new();
		let result:i32 = *qu.run_and_get("return int.sub(5, 8)").unwrap();
		assert_eq!(result, 5-8);
	}


	#[test]
	#[should_panic]
	fn class_dot_notation_panic() {
		let mut qu = Qu::new();
		qu.run("return int.foo").unwrap();
	}


	#[test]
	fn variable_dot_notation() {
		let mut qu = Qu::new();
		let result:i32 = *qu.run_and_get("
			var num int = 10
			return num.mul(2)
		").unwrap();
		assert_eq!(result, 10*2);
	}


	#[test]
	fn variable_dot_notation_2() {
		let mut qu = Qu::new();
		let result:bool = *qu.run_and_get("
			var count int = 5
			return count.add(1) == add(count, 1)
		").unwrap();
		
		assert!(result);
	}


	#[test]
	#[should_panic]
	fn variable_dot_notation_panic() {
		let mut qu = Qu::new();
		qu.run("
			var num int = 20
			num.foo
		").unwrap();
	}


	#[test]
	fn variable_dot_notation_custom_function() {
		let mut qu = Qu::new();
		let result:i32 = *qu.run_and_get("
			fn move(number int) int:
				return number + 1
			
			var location int = 5
			return location.move()
		").unwrap();
		assert_eq!(result, 5+1);
	}


	#[test]
	fn variable_dot_notation_custom_function_3() {
		let mut qu = Qu::new();
		let result:i32 = *qu.run_and_get("
			fn move(number int) int:
				return number + 1
			return 5.move()
		").unwrap();
		assert_eq!(result, 5+1);
	}


	// TODO: Allow accessing global items from between invocations of Qu::run
	//#[test]
	fn cross_run_accessing() {
		// TODO: Allow this:
		let mut qu = Qu::new(); // register_fns is called once in new().
		qu.run("
			fn thrice(num int) int:
				return num * 3
		").unwrap();
		let num = *qu.run_and_get::<i32>("
			return thrice(2)
		").unwrap();
		assert_eq!(num, 2*3);
	}


	// TODO: Prevent functions definitions from having multiple parameters of
	// 	the same name 
	//#[test]
	fn function_multiple_parameters_same_name() {
		let mut qu = Qu::new();
		let script = r#"
			fn some(a int, a int, c int) int:
				return a + c
		"#;
		qu.run(script).unwrap();
	}


	#[test]
	fn import() {
		let mut qu = Qu::new();
		let result:i32 = *qu.run_and_get("
			import math
			return math.foo(3)
		").unwrap();
		assert_eq!(result, 3);
	}


	#[test]
	fn import_2() {
		let mut qu = Qu::new();
		let result:i32 = *qu.run_and_get("
			import math.foo
			return foo(3)
		").unwrap();
		assert_eq!(result, 3);
	}


	#[test]
	fn import_3() {
		let mut qu = Qu::new();
		let result:i32 = *qu.run_and_get("
			import math
			import math.foo
			return math.foo(3) + foo(3)
		").unwrap();
		assert_eq!(result, 3+3);
	}


	#[test]
	#[should_panic]
	fn not_imported_panic() {
		let mut qu = Qu::new();
		let result:i32 = *qu.run_and_get("
			return math.foo(1)
		").unwrap();
		dbg!(result);
	}

	#[test]
	#[should_panic]
	/// Panic if an item from another module is accessed without importing that
	/// item.
	fn not_imported_panic_2() {
		let mut qu = Qu::new();
		let result = qu.run("
			return foo(3)
		").unwrap();
		dbg!(result);
	}


	#[test]
	#[should_panic]
	/// Panic if a specific function is imported, but not the module, then that
	/// function accessed through the module.
	fn not_imported_panic_3() {
		let mut qu = Qu::new();
		let result:i32 = *qu.run_and_get("
			import math.foo
			return math.foo(1)
		").unwrap();
		dbg!(result);
	}


	#[test]
	/// Return an imported module.
	fn return_module() {
		let mut qu = Qu::new();
		let result:&Module = qu.run_and_get("
			import math
			return math
		").unwrap();
		dbg!(result);
	}


	#[test]
	#[should_panic]
	/// Panic when a module is referenced without being imported.
	fn return_module_panic() {
		let mut qu = Qu::new();
		let result:&Module = qu.run_and_get("
			return math
		").unwrap();
		dbg!(result);
	}
}