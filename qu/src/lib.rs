
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

mod compiler;
mod errors;
mod import;
pub mod objects;
mod parser;
mod tokens;
mod vm;

use std::any::type_name;
use std::marker::PhantomData;

use compiler::RegistrationMethod;
use import::ClassId;
use tokens::TOKEN_TYPE_NAME;
use tokens::QuToken;
pub use errors::QuMsg;
pub use compiler::QuCompiler;
pub use objects::*;
pub use parser::QuParser;
pub use vm::QuOp;
pub use vm::QuVm;
pub use vm::QuStackId;
pub use import::RegistererLayer;

type Uuid = uuid::Uuid;

/// The interface for the Qu programming language.
/// 
/// # Examples
/// ```
/// use qu::Qu;
/// 
/// # fn main() {run_test().unwrap();}
/// # fn run_test() -> Result<(), qu::QuMsg>{
/// let mut qu = Qu::new();
/// 
/// qu.run(r#"
/// 	fn adder(a int) int:
/// 		var left int = 2
/// 		var right int = 5
/// 		return left + right
/// 
/// 	adder(0)
/// "#)?;
/// # return Ok(());
/// # }
/// ```
#[derive(Default)]
pub struct Qu<'a> {
	vm: QuVm,
	ph: PhantomData<&'a ()>,
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
			vm: QuVm::new(Uuid::new_v4()),
			ph: PhantomData {},
		}
	}

	/// Returns the [`ClassId`] registered to the struct in this Qu instance.
	pub fn get_class_id_of<T: Register + 'static>(
		&self
	) -> Option<ClassId> {
		T::get_id(self.get_uuid())
	}

	/// Compiles Qu script without running it.
	/// 
	/// Does not return the compiled bytecode.
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
	/// qu.compile("
	/// 	fn foo() int:
	/// 		return 3
	/// ")?;
	/// let result:i32 = *qu.run_and_get("return foo()")?;
	/// assert_eq!(result, 3);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn compile(&mut self, code:&str) -> Result<(), QuMsg> {
		// Compile
		let mut c = QuCompiler::new();
		c.compile(code, &mut self.vm.definitions)?;
		Ok(())
	}


	/// Returns the unique identifier of this Qu instance.
	pub fn get_uuid(&self) -> &Uuid {
		&self.vm.definitions.uuid
	}

	/// Registers external items (such as functions, classes, and modules).
	/// 
	/// # Example
	/// 
	/// ```
	/// # use qu::QuMsg;
	/// # fn main(){example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// use qu::Qu;
	/// use qu::QuRegisterStruct;
	/// use qu::RegistererLayer;
	/// 
	/// struct MyClass(i32);
	/// impl QuRegisterStruct for MyClass {
	/// 	fn name() -> &'static str {
	/// 		"MyClass"
	/// 	}
	/// }
	///
	/// let mut qu = Qu::new();
	/// qu.register(&|r| {
	/// 	r.add_module("my_module", &|m| {
	/// 		let my_class_id = m.add_class::<MyClass>()?;
	/// 		m.add_class_static_function(
	/// 			my_class_id,
	/// 			".new",
	/// 			&[],
	/// 			my_class_id,
	/// 			&|api| {
	/// 				api.set(MyClass(25));
	/// 				Ok(())
	/// 			}
	/// 		)?;
	/// 		Ok(())
	/// 	})?;
	/// 	Ok(())
	/// }).unwrap();
	///
	/// let value:&MyClass = qu.run_and_get("
	/// 	import my_module.MyClass
	/// 	return MyClass()
	/// ").unwrap();
	/// assert_eq!(value.0, 25);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn register(&mut self, body: &RegistrationMethod) -> Result<(), QuMsg>{
		self.vm.definitions.register(body)
	}


	/// Run a [`&str`] as Qu script.
	/// 
	/// # Errors
	/// 
	/// If `code` contains improper syntax or a problem occurs at runtime
	/// then an [`Err`] is returned.
	/// 
	/// # Example
	/// 
	/// ```
	/// # use qu::QuMsg;
	/// # fn main(){example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// use qu::Qu;
	/// 
	/// let mut qu = Qu::new();
	/// 
	/// qu.run("var count int = 5 + 6")?;
	/// # return Ok(());
	/// # }
	/// ```
	pub fn run(&mut self, script:&str) -> Result<(), QuMsg> {
		self.compile(script)?;
		self.vm.loop_ops(self.vm.definitions.byte_code_blocks.len()-1)
	}


	/// Run a [`&str`] as Qu script and get the result.
	/// 
	/// This is evuivalent to running [`Qu::run`] followed
	/// by [`Qu::get_result`].
	/// 
	/// # Errors
	/// 
	/// If `code` contains improper syntax or a problem occurs at runtime
	/// then an [`Err`] is returned.
	/// 
	/// # Example
	/// 
	/// ```
	/// # use qu::QuMsg;
	/// # fn main(){example().unwrap()}
	/// # fn example() -> Result<(), QuMsg> {
	/// use qu::Qu;
	/// 
	/// let mut qu = Qu::new();
	/// 
	/// let value:i32 = *qu.run_and_get("return 5 + 6")?;
	/// assert_eq!(value, 5 + 6);
	/// # return Ok(());
	/// # }
	/// ```
	pub fn run_and_get<T: Register + 'static>(
		&mut self,
		script:&str,
	) -> Result<&T, QuMsg> {
		let Some(type_class_id) = self.get_class_id_of::<T>()
			else {
				return Err(format!(
					"Type {} is not registered in this Qu isntance",
					type_name::<T>(),
				).into());
			};

		self.run(script)?;
		let return_id = self.vm.return_value_id();

		if type_class_id != return_id {
			return Err(format!(
				"Returned value of type {} does not match requested value ot type {}",
				self.vm.definitions.classes[return_id.0].common.name,
				type_name::<T>(),
			).into())
		}

		self.vm.read::<T>(QuStackId::new(0, return_id))
	}

}


#[cfg(test)]
mod lib {
    use crate::{Qu, Module, Float};

	// TODO: Test what happens when a function overrides a class name
	// TODO: Test what happens when a class constructor that doesn't exist is called

	#[test]
	fn floats() {
		let mut qu = Qu::new();
		let result:Float = *qu.run_and_get("
			var number float = 3.12
			var added float = number + 4.2
			added = added + 3.
			return added - 1.01
		").unwrap();
		assert_eq!(result, 3.12 + 4.2 + 3.0 - 1.01);
	}

	#[test]
	fn constants() {
		let mut qu = Qu::new();
		
		let pi:i32 = *qu.run_and_get("
			return PI
		").unwrap();
		assert_eq!(pi, 3);
	}

	#[test]
	fn bool_literals() {
		let mut qu = Qu::new();

		let result:bool = *qu.run_and_get::<bool>("
			var value bool = true
			return value
		").unwrap();
		assert_eq!(result, true);

		let result:bool = *qu.run_and_get::<bool>("
			var value bool = false
			return value
		").unwrap();
		assert_eq!(result, false);
	}

	#[test]
	fn instantiate_class() {
		let mut qu = Qu::new();

		let result:i32 = *qu.run_and_get("
			return int()
		").unwrap();
		assert_eq!(result, 0);

		let result:i32 = *qu.run_and_get("
			return int(5)
		").unwrap();
		assert_eq!(result, 5);

		let result:i32 = *qu.run_and_get("
			return int(0==0) + int(0==0) + int(0!=0)
		").unwrap();
		assert_eq!(result, 1+1+0+0);

		let result:bool = *qu.run_and_get::<bool>("
			return bool()
		").unwrap();
		assert_eq!(result, false);

		let result:bool = *qu.run_and_get::<bool>("
			return bool(0==0)
		").unwrap();
		assert_eq!(result, true);

		let result:bool = *qu.run_and_get::<bool>("
			return bool(1)
		").unwrap();
		assert_eq!(result, true);
	}

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

		let mut qu = Qu::new();
		let script = r#"
			fn five() void:
				return 5
			return five()
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
		qu.run_and_get::<i32>(script).unwrap();
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
	fn repeated_runs() {
		let mut qu = Qu::new();
		qu.run("var a int = 10").unwrap();
		qu.run("var b int = 13").unwrap();
	}


	// #[test]
	// TODO: Allow accessing functions from class name (Requires constant evaluation)
	fn _class_dot_notation() {
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
		
		assert!(result == true.into());
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


	#[test]
	fn cross_run_accessing() {
		// Test cross accessing functions
		let mut qu = Qu::new(); // register_fns is called once in new().
		qu.run("
			fn thrice(num int) int:
				return num * 3
		").unwrap();
		let num = *qu.run_and_get::<i32>("
			return thrice(2)
		").unwrap();
		assert_eq!(num, 2*3);

		// TODO: Allow accessing global variables across invocations of 'run'.
		// Test cross accessing variables
		// let mut qu = Qu::new();
		// qu.run("
		// 	var first int = 5
		// 	var second int = 20
		// ").unwrap();
		// let num = *qu.run_and_get::<i32>("
		// 	return first * second
		// ").unwrap();
		// assert_eq!(num, 5*20);
	}


	// TODO: Prevent functions definitions from having multiple parameters of
	// 	the same name 
	// #[test]
	// #[should_panic]
	fn _function_multiple_parameters_same_name() {
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