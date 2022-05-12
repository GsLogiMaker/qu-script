
use qu_script::tokenize;
use qu_script::QuVm;

/// var i = 0
/// var j = 2
/// while i < 10
/// 	j = j * j
/// 	i = i + 1
/// 
/// stack push 0
/// stack push 2
/// 
/// loop:
/// 	# Multiply j = j * j
/// 	push stack[-1] // Pushes value at -1 to top of stack
///		push stack[-2] // Pushes value at -2 to top of stack
/// 	mult // Muliplies the top two elements in stack
/// 	move -2 // First highest element in stack to -2 in stack
/// 
/// 	# Add i = i + 1
/// 	push stack[-2]
/// 	push_val 1
/// 	add
/// 	move -3
/// 
/// # Less than i < 10
/// push stack[-2]
/// push_val 10
/// lesser
/// if_jump loop
fn main() {
	println!("");
	println!("---START---");
	println!("");


	let script = "
var default = 128
var dave.=5
	";

	//let a = tokenize(script);
	//for token in a {
	//	println!("{token}")
	//}

	let asm = "
		load_const $constant0 0
		load_const $constant1 1
		load_const $constant2 2
		load_const $constant3 3

		flag loop
			add 1 1 1
			add 0 2 0

			lesser 0 3 4
			jump_if 4 $loop

			end

		flag constant0
			0
		flag constant1
			2
		flag constant2
			1
		flag constant3
			10
	";

	let mut vm = QuVm::new();
	let bcode_vec = vm.compile_asm(asm);
	let bcode = bcode_vec.as_slice();
	vm.run_bytes( bcode );
	println!("")
}
