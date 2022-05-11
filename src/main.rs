
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
		const 8 0
		const 9 0
		add
		end
		5
		10";

	let mut vm = QuVm::new();
	vm.run_bytes( QuVm::compile_asm(asm).as_slice() );
	println!("")
}
