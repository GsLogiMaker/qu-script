
use qu_script::tokenize;
use qu_script::QuVm;
use qu_script::QuParser;
use qu_script::RULES;

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

	let asm = format!("
		load_val 0 {i}
		load_val 2 {double}
		load_val 1 {one}
		load_val 10 {ten}
		load_val 2 {two}

		flag loop
			mul {double} {two} {double}
			add {i} {one} {i}

			lesser {i} {ten} {is_lesser}
			jump_if {is_lesser} $loop

			end
	", i=0, double=1, one=2, ten=3, two=4, is_lesser=5);

	let mut vm = QuVm::new();
	let bcode_vec = vm.compile_asm(asm.as_str());
	let bcode = bcode_vec.as_slice();
	vm.run_bytes( bcode );
	println!("");

	let expr_str = "2 + 6 * 7 - 8 / 20";
	let mut expr_tk = &mut tokenize(expr_str, RULES);
	let mut parser = QuParser::new(expr_tk);
	let a = parser.parse();
	println!("{x}", x = a[0]);
	println!("Expr(v2 + Expr(Expr(v6 * v7) - Expr(v8 / v20)))");
}
