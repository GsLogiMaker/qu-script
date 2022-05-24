
use qu_script::tokenize;
use qu_script::QuCompiler;
use qu_script::QuVm;
use qu_script::QuParser;
use qu_script::Op;
use qu_script::RULES;


fn main() {
	println!("");
	println!("---START---");
	println!("");

	let expr_str = "2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2".to_string();
	//let expr_str = "1 + 2 * 3 / 4 + 5 * 6 - 7".to_string();
	let expr_str = "
		vl bike
		vl car
		vl plane
		bike = 5
		bike = 6
		bike = 4
		car = 1
		plane = 2
	".to_string();

	let mut tokens = &mut tokenize(&expr_str, RULES);
	println!("Tokens: ");
	for tk in tokens.iter() {
		println!("	'{}' {} {}", tk.text, tk.row, tk._col);
	}

	let mut parser = QuParser::new(tokens);
	let mut a = parser.parse();
	println!("Tree:");
	for line in &a {
		println!("	{}", line);
	}

	let mut c = QuCompiler::new();
	let compiled = c.compile(&mut a);
	println!("Code: {:?}", compiled);
	
	let mut vm = QuVm::new();
	print!("Decomp:");
	for line in vm.code_to_asm(&compiled).split("\n") {
		println!("	{}", line);
	}

	vm.run_bytes( compiled.as_slice() );
	println!("Regs: {:?}", vm.registers);
	println!("");

}
