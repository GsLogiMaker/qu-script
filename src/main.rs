
use qu_script::tokenize;
use qu_script::QuCompiler;
use qu_script::QuVm;
use qu_script::QuParser;
use qu_script::RULES;

fn main() {
	println!("");
	println!("---START---");
	println!("");

	let expr_str = "2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2";
	//let expr_str = "1 + 2 * 3 / 4 + 5 * 6 - 7";
	let expr_str = "(5 == 5) + 20";
	let mut expr_tk = &mut tokenize(expr_str, RULES);
	let mut parser = QuParser::new(expr_tk);
	let mut a = parser.parse();
	println!("Tree: {}", a[0]);

	let mut c = QuCompiler::new();
	let compiled = c.compile(&mut a);
	println!("Code: {:?}", compiled);
	
	let mut vm = QuVm::new();
	println!("Decomp:");
		for line in vm.decompile_asm(&compiled).split("\n") {
		println!(" {}", line);
	}

	vm.run_bytes( compiled.as_slice() );
	println!("Regs: {:?}", vm.registers);
	println!("");

}
