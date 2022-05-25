
use qu_script::QuLeaf;
use qu_script::QuLeafExpr;
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


	let expr_str = "
		vl car =
				5 + 5
		vl bus
	".to_string();

	// Tokens
	let tokens = &mut tokenize(&expr_str, RULES);
	println!("Tokens: ");
	for tk in tokens.iter() {
		println!("	'{}' {} {}", tk.text, tk.row, tk._col);
	}

	// Parser
	let mut parser = QuParser::new(tokens);
	let mut instruction_vec = parser.parse();
	println!("Tree:");
	for line in &instruction_vec {
		println!("	{}", line);
	}
	let if_tree = QuLeaf::IfStatement(
		QuLeafExpr::Int(1),
		instruction_vec
	);

	// Compiler
	let mut c = QuCompiler::new();
	//let compiled = c.compile(&mut instruction_vec);
	let compiled = c.compile(&mut vec![if_tree]);
	println!("Code: {:?}", compiled);
	
	// Vm
	let mut vm = QuVm::new();
	print!("Decomp:");
	for line in vm.code_to_asm(&compiled).split("\n") {
		println!("	{}", line);
	}

	vm.run_bytes( compiled.as_slice() );
	println!("Regs: {:?}", vm.registers);
	println!("Mem: {:?}", vm.mem);
	println!("{} {} {}", !0, !-1, !-2);

}
