
use qu_script::QuLeaf;
use qu_script::tokenize;
use qu_script::QuCompiler;
use qu_script::QuVm;
use qu_script::QuParser;
use qu_script::RULES;


fn main() {
	println!("");
	println!("---START---");
	println!("");

	let expr_str = r#"
vl n1 = 0
vl n2 = 1
vl nth = 0

vl nterms = 50
vl count = 0
while count < nterms:
	nth = n1 + n2
	n1 = n2
	n2 = nth
	print nth
	count = count + 1
"#.to_string();
	println!("Script: {}", expr_str);

	// Tokens
	let tokens = &mut tokenize(&expr_str, RULES);
	println!("Tokens: ");
	for tk in tokens.iter() {
		println!("	'{}' {} {}", tk.text, tk.row, tk._col);
	}

	// Parser
	let mut parser = QuParser::new(tokens, &expr_str);
	let instruction_vec = parser.parse();
	let leaf_block = QuLeaf::Block(instruction_vec);
	println!("Tree: \n{}", &leaf_block.tree_fmt(0));

	// Compiler
	let mut c = QuCompiler::new(&expr_str);
	let compiled = match c.compile(&leaf_block) {
		Ok(compiled) => compiled,
		Err(e) => {
			panic!("Qu Error: {}", e);
		}
	};
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

}

