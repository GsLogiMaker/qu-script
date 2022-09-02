
use qu_script::Qu;
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

	let code = "
	vl perms = 0
	vl i = 0
	print i
	vl i = 5
	print i
	vl n1 = 0
	vl n2 = 1
	while i < perms:
		vl n3 = n1 + n2
		n1 = n2
		n2 = n3
		i = i + 1
		print n3
		i = i + 1
	";

	let mut qu = Qu::new();
	qu.run(code).unwrap_or_else(|err|{panic!("{}", err)});
	println!("{}", qu.compile_to_asm(code).unwrap());

}

