
use qu_script::Qu;
use qu_script::QuLeaf;
use qu_script::QuCompiler;
use qu_script::QuVm;
use qu_script::QuParser;


fn main() {
	println!("");
	println!("---START---");
	println!("");

	let code = "
	if 1 == 10:
		print 55
	";

	let mut qu = Qu::new();
	qu.run(code).unwrap_or_else(|err|{panic!("{}", err)});
	println!("{}", qu.compile_to_asm(code).unwrap());

}

