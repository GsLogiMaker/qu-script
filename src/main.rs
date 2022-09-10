
use qu_script::Qu;
use qu_script::OPLIB;


fn main() {
	println!("---START---\n");

	let script = r##"
	fn calc():
		vl left = 10
		vl right = 23
		vl result = left + right
		print result
	
	vl stuff = 3

	calc()
	calc()
	calc()
	calc()

	print stuff
	"##;

	let mut qu = Qu::new();

	//println!("{}", qu.compile_to_asm(script));
	qu.run(script);

}

