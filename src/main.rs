
use qu_script::Qu;
use qu_script::OPLIB;


fn main() {
	println!("---START---\n");

	let script = r##"
	vl count = 0
	vl count = 0
	while 
			count
			<
			10
			:
		count = count + 1
		if count > 4:
			print count
	"##;

	let mut qu = Qu::new();

	//println!("{}", qu.compile_to_asm(script));
	qu.run(script);

}

