
use qu_script::Qu;
use qu_script::OPLIB;


fn main() {
	println!("---START---\n");

	let script = r##"
	vl count = 0
	print 100
	while count < 10:
		count = count + 1
		if count > 4:
			print count
	print 700
	"##;

	let mut qu = Qu::new();

	println!("{}", qu.compile_to_asm(script).expect("oh no"));
	qu.run(script).unwrap_or_else(|err|{panic!("{}", err)});



}

