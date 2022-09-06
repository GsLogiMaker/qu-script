
use qu_script::Qu;
use qu_script::OPLIB;


fn main() {
	println!("---START---\n");

	let test_fn_define_bytecode = &[
		// Define constants
		OPLIB.define_const_str, 2, 'p' as u8,'r' as u8,

		// Define print variables
		OPLIB.load_val_u8, 10, 0,
		OPLIB.load_val_u8, 20, 1,
		OPLIB.load_val_u8, 1, 2,

		// Pre print
		OPLIB.print, 0,

		// Define 'pr' function
		OPLIB.define_fn, 0,0, 0,16,
			OPLIB.print, 0,
			OPLIB.print, 1,
			OPLIB.print, 2,
			OPLIB.load_val_u8, 10, 0,
			OPLIB.load_val_u8, 20, 1,
			OPLIB.load_val_u8, 1, 2,
			OPLIB.end,

		// Post print
		OPLIB.print, 1,

		// Call 'pr'
		OPLIB.call, 0,0,0,0,
		OPLIB.call, 0,0,0,0,

		OPLIB.print, 0,
		OPLIB.print, 1,
		OPLIB.print, 2,
	];

	let mut qu = Qu::new();

	//qu.run(code).unwrap_or_else(|err|{panic!("{}", err)});
	qu.run_bytes(test_fn_define_bytecode).unwrap_or_else(|err|{panic!("{}", err)});


}

