
#[macro_use]
extern crate criterion;

use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;

use qu::OPLIB;
use qu::QuCompiler;
use qu::QuInt;
use qu::QuMsg;
use qu::QuRegId;
use qu::QuVm;


fn speed(c: &mut Criterion) {
	let script = black_box(r#"
	var nterms = 9
	var n1 = 0
	var n2 = 1
	var count = 0

	while count < nterms:
		var nth = n1 + n2
		n1 = n2
		n2 = nth
		count = count + 1
	"#);
	let mut compiler = QuCompiler::new();
	let bytes = black_box(compiler.compile(script).unwrap());
	
	let mut vm = QuVm::new();
	vm.external_struct_register::<QuInt>();

	let mut code = vec![];

	// Defines a constant zero
	code.append(&mut vec![OPLIB.load_val_u8, 0, 20]);
		
	// var nterms = 9
	let p_nterms = 0;
	code.append(&mut vec![OPLIB.load_val_u8, 9, p_nterms]);
	// var n1 = 0
	let p_n1 = 1;
	code.append(&mut vec![OPLIB.load_val_u8, 0, p_n1]);
	// var n2 = 1
	let p_n2 = 2;
	code.append(&mut vec![OPLIB.load_val_u8, 1, p_n2]);
	// var count = 0
	let p_count = 3;
	code.append(&mut vec![OPLIB.load_val_u8, 0, p_count]);

	// while count < nterms:
	let while_body_len = (8*4)+3+5;
	code.append(&mut vec![OPLIB.call_ext, p_count, 0,0,0,1, 4, p_nterms]);
	code.append(&mut vec![OPLIB.jump_by_if_not, 0,0,0,while_body_len]);

		// var nth = n1 + n2
		code.append(&mut vec![OPLIB.call_ext, p_n1, 0,0,0,0, 4, p_n2]);
		// n1 = n2
		code.append(&mut vec![OPLIB.call_ext, p_n2, 0,0,0,0, p_n1, 20]);
		// n2 = nth
		code.append(&mut vec![OPLIB.call_ext, 4, 0,0,0,0, p_n2, 20]);
		// count = count + 1
		code.append(&mut vec![OPLIB.load_val_u8, 1, 5]);
		code.append(&mut vec![OPLIB.call_ext, p_count, 0,0,0,0, p_count, 5]);

	code.append(&mut vec![OPLIB.jump_by]);
	code.append(&mut (-((8+5)+(while_body_len as i8)) as i32).to_be_bytes().to_vec());

	c.bench_function(
		"regs", |b| {
			b.iter(|| {
				vm.run_bytes(&code).unwrap();
			})
		}
	);

	println!("{:?}", vm.reg_get_as::<QuInt>(QuRegId(1)).unwrap());
	println!("{:?}", vm.reg_get_as::<QuInt>(QuRegId(2)).unwrap());
	println!("{:?}", vm.reg_get_as::<QuInt>(QuRegId(3)).unwrap());
}


criterion_group!(
	name = bench;
	config = Criterion::default();
	targets = speed
);

criterion_main!(bench);