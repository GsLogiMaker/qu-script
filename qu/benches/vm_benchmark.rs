
#[macro_use]
extern crate criterion;

use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;

use qu::OPLIB;
use qu::Qu;
use qu::QuCompiler;
use qu::QuFunctionId;
use qu::QuInt;
use qu::QuMsg;
use qu::QuOp;
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
	
	let mut qu = Qu::new();
	qu.register_struct::<QuInt>();
	qu.register_fns();

	let nterms:QuRegId = 0.into();
	let n1:QuRegId = 1.into();
	let n2:QuRegId = 2.into();
	let count:QuRegId = 3.into();
	let zero:QuRegId = 20.into();
	let add:QuFunctionId = 0.into();
	let lesser:QuFunctionId = 1.into();

	let mut code = vec![];

	// Defines a constant zero
	code.append(&mut vec![OPLIB.load_val_u8, 0, 20]);
			
	// var nterms = 9
	let p_nterms = 0;
	code.append(&mut vec![OPLIB.load_val_u8, 25, p_nterms]);
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
	code.append(&mut vec![OPLIB.call_ext, 0,0,0,1, p_count, p_nterms, 4]);
	code.append(&mut vec![OPLIB.jump_by_if_not, 0,0,0,while_body_len]);

		// var nth = n1 + n2
		code.append(&mut vec![OPLIB.call_ext, 0,0,0,0, p_n1, p_n2, 4]);
		// n1 = n2
		code.append(&mut vec![OPLIB.call_ext, 0,0,0,0, p_n2, 20, p_n1]);
		// n2 = nth
		code.append(&mut vec![OPLIB.call_ext, 0,0,0,0, 4, 20, p_n2]);
		// count = count + 1
		code.append(&mut vec![OPLIB.load_val_u8, 1, 5]);
		code.append(&mut vec![OPLIB.call_ext, 0,0,0,0, p_count, 5, p_count]);

	code.append(&mut vec![OPLIB.jump_by]);
	code.append(&mut (-((8+5)+(while_body_len as i8)) as i32).to_be_bytes().to_vec());


	let ops = vec![
		QuOp::Value(QuInt(0), zero),
		QuOp::Value(QuInt(25), nterms),
		QuOp::Value(QuInt(0), n1),
		QuOp::Value(QuInt(1), n2),
		QuOp::Value(QuInt(0), count),

		QuOp::CallExt(lesser, vec![count, nterms], QuRegId(4)),
		QuOp::JumpByIfNot(6),

			QuOp::CallExt(add, vec![n1, n2], QuRegId(4)),
			QuOp::CallExt(add, vec![n2, zero], n1),
			QuOp::CallExt(add, vec![QuRegId(4), zero], n2),

			QuOp::Value(QuInt(1), QuRegId(5)),
			QuOp::CallExt(add, vec![count, QuRegId(5)], count),

		QuOp::JumpBy(-8),

		QuOp::End,
	];

	c.bench_function(
		"bytes", |b| {
			b.iter(|| {
				qu.run_bytes(&code).unwrap();
			})
		}
	);

	c.bench_function(
		"ops", |b| {
			b.iter(|| {
				qu.run_ops(&ops).unwrap();
			})
		}
	);

	println!("{:?}", qu.reg_get_as::<QuInt>(QuRegId(1)).unwrap());
	println!("{:?}", qu.reg_get_as::<QuInt>(QuRegId(2)).unwrap());
	println!("{:?}", qu.reg_get_as::<QuInt>(QuRegId(3)).unwrap());
}


criterion_group!(
	name = bench;
	config = Criterion::default();
	targets = speed
);

criterion_main!(bench);