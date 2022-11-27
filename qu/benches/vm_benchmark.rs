
#[macro_use]
extern crate criterion;

use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;

use qu::Qu;
use qu::QuCompiler;
use qu::QuFunctionId;
use qu::QuInt;
use qu::QuMsg;
use qu::QuOp;
use qu::QuStackId;
use qu::QuVm;


fn speed(c: &mut Criterion) {
	let script = black_box(r#"
	var nterms = 25
	var n1 = 0
	var n2 = 1
	var count = 0

	while count < nterms:
		var nth = n1 + n2
		n1 = n2
		n2 = nth
		count = count + 1
	"#);
	
	let mut qu = Qu::new();

	let compiled_code = black_box(qu.compile(script).unwrap());

	let nterms:QuStackId = 0.into();
	let n1:QuStackId = 1.into();
	let n2:QuStackId = 2.into();
	let count:QuStackId = 3.into();
	let zero:QuStackId = 20.into();
	let add:QuFunctionId = 0.into();
	let lesser:QuFunctionId = 1.into();

	let ops = vec![
		QuOp::Value(0, zero),
		QuOp::Value(25, nterms),
		QuOp::Value(0, n1),
		QuOp::Value(1, n2),
		QuOp::Value(0, count),

		QuOp::CallExt(lesser, vec![count, nterms], QuStackId(4)),
		QuOp::JumpByIfNot(6),

			QuOp::CallExt(add, vec![n1, n2], QuStackId(4)),
			QuOp::CallExt(add, vec![n2, zero], n1),
			QuOp::CallExt(add, vec![QuStackId(4), zero], n2),

			QuOp::Value(1, QuStackId(5)),
			QuOp::CallExt(add, vec![count, QuStackId(5)], count),

		QuOp::JumpBy(-8),

		QuOp::End,
	];

	c.bench_function(
		"ops", |b| {
			b.iter(|| {
				qu.run_ops(&ops).unwrap();
			})
		}
	);

	println!("{:?}", qu.reg_get_as::<QuInt>(QuStackId(1)).unwrap());
	println!("{:?}", qu.reg_get_as::<QuInt>(QuStackId(2)).unwrap());
	println!("{:?}", qu.reg_get_as::<QuInt>(QuStackId(3)).unwrap());
}


criterion_group!(
	name = bench;
	config = Criterion::default();
	targets = speed
);

criterion_main!(bench);