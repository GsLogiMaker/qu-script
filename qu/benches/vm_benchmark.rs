
extern crate criterion;

use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;

use qu::Qu;
use qu::QuExtFnId;
use qu::QuOp;
use qu::QuStackId;


fn speed(c: &mut Criterion) {
	let mut qu = Qu::new();

	let n1:QuStackId = 0.into();
	let nterms:QuStackId = 1.into();
	let n2:QuStackId = 2.into();
	let count:QuStackId = 3.into();
	let zero:QuStackId = 20.into();
	let add:QuExtFnId = 0.into();
	let lesser:QuExtFnId = 1.into();

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
}


criterion_group!(
	name = bench;
	config = Criterion::default();
	targets = speed
);

criterion_main!(bench);