
extern crate criterion;

use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;

use qu::Qu;
use qu::QuCompiler;
use qu::QuMsg;
use qu::QuOp;
use qu::QuStackId;


fn speed(c: &mut Criterion) {
	let mut qu = Qu::new();

	let n1:QuStackId = 0.into();
	let nterms:QuStackId = 1.into();
	let n2:QuStackId = 2.into();
	let count:QuStackId = 3.into();
	let zero:QuStackId = 20.into();
}


criterion_group!(
	name = bench;
	config = Criterion::default();
	targets = speed
);

criterion_main!(bench);