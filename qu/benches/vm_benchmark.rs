
extern crate criterion;

use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;

use qu::Qu;
use qu::QuMsg;


fn speed(c: &mut Criterion) {
	let mut qu = Qu::new();
}


criterion_group!(
	name = bench;
	config = Criterion::default();
	targets = speed
);

criterion_main!(bench);