
extern crate criterion;

use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;


fn speed(_c: &mut Criterion) {
}


criterion_group!(
	name = bench;
	config = Criterion::default();
	targets = speed
);

criterion_main!(bench);