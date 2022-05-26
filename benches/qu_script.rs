
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use qu_script::{QuVm, tokenize, RULES, QuParser, QuCompiler};

fn fibonacci() -> () {
	let expr_str = r#"
	vl nterms = 50

	vl n1 = 0
	vl n2 = 1
	vl count = 0

	vl nth = 0
	while count < nterms:
		nth = n1 + n2
		n1 = n2
		n2 = nth
		count = count + 1
	"#.to_string();
	let tokens = &mut tokenize(&expr_str, RULES);
	// Parse
	let mut parser = QuParser::new(tokens, &expr_str);
	let mut instruction_vec = parser.parse();
	// Compile
	let mut c = QuCompiler::new();
	let code = c.compile(&mut instruction_vec);

	// Run
	let mut vm = QuVm::new();
	vm.run_bytes( &code );
}


fn fibonacci_make_byte_code() -> Vec<u8> {
	let expr_str = r#"
	vl nterms = 50

	vl n1 = 0
	vl n2 = 1
	vl count = 0

	vl nth = 0
	while count < nterms:
		nth = n1 + n2
		n1 = n2
		n2 = nth
		count = count + 1
	"#.to_string();
	let tokens = &mut tokenize(&expr_str, RULES);
	// Parse
	let mut parser = QuParser::new(tokens, &expr_str);
	let mut instruction_vec = parser.parse();
	// Compile
	let mut c = QuCompiler::new();
	let code = c.compile(&mut instruction_vec);
	return code;
}


fn fibonacci_run(code:&[u8]) -> () {
	// Run
	let mut vm = QuVm::new();
	vm.run_bytes(code);
}


fn criterion_benchmark(c: &mut Criterion) {
	c.bench_function(
		"Fib Full",
		|b| b.iter(|| fibonacci()));

	c.bench_function(
		"Fib Compile Byte Code",
		|b| b.iter(|| fibonacci_make_byte_code()));
	
	let code_vec = fibonacci_make_byte_code();
	let code = code_vec.as_slice();
	c.bench_function(
		"Fib Run",
		|b| b.iter(|| fibonacci_run(code)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);