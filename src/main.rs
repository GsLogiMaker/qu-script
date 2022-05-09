
use ge_script::tokenize;


fn main() {
	println!("");
	println!("---START---");
	println!("");


	let script = "
var default = 128
var dave.=5
	";

	let a = tokenize(script);
	for token in a {
		println!("{token}")
	}
}
