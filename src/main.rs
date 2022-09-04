
#[macro_use]
extern crate lazy_static;


use qu_script::Qu;
use qu_script::QuLeaf;
use qu_script::QuCompiler;
use qu_script::QuVm;
use qu_script::QuParser;
use qu_script::QuOperation;

use std::ops::Index;

struct A {
	a:i32,
	b:i32,
	c:i32,
	d:i32,
}


macro_rules! struct_QuOpLibrary {
	( $(  $name:ident:$asm_keyword:ident( $($args:expr),+ )  )+ ) => {
		struct_QuOpLibrary!(
			$(
				[  0] $name:$asm_keyword(
					$(
						$args
					),+
				)
			)+
		);
	};

	( $(  [$idx:expr] $name:ident:$asm_keyword:ident( $($args:expr),+ )  )+ ) => {
		pub struct QuOpLibrary<'a> {
			ops:Vec<QuOperation<'a>>,
			$(
				$name:u8,
			)+
		} impl<'a> QuOpLibrary<'a> {
			
			fn new() -> Self {
				return Self{
					ops:vec![
						$(
							QuOperation::new(stringify!($name), stringify!($asm_keyword), &[   $( ($args,), )+   ]),
						)+
					],
					
					$(
						$name:$idx,
					)+
				};
			}

		}
	};
}




// (main, sub)(1)[0]
// main:{ASM, (1), 0}
// [0] main:ASM(1)

//struct_QuOpLibrary!(
//	[  0] end:END(1)
//	[  1] add:ADD(1, 1, 1)
//);

struct_QuOpLibrary!(
	end:END(1)
	add:ADD(1, 1)
);


//lazy_static! {
//    static ref OPLIB:QuOpLibrary<'static> = QuOpLibrary::new();
//}

fn main() {
	println!("");
	println!("---START---");
	println!("");

	let code = "
	if 1 == 10:
		print 55
	";

	let mut qu = Qu::new();
	qu.run(code).unwrap_or_else(|err|{panic!("{}", err)});
	println!("{}", qu.compile_to_asm(code).unwrap());

}

