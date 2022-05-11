

//! TODO: Project level documentation.

#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
#![warn(rustdoc::broken_intra_doc_links)]

use std::fmt::{self};

const OP_NULL:u8 = 0;
const OP_CONST_TO_STACK:u8 = 1;
const OP_ADD_U8:u8 = 2;
const OP_JUMP_TO:u8 = 3;
const OP_JUMP_BY:u8 = 4;
const OP_JUMP_TO_IF:u8 = 5;
const OP_JUMP_BY_IF:u8 = 6;

const TK_VARIENT_NAME:u8 = 0;
const TK_VARIENT_NUMBER:u8 = 1;

/// An array of function pointers to be used in [`chars_fit_rule`].
/// 
/// This array is used by [`chars_fit_rule`] to determin if a pattern of
/// characters should be turned into a [`Token`]. See [`tokenrule_name`] or 
/// [`tokenrule_keyword`] for examples of how a `tokenrule_*` function should
/// be structured.
pub const RULES:&[& dyn Fn(&[char])->bool] = &[
	&tokenrule_keyword,
	&tokenrule_symbols,
	&tokenrule_number,
	&tokenrule_name,
];


/// A slice of a script file with information on the row, column, and indent of
/// the slice.
pub struct GeToken<'a> {
	begin:u64,
	end:u64,
	row:u64,
	_col:u64,
	indent:u8,
	source:&'a str,
	varient:u8,

} impl<'a> GeToken<'a> {

	/// Makes a new [`Token`].
	pub fn new(
			begin:u64, end:u64, row:u64, 
			col:u64, indent:u8, source:&'a str) -> GeToken {
		return GeToken{begin, end, row, _col: col, indent, source, varient:0};
	}


	/// Returns a [`String`] copy of this [`Token`].
	pub fn text(&self, text:&str) -> String {
		let mut result = String::default();
		for (idx, char) in text.char_indices() {
			if idx > self.end as usize {
				break
			}
			if idx >= self.begin as usize {
				result.push(char);
			}
		}
		return result;
	}

} impl<'a> std::fmt::Display for GeToken<'a> {
	
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		return write!(
				f, "<'{}' row:{}  indent:{}>",
				self.text(self.source),
				self.row,
				self.indent,);
	}
}


pub struct GeVm {
	registers:[i32;32],
	pc:usize,
	stack:Vec<u8>,
	pub source:Vec<u8>,
} impl GeVm {
	
	/// Makes a new [`Vm`].
	pub fn new() -> Self {
		return GeVm { pc:0, stack:vec![], source:vec![], registers:[0;32] };
	}


	fn u8_pair_to_u16(b1:u8, b2:u8) -> u16 {
		return b1 as u16 + ((b2 as u16) << 8);
	}


	fn u16_pair_to_u32(b1:u16, b2:u16) -> u32 {
		return b1 as u32 + ((b2 as u32) << 16);
	}


	fn u32_pair_to_u64(b1:u32, b2:u32) -> u64 {
		return b1 as u64 + ((b2 as u64) << 32);
	}


	/// Compiles byte code from low level VM instructions.
	pub fn compile_asm(code:&str) -> Vec<u8> {
		let mut bcode = vec![];

		let mut bytes_to_grab = 0;
		for tk in tokenize(code) {
			if bytes_to_grab > 0 {
				bcode.push(
						tk.text(tk.source)
							.to_string()
							.parse::<u8>()
							.unwrap());
				bytes_to_grab -= 1;
				continue;
			}

			match tk.text(tk.source).to_lowercase().as_str() {
				"end" => {
					bcode.push(OP_NULL);
				}
				"const" => {
					bcode.push(OP_CONST_TO_STACK);
					bytes_to_grab = 2;
				}
				"add" => {
					bcode.push(OP_ADD_U8);
				}
				_ => {
					bcode.push(
						tk.text(tk.source)
							.to_string()
							.parse::<u8>()
							.unwrap());
				}
			}
		}

		return bcode;
	}


	/// Executes a load instruction.
	fn exc_add_u8(&mut self) {
		let rhs = self.stack.pop().expect("");
		let lhs = self.stack.pop().expect("");
		self.stack.push(lhs + rhs);
	}


	fn exc_jump_by(&mut self) {
		let to_a = Self::u8_pair_to_u16(
				self.stack.pop().expect(""),
				self.stack.pop().expect("")
				);
		let to_b = Self::u8_pair_to_u16(
				self.stack.pop().expect(""),
				self.stack.pop().expect("")
				);
		self.pc += Self::u16_pair_to_u32(to_a, to_b) as usize;
	}


	fn exc_jump_by_if(&mut self) {
		if self.next_u8() != 0 {
			self.exc_jump_by();
		} else {
			self.stack.pop().expect("");
			self.stack.pop().expect("");
			self.stack.pop().expect("");
			self.stack.pop().expect("");
		}
	}


	fn exc_jump_to(&mut self) {
		let to_a = Self::u8_pair_to_u16(
				self.stack.pop().expect(""),
				self.stack.pop().expect("")
				);
		let to_b = Self::u8_pair_to_u16(
				self.stack.pop().expect(""),
				self.stack.pop().expect("")
				);
		self.pc = Self::u16_pair_to_u32(to_a, to_b) as usize;
	}


	fn exc_jump_to_if(&mut self) {
		if self.next_u8() != 0 {
			self.exc_jump_to();
		} else {
			self.stack.pop().expect("");
			self.stack.pop().expect("");
			self.stack.pop().expect("");
			self.stack.pop().expect("");
		}
	}


	/// Executes a load instruction.
	fn exc_load_const_u8(&mut self) {
		let load_from = self.next_u16() as usize;
		self.stack.push(self.source[load_from]);
	}


	/// Gets the next byte in the source code as a u8 int.
	fn next_u8(&mut self) -> u8 {
		let val = self.source[self.pc];
		self.pc += 1;
		return val;
	}

	/// Gets the next 2 bytes in the source code as a u16 int.
	fn next_u16(&mut self) -> u16 {
		return Self::u8_pair_to_u16(self.next_u8(), self.next_u8());
	}


	/// Gets the next 4 bytes in the source code as a u32 int.
	fn next_u32(&mut self) -> u32 {
		return Self::u16_pair_to_u32(
				self.next_u16(), 
				self.next_u16()
				);
	}

	
	pub fn run_bytes(&mut self, code:&[u8]) {
		self.source = code.to_vec();
		loop {
			
			match self.next_u8() {
				OP_NULL => {
					println!("Halting");
					break;
				}
				OP_CONST_TO_STACK => {
					self.exc_load_const_u8();
				}
				OP_ADD_U8 => {
					self.exc_add_u8();
				}
				OP_JUMP_TO => {
					self.exc_jump_to();
				}
				OP_JUMP_BY => {
					self.exc_jump_by();
				}
				OP_JUMP_TO_IF => {
					self.exc_jump_to_if();
				}
				OP_JUMP_BY_IF => {
					self.exc_jump_by_if();
				}
				_ => { todo!() }
			}

		}
	}

}


/// Takes a [`Vec`] of [`char`]s and returns true if it matches a name.
///
/// A name could be a type, class, function name, or variable name.
/// 
/// Example
/// ```
/// use ge_script::tokenrule_name;
///
///	let chars1:&[char] = &['_', '_', 'i', 'n', 'i', 't', '_', '_',];
///	assert!(ge_script::tokenrule_name(chars1));
///
///	let chars2:&[char] = &['a', 'b', '1', ];
///	assert!(ge_script::tokenrule_name(chars2));
///
///	let chars3:&[char] = &['a', '+', '=', ];
///	assert!(!ge_script::tokenrule_name(chars3));
/// ```
pub fn tokenrule_name(added_so_far:&[char]) -> bool {
	for char in  added_so_far {
		if *char == ' ' {
			return false
		}
		if !char.is_alphanumeric() && *char != '_' {
			return false;
		}
	}

	return true;
}


/// Takes a [`Vec`] of [`char`]s and returns true if it matches a number.
/// Some examples of numbers are *3.2* and *16*.
/// 
/// Example
/// ```
/// use ge_script::tokenrule_number;
///
///	let chars1:&[char] = &['5', '.', '6',];
///	assert!(ge_script::tokenrule_number(chars1));
///
///	let chars2:&[char] = &['1','0',];
///	assert!(ge_script::tokenrule_number(chars2));
///
///	let chars3:&[char] = &['a', ];
///	assert!(!ge_script::tokenrule_number(chars3));
/// ```
pub fn tokenrule_number(added_so_far:&[char]) -> bool {
	
	for char in  added_so_far {
		if char == &' ' {
			return false
		}
		if !(char.is_numeric() || char == &'.') {
			return false;
		}
	}

	return true;
}


/// Takes a [`Vec`] of [`char`]s and returns true if it matches a keyword.
/// 
/// Some examples of keywords are *var*, *if*, and *fn*.
/// 
/// Example
/// ```
/// use ge_script::tokenrule_keyword;
///
///	let chars1:&[char] = &['v', 'a', 'r',];
///	assert!(ge_script::tokenrule_keyword(chars1));
///
///	let chars2:&[char] = &['i','f',];
///	assert!(ge_script::tokenrule_keyword(chars2));
///
///	let chars3:&[char] = &['d', 'u', 'd', 'e',];
///	assert!(!ge_script::tokenrule_keyword(chars3));
/// ```
pub fn tokenrule_keyword(added_so_far:&[char]) -> bool {
	return match added_so_far {
		['v', 'a', 'r',] => true,
		['f', 'n',] => true,
		['c', 'l', 'a', 's', 's',] => true,
		['i', 'f',] => true,
		['e', 'l', 's', 'e',] => true,
		['e', 'l', 'i', 'f',] => true,
		_ => false,
	};
}


/// Takes a [`Vec`] of [`char`]s and returns true if it matches an operator.
/// 
/// Some examples of operators are *+*, *-*, and *+=*.
/// 
/// Example
/// ```
/// use ge_script::tokenrule_symbols;
///
///	let chars1:&[char] = &['*',];
///	assert!(ge_script::tokenrule_symbols(chars1));
///
///	let chars2:&[char] = &['=','=',];
///	assert!(ge_script::tokenrule_symbols(chars2));
///
///	let chars3:&[char] = &['+', '1'];
///	assert!(!ge_script::tokenrule_symbols(chars3));
/// ```
pub fn tokenrule_symbols(added_so_far:&[char]) -> bool {
	return match added_so_far {
		['+',] => true,
		['-',] => true,
		['*',] => true,
		['/',] => true,
		['%',] => true,
		['^',] => true,
		['!',] => true,
		[':',] => true,
		[',',] => true,
		[';',] => true,
		['=',] => true,
		['=', '=',] => true,
		['!', '=',] => true,
		['>', '=',] => true,
		['<', '=',] => true,
		['+', '=',] => true,
		['-', '=',] => true,
		['*', '=',] => true,
		['/', '=',] => true,
		['%', '=',] => true,
		['^', '=',] => true,
		_ => false,
	};
}


/// Takes a &[`str`] and returns a [`Vec`] of [`Token`]s.
/// 
/// The rules govorning what becomes a [`Token`] are specified by the functions
/// in [`RULES`].
/// 
/// Example
/// ```
/// use ge_script::Token;
/// use ge_script::tokenize;
/// 
/// let script:&str = " hello=world ;! ";
/// 
/// let tokens:Vec<Token> = tokenize(&script);
/// 
///	assert!(tokens.len() == 5);
///	assert!(tokens[0].text(&script) == "hello");
///	assert!(tokens[1].text(&script) == "=");
///	assert!(tokens[2].text(&script) == "world");
///	assert!(tokens[3].text(&script) == ";");
///	assert!(tokens[4].text(&script) == "!");
/// ```
pub fn tokenize<'a>(script:&'a str) -> Vec<GeToken> {
	let mut tokens = vec!();

	/* WARNING: This does not account for grapheme clusters. Currently hoping
	This won't be a problem. */
	let mut row:u64 = 0;
	let mut col:u64 = 0;
	let mut indent:u8 = 0;
	let mut in_begining:bool = true;
	let mut added_so_far:Vec<char> = Vec::with_capacity(20);
	let mut curr_token = 0;
	for (idx, char) in script.char_indices() {
		col += 1;

		if char != '\t' && char != ' ' {
			in_begining = false;
		}

		// Check tab
		if char == '\t' {
			if in_begining {
				indent += 1;
				added_so_far.clear();
			}
			
		// Check newline
		} else if char == '\n' {
			col = 0;
			row += 1;
			indent = 0;
			in_begining = true;		
		}

		// Any other characters
		added_so_far.push(char);
		
		// Update token end if it fits rule, 
		// otherwise clear the added so far
		loop {
			if chars_fit_rule(&added_so_far){
				if curr_token <= tokens.len() && added_so_far.len() == 1 {
					tokens.push(GeToken::new(
						idx as u64,
						idx as u64,
						row,
						col,
						indent,
						script,));
				}
				tokens[curr_token].end = idx as u64;
				break;
				
			} else if added_so_far.len() == 1 {
				added_so_far.clear();
				break;
			}else {
				if curr_token+1 == tokens.len() {
					curr_token += 1;
				}
				added_so_far.clear();
				added_so_far.push(char);
			}
		}
	}

	return tokens;
}


/// Checks a [`Vec`] of [`char`]s against [`RULES`].
/// 
/// Returns *true* if the [`Vec`] of [`char`]s fits at least one of the rules
/// specified in [`RULES`].
pub fn chars_fit_rule(chars:&Vec<char>) -> bool {
	let mut fits_rule = false;
	for rule in RULES {
		fits_rule = fits_rule || rule(&chars);
		if fits_rule{
			break;
		}
	}

	return fits_rule;
}