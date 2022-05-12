

//! TODO: Project level documentation.

#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
#![warn(rustdoc::broken_intra_doc_links)]

use std::{fmt::{self}, str::FromStr};

const TK_VARIENT_NAME:u8 = 0;
const TK_VARIENT_NUMBER:u8 = 1;

type Rules<'a> = [(&'a dyn Fn(&[char])->bool, u8)];

/// An array of function pointers to be used in [`chars_fit_rule`].
/// 
/// This array is used by [`chars_fit_rule`] to determin if a pattern of
/// characters should be turned into a [`Token`]. See [`tokenrule_name`] or 
/// [`tokenrule_keyword`] for examples of how a `tokenrule_*` function should
/// be structured.
pub const RULES:&Rules = &[
	(&tokenrule_keyword, 0),
	(&tokenrule_symbols, 0),
	(&tokenrule_number, 0),
	(&tokenrule_name, 0),
];

pub const ASM_RULES:&Rules = &[
	(&tokenrule_symbols, 0),
	(&tokenrule_flagref, 0),
	(&tokenrule_number, 0),
	(&tokenrule_name, 0),
];


struct Operation<'a> {
	code:u8,
	keyword:&'a str,
}


/// A slice of a script file with information on the row, column, and indent of
/// the slice.
pub struct QuToken<'a> {
	begin:u64,
	end:u64,
	row:u64,
	_col:u64,
	indent:u8,
	source:&'a str,
	varient:u8,

} impl<'a> QuToken<'a> {

	/// Makes a new [`Token`].
	pub fn new(
			begin:u64, end:u64, row:u64, col:u64, indent:u8,
			varient:u8, source:&'a str,) -> QuToken {
		return QuToken{
			begin,
			end,
			row,
			_col:col,
			indent,
			source,
			varient:varient
		};
	}


	/// Returns a [`String`] copy of this [`Token`].
	pub fn text(&self) -> String {
		let mut result = String::default();
		for (idx, char) in self.source.char_indices() {
			if idx > self.end as usize {
				break
			}
			if idx >= self.begin as usize {
				result.push(char);
			}
		}
		return result;
	}

} impl<'a> std::fmt::Display for QuToken<'a> {
	
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		return write!(
				f, "<'{}' row:{}  indent:{}>",
				self.text(),
				self.row,
				self.indent,);
	}
}


pub struct QuVm<'a> {
	registers:[u64;16],
	pc:usize,
	pub source:Vec<u8>,
	pub mem:Vec<u64>,
	operations:Vec<Operation<'a>>,

} impl<'a> QuVm<'a> {
	
	const OP_NULL:u8 = 0;
	const OP_LOAD_CONST:u8 = 1;
	const OP_ADD:u8 = 2;
	const OP_SUB:u8 = 3;
	const OP_MUL:u8 = 4;
	const OP_DIV:u8 = 5;
	const OP_POW:u8 = 6;
	const OP_MOD:u8 = 7;
	const OP_LESSER:u8 = 8;
	const OP_GREATER:u8 = 9;
	const OP_EQUAL:u8 = 10;
	const OP_JUMP_TO:u8 = 11;
	const OP_JUMP_BY:u8 = 12;
	const OP_JUMP_TO_IF:u8 = 13;
	const OP_JUMP_BY_IF:u8 = 14;

	/// Makes a new [`Vm`].
	pub fn new() -> Self {
		let mut vm = QuVm { 
			pc:0,
			source:vec![],
			registers:[0;16],
			mem:vec![],
			operations:vec![
				Operation{code:QuVm::OP_NULL, keyword:"end",},
				Operation{code:QuVm::OP_LOAD_CONST, keyword:"load_const",},
				Operation{code:QuVm::OP_ADD, keyword:"add",},
				Operation{code:QuVm::OP_SUB, keyword:"sub",},
				Operation{code:QuVm::OP_MUL, keyword:"mul",},
				Operation{code:QuVm::OP_DIV, keyword:"div",},
				Operation{code:QuVm::OP_POW, keyword:"pow",},
				Operation{code:QuVm::OP_MOD, keyword:"mod",},
				Operation{code:QuVm::OP_LESSER, keyword:"lesser",},
				Operation{code:QuVm::OP_GREATER, keyword:"greater",},
				Operation{code:QuVm::OP_EQUAL, keyword:"equal",},
				Operation{code:QuVm::OP_JUMP_TO, keyword:"jump",},
				Operation{code:QuVm::OP_JUMP_BY, keyword:"jump_by",},
				Operation{code:QuVm::OP_JUMP_TO_IF, keyword:"jump_if",},
				Operation{code:QuVm::OP_JUMP_BY_IF, keyword:"jump_by_if",},
			],
		};

		return vm;
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
	pub fn compile_asm(&mut self, code:&str) -> Vec<u8> {
		let mut bcode = vec![];

		let tokens = tokenize(code, ASM_RULES);
		

		// Look for flags
		let mut flags = vec![];
		let mut next_is_flag = false;
		let mut i = 0;
		for tk in &tokens {
			let tk_string = tk.text();

			if tk_string == "flag" {
				next_is_flag = true;
				continue;

			} else if next_is_flag {
				next_is_flag = false;
				flags.push(
					(tk_string, i)
				);
				continue;
			}

			i += 1;
		}

		// Compile
		for tk in &tokens {
			let a = tk.text().to_lowercase();
			let tk_str = a.as_str();

			// Flag reference
			if tk_str.chars().next() == Some('$') {
				for (flag, idx) in &flags {
					let mut flag2 = "$".to_string();
					flag2.push_str(flag);
					if tk_str == flag2 {
						bcode.push(*idx as u8);
					}
				}
				continue;
			}

			// Keyword
			let mut is_oper:bool = false;
			for oper in &self.operations {
				if tk_str == oper.keyword {
					bcode.push(oper.code);
					is_oper = true;
					break;
				}
			}

			// Number
			if !is_oper {
				match tk.text().to_string().parse::<u8>() {
					Ok(number) => {bcode.push(number)}
					Err(x) => {}
				}
			}
			
		}

		return bcode;
	}


	fn exc_jump_by(&mut self) {
	}


	fn exc_jump_by_if(&mut self) {
	}


	fn exc_jump_to(&mut self) {
	}


	fn exc_jump_to_if(&mut self) {
		let rg_if = self.next() as usize;
		let val_to = self.next() as usize;
		if self.registers[rg_if] != 0 {
			self.pc = val_to as usize;
		}
	}


	/// Executes a load instruction.
	fn exc_load_const_u8(&mut self) {
		let src_from = self.next() as usize;
		let rg_to = self.next() as usize;
		self.registers[rg_to] = self.source[src_from] as u64;
	}


	fn exc_math_add(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				+ self.registers[rg_right] as u64;
	}


	fn exc_math_div(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				/ self.registers[rg_right] as u64;
	}


	fn exc_math_mult(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				* self.registers[rg_right] as u64;
	}


	fn exc_math_sub(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				- self.registers[rg_right] as u64;
	}


	fn exc_logi_equal(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				(self.registers[rg_left] == self.registers[rg_right]) as u64;
	}


	fn exc_logi_greater(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				(self.registers[rg_left] > self.registers[rg_right]) as u64;
	}


	fn exc_logi_lesser(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				(self.registers[rg_left] < self.registers[rg_right]) as u64;
	}


	/// Gets the next byte in the source code as a u8 int.
	fn next(&mut self) -> u8 {
		let val = self.source[self.pc];
		self.pc += 1;
		return val;
	}

	
	pub fn run_bytes(&mut self, code:&[u8]) {
		self.source = code.to_vec();
		loop {
			
			match self.next() {
				QuVm::OP_NULL => {
					println!("Halting");
					break;
				}
				QuVm::OP_LOAD_CONST => {
					self.exc_load_const_u8();
				}
				QuVm::OP_ADD => {
					self.exc_math_add();
				}
				QuVm::OP_JUMP_TO => {
					self.exc_jump_to();
				}
				QuVm::OP_JUMP_BY => {
					self.exc_jump_by();
				}
				QuVm::OP_JUMP_TO_IF => {
					self.exc_jump_to_if();
				}
				QuVm::OP_JUMP_BY_IF => {
					self.exc_jump_by_if();
				}
				QuVm::OP_GREATER => {
					self.exc_logi_greater();
				}
				QuVm::OP_LESSER => {
					self.exc_logi_lesser();
				}
				QuVm::OP_EQUAL => {
					self.exc_logi_equal();
				}
				x => { println!("{x}"); todo!(); }
			}

		}
	}

}


pub fn tokenrule_flagref(added_so_far:&[char]) -> bool {
	if added_so_far[0] != '$' {
		return false;
	}

	let mut i = 1; // Set to 1 because first checked previously
	while i < added_so_far.len() {
		if added_so_far[i] == ' ' {
			return false
		}
		if !added_so_far[i].is_alphanumeric() && added_so_far[i] != '_' {
			return false;
		}
		i += 1;
	}

	return true;
}


/// Takes a [`Vec`] of [`char`]s and returns true if it matches a name.
///
/// A name could be a type, class, function name, or variable name.
/// 
/// Example
/// ```
/// use qu_script::tokenrule_name;
///
///	let chars1:&[char] = &['_', '_', 'i', 'n', 'i', 't', '_', '_',];
///	assert!(qu_script::tokenrule_name(chars1));
///
///	let chars2:&[char] = &['a', 'b', '1', ];
///	assert!(qu_script::tokenrule_name(chars2));
///
///	let chars3:&[char] = &['a', '+', '=', ];
///	assert!(!qu_script::tokenrule_name(chars3));
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
/// use qu_script::tokenrule_number;
///
///	let chars1:&[char] = &['5', '.', '6',];
///	assert!(qu_script::tokenrule_number(chars1));
///
///	let chars2:&[char] = &['1','0',];
///	assert!(qu_script::tokenrule_number(chars2));
///
///	let chars3:&[char] = &['a', ];
///	assert!(!qu_script::tokenrule_number(chars3));
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
/// use qu_script::tokenrule_keyword;
///
///	let chars1:&[char] = &['v', 'a', 'r',];
///	assert!(qu_script::tokenrule_keyword(chars1));
///
///	let chars2:&[char] = &['i','f',];
///	assert!(qu_script::tokenrule_keyword(chars2));
///
///	let chars3:&[char] = &['d', 'u', 'd', 'e',];
///	assert!(!qu_script::tokenrule_keyword(chars3));
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
/// use qu_script::tokenrule_symbols;
///
///	let chars1:&[char] = &['*',];
///	assert!(qu_script::tokenrule_symbols(chars1));
///
///	let chars2:&[char] = &['=','=',];
///	assert!(qu_script::tokenrule_symbols(chars2));
///
///	let chars3:&[char] = &['+', '1'];
///	assert!(!qu_script::tokenrule_symbols(chars3));
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
		['$',] => true,
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
/// use qu_script::Token;
/// use qu_script::tokenize;
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
pub fn tokenize<'a>(script:&'a str, rules:&Rules<'a>) -> Vec<QuToken<'a>> {
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
			let (does_fit, char_type) = chars_fit_rule(
				&added_so_far,
				rules,
			);
			if does_fit{
				if curr_token <= tokens.len() && added_so_far.len() == 1 {
					tokens.push(QuToken::new(
						idx as u64,
						idx as u64,
						row,
						col,
						indent,
						char_type,
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
pub fn chars_fit_rule<'a>(chars:&Vec<char>, rules:&Rules<'a>) -> (bool, u8) {
	let mut fits_rule = false;
	let mut i = 0;
	for rule in rules {
		fits_rule = fits_rule || rule.0(&chars);
		if fits_rule{
			break;
		}
		i += rule.1;
	}

	return (fits_rule, i);
}