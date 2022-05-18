

//! TODO: Project level documentation.

#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
#![warn(rustdoc::broken_intra_doc_links)]

use std::{fmt::{self, Display}, str::FromStr, any::Any};

#[repr(u8)]
#[derive(PartialEq)]
enum Op {
	END = 0,
	LD_CONST,
	LD_VAL,
	ADD,
	SUB,
	MUL,
	DIV,
	MOD,
	POW,
	LESSER,
	GREATER,
	EQUAL,
	JP_TO,
	JP_BY,
	JP_TO_IF,
	JP_BY_IF,
}

pub enum QuLeaf {
	Expression(QuOp, Box<QuLeaf>, Box<QuLeaf>),
	Value(u64),

} impl Display for QuLeaf {

	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			QuLeaf::Expression(op, lft, rht) => {
				let opstr:&str = (*op).into();
				return write!(f, "Expr({lft} {opstr} {rht})");
			}
			QuLeaf::Value(val) => {
				return write!(f, "v{val}");
			}
			_ => {
				return write!(f, "<QuLeaf NULL>");
			}
		}
	}

}

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


pub struct QuParser<'a> {
	tk_idx:usize,
	tk_stack:Vec<usize>,
	tokens:&'a Vec<QuToken<'a>>,

} impl<'a> QuParser<'a> {

	pub fn new(tokens:&'a mut Vec<QuToken<'a>>) -> Self {
		tokens.push(
			QuToken::new(tokens.len() as u64, tokens.len() as u64, 
			0, 0, 0, u8::MAX, "")
		);
		return QuParser {
			tk_idx:0,
			tk_stack:vec![],
			tokens:tokens,
		}
	}

	/// Checks for an expression
	fn ck_expr(&mut self) -> Option<QuLeaf> {
		if let Some(check) = self.ck_op_sub() {
			return Some(check);
		}

		return None;
	}


	fn ck_op_add(&mut self) -> Option<(QuLeaf)> {
		return self.ck_operation("+", &Self::ck_op_div);
	}


	fn ck_op_div(&mut self) -> Option<(QuLeaf)> {
		return self.ck_operation("/", &Self::ck_op_mul);
	}


	fn ck_op_mul(&mut self) -> Option<(QuLeaf)> {
		return self.ck_operation("*", &Self::ck_value);
	}


	fn ck_op_sub(&mut self) -> Option<(QuLeaf)> {
		return self.ck_operation("-", &Self::ck_op_add);
	}


	/// Returns a tuple with the type, and two box pointers to expressions
	fn ck_operation(
			&mut self, operator:&str,
			next:&dyn Fn(&mut Self)->Option<(QuLeaf)>,
			) -> Option<(QuLeaf)> {

		self.tk_push();

		// Check left side for value
		let data_l = next(self);
		if let None = data_l {
			self.tk_pop();
			return None;
		}
		let data_l = data_l.unwrap();

		let tk_op = self.tk_spy(0);
		if tk_op.text() != operator {
			return Some(data_l);
		}
		self.tk_next();

		// Check right side for expression
		let data_r = next(self);
		if let None = data_r {
			self.tk_pop();
			return None;
		}
		let data_r = data_r.unwrap();

		return Some(
			QuLeaf::Expression(
				QuOp::from(operator),
				Box::new(data_l),
				Box::new(data_r)
			)
		);
	}


	fn ck_value(&mut self) -> Option<(QuLeaf)> {
		self.tk_push();
		return match self.tk_next().text().parse::<u64>() {
			Ok(x) => Some(QuLeaf::Value(x)),
			Err(x) => {
				self.tk_pop();
				None
			},
		};
	}


	pub fn parse(&mut self) -> Vec<QuLeaf> {
		self.tk_idx = 0;
		let mut leafs = vec![];

		while self.tk_idx < self.tokens.len() {
			match self.ck_expr() {
				Some(x) => {leafs.push(x);}
				None => {
					break;
				//	panic!("Parsing failed at {i}:'{j}'", 
				//		i=self.tk_idx, 
				//		j=self.tokens[self.tk_idx].text());
				}
			}
			
		}

		return leafs;
	}


	fn tk_next(&mut self) -> &QuToken {
		let a = &self.tokens[self.tk_idx];
		self.tk_idx += 1;
		return a;
	}


	fn tk_pop(&mut self) {
		self.tk_idx = self.tk_stack.pop().unwrap();
	}


	fn tk_push(&mut self) {
		self.tk_stack.push(self.tk_idx);
	}


	fn tk_spy(&mut self, at:usize) -> &QuToken {
		if self.tk_idx+at >= self.tokens.len() {
			return &self.tokens[self.tokens.len()-1];
		}
		return &self.tokens[self.tk_idx+at];
	}

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
		if self.begin == u64::MAX {
			return "".to_string();
		}

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

} impl<'a> Display for QuToken<'a> {
	
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

	/// Makes a new [`Vm`].
	pub fn new() -> Self {
		let mut vm = QuVm { 
			pc:0,
			source:vec![],
			registers:[0;16],
			mem:vec![],
			operations:vec![
				Operation{code:Op::END as u8, keyword:"end",},
				Operation{code:Op::LD_CONST as u8, keyword:"load_const",},
				Operation{code:Op::LD_VAL as u8, keyword:"load_val",},
				Operation{code:Op::ADD as u8, keyword:"add",},
				Operation{code:Op::SUB as u8, keyword:"sub",},
				Operation{code:Op::MUL as u8, keyword:"mul",},
				Operation{code:Op::DIV as u8, keyword:"div",},
				Operation{code:Op::MOD as u8, keyword:"mod",},
				Operation{code:Op::POW as u8, keyword:"pow",},
				Operation{code:Op::LESSER as u8, keyword:"lesser",},
				Operation{code:Op::GREATER as u8, keyword:"greater",},
				Operation{code:Op::EQUAL as u8, keyword:"equal",},
				Operation{code:Op::JP_TO as u8, keyword:"jump",},
				Operation{code:Op::JP_BY as u8, keyword:"jump_by",},
				Operation{code:Op::JP_TO_IF as u8, keyword:"jump_if",},
				Operation{code:Op::JP_BY_IF as u8, keyword:"jump_by_if",},
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


	fn exc_load_val_u8(&mut self) {
		let val = self.next() as usize;
		let rg_to = self.next() as usize;
		self.registers[rg_to] = val as u64;
	}


	fn exc_math_add(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				+ self.registers[rg_right] as u64;
	}


	fn exc_math_sub(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				- self.registers[rg_right] as u64;
	}


	fn exc_math_mul(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				* self.registers[rg_right] as u64;
	}


	fn exc_math_div(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				/ self.registers[rg_right] as u64;
	}


	fn exc_math_mod(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				% self.registers[rg_right] as u64;
	}


	fn exc_math_pow(&mut self) {
		let rg_left = self.next() as usize;
		let rg_right = self.next() as usize;
		let rg_result = self.next() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				^ self.registers[rg_right] as u64;
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
				 x if x == Op::END as u8 => {println!("Halting"); break;},

				x if x == Op::LD_CONST as u8 => self.exc_load_const_u8(),
				x if x == Op::LD_VAL as u8 => self.exc_load_val_u8(),

				x if x == Op::ADD as u8 => self.exc_math_add(),
				x if x == Op::SUB as u8 => self.exc_math_sub(),
				x if x == Op::MUL as u8 => self.exc_math_mul(),
				x if x == Op::DIV as u8 => self.exc_math_div(),
				x if x == Op::MOD as u8 => self.exc_math_mod(),
				x if x == Op::POW as u8 => self.exc_math_pow(),

				x if x == Op::LESSER as u8 => self.exc_logi_lesser(),
				x if x == Op::GREATER as u8 => self.exc_logi_greater(),
				x if x == Op::EQUAL as u8 => self.exc_logi_equal(),

				x if x == Op::JP_TO as u8 => self.exc_jump_to(),
				x if x == Op::JP_BY as u8 => self.exc_jump_by(),
				x if x == Op::JP_TO_IF as u8 => self.exc_jump_to_if(),
				x if x == Op::JP_BY_IF as u8 => self.exc_jump_by_if(),

				x => { println!("{x}"); todo!(); }
			}

		}
	}

}


#[derive(Clone, Copy)]
pub struct QuOp {
	op:u8,
} impl From<&str> for QuOp {

	fn from(x:&str) -> Self {
		match x {
			"+"  => QuOp{op:0},
			"-"  => QuOp{op:1},
			"*"  => QuOp{op:2},
			"/"  => QuOp{op:3},
			"%"  => QuOp{op:4},
			"**" => QuOp{op:5},
			"//" => QuOp{op:6},
			"&"  => QuOp{op:7},
			"|"  => QuOp{op:8},
			"^"  => QuOp{op:9},
			
			"+="  => QuOp{op:10},
			"-="  => QuOp{op:11},
			"*="  => QuOp{op:12},
			"/="  => QuOp{op:13},
			"%="  => QuOp{op:16},
			"**=" => QuOp{op:14},
			"//=" => QuOp{op:15},
			"&="  => QuOp{op:17},
			"|="  => QuOp{op:18},
			"^="  => QuOp{op:19},

			":" => QuOp{op:20},
			"," => QuOp{op:21},
			"=" => QuOp{op:22},
			
			"==" => QuOp{op:23},
			"!=" => QuOp{op:24},
			">=" => QuOp{op:25},
			"<=" => QuOp{op:26},

			_ => QuOp{op:u8::MAX},
		}
	}

}

impl From<QuOp> for &str {

	fn from(x:QuOp) -> Self {
		match x.op {
			0 => "+",
			1 => "-",
			2 => "*",
			3 => "/",
			4 => "%",
			5 => "**",
			6 => "//",
			7 => "&",
			8 => "|",
			9 => "^",
			
			10 => "+=" ,
			11 => "-=" ,
			12 => "*=" ,
			13 => "/=" ,
			16 => "%=" ,
			14 => "**=",
			15 => "//=",
			17 => "&=" ,
			18 => "|=" ,
			19 => "^=" ,

			20 => ":",
			21 => ",",
			22 => "=",
			
			23 => "==",
			24 => "!=",
			25 => ">=",
			26 => "<=",

			_ => "",
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
		['&',] => true,
		['|',] => true,
		['^',] => true,
		[':',] => true,
		[',',] => true,
		['=',] => true,
		['*', '*',] => true,
		['/', '/',] => true,
		['=', '=',] => true,
		['!', '=',] => true,
		['>', '=',] => true,
		['<', '=',] => true,
		['+', '=',] => true,
		['-', '=',] => true,
		['*', '=',] => true,
		['/', '=',] => true,
		['%', '=',] => true,
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