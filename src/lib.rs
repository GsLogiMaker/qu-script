
//! TODO: Project level documentation.

#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
#![warn(rustdoc::broken_intra_doc_links)]

use std::{fmt::{self, Display, Debug}, str::FromStr, any::Any, vec};
use std::fmt::Write;

#[repr(u8)]
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Op {
	End = 0,
	LoadConst,
	LoadValU8,
	LoadValU16,
	LoadValU32,
	LoadValU64,
	LoadMem,
	StoreMem,
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Pow,
	Lesser,
	Greater,
	Equal,
	JumpTo,
	JumpBy,
	JumpToIf,
	JumpByIf,
} impl From<&str> for Op {

	fn from(x:&str) -> Self {
		match x.to_uppercase().as_str() {
			"ADD" => Op::Add,
			"SUB" => Op::Sub,
			"MUL" => Op::Mul,
			"DIV" => Op::Div,
			"MOD" => Op::Mod,
			"POW" => Op::Pow,
			"GRT" => Op::Greater,
			"LES" => Op::Lesser,
			"EQL" => Op::Equal,

			"+"   => Op::Add,
			"-"   => Op::Sub,
			"*"   => Op::Mul,
			"/"   => Op::Div,
			"%"   => Op::Mod,
			"**"  => Op::Pow,
			"<"   => Op::Lesser,
			">"   => Op::Greater,
			"=="  => Op::Equal,

			"//"  => {unimplemented!(); Op::End},
			"&"   => {unimplemented!(); Op::End},
			"|"   => {unimplemented!(); Op::End},
			"^"   => {unimplemented!(); Op::End},
			
			"+="  => {unimplemented!(); Op::End},
			"-="  => {unimplemented!(); Op::End},
			"*="  => {unimplemented!(); Op::End},
			"/="  => {unimplemented!(); Op::End},
			"%="  => {unimplemented!(); Op::End},
			"**=" => {unimplemented!(); Op::End},
			"//=" => {unimplemented!(); Op::End},
			"&="  => {unimplemented!(); Op::End},
			"|="  => {unimplemented!(); Op::End},
			"^="  => {unimplemented!(); Op::End},

			":"   => {unimplemented!(); Op::End},
			","   => {unimplemented!(); Op::End},
			OP_ASSIGN_WORD   => {unimplemented!(); Op::End},
			
			"!="  => {unimplemented!(); Op::End},
			">="  => {unimplemented!(); Op::End},
			"<="  => {unimplemented!(); Op::End},

			_ => Op::End,
		}
	}

} impl Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		return match self {
			Op::Add => write!(f, "+"),
			Op::Sub => write!(f, "-"),
			Op::Mul => write!(f, "*"),
			Op::Div => write!(f, "/"),
			Op::Mod => write!(f, "%"),
			Op::Pow => write!(f, "**"),
			Op::Greater => write!(f, ">"),
			Op::Lesser => write!(f, "<"),
			Op::Equal => write!(f, "=="),
			_ => write!(f, "{}", *self as u8),
		};
	}
}


#[derive(Debug, Clone)]
pub enum QuLeaf {
	Expression(Op, Box<QuLeaf>, Box<QuLeaf>),
	Value(u64),
	VarName(String),
	/// Name, Type, Value
	VarDecl(Box<QuLeaf>, Option<Box<QuLeaf>>, Option<Box<QuLeaf>>),

} impl<'a> Display for QuLeaf {

	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			QuLeaf::Expression(op, lft, rht) => {
				let string = format!("{}", op);
				let opstr:&str = string.as_str();
				return write!(f, "Expr({lft} {opstr} {rht})");
			}
			QuLeaf::Value(val) => {
				return write!(f, "v{val}");
			}
			QuLeaf::VarName(name) => {
				return write!(f, "'{name}'");
			}
			QuLeaf::VarDecl(
					name, 
					var_type, 
					val) => {
				return write!(f, "VarDecl({} {} {})", name, "todo", "todo");
			}
			_ => {
				return write!(f, "<QuLeaf Unimplemented Format>");
			}
		}
	}

}

const KEYWORD_CLASS:&str = "cl";
const KEYWORD_ELSE:&str = "else";
const KEYWORD_ELIF:&str = "elif";
const KEYWORD_FUNCTION:&str = "fn";
const KEYWORD_IF:&str = "if";
const KEYWORD_TRAIT:&str = "tr";
const KEYWORD_TRAIT_IMPL:&str = "does";
const KEYWORD_VAR:&str = "vl";
const KEYWORD_WHILE:&str = "while";

const OP_ASSIGN_WORD:&str = "=";

const TOKEN_TYPE_KEYWORD:u8 = 1;
const TOKEN_TYPE_SYMBOL:u8 = 2;
const TOKEN_TYPE_NUMBER:u8 = 3;
const TOKEN_TYPE_NAME:u8 = 4;

type Rules<'a> = [(&'a dyn Fn(&[char])->bool, u8)];

/// An array of function pointers to be used in [`chars_fit_rule`].
/// 
/// This array is used by [`chars_fit_rule`] to determin if a pattern of
/// characters should be turned into a [`Token`]. See [`tokenrule_name`] or 
/// [`tokenrule_keyword`] for examples of how a `tokenrule_*` function should
/// be structured.
pub const RULES:&Rules = &[
	(&tokenrule_keyword, TOKEN_TYPE_KEYWORD),
	(&tokenrule_symbols, TOKEN_TYPE_SYMBOL),
	(&tokenrule_number, TOKEN_TYPE_NUMBER),
	(&tokenrule_name, TOKEN_TYPE_NAME),
];

pub const ASM_RULES:&Rules = &[
	(&tokenrule_symbols, TOKEN_TYPE_KEYWORD),
	(&tokenrule_flagref, 0),
	(&tokenrule_number, TOKEN_TYPE_NUMBER),
	(&tokenrule_name, TOKEN_TYPE_NAME),
];


pub struct QuCompiler {
	reserved_vregs:[bool;16],
	variables:Vec<(String, usize)>,
	stack_ptr:usize,
} impl QuCompiler {

	pub fn new() -> Self {
		return Self{
			reserved_vregs:[false;16],
			variables:vec![],
			stack_ptr:0,
		};
	}


	/// Calculates an expression and returns the virtual register to the result.
	fn cmp_expression(
			&mut self, op_code:Op, lft:&QuLeaf, rgh:&QuLeaf
			) -> (Vec<u8>, u8) {
		
		// Right hand value code
		let mut rgh_data = match rgh {
			QuLeaf::Expression(op, lft, rgh)
				=> self.cmp_expression(*op, lft, rgh),
			QuLeaf::Value(v) => self.cmp_value(*v as u8),
			_ => {unimplemented!()}
		};

		// Left hand value code
		let mut lft_data = match lft {
			QuLeaf::Expression(op, lft, rgh)
				=> self.cmp_expression(*op, lft, rgh),
			QuLeaf::Value(v) => self.cmp_value(*v as u8),
			_ => {unimplemented!()}
		};

		let mut code:Vec<u8> = Vec::with_capacity(
				lft_data.0.len()+rgh_data.0.len() + 4);
		let to_reg = self.reg_reserve();
		code.append(&mut rgh_data.0);
		code.append(&mut lft_data.0);
		code.append(&mut vec![
			op_code as u8, lft_data.1, rgh_data.1, to_reg
		]);

		// Free registers
		self.reg_free(lft_data.1);
		self.reg_free(rgh_data.1);

		return (code, to_reg);
	}


	/// Reserves a virtual register, stores the literal in it, and returns
	/// the register.
	fn cmp_value(&mut self, value:u8) -> (Vec<u8>, u8) {
		let reg = self.reg_reserve();
		let mut code = Vec::with_capacity(10);
		code.push(Op::LoadValU8 as u8);
		code.append(&mut value.to_be_bytes().to_vec());
		code.push(reg);
		return (code, reg);
	}


	fn cmp_var_decl(&mut self, name:String, var_type:u8, val:u8) -> Vec<u8> {
		let stk = self.stack_reserve();
		self.variables.push(
			(name.clone(), stk)
		);

		// Construct code
		let mut code = Vec::with_capacity(10);
		// Get default val
		let rg_default_val = self.reg_reserve();
		code.push(Op::LoadValU8 as u8);
		code.push(0);
		code.push(rg_default_val);
		// Store in mem
		code.push(Op::StoreMem as u8);
		code.push(rg_default_val);
		code.append(&mut (stk as u32).to_be_bytes().to_vec());

		return code;
	}


	pub fn compile(&mut self, leafs:&mut Vec<QuLeaf>) -> Vec<u8> {
		let mut code:Vec<u8> = vec![];
		for leaf in leafs {
			match leaf {
				QuLeaf::Expression(
						op, 
						lft, 
						rgh) => {
					let (mut expr_code,_reg) = self.cmp_expression(
							*op, lft, rgh);
					code.append(&mut expr_code);
				}
				QuLeaf::Value(val) => {
					panic!(
						"QuLeaf::Value probobly should not be compiled alone!");
					code.push(Op::LoadValU8 as u8);
					code.append(&mut val.to_be_bytes().to_vec());
					code.push(self.reg_reserve());
				}
				QuLeaf::VarDecl(
						name_leaf,
						static_type_leaf,
						value_leaf
						) => {
					assert!(matches!(&(**name_leaf), QuLeaf::VarName(name)));
					
					let mut name = "".to_string();
					if let QuLeaf::VarName(name_) = &(**name_leaf) {
						name = name_.clone()
					}
					code.append(
						&mut self.cmp_var_decl(name, 0, 0)
					);
				}
				_ => {unimplemented!()}
			}
		}
		code.push(Op::End as u8);
		return code;
	}


	/// Reserves a register so that nothing else uses it until it's freed.
	fn reg_reserve(&mut self) -> u8 {
		let mut i:u8 = 0;
		while i < 16 {
			if !self.reserved_vregs[i as usize] {
				self.reserved_vregs[i as usize] = true;
				return i;
			}
			i += 1;
		}
		panic!("No more regs");
	}


	/// Frees a previously reserved register.
	fn reg_free(&mut self, reg:u8) {
		self.reserved_vregs[reg as usize] = false;
	}


	/// Returns and increments the stack pointer.
	fn stack_reserve(&mut self) -> usize {
		let x = self.stack_ptr;
		self.stack_ptr += 1;
		return x;
	}


	/// Closes the current stack frame.
	fn stack_frame_pop(&mut self) {
		unimplemented!()
	}


	/// Starts a new stack frame.
	fn stack_frame_push(&mut self) {
		unimplemented!()
	}

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


	fn ck_var_decl(&mut self) -> Result<Option<QuLeaf>, &str> {
		// Match keyword
		let keyword_tk = self.tk_spy(0);
		if keyword_tk != KEYWORD_VAR {
			return Ok(None);
		}
		self.tk_next();

		// Match variable name
		let name_data = self.ck_var_name();
		if let None = name_data {
			return Err("Token after 'var' does not match a name.");
		}
		let name_data = name_data.unwrap();

		// Match variable type
		let type_data = self.ck_type_name();

		// Match assign operator
		let keyword_tk = self.tk_spy(0);
		let mut assign_data = None;
		if keyword_tk == OP_ASSIGN_WORD {
			self.tk_next();
			assign_data = self.ck_expr();
			if let None = assign_data {
				return Err("Expected expression after '='.");
			}
		}

		// Create leaf boxes
		let name_box = Box::new(name_data);
		let mut type_box = None;
		if let Some(type_data) = type_data {
			type_box = Some(Box::new(type_data));
		}
		let mut assign_box = None;
		if let Some(assign_data) = assign_data {
			assign_box = Some(Box::new(assign_data));
		}
		
		return Ok(Some(QuLeaf::VarDecl(
			name_box,
			type_box,
			assign_box,
		)));
	}


	fn ck_var_name(&mut self) -> Option<QuLeaf> {
		let tk = self.tk_spy(0);
		if tk.tk_type != TOKEN_TYPE_NAME {
			return None;
		}
		return Some(QuLeaf::VarName( self.tk_next().text() ));
	}


	/// Checks for an expression
	fn ck_expr(&mut self) -> Option<QuLeaf> {
		if let Some(check) = self.ck_op_les() {
			return Some(check);
		}

		return None;
	}


	fn ck_op_les(&mut self) -> Option<QuLeaf> {
		return self.ck_operation("<", &Self::ck_op_grt);
	}


	fn ck_op_grt(&mut self) -> Option<QuLeaf> {
		return self.ck_operation(">", &Self::ck_op_eql);
	}


	fn ck_op_eql(&mut self) -> Option<QuLeaf> {
		return self.ck_operation("==", &Self::ck_op_sub);
	}


	fn ck_op_sub(&mut self) -> Option<QuLeaf> {
		return self.ck_operation("-", &Self::ck_op_add);
	}


	fn ck_op_add(&mut self) -> Option<QuLeaf> {
		return self.ck_operation("+", &Self::ck_op_div);
	}


	fn ck_op_div(&mut self) -> Option<QuLeaf> {
		return self.ck_operation("/", &Self::ck_op_mul);
	}


	fn ck_op_mul(&mut self) -> Option<QuLeaf> {
		return self.ck_operation("*", &Self::ck_op_paren_expr);
	}


	fn ck_op_paren_expr(&mut self) -> Option<QuLeaf> {
		self.tk_push();

		let tk = self.tk_next();
		if tk != "(" {
			self.tk_pop();
			self.tk_push();

			// Match for a value if no parenthesis expression is matched.
			let data = self.ck_value();
			if let None = data {
				self.tk_pop();
				return None;
			}
			return data;
		}

		let data = self.ck_expr();
		if let None = data {
			self.tk_pop();
			return None;
		}

		if self.tk_next() != ")" {
			panic!("Parenthesis expression ended without closing parenthesis.");
			self.tk_pop();
			return None;
		}

		return data;
	}


	/// Returns a QuLeaf
	fn ck_operation(
			&mut self, operator:&str,
			next:&dyn Fn(&mut Self)->Option<(QuLeaf)>,
			) -> Option<QuLeaf> {

		self.tk_push();

		// Check left side for value
		let data_l = next(self);
		if let None = data_l {
			self.tk_pop();
			return None;
		}
		let data_l = data_l.unwrap();

		// Check operator
		let tk_op = self.tk_spy(0);
		if tk_op != operator {
			return Some(data_l);
		}
		self.tk_next();

		// Check right side for expression
		let data_r = self.ck_operation(operator, next);//next(self);
		if let None = data_r {
			self.tk_pop();
			return None;
		}
		let data_r = data_r.unwrap();

		return Some(
			QuLeaf::Expression(
				Op::from(operator),
				Box::new(data_l),
				Box::new(data_r)
			)
		);
	}


	fn ck_type_name(&mut self) -> Option<QuLeaf> {
		return self.ck_var_name();
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
			// Variable declaration
			if let Some(data) = self.ck_var_decl().unwrap() {
				leafs.push(data);
			// Expression
			} else if let Some(data) = self.ck_expr() {
				leafs.push(data);
			// No more matches
			} else {
				break;
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
	pub begin:u64,
	pub end:u64,
	pub row:u64,
	pub _col:u64,
	pub indent:u8,
	pub source:&'a str,
	pub tk_type:u8,

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
			source:&source,
			tk_type:varient
		};
	}


	/// Returns a ['String'] copy of this ['Token']'s slice of
	/// the ['source'] text.
	pub fn text(&self) -> String {
		if self.tk_type == u8::MAX {
			return "".to_string();
		}
		return self.source[self.begin as usize..=self.end as usize].to_string();
	}

} impl<'a> Display for QuToken<'a> {
	
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		return write!(
				f, "<'{}' row:{}  indent:{}>",
				self.text(),
				self.row,
				self.indent,);
	}
} impl<'a> Debug for QuToken<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		return write!(f, "Hi: {}", 0);
		//f.debug_struct("QuToken")
		//	.field("begin", &self.begin)
		//	.field("end", &self.end)
		//	.field("row", &self.row)
		//	.field("_col", &self._col)
		//	.field("indent", &self.indent)
		//	.field("source", &self.source)
		//	.field("tk_type", &self.tk_type).finish()
	}
} impl<'a> PartialEq for QuToken<'a> {
	fn eq(&self, other:&Self) -> bool {
		if self.tk_type == u8::MAX {
			return false;
		}
		return self.source[self.begin as usize..self.end as usize]
			== other.source[other.begin as usize..=other.end as usize];
	}
} impl<'a> PartialEq<str> for QuToken<'a> {
	fn eq(&self, other:&str) -> bool {
		if self.tk_type == u8::MAX {
			return false;
		}
		return &self.source[self.begin as usize..=self.end as usize]
			== other;
	}
} impl<'a> PartialEq<String> for QuToken<'a> {
	fn eq(&self, other:&String) -> bool {
		if self.tk_type == u8::MAX {
			return false;
		}
		return &self.source[self.begin as usize..=self.end as usize]
			== other;
	}
}


type OpData<'a> = [(Op,&'a str,&'a [(&'a str, u8)])];
const OP_DATA:&OpData = &[
	(
		Op::End,
		"end", 
		&[],
	),
	(
		Op::LoadConst,
		"load_const",
		&[("s", 1), ("r", 1)],
	),
	(
		Op::LoadValU8,
		"load_val_u8",
		&[("", 1), ("r", 1)],
	),
	(
		Op::LoadValU16,
		"load_val_u16",
		&[("", 1), ("r", 1)],
	),
	(
		Op::LoadValU32,
		"load_val_u32",
		&[("", 1), ("r", 1)],
	),
	(
		Op::LoadValU64,
		"load_val_u64",
		&[("", 1), ("r", 1)],
	),
	(
		Op::LoadMem,
		"load",
		&[("m", 4), ("r", 1)],
	),
	(
		Op::StoreMem,
		"store",
		&[("r", 1), ("m", 4)],
	),
	(
		Op::Add,
		"add",
		&[("r", 1), ("r", 1)]
	),
	(
		Op::Sub,
		"sub",
		&[("r", 1), ("r", 1)]
	),
	(
		Op::Mul,
		"mul",
		&[("r", 1), ("r", 1)]
	),
	(
		Op::Div,
		"div",
		&[("r", 1), ("r", 1)]
	),
	(
		Op::Mod,
		"mod",
		&[("r", 1), ("r", 1)]
	),
	(
		Op::Pow,
		"pow",
		&[("r", 1), ("r", 1)]
	),
	(
		Op::Lesser,
		"lesser",
		&[("r", 1), ("r", 1)]
	),
	(
		Op::Greater,
		"greater",
		&[("r", 1), ("r", 1)]
	),
	(
		Op::Equal,
		"equal",
		&[("r", 1), ("r", 1)]
	),
	(
		Op::JumpTo,
		"jump",
		&[("", 4)]),
	(
		
		Op::JumpBy,
		"jump_by",
		&[("", 2)]),
	(
		
		Op::JumpToIf,
		"jump_if",
		&[("r", 1), ("", 4)]
	),
	(
		Op::JumpByIf,
		"jump_by_if",
		&[("r", 1), ("", 2)]
	),
];


pub struct QuVm {
	pub registers:[u64;16],
	pc:usize,
	pub source:Vec<u8>,
	pub mem:Vec<u64>,
	pub stack:Vec<u64>,

} impl QuVm {

	/// Makes a new [`Vm`].
	pub fn new() -> Self {
		let mut vm = QuVm { 
			pc:0,
			source:vec![],
			registers:[0;16],
			mem:vec![],
			stack:vec![],
		};

		return vm;
	}


	/// Compiles byte code from low level VM instructions.
	pub fn compile_asm(&mut self, code:&String) -> Vec<u8> {
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
		let mut i = 0;
		while i < tokens.len() {
			let tk = &tokens[i];
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
			for oper in OP_DATA {
				if tk_str == oper.1 {
					bcode.push(oper.0 as u8);
					is_oper = true;
					break;
				}

				// Parameters
				for (_p_prefix, p_size) in oper.2 {
					i += 1;
					let tk = &tokens[i];
					match p_size {
						1 => bcode.push(tk.text().parse::<u8>().unwrap()),
						2 => bcode.append(
							&mut tk.text()
								.parse::<u16>()
								.unwrap()
								.to_be_bytes()
								.to_vec()),
						4 => bcode.append(
							&mut tk.text()
								.parse::<u32>()
								.unwrap()
								.to_be_bytes()
								.to_vec()),
						8 => bcode.append(
							&mut tk.text()
								.parse::<u64>()
								.unwrap()
								.to_be_bytes()
								.to_vec()),
						_ => panic!(),
					}
					
				}
			}

			i += 1;
		}

		return bcode;
	}


	/// Converts byte code to human readable instructions.
	pub fn code_to_asm(&mut self, code:&Vec<u8>) -> String {
		let mut asm = String::new();
		
		let mut i = 0;
		while i < code.len() {
			let op_code = code[i];
			let (op, name, params)
					= OP_DATA[op_code as usize];
			assert!(op as u8 == op_code);

			// Add command text
			asm.push_str(format!("\n{}", name.to_uppercase()).as_str());

			// Add parameter text
			for (prefix, size) in params {
				// Get value
				let val = match size {
					1 => {
						let bytes = [code[i+1]];
						i += 1;
						u8::from_be_bytes(bytes) as u64
					}
					2 => {
						let bytes = [code[i+1], code[i+2]];
						i += 2;
						u16::from_be_bytes(bytes) as u64
					}
					4 => {
						let bytes = [
							code[i+1], code[i+2], code[i+3], code[i+4]];
						i += 4;
						u32::from_be_bytes(bytes) as u64
					}
					8 => {
						let bytes = [
							code[i+1], code[i+2], code[i+3], code[i+4],
							code[i+5], code[i+6], code[i+7], code[i+8]];
						i += 8;
						u64::from_be_bytes(bytes) as u64
					}
					_ => panic!(),
				};
				asm.push_str(format!(" {}{}", prefix, val).as_str());
			}
			i += 1;
		}

		return asm;
	}


	fn exc_jump_by(&mut self) {
	}


	fn exc_jump_by_if(&mut self) {
	}


	fn exc_jump_to(&mut self) {
	}


	fn exc_jump_to_if(&mut self) {
		let rg_if = self.next_u8() as usize;
		let val_to = self.next_u32() as usize;
		if self.registers[rg_if] != 0 {
			self.pc = val_to as usize;
		}
	}


	/// Executes a load instruction.
	fn exc_load_const_u8(&mut self) {
		let src_from = self.next_u32() as usize;
		let rg_to = self.next_u8() as usize;
		self.registers[rg_to] = self.source[src_from] as u64;
	}


	fn exc_load_val_u8(&mut self) {
		let val = self.next_u8() as u64;
		let rg_to = self.next_u8() as usize;
		self.registers[rg_to] = val;
	}


	fn exc_load_val_u16(&mut self) {
		let val = self.next_u16() as u64;
		let rg_to = self.next_u8() as usize;
		self.registers[rg_to] = val;
	}


	fn exc_load_val_u32(&mut self) {
		let val = self.next_u32() as u64;
		let rg_to = self.next_u8() as usize;
		self.registers[rg_to] = val;
	}


	fn exc_load_val_u64(&mut self) {
		let val = self.next_u64() as u64;
		let rg_to = self.next_u8() as usize;
		self.registers[rg_to] = val;
	}


	fn exc_load_mem(&mut self) {
		let mem_from = self.next_u32() as usize;
		let rg_to = self.next_u8() as usize;
		self.registers[rg_to] = self.mem[mem_from] as u64;
	}


	fn exc_store_mem(&mut self) {
		let rg_from = self.next_u8() as usize;
		let mem_to = self.next_u32() as usize;
		if mem_to >= self.mem.len() {
			self.mem.resize(mem_to+1, 0);
		}
		self.mem[mem_to] = self.registers[rg_from];
	}


	fn exc_math_add(&mut self) {
		let rg_left = self.next_u8() as usize;
		let rg_right = self.next_u8() as usize;
		let rg_result = self.next_u8() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				+ self.registers[rg_right] as u64;
	}


	fn exc_math_sub(&mut self) {
		let rg_left = self.next_u8() as usize;
		let rg_right = self.next_u8() as usize;
		let rg_result = self.next_u8() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				- self.registers[rg_right] as u64;
	}


	fn exc_math_mul(&mut self) {
		let rg_left = self.next_u8() as usize;
		let rg_right = self.next_u8() as usize;
		let rg_result = self.next_u8() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				* self.registers[rg_right] as u64;
	}


	fn exc_math_div(&mut self) {
		let rg_left = self.next_u8() as usize;
		let rg_right = self.next_u8() as usize;
		let rg_result = self.next_u8() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				/ self.registers[rg_right] as u64;
	}


	fn exc_math_mod(&mut self) {
		let rg_left = self.next_u8() as usize;
		let rg_right = self.next_u8() as usize;
		let rg_result = self.next_u8() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				% self.registers[rg_right] as u64;
	}


	fn exc_math_pow(&mut self) {
		let rg_left = self.next_u8() as usize;
		let rg_right = self.next_u8() as usize;
		let rg_result = self.next_u8() as usize;
		self.registers[rg_result] = 
				self.registers[rg_left] as u64
				^ self.registers[rg_right] as u64;
	}


	fn exc_logi_equal(&mut self) {
		let rg_left = self.next_u8() as usize;
		let rg_right = self.next_u8() as usize;
		let rg_result = self.next_u8() as usize;
		self.registers[rg_result] = 
				(self.registers[rg_left] == self.registers[rg_right]) as u64;
	}


	fn exc_logi_greater(&mut self) {
		let rg_left = self.next_u8() as usize;
		let rg_right = self.next_u8() as usize;
		let rg_result = self.next_u8() as usize;
		self.registers[rg_result] = 
				(self.registers[rg_left] > self.registers[rg_right]) as u64;
	}


	fn exc_logi_lesser(&mut self) {
		let rg_left = self.next_u8() as usize;
		let rg_right = self.next_u8() as usize;
		let rg_result = self.next_u8() as usize;
		self.registers[rg_result] = 
				(self.registers[rg_left] < self.registers[rg_right]) as u64;
	}


	/// Gets the next byte in the source code as a u8 int.
	fn next_u8(&mut self) -> u8 {
		let val = self.source[self.pc];
		self.pc += 1;
		return val;
	}


	/// Gets the next 2 byte in the source code as a u16 int.
	fn next_u16(&mut self) -> u16 {
		let bytes = [self.source[self.pc], self.source[self.pc+1]];
		self.pc += 2;
		return u16::from_be_bytes(bytes);
	}


	/// Gets the next 4 byte in the source code as a u32 int.
	fn next_u32(&mut self) -> u32 {
		let bytes = [
			self.source[self.pc], self.source[self.pc+1],
			self.source[self.pc+2], self.source[self.pc+3],
		];
		self.pc += 4;
		return u32::from_be_bytes(bytes);
	}


	// Gets the next 8 byte in the source code as a u64 int.
	fn next_u64(&mut self) -> u64 {
		let bytes = [
			self.source[self.pc], self.source[self.pc+1],
			self.source[self.pc+2], self.source[self.pc+3],
			self.source[self.pc+4], self.source[self.pc+5],
			self.source[self.pc+6], self.source[self.pc+7],
		];
		self.pc += 8;
		return u64::from_be_bytes(bytes);
	}

	
	pub fn run_bytes(&mut self, code:&[u8]) {
		self.source = code.to_vec();
		loop {
			
			match self.next_u8() {
				x if x == Op::End as u8 => {println!("Halting"); break;},

				x if x == Op::LoadConst as u8 => self.exc_load_const_u8(),
				x if x == Op::LoadValU8 as u8 => self.exc_load_val_u8(),
				x if x == Op::LoadValU16 as u8 => self.exc_load_val_u16(),
				x if x == Op::LoadValU32 as u8 => self.exc_load_val_u32(),
				x if x == Op::LoadValU64 as u8 => self.exc_load_val_u64(),

				x if x == Op::LoadMem as u8 => self.exc_load_mem(),
				x if x == Op::StoreMem as u8 => self.exc_store_mem(),

				x if x == Op::Add as u8 => self.exc_math_add(),
				x if x == Op::Sub as u8 => self.exc_math_sub(),
				x if x == Op::Mul as u8 => self.exc_math_mul(),
				x if x == Op::Div as u8 => self.exc_math_div(),
				x if x == Op::Mod as u8 => self.exc_math_mod(),
				x if x == Op::Pow as u8 => self.exc_math_pow(),

				x if x == Op::Lesser as u8 => self.exc_logi_lesser(),
				x if x == Op::Greater as u8 => self.exc_logi_greater(),
				x if x == Op::Equal as u8 => self.exc_logi_equal(),

				x if x == Op::JumpTo as u8 => self.exc_jump_to(),
				x if x == Op::JumpBy as u8 => self.exc_jump_by(),
				x if x == Op::JumpToIf as u8 => self.exc_jump_to_if(),
				x if x == Op::JumpByIf as u8 => self.exc_jump_by_if(),

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
	for word in [
		KEYWORD_VAR,
		KEYWORD_FUNCTION,
		KEYWORD_CLASS,
		KEYWORD_IF,
		KEYWORD_ELSE,
		KEYWORD_ELIF,
	] {
		let mut mismatched = false;
		for (char1, char2) in added_so_far.iter().zip(word.chars()) {
			mismatched = char1 != &char2;
			if mismatched {
				break;
			}
		}
		if !mismatched {
			return true && added_so_far.len() == word.len();
		}
	}

	return false;
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
		['(',] => true,
		[')',] => true,
		['>',] => true,
		['<',] => true,
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
pub fn tokenize<'a>(script:&'a String, rules:&Rules<'a>) -> Vec<QuToken<'a>> {
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
				tokens[curr_token].tk_type = char_type;
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
	let mut tk_type = u8::MAX;
	for rule in rules {
		fits_rule = fits_rule || rule.0(&chars);
		if fits_rule{
			tk_type = rule.1;
			break;
		}
	}

	return (fits_rule, tk_type);
}