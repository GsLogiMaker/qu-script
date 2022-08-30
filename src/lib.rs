
//! TODO: Project level documentation.

#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
#![warn(rustdoc::broken_intra_doc_links)]

use std::{fmt::{self, Display, Debug}, vec};


#[derive(Debug, Clone)]
/// An error enum for the QuParser
pub enum QuErrorParser {
	/// A line has an incorrect indentation level.
	BadIndentation,
	/// A code block and flow statement are on the same line.
	BadIndentation2,
	/// A code block was started, but no code was written to it.
	EmptyCodeBlock,
} impl Display for QuErrorParser {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			QuErrorParser::BadIndentation => {
				return write!(f, "Bad indentation");
			}
			QuErrorParser::BadIndentation2 => {
				return write!(f, "Bad indentation 2");
			}
			QuErrorParser::EmptyCodeBlock => {
				return write!(f, "Empty code block");
			}
			_ => {
				unimplemented!();
			}
		}
	}
}


#[derive(Debug, Clone)]
pub enum QuLeafExpr {
	/// A calculable expression. Contains an operator and two [`QuLeafExpr`]s.
	Equation(u8, Box<QuLeafExpr>, Box<QuLeafExpr>),
	/// A literal int value.
	Int(u64),
	/// A variable name.
	Var(QuToken),
} impl Display for QuLeafExpr {

	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			QuLeafExpr::Equation(op, lft, rht) => {
				let string = format!("{}", op);
				let opstr:&str = string.as_str();
				return write!(f, "Equate({lft} {opstr} {rht})");
			}
			QuLeafExpr::Int(val) => {
				return write!(f, "{val}:Int");
			}
			QuLeafExpr::Var(name) => {
				return write!(f, "{}:Var", name.text);
			}
			_ => {
				return write!(f, "<QuLeafExpr Unimplemented Format>");
			}
		}
	}

}

#[derive(Debug, Clone)]
/// A Qu instruction.
pub enum QuLeaf {
	/// A Block of leafs.
	Block(Vec<QuLeaf>),
	/// An if statement. Contains an assertion statement and a [`Vec`] of
	/// instructions.
	FlowStatement(u8, QuLeafExpr, Box<QuLeaf>),
	/// Prints a register to the console.
	Print(QuLeafExpr),
	/// A variable assignment. Contains a var name and a [`QuLeafExpr`].
	VarAssign(QuToken, QuLeafExpr),
	/// A variable declaration. Contains a var name, type(TODO), and
	/// [`QuLeafExpr`].
	VarDecl(QuToken, Option<QuToken>, Option<QuLeafExpr>),

} impl QuLeaf {

	/// Returns the [`QuLeaf`] as a [`String`] formatted into a tree.
	pub fn tree_fmt(&self, indent:u8) -> String {
		let mut indentstr = "\n".to_string();
		for i in 0..indent {
			indentstr += "  ";
		}
		
		match self {
			QuLeaf::Block(
				body) => {
				let mut bodystr = "".to_string();
				for leaf in body {
					bodystr += &leaf.tree_fmt(indent + 1);
				}
				return format!("BLOCK:{}", bodystr);
			}
			QuLeaf::FlowStatement(
				op, 
				cond, 
				body) => {
					let mut bodystr = body.tree_fmt(indent + 1);
					return format!("{}FLOW {} {} {}", indentstr, op, cond, bodystr);
			}
			QuLeaf::Print(register) => {
				return format!("{}PRINT {}", indentstr, register);
			}
			QuLeaf::VarAssign(
				name, 
				val) => {
				return format!("{}ASSIGN {} = {}", indentstr, name.text, val);
			}
			QuLeaf::VarDecl(
				name, 
				_var_type, 
				_val) => {
				let val_str = match _val {
					Some(val) => format!("{}", val),
					None => "".to_string(),
				};
				// TODO: Add variable declaration type
				return format!("{}VAR {} {} = {}", indentstr, name.text, "", val_str);
			}
		}
	}

} impl Display for QuLeaf {

	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		return write!(f, "{}", self.tree_fmt(0));
	}

}

const FLOW_IF:u8 = 0;
const FLOW_WHILE:u8 = 1;
const FLOW_FOR:u8 = 2;
const FLOW_ELIF:u8 = 3;
const FLOW_ELSE:u8 = 4;

const KEYWORD_CLASS:&str = "cl";
const KEYWORD_ELSE:&str = "else";
const KEYWORD_ELIF:&str = "elif";
const KEYWORD_FUNCTION:&str = "fn";
const KEYWORD_IF:&str = "if";
const KEYWORD_PRINT:&str = "print";
const KEYWORD_TRAIT:&str = "tr";
const KEYWORD_TRAIT_IMPL:&str = "does";
const KEYWORD_VAR:&str = "vl";
const KEYWORD_WHILE:&str = "while";

const OP_ASSIGN_WORD:&str = "=";
const OP_BLOCK_START_WORD:&str = ":";

const TOKEN_TYPE_KEYWORD:u8 = 1;
const TOKEN_TYPE_SYMBOL:u8 = 2;
const TOKEN_TYPE_NUMBER:u8 = 3;
const TOKEN_TYPE_NAME:u8 = 4;

type Rules<'a> = [(&'a dyn Fn(&[char])->bool, u8)];
/// A tuple of a name and a size.
type CommandArg = (i8,);

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


pub struct QuOperation {
	name:String,
	asm_keyword:String,
	args:Vec<CommandArg>,
	id:u8,
} impl QuOperation {
	pub fn new(name:&str, asm_keyword:&str, args:Vec<CommandArg>) -> Self {
		return Self{
			name:name.to_string(),
			asm_keyword:asm_keyword.to_string(),
			args:args,
			id:0,
		};
	}
}

pub struct QuOpLibrary {
	ops:Vec<QuOperation>,
	// Operations
	end:u8,
	load_const:u8,
	load_val_u8:u8,
	load_val_u16:u8,
	load_val_u32:u8,
	load_val_u64:u8,
	load_mem:u8,
	store_mem:u8,
	copy_reg:u8,
	add:u8,
	sub:u8,
	mul:u8,
	div:u8,
	modulate:u8,
	pow:u8,
	lesser:u8,
	greater:u8,
	equal:u8,
	not_equal:u8,
	not:u8,
	jump_to:u8,
	jump_by:u8,
	jump_to_if:u8,
	jump_by_if:u8,
	print:u8,
} impl QuOpLibrary {

	fn new() -> Self {
		let mut obj = Self{
			ops:vec![],
			end:0,
			load_const:0,
			load_val_u8:0,
			load_val_u16:0,
			load_val_u32:0,
			load_val_u64:0,
			load_mem:0,
			store_mem:0,
			copy_reg:0,
			add:0,
			sub:0,
			mul:0,
			div:0,
			modulate:0,
			pow:0,
			lesser:0,
			greater:0,
			equal:0,
			not_equal:0,
			not:0,
			jump_to:0,
			jump_by:0,
			jump_to_if:0,
			jump_by_if:0,
			print:0,
		};
		obj.generate();
		return obj;
	}


	fn add_next(&self, mut op:QuOperation) -> QuOperation {
		op.id = self.ops.len() as u8;
		return op;
	}


	fn generate(&mut self) {
		self.new_op("END", "End", &[]); self.end = self.ops.len() as u8 - 1;
		self.new_op("LDU8", "LoadU8", &[(1,), (1,)]); self.load_val_u8 = self.ops.len() as u8 - 1;
		self.new_op("LDU16", "LoadU16", &[(2,), (1,)]); self.load_val_u16 = self.ops.len() as u8 - 1;
		self.new_op("LDU32", "LoadU32", &[(4,), (1,)]); self.load_val_u32 = self.ops.len() as u8 - 1;
		self.new_op("LDU64", "LoadU64", &[(8,), (1,)]); self.load_val_u64 = self.ops.len() as u8 - 1;
		self.new_op("LDM", "LoadMem", &[(4,), (1,)]); self.load_mem = self.ops.len() as u8 - 1;
		self.new_op("STM", "StoreMem", &[(1,), (4,)]); self.store_mem = self.ops.len() as u8 - 1;
		self.new_op("CPY", "Copy", &[(1,), (1,)]); self.copy_reg = self.ops.len() as u8 - 1;
		self.new_op("ADD", "Add", &[(1,), (1,), (1,)]); self.add = self.ops.len() as u8 - 1;
		self.new_op("SUB", "Subtract", &[(1,), (1,), (1,)]); self.sub = self.ops.len() as u8 - 1;
		self.new_op("MUL", "Multiply", &[(1,), (1,), (1,)]); self.mul = self.ops.len() as u8 - 1;
		self.new_op("DIV", "Divide", &[(1,), (1,), (1,)]); self.div = self.ops.len() as u8 - 1;
		self.new_op("MOD", "Modulate", &[(1,), (1,), (1,)]); self.modulate = self.ops.len() as u8 - 1;
		self.new_op("POW", "Power", &[(1,), (1,), (1,)]); self.pow = self.ops.len() as u8 - 1;
		self.new_op("LES", "Lesser", &[(1,), (1,), (1,)]); self.lesser = self.ops.len() as u8 - 1;
		self.new_op("GRT", "Greater", &[(1,), (1,), (1,)]); self.greater = self.ops.len() as u8 - 1;
		self.new_op("EQ", "Equal", &[(1,), (1,), (1,)]); self.equal = self.ops.len() as u8 - 1;
		self.new_op("NEQ", "NotEqual", &[(1,), (1,), (1,)]); self.not_equal = self.ops.len() as u8 - 1;
		self.new_op("NOT", "Not", &[(1,), (1,)]); self.not = self.ops.len() as u8 - 1;
		self.new_op("JP", "JumpTo", &[(4,)]); self.jump_to = self.ops.len() as u8 - 1;
		self.new_op("JB", "JumpBy", &[(4,)]); self.jump_by = self.ops.len() as u8 - 1;
		self.new_op("JPI", "JumpIf", &[(4,), (1,)]); self.jump_to_if = self.ops.len() as u8 - 1;
		self.new_op("JBI", "JumpByIf", &[(4,), (1,)]); self.jump_by_if = self.ops.len() as u8 - 1;
		self.new_op("PRT", "Print", &[(1,)]); self.print = self.ops.len() as u8 - 1;
		
	}


	fn new_op(&mut self, asm:&str, name:&str, args:&[CommandArg]) {
		self.ops.push(
			self.add_next(QuOperation::new(asm, name, args.to_vec() ))
		);
	}


	fn op_id_from_symbol(&self, symbol:&str) -> u8 {
		return match symbol {
			"+" => self.add,
			"-" => self.sub,
			"*" => self.mul,
			"/" => self.div,
			"**" => self.pow,
			"%" => self.modulate,
			">" => self.greater,
			"<" => self.lesser,
			"==" => self.equal,
			"!=" => self.not_equal,
			_ => panic!("Unknown symbol: {}", symbol),
		};
	}

}

pub struct QuCompiler {
	/// Name, Type, Pointer
	variables:Vec<(String, usize, u8)>,
	stack_layers:Vec<u8>,
	stack_idx:u8,
	ops:QuOpLibrary,
} impl QuCompiler {

	pub fn new() -> Self {
		return Self{
			variables:vec![],
			stack_layers:vec![],
			stack_idx:0,
			ops:QuOpLibrary::new(),
		};
	}


	/// Compiles a [QuLeafExpr] into bytecode.
	fn cmp_expr(&mut self, leaf:&QuLeafExpr, output_reg:u8) -> Vec<u8> {
		return match leaf {
			QuLeafExpr::Equation(
				op,
				left,
				right
			) => self.cmp_expr_math(*op, &**left, &**right, output_reg),
			QuLeafExpr::Int(val)
				=> self.cmp_expr_int(*val, output_reg),
			QuLeafExpr::Var(token)
				=> self.cmp_expr_val(token, output_reg),
		};
	}


	/// Compiles a [QuLeafExpr] into bytecode.
	fn cmp_expr_math(&mut self, op:u8, left:&QuLeafExpr, right:&QuLeafExpr,
			output_reg:u8) -> Vec<u8> {
		
		let right_reg = self.stack_reserve();

		let mut code:Vec<u8> = vec![];
		
		// Compile right expression
		let mut rgh_bytes =
			self.cmp_expr(right, right_reg);
		code.append(&mut rgh_bytes);
		// Compile left expression
		let mut lft_bytes =
			self.cmp_expr(left, output_reg);
		code.append(&mut lft_bytes);

		// Compile expression calculation
		code.append(&mut vec![op]);
		code.append(&mut vec![output_reg]);
		code.append(&mut vec![right_reg]);
		code.append(&mut vec![output_reg]);

		return code;
	}


	/// Compiles a [QuLeafExpr] into bytecode that loads an integer into a
	/// register.
	fn cmp_expr_int(&mut self, val:u64, output_reg:u8) -> Vec<u8> {
		// TODO: Support other int sizes

		let mut code = vec![];
		// The vm operation
		code.push(self.ops.load_val_u8);
		// The number as bytes
		code.append(&mut (val as u8).to_be_bytes().to_vec());
		// The register to load into
		code.push(output_reg);

		return code;
	}


	/// Compiles a [QuLeafExpr] into bytecode that copies one register into a
	/// another register.
	fn cmp_expr_val(&mut self, token:&QuToken, output_reg:u8) -> Vec<u8> {
		let mut code = Vec::with_capacity(3);
		// The vm operation
		code.push(self.ops.copy_reg);
		// The register to copy the variable value from
		let var_reg
			= self.get_var_register(token.text.as_str()).unwrap();
		code.append(&mut (var_reg as u8).to_be_bytes().to_vec());
		// The register to copy the variable value into
		code.push(output_reg);

		return code;
	}


	/// Compiles code to calculate an if expression and jump code. Returns the
	/// resulting code.
	fn cmp_flow_if(&mut self, expr:&QuLeafExpr, leafs:&Box<QuLeaf>
	) -> Vec<u8> {
		self.stack_frame_push();
		let if_expr_reg = self.stack_reserve();

		// Compile expression and code block
		let mut expr_code
			= self.cmp_expr(expr, if_expr_reg);
		let mut block_code = self.cmp_block(leafs);

		// Compile if's jump
		let mut jump_code = Vec::with_capacity(7);
		let jump_assert_reg = self.stack_reserve();
		// Negate expression
		jump_code.push(self.ops.not);
		jump_code.push(if_expr_reg);
		jump_code.push(jump_assert_reg);
		// Jump instruction
		jump_code.push(self.ops.jump_by_if);
		jump_code.push(jump_assert_reg);
		jump_code.append(&mut (block_code.len() as i32).to_be_bytes().to_vec());

		// Compile code together
		let mut code = Vec::with_capacity(
				expr_code.len() + jump_code.len() + block_code.len());
		code.append(&mut expr_code);
		code.append(&mut jump_code);
		code.append(&mut block_code);

		self.stack_frame_pop();

		return code;
	}


	/// Compiles code to calculate a while expression and jump code. Returns the
	/// resulting code.
	fn cmp_flow_while(&mut self, expr:&QuLeafExpr, leafs:&Box<QuLeaf>
	) -> Vec<u8> {
		// Structure
		// - Jump to Loopback Expression
		// - Code Block
		// - Loopback Expression
		// - Loopback to Code Block
		self.stack_frame_push();
		let while_expr_reg = self.stack_reserve();

		// Compile expression and code block
		let mut expr_code
			= self.cmp_expr(expr, while_expr_reg);
		let mut block_code = self.cmp_block(leafs);

		// Compile while's prejump
		let prejump_travel = block_code.len() as i32;
		let mut prejump_code = vec![];
		prejump_code.push(self.ops.jump_by);
		prejump_code.append(&mut prejump_travel.to_be_bytes().to_vec());

		// Compile while's loopback
		let loopback_travel = 
				-((block_code.len() + expr_code.len() + 6) as i32);
		let mut loopback_code = vec![];
		loopback_code.push(self.ops.jump_by_if);
		loopback_code.append(&mut loopback_travel.to_be_bytes().to_vec());
		loopback_code.push(while_expr_reg);

		// Compile code together
		let mut code = vec![];
		code.append(&mut prejump_code);
		code.append(&mut block_code);
		code.append(&mut expr_code);
		code.append(&mut loopback_code);

		self.stack_frame_pop();
		
		return code;
	}


	fn cmp_leaf(&mut self, leaf:&QuLeaf) -> Vec<u8> {
		return match leaf {
			QuLeaf::Block(leafs) => {
				let mut code = vec![];
				for block_leaf in leafs {
					code.append(&mut self.cmp_leaf(block_leaf));
				}
				return code;
			},
			QuLeaf::FlowStatement(
				flow_type,
				expr,
				statements,
			) => {
				match *flow_type{
					FLOW_IF => {
						return self.cmp_flow_if(expr, statements);
					},
					FLOW_WHILE => {
						return self.cmp_flow_while(expr, statements);
					}
					_ => unimplemented!(),
				}
			}
			QuLeaf::Print(leaf_expr) => {
				let mut code = vec![];
				let print_reg = self.stack_reserve();
				code.append(&mut self.cmp_expr(leaf_expr, print_reg));
				code.push(self.ops.print);
				code.push(print_reg);
				return code;
			},
			QuLeaf::VarDecl(
					name_tk,
					_type_tk,
					value_leaf
			) => {
				return self.cmp_var_decl(
					name_tk, 0, value_leaf);
			}

			QuLeaf::VarAssign(
					name_rk,
					value_leaf
			) => {
				return  self.cmp_var_assign(name_rk, value_leaf);
			}

			_ => {unimplemented!()}
		}
	}


	/// Compiles code to assign a value to a variable. Returns the resulting
	/// code.
	fn cmp_var_assign(&mut self,
			name_tk:&QuToken, value_leaf:&QuLeafExpr) -> Vec<u8> {
		// Get variable register
		let var_reg= self.get_var_register(
				&name_tk.text
				).expect(format!(
				"No variable with name {} has been defined",
				&name_tk.text).as_str());
		
		// Compile assignment to expression
		return self.cmp_expr(value_leaf, var_reg);
	}


	fn cmp_var_decl(&mut self, name:&QuToken, var_type:usize,
			val_leaf_op:&Option<QuLeafExpr>) -> Vec<u8> {
		// Create variable
		let var_reg = self.stack_reserve();
		self.variables.push(
			(name.text.clone(), var_type, var_reg)
		);

		// Compile variable assignment
		return match val_leaf_op {
			// Compile variable value
			Some(val_leaf)
				=> self.cmp_expr(&val_leaf, var_reg),

			// No default value, compile fallback to zero
			None => {
				// TODO: Support u64
				let mut code = Vec::with_capacity(3);
				code.push(self.ops.load_val_u8); 
				code.push(0);
				code.push(var_reg);
				code
			},
		};
	}


	fn cmp_block(&mut self, leaf:&QuLeaf) -> Vec<u8> {
		self.stack_frame_push();

		let mut code:Vec<u8> = self.cmp_leaf(leaf);
		
		self.stack_frame_pop();
		return code;
	}


	pub fn compile(&mut self, leafs:&QuLeaf) -> Vec<u8> {
		let mut code:Vec<u8> = self.cmp_block(leafs);
		code.push(self.ops.end);

		return code;
	}


	/// Gets the pointer to a variable by the variable's name.
	fn get_var_register(&self, var_name:&str) -> Option<u8> {
		for (name_, _type_, pointer_) in &self.variables {
			if name_ == var_name {
				return Some(*pointer_);
			}
		}
		return None;
	}


	/// Returns and increments the stack pointer.
	fn stack_reserve(&mut self) -> u8 {
		let x = self.stack_idx;
		self.stack_idx += 1;
		return x;
	}


	/// Closes the current stack frame.
	fn stack_frame_pop(&mut self) {
		self.stack_idx = self.stack_layers.pop().unwrap();
	}


	/// Starts a new stack frame.
	fn stack_frame_push(&mut self) {
		self.stack_layers.push(self.stack_idx);
	}

}


pub struct QuParser<'a> {
	indent:u8,
	line:usize,
	tk_idx:usize,
	tk_stack:Vec<usize>,
	tokens:&'a Vec<QuToken>,
	script:&'a String,
	ops:QuOpLibrary,

} impl<'a> QuParser<'a> {

	pub fn new(tokens:&'a mut Vec<QuToken>, script:&'a String) -> Self {
		tokens.push(
			QuToken::new(tokens.len() as u64, tokens.len() as u64, 
			0, 0, 0, u8::MAX)
		);
		return QuParser {
			indent:u8::MAX,
			line:0,
			tk_idx:0,
			tk_stack:vec![],
			tokens:tokens,
			script:script,
			ops:QuOpLibrary::new(),
		}
	}


	/// Returns a Qu error message as a [`String`]
	fn err_msg_make(&self, kind:QuErrorParser, tk:&QuToken) -> String {
		// Line numbers
		let line_nm_pre_pre = (tk.row as usize).saturating_sub(1);
		let line_nm_pre = (tk.row as usize).saturating_sub(0);
		let line_nm = (tk.row as usize).saturating_add(1);
		let line_nm_post = (tk.row as usize).saturating_add(2);
		let line_nm_post_post = (tk.row as usize).saturating_add(3);

		// Line text
		let mut script_lines = self.script.split("\n");
		let line_pre_pre = if tk.row > 1 {
			script_lines.nth(line_nm_pre_pre-1).unwrap_or("").to_string()
		} else {
			"".to_string()
		};
		let line_pre = if tk.row > 0 {
			script_lines.next().unwrap_or("").to_string()
		} else {
			"".to_string()
		};
		let line = script_lines.next().unwrap_or("");
		let line_post =
				script_lines.next().unwrap_or("");
		let line_post_post =
				script_lines.next().unwrap_or("");

		// Build code view
		let code_view = format!(
"    {:0>4}:{}\n    {:0>4}:{}\n >> {:0>4}:{}\n    {:0>4}:{}\n    {:0>4}:{}",
			line_nm_pre_pre,
			line_pre_pre,
			line_nm_pre,
			line_pre,
			line_nm,
			line,
			line_nm_post,
			line_post,
			line_nm_post_post,
			line_post_post,
		);

		// Build error message
		let msg =  format!("ERROR on line {row}, col {col}: {kind}\n{script}",
				row=tk.row+1, col=tk._col, script=code_view);
		return msg;
		
	}


	fn ck_code_block(&mut self) -> Result<Option<Vec<QuLeaf>>, String> {
		let mut leafs = vec![];

		while self.tk_idx < self.tokens.len()-1 {
			// Variable declaration
			match self.ck_var_decl() {
				Ok(data_opt) => {
					if let Some(data) = data_opt {
						leafs.push(data);
						continue;
					}
				}
				Err(msg) => {
					return Err(format!("{}", msg));
				}
			}

			// Variable assignment
			match self.ck_var_assign() {
				Ok(data_opt) => {
					if let Some(data) = data_opt {
						leafs.push(data);
						continue;
					}
				}
				Err(msg) => {
					return Err(format!("{}", msg));
				}
			}

			// If Statement
			match self.ck_flow_if() {
				Ok(data_opt) => {
					if let Some(data) = data_opt {
						leafs.push(data);
						continue;
					}
				}
				Err(msg) => {
					return Err(format!("{}", msg));
				}
			}

			// while Statement
			match self.ck_flow_while() {
				Ok(data_opt) => {
					if let Some(data) = data_opt {
						leafs.push(data);
						continue;
					}
				}
				Err(msg) => {
					return Err(format!("{}", msg));
				}
			}

			// Print Statement
			match self.ch_print() {
				Ok(data_opt) => {
					if let Some(data) = data_opt {
						leafs.push(data);
						continue;
					}
				}
				Err(msg) => {
					return Err(format!("{}", msg));
				}
			}

			// End block
			break;
		}

		if leafs.len() == 0 {
			return Err(self.err_msg_make(
						QuErrorParser::EmptyCodeBlock,
						&self.tokens[self.tk_idx-1]));
		}

		return Ok(Some(leafs));
	}


	fn ck_code_scope(&mut self) -> Result<Option<Vec<QuLeaf>>, String> {
		self.tk_push();

		// Check operator
		let start_tk = self.tk_next()
				.expect("Improper indentation TODO: Bette message");
		if start_tk != OP_BLOCK_START_WORD {
			self.tk_pop();
			return Ok(None);
		}

		self.indent += 1;
		let block_data = self.ck_code_block();
		self.indent -= 1;
		return block_data;
	}


	fn ck_flow(&mut self, token_type:u8) -> Result<Option<QuLeaf>, String> {
		match self.utl_statement_start() {
			Ok(opt) => (
				match opt {
					Some(_) => {/*pass*/},
					None => return Ok(None),
				}
			),
			Err(msg) => return Err(msg),
		}

		self.tk_push();

		// Check keyword
		let keyword = match token_type {
			FLOW_IF => "if",
			FLOW_WHILE => "while",
			_ => unimplemented!(),
		};
		let keyword_tk = self.tk_next()
				.expect("Improper indentation TODO: Bette message");
		if keyword_tk != keyword {
			self.tk_pop();
			return Ok(None);
		}

		// Check expression
		let expr_data = self.ck_expr();
		if let Some(expr) = expr_data {
			// Check scope
			let scope_data_opt = match self.ck_code_scope() {
				Ok(data_opt) => data_opt,
				Err(msg) => {return Err(msg);},
			};
			if let Some(scope_data) = scope_data_opt {
				// Success!
				return Ok(Some(
					QuLeaf::FlowStatement(token_type, expr, Box::new(QuLeaf::Block(scope_data)))
				));
			}

			self.tk_pop();
			return Err("If statement expected scope 'TODO: Better message'"
					.to_string());
		}

		self.tk_pop();
		return Err("If statement expected expression 'TODO: Better message'"
				.to_string());
	}


	fn ck_flow_if(&mut self) -> Result<Option<QuLeaf>, String> {
		return self.ck_flow(FLOW_IF);
	}


	fn ck_flow_while(&mut self) -> Result<Option<QuLeaf>, String> {
		return self.ck_flow(FLOW_WHILE);
	}


	fn ch_print(&mut self) -> Result<Option<QuLeaf>, String> {
		match self.utl_statement_start() {
			Ok(opt) => (
				match opt {
					Some(_) => {/*pass*/},
					None => return Ok(None),
				}
			),
			Err(msg) => return Err(msg),
		}

		// Match keyword
		let keyword_tk = self.tk_spy(0);
		if keyword_tk != KEYWORD_PRINT {
			return Ok(None);
		}
		self.tk_next().expect("Improper indentation TODO:Better msg");

		// Match register
		let reg_tk = self.ck_value().expect("Print needs number TODO: Better msg");

		return Ok(Some(QuLeaf::Print(reg_tk)));
	}


	/// Matches tokens to a variable assignment.
	fn ck_var_assign(&mut self) -> Result<Option<QuLeaf>, String> {
		match self.utl_statement_start() {
			Ok(opt) => (
				match opt {
					Some(_) => {/*pass*/},
					None => return Ok(None),
				}
			),
			Err(msg) => return Err(msg),
		}

		self.tk_push();

		// Match variable name
		let name_data = self.ck_var_name();
		if let None = name_data {
			self.tk_pop();
			return Ok(None);
		}
		let name_tk = name_data.unwrap();
		
		
		// Match assign operator
		let assign_op_tk = self.tk_next()
				.expect("Improper indentation TODO: Bette message");
		if assign_op_tk != OP_ASSIGN_WORD {
			self.tk_pop();
			return Ok(None);
		}

		// Match expression
		let expr_data = self.ck_expr();
		if let None = expr_data {
			self.tk_pop();
			return Err(
				"Expected variable to be assigned to expr TODO:Better Msg"
				.to_string());
		}
		let expr_leaf = expr_data.unwrap();

		return Ok(Some(
			QuLeaf::VarAssign(name_tk, expr_leaf)
		));
	}


	fn ck_var_decl(&mut self) -> Result<Option<QuLeaf>, String> {
		match self.utl_statement_start() {
			Ok(opt) => (
				match opt {
					Some(_) => {/*pass*/},
					None => return Ok(None),
				}
			),
			Err(msg) => return Err(msg),
		}
		
		// Match keyword
		let keyword_tk = self.tk_spy(0);
		if keyword_tk != KEYWORD_VAR {
			return Ok(None);
		}
		self.tk_next().expect("Improper indentation TODO:Better msg");

		// Match variable name
		let name_data = self.ck_var_name();
		if let None = name_data {
			return Err(
				"Token after 'var' does not match a name. 'TODO:Better msg'"
				.to_string());
		}
		let name_tk = name_data.unwrap();

		// Match variable type
		let type_tk_opt = self.ck_type_name();

		// Match assign operator
		let keyword_tk = self.tk_spy(0);
		let mut assign_leaf_opt = None;
		if keyword_tk == OP_ASSIGN_WORD {
			self.tk_next()
					.expect("Improper indentation TODO:Better msg");
			assign_leaf_opt = self.ck_expr();
			if let None = assign_leaf_opt {
				return Err("Expected expression after '='. TODO:Better msg".to_string());
			}
		}
		
		return Ok(Some(QuLeaf::VarDecl(
			name_tk,
			type_tk_opt,
			assign_leaf_opt,
		)));
	}


	fn ck_var_name(&mut self) -> Option<QuToken> {
		let tk = self.tk_spy(0);

		if tk.tk_type != TOKEN_TYPE_NAME {
			return None;
		}
		let tk = tk.clone();

		self.tk_next().expect("Improper indentation TODO: Bette message");
		return Some(tk);
	}


	/// Checks for an expression
	fn ck_expr(&mut self) -> Option<QuLeafExpr> {
		if let Some(check) = self.ck_op_les() {
			return Some(check);
		}

		return None;
	}


	fn ck_op_les(&mut self) -> Option<QuLeafExpr> {
		return self.ck_operation("<", &Self::ck_op_grt);
	}


	fn ck_op_grt(&mut self) -> Option<QuLeafExpr> {
		return self.ck_operation(">", &Self::ck_op_eql);
	}


	fn ck_op_eql(&mut self) -> Option<QuLeafExpr> {
		return self.ck_operation("==", &Self::ck_op_not_eql);
	}


	fn ck_op_not_eql(&mut self) -> Option<QuLeafExpr> {
		return self.ck_operation("!=", &Self::ck_op_sub);
	}


	fn ck_op_sub(&mut self) -> Option<QuLeafExpr> {
		return self.ck_operation("-", &Self::ck_op_add);
	}


	fn ck_op_add(&mut self) -> Option<QuLeafExpr> {
		return self.ck_operation("+", &Self::ck_op_div);
	}


	fn ck_op_div(&mut self) -> Option<QuLeafExpr> {
		return self.ck_operation("/", &Self::ck_op_mul);
	}


	fn ck_op_mul(&mut self) -> Option<QuLeafExpr> {
		return self.ck_operation("*", &Self::ck_op_paren_expr);
	}


	fn ck_op_paren_expr(&mut self) -> Option<QuLeafExpr> {
		self.tk_push();

		let tk = self.tk_next()
				.expect("Improper indentation TODO: Bette message");
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

		let closing_tk = self.tk_next()
				.expect("Improper indentation TODO: Bette message");
		if closing_tk != ")" {
			panic!("Parenthesis expression ended without closing parenthesis.");
		}

		return data;
	}


	/// Returns a QuLeaf
	fn ck_operation(
			&mut self, operator:&str,
			next:&dyn Fn(&mut Self)->Option<QuLeafExpr>,
			) -> Option<QuLeafExpr> {

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
		self.tk_next().expect("Improper indentation TODO: Bette message");

		// Check right side for expression
		let data_r = self.ck_operation(operator, next);//next(self);
		if let None = data_r {
			self.tk_pop();
			return None;
		}
		let data_r = data_r.unwrap();

		return Some(
			QuLeafExpr::Equation(
				self.ops.op_id_from_symbol(operator),
				Box::new(data_l),
				Box::new(data_r)
			)
		);
	}


	fn ck_type_name(&mut self) -> Option<QuToken> {
		return self.ck_var_name();
	}


	fn ck_value(&mut self) -> Option<QuLeafExpr> {
		self.tk_push();
		let tk = self.tk_next()
				.expect("Improper indentation TODO: Bette message");
		return match tk.text.parse::<u64>() {
			Ok(x) => Some(QuLeafExpr::Int(x)),
			Err(_) => {
				self.tk_pop();
				match self.ck_var_name() {
					Some(data) => Some(QuLeafExpr::Var(data)),
					None => None,
				}
			},
		};
	}


	pub fn parse(&mut self) -> Vec<QuLeaf> {
		self.tk_idx = 0;
		self.line = 0;
		self.indent = u8::MAX;

		let res = self.ck_code_block();
		if let Err(msg) = res {
			println!("{}", msg);
			panic!("^^^ Encountered Qu error ^^^");
		}
		else if let Ok(data_opt) = res{
			return data_opt.unwrap();
		}
		unreachable!();
	}


	/// A utility function. Used whenever starting to parse a statement.
	fn utl_statement_start(&mut self) -> Result<Option<()>, String> {
		let tk = self.tk_spy(0);
		let tk_row = tk.row as usize;
		let tk_indent = tk.indent as u8;

		if self.indent == u8::MAX {
			self.indent = tk_indent;
		}
		else if self.indent != tk_indent {

			if tk_indent < self.indent && tk_row != self.line {
				return Ok(None);
			}

			if self.line == tk_row {
				let tk = self.tokens[self.tk_idx].clone();
				return Err(self.err_msg_make(
					QuErrorParser::BadIndentation2, 
					&tk));
			}

			let tk = self.tokens[self.tk_idx-0].clone();
			self.line = tk_row;
			return Err(self.err_msg_make(
					QuErrorParser::BadIndentation, 
					&tk));
		}
		
		self.line = tk_row;
		return Ok(Some(()));
	}


	/// Returns a [`Result<&QuToken, &QuToken>`] and increments the token index.
	/// 
	/// Returns [`Err`] if the token breaks indentation rules, Although the
	/// token can still be accessed from the [`Err`] if the indentation rules
	/// need to be ignored.
	/// 
	/// For a [`QuToken`] to follow the indentation rules it must be on
	/// the same line as its statement, unless the token is indented two times
	/// more than the statement.
	/// 
	/// Example:
	/// 
	/// ``` qu
	/// # Allowed
	/// vl counter = 1 + 2
	/// 
	/// # Allowed
	/// vl counter = 1
	/// 		+ 2
	/// 
	/// # Not allowed
	/// vl counter = 1
	/// 	+ 2
	/// 
	/// # Not allowed
	/// vl counter = 1
	/// + 2
	/// ```
	fn tk_next(&mut self) -> Result<&QuToken, &QuToken> {
		let tk = &self.tokens[self.tk_idx];

		// Check for proper indentation
		if tk.row != self.line as u64 {
			if tk.indent != self.indent+2 {
				return Err(tk);
			}
		}

		self.tk_idx += 1;
		return Ok(tk);
	}


	/// Returns to a previously saved token index.
	fn tk_pop(&mut self) {
		self.tk_idx = self.tk_stack.pop().unwrap();
	}


	/// Saves a return point for if a check fails.
	fn tk_push(&mut self) {
		self.tk_stack.push(self.tk_idx);
	}


	/// Returns a &[`QuToken`] relative to the current token index.
	/// 
	/// This function will not check if the token follows indentation rules.
	fn tk_spy(&mut self, at:usize) -> &QuToken {
		if self.tk_idx+at >= self.tokens.len() {
			return &self.tokens[self.tokens.len()-1];
		}
		return &self.tokens[self.tk_idx+at];
	}

}


/// A slice of a script file with information on the row, column, and indent of
/// the slice.
pub struct QuToken {
	pub begin:u64,
	pub end:u64,
	pub row:u64,
	pub _col:u64,
	pub indent:u8,
	pub text:String,
	pub tk_type:u8,

} impl QuToken {

	/// Makes a new [`Token`].
	pub fn new(
			begin:u64, end:u64, row:u64, col:u64, indent:u8,
			varient:u8,) -> QuToken {
		return QuToken{
			begin,
			end,
			row,
			_col:col,
			indent,
			text:String::new(),
			tk_type:varient
		};
	}

	pub fn text(&self, source:&str) -> String {
		let mut text = String::new();
		if source.len() > 0 {
			text = source[self.begin as usize..=self.end as usize].to_string();
		}
		return text;
	}

} impl Clone for QuToken {
    fn clone(&self) -> Self {
        Self { 
			begin:self.begin.clone(),
			end:self.end.clone(),
			row:self.row.clone(),
			_col:self._col.clone(),
			indent:self.indent.clone(),
			text:self.text.clone(),
			tk_type:self.tk_type.clone()
		}
    }
} impl Display for QuToken {
	
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		return write!(
				f, "<'{}' row:{}  indent:{}>",
				self.text,
				self.row,
				self.indent,);
	}
} impl Debug for QuToken {
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
} impl PartialEq for QuToken {
	fn eq(&self, other:&Self) -> bool {
		if self.tk_type == u8::MAX {
			return false;
		}
		return self.text == other.text;
	}
} impl PartialEq<str> for QuToken {
	fn eq(&self, other:&str) -> bool {
		if self.tk_type == u8::MAX {
			return false;
		}
		return &self.text == other;
	}
} impl<'a> PartialEq<&str> for QuToken {
	fn eq(&self, other:&&str) -> bool {
		if self.tk_type == u8::MAX {
			return false;
		}
		return &self.text == *other;
	}
} impl PartialEq<String> for QuToken {
	fn eq(&self, other:&String) -> bool {
		if self.tk_type == u8::MAX {
			return false;
		}
		return &self.text == other;
	}
}


pub struct QuVm {
	/// The registers of the VM.
	pub registers:[u64;256],
	/// Keeps track of which instruction to execute next.
	pc:usize,
	/// The source bytecode being run.
	pub source:Vec<u8>,
	/// The memory of the VM.
	pub mem:Vec<u64>,
	/// The library of available operations.
	pub op_lib:QuOpLibrary,

} impl QuVm {

	/// Makes a new [`Vm`].
	pub fn new() -> Self {
		let vm = QuVm { 
			pc:0,
			source:vec![],
			registers:[0;256],
			mem:vec![],
			op_lib:QuOpLibrary::new(),
		};

		return vm;
	}


	/// Converts byte code to human readable instructions.
	pub fn code_to_asm(&mut self, code:&Vec<u8>) -> String {
		let mut asm = String::new();
		
		let mut i = 0;
		while i < code.len() {
			let op_code = code[i];
			// HACK: Skip commands if they exceed the ops length.
			if op_code as usize >= self.op_lib.ops.len() {
				i += 1;
				// Add error text
				asm.push_str(format!("\n{:.>8}-{:.<8} {}",
					i, i, "INVALID OPERATION").as_str());
				continue;
			}
			let op
					= &self.op_lib.ops[op_code as usize];
			assert!(op.id == op_code);

			// Add command text
			asm.push_str(format!("\n{:.>8}-{:.<8} {}",
					i, i+op.args.len(), op.asm_keyword).as_str());

			// Add parameter text
			for (size,) in op.args.iter() {
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
				asm.push_str(format!(" {}{}", "", val).as_str());
			}
			i += 1;
		}

		return asm;
	}


	fn exc_copy_reg(&mut self) {
		let from_reg = self.next_u8() as usize;
		let to_reg = self.next_u8() as usize;
		self.registers[to_reg] = self.registers[from_reg];
	}


	fn exc_jump_by(&mut self) {
		let val_by = self.next_u32() as usize;
		// Add
		if val_by as isize > 0 {
			self.pc += val_by;
		// Subtract
		} else {
			self.pc = self.pc.wrapping_add(val_by);
		}
		
	}


	fn exc_jump_by_if(&mut self) {
		let val_by = self.next_u32() as i32;
		let rg_if = self.next_u8() as usize;
		if self.registers[rg_if] as i64 > 0 {
			// Add
			if val_by > 0 {
				self.pc += val_by as usize;
			// Subtract
			} else {
				self.pc -= val_by.abs() as usize;
			}
		}
	}


	fn exc_jump_to(&mut self) {
		unimplemented!()
	}


	fn exc_jump_to_if(&mut self) {
		let val_to = self.next_u32() as usize;
		let rg_if = self.next_u8() as usize;
		if self.registers[rg_if] > 0 {
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


	fn exc_print(&mut self) {
		let read_from_reg = self.next_u8() as usize;
		let val = self.registers[read_from_reg];
		println!("Qu Print: {}", val);
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


	fn exc_logi_not_equal(&mut self) {
		let rg_left = self.next_u8() as usize;
		let rg_right = self.next_u8() as usize;
		let rg_result = self.next_u8() as usize;
		self.registers[rg_result] = 
				(self.registers[rg_left] != self.registers[rg_right]) as u64;
	}


	fn exc_logi_not(&mut self) {
		let rg_left = self.next_u8() as usize;
		let rg_result = self.next_u8() as usize;
		let x = self.registers[rg_left];
		self.registers[rg_result] = 
				(x*0 == x) as u64;
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
		let ops = QuOpLibrary::new();
		self.source = code.to_vec();
		loop {
			
			match self.next_u8() {
				x if x == ops.end as u8 => {break;},

				x if x == ops.load_const => self.exc_load_const_u8(),
				x if x == ops.load_val_u8 => self.exc_load_val_u8(),
				x if x == ops.load_val_u16 => self.exc_load_val_u16(),
				x if x == ops.load_val_u32 => self.exc_load_val_u32(),
				x if x == ops.load_val_u64 => self.exc_load_val_u64(),
				x if x == ops.load_mem => self.exc_load_mem(),
				x if x == ops.store_mem => self.exc_store_mem(),
				x if x == ops.copy_reg => self.exc_copy_reg(),
				x if x == ops.add => self.exc_math_add(),
				x if x == ops.sub => self.exc_math_sub(),
				x if x == ops.mul => self.exc_math_mul(),
				x if x == ops.div => self.exc_math_div(),
				x if x == ops.modulate => self.exc_math_mod(),
				x if x == ops.pow => self.exc_math_pow(),
				x if x == ops.lesser => self.exc_logi_lesser(),
				x if x == ops.greater => self.exc_logi_greater(),
				x if x == ops.equal => self.exc_logi_equal(),
				x if x == ops.not_equal => self.exc_logi_not_equal(),
				x if x == ops.not => self.exc_logi_not(),
				x if x == ops.jump_to => self.exc_jump_to(),
				x if x == ops.jump_by => self.exc_jump_by(),
				x if x == ops.jump_to_if => self.exc_jump_to_if(),
				x if x == ops.jump_by_if => self.exc_jump_by_if(),
				x if x == ops.print => self.exc_print(),

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
		['*',] => true,
		['/',] => true,
		['=',] => true,
		['!',] => true,
		['>',] => true,
		['<',] => true,
		['+',] => true,
		['-',] => true,
		['%',] => true,
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
		['&',] => true,
		['|',] => true,
		['^',] => true,
		[':',] => true,
		[',',] => true,
		['(',] => true,
		[')',] => true,
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
pub fn tokenize<'a>(script:&'a String, rules:&Rules<'a>) -> Vec<QuToken> {
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
					));
				}
				tokens[curr_token].end = idx as u64;
				tokens[curr_token].tk_type = char_type;

				if idx+1 == script.len() {
					tokens[curr_token].text
							= tokens[curr_token].text(script);
				}
				break;
				
			} else if added_so_far.len() == 1 {
				added_so_far.clear();
				break;
			}else {
				if curr_token+1 == tokens.len() {
					tokens[curr_token].text
							= tokens[curr_token].text(script);
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