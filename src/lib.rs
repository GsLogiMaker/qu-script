
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
	NotEqual,
	Not,
	JumpTo,
	JumpBy,
	JumpToIf,
	JumpByIf,
} impl From<&str> for Op {

	fn from(x:&str) -> Self {
		match x.to_uppercase().as_str() {
			"ADD" => {panic!("DEPRICATED")},
			"SUB" => {panic!("DEPRICATED")},
			"MUL" => {panic!("DEPRICATED")},
			"DIV" => {panic!("DEPRICATED")},
			"MOD" => {panic!("DEPRICATED")},
			"POW" => {panic!("DEPRICATED")},
			"GRT" => {panic!("DEPRICATED")},
			"LES" => {panic!("DEPRICATED")},
			"EQL" => {panic!("DEPRICATED")},

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
			
			"!="  => Op::NotEqual,
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
pub enum QuLeafExpr {
	/// A calculable expression. Contains an operator and two [`QuLeafExpr`]s.
	Equation(Op, Box<QuLeafExpr>, Box<QuLeafExpr>),
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
pub enum QuLeaf {
	/// An if statement. Contains an assertion statement and a [`Vec`] of
	/// instructions.
	IfStatement(QuLeafExpr, Vec<QuLeaf>),
	/// A variable assignment. Contains a var name and a [`QuLeafExpr`].
	VarAssign(QuToken, QuLeafExpr),
	/// A variable declaration. Contains a var name, type(TODO), and
	/// [`QuLeafExpr`].
	VarDecl(QuToken, Option<QuToken>, Option<QuLeafExpr>),

} impl Display for QuLeaf {

	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			QuLeaf::IfStatement(
					expr,
					block) => {
				return write!(f, "(If {}, {:?})", expr, block);
			}
			QuLeaf::VarAssign(
					name,
					val) => {
				return write!(f, "VarAssign({}, {})", name.text, val);
			}
			QuLeaf::VarDecl(
					name, 
					var_type, 
					val) => {
				return write!(f, "VarDecl({}, {}, {})", name.text, "todo", "todo");
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
const OP_BLOCK_START_WORD:&str = ":";

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
	/// Name, Type, Pointer
	variables:Vec<(String, usize, usize)>,
	stack_ptr:usize,
} impl QuCompiler {

	pub fn new() -> Self {
		return Self{
			reserved_vregs:[false;16],
			variables:vec![],
			stack_ptr:0,
		};
	}


	fn cmp_expr(&mut self, leaf:&QuLeafExpr) -> (Vec<u8>, u8) {
		return match leaf {
			QuLeafExpr::Equation(
				op,
				left,
				right
			) => self.cmp_expr_equate(op, &**left, &**right),
			QuLeafExpr::Int(val) => self.cmp_expr_int(*val),
			QuLeafExpr::Var(token) => self.cmp_expr_val(token),
		};
	}


	/// Calculates an expression and returns the virtual register to the result.
	fn cmp_expr_equate(&mut self, op:&Op, left:&QuLeafExpr, right:&QuLeafExpr)
			-> (Vec<u8>, u8) {
		
		// Right hand value code
		let mut rgh_data = self.cmp_expr(right);
		let mut lft_data = self.cmp_expr(left);

		let mut code:Vec<u8> = Vec::with_capacity(
				lft_data.0.len()+rgh_data.0.len() + 4);
		let to_reg = self.reg_reserve();
		code.append(&mut rgh_data.0);
		code.append(&mut lft_data.0);
		code.append(&mut vec![
			*op as u8, lft_data.1, rgh_data.1, to_reg
		]);

		// Free registers
		self.reg_free(lft_data.1);
		self.reg_free(rgh_data.1);

		return (code, to_reg);
	}


	fn cmp_expr_int(&mut self, val:u64) -> (Vec<u8>, u8) {
		let reg = self.reg_reserve();
		let mut code = Vec::with_capacity(10);
		// TODO: Support other int sizes
		code.push(Op::LoadValU8 as u8);
		code.append(&mut (val as u8).to_be_bytes().to_vec());
		code.push(reg);
		return (code, reg);
	}


	fn cmp_expr_val(&mut self, token:&QuToken) -> (Vec<u8>, u8) {
		let var_ptr
			= self.get_var_pointer(
				token.text.as_str()).unwrap();

		let reg = self.reg_reserve();
		let mut code = Vec::with_capacity(10);
		code.push(Op::LoadMem as u8);
		code.append(&mut (var_ptr as u32).to_be_bytes().to_vec());
		code.push(reg);
		return (code, reg);
	}


	fn cmp_if_statement(&mut self, expr:&QuLeafExpr, leafs:&Vec<QuLeaf>
	) -> Vec<u8> {
		let (mut expr_code, expr_reg) = self.cmp_expr(expr);
		let mut block_code = self.cmp_vec(leafs);

		let mut jump_code = Vec::with_capacity(7);
		let jump_assert_reg = self.reg_reserve();
		// Negate expression
		jump_code.push(Op::Not as u8);
		jump_code.push(expr_reg);
		jump_code.push(jump_assert_reg);
		// Jump instruction
		jump_code.push(Op::JumpByIf as u8);
		jump_code.push(jump_assert_reg);
		jump_code.append(&mut (block_code.len() as u16).to_be_bytes().to_vec());

		// Compile code together
		let mut code = Vec::with_capacity(
				expr_code.len() + jump_code.len() + block_code.len());
		code.append(&mut expr_code);
		code.append(&mut jump_code);
		code.append(&mut block_code);

		return code;
	}


	fn cmp_var_assign(&mut self,
			name_tk:&QuToken, value_leaf:&QuLeafExpr) -> Vec<u8> {
		// Get variable pointer
		let var_ptr= self.get_var_pointer(
				&name_tk.text
				).expect(format!(
				"No variable with name {} has been defined",
				&name_tk.text).as_str());
		
		// Compile
		let mut code = Vec::with_capacity(10);
		// Compile value code
		let (mut val_code, val_reg) = self.cmp_expr(value_leaf);
		code.append(&mut val_code);
		// Compile store mem code
		code.push(Op::StoreMem as u8);
		code.push(val_reg);
		code.append(&mut (var_ptr as u32).to_be_bytes().to_vec());

		return code;
	}


	fn cmp_var_decl(&mut self,
			name:&QuToken, var_type:usize, val_leaf_op:&Option<QuLeafExpr>)
			-> Vec<u8> {
		let stk = self.stack_reserve();
		self.variables.push(
			(name.text.clone(), var_type, stk)
		);

		// Compile variable assign
		let (mut val_code, val_reg) = match val_leaf_op {
			Some(val_leaf) => self.cmp_expr(&val_leaf),
			None => {
				let mut code = Vec::with_capacity(3);
				let reg = self.reg_reserve();
				code.push(Op::LoadValU8 as u8); // TODO: Support u64
				code.push(0); // TODO: Support u64
				code.push(reg);
				(code, reg)
			},
		};
		let mut code = Vec::with_capacity(val_code.len() + 6);
		code.append(&mut val_code);

		// Store in mem
		code.push(Op::StoreMem as u8);
		code.push(val_reg);
		code.append(&mut (stk as u32).to_be_bytes().to_vec());

		return code;
	}


	fn cmp_vec(&mut self, leafs:&Vec<QuLeaf>) -> Vec<u8> {
		let mut code:Vec<u8> = vec![];
		for leaf in leafs {
			match leaf {
				QuLeaf::IfStatement(
					expr,
					statements
				) => {
					code.append(
						&mut self.cmp_if_statement(expr, statements)
					);
				}

				QuLeaf::VarDecl(
						name_tk,
						_type_tk,
						value_leaf
				) => {
					code.append(
						&mut self.cmp_var_decl(
								name_tk, 0, value_leaf)
					);
				}

				QuLeaf::VarAssign(
						name_rk,
						value_leaf
				) => {
					// Add code
					code.append(
						&mut self.cmp_var_assign(name_rk, value_leaf)
					);
				}

				_ => {unimplemented!()}
			}
		}
		
		return code;
	}


	pub fn compile(&mut self, leafs:&mut Vec<QuLeaf>) -> Vec<u8> {
		let mut code:Vec<u8> = self.cmp_vec(leafs);
		code.push(Op::End as u8);
		return code;
	}


	/// Gets the pointer to a variable by the variable's name.
	fn get_var_pointer(&self, var_name:&str) -> Option<usize> {
		for (name_, _type_, pointer_) in &self.variables {
			if name_ == var_name {
				return Some(*pointer_);
			}
		}
		return None;
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
	indent:u8,
	line:usize,
	tk_idx:usize,
	tk_stack:Vec<usize>,
	tokens:&'a Vec<QuToken>,

} impl<'a> QuParser<'a> {

	pub fn new(tokens:&'a mut Vec<QuToken>) -> Self {
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
		}
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
			match self.ck_if_statment() {
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

			// Error
			break;
		}

		if leafs.len() == 0 {
			return Err("Empty code block 'TODO: Better error messages'"
					.to_string());
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
		return self.ck_code_block();
	}


	fn ck_if_statment(&mut self) -> Result<Option<QuLeaf>, String> {
		match self.statement_start() {
			Ok(_) => (),
			Err(msg) => return Err(msg),
		}

		self.tk_push();

		// Check 'if' keyword
		let keyword_tk = self.tk_next()
				.expect("Improper indentation TODO: Bette message");
		if keyword_tk != KEYWORD_IF {
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
					QuLeaf::IfStatement(expr, scope_data)
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


	/// Matches tokens to a variable assignment.
	fn ck_var_assign(&mut self) -> Result<Option<QuLeaf>, String> {
		match self.statement_start() {
			Ok(_) => (),
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
		match self.statement_start() {
			Ok(_) => (),
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
			self.tk_pop();
			return None;
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
				Op::from(operator),
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
		return self.ck_code_block().unwrap().unwrap();
	}


	/// A helper function. Use whenever starting to parse a statement.
	fn statement_start(&mut self) -> Result<(), String> {
		let tk = self.tk_spy(0);
		let tk_row = tk.row;
		let tk_indent = tk.indent;
		self.line = tk_row as usize;

		if self.indent == u8::MAX {
			self.indent = tk_indent as u8;
		}
		else if self.indent != tk_indent as u8 {
			return Err("Bad indentation 'TODO: Better message'".to_string());
		}
		
		return Ok(());
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


	fn tk_pop(&mut self) {
		self.tk_idx = self.tk_stack.pop().unwrap();
	}


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
		&[("r", 1), ("r", 1), ("r", 1)]
	),
	(
		Op::Sub,
		"sub",
		&[("r", 1), ("r", 1), ("r", 1)]
	),
	(
		Op::Mul,
		"mul",
		&[("r", 1), ("r", 1), ("r", 1)]
	),
	(
		Op::Div,
		"div",
		&[("r", 1), ("r", 1), ("r", 1)]
	),
	(
		Op::Mod,
		"mod",
		&[("r", 1), ("r", 1), ("r", 1)]
	),
	(
		Op::Pow,
		"pow",
		&[("r", 1), ("r", 1), ("r", 1)]
	),
	(
		Op::Lesser,
		"lesser",
		&[("r", 1), ("r", 1), ("r", 1)]
	),
	(
		Op::Greater,
		"greater",
		&[("r", 1), ("r", 1), ("r", 1)]
	),
	(
		Op::Equal,
		"equal",
		&[("r", 1), ("r", 1), ("r", 1)]
	),
	(
		Op::NotEqual,
		"not_equal",
		&[("r", 1), ("r", 1), ("r", 1)]
	),
	(
		Op::Not,
		"not",
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
		let vm = QuVm { 
			pc:0,
			source:vec![],
			registers:[0;16],
			mem:vec![],
			stack:vec![],
		};

		return vm;
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
		unimplemented!()
	}


	fn exc_jump_by_if(&mut self) {
		let rg_if = self.next_u8() as usize;
		let val_by = self.next_u16() as usize;
		println!("{}", self.registers[rg_if] as i64);
		if self.registers[rg_if] as i64 > 0 {
			self.pc += val_by as usize;
		}
	}


	fn exc_jump_to(&mut self) {
		unimplemented!()
	}


	fn exc_jump_to_if(&mut self) {
		let rg_if = self.next_u8() as usize;
		let val_to = self.next_u32() as usize;
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
				x if x == Op::NotEqual as u8 => self.exc_logi_not_equal(),
				x if x == Op::Not as u8 => self.exc_logi_not(),

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