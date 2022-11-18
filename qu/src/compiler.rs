
use crate::QuParser;
use crate::parser::FLOW_TYPE_IF;
use crate::parser::FLOW_TYPE_WHILE;
use crate::parser::QuParamNode;
use crate::vm::OPLIB;
use crate::QuLeaf;
use crate::QuLeafExpr;
use crate::QuMsg;
use crate::QuToken;
use crate::QuType2;

use core::panic;
use std::collections::HashMap;
use std::mem::take;

// TODO: Fix compiler's documentation

/// Acts like a Python 'with' statement. Takes an opening and closing function,
/// calling the code between them.
macro_rules! with {
	($($start:ident).+, $($end:ident).+ $code:block) => {
		{
			$($start).+();
			let mut lmda = ||{$code};
			let output = lmda();
			$($end).+();
			/*return*/ output
		}
	};
}


struct QuCmpContext {
	var_refs:Vec<String>,
	stack_frames:Vec<QuCmpFrame>,
} impl QuCmpContext {

	fn new() -> Self {
		return Self{
			var_refs: Vec::default(),
			stack_frames: Vec::default(),
		};
	}

}


struct QuCmpFrame {
	var_refs_added:u8,
	stack_idx:u8,
} impl QuCmpFrame {

	fn new(stack_idx:u8) -> Self {
		return Self{
			var_refs_added: 0,
			stack_idx,
		};
	}

}


/// Compiles [QuLeaf]s into Qu bytecode.
pub struct QuCompiler {
	contexts:Vec<QuCmpContext>,
	constants_code:Vec<u8>,
	name_refs:HashMap<String, u32>,
	types:Vec<QuType2>,
	types_map:HashMap<String, usize>,


} impl QuCompiler {

	/// Creates and returns a new [QuCompiler].
	pub fn new() -> Self {
		let mut inst = Self{
			contexts:Vec::default(),
			constants_code:Vec::default(),
			name_refs:HashMap::default(),
			types:vec![QuType2::int(), QuType2::uint(), QuType2::bool()],
			types_map:HashMap::new(),
		};

		let mut i:usize = 0;
		for tp in &inst.types {
			inst.types_map.insert(tp.name.clone(), i);
			i += 1;
		}

		return inst;
	}


	fn add_name_ref(&mut self, name:String) {
		self.name_refs.insert(name, self.name_refs.len() as u32);
	}


	fn add_variable(&mut self, name:&str) {
		self.contexts_get_top().var_refs.push(name.to_owned());
		self.context_get_top_frame().var_refs_added += 1;
	}


	/// Compiles an expression into bytecode.
	fn cmp_expr(&mut self, leaf:QuLeafExpr, output_reg:u8)
			-> Result<QuAsmBuilder, QuMsg> {
		return match leaf {
			QuLeafExpr::FnCall(
				name,
				params,
			) => self.cmp_fn_call(&name.slice, params, output_reg),
			QuLeafExpr::Equation(
				op,
				left,
				right
			) => self.cmp_expr_math(op, *left, *right, output_reg),
			QuLeafExpr::Number(val)
				=> Ok(self.cmp_expr_int(*val, output_reg)),
				QuLeafExpr::Tuple(items)
				=> self.cmp_expr_tuple(items, output_reg),
			QuLeafExpr::Var(token)
				=> self.cmp_expr_val(*token, output_reg),
		};
	}


	/// Compiles a math or logic expression into bytecode.
	fn cmp_expr_math(
		&mut self,
		op:u8,
		mut left:QuLeafExpr,
		mut right:QuLeafExpr,
		output_reg:u8
	)-> Result<QuAsmBuilder, QuMsg> {
		let left_reg = if let QuLeafExpr::Var(_) = left {
			self.get_expr_reg(&mut left)?
		} else { output_reg };

		let right_reg = if let QuLeafExpr::Var(_) = right {
			self.get_expr_reg(&mut right)?
		} else {
			if left_reg == output_reg {
				self.get_expr_reg(&mut right)?
			} else { output_reg }
		};

		let mut builder = QuAsmBuilder::new();
		builder.add_builder(self.cmp_expr(left, left_reg)?);
		builder.add_builder(self.cmp_expr(right, right_reg)?);
		builder.add_code(vec![op, left_reg, right_reg, output_reg]);

		return Ok(builder);
	}


	/// Compiles a constant integer expression into bytecode.
	/// 
	/// # Panics
	/// 
	/// Panics if `val` can't be parsed to a number.
	fn cmp_expr_int(&mut self, val:QuToken, output_reg:u8) -> QuAsmBuilder {
		// TODO: Support other int sizes
		
		let Ok(val) = val.slice.parse::<u8>() else {
			panic!("Could not convert text '{}' to number!", val.slice);
		};

		let mut b = QuAsmBuilder::new();
		b.add_u8(OPLIB.load_val_u8);
		b.add_code((val as u8).to_be_bytes().to_vec());
		b.add_u8(output_reg);

		return b;
	}


	/// Compiles a tuple construction into bytecode.
	fn cmp_expr_tuple(&mut self, items:Vec<QuLeafExpr>, to_reg:u8
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut b = QuAsmBuilder::new();
		let mut i = to_reg;
		for item in items {
			b.add_builder(self.cmp_expr(item, i)?);
			i += 1;
		}

		return Ok(b);
	}


	/// Compiles a variable-expression into bytecode.
	fn cmp_expr_val(&mut self, token:QuToken, output_reg:u8)
			-> Result<QuAsmBuilder, QuMsg> {
		
		// The register to copy the variable value from
		let Some(var_reg)
			= self.get_var_register(&token.slice)
			else {
				let mut msg
					= QuMsg::undefined_var_access(&token.slice);
				msg.token = token.char_index;
				return Err(msg);
			};
		// Copying to same register location, return nothing
		if var_reg == output_reg {
			return Ok(QuAsmBuilder::default());
		}

		let mut b = QuAsmBuilder::new();
		b.add_u8(OPLIB.copy_reg);
		b.add_code((var_reg as u8).to_be_bytes().to_vec());
		b.add_u8(output_reg);

		return Ok(b);
	}


	/// Compiles an *if* statement into bytecode.
	fn cmp_flow_if(&mut self, mut condition:QuLeafExpr, body:Vec<QuLeaf>
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get expression register
		let mut expr_b = with!{
			self.stack_frame_start, self.stack_frame_end {
				let expr_reg = self.get_expr_reg(&mut condition)?;
				self.cmp_expr(condition, expr_reg)
			}
		}?;

		// New frame for the code in the 'if' body
		let b = with!{
			self.stack_frame_start, self.stack_frame_end {
				let body_code = self.cmp_scope(body)?;

				let mut b = QuAsmBuilder::new();
				// Conditional brancher
				b.add_builder(expr_b);
				b.add_u8(OPLIB.jump_by_if_not);
				b.add_i32(body_code.len() as i32);

				// If body
				b.add_builder(body_code);

				/*return*/ Ok(b)
			}
		}?;

		return Ok(b);
	}


	/// Compiles a *while* statement into bytecode.
	fn cmp_flow_while(&mut self, mut condition:QuLeafExpr, body:Vec<QuLeaf>
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get expression register
		let mut expr_code = with!(
			self.stack_frame_start, self.stack_frame_end {
				let if_expr_reg = self.get_expr_reg(&mut condition)?;
				// Expression code
				self.cmp_expr(condition, if_expr_reg)
			}
		)?;

		// New frame for the code in the 'if' body
		let code= with!{
			self.stack_frame_start, self.stack_frame_end {
				// --- Compile Pieces ---
				// Code block
				let mut block_code = self.cmp_scope(body)?;
				let block_code_len = block_code.len();

				let mut b = QuAsmBuilder::new();

				// Loop check expression
				let expr_code_len = expr_code.len();
				b.add_builder(expr_code);

				// Skip body if expression false
				let mut skip_body_b = QuAsmBuilder::new();
				skip_body_b.add_u8(OPLIB.jump_by_if_not);
				skip_body_b.add_i32(block_code_len as i32);
				let skip_body_b_len = skip_body_b.len();
				b.add_builder(skip_body_b);

				// Loop body
				b.add_builder(block_code);

				// Jump back to expression
				b.add_u8(OPLIB.jump_by);
				b.add_i32(-((
					block_code_len
					+ skip_body_b_len
					+ expr_code_len
					+ 2
				) as i32));

				/*return*/ Ok(b)
			}
		}?;
		
		return Ok(code);
	}


	fn cmp_fn_call(&mut self, name:&str, params:Vec<QuLeafExpr>, store_to:u8
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut builder = QuAsmBuilder::new();

		with!(self.stack_frame_start, self.stack_frame_end {
			for p in params {
				let reg = self.stack_reserve();
				let parameter_expr = self.cmp_expr(p, reg)?;
				builder.add_builder(parameter_expr);
			}
			Ok(())
		})?;

		builder.add_u8(OPLIB.call);
		builder.add_name_ref(name.to_owned());
		builder.add_u8(store_to);

		return Ok(builder);
	}


	fn cmp_fn_decl(
		&mut self,
		name:&str,
		params:Vec<QuParamNode>,
		body:Vec<QuLeaf>
	) -> Result<QuAsmBuilder, QuMsg> {
		self.add_name_ref(name.to_owned());

		let body_code
			= with!(self.context_start, self.context_end {

			// Add return value variable for padding.
			self.add_variable("return value");
			let _ = self.stack_reserve();

			// Compile parameters
			for p in params {
				self.add_variable(&p.0.slice);
				// Reserver space for parameter
				let _ = self.stack_reserve();
			}

			// Compile code block
			let mut b = QuAsmBuilder::new();
			b.add_builder(self.cmp_leafs(body)?);
			b.add_u8(OPLIB.end);
			Ok(b)
		})?;

		// Add fn declaration operation
		let mut code = QuAsmBuilder::new();
		code.add_u8(OPLIB.define_fn);
		code.add_short_str(name.to_owned())?;
		code.add_u32(body_code.len() as u32);

		// Add body
		code.add_builder(body_code);

		return Ok(code);
	}


	/// Compiles a [QuLeaf] into bytecode.
	fn cmp_leaf(&mut self, leaf:QuLeaf) -> Result<QuAsmBuilder, QuMsg> {
		match leaf {
			QuLeaf::Expression(
				mut expr_leaf,
			) => {
				return with!(self.stack_frame_start, self.stack_frame_end {
					let reg = self.get_expr_reg(&mut expr_leaf)?;
					self.cmp_expr(*expr_leaf, reg)
				});
			}
			QuLeaf::FlowStatement(
				flow_type,
				expr,
				statements,
			) => {
				match flow_type{
					FLOW_TYPE_IF => {
						return self.cmp_flow_if(
							*expr,
							statements
						);
					},
					FLOW_TYPE_WHILE => {
						return self.cmp_flow_while(
							*expr,
							statements
						);
					}
					_ => unimplemented!(),
				}
			}
			QuLeaf::FnDecl(
				mut name,
				parameters,
				statements,
			) => {
				// TODO: Compiler fn declaration parameters
				return self.cmp_fn_decl(
					&name.slice,
					parameters,
					statements
				);
			}
			QuLeaf::Print(mut leaf_expr) => {
				// TODO Handle errors for compiling Print
				let print_reg = self.get_expr_reg(&mut leaf_expr)?;
				let mut code = self.cmp_expr(
					*leaf_expr,
					print_reg
				)?;
				code.add_u8(OPLIB.print);
				code.add_u8(print_reg);
				return Ok(code);
			},
			QuLeaf::Return(expr) => {
				let mut code = QuAsmBuilder::new();
				match expr {
					Some(mut l) => {
						let reg = self.get_expr_reg(&mut l)?;
						code.add_builder(self.cmp_expr(*l, reg)?);
						code.add_code(vec![OPLIB.copy_reg, reg, 0]);
					},
					None => {},
				}
				code.add_u8(OPLIB.end);
				return Ok(code);
			}
			QuLeaf::VarDecl(
					name_tk,
					type_tk,
					value_leaf
			) => {
				return self.cmp_var_decl(
					*name_tk,
					type_tk,
					value_leaf
				);
			}
			QuLeaf::VarAssign(
					name_rk,
					value_leaf
			) => {
				return  self.cmp_var_assign(
					*name_rk, *value_leaf);
			}
		};
	}


	/// Compiles a [`Vec<QuLeaf>`] into bytecode.
	fn cmp_leafs(&mut self, leafs:Vec<QuLeaf>) -> Result<QuAsmBuilder, QuMsg> {
		let mut b = QuAsmBuilder::new();
		for l in leafs {
			b.add_builder(self.cmp_leaf(l)?);
		}
		return Ok(b);
	}


	/// Compiles code variable assignment.
	fn cmp_var_assign(&mut self,
			var_token:QuToken, assign_to:QuLeafExpr
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get variable register
		let var_reg
			= self.get_var_register(&var_token.slice)
				.ok_or_else(||{
					let mut msg = QuMsg::undefined_var_assign(
						&var_token.slice);
					msg.token = var_token.char_index;
					return msg;
				}
			)?
		;
		// Compile assignment to expression
		return self.cmp_expr(assign_to, var_reg);
	}


	/// Compiles a variable declaration.
	fn cmp_var_decl(
		&mut self, var_token:QuToken,
		variable_type_token:Option<Box<QuToken>>,
		assign_to:Option<Box<QuLeafExpr>>
	) -> Result<QuAsmBuilder, QuMsg> {

		// Check if the variable is already defined
		if self.is_var_defined(&var_token.slice) {
			let mut msg = QuMsg::var_redefined(&var_token.slice);
			msg.token = var_token.char_index;
			return Err(msg);
		}

		// Get var type
		let mut var_type:usize = 0;
		if let Some(type_tk) = variable_type_token {
			let var_type_str = &type_tk.slice;
			if var_type_str != "" {
				var_type = *self.types_map.get(var_type_str)
					.ok_or_else(|| {
						let mut msg = QuMsg::undefined_type_access(
							&type_tk.slice);
						msg.token = type_tk.char_index;
						return msg;
					}
				)?;
			}
		}

		// Create variable
		let var_reg = self.stack_reserve();
		self.add_variable(&var_token.slice);

		// Compile variable assignment
		return match assign_to {
			// Compile variable value
			Some(val_leaf)
				=> self.cmp_expr(*val_leaf, var_reg),

			// No default value, compile fallback to zero
			None => {
				// TODO: Support u64
				let mut code = QuAsmBuilder::new();
				code.add_code(vec![
					OPLIB.load_val_u8,
					0,
					var_reg,
				]); 
				Ok(code)
			},
		};
	}


	/// Compiles a scope.
	fn cmp_scope(&mut self, leaf:Vec<QuLeaf>) -> Result<QuAsmBuilder, QuMsg> {
		let compiled = with!{
			self.stack_frame_start, self.stack_frame_end {
				/*return*/ self.cmp_leafs(leaf)
			}
		};
		return compiled;
	}


	/// Compiles Qu code from a [`&str`] into a [`Vec<u8>`].
	pub fn compile(&mut self, code:&str
	) -> Result<Vec<u8>, QuMsg> {
		let mut p = QuParser::new();
		let leafs = p.parse(code)?;
		return Ok(self.compile_from_leafs(leafs)?);
	}


	/// Compiles Qu code from a [QuLeaf] into a [`Vec<u8>`].
	pub fn compile_from_leafs(&mut self, leafs:Vec<QuLeaf>
	) -> Result<Vec<u8>, QuMsg> {
		// Main code
		let mut code = with!(self.context_start, self.context_end {
			let mut code = self.cmp_scope(leafs)?;
			code.add_u8(OPLIB.end);
			Ok(code)
		})?;
		

		return Ok(code.compile(&self.name_refs)?);
	}


	fn context_end(&mut self) {
		self.stack_frame_end();
		self.contexts.pop().unwrap();
	}


	fn context_get_top_frame(&mut self) -> &mut QuCmpFrame{
		let c_top = self.contexts_get_top();
		if c_top.stack_frames.len() == 0 {
			panic!();
		}
		let l = c_top.stack_frames.len();
		return &mut c_top.stack_frames[l-1];
	}


	fn context_get_top_frame_option(&mut self) -> Option<&mut QuCmpFrame> {
		let c_top = self.contexts_get_top();
		if c_top.stack_frames.len() == 0 {
			return None;
		}
		let l = c_top.stack_frames.len();
		return Some(&mut c_top.stack_frames[l-1]);
	}


	fn context_start(&mut self) {
		let c = QuCmpContext::new();
		self.contexts.push(c);
		self.stack_frame_start();
	}


	fn contexts_get_top(&mut self) -> &mut QuCmpContext{
		if self.contexts.len() == 0 {
			panic!();
		}
		let l = self.contexts.len();
		return &mut self.contexts[l-1];
	}


	/// Returns an appropriate location to store an expression.
	/// 
	/// Most expressions require a new memory location, but variables
	/// should just return the register of the variable.
	fn get_expr_reg(&mut self, expr_leaf:&mut QuLeafExpr) -> Result<u8, QuMsg> {
		return  match expr_leaf {
			QuLeafExpr::Equation(_, _, _) => Ok(self.stack_reserve()),
			QuLeafExpr::FnCall(_, _) => Ok(self.stack_reserve()),
			QuLeafExpr::Number(_) => Ok(self.stack_reserve()),
			QuLeafExpr::Tuple(_) => Ok(self.stack_reserve()),
			QuLeafExpr::Var(name) => {
				self.get_var_register(&name.slice)
					.ok_or_else(||{
						let mut msg
							= QuMsg::undefined_var_access(&name.slice);
						msg.token = name.char_index.clone();
						return msg;
					}
				)
			}
		};
	}


	/// Returns the current stack index.
	fn get_stack_idx(&mut self) -> u8 {
		return self.get_stack_idx_option().unwrap();
	}


	/// Returns the current stack index or [`None`].
	fn get_stack_idx_option(&mut self) -> Option<u8> {
		let Some(f) = self.context_get_top_frame_option()
			else {return None};
		return Some(f.stack_idx);
	}


	/// Gets the pointer to a variable by the variable's name.
	fn get_var_register(&mut self, var_name:&str) -> Option<u8> {
		let mut i = 0;
		for var_ref in &self.contexts_get_top().var_refs {
			if var_ref == var_name {
				return Some(i);
			}
			i += 1;
		}
		return None;
	}


	/// Returns true if the given variable is already defined.
	fn is_var_defined(&mut self, var_name:&str) -> bool {
		// TODO: Maybe use a faster algorithm?
		for var_ref in &self.contexts_get_top().var_refs {
			if var_ref == var_name {
				return true;
			}
		}
		return false;
	}


	/// Closes the current stack frame returning the stake frame to the
	/// beginning of the frame.
	fn stack_frame_end(&mut self) {
		let context = self.contexts_get_top();
		let frame = context.stack_frames.pop().unwrap();
		context.var_refs.resize(
			context.var_refs.len()-(frame.var_refs_added as usize),
			"".to_owned(),
		);
	}


	/// Starts a new stack frame.
	fn stack_frame_start(&mut self) {
		let f = QuCmpFrame::new(
			self.get_stack_idx_option().unwrap_or(0)
		);
		self.contexts_get_top().stack_frames.push(f);
	}

	/// Returns the current stack pointer and increments it.
	fn stack_reserve(&mut self) -> u8 {
		let mut top_frame = self.context_get_top_frame();
		let x = top_frame.stack_idx;
		top_frame.stack_idx += 1;
		return x;
	}

}


#[derive(Debug, Clone)]
enum QuBuilderPiece {
	/// A [`Vec<u8>`] of code.
	Code(Vec<u8>),
	/// A [`String`] that gets converted to a [`u32`] ID later.
	NameRefU32(String),
} impl QuBuilderPiece {

	fn len(&self) -> usize{
		match self {
			QuBuilderPiece::Code(v) => v.len(),
			QuBuilderPiece::NameRefU32(_) => 4,
		}
	}

}


#[derive(Debug, Default, Clone)]

struct QuAsmBuilder {
	code_pieces:Vec<QuBuilderPiece>,
} impl QuAsmBuilder {

	fn new() -> Self {
		return Self {
			code_pieces: vec![],
		}
	}


	fn add_builder(&mut self, mut builder:QuAsmBuilder) {
		self.code_pieces.append(&mut builder.code_pieces);
	}


	fn add_code(&mut self, code:Vec<u8>) {
		self.code_pieces.push(QuBuilderPiece::Code(code));
	}


	fn add_name_ref(&mut self, name:String) {
		self.code_pieces.push(
			QuBuilderPiece::NameRefU32(name)
		);
	}


	fn add_i32(&mut self, num:i32) {
		self.code_pieces.push(QuBuilderPiece::Code(num.to_be_bytes().to_vec()));
	}


	fn add_short_str(&mut self, string:String) -> Result<(), QuMsg>{
		if string.len() > 255 {
			return Err(QuMsg::general(
				"The string is too long. TODO: Better msg"
			));
		}
		let mut string_vec = string.as_bytes().to_vec();
		let mut code = vec![(string_vec.len() as u8)];
		code.append(&mut string_vec);
		self.code_pieces.push(QuBuilderPiece::Code(code));

		return Ok(());
	}


	fn add_u8(&mut self, num:u8) {
		self.code_pieces.push(QuBuilderPiece::Code(vec![num]));
	}


	fn add_u32(&mut self, num:u32) {
		self.code_pieces.push(QuBuilderPiece::Code(num.to_be_bytes().to_vec()));
	}


	fn compile(&mut self, name_references:&HashMap<String, u32>
	) -> Result<Vec<u8>, QuMsg> {
		let mut code = vec![];
		for x in &mut self.code_pieces {
			match x {
				QuBuilderPiece::Code(c) => {code.append(c);},
				QuBuilderPiece::NameRefU32(name) => {
					let Some(c) = name_references.get(name)
						else {return Err(QuMsg::general("Could not find variable. TODO: Better msg"))};
					code.append(&mut c.to_be_bytes().to_vec());
				},
			};
		}

		return Ok(code);
	}


	fn len(&self) -> usize {
		let mut l = 0;
		for x in &self.code_pieces {
			l += x.len();
		}
		return l;
	}

}

#[cfg(test)]
mod test {
    use std::fmt::format;

    use crate::{QuCompiler, QuVm, QuMsg, vm::OPLIB, Qu};


	#[test]
	fn recursive_fn_addinate() -> Result<(), QuMsg>{
		let qu = Qu::new();
		let script = r#"
			fn addinate(val):
				if val < 100:
					addinate(val + val)
				return val
			
			return addinate(1)
		"#;

		assert_eq!(
			qu.compile_to_asm(script, false)?,
			format!("{}{}{}{}{}{}{}{}{}{}{}{}{}{}",
				"\nDFFN \"addinate\" 27",
				"\nLDU8 100 2",
				"\nLES 1 2 2",
				"\nJBIN 10",
				"\nADD 1 1 3",
				"\nCALL 0 2",
				"\nCPY 1 0",
				"\nEND",
				// TODO: Remove redundent 'END' when using 'return' keyword.
				"\nEND",
				"\nLDU8 1 1",
				"\nCALL 0 0",
				"\nCPY 0 0",
				"\nEND",
				// TODO: Remove redundent 'END' when using 'return' keyword.
				"\nEND",
			)
		);

		return Ok(());
	}


	#[test]
	fn while_count_down() -> Result<(), QuMsg>{
		let qu = Qu::new();
		let script = r#"
			var counter = 10
			while 0 > counter:
				counter = counter - 1
		"#;

		assert_eq!(
			qu.compile_to_asm(script, false)?,
			format!("{}{}{}{}{}{}{}{}",
				"\nLDU8 10 0",
				"\nLDU8 0 1",
				"\nGRT 1 0 1",
				"\nJBIN 7",
				"\nLDU8 1 1",
				"\nSUB 0 1 0",
				"\nJB 4294967275",
				"\nEND",
			)
		);

		return Ok(());
	}


	#[test]
	fn while_count_up() -> Result<(), QuMsg>{
		let qu = Qu::new();
		let script = r#"
			var counter = 0
			while counter < 10:
				counter = counter + 1
		"#;

		assert_eq!(
			qu.compile_to_asm(script, false)?,
			format!("{}{}{}{}{}{}{}{}",
				"\nLDU8 0 0",
				"\nLDU8 10 1",
				"\nLES 0 1 1",
				"\nJBIN 7",
				"\nLDU8 1 1",
				"\nADD 0 1 0",
				"\nJB 4294967275",
				"\nEND",
			)
		);

		return Ok(());
	}

}