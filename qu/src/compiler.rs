
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
	name_refs:HashMap<String, u32>,
	contexts:Vec<QuCmpContext>,
	types:Vec<QuType2>,
	types_map:HashMap<String, usize>,


} impl QuCompiler {

	/// Creates and returns a new [QuCompiler].
	pub fn new() -> Self {
		let mut inst = Self{
			name_refs:HashMap::default(),
			contexts:Vec::default(),
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


	fn add_variable(&mut self, name:String) {
		self.contexts_get_top().var_refs.push(name);
		self.context_get_top_frame().var_refs_added += 1;
	}


	/// Compiles an expression into bytecode.
	fn cmp_expr(&mut self, leaf:&QuLeafExpr, output_reg:u8)
			-> Result<QuAsmBuilder, QuMsg> {
		return match leaf {
			QuLeafExpr::FnCall(
				name,
				params,
			) => self.cmp_fn_call(&name.text, params, output_reg),
			QuLeafExpr::Equation(
				op,
				left,
				right
			) => self.cmp_expr_math(*op, &**left, &**right, output_reg),
			QuLeafExpr::Number(val)
				=> Ok(self.cmp_expr_int(val, output_reg)),
				QuLeafExpr::Tuple(items)
				=> self.cmp_expr_tuple(items, output_reg),
			QuLeafExpr::Var(token)
				=> self.cmp_expr_val(token, output_reg),
		};
	}


	/// Compiles a math or logic expression into bytecode.
	fn cmp_expr_math(&mut self, op:u8, left:&QuLeafExpr, right:&QuLeafExpr,
			output_reg:u8) -> Result<QuAsmBuilder, QuMsg> {
		let right_reg = self.get_expr_reg(right)?;

		let mut builder = QuAsmBuilder::new();
		builder.add_builder(self.cmp_expr(left, output_reg)?);
		builder.add_builder(self.cmp_expr(right, right_reg)?);
		builder.add_code(vec![op, output_reg, right_reg, output_reg]);

		return Ok(builder);
	}


	/// Compiles a constant integer expression into bytecode.
	/// 
	/// # Panics
	/// 
	/// Panics if `val` can't be parsed to a number.
	fn cmp_expr_int(&mut self, val:&QuToken, output_reg:u8) -> QuAsmBuilder {
		// TODO: Support other int sizes
		
		let Ok(val) = val.text.parse::<u8>() else {
			panic!("Could not convert text '{}' to number!", val.text);
		};

		let mut b = QuAsmBuilder::new();
		b.add_u8(OPLIB.load_val_u8);
		b.add_code((val as u8).to_be_bytes().to_vec());
		b.add_u8(output_reg);

		return b;
	}


	/// Compiles a tuple construction into bytecode.
	fn cmp_expr_tuple(&mut self, items:&[QuLeafExpr], to_reg:u8
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
	fn cmp_expr_val(&mut self, token:&QuToken, output_reg:u8)
			-> Result<QuAsmBuilder, QuMsg> {
		
		// The register to copy the variable value from
		let Some(var_reg)
			= self.get_var_register(token.text.as_str())
			else {
				let mut msg
					= QuMsg::undefined_var_access(&token.text);
				msg.token = token.clone();
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
	fn cmp_flow_if(&mut self, condition:&QuLeafExpr, body:&Vec<QuLeaf>
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get expression register
		let mut expr_b = with!{
			self.stack_frame_start, self.stack_frame_end {
				let expr_reg = self.get_expr_reg(condition)?;
				/*return*/ self.cmp_expr(
					condition,
					expr_reg
				)
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
	fn cmp_flow_while(&mut self, condition:&QuLeafExpr, body:&Vec<QuLeaf>
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get expression register
		let mut expr_code = with!(
			self.stack_frame_start, self.stack_frame_end {
				let if_expr_reg = self.get_expr_reg(condition)?;
				// Expression code
				/*return*/ self.cmp_expr(condition, if_expr_reg)
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


	fn cmp_fn_call(&mut self, name:&str, params:&Vec<QuLeafExpr>, store_to:u8
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut builder = QuAsmBuilder::new();

		with!(self.stack_frame_start, self.stack_frame_end {
			for p in params {
				let reg = self.stack_reserve();
				let parameter_expr = self.cmp_expr(&p, reg)?;
				builder.add_builder(parameter_expr);
				builder.add_code(vec![OPLIB.parameter_add, reg]);
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
		params:&Vec<QuParamNode>,
		body:&Vec<QuLeaf>
	) -> Result<QuAsmBuilder, QuMsg> {
		self.add_name_ref(name.to_string());

		let body_code
			= with!(self.context_start, self.context_end {
			let mut b = QuAsmBuilder::new();

			// Compile parameters
			for p in params {
				self.add_variable(p.0.text.clone());
				let reg = self.stack_reserve();
				b.add_builder(self.cmp_parameter_pop(reg));
			}

			// Compile code block
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
	fn cmp_leaf(&mut self, leaf:&QuLeaf) -> Result<QuAsmBuilder, QuMsg> {
		match leaf {
			QuLeaf::Expression(
				expr_leaf,
			) => {
				return with!(self.stack_frame_start, self.stack_frame_end {
					let reg = self.get_expr_reg(expr_leaf)?;
					/*return*/ self.cmp_expr(expr_leaf, reg)
				});
			}
			QuLeaf::FlowStatement(
				flow_type,
				expr,
				statements,
			) => {
				match *flow_type{
					FLOW_TYPE_IF => {
						return self.cmp_flow_if(expr, statements);
					},
					FLOW_TYPE_WHILE => {
						return self.cmp_flow_while(expr, statements);
					}
					_ => unimplemented!(),
				}
			}
			QuLeaf::FnDecl(
				name,
				parameters,
				statements,
			) => {
				// TODO: Compiler fn declaration parameters
				return self.cmp_fn_decl(&name.text, parameters, statements);
			}
			QuLeaf::Print(leaf_expr) => {
				// TODO Handle errors for compiling Print
				let print_reg = self.get_expr_reg(leaf_expr)?;
				let mut code = self.cmp_expr(
					leaf_expr,
					print_reg
				)?;
				code.add_u8(OPLIB.print);
				code.add_u8(print_reg);
				return Ok(code);
			},
			QuLeaf::Return(expr) => {
				return self.cmp_expr(expr, 0);
			}
			QuLeaf::VarDecl(
					name_tk,
					type_tk,
					value_leaf
			) => {
				return self.cmp_var_decl(
					name_tk, type_tk, value_leaf);
			}
			QuLeaf::VarAssign(
					name_rk,
					value_leaf
			) => {
				return  self.cmp_var_assign(
					name_rk, value_leaf);
			}
		};
	}


	/// Compiles a [`Vec<QuLeaf>`] into bytecode.
	fn cmp_leafs(&mut self, leafs:&Vec<QuLeaf>) -> Result<QuAsmBuilder, QuMsg> {
		let mut b = QuAsmBuilder::new();
		for l in leafs {
			b.add_builder(self.cmp_leaf(l)?);
		}
		return Ok(b);
	}


	fn cmp_parameter_pop(&mut self, reg:u8) -> QuAsmBuilder {
		let mut b = QuAsmBuilder::new();
		b.add_code(vec![OPLIB.parameter_pop, reg]);
		return b;
	}


	/// Compiles code variable assignment.
	fn cmp_var_assign(&mut self,
			var_token:&QuToken, assign_to:&QuLeafExpr
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get variable register
		let var_reg
			= self.get_var_register(&var_token.text)
				.ok_or_else(||{
					let mut msg = QuMsg::undefined_var_assign(
						&var_token.text);
					msg.token = var_token.clone();
					return msg;
				}
			)?
		;
		// Compile assignment to expression
		return self.cmp_expr(assign_to, var_reg);
	}


	/// Compiles a variable declaration.
	fn cmp_var_decl(
		&mut self, var_token:&QuToken,
		variable_type_token:&Option<Box<QuToken>>,
		assign_to:&Option<Box<QuLeafExpr>>
	) -> Result<QuAsmBuilder, QuMsg> {

		// Check if the variable is already defined
		if self.is_var_defined(&var_token.text) {
			let mut msg = QuMsg::var_redefined(&var_token.text);
			msg.token = var_token.clone();
			return Err(msg);
		}

		// Get var type
		let mut var_type:usize = 0;
		if let Some(type_tk) = variable_type_token {
			let var_type_str = type_tk.text.as_str();
			if var_type_str != "" {
				var_type = *self.types_map.get(var_type_str)
					.ok_or_else(|| {
						let mut msg = QuMsg::undefined_type_access(
							&type_tk.text);
						msg.token = *type_tk.clone();
						return msg;
					}
				)?;
			}
		}

		// Create variable
		let var_reg = self.stack_reserve();
		self.add_variable(var_token.text.clone());

		// Compile variable assignment
		return match assign_to {
			// Compile variable value
			Some(val_leaf)
				=> self.cmp_expr(&val_leaf, var_reg),

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
	fn cmp_scope(&mut self, leaf:&Vec<QuLeaf>) -> Result<QuAsmBuilder, QuMsg> {
		let compiled = with!{
			self.stack_frame_start, self.stack_frame_end {
				/*return*/ self.cmp_leafs(leaf)
			}
		};
		return compiled;
	}


	/// Compiles Qu code.
	pub fn compile(&mut self, code:&str
	) -> Result<Vec<u8>, QuMsg> {
		let mut p = QuParser::new();
		let leafs = p.parse(code)?;
		return Ok(self.compile_from_leafs(leafs)?);
	}


	/// Compiles from a [QuLeaf] instruction into bytecode (into a [Vec]<[u8]>.)
	pub fn compile_from_leafs(&mut self, leafs:Vec<QuLeaf>
	) -> Result<Vec<u8>, QuMsg> {
		// Main code
		let mut code = with!(self.context_start, self.context_end {
			let mut code = self.cmp_scope(&leafs)?;
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
	fn get_expr_reg(&mut self, expr_leaf:&QuLeafExpr) -> Result<u8, QuMsg> {
		return  match expr_leaf {
			QuLeafExpr::Equation(_, _, _) => Ok(self.stack_reserve()),
			QuLeafExpr::FnCall(_, _) => Ok(self.stack_reserve()),
			QuLeafExpr::Number(_) => Ok(self.stack_reserve()),
			QuLeafExpr::Tuple(_) => Ok(self.stack_reserve()),
			QuLeafExpr::Var(name) => {
				self.get_var_register(&name.text)
					.ok_or_else(||{
						let mut msg
							= QuMsg::undefined_var_access(&name.text);
						msg.token = *name.clone();
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
	fn is_var_defined(&mut self, var_name:&String) -> bool {
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
	Code(Vec<u8>),
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
    use crate::{QuCompiler, QuVm, QuMsg, vm::OPLIB};


	#[test]
	fn compile() -> Result<(), QuMsg>{
		let mut c = QuCompiler::new();
		let code = c.compile(r#"
			var counter = 0
			while counter < 10:
				counter = counter + 1
		"#)?;

		assert_eq!(
			&code,
			&vec![
				OPLIB.load_val_u8, 0, 0,
				OPLIB.copy_reg, 0, 1,
				OPLIB.load_val_u8, 10, 2,
				OPLIB.lesser, 1, 2, 1,
				OPLIB.jump_by_if_not, 0, 0, 0, 7,
				OPLIB.load_val_u8, 1, 1,
				OPLIB.add, 0, 1, 0,
				OPLIB.jump_by, 255, 255, 255, 232,
				OPLIB.end,
			],
		);

		return Ok(());
	}

}