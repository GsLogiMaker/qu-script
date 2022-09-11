use crate::vm::OPLIB;
use crate::QuLeaf;
use crate::QuLeafExpr;
use crate::QuMsg;
use crate::QuToken;
use crate::QuType;
use crate::QuVar;

use std::collections::HashMap;

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

/// Compiles [QuLeaf]s into Qu bytecode.
pub struct QuCompiler {
	/// Name, QuType, Pointer
	str_constants:Vec<String>,
	variables:Vec<QuVar>,
	functions:HashMap<String, u32>,
	stack_layers:Vec<u8>,
	stack_idx:u8,
	types:Vec<QuType>,
	types_map:HashMap<String, usize>,


} impl QuCompiler {

	/// Creates and returns a new [QuCompiler].
	pub fn new() -> Self {
		let mut inst = Self{
			str_constants:Vec::default(),
			variables:Vec::default(),
			functions:HashMap::default(),
			stack_layers:Vec::default(),
			stack_idx:0,
			types:vec![QuType::int(), QuType::uint(), QuType::bool()],
			types_map:HashMap::new(),
		};

		let mut i:usize = 0;
		for tp in &inst.types {
			inst.types_map.insert(tp.name.clone(), i);
			i += 1;
		}

		return inst;
	}


	/// Compiles an expression into bytecode.
	fn cmp_expr(&mut self, leaf:&QuLeafExpr, output_reg:u8)
			-> Result<Vec<u8>, QuMsg> {
		return match leaf {
			QuLeafExpr::FnCall(
				name
			) => self.cmp_fn_call(name),
			QuLeafExpr::Equation(
				op,
				left,
				right
			) => self.cmp_expr_math(*op, &**left, &**right, output_reg),
			QuLeafExpr::Int(val)
				=> Ok(self.cmp_expr_int(*val, output_reg)),
			QuLeafExpr::Var(token)
				=> self.cmp_expr_val(token, output_reg),
		};
	}


	/// Compiles a math or logic expression into bytecode.
	fn cmp_expr_math(&mut self, op:u8, left:&QuLeafExpr, right:&QuLeafExpr,
			output_reg:u8) -> Result<Vec<u8>, QuMsg> {
		
		let right_reg = self.get_expr_reg(right)?;

		let mut code:Vec<u8> = vec![];
		
		// Compile right expression
		let mut rgh_bytes
			= self.cmp_expr(right, right_reg)?;
		code.append(&mut rgh_bytes);
		// Compile left expression
		let mut lft_bytes
			= self.cmp_expr(left, output_reg)?;
		code.append(&mut lft_bytes);

		// Compile expression calculation
		code.append(&mut vec![op]);
		code.append(&mut vec![output_reg]);
		code.append(&mut vec![right_reg]);
		code.append(&mut vec![output_reg]);

		return Ok(code);
	}


	/// Compiles a constant integer expression into bytecode.
	fn cmp_expr_int(&mut self, val:u64, output_reg:u8) -> Vec<u8> {
		// TODO: Support other int sizes

		let mut code = vec![];
		// The vm operation
		code.push(OPLIB.load_val_u8);
		// The number as bytes
		code.append(&mut (val as u8).to_be_bytes().to_vec());
		// The register to load into
		code.push(output_reg);

		return code;
	}


	/// Compiles a variable-expression into bytecode.
	fn cmp_expr_val(&mut self, token:&QuToken, output_reg:u8)
			-> Result<Vec<u8>, QuMsg> {
		
		// The register to copy the variable value from
		let var_reg
			= self.get_var_register(token.text.as_str())
			.ok_or_else(||{
				let mut msg
					= QuMsg::undefined_var_access(&token.text);
				msg.token = token.clone();
				return msg;
			}
		)?;
		// Copying to same register location, return nothing
		if var_reg == output_reg {
			return Ok(Vec::default());
		}

		let mut code = Vec::with_capacity(3);
		// The vm operation
		code.push(OPLIB.copy_reg);
		code.append(&mut (var_reg as u8).to_be_bytes().to_vec());
		// The register to copy the variable value into
		code.push(output_reg);

		return Ok(code);
	}


	/// Compiles an *if* statement into bytecode.
	fn cmp_flow_if(&mut self, condition:&QuLeafExpr, body:&Box<QuLeaf>
	) -> Result<Vec<u8>, QuMsg> {
		// Get expression register
		let mut expr_code = with!{
			self.stack_frame_start, self.stack_frame_end {
				let if_expr_reg = self.get_expr_reg(condition)?;
				/*return*/ self.cmp_expr(condition, if_expr_reg)
			}
		}?;

		// New frame for the code in the 'if' body
		let code = with!{
			self.stack_frame_start, self.stack_frame_end {
				// Compile pieces
				let mut block_code = self.cmp_scope(body)?;
				let mut jump_code = Vec::with_capacity(7);
				jump_code.push(OPLIB.jump_by_if_not);
				jump_code.append(&mut (block_code.len() as i32).to_be_bytes().to_vec());

				// Combine code pieces together
				let mut code = Vec::default();
				code.append(&mut expr_code);
				code.append(&mut jump_code);
				code.append(&mut block_code);
				/*return*/ Ok(code)
			}
		}?;

		return Ok(code);
	}


	/// Compiles a *while* statement into bytecode.
	fn cmp_flow_while(&mut self, condition:&QuLeafExpr, body:&Box<QuLeaf>
	) -> Result<Vec<u8>, QuMsg> {
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
				// Assertion jump code
				let mut jump_code = Vec::with_capacity(7);
				let jump_distance = block_code.len() as i32
					+ 1 + OPLIB.op_args_size(OPLIB.jump_by) as i32;
				jump_code.push(OPLIB.jump_by_if_not);
				jump_code.append(&mut jump_distance.to_be_bytes().to_vec());
				// Jump back code
				let mut jump_back_code = Vec::default();
				jump_back_code.push(OPLIB.jump_by);
				let jump_back_distance = -(block_code.len() as i32
					+ jump_code.len() as i32
					+ expr_code.len() as i32
					+ 1 + OPLIB.op_args_size(OPLIB.jump_by) as i32);
				jump_back_code.append(&mut (jump_back_distance).to_be_bytes().to_vec());

				// Combine code pieces together
				let mut code = Vec::with_capacity(
						expr_code.len() + jump_code.len() + block_code.len());
				code.append(&mut expr_code);
				code.append(&mut jump_code);
				code.append(&mut block_code);
				code.append(&mut jump_back_code);
				/*return*/ Ok(code)
			}
		}?;
		
		return Ok(code);
	}


	fn cmp_fn_call(&mut self, name:&str) -> Result<Vec<u8>, QuMsg> {
		let name_index = *self.functions.get(&name.to_string())
			.ok_or_else(||{
				panic!("TODO: Need token for QuMsg");
//				let msg = QuMsg::general(
//					"QuCompiler attempted to call a function that was not defined. TODO: Better error message");
//				return msg;
			}
		)? as u32;

		let mut code = vec![];
		// Add fn declaration operation
		code.push(OPLIB.call);
		code.append(&mut name_index.to_be_bytes().to_vec());

		return Ok(code);
	}


	fn cmp_fn_decl(&mut self, name:&str, body:&QuLeaf
	) -> Result<Vec<u8>, QuMsg> {
		let name_index = self.str_constants.len() as u32;
		self.str_constants.push(name.to_owned());

		self.functions.insert(name.to_string(), name_index);

		// Compile code block
		let mut body_compiled = self.cmp_leaf(body)?;
		body_compiled.push(OPLIB.end);
		let code_length = body_compiled.len() as u32;

		let mut code = vec![];
		// Add fn declaration operation
		code.push(OPLIB.define_fn);
		code.append(&mut name_index.to_be_bytes().to_vec());
		code.append(&mut code_length.to_be_bytes().to_vec());
		// Add body
		code.append(&mut body_compiled);

		return Ok(code);
	}


	/// Compiles a [QuLeaf] into bytecode.
	fn cmp_leaf(&mut self, leaf:&QuLeaf) -> Result<Vec<u8>, QuMsg> {
		match leaf {
			QuLeaf::Block(leafs) => {
				let mut code = vec![];
				for block_leaf in leafs {
					let mut block_code = self.cmp_leaf(block_leaf)?;
					code.append(&mut block_code);
				}
				return Ok(code);
			},
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
					FLOW_IF => {
						return self.cmp_flow_if(expr, statements);
					},
					FLOW_WHILE => {
						return self.cmp_flow_while(expr, statements);
					}
					_ => unimplemented!(),
				}
			}
			QuLeaf::FnDecl(
				name,
				statements,
			) => {
				return self.cmp_fn_decl(name, statements);
			}
			QuLeaf::Print(leaf_expr) => {
				// TODO Handle errors for compiling Print
				let mut code = vec![];
				let print_reg = self.get_expr_reg(leaf_expr)?;
				let mut expression_code
					= self.cmp_expr(leaf_expr, print_reg)?;
				
				code.append(&mut expression_code);
				code.push(OPLIB.print);
				code.push(print_reg);
				return Ok(code);
			},
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


	/// Compiles code variable assignment.
	fn cmp_var_assign(&mut self,
			var_token:&QuToken, assign_to:&QuLeafExpr
	) -> Result<Vec<u8>, QuMsg> {
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
	fn cmp_var_decl(&mut self, var_token:&QuToken,
			variable_type_token:&Option<QuToken>,
	assign_to:&Option<QuLeafExpr>) -> Result<Vec<u8>, QuMsg> {

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
						msg.token = type_tk.clone();
						return msg;
					}
				)?;
			}
		}

		// Create variable
		let var_reg = self.stack_reserve();
		self.variables.push(
			QuVar::new(var_token.text.as_str(),
				Some(var_type), var_reg)
		);

		// Compile variable assignment
		return match assign_to {
			// Compile variable value
			Some(val_leaf)
				=> self.cmp_expr(&val_leaf, var_reg),

			// No default value, compile fallback to zero
			None => {
				// TODO: Support u64
				let mut code = Vec::with_capacity(3);
				code.push(OPLIB.load_val_u8); 
				code.push(0);
				code.push(var_reg);
				Ok(code)
			},
		};
	}


	/// Compiles a scope.
	fn cmp_scope(&mut self, leaf:&QuLeaf) -> Result<Vec<u8>, QuMsg> {
		let compiled = with!{
			self.stack_frame_start, self.stack_frame_end {
				/*return*/ self.cmp_leaf(leaf)
			}
		};
		return compiled;
	}


	fn cmp_str_constants(&mut self) -> Result<Vec<u8>, QuMsg> {
		let mut code = vec![];
		for s in &self.str_constants {
			code.push(OPLIB.define_const_str);
			code.push(s.len() as u8);
			for char in s.chars() {
				if !char.is_ascii() {
//					let msg = QuMsg::general(
//						"String is not ASCII. TODO: Better error message"
//					);
					panic!("TODO: Need token for QuMsg");
//					return Err(msg);
				}
				code.push(char as u8);
			}
		}
		return Ok(code);
	}


	/// Compiles from a [QuLeaf] instruction into bytecode (into a [Vec]<[u8]>.)
	pub fn compile(&mut self, leafs:&QuLeaf) -> Result<Vec<u8>, QuMsg> {
		// Main code
		let mut code = self.cmp_scope(leafs)?;
		code.push(OPLIB.end);

		// Constants and joining with main code
		let mut constants_code = self.cmp_str_constants()?;
		constants_code.append(&mut code);

		return Ok(constants_code);
	}


	/// Returns an appropriate location to store an expression.
	/// 
	/// Most expressions require a new memory location, but variables
	/// should just return the register of the variable.
	fn get_expr_reg(&mut self, expr_leaf:&QuLeafExpr) -> Result<u8, QuMsg> {
		return  match expr_leaf {
			QuLeafExpr::Equation(_, _, _) => Ok(self.stack_reserve()),
			QuLeafExpr::FnCall(_) => Ok(self.stack_reserve()),
			QuLeafExpr::Int(_) => Ok(self.stack_reserve()),
			QuLeafExpr::Var(name) => {
				self.get_var_register(&name.text)
					.ok_or_else(||{
						let mut msg
							= QuMsg::undefined_var_access(&name.text);
						msg.token = name.clone();
						return msg;
					}
				)
			}
		};
	}


	/// Gets the pointer to a variable by the variable's name.
	fn get_var_register(&self, var_name:&str) -> Option<u8> {
		for var_metadata in &self.variables {
			if var_metadata.name == var_name {
				return Some(var_metadata.register);
			}
		}
		return None;
	}


	/// Returns true if the given variable is already defined.
	fn is_var_defined(&self, var_name:&String) -> bool {
		// TODO: Maybe use a faster algorithm??
		for var_metadata in &self.variables {
			if var_metadata.name == *var_name {
				return true;
			}
		}
		return false;
	}


	/// Returns the current stack pointer and increments it.
	fn stack_reserve(&mut self) -> u8 {
		let x = self.stack_idx;
		self.stack_idx += 1;
		return x;
	}


	/// Closes the current stack frame returning the stake frame to the
	/// beginning of the frame.
	fn stack_frame_end(&mut self) {
		self.stack_idx = self.stack_layers.pop().unwrap();
	}


	/// Starts a new stack frame.
	fn stack_frame_start(&mut self) {
		self.stack_layers.push(self.stack_idx);
	}

}