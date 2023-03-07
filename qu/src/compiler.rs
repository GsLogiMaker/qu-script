
use crate::QuFunctionId;
use crate::QuInt;
use crate::QuOp;
use crate::QuOp::*;
use crate::QuParser;
use crate::QuRegisterStruct;
use crate::QuStackId;
use crate::import::QuRegistered;
use crate::import::QuStructId;
use crate::parser::FLOW_TYPE_IF;
use crate::parser::FLOW_TYPE_WHILE;
use crate::parser::KEYWORD_IF;
use crate::parser::QuOperator;
use crate::parser::QuParamNode;
use crate::parser::parsed::*;

use crate::QuMsg;
use crate::QuToken;
use crate::QuType2;
use crate::vm::QuConstId;

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

#[derive(Debug, Clone)]
struct ClassMetadata {
	/// The name of this class.
	name: String,
	/// The ID of this class.
	id: usize,
	/// A map of names to this class's function IDs.
	id_map: HashMap<String, usize>,
	/// A list of compiled fuctions in this class.
	methods: Vec<FunctionMetadata>,
} impl ClassMetadata {
	fn new() -> Self {
		Self {
			name: "".to_owned(),
			..Default::default()
		}
	}
} impl Default for ClassMetadata {
    fn default() -> Self {
        Self {
			name: "void".to_owned(),
			..Default::default()
		}
    }
}


#[derive(Debug, Default, Clone)]
struct Definitions {
	/// A map of names to module IDs.
	id_map: HashMap<String, usize>,
	/// A list of all compiled modules
	modules: Vec<ModuleMetadata>
}


#[derive(Debug, Default, Clone)]
struct ModuleMetadata {
	/// The name of this module.
	name: String,
	/// The ID of this module.
	id: usize,
	/// A map of names to class and function IDs.
	id_map: HashMap<String, usize>,
	/// A list of all compiled classes in the module.
	classes: Vec<ClassMetadata>,
	/// A list of all compiled global functions in the module.
	functions: Vec<FunctionMetadata>,		
}


#[derive(Debug, Default, Clone)]
struct FunctionMetadata {
	name: String,
	id: u32,
	parameters: Vec<()>,
	retur_type_id: u32,
}


struct QuCmpContext {
	var_identities:Vec<QuVarIdentity>,
	stack_frames:Vec<QuCmpFrame>,
} impl QuCmpContext {

	fn new() -> Self {
		return Self{
			var_identities: Vec::default(),
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
pub struct QuCompiler<'a> {
	contexts:Vec<QuCmpContext>,
	constants_code:Vec<u8>,
	name_refs:HashMap<String, u32>,
	types:Vec<QuType2>,
	types_map:HashMap<String, usize>,
	definitions: Definitions,
	imports: Option<&'a QuRegistered>
} impl<'a> QuCompiler<'a> {

	/// Creates and returns a new [QuCompiler].
	pub fn new() -> Self {
		let mut inst = Self {
			contexts: Vec::default(),
			constants_code: Vec::default(),
			name_refs: HashMap::default(),
			types: vec![QuType2::int(), QuType2::uint(), QuType2::bool()],
			types_map: HashMap::new(),
			definitions: Definitions::default(),
			imports: None,
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


	fn add_variable(&mut self, var_identity:QuVarIdentity) {
		self.contexts_get_top().var_identities.push(var_identity);
		self.context_get_top_frame().var_refs_added += 1;
	}


	/// Compiles an expression into bytecode.
	fn cmp_expr(
		&mut self, expression:&Expression, output_reg:QuStackId
	) -> Result<QuAsmBuilder, QuMsg> {
		return match expression {
			Expression::Call(
				call_expression,
			) => self.cmp_fn_call(
				&call_expression.name,
				&call_expression.parameters,
				output_reg
			),
			Expression::Operation(
				operation_expression,
			) => self.cmp_expr_operation(
				&operation_expression.operator,
				&operation_expression.left,
				&operation_expression.right,
				output_reg,
			),
			Expression::Number(number)
				=> Ok(self.cmp_expr_int(&number.value, output_reg)),
			Expression::Tuple(tuple)
				=> self.cmp_expr_tuple(
					&tuple.elements,
					output_reg,
				),
			Expression::Var(var)
				=> self.cmp_expr_val(&var.name, output_reg),
		};
	}


	/// Compiles a math or logic expression into bytecode.
	fn cmp_expr_operation(
		&mut self,
		operator: &QuToken,
		left: &Expression,
		right: &Expression,
		// TODO: Change output_reg to a struct without type information
		output_reg: QuStackId,
	)-> Result<QuAsmBuilder, QuMsg> {
		let left_reg = if let Expression::Var(_) = left {
			self.get_expr_reg(&left)?
		} else { output_reg };

		let right_reg = if let Expression::Var(_) = right {
			self.get_expr_reg(&right)?
		} else {
			if left_reg == output_reg {
				self.get_expr_reg(&right)?
			} else { output_reg }
		};

		let left_b = self.cmp_expr(&left, left_reg)?;
		let right_b = self.cmp_expr(&right, right_reg)?;
		let function_name = QuOperator::from_symbol(&operator.slice).get_function_name();

		let mut builder = QuAsmBuilder::new();
		

		// Set builder return type
		let fn_id = self.imports.unwrap()
			.get_struct(&left_b.return_type)?
			.get_fn_id(function_name)?;
		let return_type = self.imports.unwrap()
			.get_struct_by_id(
				self.imports.unwrap().get_fn_data_by_id(fn_id)?.return_type
			)?
			.name.clone();
		builder.return_type = return_type.clone();

		// Generate code
		builder.add_builder(left_b);
		builder.add_builder(right_b);
		builder.add_bp(QuBuilderPiece::ReprCallExt(
			function_name.into(),
			vec![left_reg, right_reg],
			output_reg
		));
		
		return Ok(builder);
	}


	/// Compiles a constant integer expression into bytecode.
	/// 
	/// # Panics
	/// 
	/// Panics if `val` can't be parsed to a number.
	fn cmp_expr_int(
		// TODO: Change output_reg to a struct without type information
		&mut self, value:&QuToken, output_reg:QuStackId
	) -> QuAsmBuilder {
		// TODO: Support other int sizes
		
		let Ok(val) = value.slice.parse::<isize>() else {
			panic!("Could not convert text '{}' to number!", value.slice);
		};

		let mut b = QuAsmBuilder::new();
		b.add_op(Value(val, output_reg));
		b.return_type = "int".into();

		return b;
	}


	/// Compiles a tuple construction into bytecode.
	fn cmp_expr_tuple(
		&mut self, elements:&Vec<Expression>, to_reg:QuStackId
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut b = QuAsmBuilder::new();
		let mut i:u8 = to_reg.into();
		for item in elements {
			b.add_builder(self.cmp_expr(item, i.into())?);
			i += 1;
		}

		return Ok(b);
	}


	/// Compiles a variable-expression into bytecode.
	fn cmp_expr_val(
		&mut self, name:&QuToken, output_reg:QuStackId
	) -> Result<QuAsmBuilder, QuMsg> {

		// TODO: Error handling
		let var_identity = self.get_var_identity(&name.slice).unwrap();
		
		// Copying to same register location, return nothing
		if var_identity.id == output_reg {
			let mut b = QuAsmBuilder::default();
			b.return_type = var_identity.static_type.clone();
			return Ok(b);
		}

		let mut b = QuAsmBuilder::new();
		b.add_bp(QuBuilderPiece::ReprCallExt(
			"copy".into(),
			vec![var_identity.id],
			output_reg,
		));

		b.return_type = var_identity.static_type.clone();

		return Ok(b);
	}


	/// Compiles an *if* statement into bytecode.
	fn cmp_flow_if(
		&mut self, mut condition:&Expression, body:&CodeScope,
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get expression register
		let mut expr_b = with!{
			self.stack_frame_start, self.stack_frame_end {
				let expr_reg = self.get_expr_reg(&mut condition)?;
				self.cmp_expr(condition, expr_reg)
			}
		}?;

		// New frame for the code in the 'if' body
		let b:Result<QuAsmBuilder, QuMsg> = with!{
			self.stack_frame_start, self.stack_frame_end {
				let body_code = self.cmp_scope(body)?;

				let mut b = QuAsmBuilder::new();
				b.add_op(JumpByIfNot(body_code.len() as isize));
				b.add_builder(body_code);

				/*return*/ Ok(b)
			}
		};

		expr_b.add_builder(b?);

		return Ok(expr_b);
	}


	/// Compiles a *while* statement into bytecode.
	fn cmp_flow_while(
		&mut self, mut condition:&Expression, body:&CodeScope,
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
				skip_body_b.add_op(JumpByIfNot(block_code_len as isize + 1));
				let skip_body_b_len = skip_body_b.len();
				b.add_builder(skip_body_b);

				// Loop body
				b.add_builder(block_code);

				// Jump back to expression
				b.add_op(JumpBy(-((
					block_code_len
					+ skip_body_b_len
					+ expr_code_len
					+ 1
				) as isize)));

				Ok::<QuAsmBuilder, QuMsg>(b)
			}
		}?;
		
		return Ok(code);
	}


	fn cmp_fn_call(
		&mut self,
		name:&QuToken,
		parameters:&TupleExpression,
		store_to:QuStackId,
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut builder = QuAsmBuilder::new();

		with!(self.stack_frame_start, self.stack_frame_end {
			for p in &parameters.elements {
				// TODO: Support more than int
				let object_id = self.imports.unwrap().get_struct_id("int")?;
				let reg = self.stack_reserve(object_id)?;
				let parameter_expr = self.cmp_expr(p, reg)?;
				builder.add_builder(parameter_expr);
			}
			Ok::<(), QuMsg>(())
		})?;

		builder.add_bp(
			QuBuilderPiece::ReprCall(name.slice.clone(), store_to)
		);

		return Ok(builder);
	}


	fn cmp_fn_decl(
		&mut self,
		identity:&FunctionIdentity,
		body:&CodeScope,
	) -> Result<QuAsmBuilder, QuMsg> {
		self.add_name_ref(identity.name.slice.clone());

		let needs_added_end_op = match body.code_block.statements.last() {
			Some(Statement::Return(_)) => false,
			_ => true,
		};

		let body_code
			= with!(self.context_start, self.context_end {

			// Add return value variable for padding.
			let object_id = self.imports.unwrap().get_struct_id("int")?;
			let id = self.stack_reserve(object_id)?;
			self.add_variable(QuVarIdentity::new(
				"return value".to_owned(),
				"int".to_owned(),
				id,
			));

			// Compile parameters
			for p in &identity.parameters {
				let static_type = match &p.static_type {
					Some(token) => token.slice.clone(),
					None => "int".to_owned(),
				};
				let object_id = self.imports.unwrap().get_struct_id(&static_type)?;
				let id = self.stack_reserve(object_id)?;
				self.add_variable(QuVarIdentity::new(
					p.name.slice.to_owned(),
					static_type.to_owned(),
					id,
				));
			}

			// Compile code block
			let mut b = QuAsmBuilder::new();
			b.add_builder(self.cmp_code_block(&body.code_block)?);
			if needs_added_end_op {
				b.add_op(End);
			}
			Ok::<QuAsmBuilder, QuMsg>(b)
		})?;

		// Add fn declaration operation
		let mut code = QuAsmBuilder::new();
		code.add_op(DefineFn(identity.name.slice.to_owned(), body_code.len() as usize));

		// Add body
		code.add_builder(body_code);

		return Ok(code);
	}


	/// Compiles a [QuLeaf] into bytecode.
	fn cmp_statement(&mut self, statement:&Statement) -> Result<QuAsmBuilder, QuMsg> {
		match statement {
			Statement::Expression(expression) => {
				return with!(self.stack_frame_start, self.stack_frame_end {
					let reg = self.get_expr_reg(&expression)?;
					self.cmp_expr(expression, reg)
				});
			}
			Statement::FlowStatement(flow_statement) => {
				match flow_statement.flow_keyword.slice.as_str() {
					KEYWORD_IF => {
						return self.cmp_flow_if(
							&flow_statement.condition,
							&flow_statement.body,
						);
					},
					KEYWORD_WHILE => {
						return self.cmp_flow_while(
							&flow_statement.condition,
							&flow_statement.body
						);
					}
					_ => unimplemented!(),
				}
			}
			Statement::FunctionDeclaration(function_declaration) => {
				// TODO: Compiler fn declaration parameters
				return self.cmp_fn_decl(
					&function_declaration.identity,
					&function_declaration.body,
				);
			}
			Statement::Return(return_statement) => {
				let mut code = QuAsmBuilder::new();
				match &return_statement.value {
					Some(value) => {
						let reg = self.get_expr_reg(value)?;
						code.add_builder(self.cmp_expr(value, reg)?);
						code.add_bp(QuBuilderPiece::ReprCallExt(
							"copy".into(),
							vec![reg],
							0.into()
						));
						if self.contexts.len() == 1 {
							code.add_op(Return(reg.struct_id()));
						} else {
							code.add_op(End);
						}
					},
					None => {},
				}
				return Ok(code);
			}
			Statement::VarDeclaration(var_declaration) => {
				return self.cmp_var_decl(
					&var_declaration.name,
					&var_declaration.static_type,
					&var_declaration.initial_value
				);
			}
			Statement::VarAssign(variable_assignment) => {
				return  self.cmp_var_assign(
					&variable_assignment.name,
					&variable_assignment.new_value,
				);
			}
		};
	}


	/// Compiles a [`Vec<QuLeaf>`] into bytecode.
	fn cmp_code_block(
		&mut self,
		code_block:&CodeBlock,
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut b = QuAsmBuilder::new();
		for statements in &code_block.statements {
			b.add_builder(self.cmp_statement(statements)?);
		}
		return Ok(b);
	}


	fn cmp_module(&mut self) {
		
	}


	/// Compiles code variable assignment.
	fn cmp_var_assign(
		&mut self,
		name:&QuToken,
		new_value:&Expression,
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get variable register
		let var_reg
			= self.get_var_register(&name.slice)
				.ok_or_else(||{
					let mut msg = QuMsg::undefined_var_assign(
						&name.slice);
					msg.token = name.char_index.clone();
					return msg;
				}
			)?
		;
		// Compile assignment to expression
		return self.cmp_expr(new_value, var_reg);
	}


	/// Compiles a variable declaration.
	fn cmp_var_decl(
		&mut self,
		name:&QuToken,
		static_type:&Option<QuToken>,
		initial_value:&Option<Expression>
	) -> Result<QuAsmBuilder, QuMsg> {

		// Check if the variable is already defined
		if self.is_var_defined(&name.slice) {
			let mut msg = QuMsg::var_redefined(&name.slice);
			msg.token = name.char_index.clone();
			return Err(msg);
		}

		// Get var type
		let var_type = if let Some(type_tk) = static_type {
			type_tk.slice.clone()
		} else {
			"int".to_owned()
		};

		// Create variable
		let object_id = self.imports.unwrap().get_struct_id(&var_type)?;
		let var_reg = self.stack_reserve(object_id)?;
		self.add_variable(QuVarIdentity {
			name: name.slice.to_owned(),
			static_type: var_type,
			id: var_reg,
		});

		// Compile variable assignment
		return match initial_value {
			// Compile variable value
			Some(expression)
				=> self.cmp_expr(expression, var_reg),

			// No default value, compile fallback to zero
			None => {
				// TODO: Support u64
				let mut code = QuAsmBuilder::new();
				code.add_op(Value(0, var_reg));
				Ok(code)
			},
		};
	}


	/// Compiles a scope.
	fn cmp_scope(&mut self, code_scope:&CodeScope) -> Result<QuAsmBuilder, QuMsg> {
		let compiled
			= with!{self.stack_frame_start, self.stack_frame_end {
				self.cmp_code_block(&code_scope.code_block)
			}
		};
		return compiled;
	}


	/// Compiles Qu code from a [`&str`] into a [`Vec<u8>`].
	pub fn compile(&mut self, code:&str, imports:&'a QuRegistered
	) -> Result<Vec<QuOp>, QuMsg> {
		self.imports = Some(imports);
		let mut p = QuParser::new();
		let code_block = p.parse(code)?;
		let compiled = self.compile_code(&code_block)?;
		self.imports = None;
		Ok(compiled)
	}


	/// Compiles Qu code from a [QuLeaf] into a [`Vec<u8>`].
	pub fn compile_code(
		&mut self, code_block:&CodeBlock
	) -> Result<Vec<QuOp>, QuMsg> {
		// Main code
		let mut code = with!(self.context_start, self.context_end {
			let mut code = self.cmp_code_block(code_block)?;
			code.add_op(End);
			Ok::<QuAsmBuilder, QuMsg>(code)
		})?;


		Ok(code.compile(
			&self.name_refs,
			self.imports.unwrap(),
		)?)
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
	fn get_expr_reg(
		&mut self, expr_leaf:&Expression
	) -> Result<QuStackId, QuMsg> {
		return  match expr_leaf {
			Expression::Operation(_) => Ok(self.stack_reserve(self.imports.unwrap().get_struct_id("int")?)?),
			Expression::Call(_) => Ok(self.stack_reserve(self.imports.unwrap().get_struct_id("int")?)?),
			Expression::Number(_) => Ok(self.stack_reserve(self.imports.unwrap().get_struct_id("int")?)?),
			Expression::Tuple(_) => Ok(self.stack_reserve(self.imports.unwrap().get_struct_id("int")?)?),
			Expression::Var(var) => {
				self.get_var_register(&var.name.slice)
					.ok_or_else(||{
						let mut msg = QuMsg::undefined_var_access(
							&var.name.slice
						);
						msg.token = var.name.char_index.clone();
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


	/// Gets the identity of a variable by the variable's name.
	fn get_var_identity(&mut self, var_name:&str) -> Option<&QuVarIdentity> {
		let mut i = 0usize;
		for var_ref in &self.contexts_get_top().var_identities {
			if var_ref == var_name {
				return Some(var_ref);
			}
			i += 1;
		}
		return None;
	}


	/// Gets the pointer to a variable by the variable's name.
	fn get_var_register(&mut self, var_name:&str) -> Option<QuStackId> {
		let mut i = 0usize;
		for var_ref in &self.contexts_get_top().var_identities {
			if var_ref == var_name {
				return Some(var_ref.id);
			}
			i += 1;
		}
		return None;
	}


	/// Returns true if the given variable is already defined.
	fn is_var_defined(&mut self, var_name:&str) -> bool {
		// TODO: Maybe use a faster algorithm?
		for var_ref in &self.contexts_get_top().var_identities {
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
		context.var_identities.resize(
			context.var_identities.len()-(frame.var_refs_added as usize),
			QuVarIdentity::default(),
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
	fn stack_reserve(&mut self, object: QuStructId) -> Result<QuStackId, QuMsg> {
		let object_size = self.imports
			.unwrap()
			.get_struct_by_id(object)?
			.size;
		let mut top_frame = self.context_get_top_frame();
		let index = top_frame.stack_idx;
		top_frame.stack_idx += object_size;
		return Ok(QuStackId::new(index as usize, object));
	}

}


#[derive(Debug, Clone)]
enum QuBuilderPiece {
	ReprCall(String, QuStackId),
	// Struct name, fn name, args, output
	ReprCallExt(String, Vec<QuStackId>, QuStackId),
	/// A [`Vec<u8>`] of code.
	Ops(Vec<QuOp>),
} impl QuBuilderPiece {

	fn len(&self) -> usize{
		match self {
			QuBuilderPiece::ReprCall(_, _) => 1,
			QuBuilderPiece::ReprCallExt(_, _, _) => 1,
			QuBuilderPiece::Ops(v) => v.len(),
		}
	}

}


#[derive(Debug, Default, Clone)]

struct QuAsmBuilder {
	code_pieces:Vec<QuBuilderPiece>,
	return_type:String,
} impl QuAsmBuilder {

	fn new() -> Self {
		return Self {
			code_pieces: vec![],
			return_type: "Void".to_owned(),
		}
	}
	

	/// Adds a builder piece
	fn add_bp(&mut self, repr:QuBuilderPiece) {
		self.code_pieces.push(repr);
	}


	fn add_builder(&mut self, mut builder:QuAsmBuilder) {
		self.code_pieces.append(&mut builder.code_pieces);
	}


	fn add_op(&mut self, op:QuOp) {
		self.code_pieces.push(QuBuilderPiece::Ops(vec![op]));
	}


	fn add_ops(&mut self, ops:Vec<QuOp>) {
		self.code_pieces.push(QuBuilderPiece::Ops(ops));
	}


	fn compile(
		&mut self, name_references:&HashMap<String, u32>, imports:&QuRegistered
	) -> Result<Vec<QuOp>, QuMsg> {
		let mut code = vec![];
		
		for x in &mut self.code_pieces {
			match x {
				QuBuilderPiece::Ops(ops) => code.append(ops),
				QuBuilderPiece::ReprCall(
					name,
					output,
				) => {
					let Some(fn_index) = name_references.get(name) else {
						panic!("Compiler could not find a function by name {name}.");
					};
					code.push(Call((*fn_index).into(), *output));
				}
    			QuBuilderPiece::ReprCallExt(
					fn_name,
					args,
					output
				) => {
					// TODO: Implement static typing
					let struct_data = imports.get_struct_by_id(
						args.get(0)
							.unwrap_or(&QuStackId::from(0))
							.struct_id()
					)?;

					let id
						= struct_data.get_fn_id(fn_name)?;
					code.push(CallExt(id, take(args), *output));
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


#[derive(Debug, Default, Clone)]
struct QuVarIdentity {
	name: String,
	static_type: String,
	id: QuStackId,
} impl QuVarIdentity {

	fn new(name:String, static_type:String, id:QuStackId) -> Self {
		QuVarIdentity{name, static_type, id}
	}

} impl PartialEq<str> for QuVarIdentity {

	fn eq(&self, other:&str) -> bool {
		&self.name == other
	} 

}

#[cfg(test)]
mod test {
}