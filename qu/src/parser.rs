
use std::fmt;
use std::fmt::Display;

use crate::errors;
use crate::TOKEN_TYPE_NAME;
use crate::vm::OPLIB;
use crate::QuToken;
use crate::QuMsg;


pub const FLOW_IF:u8 = 0;
pub const FLOW_WHILE:u8 = 1;
pub const FLOW_FOR:u8 = 2;
pub const FLOW_ELIF:u8 = 3;
pub const FLOW_ELSE:u8 = 4;

pub const KEYWORD_CLASS:&str = "cl";
pub const KEYWORD_ELSE:&str = "else";
pub const KEYWORD_ELIF:&str = "elif";
pub const KEYWORD_FUNCTION:&str = "fn";
pub const KEYWORD_IF:&str = "if";
pub const KEYWORD_PRINT:&str = "print";
pub const KEYWORD_TRAIT:&str = "tr";
pub const KEYWORD_TRAIT_IMPL:&str = "does";
pub const KEYWORD_VAR:&str = "vl";
pub const KEYWORD_WHILE:&str = "while";

pub const OP_ASSIGN_WORD:&str = "=";
pub const OP_BLOCK_START_WORD:&str = ":";


#[derive(Debug, Clone)]
/// A Qu instruction.
pub enum QuLeaf {
	/// A Block of leafs.
	Block(Vec<QuLeaf>),
	/// A floating expression
	Expression(QuLeafExpr),
	/// An if statement. Contains an assertion statement and a [`Vec`] of
	/// instructions.
	FlowStatement(u8, QuLeafExpr, Box<QuLeaf>),
	/// A function declaration branch.
	FnDecl(String, Box<QuLeaf>),
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
		for _ in 0..indent {
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
			QuLeaf::Expression(expr_leaf) => {
				return format!("{}EXPR {}", indentstr, expr_leaf);
			}
			QuLeaf::FlowStatement(op, cond, body
					) => {
				let bodystr = body.tree_fmt(indent + 1);
				return format!("{}FLOW {} {} {}", indentstr, op, cond, bodystr);
			}
			QuLeaf::FnDecl(name, body) => {
				let bodystr = body.tree_fmt(indent + 1);
				return format!("{}DECL FN {} {}", indentstr, name, bodystr);
			}
			QuLeaf::Print(register) => {
				return format!("{}PRINT {}", indentstr, register);
			}
			QuLeaf::VarAssign(name, val) => {
				return format!("{}ASSIGN {} = {}", indentstr, name.text, val);
			}
			QuLeaf::VarDecl(name, _var_type,
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


#[derive(Debug, Clone)]
/// Defines an expression in a Qu program tree.
pub enum QuLeafExpr {
	/// Call function branch.
	FnCall(String), // TODO: Implement arguments
	/// A calculable expression. Contains an operator and two [`QuLeafExpr`]s.
	Equation(u8, Box<QuLeafExpr>, Box<QuLeafExpr>),
	/// A literal int value.
	Int(u64),
	/// A variable name.
	Var(QuToken),
} impl Display for QuLeafExpr {

	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			QuLeafExpr::FnCall(name) => {
				return write!(f, "FnCall({name})");
			}
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
		}
	}

}


/// A parser for Qu scripts.
pub struct QuParser<'a> {
	/// The current indentation of the script.
	indent:u8,
	/// The current line being paresd.
	line:usize,
	/// The current [QuToken] being anylized.
	tk_idx:usize,
	/// The saved token indexs to return to.
	tk_stack:Vec<usize>, // TODO: Fix tk_stack mem-leak
	/// The [QuTokens] being parsed.
	tokens:&'a Vec<QuToken>,

} impl<'a> QuParser<'a> {

	// instantiated. Instead, it should be passed to the parse function.
	/// Creates and returns a new [QuParser].
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


	/// Attempts to parse a code block.
	fn ck_code_block(&mut self) -> Result<Option<Vec<QuLeaf>>, QuMsg> {
		let mut leafs = vec![];

		macro_rules! ck_parse {
			($fn_name:ident) => {
				if let Some(data) = self.$fn_name()? {
					leafs.push(data);
					continue;
				}
			};
		}

		while self.tk_idx < self.tokens.len()-1 {
			// Variable declaration
			ck_parse!(ck_var_decl);

			// Variable assignment
			ck_parse!(ck_var_assign);

			// If Statement
			ck_parse!(ck_flow_if);

			// while Statement
			ck_parse!(ck_flow_while);

			// Print Statement
			ck_parse!(ch_print);

			// Function declaration
			ck_parse!(ck_fn_decl);

			// Expressions
			if !self.utl_statement_start()?.is_none() {
				if let Some(expr_leaf) = self.ck_expr()? {
					leafs.push(QuLeaf::Expression(expr_leaf));
					continue;
				}
			}

			break;
		}

		if leafs.len() == 0 {
			// TODO: Add real token to QuMsg
			return Err(QuMsg::missing_code_block()
			);
		}

		return Ok(Some(leafs));
	}


	/// Attempts to pasrse a code scope.
	fn ck_code_scope(&mut self) -> Result<Option<Vec<QuLeaf>>, QuMsg> {
		self.tk_state_save();

		// Check operator
		let start_tk = self.tk_next()?;
		if start_tk != OP_BLOCK_START_WORD {
			self.tk_state_pop();
			return Ok(None);
		}

		self.indent += 1;
		let block_data = self.ck_code_block();
		self.indent -= 1;
		return block_data;
	}


	/// Attempts to parse a flow statement (Exp: if, while, for, etc).
	fn ck_flow(&mut self, token_type:u8) -> Result<Option<QuLeaf>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		self.tk_state_save();

		// Check keyword
		let keyword = match token_type {
			FLOW_IF => "if",
			FLOW_WHILE => "while",
			_ => unimplemented!(),
		};
		let keyword_tk = self.tk_next()
				.expect("Improper indentation TODO: Bette message");
		if keyword_tk != keyword {
			self.tk_state_pop();
			return Ok(None);
		}

		// Check expression
		let expr = self.ck_expr()?.ok_or(
			QuMsg::flow_statement_lacks_expression()
		)?;

		// Check for code block
		let scope_data = match self.ck_code_scope() {
			Ok(leaf) => leaf.ok_or(
				QuMsg::missing_code_block()
			)?,
			Err(msg) => {
				// If the error is related to indentation, replace the error
				// with a missing value error
				if msg.title == errors::ERR_TITLE_INVALID_INDENTATION {
					self.tk_idx -= 1;
					return Err(QuMsg::missing_code_block_in_flow());
				}
				// Return normal error
				return Err(msg);
			}
		};

		return Ok(Some(
			QuLeaf::FlowStatement(
				token_type,
				expr,
				Box::new(QuLeaf::Block(scope_data)))
		));
	}


	/// Attempts to parse an if statement.
	fn ck_flow_if(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		return self.ck_flow(FLOW_IF);
	}


	/// Attempts to parse a while statement.
	fn ck_flow_while(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		return self.ck_flow(FLOW_WHILE);
	}


	/// Attempts to parse a function call.
	fn ck_fn_call(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}
		
		self.tk_state_save();

		// Check function name
		let fn_name_opt = self.ck_fn_name()?;

		let fn_name_tk = match fn_name_opt {
			Some(name) => name,
			None => {return Ok(None)},
		};

		// Match an open '('
		let opening_p = match self.tk_next() {
			Ok(tk) => tk,
			Err(_) => {return Ok(None)},
		};
		if opening_p!= "(" {
			self.tk_state_pop();
			return Ok(None);
		}

		// Match a close ')'
		if self.tk_next()? != ")" {
			return Err(QuMsg::missing_token(")"));
		}

		return Ok(Some(QuLeafExpr::FnCall(fn_name_tk.text)));
	}


	/// Attempts to parse a function definition.
	fn ck_fn_decl(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}
		
		self.tk_state_save();

		// Check keyword
		let keyword_tk = self.tk_next()
				.expect("Improper indentation TODO: Bette message")
				.clone();
		if keyword_tk != KEYWORD_FUNCTION {
			// Check failed. Stop checking silently
			self.tk_state_pop();
			return Ok(None);
		}

		// Check function name
		let fn_name_tk = self.ck_fn_name()?.ok_or_else(
			|| {
				// TODO: Change to more appropriate message
				QuMsg::missing_code_block()
			}
		)?;

		// Match an open '('
		if self.tk_next()? != "(" {
			return Err(QuMsg::missing_token("("));
		}

		// Match a close ')'
		let closing_paren = self.tk_next()?.clone();
		if closing_paren != ")" {
			return Err(QuMsg::missing_token(")"));
		}

		// Check for code block
		let scope_leafs = self.ck_code_scope()?.ok_or(
			QuMsg::missing_code_block()
		)?;

		return Ok(Some(
			QuLeaf::FnDecl(
				fn_name_tk.text.clone(),
				Box::new(QuLeaf::Block(scope_leafs)))
		));
	}


	/// Attempts to parse a function name.
	fn ck_fn_name(&mut self) -> Result<Option<QuToken>, QuMsg> {
		// TODO: Implement function specific check for names
		return self.ck_var_name();
	}


	/// Attempts to parse a print statement.
	fn ch_print(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		// Match keyword
		let keyword_tk = self.tk_spy(0);
		if keyword_tk != KEYWORD_PRINT {
			return Ok(None);
		}
		self.tk_next().expect("Improper indentation TODO:Better msg");

		// Match register
		// Hack: Using unwrap and expect rather than properly handling errors.
		// 	Probobly isn't an issue since the print keyword is a hack and will
		// 	be removed anyway.
		let reg_tk = self.ck_expr()
			.unwrap() 
			.expect("Print needs number TODO: Better msg");

		return Ok(Some(QuLeaf::Print(reg_tk)));
	}


	/// Attempts to parse an expression
	fn ck_expr(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_op_les();
	}


	/// Attempts to parse a lesser than expression.
	fn ck_op_les(&mut self) -> Result<Option<QuLeafExpr>, QuMsg>{
		return self.ck_operation("<", &Self::ck_op_grt);
	}


	/// Attempts to parse a greater than expression.
	fn ck_op_grt(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation(">", &Self::ck_op_eql);
	}


	/// Attempts to parse an equal to expression.
	fn ck_op_eql(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation("==", &Self::ck_op_not_eql);
	}


	/// Attempts to parse a not equal to expression.
	fn ck_op_not_eql(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation("!=", &Self::ck_op_sub);
	}


	/// Attempts to parse a subtraction expression.
	fn ck_op_sub(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation("-", &Self::ck_op_add);
	}


	/// Attempts to parse an addition expression.
	fn ck_op_add(&mut self) -> Result<Option<QuLeafExpr>, QuMsg>  {
		return self.ck_operation("+", &Self::ck_op_div);
	}


	/// Attempts to parse a division expression.
	fn ck_op_div(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation("/", &Self::ck_op_mul);
	}


	/// Attempts to parse a multiplication expression.
	fn ck_op_mul(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation("*", &Self::ck_op_paren_expr);
	}


	/// Attempts to parse a parenthesized expression.
	fn ck_op_paren_expr(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		self.tk_state_save();

		let tk = self.tk_next()?;
		if tk != "(" {
			self.tk_state_pop();
			self.tk_state_save();

			// Match for a value if no parenthesis expression is matched.
			let data = self.ck_value()?;
			if let None = data {
				self.tk_state_pop();
				return Ok(None);
			}
			return Ok(data);
		}

		let data = self.ck_expr()?;
		if let None = data {
			self.tk_state_pop();
			return Ok(None);
		}

		let closing_tk = self.tk_next()?;
		if closing_tk != ")" {
			return Err(QuMsg::unclosed_paren_expr());
		}

		return Ok(data);
	}


	/// A helper function for checking operations like addition or equality.
	fn ck_operation(
			&mut self, operator:&str,
			next:&dyn Fn(&mut Self)->Result<Option<QuLeafExpr>, QuMsg>,
			) -> Result<Option<QuLeafExpr>, QuMsg> {

		self.tk_state_save();

		// Check left side for value
		let data_l = next(self)?;
		if let None = data_l {
			self.tk_state_pop();
			return Ok(None);
		}
		let data_l = data_l.unwrap();

		// Check operator
		let tk_op = self.tk_spy(0);
		if tk_op != operator {
			return Ok(Some(data_l));
		}
		self.tk_next().expect("Improper indentation TODO: Bette message");

		// Check right side for expression
		let data_r = self.ck_operation(operator, next)?;
		if let None = data_r {
			self.tk_state_pop();
			return Ok(None);
		}
		let data_r = data_r.unwrap();

		return Ok(Some(
			QuLeafExpr::Equation(
				OPLIB.op_id_from_symbol(operator),
				Box::new(data_l),
				Box::new(data_r)
			)
		));
	}


	/// Attempts to parse a type name.
	fn ck_type_name(&mut self) -> Result<Option<QuToken>, QuMsg> {
		// TODO: Implement type specific check for names
		return self.ck_var_name();
	}


	/// Attempts to parse a value.
	fn ck_value(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		self.tk_state_save();
		// Check function call
		if let Some(leaf) = self.ck_fn_call()? {
			return Ok(Some(leaf));
		}
		self.tk_state_pop();

		self.tk_state_save();
		let tk = self.tk_next()?;

		return match tk.text.parse::<u64>() {
			Ok(x) => Ok(Some(QuLeafExpr::Int(x))),
			Err(_) => {
				self.tk_state_pop();
				let var_name_opt = self.ck_var_name()?;
				match var_name_opt {
					// Matched a value
					Some(data) =>
						Ok(Some(QuLeafExpr::Var(data))),
					// Didn't match a value, return None
					None => Ok(None),
				}
			},
		};
	}



	/// Attempts to parse a variable assignment.
	fn ck_var_assign(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		self.tk_state_save();

		// Match variable name
		let name_data_opt = self.ck_var_name()?;
		let name_tk = match name_data_opt {
			Some(name_data) => name_data,
			None => {return Ok(None);},
		};
		
		
		// Match assign operator
		let assign_op_tk = self.tk_next()?;
		if assign_op_tk != OP_ASSIGN_WORD {
			self.tk_state_pop();
			return Ok(None);
		}

		// Match expression
		let expr_leaf = match self.ck_expr()? {
			Some(expr_data) => expr_data,
			None => {
				self.tk_state_pop();
				let false_expr = self.tk_spy(2).clone();
				return Err(QuMsg::var_assign_invalid_value(
					&name_tk.text, &false_expr.text
				));
			},
		};

		return Ok(Some(
			QuLeaf::VarAssign(name_tk, expr_leaf)
		));
	}


	/// Attempts to parse a variable declaration.
	fn ck_var_decl(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		self.tk_state_save();
		
		// Match keyword
		let keyword_tk = self.tk_next()?;
		if keyword_tk != KEYWORD_VAR {
			self.tk_state_pop();
			return Ok(None);
		}
		

		// Match variable name
		let name_tk = self.ck_var_name()?.ok_or_else(
			|| {
				QuMsg::general(
					"QuToken after 'var' does not match a name. 'TODO:Better msg'"
				)
			}
		)?;

		// Match variable type
		let type_tk_opt = self.ck_type_name()?;

		// Match assign operator
		let operation_tk = self.tk_spy(0);
		let mut assign_leaf_opt = None;
		if operation_tk == OP_ASSIGN_WORD {
			self.tk_next()?;
			assign_leaf_opt = match self.ck_expr() {
				Ok(leaf) => leaf,
				Err(msg) => {
					// If the error is related to indentation, replace the error
					// with a missing value error
					if msg.title == errors::ERR_TITLE_INVALID_INDENTATION {
						self.tk_idx -= 1;
						return Err(QuMsg::var_assign_lacks_value(
							&name_tk.text))
					}
					// Return normal error
					return Err(msg);
				}
			};
			if let None = assign_leaf_opt {
				return Err(QuMsg::general(
					"Expected expression after '='. TODO:Better msg"));
			}
		}
		
		return Ok(Some(QuLeaf::VarDecl(
			name_tk,
			type_tk_opt,
			assign_leaf_opt,
		)));
	}


	/// Attempts to parse a variable name.
	fn ck_var_name(&mut self) -> Result<Option<QuToken>, QuMsg> {
		let tk = self.tk_spy(0);

		if tk.tk_type != TOKEN_TYPE_NAME {
			return Ok(None);
		}
		let tk = tk.clone();

		self.tk_next()?;

		return Ok(Some(tk));
	}


	/// Parses a Qu script.
	pub fn parse(&mut self) -> Result<QuLeaf, QuMsg> {
		self.tk_idx = 0;
		self.line = 0;
		self.indent = u8::MAX;

		let leafs_res = self.ck_code_block();
		match leafs_res {
			Ok(leafs_op) => {
				if self.tk_idx != self.tokens.len()-1 {
					// Parsing ended early, must be an unexpected token
					panic!();
//					let mut msg = QuMsg::invalid_token(
//						&self.tk_spy(0).text
//					);
//					msg.token = self.tk_spy(0).clone();
//					return Err(msg);
				}

				let leafs = leafs_op.ok_or(
					QuMsg::general(
						"TODO: Proper error for the function 'parse' not getting a vec of leafs")
				)?;
				return Ok(QuLeaf::Block(leafs));
			}
			Err(mut msg) => {
				msg.token = self.tk_spy(0).clone();
				return Err(msg);
			}
		}
	}


	/// A helper function for whenever starting to parse a statement.
	fn utl_statement_start(&mut self) -> Result<Option<()>, QuMsg> {
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
				return Err(QuMsg::one_liner());
			}

			self.line = tk_row;
			return Err(QuMsg::invalid_indent());
		}
		
		self.line = tk_row;
		return Ok(Some(()));
	}


	/// Returns the next token to parse.
	/// 
	/// Returns [`Err`] if the a parser error, Although the
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
	fn tk_next(&mut self) -> Result<&QuToken, QuMsg> {
		let (line, indent) = (self.line, self.indent);
		let tk = &self.tokens[self.tk_idx];

		// Check for proper indentation
		if tk.row != line as u64 {
			if tk.indent != indent+2 {
				//panic!("");
				return Err(QuMsg::invalid_indent());
			}
		}

		self.tk_idx += 1;
		return Ok(tk);
	}


	/// Returns to a previously saved token index.
	fn tk_state_pop(&mut self) {
		self.tk_idx = self.tk_stack.pop().unwrap();
	}


	/// Saves a the curent token index to return to if a parse attempt fails.
	/// see [func@`QuParser::tk_pop`]
	fn tk_state_save(&mut self) {
		self.tk_stack.push(self.tk_idx);
	}


	/// Returns a &[QuToken] relative to the current token index without
	/// incrementing the current token index.
	/// 
	/// This function will not check if the token follows indentation rules.
	fn tk_spy(&mut self, at:usize) -> &QuToken {
		if self.tk_idx+at >= self.tokens.len() {
			return &self.tokens[self.tokens.len()-1];
		}
		return &self.tokens[self.tk_idx+at];
	}

}
