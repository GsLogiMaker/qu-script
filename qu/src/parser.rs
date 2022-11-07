
use std::fmt;
use std::fmt::Display;

use crate::errors;
use crate::TOKEN_TYPE_NAME;
use crate::tokens::TOKEN_TYPE_NUMBER;
use crate::vm::OPLIB;
use crate::QuToken;
use crate::QuMsg;


pub const FLOW_IF:u8 = 0;
pub const FLOW_WHILE:u8 = 1;
pub const FLOW_FOR:u8 = 2;
pub const FLOW_ELIF:u8 = 3;
pub const FLOW_ELSE:u8 = 4;

pub const KEYWORD_CLASS:&str = "stamp";
pub const KEYWORD_ELSE:&str = "else";
pub const KEYWORD_ELIF:&str = "elif";
pub const KEYWORD_FUNCTION:&str = "fn";
pub const KEYWORD_IF:&str = "if";
pub const KEYWORD_PRINT:&str = "print";
pub const KEYWORD_RETURN:&str = "return";
pub const KEYWORD_VAR:&str = "var";

pub const OP_ADD_SYMBOL:&str = "+";
pub const OP_ASSIGN_SYMBOL:&str = "=";
pub const OP_BLOCK_START_WORD:&str = ":";


#[derive(Debug, Clone, PartialEq)]
enum QuAction {
	BlockEnd,
	BlockStart,
	FnDeclare,
	For,
	If,
	None,
	/// Holds an expression.
	Return(Option<Box<QuAction>>),
	VarAssign,
	/// Holds the variable ref and the assignment expression.
	VarDeclare(Box<QuAction>, Option<Box<QuAction>>),
	While,

	/// Holds two expressions.
	Add(Box<QuAction>, Box<QuAction>),
	/// Holds two expressions.
	Div(Box<QuAction>, Box<QuAction>),
	/// Holds an expression action.
	Expression(Box<QuAction>),
	/// Holds two expressions.
	Mul(Box<QuAction>, Box<QuAction>),
	/// Holds it's numeric value.
	Number(usize),
	/// Holds two expressions.
	Sub(Box<QuAction>, Box<QuAction>),
	/// Holds the name of the variable.
	VarRef(String),
} impl QuAction {

	/// Returns *true* if the [QuAction] is an expression.
	fn is_expression(&self) -> bool {
		return match self {
			QuAction::Add(_, _) => true,
			QuAction::Div(_, _) => true,
			QuAction::Expression(_) => true,
			QuAction::Mul(_, _) => true,
			QuAction::Number(_) => true,
			QuAction::Sub(_, _) => true,
			QuAction::VarRef(_) => true,
			_ => false,
		};
	}

}


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
	/// A return statement for a function
	Return(QuLeafExpr),
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
			QuLeaf::Return(val) => {
				return format!("{}RETURN {}", indentstr, val);
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
	/// A tuple.
	Tuple(Vec<QuLeafExpr>),
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
			QuLeafExpr::Tuple(items) => {
				return write!(f, "({items:?})");
			}
			QuLeafExpr::Var(name) => {
				return write!(f, "{}:Var", name.text);
			}
		}
	}

}


/// A parser for Qu scripts.
#[derive(Debug)]
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

			// Return Statement
			ck_parse!(ch_return);

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


	/// Attempts to parse an expression
	fn ck_expr(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_tuple();
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
	fn ck_op_paren_expr(&mut self
	) -> Result<Option<QuLeafExpr>, QuMsg> {
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


	/// Attempts to parse a tuple, otherwise attempts to parse a "<"
	/// expression.
	/// 
	/// A tuple is denoted by expressions separated by commas (Ex: "1,2,3").
	/// Parenthesis technicly have nothing to do with tuples.
	fn ck_tuple(&mut self) -> Result<Option<QuLeafExpr>, QuMsg>{
		match self.ck_op_les()? {
			// Matched an expression
			Some(expr) => {
				if self.tk_spy(0) == "," {
					// Matched tuple, get more tuple elements
					let mut elements = vec![expr];
					self.tk_next()?;
					loop {
						match self.ck_op_les()? {
							Some(next_expr) => {
								// Found another expression, add to tuple
								elements.push(next_expr);
								if self.tk_spy(0) != "," {
									// No comma found, tuple must have ended.
									// Return tuple.
									return Ok(Some(QuLeafExpr::Tuple(elements)));
								} else {
									// Comma found, continue adding to tuple
									self.tk_next()?;
								}
							},
							None => {
								// No more expressions found, return tuple
								return Ok(Some(QuLeafExpr::Tuple(elements)))
							},
						};
					}

				} else {
					// Did not match tuple, return expression
					return Ok(Some(expr));
				}
			},
			// Did not match an expression, return quietly
			None => {return Ok(None)},
		};
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


	/// Attempts to parse a print statement.
	fn ch_return(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		// Match return keyword
		let keyword_tk = self.tk_spy(0);
		if keyword_tk != KEYWORD_RETURN {
			return Ok(None);
		}
		self.tk_next()?;

		// Match returning expression
		let reg_tk = self.ck_expr()?.ok_or_else(
			||{
				// TODO: Proper error message
				return QuMsg::flow_statement_lacks_expression();
			}
		)?;

		return Ok(Some(QuLeaf::Return(reg_tk)));
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


	fn tk_next_option(&mut self) -> Option<&QuToken> {
		let (line, indent) = (self.line, self.indent);

		// Return null if out of range
		if self.tk_idx == self.tokens.len() {
			return None;
		}

		let tk = &self.tokens[self.tk_idx];

		// Check for proper indentation
		if tk.row != line as u64 {
			if tk.indent != indent+2 {
				return None;
			}
		}

		if tk.tk_type == u8::MAX {
			return None;
		}

		self.tk_idx += 1;
		return Some(tk);
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


	/// Returns a &[QuToken] relative to the current token index without
	/// incrementing the current token index.
	/// 
	/// This function will not check if the token follows indentation rules.
	fn tk_spy_option(&mut self, at:usize) -> Option<&QuToken> {
		if self.tk_idx+at >= self.tokens.len() {
			return None;
		}
		let token = &self.tokens[self.tk_idx+at];
		if token.tk_type == u8::MAX {
			return None;
		}
		return Some(&self.tokens[self.tk_idx+at]);
	}

}


/*

let parse_matcher = ParseMatcher::new(parser)
	.match_str("let")
	.some_else(||{ Err() })
	.finish;

let pm = ParseMatcher::new(parser)
	.match_str("let")?
	.match_variable()?.else("expected variable name")?
	.match_type()?.optional()
	.match_str("=")?.else("expected a 'e'")?
	.match_expression()?.else("expected expression")?

*/


macro_rules! settup_expr_op {
	($operator:expr, $next:ident, $repeat:ident, $action_type:ident) => {
		{
			let (l, r)
				= self.get_match_operation(
					$operator,
					&Self::$next,
					&Self::$repeat
				)?;
			let Some(l) = l else {
				self.last_op = None;
				return Ok(self);
			};
			let Some(r) = r else {
				self.last_op = Some(l);
				return Ok(self);
			};
			self.last_op = Some(QuAction::$action_type(Box::new(l), Box::new(r)));
		}
	};
}


#[derive(Debug)]
struct QuMatcher<'a, 'b> {
	parser:&'a mut QuParser<'b>,
	last_op:Option<QuAction>,
	matched:Vec<Option<QuAction>>,
} impl<'a, 'b> QuMatcher<'a, 'b> {

	fn new(parser:&'a mut QuParser<'b>) -> Self {
		return QuMatcher{
			parser,
			last_op:None,
			matched:vec![],
		}
	}


	fn dbg(&mut self) {
		dbg!(self);
	}


	fn fin(&self) -> &Vec<Option<QuAction>> {
		return &self.matched;
	}


	fn get_match_token_type(&mut self, tk_type:u8) -> Option<QuToken> {
		let tk_op = self.parser.tk_spy_option(0);

		match tk_op {
			Some(tk) => {
				if tk.tk_type == tk_type {
					let tk = Some(tk.clone());
					self.parser.tk_next_option();
					return tk;
				} else {
					return None;
				}
				
			},
			None => {
				return None;
			},
		}
	}


	/// A helper function for checking operations like addition or equality.
	fn get_match_operation(
		&mut self, operator:&str,
		next:&dyn Fn(&mut Self)->Result<&mut Self, QuMsg>,
		repeat:&dyn Fn(&mut Self)->Result<&mut Self, QuMsg>,
		) -> Result<(Option<QuAction>, Option<QuAction>), QuMsg> {

	self.parser.tk_state_save();

	// Check left side for value
	next(self)?;
	if let None = self.last_op {
		self.parser.tk_state_pop();
		return Ok((None, None));
	}
	let data_l = self.last_op.clone().unwrap();

	// Check operator
	let tk_op = self.parser.tk_spy(0);
	if tk_op != operator {
		return Ok((Some(data_l), None));
	}
	self.parser.tk_next().expect("Improper indentation TODO: Bette message");

	// Check right side for expression
	repeat(self)?;
	if let None = self.last_op {
		self.parser.tk_state_pop();
		return Ok((Some(data_l), None));
	}
	let data_r = self.last_op.clone().unwrap();

	return Ok((Some(data_l), Some(data_r)));
}


	/// Stores the last calculated match in *matched*.
	fn keep(&mut self) -> Result<&mut Self, QuMsg> {
		self.matched.push(self.last_op.clone());
		return Ok(self);
	}


	fn match_expr_sub(&mut self) -> Result<&mut Self, QuMsg> {
		let (l, r)
			= self.get_match_operation(
				"-",
				&Self::match_expr_add,
				&Self::match_expr_sub
			)?;
		let Some(l) = l else {
			self.last_op = None;
			return Ok(self);
		};
		let Some(r) = r else {
			self.last_op = Some(l);
			return Ok(self);
		};
		self.last_op = Some(QuAction::Sub(Box::new(l), Box::new(r)));
		return Ok(self);
	}


	fn match_expr_add(&mut self) -> Result<&mut Self, QuMsg> {
		let (l, r)
			= self.get_match_operation(
				"+",
				&Self::match_expr_div,
				&Self::match_expr_add
			)?;
		let Some(l) = l else {
			self.last_op = None;
			return Ok(self);
		};
		let Some(r) = r else {
			self.last_op = Some(l);
			return Ok(self);
		};
		self.last_op = Some(QuAction::Add(Box::new(l), Box::new(r)));
		return Ok(self);
	}


	fn match_expr_div(&mut self) -> Result<&mut Self, QuMsg> {
		let (l, r)
			= self.get_match_operation(
				"/",
				&Self::match_expr_mul,
				&Self::match_expr_div
			)?;
		let Some(l) = l else {
			self.last_op = None;
			return Ok(self);
		};
		let Some(r) = r else {
			self.last_op = Some(l);
			return Ok(self);
		};
		self.last_op = Some(QuAction::Div(Box::new(l), Box::new(r)));
		return Ok(self);
	}


	fn match_expr_mul(&mut self) -> Result<&mut Self, QuMsg> {
		let (l, r)
			= self.get_match_operation(
				"*",
				&Self::match_number,
				&Self::match_expr_mul
			)?;
		let Some(l) = l else {
			self.last_op = None;
			return Ok(self);
		};
		let Some(r) = r else {
			self.last_op = Some(l);
			return Ok(self);
		};
		self.last_op = Some(QuAction::Mul(Box::new(l), Box::new(r)));
		return Ok(self);
	}


	fn match_expression(&mut self) -> Result<&mut Self, QuMsg> {
		self.match_expr_sub();
		return Ok(self);
	}


	fn match_number(&mut self) -> Result<&mut Self, QuMsg> {
		let num_tk
			= self.get_match_token_type(TOKEN_TYPE_NUMBER)
				.ok_or_else(||{
					self.last_op = None;
					QuMsg::failed_parser_match()
				})?;

		let num = num_tk.text
			.parse::<usize>()
			.or_else(
				|x|{Err(QuMsg::failed_parser_match())}
			)?;

		self.last_op = Some(QuAction::Number(num));
		return Ok(self);
	}


	fn match_str(&mut self, match_against:&str) -> Result<&mut Self, QuMsg> {
		let tk_op = self.parser.tk_spy_option(0);

		match tk_op {
			Some(tk) => {
				if tk == match_against {
					self.last_op = Some(QuAction::None);
					self.parser.tk_next_option();
				} else {
					self.last_op = None;
				}
			},
			None => {
				self.last_op = None;
			},
		}

		return Ok(self);
	}


	fn match_var_decl(&mut self) -> Result<&mut Self, QuMsg> {
		self
			.match_str(KEYWORD_VAR)?
				.required()?
			.match_var_name()?
				.err(||{QuMsg::general("Variable declaration lacks a name. TODO:Better msg")})?
				.keep()?
			.match_str(OP_ASSIGN_SYMBOL)?
				.err(||{QuMsg::general("Variable declaration lacks '='. TODO:Better msg")})?
			.match_expression()?
				.err(||{QuMsg::general("Variable declaration lacks expression. TODO:Better msg")})?
				.keep()?
			;

		let (Some(a), Some(b))
			= (self.matched.remove(0), self.matched.remove(0))
		else {
			panic!();
		};

		self.matched.push(
			Some(QuAction::VarDeclare(Box::new(a), Some(Box::new(b))))
		);
		return Ok(self);
	}


	fn match_var_name(&mut self) -> Result<&mut Self, QuMsg> {
		let tk_op = self.parser.tk_spy_option(0);

		match tk_op {
			Some(tk) => {
				if tk.tk_type == TOKEN_TYPE_NAME {
					self.last_op = Some(QuAction::VarRef(tk.text.clone()));
					self.parser.tk_next_option();
				} else {
					self.last_op = None;
				}
			},
			None => {
				self.last_op = None;
			},
		}

		return Ok(self);
}


	fn err<F: FnOnce()->QuMsg>(
		&mut self, else_clause:F
	) -> Result<&mut Self, QuMsg> {
		match &self.last_op {
			Some(token) => {
				return Ok(self);
			},
			None => {
				let msg = else_clause();
				return Err(msg);
			},
		}
	}


	/// Returns an error if the previous match was unsuccesful.
	fn required(&mut self) -> Result<&mut Self, QuMsg> {
		match &self.last_op {
			Some(token) => {
				return Ok(self);
			},
			None => {
				return Err(QuMsg::failed_parser_match());
			},
		}
	}

}


#[cfg(test)]
mod test_qu_matcher {
    use crate::parser::QuAction;
    use crate::{QuParser, tokens::tokenize, tokens::RULES};
	use crate::QuMsg;

    use super::{QuMatcher, KEYWORD_VAR, OP_ASSIGN_SYMBOL};

	macro_rules! boilerplate {
		($script:expr, $matcher_symbol:ident) => {
			let tokens
				= &mut tokenize(&$script.to_owned(), RULES);
			let mut p = QuParser::new(tokens);
			let mut $matcher_symbol = QuMatcher::new(&mut p); 
		};
	}


	#[test]
	fn match_expr_add() -> Result<(), QuMsg>{
		boilerplate!("2 + 3", m);

		let matched = m
			.match_expr_add()?
				.keep()?
			.fin();

		return Ok(());
	}


	#[test]
	fn match_expression() -> Result<(), QuMsg>{
		boilerplate!("2 - 3 + 3 - 5 - 7 + 8", m);

		let matched = m
			.match_expression()?
				.keep()?
			.fin();
		
		let expected = QuAction::Sub(
			Box::new(QuAction::Number(2)),
			Box::new(QuAction::Sub(
				Box::new(QuAction::Add(
					Box::new(QuAction::Number(3)),
					Box::new(QuAction::Number(3)),
				)),
				Box::new(QuAction::Sub(
					Box::new(QuAction::Number(5)),
					Box::new(QuAction::Add(
						Box::new(QuAction::Number(7)),
						Box::new(QuAction::Number(8)),
					)),
				),
			)),
		));

		assert_eq!(matched.len(), 1);
		assert_eq!(matched[0], Some(expected));

		return Ok(());
	}


	#[test]
	fn match_expression_extended() -> Result<(), QuMsg>{
		boilerplate!("2 - 3 + 2 * 3 / 5 - 7 + 8", m);

		let matched = m
			.match_expression()?
				.keep()?
			.fin();
		
		let expected = QuAction::Sub(
            Box::new(QuAction::Number(2)),
            Box::new(QuAction::Sub(
                Box::new(QuAction::Add(
                    Box::new(QuAction::Number(3)),
                    Box::new(QuAction::Div(
                        Box::new(QuAction::Mul(
                            Box::new(QuAction::Number(2)),
                            Box::new(QuAction::Number(3)),
                        )),
                        Box::new(QuAction::Number(5)),
                    )),
                )),
                Box::new(QuAction::Add(
                    Box::new(QuAction::Number(7)),
                    Box::new(QuAction::Number(8)),
                )),
			)),
        );
		assert_eq!(matched.len(), 1);
		assert_eq!(matched[0], Some(expected));

		return Ok(());
	}


	#[test]
	fn match_str() -> Result<(), QuMsg>{
		boilerplate!("Hello world!", m);

		let matched = m
			.match_str("Hello")?
			.match_str("world")?
				.keep()?
			.match_str("?")?
				.keep()?
			.fin();

		return Ok(());
	}


	#[test]
	fn match_var_decl() -> Result<(), QuMsg>{
		boilerplate!("var count = 4 * 9 + 2", m);

		let matched = m
			.match_var_decl()?
			.fin();
		
		let expected = QuAction::VarDeclare(
			Box::new(QuAction::VarRef("count".to_owned())),
			Some(Box::new(
				QuAction::Add(
					Box::new(QuAction::Mul(
						Box::new(QuAction::Number(4)),
						Box::new(QuAction::Number(9)),
					)),
					Box::new(QuAction::Number(2)),
				),
			)),
		);
		
		assert_eq!(matched.len(), 1);
		assert_eq!(matched[0], Some(expected));
		return Ok(());
		}


	#[test]
	fn match_var_decl_err() -> Result<(), QuMsg> {
		boilerplate!("var count = ", m);

		let matched = m
			.match_var_decl();

		// TODO: More precise error checking. Should check error type
		assert!(matched.is_err());

		return Ok(());
	}


	#[test]
	fn match_var_decl_err_2() -> Result<(), QuMsg> {
		boilerplate!("var", m);

		let matched = m
			.match_var_decl();
		
		// TODO: More precise error checking. Should check error type
		assert!(matched.is_err());

		return Ok(());
	}


	#[test]
	fn match_variable() -> Result<(), QuMsg> {
		boilerplate!("var count int =", m);

		let mut matched = m
			// Match 'var' keyword
			.match_str(KEYWORD_VAR)?
				.required()?

			// Match variable name
			// TODO: Fix match_name
//			.match_name()?
//				.err(||{QuMsg::general(
//					"Variable definition expected a name."
//				)})?
//				.keep()?

			// Match variable type
			// TODO: Fix match_name
//			.match_name()?
//				.keep()?
			
			// Match '=' symbol
			.match_str(OP_ASSIGN_SYMBOL)?
				.err(||{QuMsg::general(
					"Variable definitions expect a '='."
				)})?
			
			// TODO: Match the expression to assign to variable

			.fin();
		
			dbg!(matched);
		
		return Ok(());
	}

}