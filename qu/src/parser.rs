
use std::vec;

use crate::errors;
use crate::TOKEN_TYPE_NAME;
use crate::tokens::RULES;
use crate::tokens::TOKEN_TYPE_NUMBER;
use crate::tokens::tokenize;
use crate::QuToken;
use crate::QuMsg;


pub const FLOW_TYPE_IF:u8 = 0;
pub const FLOW_TYPE_WHILE:u8 = 1;

pub const KEYWORD_CLASS:&str = "class";
pub const KEYWORD_ELSE:&str = "else";
pub const KEYWORD_ELIF:&str = "elif";
pub const KEYWORD_FN:&str = "fn";
pub const KEYWORD_IF:&str = "if";
pub const KEYWORD_RETURN:&str = "return";
pub const KEYWORD_VAR:&str = "var";
pub const KEYWORD_WHILE:&str = "while";

pub const OP_ASSIGN_SYMBOL:&str = "=";
pub const OP_BLOCK_START:&str = ":";
pub const OP_EXPR_ADD:&str = "+";
pub const OP_EXPR_AND:&str = "and";
pub const OP_EXPR_DIV:&str = "/";
pub const OP_EXPR_EQL:&str = "==";
pub const OP_EXPR_GRT:&str = ">";
pub const OP_EXPR_GTE:&str = ">=";
pub const OP_EXPR_LES:&str = "<";
pub const OP_EXPR_LSE:&str = "<=";
pub const OP_EXPR_MOD:&str = "%";
pub const OP_EXPR_MUL:&str = "*";
pub const OP_EXPR_NEQ:&str = "!=";
pub const OP_EXPR_OR:&str = "or";
pub const OP_EXPR_POW:&str = "**";
pub const OP_EXPR_SUB:&str = "-";
pub const OP_EXPR_SQRT:&str = "//";

pub type QuBlockNode = Vec<QuLeaf>;
pub type QuParamNode = (QuToken, Option<QuToken>);


#[derive(Debug, Clone, PartialEq)]
/// A Qu instruction.
pub enum QuLeaf {
	/// A floating expression
	Expression(Box<QuLeafExpr>),
	/// An if statement. Contains an assertion statement and a [`Vec`] of
	/// instructions.
	FlowStatement(u8, Box<QuLeafExpr>, QuBlockNode),
	/// A function declaration branch. Contains the function name,
	/// parameters, and instructions.
	FnDecl(Box<QuToken>, Vec<QuParamNode>, QuBlockNode),
	/// A return statement for a function
	Return(Option<Box<QuLeafExpr>>),
	/// A variable assignment. Contains a var name and a [`QuLeafExpr`].
	VarAssign(Box<QuToken>, Box<QuLeafExpr>),
	/// A variable declaration. Contains a var name, type(TODO), and
	/// [`QuLeafExpr`].
	VarDecl(Box<QuToken>, Option<Box<QuToken>>, Option<Box<QuLeafExpr>>),

}


#[derive(Debug, Clone, PartialEq)]
/// Defines an expression in a Qu program tree.
pub enum QuLeafExpr {
	/// Call function branch.
	FnCall(Box<QuToken>, Vec<QuLeafExpr>), // TODO: Implement arguments
	/// A calculable expression. Contains an operator and two [`QuLeafExpr`]s.
	Equation(QuOperator, Box<QuLeafExpr>, Box<QuLeafExpr>),
	/// A literal int value.
	Number(Box<QuToken>),
	/// A tuple.
	Tuple(Vec<QuLeafExpr>),
	/// A variable name.
	Var(Box<QuToken>),
}


#[derive(Debug, Clone, PartialEq)]
pub enum QuOperator {
	/// The `+` or add math operator.
	Add,
	/// The `-` or subtract math operator.
	Sub,
	/// The `*` math operator.
	Mul,
	/// The `/` math operator.
	Div,
	/// The `%` math operator.
	Mod,
	/// The `**` or power math operator.
	Pow,
	/// The `//` or square root math operator.
	Sqrt,

	/// The `<` logical operator.
	Less,
	/// The `<=` logical operator.
	LessEq,
	/// The `>` logical operator.
	Great,
	/// The `>=` logical operator.
	GreatEq,
	/// The `==` logical operator.
	Eq,
	/// The `!=` logical operator.
	NotEq,
	/// The `and` logical operator.
	And,
	/// The `or` logical operator.
	Or,

} impl QuOperator {

	fn from_symbol(symbol:&str) -> Self {
		use QuOperator::*;
		match symbol {
			OP_EXPR_ADD => Add,
			OP_EXPR_SUB => Sub,
			OP_EXPR_MUL => Mul,
			OP_EXPR_DIV => Div,
			OP_EXPR_MOD => Mod,
			OP_EXPR_POW => Pow,
			OP_EXPR_SQRT => Sqrt,

			OP_EXPR_LES => Less,
			OP_EXPR_LSE => LessEq,
			OP_EXPR_GRT => Great,
			OP_EXPR_GTE => GreatEq,
			OP_EXPR_EQL => Eq,
			OP_EXPR_NEQ => NotEq,

			OP_EXPR_AND => And,
			OP_EXPR_OR => Or,

			_ => unimplemented!(),
		}
	}


	pub fn get_dunder(&self) -> &'static str {
		match self {
			QuOperator::Add => "__add__",
			QuOperator::Sub => "__subtract__",
			QuOperator::Mul => "__multiply__",
			QuOperator::Div => "__divide__",
			QuOperator::Mod => "__modular__",
			QuOperator::Pow => "__power__",
			QuOperator::Sqrt => "__sqareroot__",
			QuOperator::Less => "__lesser__",
			QuOperator::LessEq => "__lesserorequal__",
			QuOperator::Great => "__greater__",
			QuOperator::GreatEq => "__greaterorequal__",
			QuOperator::Eq => "__equal__",
			QuOperator::NotEq => "__notequal__",
			QuOperator::And => "__and__",
			QuOperator::Or => "__or__",
		}
	}

}


/// A parser for Qu scripts.
#[derive(Debug)]
pub struct QuParser {
	/// The current indentation of the script.
	indent:u8,
	/// The current line being paresd.
	line:usize,
	/// The current [QuToken] being anylized.
	tk_idx:usize,
	/// The saved token indexs to return to.
	tk_stack:Vec<usize>, // TODO: Fix tk_stack mem-leak
	/// The [QuTokens] being parsed.
	tokens:Vec<QuToken>,

} impl QuParser {

	/// Creates and returns a new [QuParser].
	pub fn new() -> Self {
		return QuParser {
			indent:u8::MAX,
			line:0,
			tk_idx:0,
			tk_stack:vec![],
			tokens:vec![],
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

			// Return Statement
			ck_parse!(ch_keyword_return);

			// Function declaration
			ck_parse!(ck_fn_decl);

			// Expressions
			if !self.utl_statement_start()?.is_none() {
				if let Some(expr_leaf) = self.ck_expr()? {
					leafs.push(QuLeaf::Expression(Box::new(expr_leaf)));
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
		if start_tk != OP_BLOCK_START {
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
			FLOW_TYPE_IF => KEYWORD_IF,
			FLOW_TYPE_WHILE => KEYWORD_WHILE,
			_ => unimplemented!(),
		};
		let keyword_tk = self.tk_next()
				.expect("Improper indentation TODO: Bette message");
		if keyword_tk != keyword {
			self.tk_state_pop();
			return Ok(None);
		}

		// Check expression
		let expr = self.ck_expr()?.ok_or_else(||{
			QuMsg::flow_statement_lacks_expression()
		})?;

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
				Box::new(expr),
				scope_data,
			)
		));
	}


	/// Attempts to parse an if statement.
	fn ck_flow_if(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		return self.ck_flow(FLOW_TYPE_IF);
	}


	/// Attempts to parse a while statement.
	fn ck_flow_while(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		return self.ck_flow(FLOW_TYPE_WHILE);
	}


	/// Attempts to parse a function call.
	fn ck_fn_call(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}
		
		self.tk_state_save();

		// Check function name
		let Some(fn_name_tk) = self.ck_fn_name()?
			else {return Ok(None)};

		// Match an open '('
		let Ok(Some(_)) = self.ck_str("(")
			else {
				self.tk_state_pop();
				return Ok(None);
			};

		// Match function parameters
		let params = match self.ck_expr()? {
			Some(l) => {
				if let QuLeafExpr::Tuple(tup) = l {
					tup
				} else {
					vec![l]
				}
			},
			None => vec![],
		};

		// Match a close ')'
		let Some(_) = self.ck_str(")")?
			else {return Err(QuMsg::missing_token(")"))};

		return Ok(Some(QuLeafExpr::FnCall(
			Box::new(fn_name_tk.clone()),
			params,
		)));
	}


	/// Attempts to parse a function definition.
	fn ck_fn_decl(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		let Some(_) = self.ck_str(KEYWORD_FN)?
			else {return Ok(None)};
		let Some(fn_name_tk) = self.ck_fn_name()?
			// TODO: Change to more appropriate message
			else {return Err(QuMsg::missing_code_block())};
		let Some(params) = self.ck_fn_parameters()?
			else {return Err(QuMsg::general(
				"Function definition expected parameters. TODO: better msg"
			))};
		let Some(scope_leafs) = self.ck_code_scope()?
			else {return Err(QuMsg::missing_code_block())};

		return Ok(Some(
			QuLeaf::FnDecl(
				Box::new(fn_name_tk.clone()),
				params,
				scope_leafs,
			)
		));
	}


	/// Attempts to parse a function name.
	fn ck_fn_name(&mut self) -> Result<Option<QuToken>, QuMsg> {
		// TODO: Implement function specific check for names
		return self.ck_var_name();
	}


	/// Attempts to parse function parameters, not including parenthesis.
	fn ck_fn_parameters(&mut self) -> Result<Option<Vec<QuParamNode>>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		let Some(_) = self.ck_str("(")?
			else {return Ok(None)};

		let mut params = vec![];
		loop {
			let Some(param)
				= self.ck_var_name_type()? else {break};
			params.push(param);
			let Some(_) = self.ck_str(",")?
				else {break};
		}

		let Some(_) = self.ck_str(")")?
			else {return Err(
				QuMsg::general("Parameters expected a ')'")
			)};

		return Ok(Some(params));
	}


	/// Attempts to return statemente.
	fn ch_keyword_return(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		// Match keyword
		let Some(_) = self.ck_str(KEYWORD_RETURN)?
			else {return Ok(None)};

		return match self.ck_expr()? {
			Some(l) => Ok(Some(QuLeaf::Return(Some(Box::new(l))))),
			None => Ok(Some(QuLeaf::Return(None))),
		};
	}


	/// Attempts to parse a number literal
	fn ck_number(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		let tk = self.tk_spy(0);
		if tk.tk_type == TOKEN_TYPE_NUMBER {
			return Ok(Some(
				QuLeafExpr::Number(Box::new(self.tk_next()?.clone()))
			))
		}
		return Ok(None);
	}


	/// Attempts to parse a lesser than expression.
	fn ck_op_les(&mut self) -> Result<Option<QuLeafExpr>, QuMsg>{
		return self.ck_operation(OP_EXPR_LES, &Self::ck_op_grt);
	}


	/// Attempts to parse a lesser or equal than expression.
	//fn ck_op_lse(&mut self) -> Result<Option<QuLeafExpr>, QuMsg>{
	//	return self.ck_operation(OP_EXPR_LSE, &Self::ck_op_grt);
	//}


	/// Attempts to parse a greater than expression.
	fn ck_op_grt(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation(OP_EXPR_GRT, &Self::ck_op_eql);
	}


	/// Attempts to parse a greater or equal than expression.
	//fn ck_op_gte(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
	//	return self.ck_operation(OP_EXPR_GTE, &Self::ck_op_eql);
	//}


	/// Attempts to parse an equal to expression.
	fn ck_op_eql(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation(OP_EXPR_EQL, &Self::ck_op_not_eql);
	}


	/// Attempts to parse a not equal to expression.
	fn ck_op_not_eql(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation(OP_EXPR_NEQ, &Self::ck_op_sub);
	}


	/// Attempts to parse a subtraction expression.
	fn ck_op_sub(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation(OP_EXPR_SUB, &Self::ck_op_add);
	}


	/// Attempts to parse an addition expression.
	fn ck_op_add(&mut self) -> Result<Option<QuLeafExpr>, QuMsg>  {
		return self.ck_operation(OP_EXPR_ADD, &Self::ck_op_div);
	}


	/// Attempts to parse a division expression.
	fn ck_op_div(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation(OP_EXPR_DIV, &Self::ck_op_mul);
	}


	/// Attempts to parse a multiplication expression.
	fn ck_op_mul(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		return self.ck_operation(OP_EXPR_MUL, &Self::ck_op_paren_expr);
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
				QuOperator::from_symbol(operator),
				Box::new(data_l),
				Box::new(data_r)
			)
		));
	}


	/// Attempts to parse a tuple, otherwise attempts to parse a
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


	/// Attempts to parse `text`.
	fn ck_str(&mut self, text:&str) -> Result<Option<()>, QuMsg> {
		if self.tk_spy(0) == text {
			self.tk_next()?;
			return Ok(Some(()));
		}
		return Ok(None);
	}


	/// Attempts to parse a type name.
	fn ck_type_name(&mut self) -> Result<Option<QuToken>, QuMsg> {
		// TODO: Implement type specific check for names
		return self.ck_var_name();
	}


	/// Attempts to parse a value.
	fn ck_value(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		self.tk_state_save();
		if self.tk_next_option().is_none() {
			self.tk_state_pop();
			return Ok(None);
		}
		self.tk_state_pop();

		// Check function call
		if let Some(leaf) = self.ck_fn_call()? {
			return Ok(Some(leaf));
		}

		// Check variable
		if let Some(leaf) = self.ck_var()? {
			return Ok(Some(leaf));
		}

		return Ok(self.ck_number()?);
	}


	/// Attempts to parse a variable as an expression.
	fn ck_var(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
		let Some(var_name) = self.ck_var_name()?
			else {return Ok(None)};

		return Ok(Some(
			QuLeafExpr::Var(Box::new(var_name))
		));
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
		if assign_op_tk != OP_ASSIGN_SYMBOL {
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
					&name_tk.slice, &false_expr.slice
				));
			},
		};

		return Ok(Some(
			QuLeaf::VarAssign(Box::new(name_tk), Box::new(expr_leaf))
		));
	}


	/// Attempts to parse a variable declaration.
	fn ck_var_decl(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		let Some(_) = self.ck_str(KEYWORD_VAR)?
			else {return Ok(None);};
		

		// Match variable name
		let Some(name_tk) = self.ck_var_name()?
			else {return Err(QuMsg::general(
			"QuToken after 'var' does not match a name. 'TODO:Better msg'"
			))};

		// Match variable type
		let type_tk_opt = match self.ck_type_name()? {
			Some(tk) => Some(Box::new(tk)),
			None => None,
		};

		// Match optional assignment
		let assign_leaf_opt
			= if self.ck_str(OP_ASSIGN_SYMBOL)?.is_some() {

			// Custom error handling
			let expr_op = self.ck_expr()
			.or_else(|msg|{
				// If the error is related to indentation, replace the error
				// with a missing value error
				if msg.title == errors::ERR_TITLE_INVALID_INDENTATION {
					self.tk_idx -= 1;
					return Err(QuMsg::var_assign_lacks_value(
						&name_tk.slice))
				}
				// Return normal error
				return Err(msg);
			})?;

			// Convert expression from Option to Option<Box>
			let expr = match expr_op {
				Some(expr) => Some(Box::new(expr)),
				None => None,
			};
			expr

		} else {None};
		
		return Ok(Some(QuLeaf::VarDecl(
			Box::new(name_tk),
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


	/// Attempts to parse a variable name and optionally a type.
	fn ck_var_name_type(
		&mut self
	) -> Result<Option<QuParamNode>, QuMsg> {
		let name_tk = match self.ck_var_name()? {
			Some(name_tk) => name_tk,
			None => {
				return Ok(None);
			},
		};
		return Ok(Some((
			name_tk,
			self.ck_type_name()?
		)));
	}


	/// Parses a Qu script.
	pub fn parse(&mut self, script:&str) -> Result<Vec<QuLeaf>, QuMsg> {
		self.tk_idx = 0;
		self.line = 0;
		self.indent = u8::MAX;
		self.tokens = tokenize(&script.to_owned(), RULES);
		self.tokens.push(
			QuToken::new(
				0, 0, 0, u8::MAX, "",
			)
		);

		let res = self.ck_code_block();
		match res {
			Ok(data_opt) => {
				if self.tk_idx != self.tokens.len()-1 {
					// Parsing ended early, must be an unexpected token
					panic!("Parsing ended without all tokens being searched");
				}

				let Some(data) = data_opt
					else {return Ok(vec![])};
				return Ok(data);
			}

			// Add token for location to error
			Err(mut msg) => {
				msg.token = self.tk_spy(0).char_index.clone();
				return Err(msg);
			}
		}
	}


	/// A helper function for whenever starting to parse a statement.
	fn utl_statement_start(&mut self) -> Result<Option<()>, QuMsg> {
		let tk = self.tk_spy(0);
		let tk_row = tk.char_index.row as usize;
		let tk_indent = tk.char_index.indent as u8;

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
	/// Error:
	/// 
	/// Returns an [`Err`] if the next [`QuToken`] violates indentation rules.
	fn tk_next(&mut self) -> Result<&QuToken, QuMsg> {
		let Some(tk) = self.tk_next_option()
			else {
				return Err(QuMsg::invalid_indent())
			};
		return Ok(tk);
	}


	/// Returns the next [`QuToken`] to parse or [`None`] if it violates
	/// indentation rules.
	fn tk_next_option(&mut self) -> Option<&QuToken> {
		let (line, indent) = (self.line, self.indent);

		// Return null if out of range
		if self.tk_idx == self.tokens.len() {
			return None;
		}

		let tk = &self.tokens[self.tk_idx];

		// Check for proper indentation
		if tk.char_index.row != line as u32 {
			if tk.char_index.indent != indent+2 {
				return None;
			}
		}

		if tk.tk_type == u8::MAX {
			return None;
		}

		self.tk_idx += 1;
		return Some(tk);
	}


	/// Reverts `tk_idx` to the last index saved by [#tk_state_save].
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


#[cfg(test)]
mod test_qu_matcher {
    use crate::QuLeaf;
    use crate::QuLeafExpr;
    use crate::parser::FLOW_TYPE_IF;
    use crate::QuParser;
	use crate::QuMsg;
	use crate::parser::QuOperator;
use crate::tokens::QuToken;

	use super::FLOW_TYPE_WHILE;

	// TODO: Make unit test for parsing expression order

	#[test]
	fn parse_code_example() -> Result<(), QuMsg>{
		let script = r#"
			fn add():
				var left = 2
				var right = 5
				return left + right
		
			add()
		"#;
		let mut p = QuParser::new();
		let res = p.parse(script)?;
		
		let expc = vec![
			QuLeaf::FnDecl(
				Box::new(QuToken::from("add")),
				vec![],
				vec![
					QuLeaf::VarDecl(
						Box::new(QuToken::from("left")),
						None,
						Some(Box::new(QuLeafExpr::Number(
							Box::new(QuToken::from("2"))
						))),
					),
					QuLeaf::VarDecl(
						Box::new(QuToken::from("right")),
						None,
						Some(Box::new(QuLeafExpr::Number(
							Box::new(QuToken::from("5"))
						))),
					),
					QuLeaf::Return(
						Some(Box::new(QuLeafExpr::Equation(
							QuOperator::Add,
							Box::new(QuLeafExpr::Var(
								Box::new(QuToken::from("left"))
							)),
							Box::new(QuLeafExpr::Var(
								Box::new(QuToken::from("right"))
							)),
						))),
					),
				]
			),
			QuLeaf::Expression(
				Box::new(QuLeafExpr::FnCall(
					Box::new(QuToken::from("add")),
					vec![]
				)),
			),
		];

		assert_eq!(&res, &expc);

		return Ok(());
	}
	

	#[test]
	fn parse_expression() -> Result<(), QuMsg>{
		let mut p = QuParser::new();
		let res = p.parse("2 + 3 - 3")?;
		dbg!(&res);
		
		let expc = vec![
			QuLeaf::Expression(Box::new(
				QuLeafExpr::Equation(
					QuOperator::Sub,
					Box::new(QuLeafExpr::Equation(
						QuOperator::Add,
						Box::new(QuLeafExpr::Number(Box::new(QuToken::from("2")))),
						Box::new(QuLeafExpr::Number(Box::new(QuToken::from("3")))),
					)),
					Box::new(QuLeafExpr::Number(Box::new(QuToken::from("3")))),
				),
			)),
		];

		assert_eq!(res.len(), expc.len());
		assert_eq!(res, expc);

		return Ok(());
	}


	macro_rules! test_equation {
		($symbol:expr, $p:ident) => {
			{
				let res = $p.parse(&format!("23 {} 18", $symbol))?;
				let expc = vec![
					QuLeaf::Expression(
						Box::new(QuLeafExpr::Equation(
							QuOperator::from_symbol($symbol),
							Box::new(QuLeafExpr::Number(
								Box::new(QuToken::from("23")))
							),
							Box::new(QuLeafExpr::Number(
								Box::new(QuToken::from("18")))
							),
						)),
					),
				];
				assert_eq!(res, expc);
			}
		};
	}

	#[test]
	fn parse_expression_outputs() -> Result<(), QuMsg>{
		let mut p = QuParser::new();
		
		test_equation!("<", p);
		//test_equation!("<=", p);
		test_equation!(">", p);
		//test_equation!(">=", p);
		test_equation!("==", p);
		test_equation!("!=", p);
		test_equation!("-", p);
		test_equation!("+", p);
		test_equation!("/", p);
		test_equation!("*", p);

		return Ok(());
	}


	#[test]
	fn parse_expression_parenthesies() -> Result<(), QuMsg>{
		let mut p = QuParser::new();
		let res = p.parse("2 + 3 * 9")?;
		let res2 = p.parse("(2 + 3) * 9")?;

		let expt = vec![
			QuLeaf::Expression(
				Box::new(QuLeafExpr::Equation(
					QuOperator::Add,
					Box::new(QuLeafExpr::Number(
						Box::new(QuToken::from("2")),
					)),
					Box::new(QuLeafExpr::Equation(
						QuOperator::Mul,
						Box::new(QuLeafExpr::Number(
							Box::new(QuToken::from("3")),
						)),
						Box::new(QuLeafExpr::Number(
							Box::new(QuToken::from("9")),
						)),
					)),
				)),
			),
		];
		let expt2 = vec![
			QuLeaf::Expression(
				Box::new(QuLeafExpr::Equation(
					QuOperator::Mul,
					Box::new(QuLeafExpr::Equation(
						QuOperator::Add,
						Box::new(QuLeafExpr::Number(
							Box::new(QuToken::from("2")),
						)),
						Box::new(QuLeafExpr::Number(
							Box::new(QuToken::from("3")),
						)),
					)),
					Box::new(QuLeafExpr::Number(
						Box::new(QuToken::from("9")),
					)),
				)),
			),
		];

		assert_ne!(&res, &res2);
		assert_eq!(&res, &expt);
		assert_eq!(&res2, &expt2);

		return Ok(());
	}


	#[test]
	fn parse_flow_if() -> Result<(), QuMsg>{
		let mut p = QuParser::new();
		let res = p.parse(r##"
		if 1:
			2
			3
		while 4:
			5
		"##)?;

		let expected = vec![
			QuLeaf::FlowStatement(
				FLOW_TYPE_IF,
				Box::new(QuLeafExpr::Number(
					Box::new(QuToken::from("1")),
				)),
				vec![
					QuLeaf::Expression(
						Box::new(QuLeafExpr::Number(
							Box::new(QuToken::from("2")),
						)),
					),
					QuLeaf::Expression(
						Box::new(QuLeafExpr::Number(
							Box::new(QuToken::from("3")),
						)),
					),
				],
			),
			QuLeaf::FlowStatement(
				FLOW_TYPE_WHILE,
				Box::new(QuLeafExpr::Number(
					Box::new(QuToken::from("4")),
				)),
				vec![
					QuLeaf::Expression(
						Box::new(QuLeafExpr::Number(
							Box::new(QuToken::from("5")),
						)),
					),
				],
			),
		];

		assert_eq!(res, expected);

		return Ok(());
	}


	#[test]
	fn parse_fn_call() -> Result<(), QuMsg>{
		let mut p = QuParser::new();
		let res = p.parse(r##"
		add() + sub()
		"##)?;

		let expected = vec![
			QuLeaf::Expression(
				Box::new(QuLeafExpr::Equation(
					QuOperator::Add,
					Box::new(QuLeafExpr::FnCall(
						Box::new(QuToken::from("add")),
						vec![],
					)),
					Box::new(QuLeafExpr::FnCall(
						Box::new(QuToken::from("sub")),
						vec![],
					)),
				)),
			),
		];

		assert_eq!(res, expected);

		return Ok(());
	}


	#[test]
	fn parse_fn_definition() -> Result<(), QuMsg>{
		let mut p = QuParser::new();
		let res = p.parse(r##"
			fn add(a, b):
				5
			fn sub(a int, b int):
				6
		"##)?;

		let expt = vec![
			QuLeaf::FnDecl(
				Box::new(QuToken::from("add")),
				vec![
					(
						QuToken::from("a"),
						None,
					),
					(
						QuToken::from("b"),
						None,
					),
				],
				vec![
					QuLeaf::Expression(
						Box::new(QuLeafExpr::Number(
							Box::new(QuToken::from("5")),
						)),
					),
				]
			),
			QuLeaf::FnDecl(
				Box::new(QuToken::from("sub")),
				vec![
					(
						QuToken::from("a"),
						Some(QuToken::from("int")),
					),
					(
						QuToken::from("b"),
						Some(QuToken::from("int")),
					),
				],
				vec![
					QuLeaf::Expression(
						Box::new(QuLeafExpr::Number(
							Box::new(QuToken::from("6")),
						)),
					),
				]
			),
		];

		assert_eq!(res, expt);

		return Ok(());
	}


	#[test]
	fn parse_variable_assignment() -> Result<(), QuMsg>{
		let mut p = QuParser::new();
		let res = p.parse("count = 3/2")?;

		let expc = vec![
			QuLeaf::VarAssign(
				Box::new(QuToken::from("count")),
				Box::new(QuLeafExpr::Equation(
					QuOperator::Div,
					Box::new(QuLeafExpr::Number(
						Box::new(QuToken::from("3")),
					)),
					Box::new(QuLeafExpr::Number(
						Box::new(QuToken::from("2")),
					)),
				)),
			),
		];

		assert_eq!(&res, &expc);

		return Ok(());
	}


	#[test]
	fn parse_variable_declaration() -> Result<(), QuMsg>{
		let mut p = QuParser::new();
		let res = p.parse("var count int = 20 * 8")?;

		dbg!(&res);
		let expc = vec![
			QuLeaf::VarDecl(
				Box::new(QuToken::from("count")),
				Some(Box::new(QuToken::from("int"))),
				Some(Box::new(QuLeafExpr::Equation(
					QuOperator::Mul,
					Box::new(QuLeafExpr::Number(
						Box::new(QuToken::from("20")),
					)),
					Box::new(QuLeafExpr::Number(
						Box::new(QuToken::from("8")),
					)),
				))),
			),
		];

		assert_eq!(&res, &expc);

		return Ok(());
	}

}