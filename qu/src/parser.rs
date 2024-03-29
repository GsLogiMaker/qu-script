
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

pub const KEYWORD_BOOL_TRUE:&str = "true";
pub const KEYWORD_BOOL_FALSE:&str = "false";
pub const KEYWORD_CLASS:&str = "class";
pub const KEYWORD_ELSE:&str = "else";
pub const KEYWORD_ELIF:&str = "elif";
pub const KEYWORD_FN:&str = "fn";
pub const KEYWORD_IF:&str = "if";
pub const KEYWORD_IMPORT:&str = "import";
pub const KEYWORD_RETURN:&str = "return";
pub const KEYWORD_VAR:&str = "var";
pub const KEYWORD_WHILE:&str = "while";
pub const KEYWORD_IS:&str = "is";

pub const OP_ASSIGN_SYMBOL:&str = "=";
pub const OP_BLOCK_START:&str = ":";
pub const OP_DOT_INDEX:&str = ".";
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
pub const OP_AS:&str = "@";

pub type QuParamNode = (QuToken, Option<QuToken>);

pub mod parsed {
	use std::fmt::Display;
	use crate::tokens::QuToken;
	use super::QuOperator;

	#[derive(Debug, Clone, PartialEq)]
	/// Defines an expression in a Qu program tree.
	pub enum Expression {
		As(Box<AsExpression>),
		/// Call function branch.
		Call(Box<CallExpression>),
		DotIndex(Box<DotIndex>),
		/// A calculable expression. Contains an operator and two [`QuLeafExpr`]s.
		Operation(Box<OperationExpression>),
		/// A literal int or boolean value.
		Bool(Box<BoolLiteral>),
		/// A literal float value.
		Number(Box<NumberLiteral>),
		/// A tuple.
		Tuple(Box<TupleExpression>),
		/// A variable name.
		Var(Box<VarExpression>),
	} impl Expression {
		/// Attempts to convert an expression into an identity, and panics if
		/// it can't.
		pub fn into_identity(&self) -> &str {
			match self {
				Expression::As(as_expr) => &as_expr.left.into_identity(),
				Expression::Call(_) => todo!(),
				Expression::DotIndex(dot_index) => {
					&dot_index.right.slice
				},
				Expression::Operation(_) => todo!(),
				Expression::Bool(_) => todo!(),
				Expression::Tuple(_) => todo!(),
				Expression::Var(var) => &var.name.slice,
    			Expression::Number(_) => todo!(),
			}
		}
	} impl Default for Expression {
		fn default() -> Self {
			unreachable!()
		}
	} impl Display for Expression {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			match self {
				Expression::As(a) => write!(f, "{:?}", **a),
				Expression::Call(a) => write!(f, "{:?}", **a),
				Expression::DotIndex(a) => write!(f, "{:?}", **a),
				Expression::Operation(a) => write!(f, "{:?}", **a),
				Expression::Bool(a) => write!(f, "{:?}", **a),
				Expression::Tuple(a) => write!(f, "{:?}", **a),
				Expression::Var(a) => write!(f, "{:?}", **a),
    			Expression::Number(a) => write!(f, "{:?}", **a),
			}
		}
	}


	#[derive(Debug, Clone, PartialEq)]
	/// Defines an expression in a Qu program tree.
	pub enum Statement {
		/// A floating expression
		Expression(Box<Expression>),
		/// An if statement. Contains an assertion statement and a [`Vec`] of
		/// instructions.
		FlowStatement(Box<FlowStatement>),
		/// A function declaration branch. Contains the function name,
		/// parameters, and instructions.
		FunctionDeclaration(Box<FunctionDeclaration>),
		/// An import statement.
		Import(Box<Import>),
		/// A return statement for a function
		Return(Box<ReturnStatement>),
		/// A variable assignment. Contains a var name and a [`QuLeafExpr`].
		VarAssign(Box<VarAssignment>),
		/// A variable declaration. Contains a var name, type(TODO), and
		/// [`QuLeafExpr`].
		VarDeclaration(Box<VarDeclaration>),
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct AsExpression {
		/// The value being cast
		pub left: Expression,
		/// The type being cast to
		pub right: QuToken,
	}

	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct CallExpression {
		pub caller: Option<Expression>,
		pub name: QuToken,
		pub parameters: TupleExpression,
		pub open_parenthesy: QuToken,
		pub close_parenthesy: QuToken,
	} impl From<&OperationExpression> for CallExpression {
		fn from(value: &OperationExpression) -> Self {
			// TODO: Don't use this function anymore
			CallExpression {
				name: QuOperator::from(value.operator.slice.as_str())
					.name()
					.into(),
				parameters: vec![value.left.clone(), value.right.clone()]
					.into(),
				..Default::default()
			}
		}
	} impl From<OperationExpression> for CallExpression {
		fn from(value: OperationExpression) -> Self {
			CallExpression {
				name: QuOperator::from(value.operator.slice.as_str())
					.name()
					.into(),
				parameters: vec![value.left, value.right].into(),
				..Default::default()
			}
		}
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct CodeBlock {
		pub statements: Vec<Statement>,
	} impl CodeBlock {
		pub fn new(statements:Vec<Statement>) -> Self {
			Self {
				statements,
			}
		}
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct CodeScope {
		pub colon: QuToken,
		pub code_block: CodeBlock,
	} impl CodeScope {
		pub fn new(statements:Vec<Statement>) -> Self {
			Self {
				colon: QuToken::from(":"),
				code_block: CodeBlock::new(statements),
			}
		}
	}


	#[derive(Debug, Clone, PartialEq)]
	pub struct FlowStatement {
		pub flow_keyword: QuToken,
		pub condition: Expression,
		pub body: CodeScope,
	} impl FlowStatement {
		pub fn new(
			flow_keyword:&str, expression:Expression, code_scope:CodeScope
		) -> Self {
			Self {
				flow_keyword: QuToken::from(flow_keyword),
				condition: expression,
				body: code_scope,
			}
		}
	}


	#[derive(Debug, Clone, PartialEq)]
	pub struct FunctionDeclaration {
		pub fn_keyword: QuToken,
		pub identity: FunctionIdentity,
		pub body: CodeScope,
	} impl FunctionDeclaration {
		pub fn new(
			function_identity:FunctionIdentity, body:CodeScope
		) -> Self {
			Self {
				fn_keyword: QuToken::from("fn"),
				identity: function_identity,
				body,
			}
		}
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct FunctionIdentity {
		pub name: QuToken,
		pub parameters: Vec<FunctionParameterElement>,
		pub return_type: Option<QuToken>,
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct FunctionParameterElement {
		pub name: QuToken,
		pub static_type: Option<QuToken>,
		pub comma: Option<QuToken>,
	} impl FunctionParameterElement {
		pub fn name(&self) -> &str {
			&self.name.slice
		}
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct DotIndex {
		pub left: Expression,
		pub dot: QuToken,
		pub right: QuToken,
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct IdentityExpression {
		pub value: Expression,
	} impl Display for IdentityExpression {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			write!(f, "{}", &self.value)
		}
	}


	#[derive(Debug, Clone, PartialEq)]
	pub enum Identity {
		Single(Box<QuToken>),
		Index(Box<IdentityIndex>),
	} impl Default for Identity {
		fn default() -> Self {
			unreachable!()
		}
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct IdentityIndex {
		pub left: QuToken,
		pub dot: QuToken,
		pub right: Identity,
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct Import {
		pub import: QuToken,
		pub identity_path: Identity,
	}


	#[derive(Debug, Clone, PartialEq)]
	pub struct NumberLiteral {
		pub value: QuToken,
		pub decimal: Option<QuToken>,
	}


	#[derive(Debug, Clone, PartialEq)]
	pub struct BoolLiteral {
		pub value: QuToken,
	}


	#[derive(Debug, Clone, PartialEq)]
	pub struct OperationExpression {
		pub left: Expression,
		pub operator: QuToken,
		pub right: Expression,
	} impl OperationExpression {
		pub fn new(left:Expression, operator:&str, right:Expression) -> Self {
			Self {
				left,
				operator: QuToken::from(operator),
				right,
			}
		}
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct ReturnStatement {
		pub value: Option<Expression>,
	} impl ReturnStatement {
		pub fn new(value:Option<Expression>) -> Self {
			Self {
				value,
			}
		}
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct TupleExpression {
		pub elements:Vec<Expression>,
	} impl TupleExpression {
		pub fn new(elements:Vec<Expression>) -> Self {
			Self {
				elements,
			}
		}

		pub fn len(&self) -> usize {
			self.elements.len()
		}
	} impl From<Vec<Expression>> for TupleExpression {
		fn from(elements: Vec<Expression>) -> Self {
			Self { elements }
		}
	}


	#[derive(Debug, Clone, PartialEq)]
	pub struct VarAssignment {
		pub name: QuToken,
		pub equals_sign: QuToken,
		pub new_value: Expression,
	}


	#[derive(Debug, Clone, PartialEq)]
	pub struct VarDeclaration {
		pub var_keyword: QuToken,
		pub name: QuToken,
		pub static_type: Option<QuToken>,
		pub equals_sign: Option<QuToken>,
		pub initial_value: Option<Expression>,
	}


	#[derive(Debug, Default, Clone, PartialEq)]
	pub struct VarExpression {
		pub name: QuToken,
	}
}


use parsed::*;


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
	Is,

} impl QuOperator {
	pub fn from_symbol(symbol:&str) -> Self {
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

			KEYWORD_IS => Is,

			_ => unimplemented!(),
		}
	}


	pub fn name(&self) -> &'static str {
		match self {
			QuOperator::Add => "add",
			QuOperator::Sub => "sub",
			QuOperator::Mul => "mul",
			QuOperator::Div => "div",
			QuOperator::Mod => "mod",
			QuOperator::Pow => "pow",
			QuOperator::Sqrt => "sqrt",
			QuOperator::Less => "lesser",
			QuOperator::LessEq => "lessereq",
			QuOperator::Great => "greater",
			QuOperator::GreatEq => "greatereq",
			QuOperator::Eq => "equal",
			QuOperator::NotEq => "not_equal",
			QuOperator::And => "and",
			QuOperator::Or => "or",
			QuOperator::Is => "is",
		}
	}
} impl From<&str> for QuOperator {
    fn from(value: &str) -> Self {
        Self::from_symbol(value)
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
	fn ck_code_block(&mut self) -> Result<Option<CodeBlock>, QuMsg> {
		let mut leafs:Vec<Statement> = vec![];

		macro_rules! ck_parse {
			($fn_name:ident, $statement:path) => {
				if let Some(data) = self.$fn_name()? {
					leafs.push($statement(Box::new(data)));
					continue;
				}
			};
		}

		while self.tk_idx < self.tokens.len()-1 {
			// Variable declaration
			ck_parse!(ck_var_decl, Statement::VarDeclaration);

			// Import
			ck_parse!(ck_import, Statement::Import);

			// Variable assignment
			ck_parse!(ck_var_assign, Statement::VarAssign);

			// If Statement
			ck_parse!(ck_flow_if, Statement::FlowStatement);

			// while Statement
			ck_parse!(ck_flow_while, Statement::FlowStatement);

			// Return Statement
			ck_parse!(ch_keyword_return, Statement::Return);

			// Function declaration
			ck_parse!(ck_fn_decl, Statement::FunctionDeclaration);

			// Expressions
			if !self.utl_statement_start()?.is_none() {
				if let Some(expr_leaf) = self.ck_expr()? {
					leafs.push(Statement::Expression(Box::new(expr_leaf)));
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

		return Ok(Some(CodeBlock {statements: leafs}));
	}


	/// Attempts to pasrse a code scope.
	fn ck_code_scope(&mut self) -> Result<Option<CodeScope>, QuMsg> {
		self.tk_state_save();

		// Check operator
		let colon = self.tk_next()?.clone();
		if colon != OP_BLOCK_START {
			self.tk_state_pop();
			return Ok(None);
		}

		self.indent += 1;
		let Ok(Some(code_block)) = self.ck_code_block() else {
			return Err("No code block.".into())
		};
		self.indent -= 1;

		return Ok(Some(CodeScope {colon, code_block}));
	}


	/// Attempts to parse a dot indexing (Ex: foo.bar). If it doesn't it
	/// then attempts to parse a value. 
	fn ck_dot_index(&mut self) -> Result<Option<Expression>, QuMsg> {
		// Parse any expression
		let Some(left) = self.ck_value()? else {
			return Ok(None);
		};

		// Parse a dot indexing
		let Some(dot) = self.ck_str(OP_DOT_INDEX)? else {
			// Does not match dot index, return expression
			return Ok(Some(left));
		};
		let Some(right) = self.ck_identity()? else { return Err(
			format!(
				"Expected an identity after '.', but found something else. TODO",
			).into()
		) };

		// Parse a function call
		let Some(open_parenthesy) = self.ck_str("(")? else {
			// Does not match function call, return dot index
			return Ok(Some(Expression::DotIndex(Box::new( DotIndex {
				left,
				dot,
				right,
			} ))));
		};
		let parameters = self.ck_fn_call_parameters()?;
		let Some(close_parenthesy) = self.ck_str(")")? else {
			return Err(QuMsg::missing_token(")"))
		};

		Ok(Some(Expression::Call(Box::new( CallExpression {
			caller: Some(left),
			name: right,
			parameters,
			open_parenthesy,
			close_parenthesy,
		} ))))
	}


	/// Attempts to parse an expression
	fn ck_expr(&mut self) -> Result<Option<Expression>, QuMsg> {
		return self.ck_tuple();
	}


	/// Attempts to parse a flow statement (Ex: if, while, for, etc).
	fn ck_flow(
		&mut self, token_type:u8
	) -> Result<Option<FlowStatement>, QuMsg> {
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
		let flow_keyword = self.tk_next()
				.expect("Improper indentation TODO: Bette message")
				.clone();
		if flow_keyword != keyword {
			self.tk_state_pop();
			return Ok(None);
		}

		// Check expression
		let expression = self.ck_expr()?.ok_or_else(||{
			QuMsg::flow_statement_lacks_expression()
		})?;

		// Check for code block
		let code_scope = match self.ck_code_scope() {
			Ok(leaf) => leaf.ok_or_else(|| {
				QuMsg::missing_code_block()
			} )?,
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

		return Ok(Some(FlowStatement {
			flow_keyword,
			condition: expression,
			body: code_scope,
		}));
	}


	/// Attempts to parse an if statement.
	fn ck_flow_if(&mut self) -> Result<Option<FlowStatement>, QuMsg> {
		return self.ck_flow(FLOW_TYPE_IF);
	}


	/// Attempts to parse a while statement.
	fn ck_flow_while(&mut self) -> Result<Option<FlowStatement>, QuMsg> {
		return self.ck_flow(FLOW_TYPE_WHILE);
	}


	/// Attempts to parse a function call.
	fn ck_fn_call(&mut self) -> Result<Option<CallExpression>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}
		
		self.tk_state_save();

		// Check function name
		let Some(fn_name_tk) = self.ck_fn_name()?
			else {return Ok(None)};

		// Match an open '('
		let Some(open_parenthesy) = self.ck_str("(")? else {
			self.tk_state_pop();
			return Ok(None);
		};

		// Match function parameters
		let parameters = self.ck_fn_call_parameters()?;

		// Match a close ')'
		let Some(close_parenthesy) = self.ck_str(")")? else {
			return Err(QuMsg::missing_token(")"))
		};

		return Ok(Some(CallExpression {
			caller: None,
			name: fn_name_tk.clone(), // TODO: Remove clones
			parameters,
			open_parenthesy,
			close_parenthesy,
		}));
	}


	fn ck_fn_call_parameters(
		&mut self,
	) -> Result<TupleExpression, QuMsg> {
		let parameters = match self.ck_expr()? {
			Some(l) => {
				match l {
					Expression::Tuple(tuple_expression)
						=> *tuple_expression,
					_ => {
						TupleExpression {elements: vec![l]}
					},
				}
			},
			None => TupleExpression::default(),
		};
		Ok(parameters)
	}


	/// Attempts to parse a function definition.
	fn ck_fn_decl(&mut self) -> Result<Option<FunctionDeclaration>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		let Some(fn_keyword) = self.ck_str(KEYWORD_FN)?
			else {return Ok(None)};
		let Some(function_name) = self.ck_fn_name()?
			// TODO: Change to more appropriate message
			else {return Err(QuMsg::missing_code_block())};
		let Some(parameters) = self.ck_fn_parameters()?
			else {return Err(QuMsg::general(
				"Function definition expected parameters. TODO: better msg"
			))};
		let return_type = self.ck_type_name()?;
		let Some(code_scope) = self.ck_code_scope()?
			else {return Err(QuMsg::missing_code_block())};
		
		let function_identity = FunctionIdentity {
			name: function_name,
			parameters,
			return_type,
		};

		return Ok(Some(FunctionDeclaration {
			fn_keyword,
			identity: function_identity,
			body: code_scope,
		}));
	}


	/// Attempts to parse a function name.
	fn ck_fn_name(&mut self) -> Result<Option<QuToken>, QuMsg> {
		// TODO: Implement function specific check for names
		return self.ck_var_name();
	}


	/// Attempts to parse function parameters, not including parenthesis.
	fn ck_fn_parameters(
		&mut self
	) -> Result<Option<Vec<FunctionParameterElement>>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		let Some(_) = self.ck_str("(")?
			else {return Ok(None)};

		let mut params = vec![];
		loop {
			let Some(param)
				= self.ck_var_name_type()? else {break};
			let comma = self.ck_str(",")?.clone();
			let has_comma = comma.is_some();
			let element = FunctionParameterElement {
				name: param.0,
				static_type: param.1,
				comma,
			};
			params.push(element);
			if !has_comma {
				break
			}
		}

		let Some(_) = self.ck_str(")")?
			else {return Err(
				QuMsg::general("Parameters expected a ')'")
			)};

		return Ok(Some(params));
	}


	/// Attempts to return statemente.
	fn ch_keyword_return(&mut self) -> Result<Option<ReturnStatement>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		// Match keyword
		let Some(_) = self.ck_str(KEYWORD_RETURN)?
			else {return Ok(None)};

		return match self.ck_expr()? {
			Some(value)
				=> Ok(Some(ReturnStatement {value:Some(value)})),
			None => Ok(Some(ReturnStatement {value: None})),
		};
	}


	/// Attempts to parse an identity tree.
	fn ck_identity(&mut self) -> Result<Option<QuToken>, QuMsg> {
		self.ck_identity_item()
	}


	/// Attempts to parse identities as a path (Ex: foo.bar). Note that this is
	/// mainly used for the import statement, not expressions. See
	/// `[Parser::ck_dot_index]` for expressions.
	fn ck_identity_index(&mut self) -> Result<Option<Identity>, QuMsg> {
		let Some(left) = self.ck_identity()? else {
			return Ok(None);
		};
		let Some(dot) = self.ck_str(OP_DOT_INDEX)? else {
			return Ok(Some( Identity::Single(Box::new(left)) ));
		};
		let Some(right) = self.ck_identity_index()? else {
			return Err(format!(
				"Expected an identity after '.'. TODO",
			).into());
		};

		Ok(Some(Identity::Index(Box::new( IdentityIndex {
			left,
			dot,
			right
		} ))))
	}


	/// Attempts to parse individual identity item.
	fn ck_identity_item(&mut self) -> Result<Option<QuToken>, QuMsg> {
		self.tk_state_save();
		
		let identity_option = self.tk_next_option();
		match identity_option {
			Some(identity) => {
				if identity.tk_type == TOKEN_TYPE_NAME {
					return Ok(Some(
						identity.clone()
					));
				}
				self.tk_state_pop();
				return Ok(None);
			},
			None => {
				self.tk_state_pop();
				return Ok(None);
			},
		}
	}


	/// Attempts to parse an import statement.
	fn ck_import(&mut self) -> Result<Option<Import>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		let Some(import) = self.ck_str(KEYWORD_IMPORT)? else {
			return Ok(None)
		};

		let Some(identity_path) = self.ck_identity_index()? else {
			return Err(format!(
				"Expected an indentity path after '{}'. TODO",
				KEYWORD_IMPORT,
			).into())
		};

		Ok(Some( Import {
			import,
			identity_path,
		} ))
	}


	/// Attempts to parse a number literal
	fn ck_literal(&mut self) -> Result<Option<Expression>, QuMsg> {
		if let Some(number) = self.ck_number()? {
			return Ok(Some(number));
		}

		let tk = self.tk_spy(0);
		if
			tk.tk_type == TOKEN_TYPE_NUMBER
			||tk.slice == KEYWORD_BOOL_TRUE
			|| tk.slice == KEYWORD_BOOL_FALSE
		{
			let value = self.tk_next()?.clone();
			return Ok(Some(Expression::Bool(Box::new(
				BoolLiteral { value }
			))));
		}
		return Ok(None);
	}


	fn ck_number(&mut self) -> Result<Option<Expression>, QuMsg> {
		if self.tk_spy(0).tk_type == TOKEN_TYPE_NUMBER {
			let value = self.tk_next()?.clone();
			let mut decimal = None;
			if self.tk_spy(0) == "." {
				let maybe_decimal = self.tk_spy(1);
				if maybe_decimal.tk_type == TOKEN_TYPE_NUMBER {
					// Matched decimal after dot
					self.tk_next()?;
					decimal = Some(self.tk_next()?.clone());
				} else if maybe_decimal.tk_type != TOKEN_TYPE_NAME || !self.is_indent_ok(maybe_decimal) {
					// No number after dot, and it's not part of an
					// indexing, so it must be part of the decimal
					self.tk_next()?;
					decimal = Some(QuToken::from("0"));
				}
			}

			return Ok(Some(
				Expression::Number(Box::new(
					NumberLiteral {
						value,
						decimal,
					}
				))
			));
		}
		Ok(None)
	}


	/// The top level function for checking operators
	fn ck_ops(&mut self) -> Result<Option<Expression>, QuMsg>{
		return self.ck_op_is();
	}


	fn ck_op_is(&mut self) -> Result<Option<Expression>, QuMsg>{
		return self.ck_operation(KEYWORD_IS, &Self::ck_op_les);
	}


	/// Attempts to parse a lesser than expression.
	fn ck_op_les(&mut self) -> Result<Option<Expression>, QuMsg>{
		return self.ck_operation(OP_EXPR_LES, &Self::ck_op_grt);
	}


	/// Attempts to parse a lesser or equal than expression.
	//fn ck_op_lse(&mut self) -> Result<Option<QuLeafExpr>, QuMsg>{
	//	return self.ck_operation(OP_EXPR_LSE, &Self::ck_op_grt);
	//}


	/// Attempts to parse a greater than expression.
	fn ck_op_grt(&mut self) -> Result<Option<Expression>, QuMsg> {
		return self.ck_operation(OP_EXPR_GRT, &Self::ck_op_eql);
	}


	/// Attempts to parse a greater or equal than expression.
	//fn ck_op_gte(&mut self) -> Result<Option<QuLeafExpr>, QuMsg> {
	//	return self.ck_operation(OP_EXPR_GTE, &Self::ck_op_eql);
	//}


	/// Attempts to parse an equal to expression.
	fn ck_op_eql(&mut self) -> Result<Option<Expression>, QuMsg> {
		return self.ck_operation(OP_EXPR_EQL, &Self::ck_op_not_eql);
	}


	/// Attempts to parse a not equal to expression.
	fn ck_op_not_eql(&mut self) -> Result<Option<Expression>, QuMsg> {
		return self.ck_operation(OP_EXPR_NEQ, &Self::ck_op_sub);
	}


	/// Attempts to parse a subtraction expression.
	fn ck_op_sub(&mut self) -> Result<Option<Expression>, QuMsg> {
		return self.ck_operation(OP_EXPR_SUB, &Self::ck_op_add);
	}


	/// Attempts to parse an addition expression.
	fn ck_op_add(&mut self) -> Result<Option<Expression>, QuMsg>  {
		return self.ck_operation(OP_EXPR_ADD, &Self::ck_op_div);
	}


	/// Attempts to parse a division expression.
	fn ck_op_div(&mut self) -> Result<Option<Expression>, QuMsg> {
		return self.ck_operation(OP_EXPR_DIV, &Self::ck_op_mul);
	}


	/// Attempts to parse a multiplication expression.
	fn ck_op_mul(&mut self) -> Result<Option<Expression>, QuMsg> {
		return self.ck_operation(OP_EXPR_MUL, &Self::ck_op_paren_expr);
	}


	/// Attempts to parse a parenthesized expression.
	fn ck_op_paren_expr(&mut self
	) -> Result<Option<Expression>, QuMsg> {
		self.tk_state_save();

		let tk = self.tk_next()?;
		if tk != "(" {
			self.tk_state_pop();
			self.tk_state_save();

			// Match for a value if no parenthesis expression is matched.
			let data = self.ck_dot_index()?;
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
		next:&dyn Fn(&mut Self)->Result<Option<Expression>, QuMsg>,
	) -> Result<Option<Expression>, QuMsg> {

		self.tk_state_save();

		// Check left side for value
		let data_l = next(self)?;
		if let None = data_l {
			self.tk_state_pop();
			return Ok(None);
		}
		let left = data_l.unwrap();

		// Check operator
		let tk_op = self.tk_spy(0).clone();
		if tk_op != operator {
			return Ok(Some(left));
		}
		self.tk_next().expect("Improper indentation TODO: Bette message");

		// Check right side for expression
		let right = self.ck_operation(operator, next)?;
		if let None = right {
			self.tk_state_pop();
			return Ok(None);
		}
		let right = right.unwrap();

		return Ok(Some(Expression::Operation(Box::new(OperationExpression {
			left,
			operator: tk_op,
			right,
		}))));
	}


	/// Attempts to parse a tuple, otherwise attempts to parse a
	/// expression.
	/// 
	/// A tuple is denoted by expressions separated by commas (Ex: "1,2,3").
	/// Parenthesis technicly have nothing to do with tuples.
	fn ck_tuple(&mut self) -> Result<Option<Expression>, QuMsg>{
		match self.ck_ops()? {
			// Matched an expression
			Some(expr) => {
				if self.tk_spy(0) == "," {
					// Matched tuple, get more tuple elements
					let mut elements = vec![expr];
					self.tk_next()?;
					loop {
						match self.ck_ops()? {
							Some(next_expr) => {
								// Found another expression, add to tuple
								elements.push(next_expr);
								if self.tk_spy(0) != "," {
									// No comma found, tuple must have ended.
									// Return tuple.
									return Ok(Some(Expression::Tuple(
										Box::new(TupleExpression {elements})
									)))
								} else {
									// Comma found, continue adding to tuple
									self.tk_next()?;
								}
							},
							None => {
								// No more expressions found, return tuple
								return Ok(Some(Expression::Tuple(
									Box::new(TupleExpression {elements})
								)))
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
	fn ck_str(&mut self, text:&str) -> Result<Option<QuToken>, QuMsg> {
		if self.tk_spy(0) == text {
			return Ok(Some(self.tk_next()?.clone()));
		}
		return Ok(None);
	}


	/// Attempts to parse a type name.
	fn ck_type_name(&mut self) -> Result<Option<QuToken>, QuMsg> {
		// TODO: Implement type specific check for names
		return self.ck_identity();
	}


	/// Attempts to parse a value.
	fn ck_value(&mut self) -> Result<Option<Expression>, QuMsg> {
		self.tk_state_save();
		if self.tk_next_option().is_none() {
			self.tk_state_pop();
			return Ok(None);
		}
		self.tk_state_pop();

		let Some(expr) = (
			if let Some(expr) = self.ck_literal()? {
				Some(expr)
			} else if let Some(call) = self.ck_fn_call()? {
				Some(Expression::Call(Box::new(call)))
			} else if let Some(leaf) = self.ck_var()? {
				Some(Expression::Var(Box::new(leaf)))
			} else {
				None
			}
		) else {
			return Ok(None);
		};

		if self.tk_spy(0) != OP_AS {
			return Ok(Some(expr));
		}
		self.tk_next()?;

		let Some(right) = self.ck_type_name()?
			else {
				return Ok(Some(expr));
			};

		Ok(Some(Expression::As(Box::new(AsExpression {
			left: expr,
			right,
		}))))
	}


	/// Attempts to parse a variable as an expression.
	fn ck_var(&mut self) -> Result<Option<VarExpression>, QuMsg> {
		let Some(variable_name) = self.ck_var_name()?
			else {return Ok(None)};

		return Ok(Some(
			VarExpression {name: variable_name}
		));
	}


	/// Attempts to parse a variable assignment.
	fn ck_var_assign(&mut self) -> Result<Option<VarAssignment>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		self.tk_state_save();

		// Match variable name
		let name_data_opt = self.ck_var_name()?;
		let ident = match name_data_opt {
			Some(name_data) => name_data,
			None => {return Ok(None);},
		};
		
		// Match assign operator
		let equals_sign = self.tk_next()?.clone();
		if equals_sign != OP_ASSIGN_SYMBOL {
			self.tk_state_pop();
			return Ok(None);
		}

		// Match expression
		let new_value = match self.ck_expr()? {
			Some(expr_data) => expr_data,
			None => {
				self.tk_state_pop();
				let false_expr = self.tk_spy(2).clone();
				return Err(QuMsg::var_assign_invalid_value(
					&String::from(ident), &false_expr.slice
				));
			},
		};

		return Ok(Some(
			VarAssignment { name: ident, equals_sign, new_value }
		));
	}


	/// Attempts to parse a variable declaration.
	fn ck_var_decl(&mut self) -> Result<Option<VarDeclaration>, QuMsg> {
		if self.utl_statement_start()?.is_none() {
			return Ok(None);
		}

		let Some(var_keyword) = self.ck_str(KEYWORD_VAR)?
			else {return Ok(None);};
		

		// Match variable name
		let Some(name) = self.ck_var_name()? else {
			return Err(
				"QuToken after 'var' does not match a name. 'TODO:Better msg'".into()
			)
		};

		// Match variable type
		let static_type = self.ck_type_name()?;
		let equals_sign = self.ck_str(OP_ASSIGN_SYMBOL)?;

		// Match optional assignment
		let value
			= if equals_sign.is_some() {

			// Custom error handling
			let expr_op = self.ck_expr()
			.or_else(|msg|{
				// If the error is related to indentation, replace the error
				// with a missing value error
				if msg.title == errors::ERR_TITLE_INVALID_INDENTATION {
					self.tk_idx -= 1;
					return Err(QuMsg::var_assign_lacks_value(
						&String::from(name.clone())))
				}
				// Return normal error
				return Err(msg);
			})?;

			expr_op

		} else {None};
		
		return Ok(Some(
			VarDeclaration {var_keyword, name, static_type, equals_sign, initial_value: value}
		));
	}


	/// Attempts to parse a variable name.
	fn ck_var_name(&mut self) -> Result<Option<QuToken>, QuMsg> {
		return self.ck_identity();
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


	/// Returns true if the token follows the indentation rules.
	fn is_indent_ok(&self, tk:&QuToken) -> bool {
		if tk.char_index.row != self.line as u32 {
			if tk.char_index.indent != self.indent+1 {
				return false;
			}
		}
		return true;
	}


	/// Parses a Qu script.
	pub fn parse(&mut self, script:&str) -> Result<CodeBlock, QuMsg> {
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
					else {return Ok(CodeBlock {statements: vec![]})};
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
		// Return null if out of range
		if self.tk_idx == self.tokens.len() {
			return None;
		}

		let tk = &self.tokens[self.tk_idx];

		// Check for proper indentation
		if !self.is_indent_ok(tk) {
			return None;
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
	fn tk_spy(&self, at:usize) -> &QuToken {
		if self.tk_idx+at >= self.tokens.len() {
			return &self.tokens[self.tokens.len()-1];
		}
		return &self.tokens[self.tk_idx+at];
	}

}


#[cfg(test)]
mod test_qu_matcher {}
