
use crate::QuToken;
use std::{fmt::{self, Display, Debug}};


pub const ERR_TITLE_EMPTY_CODE_BLOCK:&str = "EMPTY CODE BLOCK";
pub const ERR_TITLE_PARSER_MATCH_FAILED:&str = "PARSER MATCH FAILED";
pub const ERR_TITLE_GENERAL:&str = "GENERAL";
pub const ERR_TITLE_INVALID_FLOW_STATEMENT:&str = "INVALID FLOW STATEMENT";
pub const ERR_TITLE_INVALID_INDENTATION:&str = "INVALID INDENTATION";
pub const ERR_TITLE_INVALID_SYNTAX:&str = "INVALID SYNTAX";
pub const ERR_TITLE_INVALID_VARIABLE_ASSIGNMENT:&str = "INVALID VARIABLE ASSIGNMENT";
pub const ERR_TITLE_INVALID_VARIABLE_DEFINITION:&str = "INVALID VARIABLE DEFINITION";
pub const ERR_TITLE_MISSING_CODE_BLOCK:&str = "MISSING CODE BLOCK";
pub const ERR_TITLE_MISSING_TOKEN:&str = "MISSING TOKEN";
pub const ERR_TITLE_UNDEFINED_TYPE:&str = "UNDEFINED TYPE";
pub const ERR_TITLE_UNDEFINED_FN:&str = "UNDEFINED FUNCTION";


#[derive(Debug)]
pub struct QuMsg {
	pub title:String,
	pub description:String,
	pub token:QuToken,

} impl QuMsg {

	pub fn new() -> Self {
		Self {
			title:String::default(),
			description:String::default(),
			token:QuToken::default()
		}
	}


	pub fn panic(&self, script:&str) {
		panic!("{}", self.make_pretty_message(script));
	}


	pub fn print(&self, script:&str) {
		println!("{}", self.make_pretty_message(script));
	}


	/// Creates a pretty Qu error message as a [String].
	pub fn make_pretty_message(&self, script:&str) -> String {
		// Line numbers
		let line_nm_pre_pre = (self.token.row as usize).saturating_sub(1);
		let line_nm_pre = (self.token.row as usize).saturating_sub(0);
		let line_nm = (self.token.row as usize).saturating_add(1);
		let line_nm_post = (self.token.row as usize).saturating_add(2);
		let line_nm_post_post = (self.token.row as usize).saturating_add(3);

		// Line text
		let mut script_lines = script.split("\n");
		let line_pre_pre = if self.token.row > 1 {
			script_lines.nth(line_nm_pre_pre-1).unwrap_or("")
			.to_string()
		} else {
			"".to_string()
		};
		let line_pre = if self.token.row > 0 {
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
			"    {:0>4}:{}\n    {:0>4}:{}\n >> {:0>4}:{}\n    {:0>4}:{}\n    {:0>4}:{}\n\n",
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
		let msg = format!(
			"ERROR on line {row}, col {col}; {m0}:\"{m1}\"\n{script}",
			row=self.token.row+1, col=self.token._col, m0=self.title,
			m1=self.description, script=code_view
		);
		return msg;
		
	}


	// --- Messages ---

	pub fn empty_code_block() -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_EMPTY_CODE_BLOCK.to_string();
		msg.description = "A code block was started, but no code was found.".to_string();
		return msg;
	}


	pub fn flow_statement_lacks_expression() -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_INVALID_FLOW_STATEMENT.to_string();
		msg.description = format!("Flow statement requires an expression but non was given.");
		return msg;
	}


	pub fn failed_parser_match() -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_PARSER_MATCH_FAILED.to_string();
		msg.description = "Failed to parse something.".to_string();
		return msg;
	}


	pub fn general(description:&str) -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_GENERAL.to_string();
		msg.description = description.to_string();
		return msg;
	}


	pub fn missing_token(expected_token:&str) -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_MISSING_TOKEN.to_string();
		msg.description = format!("Epected a '{expected_token}' token, but it was not found.");
		return msg;
	}


	pub fn missing_code_block() -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_MISSING_CODE_BLOCK.to_string();
		msg.description = "A code block was expected, but none was found.".to_string();
		return msg;
	}


	pub fn missing_code_block_in_flow() -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_MISSING_CODE_BLOCK.to_string();
		msg.description = "Flow statement requires a code block, but none was found.".to_string();
		return msg;
	}


	pub fn invalid_indent() -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_INVALID_INDENTATION.to_string();
		msg.description = "Encountered invalid indentation.".to_string();
		return msg;
	}


	pub fn invalid_syntax() -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_INVALID_SYNTAX.to_string();
		msg.description = "A line has an incorrect indentation level.".to_string();
		return msg;
	}


	pub fn invalid_token(invalid_tk:&str) -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_INVALID_SYNTAX.to_string();
		msg.description = format!{"Encountered invalid token '{invalid_tk}'."};
		return msg;
	}


	pub fn one_liner() -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_INVALID_INDENTATION.to_string();
		msg.description = "A code block and flow statement are on the same line.".to_string();
		return msg;
	}


	pub fn unclosed_paren_expr() -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_INVALID_SYNTAX.to_string();
		msg.description = format!("Parenthesy expression remained unclosed.");
		return msg;
	}


	pub fn undefined_fn_access(the_fn:&str) -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_UNDEFINED_FN.to_string();
		msg.description = format!("Can't use '{the_fn}' because it was not previously defined.");
		return msg;
	}


	pub fn undefined_type_access(the_type:&str) -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_UNDEFINED_TYPE.to_string();
		msg.description = format!("Can't use '{the_type}' because it was not previously defined.");
		return msg;
	}


	pub fn undefined_var_access(the_var:&str) -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_INVALID_VARIABLE_ASSIGNMENT.to_string();
		msg.description = format!
			("Can't use '{the_var}' because it was not previously defined.",
		);
		return msg;
	}


	pub fn undefined_var_assign(the_var:&str) -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_INVALID_VARIABLE_ASSIGNMENT.to_string();
		msg.description = format!("Can't assign to '{the_var}' because it was not previously defined.");
		return msg;
	}


	pub fn var_assign_invalid_value(the_var:&str,
	invalid_value:&str) -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_INVALID_VARIABLE_ASSIGNMENT.to_string();
		msg.description = format!("Can't assign to variable '{the_var}' with '{invalid_value}' because it is not a valid value.");
		return msg;
	}


	pub fn var_assign_lacks_value(the_var:&str) -> Self{
			let mut msg = Self::new();
			msg.title = ERR_TITLE_INVALID_VARIABLE_ASSIGNMENT.to_string();
			msg.description = format!("Variable assignment for '{the_var}' lacks an expression.");
			return msg;
		}


	pub fn var_redefined(the_var:&str) -> Self{
		let mut msg = Self::new();
		msg.title = ERR_TITLE_INVALID_VARIABLE_DEFINITION.to_string();
		msg.description = format!("Can't define the variable '{the_var}' because it was already defined previously.");
		return msg;
	}	
	
} impl Display for QuMsg {

	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		return write!(f, "ERROR on line {row}, col {col}; {title}:\"{descr}",
			row=self.token.row+1,
			col=self.token._col,
			title=self.title,
			descr=self.description,
		);
	}
	
}