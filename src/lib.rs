
//! TODO: Project level documentation.

#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
#![warn(rustdoc::broken_intra_doc_links)]


#[macro_use]
extern crate derive_new;
#[macro_use]
extern crate lazy_static;


mod qu_error {

	use crate::qu_tokens::QuToken;
	use std::{fmt::{self, Display, Debug}};

	type QuErrorMessage = (String, String);


	pub const ERR_TITLE_EMPTY_CODE_BLOCK:&str = "EMPTY CODE BLOCK";
	pub const ERR_TITLE_GENERAL:&str = "GENERAL";
	pub const ERR_TITLE_INVALID_FLOW_STATEMENT:&str = "INVALID FLOW STATEMENT";
	pub const ERR_TITLE_INVALID_INDENTATION:&str = "INVALID INDENTATION";
	pub const ERR_TITLE_INVALID_SYNTAX:&str = "INVALID SYNTAX";
	pub const ERR_TITLE_INVALID_VARIABLE_ASSIGNMENT:&str = "INVALID VARIABLE ASSIGNMENT";
	pub const ERR_TITLE_INVALID_VARIABLE_DEFINITION:&str = "INVALID VARIABLE DEFINITION";
	pub const ERR_TITLE_MISSING_CODE_BLOCK:&str = "MISSING CODE BLOCK";
	pub const ERR_TITLE_MISSING_TOKEN:&str = "MISSING TOKEN";
	pub const ERR_TITLE_UNDEFINED_TYPE:&str = "UNDEFINED TYPE";
	

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

		pub fn empty_code_block(expected_token:&str) -> Self{
			let mut msg = Self::new();
			msg.title = ERR_TITLE_EMPTY_CODE_BLOCK.to_string();
			msg.description = "A code block was started, but no code was found.".to_string();
			return msg;
		}


		pub fn flow_statement_lacks_expression(flow_keyword:&str) -> Self{
			let mut msg = Self::new();
			msg.title = ERR_TITLE_INVALID_FLOW_STATEMENT.to_string();
			msg.description = format!("Flow statement requires an expression but non was given.");
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


		pub fn invalid_syntax(expected_token:&str) -> Self{
			let mut msg = Self::new();
			msg.title = ERR_TITLE_INVALID_INDENTATION.to_string();
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


	/// Creates a Qu error message as a [String].
	/// 
	/// Examples:
	/// ```
	/// use qu_script::err_msg_make;
	/// use qu_script::QuError;
	/// use qu_script::QuToken;
	/// 
	/// let script = "print 1";
	/// let token = QuToken::new(0, 4, 0, 0, 0, 0);
	/// 
	/// let err_msg = err_msg_make(
	/// 	("My Error", "It crashed with My Error!"), token, script);
	/// println!(err_msg);
	/// ```
	pub fn make_message(err_msg:QuErrorMessage, tk:&QuToken, script:&String) -> String {
		// Line numbers
		let line_nm_pre_pre = (tk.row as usize).saturating_sub(1);
		let line_nm_pre = (tk.row as usize).saturating_sub(0);
		let line_nm = (tk.row as usize).saturating_add(1);
		let line_nm_post = (tk.row as usize).saturating_add(2);
		let line_nm_post_post = (tk.row as usize).saturating_add(3);

		// Line text
		let mut script_lines = script.split("\n");
		let line_pre_pre = if tk.row > 1 {
			script_lines.nth(line_nm_pre_pre-1).unwrap_or("").to_string()
		} else {
			"".to_string()
		};
		let line_pre = if tk.row > 0 {
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
			row=tk.row+1, col=tk._col, m0=err_msg.0, m1=err_msg.1,
			script=code_view
		);
		return msg;
		
	}

}


mod qu_tokens {

	//use std::{fmt::{self, Display, Debug}, vec, collections::HashMap};
	use std::fmt::{self, Display, Debug};

	use super::{
		KEYWORD_VAR,
		KEYWORD_FUNCTION,
		KEYWORD_CLASS,
		KEYWORD_IF,
		KEYWORD_ELSE,
		KEYWORD_ELIF
	};


	pub const TOKEN_TYPE_KEYWORD:u8 = 1;
	pub const TOKEN_TYPE_SYMBOL:u8 = 2;
	pub const TOKEN_TYPE_NUMBER:u8 = 3;
	pub const TOKEN_TYPE_NAME:u8 = 4;


	/// Base token rules for the Qu language.
	pub const RULES:&Rules = &[
		(&tokenrule_keyword, TOKEN_TYPE_KEYWORD),
		(&tokenrule_symbols, TOKEN_TYPE_SYMBOL),
		(&tokenrule_number, TOKEN_TYPE_NUMBER),
		(&tokenrule_name, TOKEN_TYPE_NAME),
	];

	/// Token rules for Qu assembly.
	pub const ASM_RULES:&Rules = &[
		(&tokenrule_symbols, TOKEN_TYPE_KEYWORD),
		(&tokenrule_flagref, 0),
		(&tokenrule_number, TOKEN_TYPE_NUMBER),
		(&tokenrule_name, TOKEN_TYPE_NAME),
	];


	/// A [Vec] of tokenfule_* functions and their types.
	///
	/// This is used by [chars_fit_rule] to determin if a pattern of
	/// characters should be turned into a [QuToken]. See [tokenrule_name] or 
	/// [tokenrule_keyword] for examples of how a `tokenrule_*` function should
	/// be structured.
	pub type Rules<'a> = [(&'a dyn Fn(&[char])->bool, u8)];


	/// Returns *true* if the passed characters matches to an assembly line-flag.
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


	/// Returns *true* if the passed characters match to a name.
	///
	/// A name could be a type, class, function name, or variable name.
	/// 
	/// Examples
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


	/// Returns *true* if the passed characters match to a number (like *int* or
	/// *float*).
	/// 
	/// Examples
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


	/// Returns *true* if the passed characters match to a keyword. 
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


	/// Returns *true* if the passed characters match to a symbol.
	/// 
	/// Some examples of operators are *+*, *-*, and *+=*.
	/// 
	/// Examples
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
			['\\',] => true, // This is just '\'
			['=',] => true,
			['!',] => true,
			['?',] => true,
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
			['@',] => true,
			['|',] => true,
			['^',] => true,
			[':',] => true,
			[';',] => true,
			[',',] => true,
			['(',] => true,
			[')',] => true,
			['[',] => true,
			[']',] => true,
			['{',] => true,
			['}',] => true,
			['`',] => true,
			['"',] => true,
			['\'',] => true,
			_ => false,
		};
	}


	/// Tokenizes a [String] according to the passed [Rules].
	/// 
	/// Examples
	/// ```
	/// use qu_script::Token;
	/// use qu_script::tokenize;
	/// use qu_script::tokenrule_name;
	/// use qu_script::tokenrule_symbols;
	/// use qu_script::TOKEN_TYPE_NAME;
	/// use qu_script::TOKEN_TYPE_SYMBOL;
	/// 
	/// let script:&str = " hello=world ;! ";
	/// 
	/// let tokens:Vec<Token> = tokenize(
	/// 	&script,
	/// 	&[
	/// 		(&tokenrule_name, TOKEN_TYPE_NAME),
	/// 		(&tokenrule_symbols, TOKEN_TYPE_SYMBOL),
	/// 	]
	/// );
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

					if idx+1 == script.len() {
						tokens[curr_token].text
								= tokens[curr_token].text(script);
					}
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


	/// A slice of a script file with information on the row, column, and indent of
	/// the slice.
	pub struct QuToken {
		/// Where in the script this token starts.
		pub begin:u64,
		/// Where in the script this token ends.
		pub end:u64,
		/// The row this token is on.
		pub row:u64,
		/// The column this token starts on.
		pub _col:u64,
		/// The indentation of this token.
		pub indent:u8,
		/// The text of this token.
		pub text:String,
		/// The type of this token.
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


		/// Makes a new empty [`Token`].
		pub fn default() -> QuToken {
		return QuToken{
			begin:0,
			end:0,
			row:0,
			_col:0,
			indent:0,
			text:"".to_string(),
			tk_type:0
		};
	}

		// TODO: Replace this function with the text parameter of the struct.
		/// Returns the text of this token.
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

}


use std::{fmt::{self, Display, Debug, format}, vec, collections::HashMap, rc::Rc, mem, hash::Hash};
use derive_new::new;
use qu_tokens::{RULES, TOKEN_TYPE_NAME, tokenize, QuToken};
use qu_error::{QuMsg};


/// A tuple of for specifying arguments for a [QuOperation].
type CommandArg = QuAsmTypes;
type QuRegisterValue = usize;


enum QuAsmTypes {
	uint8,
	uint16,
	uint32,
	uint64,
	str,
} impl QuAsmTypes {
	fn size(&self, code:&Vec<u8>, at:usize) -> usize {
		return match self {
			Self::uint8 => 1,
			Self::uint16 => 2,
			Self::uint32 => 4,
			Self::uint64 => 8,
			Self::str => code[at] as usize,
		};
	}
}


#[derive(Debug, Clone)]
/// A Qu instruction.
pub enum QuLeaf {
	/// A Block of leafs.
	Block(Vec<QuLeaf>),
	/// An if statement. Contains an assertion statement and a [`Vec`] of
	/// instructions.
	FlowStatement(u8, QuLeafExpr, Box<QuLeaf>),
	// A function declaration branch.
	FnDecl(String, Box<QuLeaf>),
	// Call function branch.
	FnCall(String), // TODO: Implement arguments
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
			QuLeaf::FlowStatement(op, cond, body
					) => {
				let bodystr = body.tree_fmt(indent + 1);
				return format!("{}FLOW {} {} {}", indentstr, op, cond, bodystr);
			}
			QuLeaf::FnCall(name) => {
				return format!("{}CALL {}", indentstr, name);
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
	/// A calculable expression. Contains an operator and two [`QuLeafExpr`]s.
	Equation(u8, Box<QuLeafExpr>, Box<QuLeafExpr>),
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


const FLOW_IF:u8 = 0;
const FLOW_WHILE:u8 = 1;
const FLOW_FOR:u8 = 2;
const FLOW_ELIF:u8 = 3;
const FLOW_ELSE:u8 = 4;

const KEYWORD_CLASS:&str = "cl";
const KEYWORD_ELSE:&str = "else";
const KEYWORD_ELIF:&str = "elif";
const KEYWORD_FUNCTION:&str = "fn";
const KEYWORD_IF:&str = "if";
const KEYWORD_PRINT:&str = "print";
const KEYWORD_TRAIT:&str = "tr";
const KEYWORD_TRAIT_IMPL:&str = "does";
const KEYWORD_VAR:&str = "vl";
const KEYWORD_WHILE:&str = "while";

const OP_ASSIGN_WORD:&str = "=";
const OP_BLOCK_START_WORD:&str = ":";


lazy_static! {
    pub static ref OPLIB:QuOpLibrary<'static> = QuOpLibrary::new();
}


/// Defines a [QuOpLibrary] struct
/// 
/// Example:
/// ```
/// struct_QuOpLibrary!{
/// 	[0] end:END()
/// 	[1] add:ADD((1,), (1,), (1,))
/// }
/// let oplib = QuOpLibrary::new();
/// assert_eq(oplib.end, 0);
/// assert_eq(oplib.add, 1);
/// ```
/// 
/// Expands into:
/// ```
/// pub struct QuOpLibrary<'a> {
/// 	ops: Vec<QuOperation<'a>>,
/// 	end: u8,
/// 	add: u8,
/// }
/// impl<'a> QuOpLibrary<'a> {
/// 	fn new() -> Self {
/// 		return Self {
/// 			ops: vec![
/// 				QuOperation::new("end", "END", &[(1,)]),
/// 				QuOperation::new("add", "ADD", &[(1,), (1,), (1,)]),
/// 			],
/// 			end: 0,
/// 			add: 1,
/// 		};
/// 	}
/// }
/// ```
macro_rules! struct_QuOpLibrary {
	// Start (BROKEN)
	( $(  $name:ident:$asm_keyword:ident( $($args:expr),+ )  )+ ) => {
		struct_QuOpLibrary!(
			0, $($name:$asm_keyword($($args),+))+
		);
	};

	// Body (BROKEN)
	( $id:expr, $name:ident:$asm_keyword:ident( $($args:expr),+ ) $(  $name2:ident:$asm_keyword2:ident( $($args2:expr),+ )  )+ ) => {
		struct_QuOpLibrary!(
			id, $name:$asm_keyword($($args),+)
			struct_QuOpLibrary!{
				$(  $id+1, $name2:$asm_keyword2( $($args2),+ )  )+
			}
		);
	};


	// Final (do nothing) (BROKEN)
	( $id:expr ) => {};


	( $(  [$idx:expr] $name:ident:$asm_keyword:ident( $($args:expr),* )  )+ ) => {
		pub struct QuOpLibrary<'a> {
			pub ops:Vec<QuOperation<'a>>,
			$(
				pub $name:u8,
			)+
		} impl<'a> QuOpLibrary<'a> {
			
			fn new() -> Self {
				return Self{
					ops:vec![
						$(
							QuOperation::new(stringify!($name), stringify!($asm_keyword), $idx, &[   $( $args, )*   ]),
						)+
					],
					
					$(
						$name:$idx,
					)+
				};
			}


			fn op_args_size(&self, op:u8) -> usize {
				let mut size = 0;
				for arg in self.ops[op as usize].args {
					size += arg.size(&vec![0 as u8], 0);
				}
				return size;
			}


			/// Converts a math or logic symbol to the id of the [QuOperation] that will
			/// perform it.
			fn op_id_from_symbol(&self, symbol:&str) -> u8 {
				return match symbol {
					"+" => self.add,
					"-" => self.sub,
					"*" => self.mul,
					"/" => self.div,
					"**" => self.pow,
					"%" => self.modulate,
					">" => self.greater,
					"<" => self.lesser,
					"==" => self.equal,
					"!=" => self.not_equal,
					_ => panic!("Unknown Qu VM operation symbol: {}", symbol),
				};
			}

		}
	};
}


/// Generates code for [QuVm] math operation functions.
macro_rules! vm_exc_math_all {
	( $(op $name:ident:$op:tt),* ) => {
		$(
			/// Excecutes a Vm unsigned math operation
			fn $name(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
				// TODO: Performance improvements by operating on value_hold
				// rather than reading a register
				let rg_left = self.next_u8(frame, code) as usize;
				let rg_right = self.next_u8(frame, code) as usize;
				let rg_output = self.next_u8(frame, code) as usize;
				self.hold = (
					frame.registers[rg_left]
					$op frame.registers[rg_right]
				) as usize;
				frame.registers[rg_output] = self.hold;
			}
		)*
	};

	( $(fn $name:ident:$func:path),* ) => {
		$(
			/// Excecutes a Vm unsigned function call
			fn $name(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
				let rg_right = self.next_u8(frame, code) as usize;
				self.hold = ($func(
					self.hold,
					frame.registers[rg_right] as u32)
				) as usize;
			}
		)*
	};
}


// Define the QuOpLibrary struct
struct_QuOpLibrary!{
	[  0] end:END()

	[  1] load_val_u8:LDU8(QuAsmTypes::uint8, QuAsmTypes::uint8)
	[  2] load_val_u16:LDU16(QuAsmTypes::uint16, QuAsmTypes::uint8)
	[  3] load_val_u32:LDU32(QuAsmTypes::uint32, QuAsmTypes::uint8)
	[  4] load_val_u64:LDU64(QuAsmTypes::uint64, QuAsmTypes::uint8)

	[  5] load_mem:LDM(QuAsmTypes::uint32, QuAsmTypes::uint8)
	[  6] store_mem:STM(QuAsmTypes::uint8, QuAsmTypes::uint32)
	[  7] copy_reg:CPY(QuAsmTypes::uint8, QuAsmTypes::uint8)

	[  8] add:ADD(QuAsmTypes::uint8, QuAsmTypes::uint8, QuAsmTypes::uint8)
	[  9] sub:SUB(QuAsmTypes::uint8, QuAsmTypes::uint8, QuAsmTypes::uint8)
	[ 10] mul:MUL(QuAsmTypes::uint8, QuAsmTypes::uint8, QuAsmTypes::uint8)
	[ 11] div:DIV(QuAsmTypes::uint8, QuAsmTypes::uint8, QuAsmTypes::uint8)
	[ 12] modulate:MOD(QuAsmTypes::uint8, QuAsmTypes::uint8, QuAsmTypes::uint8)
	[ 13] pow:POW(QuAsmTypes::uint8, QuAsmTypes::uint8, QuAsmTypes::uint8)
	[ 14] lesser:LES(QuAsmTypes::uint8, QuAsmTypes::uint8, QuAsmTypes::uint8)
	[ 15] greater:GRT(QuAsmTypes::uint8, QuAsmTypes::uint8, QuAsmTypes::uint8)
	[ 16] equal:EQ(QuAsmTypes::uint8, QuAsmTypes::uint8, QuAsmTypes::uint8)
	[ 17] not_equal:NEQ(QuAsmTypes::uint8, QuAsmTypes::uint8, QuAsmTypes::uint8)
	[ 18] not:NOT(QuAsmTypes::uint8, QuAsmTypes::uint8)

	[ 19] jump_to:JP(QuAsmTypes::uint32)
	[ 20] jump_by:JB(QuAsmTypes::uint32)
	[ 21] jump_to_if_not:JPIN(QuAsmTypes::uint32)
	[ 22] jump_by_if_not:JBIN(QuAsmTypes::uint32)

	[ 23] print:PRT(QuAsmTypes::uint8)

	[ 24] call:CALL(QuAsmTypes::uint32)

	[ 25] define_fn:DFFN(QuAsmTypes::uint32, QuAsmTypes::uint32)
	[ 26] define_const_str:DFCS(QuAsmTypes::str)
}


/// The interface for the Qu programming language.
pub struct Qu {
	vm:QuVm,
} impl Qu {
	
	/// Creates a new Qu instance.
	pub fn new() -> Self {
		Qu {
			vm:QuVm::new(),
		}
	}


	/// Compiles Qu code into bytecode.
	pub fn compile(&self, code:&str) -> Result<Vec<u8>, QuMsg> {
		// Tokenize
		let code_str = code.to_string();
		let tokens = &mut tokenize(&code_str, RULES);

		// Parse
		let mut parser = QuParser::new(tokens);
		let leaf_block = parser.parse()?;

		// Compile
		let mut c = QuCompiler::new(&code_str);
		return Ok(c.compile(&leaf_block)?);
	}


	/// Compiles Qu code to Qu assembly.
	pub fn compile_to_asm(&mut self, code:&str) -> Result<String, QuMsg> {
		let code = self.compile(code)?;

		return Ok(self.vm.code_to_asm(&code, false));
	}


	/// Runs a [String] of Qu code.
	pub fn run(&mut self, script:&str) -> Result<(), QuMsg> {
		let code_res = self.compile(script);
		match code_res{
			Ok(code) => {
				let run_res = self.run_bytes(&code);
				if let Err(msg) = &run_res {
					msg.print(&script);
				}
				return run_res;
			}
			Err(msg) => {
				msg.print(&script);
				return Err(msg);
			}
		}
	}


	/// Runs Qu bytecode.
	pub fn run_bytes(&mut self, bytes:&[u8]) -> Result<(), QuMsg> {
		return self.vm.run_bytes(bytes);
	}
	

	/// Runs a [QuFunc].
	pub fn run_fn(&mut self, glob_fn:&QuFunc) -> Result<(), QuMsg> {
		return self.vm.run_bytes(&glob_fn.code);
	}


	/// Runs a global scope [QuFunc].
	pub fn run_global_fn(&mut self, glob_fn:&QuFunc) -> Result<(), QuMsg> {
		return self.run_fn(glob_fn);
	}

}


struct QuCallFrame {
	/// The current instruction pointer.
	pc:usize,
	registers:[QuRegisterValue; 256],

} impl QuCallFrame {

	fn new() -> Self {
		QuCallFrame {
			pc:0,
			registers:[0; 256],
		}
	}

}


/// Compiles [QuLeaf]s into Qu bytecode.
pub struct QuCompiler<'a> {
	/// Name, Type, Pointer
	str_constants:Vec<String>,
	variables:Vec<(String, usize, u8)>,
	functions:HashMap<String, u32>,
	script:&'a String,
	stack_layers:Vec<u8>,
	stack_idx:u8,
	types:Vec<QuType>,
	types_map:HashMap<String, usize>,
} impl<'a> QuCompiler<'a> {

	// TODO: Refactor so that QuCompiler does not need the script to be
	// instantiated. Instead, it should be passed to the compile function.
	/// Creates and returns a new [QuCompiler].
	pub fn new(script:&'a String) -> Self {
		let mut inst = Self{
			str_constants:Vec::default(),
			variables:Vec::default(),
			functions:HashMap::default(),
			script:script,
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
		
		let right_reg = self.stack_reserve();

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
		self.stack_frame_push();
		let if_expr_reg = self.stack_reserve();
		let mut expr_code
			= self.cmp_expr(condition, if_expr_reg)?;
		self.stack_frame_pop();

		// New frame for the code in the 'if' body
		self.stack_frame_push();

		// Compile pieces
		let mut block_code = self.cmp_scope(body)?;
		let mut jump_code = Vec::with_capacity(7);
		jump_code.push(OPLIB.jump_by_if_not);
		jump_code.append(&mut (block_code.len() as i32).to_be_bytes().to_vec());

		// Combine code pieces together
		let mut code = Vec::with_capacity(
				expr_code.len() + jump_code.len() + block_code.len());
		code.append(&mut expr_code);
		code.append(&mut jump_code);
		code.append(&mut block_code);

		// Close 'if' body's frame
		self.stack_frame_pop();

		return Ok(code);
	}


	/// Compiles a *while* statement into bytecode.
	fn cmp_flow_while(&mut self, condition:&QuLeafExpr, body:&Box<QuLeaf>
	) -> Result<Vec<u8>, QuMsg> {
		// Get expression register
		self.stack_frame_push();
		let if_expr_reg = self.stack_reserve();
		// Expression code
		let mut expr_code
			= self.cmp_expr(condition, if_expr_reg)?;
		self.stack_frame_pop();

		// New frame for the code in the 'if' body
		self.stack_frame_push();

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

		// Close 'if' body's frame
		self.stack_frame_pop();
		
		return Ok(code);
	}


	fn cmp_fn_call(&mut self, name:&str) -> Result<Vec<u8>, QuMsg> {
		let name_index = *self.functions.get(&name.to_string())
			.ok_or_else(||{
				panic!("TODO: Need token for QuMsg");
				let msg = QuMsg::general(
					"Compiler attempted to call a function that was not defined. TODO: Better error message");
				return msg;
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
		let result = match leaf {
			QuLeaf::Block(leafs) => {
				let mut code = vec![];
				for block_leaf in leafs {
					let mut block_code = self.cmp_leaf(block_leaf)?;
					code.append(&mut block_code);
				}
				return Ok(code);
			},
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
			QuLeaf::FnCall(
				name,
			) => {
				return self.cmp_fn_call(name);
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
				let print_reg = self.stack_reserve();
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
				// TODO: Make cmp_var_assign return Result and remove this
				// Ok wrapper
				return  self.cmp_var_assign(
					name_rk, value_leaf);
			}

			_ => {
				unimplemented!()
			}
		};
		return result;
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
			(var_token.text.clone(), var_type, var_reg)
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
		self.stack_frame_push();
		let compiled = self.cmp_leaf(leaf);
		self.stack_frame_pop();
		return compiled;
	}


	fn cmp_str_constants(&mut self) -> Result<Vec<u8>, QuMsg> {
		let mut code = vec![];
		for s in &self.str_constants {
			code.push(OPLIB.define_const_str);
			code.push(s.len() as u8);
			for char in s.chars() {
				if !char.is_ascii() {
					let msg = QuMsg::general(
						"String is not ASCII. TODO: Better error message"
					);
					panic!("TODO: Need token for QuMsg");
					return Err(msg);
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


	/// Gets the pointer to a variable by the variable's name.
	fn get_var_register(&self, var_name:&str) -> Option<u8> {
		for (name_, _type_, pointer_) in &self.variables {
			if name_ == var_name {
				return Some(*pointer_);
			}
		}
		return None;
	}


	/// Returns true if the given variable is already defined.
	fn is_var_defined(&self, var_name:&String) -> bool {
		// TODO: Maybe use a faster algorithm??
		for (name_, _, _) in &self.variables {
			if name_ == var_name {
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
	fn stack_frame_pop(&mut self) {
		self.stack_idx = self.stack_layers.pop().unwrap();
	}


	/// Starts a new stack frame.
	fn stack_frame_push(&mut self) {
		self.stack_layers.push(self.stack_idx);
	}

}


/// A declared Qu function. Contains all the metadata for a defined function.
pub struct QuFunc {
	/// The name of this funciton.
	name:String,
	/// The index that is function is stored at in the function list.
	id:usize,
	arg_list:Vec<u8>,
	/// The code of this function.
	code:Vec<u8>,
	/// The start index of this function's code.
	code_start:usize,

} impl QuFunc {

	fn new(name:&str, id:usize, code_start:usize) -> Self {
		return Self{
			name: name.to_owned(),
			id: id,
			arg_list: Vec::default(),
			code: Vec::default(),
			code_start: code_start,
		}
	}

}


/// A single Qu VM operation.
pub struct QuOperation<'a> {
	name:String,
	asm_keyword:String,
	args:&'a [CommandArg],
	id:u8,
} impl<'a> QuOperation<'a> {
	fn new(name:&str, asm_keyword:&str, id:u8, args:&'a [CommandArg]) -> Self {
		return Self{
			name:name.to_string(),
			asm_keyword:asm_keyword.to_string(),
			args:args,
			id:id,
		};
	}
}


/// Parses Qu code into a [QuLeaf] tree.
pub struct QuParser<'a> {
	indent:u8,
	line:usize,
	tk_idx:usize,
	tk_stack:Vec<usize>,
	tokens:&'a Vec<QuToken>,
	opslib:QuOpLibrary<'a>,

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
			opslib:QuOpLibrary::new(),
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

			// Function call
			ck_parse!(ck_fn_call);

			// Unexpected token
			return Err(QuMsg::invalid_token(
				&self.tokens[self.tk_idx].text));
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
			QuMsg::flow_statement_lacks_expression(keyword)
		)?;

		// Check for code block
		let scope_data = match self.ck_code_scope() {
			Ok(leaf) => leaf.ok_or(
				QuMsg::missing_code_block()
			)?,
			Err(msg) => {
				// If the error is related to indentation, replace the error
				// with a missing value error
				if msg.title == qu_error::ERR_TITLE_INVALID_INDENTATION {
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
	fn ck_fn_call(&mut self) -> Result<Option<QuLeaf>, QuMsg> {
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
		if self.tk_next()? != "(" {
			return Err(QuMsg::missing_token("("));
		}

		// Match a close ')'
		if self.tk_next()? != ")" {
			return Err(QuMsg::missing_token(")"));
		}

		return Ok(Some(QuLeaf::FnCall(fn_name_tk.text)));
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
		let fn_name_tk = self.ck_fn_name()?.ok_or(
			// TODO: Change to more appropriate message
			QuMsg::missing_code_block()
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
		let reg_tk = self.ck_value()
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
				self.opslib.op_id_from_symbol(operator),
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
		let name_tk = self.ck_var_name()?.ok_or(
			QuMsg::general(
				"Token after 'var' does not match a name. 'TODO:Better msg'")
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
					if msg.title == qu_error::ERR_TITLE_INVALID_INDENTATION {
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
				let tk = &self.tokens[self.tk_idx];
				return Err(QuMsg::one_liner());
			}

			let tk = &self.tokens[self.tk_idx-0];
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
	/// see [func@`Parser::tk_pop`]
	fn tk_state_save(&mut self) {
		self.tk_stack.push(self.tk_idx);
	}


	/// Returns a &[`QuToken`] relative to the current token index without
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

/// An object type (Example: int, bool, String, Object).
pub struct QuType {
	name:String,
	size:usize,
} impl QuType {

	/// Makes a new [`QuType`].
	fn new(name:String, size:usize) -> QuType {
		return QuType{
			name,
			size,
		};
	}


	/// Makes a boolean [`QuType`].
	fn bool() -> QuType {
		// TODO:: Make "bool" string a constant
		return QuType::new("bool".to_string(), 1);
	}


	/// Makes an integer [`QuType`].
	fn int() -> QuType {
		// TODO: Make "int" string a constant
		return QuType::new("int".to_string(), 1);
	}


	/// Makes an unsigned integer [`QuType`].
	fn uint() -> QuType {
		// TODO:: Make "uint" string a constant
		return QuType::new("uint".to_string(), 1);
	}

}


/// The virtual machine that runs Qu code.
pub struct QuVm {
	/// A [Vec] of all defined [QuFunc]s.
	functions:Vec<QuFunc>,
	/// All defined constant [String]s.
	str_constants:Vec<String>,
	/// Holds the outputed value of the last executed operation.
	hold:usize,

} impl QuVm {

	/// Makes a new [QuVm].
	pub fn new() -> Self {
		let vm = QuVm { 
			functions: Vec::default(),
			str_constants: Vec::default(),
			hold: usize::default(),
		};

		return vm;
	}


	/// Returns a new [QuVm] wrapped in an [Rc].
	pub fn new_rc() -> Rc<Self> {
		return Rc::new(Self::new());
	}


	/// Converts byte code to human readable Qu assembly instructions.
	pub fn code_to_asm(&mut self, code:&Vec<u8>, include_line_columns:bool
	) -> String {
		let mut asm = String::new();
		
		let mut i = 0;
		while i < code.len() {
			let op_code = code[i];
			// HACK: Skip commands if they exceed the ops length.
			if op_code as usize >= OPLIB.ops.len() {
				i += 1;
				// Add error text
				asm.push_str(format!("\n{:.>8}-{:.<8} {}",
					i, i, "INVALID OPERATION").as_str());
				continue;
			}
			let op
					= &OPLIB.ops[op_code as usize];
			assert!(op.id == op_code);

			asm.push_str("\n");
			// Add line/index columns
			if include_line_columns {
				asm.push_str(
					format!("{:.>8}-{:.<8} ",
					i, i+op.args.len()).as_str()
				);
			}
			// Add code text
			asm.push_str(
				format!("{}",op.asm_keyword).as_str());

			// Add parameter text
			for asm_type in op.args.iter() {
				let size = asm_type.size(code, i+1);
				// Get value
				let val = match asm_type {
					QuAsmTypes::uint8 => {
						let bytes = [code[i+1]];
						i += 1;
						format!("{}", u8::from_be_bytes(bytes))
					}
					QuAsmTypes::uint16 => {
						let bytes = [code[i+1], code[i+2]];
						i += 2;
						format!("{}", u16::from_be_bytes(bytes))
					}
					QuAsmTypes::uint32 => {
						let bytes = [
							code[i+1], code[i+2], code[i+3], code[i+4]];
						i += 4;
						format!("{}", u32::from_be_bytes(bytes))
					}
					QuAsmTypes::uint64 => {
						let bytes = [
							code[i+1], code[i+2], code[i+3], code[i+4],
							code[i+5], code[i+6], code[i+7], code[i+8]];
						i += 8;
						format!("{}", u64::from_be_bytes(bytes))
					}
					QuAsmTypes::str => {
						let mut val = "\"".to_string();
						for _ in 0..size+1 {
							val.push(code[i+1] as char);
							i += 1;
						}
						val.push('"');
						val
					}
					_ => panic!(),
				};
				asm.push_str(format!(" {}{}", "", val).as_str());
			}
			i += 1;
		}

		return asm;
	}


	/// Defines a constant [String]. TODO: Better documentation
	pub fn define_const_string(&mut self, value:&str) {
		// TODO: Error handling
		self.str_constants.push(value.to_owned());
	}


	/// Defines a Qu function. TODO: Better documentation
	pub fn define_fn(&mut self, name:&str, code_start:usize) {
		let id = self.functions.len();
		self.functions.push(QuFunc::new(&name, id, code_start));
	}


	fn exc_copy_reg(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		let from_reg = self.next_u8(frame, code) as usize;
		let to_reg = self.next_u8(frame, code) as usize;
		frame.registers[to_reg] = frame.registers[from_reg];
	}


	/// Reads the bytecode of a function call command and executes it.
	fn exc_call_fn(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		let mut next_frame = QuCallFrame::new();

		let fn_id = self.next_u32(frame, code) as usize;
		next_frame.pc = self.functions[fn_id].code_start;

		self.do_loop(&mut next_frame, code);
	}


	fn exc_define_const_str(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		let string = self.next_ascii(frame, code);
		self.str_constants.push(string);
	}


	/// Defines a function from next bytes in the code.
	fn exc_define_fn(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		let name_const_idx = self.next_u32(frame, code) as usize;
		let fn_length = self.next_u32(frame, code) as usize;

		let name = &self.str_constants[name_const_idx];
		let code_start = frame.pc;

		self.define_fn(&name.clone(), code_start);
		frame.pc += fn_length;
	}


	fn exc_jump_by(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		let val_by = self.next_u32(frame, code) as i32;
		// Add
		if val_by > 0 {
			frame.pc += val_by as usize;
		// Subtract
		} else {
			frame.pc -= val_by.abs() as usize;
		}
		
	}


	fn exc_jump_by_if_not(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		let val_by = self.next_u32(frame, code) as i32;
		if !self.is_hold_true() {
			// Add
			if val_by > 0 {
				frame.pc += val_by as usize;
			// Subtract
			} else {
				frame.pc -= val_by.abs() as usize;
			}
		}
	}


	fn exc_jump_to(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		unimplemented!()
	}


	fn exc_jump_to_if_not(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		let val_to = self.next_u32(frame, code) as usize;
		if !self.is_hold_true() {
			frame.pc = val_to as usize;
		}
	}


	fn exc_load_val_u8(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		self.hold = self.next_u8(frame, code) as QuRegisterValue;
		let rg_to = self.next_u8(frame, code) as usize;
		frame.registers[rg_to] = self.hold;
	}


	fn exc_load_val_u16(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		self.hold = self.next_u16(frame, code) as QuRegisterValue;
		let rg_to = self.next_u8(frame, code) as usize;
		frame.registers[rg_to] = self.hold;
	}


	fn exc_load_val_u32(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		self.hold = self.next_u32(frame, code) as QuRegisterValue;
		let rg_to = self.next_u8(frame, code) as usize;
		frame.registers[rg_to] = self.hold;
	}


	fn exc_load_val_u64(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		self.hold = self.next_u64(frame, code) as QuRegisterValue;
		let rg_to = self.next_u8(frame, code) as usize;
		frame.registers[rg_to] = self.hold;
	}


	fn exc_load_mem(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		unimplemented!();
	}


	fn exc_print(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		let read_from_reg = self.next_u8(frame, code) as usize;
		let val = frame.registers[read_from_reg];
		println!("Qu Print: {}", val);
	}


	fn exc_store_mem(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		unimplemented!();
	}


	vm_exc_math_all!{
		op exc_math_add: +,
		op exc_math_sub: -,
		op exc_math_mul: *,
		op exc_math_div: /,
		op exc_math_mod: %,
		op exc_logi_equal: ==,
		op exc_logi_greater: >,
		op exc_logi_lesser: <,
		op exc_logi_not_equal: !=
	}

	
	vm_exc_math_all!{
		fn exc_math_pow: usize::pow
	}


	fn exc_logi_not(&mut self, frame:&mut QuCallFrame, code:&[u8]) {
		let rg_left = self.next_u8(frame, code) as usize;
		let rg_result = self.next_u8(frame, code) as usize;
		let x = frame.registers[rg_left];
		frame.registers[rg_result] = 
				(x*0 == x) as QuRegisterValue;
	}


	#[inline]
	fn is_hold_true(&mut self) -> bool {
		return self.hold != 0;
	}


	/// Loads a constant [String] from the specified location in the bytecode.
	fn load_constant_string(&mut self, code:&[u8],
	at:usize) -> String {
		let str_size = code[at] as usize;
		let mut str_vec = Vec::with_capacity(str_size);
		for i in at+1..(at+1+str_size) {
			str_vec.push(code[i]);
		}
		
		return String::from_utf8(str_vec).expect("TODO: Handle this error");
	}


	fn next_ascii(&mut self, frame:&mut QuCallFrame, code:&[u8]) -> String {
		let str_size = self.next_u8(frame, code);
		let mut str_vec = Vec::with_capacity(str_size as usize);
		for _ in 0..str_size {
			str_vec.push(self.next_u8(frame, code));
		}

		return String::from_utf8(str_vec).expect("TODO: Handle this error");
	}


	/// Gets the next byte in the source code as a u8 int.
	fn next_u8(&mut self, frame:&mut QuCallFrame, code:&[u8]) -> u8 {
		let val = code[frame.pc];
		frame.pc += 1;
		return val;
	}


	/// Gets the next 2 byte in the source code as a u16 int.
	fn next_u16(&mut self, frame:&mut QuCallFrame, code:&[u8]) -> u16 {
		let bytes = [code[frame.pc], code[frame.pc+1]];
		frame.pc += 2;
		return u16::from_be_bytes(bytes);
	}


	/// Gets the next 4 byte in the source code as a u32 int.
	fn next_u32(&mut self, frame:&mut QuCallFrame, code:&[u8]) -> u32 {
		let bytes = [
			code[frame.pc], code[frame.pc+1],
			code[frame.pc+2], code[frame.pc+3],
		];
		frame.pc += 4;
		return u32::from_be_bytes(bytes);
	}


	// Gets the next 8 byte in the source code as a u64 int.
	fn next_u64(&mut self, frame:&mut QuCallFrame, code:&[u8]) -> u64 {
		let bytes = [
			code[frame.pc], code[frame.pc+1],
			code[frame.pc+2], code[frame.pc+3],
			code[frame.pc+4], code[frame.pc+5],
			code[frame.pc+6], code[frame.pc+7],
		];
		frame.pc += 8;
		return u64::from_be_bytes(bytes);
	}


	/// Runs inputed bytecode in a loop.
	fn do_loop(&mut self, frame:&mut QuCallFrame, bytecode:&[u8]
	) -> Result<(), QuMsg>{
		while frame.pc != bytecode.len() {
			let result = self.do_next(frame, bytecode);
			if let Err(e) = result {
				if e.description == "Done" {return Ok(())};
			}
		}
		return Ok(());
	}


	/// Runs the next command in the given bytecode.
	fn do_next(&mut self, frame:&mut QuCallFrame, bytecode:&[u8]) -> Result<(), QuMsg> {
		// TODO: Error handling for running operations
		let op = self.next_u8(frame, bytecode);
//		println!("DidOp {}", OPLIB.ops[op as usize].name);
		match op {
			x if x == OPLIB.end as u8 => {return Err(QuMsg::general("Done"))},

			x if x == OPLIB.load_val_u8 => self.exc_load_val_u8(frame, bytecode),
			x if x == OPLIB.load_val_u16 => self.exc_load_val_u16(frame, bytecode),
			x if x == OPLIB.load_val_u32 => self.exc_load_val_u32(frame, bytecode),
			x if x == OPLIB.load_val_u64 => self.exc_load_val_u64(frame, bytecode),
			x if x == OPLIB.load_mem => self.exc_load_mem(frame, bytecode),
			x if x == OPLIB.store_mem => self.exc_store_mem(frame, bytecode),
			x if x == OPLIB.copy_reg => self.exc_copy_reg(frame, bytecode),
			x if x == OPLIB.add => self.exc_math_add(frame, bytecode),
			x if x == OPLIB.sub => self.exc_math_sub(frame, bytecode),
			x if x == OPLIB.mul => self.exc_math_mul(frame, bytecode),
			x if x == OPLIB.div => self.exc_math_div(frame, bytecode),
			x if x == OPLIB.modulate => self.exc_math_mod(frame, bytecode),
			x if x == OPLIB.pow => self.exc_math_pow(frame, bytecode),
			x if x == OPLIB.lesser => self.exc_logi_lesser(frame, bytecode),
			x if x == OPLIB.greater => self.exc_logi_greater(frame, bytecode),
			x if x == OPLIB.equal => self.exc_logi_equal(frame, bytecode),
			x if x == OPLIB.not_equal => self.exc_logi_not_equal(frame, bytecode),
			x if x == OPLIB.not => self.exc_logi_not(frame, bytecode),
			x if x == OPLIB.jump_to => self.exc_jump_to(frame, bytecode),
			x if x == OPLIB.jump_by => self.exc_jump_by(frame, bytecode),
			x if x == OPLIB.jump_to_if_not => self.exc_jump_to_if_not(frame, bytecode),
			x if x == OPLIB.jump_by_if_not => self.exc_jump_by_if_not(frame, bytecode),
			x if x == OPLIB.print => self.exc_print(frame, bytecode),
			x if x == OPLIB.call => self.exc_call_fn(frame, bytecode),
			x if x == OPLIB.define_fn => self.exc_define_fn(frame, bytecode),
			x if x == OPLIB.define_const_str => self.exc_define_const_str(frame, bytecode),

			x => { println!("{x}"); todo!(); }
		};
		return Ok(());
	}



	/// Run the passed [QuFunc] in a new [QuCallFrame].
	pub fn run_func(&mut self, func:&QuFunc) -> Result<(), QuMsg> {
		return self.run_bytes(&func.code);
	}


	/// Run the passed bytecode in the topmost [QuCallFrame].
	pub fn run_bytes(&mut self, bytecode:&[u8]) -> Result<(), QuMsg> {
		let mut call_frame = QuCallFrame::new();
		while call_frame.pc != bytecode.len() {
			match self.do_next(&mut call_frame, bytecode) {
				// No errors, continue running
				Ok(_) => {/*pass*/},
				// Encountered error; check if done and finish, otherwise
				// propogate error
				Err(msg) => {
					if msg.description == "Done" {
						break;
					}
					return Err(msg);
				},
			};
		}

		return Ok(());
	}

}

