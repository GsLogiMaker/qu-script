

//! TODO: Project level documentation.


#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
#![warn(rustdoc::broken_intra_doc_links)]


use std::fmt::{self};


/// An array of function pointers to be used in [`tokenize`].
/// 
/// This array is used by [`tokenize`] to determin if a pattern of characters
/// should be turned into a [`Token`]. See [`tokenrule_name`] or 
/// [`tokenrule_keyword`] for examples of how a `tokenrule_*` function should
/// be structured.
const RULES:&[& dyn Fn(&Vec<char>)->bool] = &[
	&tokenrule_keyword,
	&tokenrule_symbols,
	&tokenrule_number,
	&tokenrule_name,
];


pub struct Token<'a> {
	begin:u64,
	end:u64,
	row:u64,
	_col:u64,
	indent:u8,
	source:&'a str,

} impl<'a> Token<'a> {

	fn new(
			begin:u64, end:u64, row:u64, 
			col:u64, indent:u8, source:&'a str) -> Token {
		return Token{begin, end, row, _col: col, indent, source};
	}


	fn text(&self, text:&str) -> String {
		let mut result = String::default();
		for (idx, char) in text.char_indices() {
			if idx > self.end as usize {
				break
			}
			if idx >= self.begin as usize {
				result.push(char);
			}
		}
		return result;
	}

} impl<'a> std::fmt::Display for Token<'a> {
	
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		return write!(
				f, "<'{}' row:{}  indent:{}>",
				self.text(self.source),
				self.row,
				self.indent,);
	}
}


pub fn tokenrule_name(added_so_far:&Vec<char>) -> bool {
	for char in  added_so_far.as_slice() {
		if char == &' ' {
			return false
		}
		if !char.is_alphanumeric() || char == &'_' {
			return false;
		}
	}

	return true;
}


pub fn tokenrule_number(added_so_far:&Vec<char>) -> bool {
	for char in  added_so_far.as_slice() {
		if char == &' ' {
			return false
		}
		if !(char.is_numeric() || char == &'.') {
			return false;
		}
	}

	return true;
}


pub fn tokenrule_keyword(added_so_far:&Vec<char>) -> bool {
	return match added_so_far.as_slice() {
		['v', 'a', 'r',] => true,
		['f', 'n',] => true,
		['c', 'l', 'a', 's', 's',] => true,
		['i', 'f',] => true,
		['e', 'l', 's', 'e',] => true,
		['e', 'l', 'i', 'f',] => true,
		_ => false,
	};
}


pub fn tokenrule_symbols(added_so_far:&Vec<char>) -> bool {
	return match added_so_far.as_slice() {
		['+',] => true,
		['-',] => true,
		['*',] => true,
		['/',] => true,
		['%',] => true,
		['^',] => true,
		['!',] => true,
		[':',] => true,
		[',',] => true,
		[';',] => true,
		['=',] => true,
		['=', '=',] => true,
		['!', '=',] => true,
		['>', '=',] => true,
		['<', '=',] => true,
		['+', '=',] => true,
		['-', '=',] => true,
		['*', '=',] => true,
		['/', '=',] => true,
		['%', '=',] => true,
		['^', '=',] => true,
		_ => false,
	};
}


/// Takes a &[str] and returns a [Vec] of [Token]s.
/// 
/// The rules govorning what becomes a [Token] are specified by the functions
/// in [RULES].
/// 
/// Example
/// ```
/// extern crate ge_script;
/// use ge_script::Token;
/// use ge_script::tokenize;
/// 
/// let script:&str = " hello=world ;! ";
/// 
/// let tokens:Vec<Token> = tokenize(&script);
/// 
///	assert!(tokens.len() == 5);
///	assert!(tokens[0].text(&script) == "hello");
///	assert!(tokens[1].text(&script) == "=");
///	assert!(tokens[2].text(&script) == "world");
///	assert!(tokens[3].text(&script) == ";");
///	assert!(tokens[4].text(&script) == "!");
/// ```
pub fn tokenize<'a>(script:&'a str) -> Vec<Token> {
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
			if chars_fit_rule(&added_so_far){
				if curr_token <= tokens.len() && added_so_far.len() == 1 {
					tokens.push(Token::new(
						idx as u64,
						idx as u64,
						row,
						col,
						indent,
						script,));
				}
				tokens[curr_token].end = idx as u64;
				break;
				
			} else if added_so_far.len() == 1 {
				added_so_far.clear();
				break;
			}else {
				if curr_token+1 == tokens.len() {
					curr_token += 1;
				}
				added_so_far.clear();
				added_so_far.push(char);
			}
		}
	}

	return tokens;
}


pub fn chars_fit_rule(chars:&Vec<char>) -> bool {
	let mut fits_rule = false;
	for rule in RULES {
		fits_rule = fits_rule || rule(&chars);
		if fits_rule{
			break;
		}
	}

	return fits_rule;
}