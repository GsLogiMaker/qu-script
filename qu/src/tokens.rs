
use std::fmt;
use std::fmt::Display;
use std::fmt::Debug;

use crate::parser::KEYWORD_VAR;
use crate::parser::KEYWORD_FN;
use crate::parser::KEYWORD_CLASS;
use crate::parser::KEYWORD_IF;
use crate::parser::KEYWORD_ELSE;
use crate::parser::KEYWORD_ELIF;
use crate::parser::QuOperator;


pub const TOKEN_TYPE_KEYWORD:u8 = 1;
pub const TOKEN_TYPE_SYMBOL:u8 = 2;
pub const TOKEN_TYPE_NUMBER:u8 = 3;
pub const TOKEN_TYPE_NAME:u8 = 4;


/// Base token rules for the Qu language.
pub const RULES:&Rules = &[
	(&tokenrule_keyword, TOKEN_TYPE_KEYWORD),
	(&tokenrule_symbols, TOKEN_TYPE_SYMBOL),
	(&tokenrule_number, TOKEN_TYPE_NUMBER),
	(&tokenrule_identity, TOKEN_TYPE_NAME),
];


/// A [Vec] of tokenfule_* functions and their types.
pub type Rules<'a> = [(&'a dyn Fn(&[char])->bool, u8)];


/// Returns *true* if the passed characters match to a name.
pub fn tokenrule_identity(added_so_far:&[char]) -> bool {
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
pub fn tokenrule_number(added_so_far:&[char]) -> bool {
	for char in  added_so_far {
		if char == &' ' {
			return false
		}
		if !(char.is_numeric()) {
			return false;
		}
	}

	return true;
}


/// Returns *true* if the passed characters match to a keyword.
pub fn tokenrule_keyword(added_so_far:&[char]) -> bool {
	for word in [
		KEYWORD_VAR,
		KEYWORD_FN,
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
pub fn tokenrule_symbols(added_so_far:&[char]) -> bool {
	return match added_so_far {
		['.',] => true,
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
pub fn tokenize<'a>(script:&'a str, rules:&Rules<'a>) -> Vec<QuToken> {
	let mut tokens = vec!();

	/* WARNING: This does not account for grapheme clusters. Currently hoping
	This won't be a problem. */
	let mut row:u32 = 0;
	let mut column:u8 = 0;
	let mut indent:u8 = 0;
	let mut tk_start:usize = 0;
	let mut in_new_line:bool = true;
	let mut added_so_far:Vec<char> = Vec::with_capacity(20);
	let mut curr_token = 0;

	for (index, char) in script.char_indices() {
		column += 1;

		if char != '\t' && char != ' ' {
			in_new_line = false;
		}

		// Check tab
		if char == '\t' {
			if in_new_line {
				indent += 1;
				added_so_far.clear();
			}
			
		// Check newline
		} else if char == '\n' {
			column = 0;
			row += 1;
			indent = 0;
			in_new_line = true;		
		}

		// Any other characters
		added_so_far.push(char);
		
		// Update token end if it fits rule, 
		// otherwise clear the added so far
		loop {
			let (does_fit, tk_type) = chars_fit_rule(
				&added_so_far,
				rules,
			);
			if does_fit{
				if curr_token <= tokens.len() && added_so_far.len() == 1 {
					tokens.push(QuToken::new(
						row,
						column,
						indent,
						tk_type,
						""
					));
					tk_start = index;
				}
				tokens[curr_token].tk_type = tk_type;
				if index+1 == script.len() {
					tokens[curr_token].slice
						= script[tk_start..=index].to_owned();
				}
				break;
				
			} else if added_so_far.len() == 1 {
				added_so_far.clear();
				break;
			}else {
				if curr_token+1 == tokens.len() {
					tokens[curr_token].slice
						= script[tk_start..index].to_owned();
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
#[derive(Clone, Default)]
pub struct QuToken {
	pub char_index:QuCharIndex,
	/// The type of this token.
	pub tk_type:u8,
	/// The slice of text this token represents.
	pub slice:String,

} impl QuToken {

	/// Makes a new [`QuToken`].
	pub fn new(
			row:u32, column:u8, indent:u8, tk_type:u8, slice:&str
	) -> QuToken {
		return QuToken {
			char_index: QuCharIndex::new(row, column, indent),
			tk_type,
			slice: slice.to_owned(),
		};
	}


	pub fn from(slice:&str) -> QuToken {
		return Self {
			char_index: QuCharIndex::default(),
			tk_type: 0,
			slice: slice.to_owned(),
		};
	}

} impl Display for QuToken {
	
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		return write!(
				f, "<row:{}  indent:{}>",
				self.char_index.row,
				self.char_index.indent,);
	}

} impl Debug for QuToken {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		return write!(f, "QuToken(\"{}\")", self.slice);
	}
} impl PartialEq for QuToken {
	fn eq(&self, other:&Self) -> bool {
		if self.tk_type == u8::MAX {
			return false;
		}
		return self.slice == other.slice;
	}
} impl PartialEq<str> for QuToken {
	fn eq(&self, other:&str) -> bool {
		if self.tk_type == u8::MAX {
			return false;
		}
		return self.slice == other;
	}
} impl PartialEq<&str> for QuToken {
	fn eq(&self, other:&&str) -> bool {
		if self.tk_type == u8::MAX {
			return false;
		}
		return self.slice == *other;
	}
} impl PartialEq<String> for QuToken {
	fn eq(&self, other:&String) -> bool {
		if self.tk_type == u8::MAX {
			return false;
		}
		return &self.slice == other;
	}
} impl From<&str> for QuToken {
	fn from(value: &str) -> Self {
		QuToken::from(value)
	}
}

impl From<QuToken> for String {
	fn from(value: QuToken) -> Self {
		value.slice
	}
}

impl<'a> From<&'a QuToken> for &'a str {
	fn from(value: &'a QuToken) -> &'a str {
		&value.slice
	}
}


#[derive(Clone, Debug, Default)]
/// Stores human readable information the location of a text character.
pub struct QuCharIndex {
	/// The row this chararacter is on.
	pub row:u32,
	/// The column this chararacter on.
	pub column:u8,
	/// The indentation of this chararacter.
	pub indent:u8,
} impl QuCharIndex {

	fn new(row:u32, column:u8, indent:u8) -> Self {
		return Self {
			row,
			column,
			indent,
		};
	}

}