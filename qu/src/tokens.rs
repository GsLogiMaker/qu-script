
use std::fmt::{self, Display, Debug};

use crate::parser::{
	KEYWORD_VAR,
	KEYWORD_FN,
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


/// A [Vec] of tokenfule_* functions and their types.
pub type Rules<'a> = [(&'a dyn Fn(&[char])->bool, u8)];


/// Returns *true* if the passed characters match to a name.
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

	/// Makes a new [`QuToken`].
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


	/// Makes a new empty [`QuToken`].
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
		//return write!(f, "Tokwn: {txt}", 0);
		f.debug_struct("QuToken")
		//	.field("begin", &self.begin)
		//	.field("end", &self.end)
		//	.field("row", &self.row)
		//	.field("_col", &self._col)
		//	.field("indent", &self.indent)
		//	.field("source", &self.source)
			.field("text", &self.text)
		//	.field("tk_type", &self.tk_type).finish()
			.finish()
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