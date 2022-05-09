
const RULES:&[& dyn Fn(&Vec<char>)->bool] = &[
	&tokenrule_keyword,
	&tokenrule_symbols,
	&tokenrule_number,
	&tokenrule_name,
];


struct Token {
	begin:u64,
	end:u64,
	row:u64,
	col:u64,
	indent:u8,

} impl Token {

	fn new(begin:u64, end:u64, row:u64, col:u64, indent:u8) -> Token {
		return Token{begin, end, row, col, indent};
	}


	fn len(self) -> u64 {
		return self.end-self.begin;
	}


	fn text(self, text:&str) -> String {
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

}


fn tokenrule_name(added_so_far:&Vec<char>) -> bool {
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


fn tokenrule_number(added_so_far:&Vec<char>) -> bool {
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


fn tokenrule_keyword(added_so_far:&Vec<char>) -> bool {
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


fn tokenrule_symbols(added_so_far:&Vec<char>) -> bool {
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


fn tokenize<'a>(script:&'a str) -> Vec<Token> {
	let mut tokens = vec!();

	

	/* WARNING: This does not account for grapheme clusters. Currently hoping
	This won't be a problem. */
	let mut state:u8 = 0;
	let mut row:u64 = 0;
	let mut col:u64 = 0;
	let mut indent:u8 = 0;
	let mut in_begining:bool = true;
	let mut added_so_far:Vec<char> = Vec::with_capacity(20);
	let mut curr_token = 0;
	for (idx, char) in script.char_indices() {
		col += 1;

		if char != '\t' {
			in_begining = false;
		}

		// Check tab
		if char == '\t' {
			indent += 1;
			added_so_far.clear();

		// Check newline
		} else if char == '\n' {
			col = 0;
			row += 1;
			indent = 0;

		// Any other characters
		} else {
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
							indent));
					}
					tokens[curr_token].end = idx as u64;
					break;
					
				} else if added_so_far.len() == 1 {
					added_so_far.clear();
					break;
				}else {
					curr_token += 1;
					added_so_far.clear();
					added_so_far.push(char);
				}
			}
			
		}
	}

	return tokens;
}


fn chars_fit_rule(chars:&Vec<char>) -> bool {
	let mut fits_rule = false;
	for rule in RULES {
		fits_rule = fits_rule || rule(&chars);
		if fits_rule{
			break;
		}
	}

	return fits_rule;
}


fn main() {
	let script = "var default=128";
	let a = tokenize(script);

	for token in a {
		let b = token.text(script);
		println!("'{b}'")
	}
}
