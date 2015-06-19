use syntax::reader::Reader;
use ::{JsResult, JsError};
use std::char;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
	Null,
	Boolean(bool),
	String(Rc<String>),
	Number(f64)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
	CloseBrace,
	CloseBracket,
	Colon,
	Comma,
	Literal(Lit),
	OpenBrace,
	OpenBracket
}

pub struct Lexer<'a> {
	reader: &'a mut Reader,
	look_ahead: Option<Token>
}

impl<'a> Lexer<'a> {
	pub fn new(reader: &'a mut Reader) -> JsResult<Lexer<'a>> {
		Ok(Lexer {
			reader: reader,
			look_ahead: None
		})
	}
	
	pub fn is_eof(&mut self) -> JsResult<bool> {
		Ok(try!(self.peek()).is_none())
	}
	
	pub fn next(&mut self) -> JsResult<Token> {
		if let Some(token) = try!(self.peek()) {
			self.look_ahead = None;
			
			Ok(token)
		} else {
			self.fatal("unexpected end of input")
		}
	}
	
	pub fn peek(&mut self) -> JsResult<Option<Token>> {
		if let Some(ref token) = self.look_ahead {
			Ok(Some(token.clone()))
		} else {
			loop {
				if self.reader.is_eof() {
					return Ok(None);
				}
				
				let token = try!(self.parse());
				
				if token.is_some() {
					self.look_ahead = token.clone();
					
					return Ok(token);
				}
			}
		}
	}

	fn fatal<T>(&self, message: &str) -> JsResult<T> {
		let (line, col) = self.reader.pos();
		
		Err(JsError::Lex(format!("{}:{}: {}", line, col, message.to_string())))
	}
	
	fn parse(&mut self) -> JsResult<Option<Token>> {
		let token = match self.reader.next() {
			'[' => Token::OpenBracket,
			']' => Token::CloseBracket,
			'{' => Token::OpenBrace,
			'}' => Token::CloseBrace,
			',' => Token::Comma,
			':' => Token::Colon,
			'.' if is_digit(self.reader.peek()) => try!(self.parse_decimal_tail(".".to_string())),
			c @ '0' ... '9' => try!(self.parse_number(c, true)),
			'"' => try!(self.parse_string()),
			c @ _ if is_line_terminator(c) => return Ok(None),
			c @ _ if is_whitespace(c) => {
				self.skip_while(is_whitespace);
				return Ok(None)
			},
			_ => {
				self.reader.skip(-1);
				
				if let Some(identifier) = try!(self.parse_identifier()) {
					try!(self.token_from_identifier(identifier))
				} else {
					return self.fatal("cannot parse");
				}
			}
		};
		
		Ok(Some(token))
	}
	
	fn token_from_identifier(&self, identifier: String) -> JsResult<Token> {
		match &*identifier {
			"null" => Ok(Token::Literal(Lit::Null)),
			"true" => Ok(Token::Literal(Lit::Boolean(true))),
			"false" => Ok(Token::Literal(Lit::Boolean(false))),
			_ => self.fatal("cannot parse identifier")
		}
	}
	
	fn skip_while<F: Fn(char) -> bool>(&mut self, predicate: F) {
		while !self.reader.is_eof() && predicate(self.reader.peek()) {
			self.reader.next();
		}
	}
	
	fn parse_str_number(&self, value: &str, radix: u32) -> Option<Token> {
		match i64::from_str_radix(value, radix) {
			Ok(value) => Some(Token::Literal(Lit::Number(value as f64))),
			_ => None
		}
	}
	
	fn parse_number(&mut self, c: char, positive: bool) -> JsResult<Token> {
		// Scan for a decimal point or exponent letter. If so, don't parse this
		// number as having a leading zero.
		
		let mut is_decimal = false;
		let pos = self.reader.offset();
		
		while !self.reader.is_eof() {
			match self.reader.next() {
				'.' | 'e' | 'E' => {
					is_decimal = true;
					break;
				}
				'0'...'9' => {},
				_ => break
			}
		}
		
		self.reader.seek(pos);
		
		if !is_decimal && c == '0' {
			if positive {
				Ok(Token::Literal(Lit::Number(0_f64)))
			} else {
				Ok(Token::Literal(Lit::Number(-0_f64)))
			}
		} else {
			let mut prefix = String::new();
			if !positive {
				prefix.push('-');
			}
			prefix.push(c);
			self.parse_decimal(prefix.to_string())
		}
	}
	
	fn parse_decimal(&mut self, prefix: String) -> JsResult<Token> {
		// This method parses a decimal without already having seen a dot. The decimal
		// prefix is parsed here and the rest is handled  by parse_decimal_tail.
		
		let mut s = prefix;
		
		while !self.reader.is_eof() {
			let c = self.reader.peek();
			if c == '.' || is_digit(c) {
				s.push(self.reader.next());
				
				if c == '.' {
					break;
				}
			} else {
				break;
			}
		}
		
		self.parse_decimal_tail(s)
	}
	
	fn parse_decimal_tail(&mut self, prefix: String) -> JsResult<Token> {
		// This method parses decimal tails. The prefix contains what has already been
		// parsed and may contain a dot. This method will not parse dots.
		
		let mut s = prefix;
		
		while !self.reader.is_eof() && is_digit(self.reader.peek()) {
			s.push(self.reader.next());
		}
		
		if !self.reader.is_eof() {
			let c = self.reader.peek();
			if c == 'e' || c == 'E' {
				let mut index: usize = 1;
				
				if !self.reader.is_eof() {
					let c1 = self.reader.peek_at(index);
					if c1 == '+' || c1 == '-' {
						index += 1;
					}
				}
				
				if !self.reader.is_eof() {
					let c2 = self.reader.peek_at(index);
					if is_digit(c2) {
						while index > 0 {
							s.push(self.reader.next());
							index -= 1;
						}
						
						while !self.reader.is_eof() && is_digit(self.reader.peek()) {
							s.push(self.reader.next());
						}
					}
				}
			}
		}
		
		if let Some(token) = self.parse_str_number(&s, 10) {
			Ok(token)
		} else {
			match s.parse() {
				Ok(value) => Ok(Token::Literal(Lit::Number(value))),
				_ => self.fatal(&format!("cannot parse number {:?}", s))
			}
		}
	}
	
	fn parse_string(&mut self) -> JsResult<Token> {
		let mut s = String::new();
		
		while !self.reader.is_eof() {
			match self.reader.next() {
				'\u{0}'...'\u{1F}' => return self.fatal("invalid string character"),
				'"' => return Ok(Token::Literal(Lit::String(Rc::new(s)))),
				'\\' => {
					let parsed = try!(self.parse_escape());
					s.push_str(&parsed)
				}
				c @ _ => s.push(c)
			}
		}
		
		self.fatal("unterminated string constant")
	}
	
	fn parse_escape(&mut self) -> JsResult<String> {
		let result = match self.reader.next() {
			'\\' => "\\".to_string(),
			'/' => "/".to_string(),
			'"' => "\"".to_string(),
			'b' => "\u{8}".to_string(),
			'f' => "\u{C}".to_string(),
			'n' => "\n".to_string(),
			'r' => "\r".to_string(),
			't' => "\t".to_string(),
			'u' => try!(self.parse_escape_hex(4)).to_string(),
			_ => return self.fatal("cannot parse escape sequence")
		};
		
		Ok(result)
	}
	
	fn parse_escape_hex(&mut self, length: i32) -> JsResult<char> {
		let mut value : u32 = 0;
		
		for _ in 0..length {
			let c = self.reader.next();
			let digit;
			
			if c >= '0' && c <= '9' {
				digit = (c as u32) - ('0' as u32);
			} else if c >= 'a' && c <= 'f' {
				digit = (c as u32) - ('a' as u32) + 10u32;
			} else if c >= 'A' && c <= 'F' {
				digit = (c as u32) - ('A' as u32) + 10u32;
			} else {
				return self.fatal("Cannot parse hex escape sequence");
			}
			
			value <<= 4;
			value |= digit;
		}
		
		if let Some(c) = char::from_u32(value) {
			Ok(c)
		} else {
			// TODO: What to do when we get an invalid unicode code point?
			Ok('�')
		}
	}
	
	fn parse_identifier(&mut self) -> JsResult<Option<String>> {
		let mut s = String::new();
		
		while !self.reader.is_eof() && self.is_identifier_letter() {
			s.push(self.reader.next());
		}
		
		Ok(if s.is_empty() {
			None
		} else {
			Some(s)
		})
	}
	
	fn is_identifier_letter(&mut self) -> bool {
		// We only have to parse null, true and false.
		
		match self.reader.peek() {
			'a'...'z' => true,
			_ => false
		}
	}
}

fn is_digit(c: char) -> bool {
	match c {
		'0' ... '9' => true,
		_ => false
	}
}

fn is_line_terminator(c: char) -> bool {
	match c {
		'\r' | '\n' => true,
		_ => false
	}
}

fn is_whitespace(c: char) -> bool {
	match c {
		'\t' | ' ' => true,
		_ => false
	}
}
