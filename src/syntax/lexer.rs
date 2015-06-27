use syntax::{Name, Span};
use syntax::reader::Reader;
use syntax::token::{Token, TokenAndSpan, Lit};
use syntax::token::Token::*;
use ::{JsResult, JsError};
use util::interner::StrInterner;
use std::char;
use std::collections::VecDeque;
use ::util::matchers::{DecimalMatcher, Decimal};

#[derive(PartialEq)]
pub enum LexerMode {
	Normal,
	Runtime
}

pub struct Lexer<'a> {
	reader: &'a mut Reader,
	interner: &'a StrInterner,
	strict: bool,
	look_ahead: VecDeque<(TokenAndSpan, usize)>,
	file: Name,
	last_span: Option<Span>,
	allow_regexp: bool,
	mode: LexerMode
}

impl<'a> Lexer<'a> {
	pub fn new(reader: &'a mut Reader, interner: &'a StrInterner, mode: LexerMode) -> JsResult<Lexer<'a>> {
		let file = interner.intern(&reader.file().to_string());
		
		Ok(Lexer {
			reader: reader,
			interner: interner,
			strict: false,
			look_ahead: VecDeque::new(),
			file: file,
			last_span: None,
			allow_regexp: false,
			mode: mode
		})
	}
	
	pub fn strict(&self) -> bool {
		self.strict
	}
	
	pub fn set_strict(&mut self, strict: bool) {
		self.reset_look_ahead();
		
		self.strict = strict;
	}
	
	pub fn set_allow_regexp(&mut self, allow: bool) {
		assert!(allow != self.allow_regexp);
		
		self.reset_look_ahead();
		
		self.allow_regexp = allow;
	}
	
	fn reset_look_ahead(&mut self) {
		// This method is called when the parser state changes. Reset the reader
		// to just before the first look ahead token and clear the look ahead list.
		
		if let Some(first) = self.look_ahead.get(0) {
			self.reader.seek(first.1);
		}
		
		self.look_ahead.clear();
	}
	
	pub fn span(&mut self) -> JsResult<Span> {
		self.span_at(0)
	}
	
	pub fn span_at(&mut self, offset: usize) -> JsResult<Span> {
		if let Some(token) = try!(self.peek(offset)) {
			Ok(token.span)
		} else if self.last_span.is_none() {
			// This is a bit of a strange escape. What we're testing for
			// here is that the file is empty. If that is the case, we
			// still return a span to simplify the rest of the parser.
			
			Ok(Span::new(0, 0, 0, 0, self.file))
		} else {
			self.fatal("Unexpected end of input")
		}
	}
	
	pub fn last_span(&self) -> Option<Span> {
		self.last_span
	}
	
	pub fn is_eof(&mut self) -> JsResult<bool> {
		Ok(try!(self.peek(0)).is_none())
	}
	
	fn is_any_eof(&mut self) -> JsResult<bool> {
		Ok(try!(self.peek_any(0)).is_none())
	}
	
	pub fn bump(&mut self) -> JsResult<()> {
		loop {
			let hidden = if let Some(token) = try!(self.peek_any(0)) {
				token.token.is_hidden()
			} else {
				return self.fatal("Unexpected end of input");
			};
			
			try!(self.next_any());
			
			if !hidden {
				break;
			}
		}
		
		Ok(())
	}
	
	pub fn bump_any(&mut self) -> JsResult<()> {
		if try!(self.is_any_eof()) {
			self.fatal("Unexpected end of input")
		} else {
			try!(self.next_any());
			
			Ok(())
		}
	}
	
	pub fn next(&mut self) -> JsResult<TokenAndSpan> {
		loop {
			let token = try!(self.next_any());
			if !token.token.is_hidden() {
				return Ok(token);
			}
		}
	}
	
	pub fn next_any(&mut self) -> JsResult<TokenAndSpan> {
		if let Some(token) = try!(self.peek_any(0)) {
			self.look_ahead.pop_front();
			
			if !token.token.is_hidden() {
				self.last_span = Some(token.span)
			}
			
			Ok(token)
		} else {
			self.fatal("Unexpected end of input")
		}
	}
	
	pub fn peek(&mut self, index: usize) -> JsResult<Option<TokenAndSpan>> {
		let mut offset = 0;
		let mut index = index;
		
		loop {
			if let Some(token) = try!(self.peek_any(offset)) {
				if !token.token.is_hidden() {
					if index == 0 {
						return Ok(Some(token));
					}
					
					index -= 1;
				}
			} else {
				return Ok(None);
			}
			
			offset += 1;
		}
	}
	
	pub fn peek_any(&mut self, index: usize) -> JsResult<Option<TokenAndSpan>> {
		while self.look_ahead.len() < index + 1 {
			if self.reader.is_eof() {
				return Ok(None);
			}
			
			let index = self.look_ahead.len();
			let offset = self.reader.offset();
			
			let token = try!(self.parse());
			
			// Ensure that we did not recurse into ourselves.
			
			assert_eq!(index, self.look_ahead.len());
			
			self.look_ahead.push_back((token, offset));
		}
		
		Ok(Some(self.look_ahead[index].0))
	}

	fn fatal<T>(&self, message: &str) -> JsResult<T> {
		let (line, col) = self.reader.pos();
		
		Err(JsError::Lex(format!("{}:{}: {}", line, col, message.to_string())))
	}
	
	fn parse(&mut self) -> JsResult<TokenAndSpan> {
		let (start_line, start_col) = self.reader.pos();
		
		let token = match self.reader.next() {
			'*' => {
				if self.reader.consume('=') {
					MultiplyAssign
				} else {
					Multiply
				}
			},
			'/' => {
 				if self.reader.consume('=') {
					DivideAssign
				} else if self.reader.consume('*') {
					try!(self.parse_block_comment());
					Comment
				} else if self.reader.consume('/') {
					self.skip_while(|c| !is_line_terminator(c));
					Comment
				} else if self.allow_regexp {
					if let Some(token) = try!(self.parse_regex()) {
						token
					} else {
						Divide
					}
				} else {
					Divide
				}
			},
			'%' => {
				if self.reader.consume('=') {
					ModulusAssign
				} else {
					Modulus
				}
			},
			'+' => {
				if self.reader.consume('=') {
					PlusAssign
				} else if self.reader.consume('+') {
					PlusPlus
				} else {
					Plus
				}
			},
			'-' => {
				if self.reader.consume('=') {
					MinusAssign
				} else if self.reader.consume('-') {
					MinusMinus
				} else {
					Minus
				}
			},
			'>' => {
				if self.reader.consume('>') {
					if self.reader.consume('>') {
						if self.reader.consume('=') {
							RightShiftLogicalAssign
						} else {
							RightShiftLogical
						}
					} else if self.reader.consume('=') {
						RightShiftArithmeticAssign
					} else {
						RightShiftArithmetic
					}
				} else if self.reader.consume('=') {
					GreaterThanEquals
				} else {
					GreaterThan
				}
			},
			'<' => {
				if self.reader.consume('<') {
					if self.reader.consume('=') {
						LeftShiftArithmeticAssign
					} else {
						LeftShiftArithmetic
					}
				} else if self.reader.consume('=') {
					LessThanEquals
				} else {
					LessThan
				}
			},
			'&' => {
				if self.reader.consume('=') {
					BitAndAssign
				} else if self.reader.consume('&') {
					And
				} else {
					BitAnd
				}
			},
			'|' => {
				if self.reader.consume('=') {
					BitOrAssign
				} else if self.reader.consume('|') {
					Or
				} else {
					BitOr
				}
			},
			'^' => {
				if self.reader.consume('=') {
					BitXOrAssign
				} else {
					BitXOr
				}
			},
			'~' => BitNot,
			'=' => {
				if self.reader.consume('=') {
					if self.reader.consume('=') {
						IdentityEquals
					} else {
						Equals
					}
				} else {
					Assign
				}
			},
			'!' => {
				if self.reader.consume('=') {
					if self.reader.consume('=') {
						IdentityNotEquals
					} else {
						NotEquals
					}
				} else {
					Not
				}
			},
			'[' => OpenBracket,
			']' => CloseBracket,
			'(' => OpenParen,
			')' => CloseParen,
			'{' => OpenBrace,
			'}' => CloseBrace,
			';' => SemiColon,
			',' => Comma,
			'?' => QuestionMark,
			':' => Colon,
			'.' => {
				if is_digit(self.reader.peek()) {
					try!(self.parse_number('.'))
				} else {
					Dot
				}
			},
			c @ '0' ... '9' => try!(self.parse_number(c)),
			'"' => try!(self.parse_string('"')),
			'\'' => try!(self.parse_string('\'')),
			c @ _ if is_line_terminator(c) => Newline,
			c @ _ if is_whitespace(c) => {
				self.skip_while(is_whitespace);
				Whitespace
			},
			c @ _ => {
				self.reader.skip(-1);
				
				if let Some(identifier) = try!(self.parse_identifier(true)) {
					self.token_from_identifier(identifier)
				} else {
					return self.fatal(&format!("Cannot parse '{}'", c));
				}
			}
		};
		
		let (end_line, end_col) = self.reader.last_pos();
		let span = Span::new(start_line, start_col, end_line, end_col, self.file);
		
		Ok(TokenAndSpan::new(token, span))
	}
	
	fn token_from_identifier(&self, identifier: String) -> Token {
		match &identifier[..] {
			"null" => Literal(Lit::Null),
			"true" => Literal(Lit::Boolean(true)),
			"false" => Literal(Lit::Boolean(false)),
			// 7.6.1.1 Keywords
			"break" => Break,
			"do" => Do,
			"instanceof" => Instanceof,
			"typeof" => Typeof,
			"case" => Case,
			"else" => Else,
			"new" => New,
			"var" => Var,
			"catch" => Catch,
			"finally" => Finally,
			"return" => Return,
			"void" => Void,
			"continue" => Continue,
			"for" => For,
			"switch" => Switch,
			"while" => While,
			"debugger" => Debugger,
			"function" => Function,
			"this" => This,
			"with" => With,
			"default" => Default,
			"if" => If,
			"throw" => Throw,
			"delete" => Delete,
			"in" => In,
			"try" => Try,
			// 7.6.1.2 Future Reserved Words
			"class" => Class,
			"enum" => Enum,
			"extends" => Extends,
			"super" => Super,
			"const" => Const,
			"export" => Export,
			"import" => Import,
			// 7.6.1.2 Future Reserved Words in strict mode
			"implements" if self.strict => Implements,
			"let" if self.strict => Let,
			"private" if self.strict => Private,
			"public" if self.strict => Public,
			"interface" if self.strict => Interface,
			"package" if self.strict => Package,
			"protected" if self.strict => Protected,
			"static" if self.strict => Static,
			"yield" if self.strict => Yield,
			// 7.6 Identifier Names and Identifiers
			str @ _ => Identifier(self.interner.intern(str))
		}
	}
	
	fn parse_block_comment(&mut self) -> JsResult<()> {
		while !self.reader.is_eof() {
			if self.reader.next() == '*' && self.reader.consume('/') {
				return Ok(());
			}
		}
		
		Err(JsError::Parse("Unmatched block comment".to_string()))
	}
	
	fn skip_while<F: Fn(char) -> bool>(&mut self, predicate: F) {
		while !self.reader.is_eof() && predicate(self.reader.peek()) {
			self.reader.next();
		}
	}
	
	fn parse_number(&mut self, c: char) -> JsResult<Token> {
		let mut matcher = DecimalMatcher::new(self.mode == LexerMode::Normal);
		
		matcher.allowed(c);
		
		while !self.reader.is_eof() && matcher.allowed(self.reader.peek()) {
			self.reader.next();
		}
		
		match matcher.complete() {
			Decimal::Integer(value) | Decimal::Decimal(value)
				=> Ok(Literal(Lit::Number(self.interner.intern(&value), 10))),
			Decimal::Octal(value) => {
				if self.strict {
					self.fatal("octal numbers are not allowed in strict mode")
				} else {
					Ok(Literal(Lit::Number(self.interner.intern(&value), 8)))
				}
			}
			Decimal::Hex(value)
				=> Ok(Literal(Lit::Number(self.interner.intern(&value[2..]), 16))),
			Decimal::Error(value)
				=> self.fatal(&format!("cannot parse number {}", value))
		}
	}
	
	fn parse_string(&mut self, quote: char) -> JsResult<Token> {
		let mut s = String::new();
		let mut exact = true;
		
		while !self.reader.is_eof() {
			let c = self.reader.next();
			
			if is_line_terminator(c) {
				return self.fatal("Newline cannot appear in string constant");
			}
			
			if c == quote {
				break;
			} else if c == '\\' {
				// Do we have a line terminator (i.e. line continuation)? Otherwise parse an escape.
				
				exact = false;
				
				let c1 = self.reader.peek();
				if is_line_terminator(c1) {
					self.reader.next();
					
					if c1 == '\r' {
						self.reader.consume('\n');
					}
				} else {
					let parsed = try!(self.parse_escape());
					s.push_str(&parsed)
				}
			} else {
				s.push(c);
			}
		}
		
		Ok(Literal(Lit::String(self.interner.intern(&s), exact)))
	}
	
	fn parse_escape(&mut self) -> JsResult<String> {
		let result = match self.reader.next() {
			'\'' => "'".to_string(),
			'"' => "\"".to_string(),
			'b' => "\u{8}".to_string(),
			'f' => "\u{C}".to_string(),
			'n' => "\n".to_string(),
			'r' => "\r".to_string(),
			't' => "\t".to_string(),
			'v' => "\u{B}".to_string(),
			'x' => try!(self.parse_escape_hex(2)).to_string(),
			'u' => try!(self.parse_escape_hex(4)).to_string(),
			c @ '0'...'9' => try!(self.parse_escape_octal(c)),
			c @ _ => c.to_string()
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
	
	fn parse_escape_octal(&mut self, c1: char) -> JsResult<String> {
		if c1 <= '3' {
			let c2 = self.reader.peek();
			if c2 >= '0' && c2 <= '7' {
				self.reader.next();
				let c3 = self.reader.peek();
				if c3 >= '0' && c3 <= '7' {
					self.reader.next();
					self.parse_octal(&[c1, c2, c3])
				} else {
					self.parse_octal(&[c1, c2])
				}
			} else {
				self.parse_octal(&[c1])
			}
		} else if c1 <= '7' {
			let c2 = self.reader.peek();
			if c2 >= '0' && c2 <= '7' {
				self.reader.next();
				self.parse_octal(&[c1, c2])
			} else {
				self.parse_octal(&[c1])
			}
		} else {
			Ok(c1.to_string())
		}
	}
	
	fn parse_octal(&mut self, chars: &[char]) -> JsResult<String> {
		if self.strict {
			self.fatal("Octal escape sequence is illegal in strict mode")
		} else {
			let mut value = 0;
			
			for char in chars {
				value *= 8;
				value += char.to_digit(8).unwrap();
			}
			
			Ok(char::from_u32(value).unwrap_or('�').to_string())
		}
	}
	
	fn parse_identifier(&mut self, require_start: bool) -> JsResult<Option<String>> {
		let mut s = String::new();
		let mut had_one = !require_start;
		
		while !self.reader.is_eof() && self.is_identifier_letter(!had_one) {
			if self.reader.peek() == '\\' {
				self.reader.next();
				self.reader.next();
				
				// TODO: Invalid unicode sequences will be parsed incorrectly.
				
				let c = try!(self.parse_escape_hex(4));
				
				// TODO: There probably is a more generic case that applies here.
				
				let illegal = match c {
					'-' | '!' | '%' | '&' | '(' | ')' | '*' | ',' | '.' | '/' | ':' | ';' | '?' | '[' | ']' | '^' | '{' | '|' | '}' | '~' | '+' | '<' | '=' | '>' => true,
					c @ _ => is_line_terminator(c) || is_whitespace(c)
				};

				if illegal {
					return self.fatal("Invalid token");
				}
				
				s.push(c);
			} else {
				s.push(self.reader.next());
			}
			
			had_one = true;
		}
		
		Ok(if s.is_empty() {
			None
		} else {
			Some(s)
		})
	}
	
	fn is_identifier_letter(&mut self, start: bool) -> bool {
		match self.reader.peek() {
			'$' | '_' => true,
			'\\' => self.reader.peek_at(1) == 'u',
			c @ _ => {
				if is_unicode_letter(c) {
					true
				} else if !start && (
					is_unicode_combining(c) ||
					is_unicode_digit(c) ||
					is_unicode_connector_punctuation(c) ||
					is_unicode_zwnj(c) ||
					is_unicode_zwj(c)
				) {
					true
				} else {
					false
				}
			}
		}
	}
	
	fn parse_regex(&mut self) -> JsResult<Option<Token>> {
		let pos = self.reader.offset();
		
		let expression = match try!(self.parse_regex_expression()) {
			Some(expression) => expression,
			None => {
				// Regular expressions are parsed by attempt. If parsing fails, reset the
				// reader to the previous position and try something else.
				
				self.reader.seek(pos);
				
				return Ok(None);
			}
		};
		
		let flags = try!(self.parse_regex_flags());
		
		let expression = self.interner.intern(&expression);
		let flags = self.interner.intern(&flags);
		
		Ok(Some(Literal(Lit::Regex(expression, flags))))
	}
	
	fn parse_regex_expression(&mut self) -> JsResult<Option<String>> {
		let mut s = String::new();
		
		while !self.reader.is_eof() {
			match self.reader.next() {
				'/' => return Ok(Some(s)),
				'\\' => {
					if is_line_terminator(self.reader.peek()) {
						return Ok(None);
					}
					
					let parsed = try!(self.parse_escape());
					s.push_str(&parsed);
				},
				'[' => {
					s.push('[');
					
					while !self.reader.is_eof() {
						match self.reader.next() {
							']' => {
								s.push(']');
								break;
							},
							'\\' => {
								if is_line_terminator(self.reader.peek()) {
									return Ok(None);
								}
								
								let parsed = try!(self.parse_escape());
								s.push_str(&parsed);
							},
							c @ _ => {
								if is_line_terminator(c) {
									return Ok(None);
								}
								
								s.push(c);
							}
						}
					}
				},
				c @ _ => {
					if is_line_terminator(c) {
						return Ok(None);
					}
					
					s.push(c);
				}
			}
		}
		
		Ok(None)
	}
	
	fn parse_regex_flags(&mut self) -> JsResult<String> {
		Ok(if let Some(flags) = try!(self.parse_identifier(false)) {
			flags
		} else {
			"".to_string()
		})
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
		'\r' | '\n' | '\u{2028}' | '\u{2029}' => true,
		_ => false
	}
}

fn is_whitespace(c: char) -> bool {
	match c {
		'\t' | '\u{000B}' | '\u{000C}' => true,
		// Unicode Zs category.
		'\u{0020}' | '\u{00A0}' | '\u{1680}' | '\u{2000}' | '\u{2001}' | '\u{2002}' |
		'\u{2003}' | '\u{2004}' | '\u{2005}' | '\u{2006}' | '\u{2007}' | '\u{2008}' |
		'\u{2009}' | '\u{200A}' | '\u{202F}' | '\u{205F}' | '\u{3000}'
			=> true,
		// Others not in the spec.
		'\u{180E}' => true,
		_ => false
	}
}

fn is_unicode_letter(c: char) -> bool {
	match c {
		'\u{0041}'...'\u{005A}' => true,
		'\u{0061}'...'\u{007A}' => true,
		'\u{00AA}' => true,
		'\u{00B5}' => true,
		'\u{00BA}' => true,
		'\u{00C0}'...'\u{00D6}' => true,
		'\u{00D8}'...'\u{00F6}' => true,
		'\u{00F8}'...'\u{021F}' => true,
		'\u{0222}'...'\u{0233}' => true,
		'\u{0250}'...'\u{02AD}' => true,
		'\u{02B0}'...'\u{02B8}' => true,
		'\u{02BB}'...'\u{02C1}' => true,
		'\u{02D0}'...'\u{02D1}' => true,
		'\u{02E0}'...'\u{02E4}' => true,
		'\u{02EE}' => true,
		'\u{037A}' => true,
		'\u{0386}' => true,
		'\u{0388}'...'\u{038A}' => true,
		'\u{038C}' => true,
		'\u{038E}'...'\u{03A1}' => true,
		'\u{03A3}'...'\u{03CE}' => true,
		'\u{03D0}'...'\u{03D7}' => true,
		'\u{03DA}'...'\u{03F3}' => true,
		'\u{0400}'...'\u{0481}' => true,
		'\u{048C}'...'\u{04C4}' => true,
		'\u{04C7}'...'\u{04C8}' => true,
		'\u{04CB}'...'\u{04CC}' => true,
		'\u{04D0}'...'\u{04F5}' => true,
		'\u{04F8}'...'\u{04F9}' => true,
		'\u{0531}'...'\u{0556}' => true,
		'\u{0559}' => true,
		'\u{0561}'...'\u{0587}' => true,
		'\u{05D0}'...'\u{05EA}' => true,
		'\u{05F0}'...'\u{05F2}' => true,
		'\u{0621}'...'\u{063A}' => true,
		'\u{0640}'...'\u{064A}' => true,
		'\u{0671}'...'\u{06D3}' => true,
		'\u{06D5}' => true,
		'\u{06E5}'...'\u{06E6}' => true,
		'\u{06FA}'...'\u{06FC}' => true,
		'\u{0710}' => true,
		'\u{0712}'...'\u{072C}' => true,
		'\u{0780}'...'\u{07A5}' => true,
		'\u{0905}'...'\u{0939}' => true,
		'\u{093D}' => true,
		'\u{0950}' => true,
		'\u{0958}'...'\u{0961}' => true,
		'\u{0985}'...'\u{098C}' => true,
		'\u{098F}'...'\u{0990}' => true,
		'\u{0993}'...'\u{09A8}' => true,
		'\u{09AA}'...'\u{09B0}' => true,
		'\u{09B2}' => true,
		'\u{09B6}'...'\u{09B9}' => true,
		'\u{09DC}'...'\u{09DD}' => true,
		'\u{09DF}'...'\u{09E1}' => true,
		'\u{09F0}'...'\u{09F1}' => true,
		'\u{0A05}'...'\u{0A0A}' => true,
		'\u{0A0F}'...'\u{0A10}' => true,
		'\u{0A13}'...'\u{0A28}' => true,
		'\u{0A2A}'...'\u{0A30}' => true,
		'\u{0A32}'...'\u{0A33}' => true,
		'\u{0A35}'...'\u{0A36}' => true,
		'\u{0A38}'...'\u{0A39}' => true,
		'\u{0A59}'...'\u{0A5C}' => true,
		'\u{0A5E}' => true,
		'\u{0A72}'...'\u{0A74}' => true,
		'\u{0A85}'...'\u{0A8B}' => true,
		'\u{0A8D}' => true,
		'\u{0A8F}'...'\u{0A91}' => true,
		'\u{0A93}'...'\u{0AA8}' => true,
		'\u{0AAA}'...'\u{0AB0}' => true,
		'\u{0AB2}'...'\u{0AB3}' => true,
		'\u{0AB5}'...'\u{0AB9}' => true,
		'\u{0ABD}' => true,
		'\u{0AD0}' => true,
		'\u{0AE0}' => true,
		'\u{0B05}'...'\u{0B0C}' => true,
		'\u{0B0F}'...'\u{0B10}' => true,
		'\u{0B13}'...'\u{0B28}' => true,
		'\u{0B2A}'...'\u{0B30}' => true,
		'\u{0B32}'...'\u{0B33}' => true,
		'\u{0B36}'...'\u{0B39}' => true,
		'\u{0B3D}' => true,
		'\u{0B5C}'...'\u{0B5D}' => true,
		'\u{0B5F}'...'\u{0B61}' => true,
		'\u{0B85}'...'\u{0B8A}' => true,
		'\u{0B8E}'...'\u{0B90}' => true,
		'\u{0B92}'...'\u{0B95}' => true,
		'\u{0B99}'...'\u{0B9A}' => true,
		'\u{0B9C}' => true,
		'\u{0B9E}'...'\u{0B9F}' => true,
		'\u{0BA3}'...'\u{0BA4}' => true,
		'\u{0BA8}'...'\u{0BAA}' => true,
		'\u{0BAE}'...'\u{0BB5}' => true,
		'\u{0BB7}'...'\u{0BB9}' => true,
		'\u{0C05}'...'\u{0C0C}' => true,
		'\u{0C0E}'...'\u{0C10}' => true,
		'\u{0C12}'...'\u{0C28}' => true,
		'\u{0C2A}'...'\u{0C33}' => true,
		'\u{0C35}'...'\u{0C39}' => true,
		'\u{0C60}'...'\u{0C61}' => true,
		'\u{0C85}'...'\u{0C8C}' => true,
		'\u{0C8E}'...'\u{0C90}' => true,
		'\u{0C92}'...'\u{0CA8}' => true,
		'\u{0CAA}'...'\u{0CB3}' => true,
		'\u{0CB5}'...'\u{0CB9}' => true,
		'\u{0CDE}' => true,
		'\u{0CE0}'...'\u{0CE1}' => true,
		'\u{0D05}'...'\u{0D0C}' => true,
		'\u{0D0E}'...'\u{0D10}' => true,
		'\u{0D12}'...'\u{0D28}' => true,
		'\u{0D2A}'...'\u{0D39}' => true,
		'\u{0D60}'...'\u{0D61}' => true,
		'\u{0D85}'...'\u{0D96}' => true,
		'\u{0D9A}'...'\u{0DB1}' => true,
		'\u{0DB3}'...'\u{0DBB}' => true,
		'\u{0DBD}' => true,
		'\u{0DC0}'...'\u{0DC6}' => true,
		'\u{0E01}'...'\u{0E30}' => true,
		'\u{0E32}'...'\u{0E33}' => true,
		'\u{0E40}'...'\u{0E46}' => true,
		'\u{0E81}'...'\u{0E82}' => true,
		'\u{0E84}' => true,
		'\u{0E87}'...'\u{0E88}' => true,
		'\u{0E8A}' => true,
		'\u{0E8D}' => true,
		'\u{0E94}'...'\u{0E97}' => true,
		'\u{0E99}'...'\u{0E9F}' => true,
		'\u{0EA1}'...'\u{0EA3}' => true,
		'\u{0EA5}' => true,
		'\u{0EA7}' => true,
		'\u{0EAA}'...'\u{0EAB}' => true,
		'\u{0EAD}'...'\u{0EB0}' => true,
		'\u{0EB2}'...'\u{0EB3}' => true,
		'\u{0EBD}'...'\u{0EC4}' => true,
		'\u{0EC6}' => true,
		'\u{0EDC}'...'\u{0EDD}' => true,
		'\u{0F00}' => true,
		'\u{0F40}'...'\u{0F6A}' => true,
		'\u{0F88}'...'\u{0F8B}' => true,
		'\u{1000}'...'\u{1021}' => true,
		'\u{1023}'...'\u{1027}' => true,
		'\u{1029}'...'\u{102A}' => true,
		'\u{1050}'...'\u{1055}' => true,
		'\u{10A0}'...'\u{10C5}' => true,
		'\u{10D0}'...'\u{10F6}' => true,
		'\u{1100}'...'\u{1159}' => true,
		'\u{115F}'...'\u{11A2}' => true,
		'\u{11A8}'...'\u{11F9}' => true,
		'\u{1200}'...'\u{1206}' => true,
		'\u{1208}'...'\u{1246}' => true,
		'\u{1248}' => true,
		'\u{124A}'...'\u{124D}' => true,
		'\u{1250}'...'\u{1256}' => true,
		'\u{1258}' => true,
		'\u{125A}'...'\u{125D}' => true,
		'\u{1260}'...'\u{1286}' => true,
		'\u{1288}' => true,
		'\u{128A}'...'\u{128D}' => true,
		'\u{1290}'...'\u{12AE}' => true,
		'\u{12B0}' => true,
		'\u{12B2}'...'\u{12B5}' => true,
		'\u{12B8}'...'\u{12BE}' => true,
		'\u{12C0}' => true,
		'\u{12C2}'...'\u{12C5}' => true,
		'\u{12C8}'...'\u{12CE}' => true,
		'\u{12D0}'...'\u{12D6}' => true,
		'\u{12D8}'...'\u{12EE}' => true,
		'\u{12F0}'...'\u{130E}' => true,
		'\u{1310}' => true,
		'\u{1312}'...'\u{1315}' => true,
		'\u{1318}'...'\u{131E}' => true,
		'\u{1320}'...'\u{1346}' => true,
		'\u{1348}'...'\u{135A}' => true,
		'\u{13A0}'...'\u{13B0}' => true,
		'\u{13B1}'...'\u{13F4}' => true,
		'\u{1401}'...'\u{1676}' => true,
		'\u{1681}'...'\u{169A}' => true,
		'\u{16A0}'...'\u{16EA}' => true,
		'\u{1780}'...'\u{17B3}' => true,
		'\u{1820}'...'\u{1877}' => true,
		'\u{1880}'...'\u{18A8}' => true,
		'\u{1E00}'...'\u{1E9B}' => true,
		'\u{1EA0}'...'\u{1EE0}' => true,
		'\u{1EE1}'...'\u{1EF9}' => true,
		'\u{1F00}'...'\u{1F15}' => true,
		'\u{1F18}'...'\u{1F1D}' => true,
		'\u{1F20}'...'\u{1F39}' => true,
		'\u{1F3A}'...'\u{1F45}' => true,
		'\u{1F48}'...'\u{1F4D}' => true,
		'\u{1F50}'...'\u{1F57}' => true,
		'\u{1F59}' => true,
		'\u{1F5B}' => true,
		'\u{1F5D}' => true,
		'\u{1F5F}'...'\u{1F7D}' => true,
		'\u{1F80}'...'\u{1FB4}' => true,
		'\u{1FB6}'...'\u{1FBC}' => true,
		'\u{1FBE}' => true,
		'\u{1FC2}'...'\u{1FC4}' => true,
		'\u{1FC6}'...'\u{1FCC}' => true,
		'\u{1FD0}'...'\u{1FD3}' => true,
		'\u{1FD6}'...'\u{1FDB}' => true,
		'\u{1FE0}'...'\u{1FEC}' => true,
		'\u{1FF2}'...'\u{1FF4}' => true,
		'\u{1FF6}'...'\u{1FFC}' => true,
		'\u{207F}' => true,
		'\u{2102}' => true,
		'\u{2107}' => true,
		'\u{210A}'...'\u{2113}' => true,
		'\u{2115}' => true,
		'\u{2119}'...'\u{211D}' => true,
		'\u{2124}' => true,
		'\u{2126}' => true,
		'\u{2128}' => true,
		'\u{212A}'...'\u{212D}' => true,
		'\u{212F}'...'\u{2131}' => true,
		'\u{2133}'...'\u{2139}' => true,
		'\u{2160}'...'\u{2183}' => true,
		'\u{3005}'...'\u{3007}' => true,
		'\u{3021}'...'\u{3029}' => true,
		'\u{3031}'...'\u{3035}' => true,
		'\u{3038}'...'\u{303A}' => true,
		'\u{3041}'...'\u{3094}' => true,
		'\u{309D}'...'\u{309E}' => true,
		'\u{30A1}'...'\u{30FA}' => true,
		'\u{30FC}'...'\u{30FE}' => true,
		'\u{3105}'...'\u{312C}' => true,
		'\u{3131}'...'\u{318E}' => true,
		'\u{31A0}'...'\u{31B7}' => true,
		'\u{3400}' => true,
		'\u{4DB5}' => true,
		'\u{4E00}' => true,
		'\u{9FA5}' => true,
		'\u{A000}'...'\u{A48C}' => true,
		'\u{AC00}' => true,
		'\u{D7A3}' => true,
		'\u{F900}'...'\u{FA2D}' => true,
		'\u{FB00}'...'\u{FB06}' => true,
		'\u{FB13}'...'\u{FB17}' => true,
		'\u{FB1D}' => true,
		'\u{FB1F}'...'\u{FB28}' => true,
		'\u{FB2A}'...'\u{FB36}' => true,
		'\u{FB38}'...'\u{FB3C}' => true,
		'\u{FB3E}' => true,
		'\u{FB40}'...'\u{FB41}' => true,
		'\u{FB43}'...'\u{FB44}' => true,
		'\u{FB46}'...'\u{FBB1}' => true,
		'\u{FBD3}'...'\u{FD3D}' => true,
		'\u{FD50}'...'\u{FD8F}' => true,
		'\u{FD92}'...'\u{FDC7}' => true,
		'\u{FDF0}'...'\u{FDFB}' => true,
		'\u{FE70}'...'\u{FE72}' => true,
		'\u{FE74}' => true,
		'\u{FE76}'...'\u{FEFC}' => true,
		'\u{FF21}'...'\u{FF3A}' => true,
		'\u{FF41}'...'\u{FF5A}' => true,
		'\u{FF66}'...'\u{FFBE}' => true,
		'\u{FFC2}'...'\u{FFC7}' => true,
		'\u{FFCA}'...'\u{FFCF}' => true,
		'\u{FFD2}'...'\u{FFD7}' => true,
		'\u{FFDA}'...'\u{FFDC}' => true,
		_ => false
	}
}

fn is_unicode_combining(c: char) -> bool {
	match c {
		'\u{0300}'...'\u{034E}' => true,
		'\u{0360}'...'\u{0362}' => true,
		'\u{0483}'...'\u{0486}' => true,
		'\u{0591}'...'\u{05A1}' => true,
		'\u{05A3}'...'\u{05B9}' => true,
		'\u{05BB}'...'\u{05BD}' => true,
		'\u{05BF}' => true, 
		'\u{05C1}'...'\u{05C2}' => true,
		'\u{05C4}' => true,
		'\u{064B}'...'\u{0655}' => true,
		'\u{0670}' => true,
		'\u{06D6}'...'\u{06DC}' => true,
		'\u{06DF}'...'\u{06E4}' => true,
		'\u{06E7}'...'\u{06E8}' => true,
		'\u{06EA}'...'\u{06ED}' => true,
		'\u{0711}' => true,
		'\u{0730}'...'\u{074A}' => true,
		'\u{07A6}'...'\u{07B0}' => true,
		'\u{0901}'...'\u{0903}' => true,
		'\u{093C}' => true,
		'\u{093E}'...'\u{094D}' => true,
		'\u{0951}'...'\u{0954}' => true,
		'\u{0962}'...'\u{0963}' => true,
		'\u{0981}'...'\u{0983}' => true,
		'\u{09BC}'...'\u{09C4}' => true,
		'\u{09C7}'...'\u{09C8}' => true,
		'\u{09CB}'...'\u{09CD}' => true,
		'\u{09D7}' => true,
		'\u{09E2}'...'\u{09E3}' => true,
		'\u{0A02}' => true,
		'\u{0A3C}' => true,
		'\u{0A3E}'...'\u{0A42}' => true,
		'\u{0A47}'...'\u{0A48}' => true,
		'\u{0A4B}'...'\u{0A4D}' => true,
		'\u{0A70}'...'\u{0A71}' => true,
		'\u{0A81}'...'\u{0A83}' => true,
		'\u{0ABC}' => true,
		'\u{0ABE}'...'\u{0AC5}' => true,
		'\u{0AC7}'...'\u{0AC9}' => true,
		'\u{0ACB}'...'\u{0ACD}' => true,
		'\u{0B01}'...'\u{0B03}' => true,
		'\u{0B3C}' => true,
		'\u{0B3E}'...'\u{0B43}' => true,
		'\u{0B47}'...'\u{0B48}' => true,
		'\u{0B4B}'...'\u{0B4D}' => true,
		'\u{0B56}'...'\u{0B57}' => true,
		'\u{0B82}'...'\u{0B83}' => true,
		'\u{0BBE}'...'\u{0BC2}' => true,
		'\u{0BC6}'...'\u{0BC8}' => true,
		'\u{0BCA}'...'\u{0BCD}' => true,
		'\u{0BD7}' => true,
		'\u{0C01}'...'\u{0C03}' => true,
		'\u{0C3E}'...'\u{0C44}' => true,
		'\u{0C46}'...'\u{0C48}' => true,
		'\u{0C4A}'...'\u{0C4D}' => true,
		'\u{0C55}'...'\u{0C56}' => true,
		'\u{0C82}'...'\u{0C83}' => true,
		'\u{0CBE}'...'\u{0CC4}' => true,
		'\u{0CC6}'...'\u{0CC8}' => true,
		'\u{0CCA}'...'\u{0CCD}' => true,
		'\u{0CD5}'...'\u{0CD6}' => true,
		'\u{0D02}'...'\u{0D03}' => true,
		'\u{0D3E}'...'\u{0D43}' => true,
		'\u{0D46}'...'\u{0D48}' => true,
		'\u{0D4A}'...'\u{0D4D}' => true,
		'\u{0D57}' => true,
		'\u{0D82}'...'\u{0D83}' => true,
		'\u{0DCA}' => true,
		'\u{0DCF}'...'\u{0DD4}' => true,
		'\u{0DD6}' => true,
		'\u{0DD8}'...'\u{0DDF}' => true,
		'\u{0DF2}'...'\u{0DF3}' => true,
		'\u{0E31}' => true,
		'\u{0E34}'...'\u{0E3A}' => true,
		'\u{0E47}'...'\u{0E4E}' => true,
		'\u{0EB1}' => true,
		'\u{0EB4}'...'\u{0EB9}' => true,
		'\u{0EBB}'...'\u{0EBC}' => true,
		'\u{0EC8}'...'\u{0ECD}' => true,
		'\u{0F18}'...'\u{0F19}' => true,
		'\u{0F35}' => true,
		'\u{0F37}' => true,
		'\u{0F39}' => true,
		'\u{0F3E}'...'\u{0F3F}' => true,
		'\u{0F71}'...'\u{0F84}' => true,
		'\u{0F86}'...'\u{0F87}' => true,
		'\u{0F90}'...'\u{0F97}' => true,
		'\u{0F99}'...'\u{0FBC}' => true,
		'\u{0FC6}' => true,
		'\u{102C}'...'\u{1032}' => true,
		'\u{1036}'...'\u{1039}' => true,
		'\u{1056}'...'\u{1059}' => true,
		'\u{17B4}'...'\u{17D3}' => true,
		'\u{18A9}' => true,
		'\u{20D0}'...'\u{20DC}' => true,
		'\u{20E1}' => true,
		'\u{302A}'...'\u{302F}' => true,
		'\u{3099}'...'\u{309A}' => true,
		'\u{FB1E}' => true,
		'\u{FE20}'...'\u{FE23}' => true,
		_ => false
	}
}

fn is_unicode_digit(c: char) -> bool {
	match c {
		'\u{0030}'...'\u{0039}' => true,
		'\u{0660}'...'\u{0669}' => true,
		'\u{06F0}'...'\u{06F9}' => true,
		'\u{0966}'...'\u{096F}' => true,
		'\u{09E6}'...'\u{09EF}' => true,
		'\u{0A66}'...'\u{0A6F}' => true,
		'\u{0AE6}'...'\u{0AEF}' => true,
		'\u{0B66}'...'\u{0B6F}' => true,
		'\u{0BE7}'...'\u{0BEF}' => true,
		'\u{0C66}'...'\u{0C6F}' => true,
		'\u{0CE6}'...'\u{0CEF}' => true,
		'\u{0D66}'...'\u{0D6F}' => true,
		'\u{0E50}'...'\u{0E59}' => true,
		'\u{0ED0}'...'\u{0ED9}' => true,
		'\u{0F20}'...'\u{0F29}' => true,
		'\u{1040}'...'\u{1049}' => true,
		'\u{1369}'...'\u{1371}' => true,
		'\u{17E0}'...'\u{17E9}' => true,
		'\u{1810}'...'\u{1819}' => true,
		'\u{FF10}'...'\u{FF19}' => true,
		_ => false
	}
}

fn is_unicode_connector_punctuation(c: char) -> bool {
	match c {
		'\u{005F}' => true,
		'\u{203F}'...'\u{2040}' => true,
		'\u{30FB}' => true,
		'\u{FE33}'...'\u{FE34}' => true,
		'\u{FE4D}'...'\u{FE4F}' => true,
		'\u{FF3F}' => true,
		'\u{FF65}' => true,
		_ => false
	}
}

fn is_unicode_zwnj(c: char) -> bool {
	c == '\u{200C}'
}

fn is_unicode_zwj(c: char) -> bool {
	c == '\u{200D}'
}

/*
#[cfg(test)]
mod test {
	use super::*;
	use syntax::reader::StringReader;
	use syntax::token::Token::*;
	use syntax::token::name;
	use util::interner::StrInterner;
	
	macro_rules! assert_match {
		($l:expr, $t:pat) => {
			match *$l.next().token() {
				$t => {},
				_ => assert!(false)
			}
		};
		($l:expr, $t:pat, $e:expr) => {
			match *$l.next().token() {
				$t => assert!($e),
				_ => assert!(false)
			}
		};	
	}
		
	#[test] fn null_test() {
		let mut lexer = parse("null");
		
		assert_match!(lexer, Literal(..));
		assert!(lexer.is_eof());
	}
	
	#[test] fn function_test() {
		let mut interner = name::new_interner();
		let mut lexer = parse_with_interner("function f() { var a = 1; }", &mut interner);
		
		assert_match!(lexer, Function);
		assert_match!(lexer, Identifier(i), i == interner.intern("f"));
		assert_match!(lexer, OpenParen);
		assert_match!(lexer, CloseParen);
		assert_match!(lexer, OpenBrace);
		assert_match!(lexer, Var);
		assert_match!(lexer, Identifier(i), i == interner.intern("a"));
		assert_match!(lexer, Assign);
		assert_match!(lexer, Literal(..));
		assert_match!(lexer, SemiColon);
		assert_match!(lexer, CloseBrace);
		assert!(lexer.is_eof());
	}
	
	fn parse(text: &str) -> Lexer {
		let mut interner = name::new_interner();
		
		parse_with_interner(text, &mut interner)
	}
	
	fn parse_with_interner(text: &str, interner: &mut StrInterner) -> Lexer {
		let mut reader = StringReader::new("file.js", text);
		
		Lexer::new(&mut reader, interner, false).ok().unwrap()
	}
}
*/
