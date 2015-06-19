use syntax::Name;
use ::{JsResult, JsError};
use rt::{JsEnv, JsValue, JsDescriptor, JsString, JsItem};
use rt::env::json::lexer::{Lexer, Token, Lit};
use gc::Local;

pub struct Parser<'a> {
	lexer: &'a mut Lexer<'a>,
	env: &'a mut JsEnv
}

impl<'a> Parser<'a> {
	fn is_eof(&mut self) -> JsResult<bool> {
		self.lexer.is_eof()
	}
	
	fn peek(&mut self) -> JsResult<Option<Token>> {
		self.lexer.peek()
	}
	
	fn next(&mut self) -> JsResult<Token> {
		self.lexer.next()
	}
	
	fn bump(&mut self) -> JsResult<()> {
		try!(self.lexer.next());
		
		Ok(())
	}
	
	fn expect(&mut self, token: Token) -> JsResult<()> {
		let message = {
			let next = try!(self.next());
			
			if next == token {
				return Ok(());
			}
			
			format!("unexpected token; expected {:?}, got {:?}", token, next)
		};
		
		self.fatal(&message)
	}
	
	fn consume(&mut self, token: Token) -> JsResult<bool> {
		let matched = match try!(self.peek()) {
			None => false,
			Some(t) => t == token
		};
		
		if matched {
			try!(self.bump());
		}
		
		Ok(matched)
	}
	
	fn fatal<T>(&mut self, message: &str) -> JsResult<T> {
		Err(JsError::Parse(message.to_string()))
	}
	
	pub fn parse_json(env: &'a mut JsEnv, lexer: &'a mut Lexer<'a>) -> JsResult<Local<JsValue>> {
		let mut parser = Parser {
			lexer: lexer,
			env: env
		};
		
		let result = try!(parser.parse_value());
		
		if !try!(parser.is_eof()) {
			parser.fatal("expected end of JSON stream")
		} else {
			Ok(result)
		}
	}
	
	fn parse_value(&mut self) -> JsResult<Local<JsValue>> {
		match try!(self.next()) {
			Token::Literal(lit) => {
				match lit {
					Lit::Null => Ok(self.env.new_null()),
					Lit::Boolean(value) => Ok(self.env.new_bool(value)),
					Lit::String(value) => Ok(JsString::from_str(self.env, &value).as_value(self.env)),
					Lit::Number(value) => Ok(self.env.new_number(value))
				}
			}
			Token::OpenBrace => self.parse_object(),
			Token::OpenBracket => self.parse_array(),
			_ => self.fatal("invalid JSON token")
		}
	}
	
	fn parse_object(&mut self) -> JsResult<Local<JsValue>> {
		let mut object = self.env.create_object();
		
		if !try!(self.consume(Token::CloseBrace)) {
			loop {
				if let Token::Literal(Lit::String(key)) = try!(self.next()) {
					try!(self.consume(Token::Colon));
					
					let value = try!(self.parse_value());
					
					let key = self.env.intern(&key);
					try!(object.define_own_property(self.env, key, JsDescriptor::new_simple_value(value), false));
				} else {
					return self.fatal("cannot parse JSON object");
				}
				
				if try!(self.consume(Token::CloseBrace)) {
					break;
				}

				try!(self.expect(Token::Comma));
			}
		}
		
		Ok(object.as_value(self.env))
	}
	
	fn parse_array(&mut self) -> JsResult<Local<JsValue>> {
		let mut array = self.env.create_array();
		let mut offset = 0;
		
		if !try!(self.consume(Token::CloseBracket)) {
			loop {
				let value = try!(self.parse_value());
				
				try!(array.define_own_property(self.env, Name::from_index(offset), JsDescriptor::new_simple_value(value), false));
				offset += 1;
				
				if try!(self.consume(Token::CloseBracket)) {
					break;
				}

				try!(self.expect(Token::Comma));
			}
		}
	
		Ok(array.as_value(self.env))
	}
}
