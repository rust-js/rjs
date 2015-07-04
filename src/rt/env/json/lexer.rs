extern crate strtod;

use syntax::reader::Reader;
use ::{JsResult, JsError};
use std::char;
use std::rc::Rc;
use self::strtod::strtod;
use ::util::matchers::{DecimalMatcher, Decimal};

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
            c @ '0' ... '9' | c @ '-' | c @ '.' => try!(self.parse_decimal(c)),
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
    
    fn parse_decimal(&mut self, c: char) -> JsResult<Token> {
        let mut matcher = DecimalMatcher::new(true, true);
        
        matcher.allowed(c);
        
        while !self.reader.is_eof() && matcher.allowed(self.reader.peek()) {
            self.reader.next();
        }
        
        let result = match matcher.complete() {
            Decimal::Integer(value) => i64::from_str_radix(&value, 10).unwrap() as f64,
            Decimal::Decimal(value) => strtod(&value).unwrap(),
            Decimal::Hex(value) | Decimal::Octal(value) | Decimal::Error(value) | Decimal::IncompleteDecimal(value)
                => return self.fatal(&format!("cannot parse number {:?}", value))
        };
        
        Ok(Token::Literal(Lit::Number(result)))
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
            // TODO #60: What to do when we get an invalid unicode code point?
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
