use syntax::lexer::Lexer;
use syntax::ast::*;
use syntax::token::{Token, Lit};
use syntax::Span;
use syntax::token::keywords;
use std::rc::Rc;
use util::iter::*;
use util::interner::StrInterner;

pub enum ParseError {
	Message(String)
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
	lexer: &'a mut Lexer,
	interner: &'a mut StrInterner
}

impl<'a> Parser<'a> {
	pub fn new(lexer: &'a mut Lexer, interner: &'a mut StrInterner) -> Parser<'a> {
		Parser {
			lexer: lexer,
			interner: interner
		}
	}
	
	fn is_eof(&self) -> bool {
		self.lexer.is_eof()
	}
	
	fn peek(&mut self) -> Option<&Token> {
		self.peek_at(0)
	}
	
	fn peek_at(&self, index: usize) -> Option<&Token> {
		match self.lexer.peek(index) {
			None => None,
			Some(token) => Some(token.token())
		}
	}
	
	fn peek_any(&mut self) -> Option<&Token> {
		self.peek_any_at(0)
	}
	
	fn peek_any_at(&self, index: usize) -> Option<&Token> {
		match self.lexer.peek_any(index) {
			None => None,
			Some(token) => Some(token.token())
		}
	}
	
	fn next(&mut self) -> &Token {
		self.lexer.next().token()
	}
	
	fn bump(&mut self) {
		self.lexer.bump();
	}
	
	fn bump_any(&mut self) {
		self.lexer.bump_any();
	}
	
	fn expect(&mut self, token: &Token) -> ParseResult<()> {
		let message = {
			let next = self.next();
			
			if next == token {
				return Ok(());
			}
			
			format!("Unexpected token; expected {:?}, got {:?}", token, next)
		};
		
		self.fatal(&message)
	}
	
	fn consume(&mut self, token: &Token) -> bool {
		let matched = match self.peek() {
			None => false,
			Some(t) => t == token
		};
		
		if matched {
			self.bump();
		}
		
		matched
	}
	
	fn fatal<T>(&self, message: &str) -> ParseResult<T> {
		let span = match self.lexer.peek(0) {
			Some(token) => token.span().clone(),
			_ => Span::new(-1, -1, -1, -1)
		};
		
		let message = format!("{}:{}: {}", span.start_line, span.start_col, message.to_string());
		
		// Panic here under debug to get a stack trace.
		
		if cfg!(not(ndebug)) {
			panic!(message);
		}
		
		Err(ParseError::Message(message))
	}
	
	fn expect_eos(&mut self) -> ParseResult<()> {
		// Find a valid end of statement. A valid end of statement is:
		//
		//   * A semi colon or newline, which is consumed;
		//   * A close brace, which is not consumed; or
		//   * The end of the file.
		//
		
		loop {
			let matched = match self.peek_any() {
				None => return Ok(()),
				Some(&Token::SemiColon) | Some(&Token::Newline) => Some(true),
				Some(&Token::CloseBrace) => return Ok(()),
				Some(ref t) if t.is_hidden() => Some(false),
				_ => None
			};
			
			if !matched.is_some() {
				return self.fatal("Expected EOS");
			}
				
			self.bump_any();
			
			if matched.unwrap() {
				return Ok(());
			}
		}
	}
	
	fn is_eos(&self) -> bool {
		// Find a valid end of statement. A valid end of statement is:
		//
		//   * A semi colon or newline, which is consumed;
		//   * A close brace, which is not consumed; or
		//   * The end of the file.
		//
		
		let mut index: usize = 0;
		
		loop {
			match self.peek_any_at(index) {
				None | Some(&Token::SemiColon) | Some(&Token::Newline) | Some(&Token::CloseBrace) => return true,
				Some(ref t) if t.is_hidden() => {},
				_ => return false
			}
				
			index += 1;
		}
	}
	
	pub fn parse_program(&mut self) -> ParseResult<Program> {
		let mut items = Vec::new();
		
		while !self.is_eof() {
			items.push(try!(self.parse_item()));
		}
		
		Ok(Program {
			items: Block { stmts: items }
		})
	}
	
	fn parse_item(&mut self) -> ParseResult<Item> {
		if let Some(&Token::Function) = self.peek() {
			if let Some(&Token::Identifier(..)) = self.peek_at(1) {
				self.bump();
				
				let (ident, args, block) = try!(self.parse_function());
				
				return Ok(Item::Function(ident, args, block));
			}
		}
		
		self.parse_stmt()
	}
	
	fn parse_function(&mut self) -> ParseResult<(Option<Ident>, Vec<Ident>, Block)> {
		let ident = if let Some(&Token::Identifier(..)) = self.peek() {
			Some(try!(self.parse_ident()))
		} else {
			None
		};
		
		let args = try!(self.parse_parameter_list());
		let block = try!(self.parse_function_block());
		
		Ok((ident, args, block))
	}
	
	fn parse_function_block(&mut self) -> ParseResult<Block> {
		try!(self.expect(&Token::OpenBrace));
		
		let mut stmts = Vec::new();
		
		while !self.consume(&Token::CloseBrace) {
			stmts.push(try!(self.parse_item()));
		}
		
		Ok(Block {
			stmts: stmts
		})
	}
	
	fn parse_parameter_list(&mut self) -> ParseResult<Vec<Ident>> {
		try!(self.expect(&Token::OpenParen));
		
		if self.consume(&Token::CloseParen) {
			return Ok(Vec::new());
		}
		
		let mut args = Vec::new();
		
		while !self.is_eof() {
			args.push(try!(self.parse_ident()));
			
			match *self.next() {
				Token::Comma => continue,
				Token::CloseParen => return Ok(args),
				_ => break
			}
		}
		
		self.fatal("Cannot parse parameter list")
	}
	
	fn parse_ident(&mut self) -> ParseResult<Ident> {
		if let Token::Identifier(name) = *self.next() {
			return Ok(name.ident());
		}
		
		self.fatal("Expected identifier")
	}
	
	fn parse_ident_name(&mut self) -> ParseResult<Option<Name>> {
		// This is a special version of parse_ident that also accepts keywords.
		
		let name = match self.peek() {
			Some(&Token::Identifier(name)) => name,
			Some(&Token::Literal(ref value)) => {
				match *value.clone() {
					Lit::Boolean(value) => if value { keywords::TRUE } else { keywords::FALSE },
					Lit::Null => keywords::NULL,
					_ => return Ok(None)
				}
			},
			// Reserved words
			Some(&Token::Break) => keywords::BREAK,
			Some(&Token::Do) => keywords::DO,
			Some(&Token::Instanceof) => keywords::INSTANCEOF,
			Some(&Token::Typeof) => keywords::TYPEOF,
			Some(&Token::Case) => keywords::CASE,
			Some(&Token::Else) => keywords::ELSE,
			Some(&Token::New) => keywords::NEW,
			Some(&Token::Var) => keywords::VAR,
			Some(&Token::Catch) => keywords::CATCH,
			Some(&Token::Finally) => keywords::FINALLY,
			Some(&Token::Return) => keywords::RETURN,
			Some(&Token::Void) => keywords::VOID,
			Some(&Token::Continue) => keywords::CONTINUE,
			Some(&Token::For) => keywords::FOR,
			Some(&Token::Switch) => keywords::SWITCH,
			Some(&Token::While) => keywords::WHILE,
			Some(&Token::Debugger) => keywords::DEBUGGER,
			Some(&Token::Function) => keywords::FUNCTION,
			Some(&Token::This) => keywords::THIS,
			Some(&Token::With) => keywords::WHILE,
			Some(&Token::Default) => keywords::DEFAULT,
			Some(&Token::If) => keywords::IF,
			Some(&Token::Throw) => keywords::THROW,
			Some(&Token::Delete) => keywords::DELETE,
			Some(&Token::In) => keywords::IN,
			Some(&Token::Try) => keywords::TRY,
			// Future reserved words
			Some(&Token::Class) => keywords::CLASS,
			Some(&Token::Enum) => keywords::ENUM,
			Some(&Token::Extends) => keywords::EXTENDS,
			Some(&Token::Super) => keywords::SUPER,
			Some(&Token::Const) => keywords::CONST,
			Some(&Token::Export) => keywords::EXPORT,
			Some(&Token::Import) => keywords::IMPORT,
			Some(&Token::Implements) => keywords::IMPLEMENTS,
			Some(&Token::Let) => keywords::LET,
			Some(&Token::Private) => keywords::PRIVATE,
			Some(&Token::Public) => keywords::PUBLIC,
			Some(&Token::Interface) => keywords::INTERFACE,
			Some(&Token::Package) => keywords::PACKAGE,
			Some(&Token::Protected) => keywords::PROTECTED,
			Some(&Token::Static) => keywords::STATIC,
			Some(&Token::Yield) => keywords::YIELD,
			_ => return Ok(None)
		};
		
		self.bump();
		
		Ok(Some(name))
	}
	
	fn parse_lit(&mut self) -> ParseResult<Rc<Lit>> {
		if let Token::Literal(ref lit) = *self.next() {
			return Ok(lit.clone());
		}
		
		self.fatal("Expected literal")
	}
	
	fn parse_block(&mut self) -> ParseResult<Block> {
		try!(self.expect(&Token::OpenBrace));
		
		let stmts = try!(self.parse_stmt_list());
		
		try!(self.expect(&Token::CloseBrace));
		
		Ok(Block {
			stmts: stmts
		})
	}
	
	fn parse_stmt(&mut self) -> ParseResult<Item> {
		match self.peek() {
			Some(&Token::OpenBrace) => Ok(Item::Block(try!(self.parse_block()))),
			Some(&Token::Var) => self.parse_var_stmt(),
			Some(&Token::SemiColon) => {
				self.bump();
				Ok(Item::Empty)
			},
			Some(&Token::If) => self.parse_if(),
			Some(&Token::Do) => self.parse_do(),
			Some(&Token::While) => self.parse_while(),
			Some(&Token::For) => self.parse_for(),
			Some(&Token::Continue) => self.parse_continue(),
			Some(&Token::Break) => self.parse_break(),
			Some(&Token::Return) => self.parse_return(),
			Some(&Token::With) => self.parse_with(),
			Some(&Token::Switch) => self.parse_switch(),
			Some(&Token::Throw) => self.parse_throw(),
			Some(&Token::Try) => self.parse_try(),
			Some(&Token::Debugger) => self.parse_debugger(),
			_ => {
				if let Some(&Token::Identifier(..)) = self.peek() {
					if let Some(&Token::Colon) = self.peek_at(1) {
						return self.parse_labelled();
					}
				}
				
				self.parse_expr_stmt()
			}
		}
	}
	
	fn parse_stmt_list(&mut self) -> ParseResult<Vec<Item>> {
		let mut stmts = Vec::new();
		
		loop {
			// Parse statements until we find an end condition. We also match Default and Case
			// so this can be used for Switch too.
			
			match self.peek() {
				None | Some(&Token::CloseBrace) | Some(&Token::Case) | Some(&Token::Default) => return Ok(stmts),
				_ => {}
			}
			
			stmts.push(try!(self.parse_stmt()));
		}
	}
	
	fn parse_var_stmt(&mut self) -> ParseResult<Item> {
		self.bump();
		
		let var_decl = try!(self.parse_var_decl());
		
		try!(self.expect_eos());
		
		Ok(Item::VarDecl(var_decl))
	}
	
	fn parse_var_decl(&mut self) -> ParseResult<Vec<Var>> {
		let mut vars = Vec::new();
		
		while !self.is_eof() {
			let ident = try!(self.parse_ident());
			
			let expr = if self.consume(&Token::Assign) {
				Some(Box::new(try!(self.parse_expr())))
			} else {
				None
			};
			
			vars.push(Var {
				ident: ident,
				expr: expr
			});
			
			if !self.consume(&Token::Comma) {
				return Ok(vars);
			}
		}
		
		self.fatal("Cannot parse variable declaration")
	}
	
	fn parse_expr(&mut self) -> ParseResult<Expr> {
		let mut expr = try!(match self.peek() {
			Some(&Token::Function) => {
				self.bump();
				
				let (ident, args, block) = try!(self.parse_function());
				
				Ok(Expr::Function(ident, args, block))
			},
			Some(&Token::New) => self.parse_expr_new(),
			Some(&Token::Delete) => self.parse_expr_unary_pre(Op::Delete),
			Some(&Token::Void) => self.parse_expr_unary_pre(Op::Void),
			Some(&Token::Typeof) => self.parse_expr_unary_pre(Op::Typeof),
			Some(&Token::This) => {
				self.bump();
				Ok(Expr::This)
			},
			Some(&Token::PlusPlus) => self.parse_expr_unary_pre(Op::PreIncr),
			Some(&Token::MinusMinus) => self.parse_expr_unary_pre(Op::PreDecr),
			Some(&Token::Plus) => self.parse_expr_unary_pre(Op::Plus),
			Some(&Token::Minus) => self.parse_expr_unary_pre(Op::Minus),
			Some(&Token::BitNot) => self.parse_expr_unary_pre(Op::BitNot),
			Some(&Token::Not) => self.parse_expr_unary_pre(Op::Not),
			Some(&Token::Identifier(..)) => self.parse_expr_ident(),
			Some(&Token::Literal(..)) => self.parse_expr_literal(),
			Some(&Token::OpenParen) => self.parse_expr_paren(),
			Some(&Token::OpenBracket) => self.parse_expr_array_literal(),
			Some(&Token::OpenBrace) => self.parse_expr_object_literal(),
			_ => {
				let message = { format!("Cannot parse expression, got {:?}", self.peek()) };
				
				self.fatal(&message)
			}
		});
		
		loop {
			expr = try!(match self.peek() {
				Some(&Token::OpenBracket) => self.parse_expr_member_index(expr),
				Some(&Token::Dot) => self.parse_expr_member_dot(expr),
				Some(&Token::OpenParen) => self.parse_expr_call(expr),
				Some(&Token::PlusPlus) => {
					match self.peek_any() {
						Some(&Token::Newline) => return Ok(expr),
						_ => self.parse_expr_unary_post(expr, Op::PostIncr)
					}
				},
				Some(&Token::MinusMinus) => {
					match self.peek_any() {
						Some(&Token::Newline) => return Ok(expr),
						_ => self.parse_expr_unary_post(expr, Op::PostDecr)
					}
				},
				Some(&Token::Multiply) => self.parse_expr_binary(expr, Op::Multiply),
				Some(&Token::Divide) => self.parse_expr_binary(expr, Op::Divide),
				Some(&Token::Modulus) => self.parse_expr_binary(expr, Op::Modulus),
				Some(&Token::Plus) => self.parse_expr_binary(expr, Op::Add),
				Some(&Token::Minus) => self.parse_expr_binary(expr, Op::Subtract),
				Some(&Token::LeftShiftArithmetic) => self.parse_expr_binary(expr, Op::LeftShiftArithmetic),
				Some(&Token::RightShiftArithmetic) => self.parse_expr_binary(expr, Op::RightShiftArithmetic),
				Some(&Token::RightShiftLogical) => self.parse_expr_binary(expr, Op::RightShiftLogical),
				Some(&Token::LessThan) => self.parse_expr_binary(expr, Op::LessThan),
				Some(&Token::GreaterThan) => self.parse_expr_binary(expr, Op::GreaterThan),
				Some(&Token::LessThanEquals) => self.parse_expr_binary(expr, Op::LessThanEquals),
				Some(&Token::GreaterThanEquals) => self.parse_expr_binary(expr, Op::GreaterThanEquals),
				Some(&Token::Instanceof) => self.parse_expr_binary(expr, Op::InstanceOf),
				Some(&Token::In) => self.parse_expr_binary(expr, Op::In),
				Some(&Token::Equals) => self.parse_expr_binary(expr, Op::Equals),
				Some(&Token::NotEquals) => self.parse_expr_binary(expr, Op::NotEquals),
				Some(&Token::IdentityEquals) => self.parse_expr_binary(expr, Op::IdentityEquals),
				Some(&Token::IdentityNotEquals) => self.parse_expr_binary(expr, Op::IdentityNotEquals),
				Some(&Token::BitAnd) => self.parse_expr_binary(expr, Op::BitAnd),
				Some(&Token::BitXOr) => self.parse_expr_binary(expr, Op::BitXOr),
				Some(&Token::BitOr) => self.parse_expr_binary(expr, Op::BitOr),
				Some(&Token::And) => self.parse_expr_binary(expr, Op::And),
				Some(&Token::Or) => self.parse_expr_binary(expr, Op::Or),
				Some(&Token::QuestionMark) => self.parse_expr_ternary(expr),
				Some(&Token::Assign) => self.parse_expr_binary_assign(expr, Op::Assign),
				Some(&Token::MultiplyAssign) => self.parse_expr_binary_assign(expr, Op::Multiply),
				Some(&Token::DivideAssign) => self.parse_expr_binary_assign(expr, Op::Divide),
				Some(&Token::ModulusAssign) => self.parse_expr_binary_assign(expr, Op::Modulus),
				Some(&Token::PlusAssign) => self.parse_expr_binary_assign(expr, Op::Add),
				Some(&Token::MinusAssign) => self.parse_expr_binary_assign(expr, Op::Subtract),
				Some(&Token::LeftShiftArithmeticAssign) => self.parse_expr_binary_assign(expr, Op::LeftShiftArithmetic),
				Some(&Token::RightShiftArithmeticAssign) => self.parse_expr_binary_assign(expr, Op::RightShiftArithmetic),
				Some(&Token::RightShiftLogicalAssign) => self.parse_expr_binary_assign(expr, Op::RightShiftLogical),
				Some(&Token::BitAndAssign) => self.parse_expr_binary_assign(expr, Op::BitAnd),
				Some(&Token::BitXOrAssign) => self.parse_expr_binary_assign(expr, Op::BitXOr),
				Some(&Token::BitOrAssign) => self.parse_expr_binary_assign(expr, Op::BitOr),
				_ => return Ok(expr)
			});
		}
	}
	
	fn parse_expr_member_index(&mut self, expr: Expr) -> ParseResult<Expr> {
		self.bump();
		
		let index = try!(self.parse_expr_seq());
		
		try!(self.expect(&Token::CloseBracket));
		
		Ok(Expr::MemberIndex(Box::new(expr), index))
	}
	
	fn parse_expr_member_dot(&mut self, expr: Expr) -> ParseResult<Expr> {
		self.bump();
		
		if let Some(name) = try!(self.parse_ident_name()) {
			Ok(Expr::MemberDot(Box::new(expr), name.ident()))
		} else {
			self.fatal("Expected identifier name")
		}
	}
	
	fn parse_expr_call(&mut self, expr: Expr) -> ParseResult<Expr> {
		let args = try!(self.parse_arguments());
		
		Ok(Expr::Call(Box::new(expr), args))
	}
	
	fn parse_expr_binary(&mut self, expr: Expr, op: Op) -> ParseResult<Expr> {
		self.bump();
		
		let right = try!(self.parse_expr());
		
		Ok(Expr::Binary(op, Box::new(expr), Box::new(right)))
	}
	
	fn parse_expr_binary_assign(&mut self, expr: Expr, op: Op) -> ParseResult<Expr> {
		self.bump();
		
		let right = try!(self.parse_expr_seq());
		
		Ok(Expr::Assign(op, Box::new(expr), right))
	}
	
	fn parse_expr_ternary(&mut self, expr: Expr) -> ParseResult<Expr> {
		self.bump();
		
		let then = try!(self.parse_expr());
		
		try!(self.expect(&Token::Colon));
		
		let else_ = try!(self.parse_expr());
		
		Ok(Expr::Ternary(Box::new(expr), Box::new(then), Box::new(else_)))
	}
	
	fn parse_expr_new(&mut self) -> ParseResult<Expr> {
		self.bump();
		
		let expr = try!(self.parse_expr());
		
		let args = if let Some(&Token::OpenParen) = self.peek() {
			Some(try!(self.parse_arguments()))
		} else {
			None
		};
		
		Ok(Expr::New(Box::new(expr), args))
	}
	
	fn parse_expr_unary_pre(&mut self, op: Op) -> ParseResult<Expr> {
		self.bump();
		
		let expr = try!(self.parse_expr());
		
		Ok(Expr::Unary(op, Box::new(expr)))
	}
	
	fn parse_expr_unary_post(&mut self, expr: Expr, op: Op) -> ParseResult<Expr> {
		self.bump();
		
		Ok(Expr::Unary(op, Box::new(expr)))
	}
	
	fn parse_arguments(&mut self) -> ParseResult<Vec<Expr>> {
		try!(self.expect(&Token::OpenParen));
		
		if self.consume(&Token::CloseParen) {
			return Ok(Vec::new());
		}
		
		let mut args = Vec::new();
		
		loop {
			args.push(try!(self.parse_expr()));
			
			if self.consume(&Token::CloseParen) {
				return Ok(args);
			}
			
			try!(self.expect(&Token::Comma));
		}
	}
	
	fn parse_expr_ident(&mut self) -> ParseResult<Expr> {
		Ok(Expr::Ident(try!(self.parse_ident())))
	}
	
	fn parse_expr_literal(&mut self) -> ParseResult<Expr> {
		if let &Token::Literal(ref lit) = self.next() {
			return Ok(Expr::Literal(lit.clone()));
		}
		
		self.fatal("Expected literal")
	}
	
	fn parse_expr_paren(&mut self) -> ParseResult<Expr> {
		self.bump();
		
		let exprs = try!(self.parse_expr_seq());
		
		try!(self.expect(&Token::CloseParen));
		
		Ok(Expr::Paren(exprs))
	}
	
	fn parse_expr_array_literal(&mut self) -> ParseResult<Expr> {
		self.bump();
		
		let mut elems = Vec::new();
		
		if !self.consume(&Token::CloseBracket) {
			loop {
				// After an element is parsed, we check for a CloseBracket or Comma. This means
				// that when we find a CloseBracket or Comma here, it means that we have
				// an elision.
				
				match self.peek() {
					Some(&Token::CloseBracket) => {
						self.bump();
						elems.push(Expr::Missing);
						break;
					},
					Some(&Token::Comma) => {
						self.bump();
						elems.push(Expr::Missing);
						continue;
					},
					_ => {
						elems.push(try!(self.parse_expr()));
						
						if self.consume(&Token::CloseBracket) {
							break;
						}
						try!(self.expect(&Token::Comma));
					}
				}
			}
		}
	
		return Ok(Expr::ArrayLiteral(elems));
	}
	
	fn parse_expr_object_literal(&mut self) -> ParseResult<Expr> {
		self.bump();
		
		let mut props = Vec::new();
		
		// First try to match an empty object literal with a comma, i.e. "{,}".
		
		if !self.consume(&Token::CloseBrace) {
			if self.consume(&Token::Comma) {
				try!(self.expect(&Token::CloseBrace));
			} else {
				loop {
					props.push(try!(self.parse_expr_object_literal_prop()));
					
					// "}" or ",}" closes an object literal.
					
					if self.consume(&Token::CloseBrace) {
						break;
					}
					try!(self.expect(&Token::Comma));
					if self.consume(&Token::CloseBrace) {
						break;
					}
				}
			}
		}
		
		Ok(Expr::ObjectLiteral(props))
	}
	
	fn parse_expr_object_literal_prop(&mut self) -> ParseResult<Property> {
		if let Some(name) = try!(self.parse_ident_name()) {
			let prop_ident = match try!(self.parse_ident_name()) {
				Some(name) => Some(name.ident()),
				None => None
			};
			
			if self.consume(&Token::OpenParen) {
				if name == keywords::GET {
					try!(self.expect(&Token::CloseParen));
					
					let block = try!(self.parse_function_block());
					
					Ok(Property::Getter(prop_ident, block))
				} else if name == keywords::SET {
					let param = try!(self.parse_ident());
					
					try!(self.expect(&Token::CloseParen));
					
					let block = try!(self.parse_function_block());
					
					Ok(Property::Setter(prop_ident, param, block))
				} else {
					self.fatal("Property getter or setter must start with get or set")
				}
			} else {
				if prop_ident.is_some() {
					self.fatal("Unexpected identifier")
				} else {
					self.consume(&Token::Colon);
					
					let expr = try!(self.parse_expr());
					
					Ok(Property::Assignment(PropertyKey::Ident(name.ident()), Box::new(expr)))
				}
			}
		} else if let Some(&Token::Literal(..)) = self.peek() {
			let lit = try!(self.parse_lit());
			
			// Only strings and numbers are accepted as property keys.
			
			match *lit {
				Lit::Null | Lit::Boolean(..) | Lit::Regex(..) => return self.fatal("Expected property key literal"),
				Lit::String(..) | Lit::HexInteger(..) | Lit::OctalInteger(..) | Lit::Decimal(..) => {}
			}
			
			self.consume(&Token::Colon);
			
			let expr = try!(self.parse_expr());
			
			Ok(Property::Assignment(PropertyKey::Literal(lit), Box::new(expr)))
		} else {
			let message = { format!("Cannot parse object literal property, got {:?}", self.peek()) };
			
			self.fatal(&message)
		}
	}
	
	fn parse_if(&mut self) -> ParseResult<Item> {
		self.bump();
		
		try!(self.expect(&Token::OpenParen));
		
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect(&Token::CloseParen));
		
		let then = try!(self.parse_stmt());
		
		let else_ = if self.consume(&Token::Else) {
			Some(Box::new(try!(self.parse_stmt())))
		} else {
			None
		};
		
		Ok(Item::If(expr, Box::new(then), else_))
	}
	
	fn parse_expr_seq(&mut self) -> ParseResult<ExprSeq> {
		let mut exprs = Vec::new();
		
		exprs.push(try!(self.parse_expr()));
		
		while self.consume(&Token::Comma) {
			exprs.push(try!(self.parse_expr()));
		}
		
		Ok(ExprSeq {
			exprs: exprs
		})
	}
	
	fn parse_do(&mut self) -> ParseResult<Item> {
		self.bump();
		
		let stmt = try!(self.parse_stmt());
		
		try!(self.expect(&Token::While));
		try!(self.expect(&Token::OpenParen));
		
		let expr = try!(self.parse_expr());
		
		try!(self.expect(&Token::CloseParen));
		try!(self.expect_eos());
		
		Ok(Item::Do(Box::new(expr), Box::new(stmt)))
	}
	
	fn parse_while(&mut self) -> ParseResult<Item> {
		self.bump();
		
		try!(self.expect(&Token::OpenParen));
		
		let expr = try!(self.parse_expr());
		
		try!(self.expect(&Token::CloseParen));
		
		let stmt = try!(self.parse_stmt());
		
		Ok(Item::While(Box::new(expr), Box::new(stmt)))
	}
	
	fn parse_for(&mut self) -> ParseResult<Item> {
		self.bump();
		
		try!(self.expect(&Token::OpenParen));
		
		if self.consume(&Token::Var) {
			// Either ForVar or ForVarIn.
			
			let vars = try!(self.parse_var_decl());
			
			match *self.next() {
				Token::SemiColon => {
					let (test, incr, stmt) = try!(self.parse_for_tail());
					
					Ok(Item::ForVar(Some(vars), test, incr, Box::new(stmt)))
				},
				Token::In => {
					// A ForVarIn can only have a single var decl.
					
					if vars.len() != 1 {
						return self.fatal("For var in must have a single variable declaration");
					}
					
					let expr = try!(self.parse_expr_seq());
					
					try!(self.expect(&Token::CloseParen));
					
					let stmt = try!(self.parse_stmt());
					
					Ok(Item::ForVarIn(vars.single(), expr, Box::new(stmt)))
				},
				_ => self.fatal("Cannot parse for var")
			}
		} else {
			// Either For or ForIn.
			
			if self.consume(&Token::SemiColon) {
				// A For without an initial expression.
				
				let (test, incr, stmt) = try!(self.parse_for_tail());
				
				Ok(Item::For(None, test, incr, Box::new(stmt)))
			} else {
				let expr = try!(self.parse_expr_seq());
				
				// If the expression sequence is followed by a semi colon, we have a For.
				// Otherwise we have a ForIn.
				
				if self.consume(&Token::SemiColon) {
					let (test, incr, stmt) = try!(self.parse_for_tail());
					
					Ok(Item::For(Some(expr), test, incr, Box::new(stmt)))
				} else {
					// A ForIn can only have a single expression.
					
					if expr.exprs.len() != 1 {
						return self.fatal("For in must have a single expression");
					}
					
					// Simple in's are parsed as an In binary expression.
					
					let expr = expr.exprs.single();
					
					if let Expr::Binary(op, left, right) = expr {
						if op == Op::In {
							try!(self.expect(&Token::CloseParen));
							
							let stmt = try!(self.parse_stmt());
							
							let in_ = ExprSeq { exprs: vec![*right] };
							
							return Ok(Item::ForIn(left, in_, Box::new(stmt)));
						}
					}
					
					self.fatal("Expected in expression")
				}
			}
		}
	}
	
	fn parse_for_tail(&mut self) -> ParseResult<(Option<ExprSeq>, Option<ExprSeq>, Item)> {
		let test = if self.consume(&Token::SemiColon) {
			None
		} else {
			let expr = try!(self.parse_expr_seq());
			
			try!(self.expect(&Token::SemiColon));
			
			Some(expr)
		};
		
		let incr = if self.consume(&Token::CloseParen) {
			None
		} else {
			let expr = try!(self.parse_expr_seq());
			
			try!(self.expect(&Token::CloseParen));
			
			Some(expr)
		};
		
		let stmt = try!(self.parse_stmt());
		
		Ok((test, incr, stmt))
	}
	
	fn parse_continue(&mut self) -> ParseResult<Item> {
		self.bump();
		
		let ident = try!(self.parse_opt_ident());
		
		try!(self.expect_eos());
		
		Ok(Item::Continue(ident))
	}
	
	fn parse_break(&mut self) -> ParseResult<Item> {
		self.bump();
		
		let ident = try!(self.parse_opt_ident());
		
		try!(self.expect_eos());
		
		Ok(Item::Break(ident))
	}
	
	fn parse_opt_ident(&mut self) -> ParseResult<Option<Ident>> {
		let ident = match self.peek() {
			Some(&Token::Identifier(name)) => name.ident(),
			_ => return Ok(None)
		};
		
		self.next();
		
		Ok(Some(ident))
	}
	
	fn parse_return(&mut self) -> ParseResult<Item> {
		self.bump();
		
		let expr = if self.is_eos() {
			None
		} else {
			Some(try!(self.parse_expr_seq()))
		};
		
		try!(self.expect_eos());
		
		Ok(Item::Return(expr))
	}
	
	fn parse_with(&mut self) -> ParseResult<Item> {
		self.bump();
		
		try!(self.expect(&Token::OpenParen));
		
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect(&Token::CloseParen));
		
		let stmt = try!(self.parse_stmt());
		
		Ok(Item::With(expr, Box::new(stmt)))
	}
	
	fn parse_switch(&mut self) -> ParseResult<Item> {
		self.bump();
		
		try!(self.expect(&Token::OpenParen));
		
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect(&Token::CloseParen));
		
		try!(self.expect(&Token::OpenBrace));
		
		let mut cases: Vec<SwitchClause> = Vec::new();
		let mut have_default = false;
		
		loop {
			match *self.next() {
				Token::CloseBrace => break,
				Token::Case => {
					let case_expr = try!(self.parse_expr_seq());
					
					try!(self.expect(&Token::Colon));
					
					let stmts = try!(self.parse_stmt_list());
					
					cases.push(SwitchClause::Case(case_expr, stmts));
				},
				Token::Default => {
					try!(self.expect(&Token::Colon));
					
					// Switch can only have a single default clause.
					
					if have_default {
						return self.fatal("Only one default can be provided");
					}
					
					have_default = true;
					
					let stmts = try!(self.parse_stmt_list());
					
					cases.push(SwitchClause::Default(stmts));
				},
				_ => return self.fatal("Cannot parse switch case")
			}
		}
		
		Ok(Item::Switch(expr, cases))
	}
	
	fn parse_throw(&mut self) -> ParseResult<Item> {
		self.bump();
		
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect_eos());
		
		Ok(Item::Throw(expr))
	}
	
	fn parse_try(&mut self) -> ParseResult<Item> {
		self.bump();
		
		let try = try!(self.parse_block());
		
		let catch = if self.consume(&Token::Catch) {
			try!(self.expect(&Token::OpenParen));
			
			let ident = try!(self.parse_ident());
			
			try!(self.expect(&Token::CloseParen));
			
			let catch = try!(self.parse_block());
			
			Some(Catch {
				ident: ident,
				block: catch
			})
		} else {
			None
		};
		
		let finally = if self.consume(&Token::Finally) {
			Some(try!(self.parse_block()))
		} else {
			None
		};
		
		// Either a Catch or Finally is required.
		
		if !catch.is_some() && !finally.is_some() {
			self.fatal("Either a catch or finally is required")
		} else {
			Ok(Item::Try(try, catch, finally))
		}
	}
	
	fn parse_debugger(&mut self) -> ParseResult<Item> {
		self.bump();
		
		try!(self.expect_eos());
		
		Ok(Item::Debugger)
	}
	
	fn parse_labelled(&mut self) -> ParseResult<Item> {
		let ident = try!(self.parse_ident());
		
		self.bump();
		
		let stmt = try!(self.parse_stmt());
		
		Ok(Item::Labelled(ident, Box::new(stmt)))
	}
	
	fn parse_expr_stmt(&mut self) -> ParseResult<Item> {
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect_eos());
		
		Ok(Item::ExprStmt(expr))
	}
}
