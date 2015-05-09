use syntax::lexer::Lexer;
use syntax::ast::*;
use syntax::token::{Token, Lit};
use syntax::Span;
use syntax::token::keywords;
use ::{JsResult, JsError};
use std::rc::Rc;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use util::iter::*;

mod locals;

pub struct Parser<'a> {
	lexer: &'a mut Lexer,
	context: &'a mut AstContext,
	scopes: Vec<Scope>
}

struct Scope {
	slots: Vec<Slot>,
	blocks: Vec<BlockScope>
}

struct BlockScope {
	locals: HashMap<Name, SlotRef>
}

impl<'a> Parser<'a> {
	fn at_global(&self) -> bool {
		self.scopes.len() == 1 && self.scopes[0].blocks.len() == 1
	}
	
	fn push_scope(&mut self) {
		self.scopes.push(Scope {
			slots: Vec::new(),
			blocks: Vec::new()
		})
	}
	
	fn pop_scope(&mut self) -> Vec<Slot> {
		self.scopes.pop().unwrap().slots
	}
	
	fn push_block_scope(&mut self) {
		let len = self.scopes.len();
		self.scopes[len - 1].blocks.push(BlockScope {
			locals: HashMap::new()
		});
	}
	
	fn pop_block_scope(&mut self) -> HashMap<Name, SlotRef> {
		let len = self.scopes.len();
		self.scopes[len - 1].blocks.pop().unwrap().locals
	}
	
	fn register_local(&mut self, name: Name, throwarg: bool) {
		// TODO: Validate performance. We expect that most locals set will be relatively small.
		// Changing this into a HashSet will very likely not make sense. However, this will
		// give issues in functions/scopes with many globals (probably more than 20 or 30).
		// An alternative is to leave duplicates in here and let the IR builder take care
		// of deduplicating then. It's already pushing the locals into a HashMap so that isn't
		// a problem.
				
		let len = self.scopes.len();
		let scope = &mut self.scopes[len - 1];
		
		let block = if name == keywords::ARGUMENTS {
			// If this is the arguments variable, we can just drop it.
			
			return;
		} else if !throwarg {
			// Has this local already been declared?
			
			if scope.blocks[0].locals.contains_key(&name) {
				return;
			}
			
			&mut scope.blocks[0]
		} else {
			let len = scope.blocks.len();
			&mut scope.blocks[len - 1]
		};
		
		// We need to declare a new variable. Create a slot for it.
		
		let slot = Slot {
			name: name,
			arg: None,
			lifted: false
		};
		
		let local_slot_ref = SlotRef(scope.slots.len());
		scope.slots.push(slot);

		// Register the local.
		
		block.locals.insert(name, local_slot_ref);
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
	
	fn expect(&mut self, token: &Token) -> JsResult<()> {
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
	
	fn fatal<T>(&self, message: &str) -> JsResult<T> {
		let span = match self.lexer.peek(0) {
			Some(token) => token.span().clone(),
			_ => Span::new(-1, -1, -1, -1)
		};
		
		let message = format!("{}:{}: {}", span.start_line, span.start_col, message.to_string());
		
		// Panic here under debug to get a stack trace.
		
		if cfg!(not(ndebug)) {
			panic!(message);
		}
		
		Err(JsError::Parse(message))
	}
	
	fn expect_eos(&mut self) -> JsResult<()> {
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
	
	pub fn parse_program(context: &'a mut AstContext, lexer: &'a mut Lexer) -> JsResult<FunctionRef> {
		let program = {
			let mut parser = Parser {
				lexer: lexer,
				context: context,
				scopes: Vec::new()
			};
			
			parser.push_scope();
			parser.push_block_scope();
			
			let mut items = Vec::new();
			
			while !parser.is_eof() {
				items.push(try!(parser.parse_item()));
			}
			
			let locals = parser.pop_block_scope();
			let slots = parser.pop_scope();
	
			Box::new(Function {
				name: None,
				global: true,
				block: RootBlock {
					block: Block {
						stmts: items,
						locals: locals
					},
					locals: RefCell::new(Locals::new(slots)),
					args: Vec::new()
				}
			})
		};
		
		let program_ref = FunctionRef(context.functions.len() as u32);
		context.functions.push(program);
		
		locals::LocalResolver::resolve(context, program_ref);
		
		Ok(program_ref)
	}
	
	fn parse_item(&mut self) -> JsResult<Item> {
		if let Some(&Token::Function) = self.peek() {
			if let Some(&Token::Identifier(..)) = self.peek_at(1) {
				self.bump();
				
				let function_ref = try!(self.parse_function());
				let name = self.context.functions[function_ref.usize()].name.unwrap();
				
				if self.scopes.len() > 1 {
					self.register_local(name, false);
				}
				
				let ident = Ident {
					name: name,
					state: Cell::new(IdentState::None)
				};
				
				return Ok(Item::Function(ident, function_ref));
			}
		}
		
		self.parse_stmt(None)
	}
	
	fn parse_function(&mut self) -> JsResult<FunctionRef> {
		let name = try!(self.parse_opt_name());
		
		let args = try!(self.parse_parameter_list());
		
		let block = try!(self.parse_function_block(args));
		
		let function = Box::new(Function {
			global: false,
			name: name,
			block: block
		});
		
		let function_ref = FunctionRef(self.context.functions.len() as u32);
		self.context.functions.push(function);
		
		Ok(function_ref)
	}
	
	fn parse_function_block(&mut self, args: Vec<Name>) -> JsResult<RootBlock> {
		self.push_scope();
		
		try!(self.expect(&Token::OpenBrace));
		
		let mut stmts = Vec::new();
		
		self.push_block_scope();
		
		if args.len() > 0 {
			self.register_function_args(&args);
		}
		
		while !self.consume(&Token::CloseBrace) {
			stmts.push(try!(self.parse_item()));
		}
		
		let locals = self.pop_block_scope();
		let slots = self.pop_scope();
		
		Ok(RootBlock {
			block: Block {
				stmts: stmts,
				locals: locals
			},
			locals: RefCell::new(Locals::new(slots)),
			args: args
		})
	}
	
	fn register_function_args(&mut self, args: &Vec<Name>) {
		let len = self.scopes.len();
		let scope = &mut self.scopes[len - 1];
		
		for i in 0..args.len() {
			let name = args[i];
			
			// Create a slot for the parameter.
		
			let slot = Slot {
				name: name,
				arg: Some(i as u32),
				lifted: false
			};
			
			let local_slot_ref = SlotRef(scope.slots.len());
			scope.slots.push(slot);
	
			// Register the parameter.
			
			scope.blocks[0].locals.insert(name, local_slot_ref);
		}
	}
	
	fn parse_parameter_list(&mut self) -> JsResult<Vec<Name>> {
		try!(self.expect(&Token::OpenParen));
		
		if self.consume(&Token::CloseParen) {
			return Ok(Vec::new());
		}
		
		let mut args = Vec::new();
		
		while !self.is_eof() {
			args.push(try!(self.parse_name()));
			
			match *self.next() {
				Token::Comma => continue,
				Token::CloseParen => return Ok(args),
				_ => break
			}
		}
		
		self.fatal("Cannot parse parameter list")
	}
	
	fn parse_name(&mut self) -> JsResult<Name> {
		if let Token::Identifier(name) = *self.next() {
			Ok(name)
		} else {
			self.fatal("Expected identifier")
		}
	}
	
	fn parse_opt_name(&mut self) -> JsResult<Option<Name>> {
		let name = match self.peek() {
			Some(&Token::Identifier(name)) => name,
			_ => return Ok(None)
		};
		
		self.next();
		
		Ok(Some(name))
	}
	
	fn parse_ident(&mut self) -> JsResult<Ident> {
		Ok(Ident {
			name: try!(self.parse_name()),
			state: Cell::new(IdentState::None)
		})
	}
	
	fn parse_ident_name(&mut self) -> JsResult<Option<Name>> {
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
	
	fn parse_lit(&mut self) -> JsResult<Rc<Lit>> {
		if let Token::Literal(ref lit) = *self.next() {
			return Ok(lit.clone());
		}
		
		self.fatal("Expected literal")
	}
	
	fn parse_block(&mut self) -> JsResult<Block> {
		try!(self.expect(&Token::OpenBrace));
		
		let stmts = try!(self.parse_stmt_list());
		
		try!(self.expect(&Token::CloseBrace));
		
		Ok(Block {
			stmts: stmts,
			locals: HashMap::new()
		})
	}
	
	fn parse_stmt(&mut self, label: Option<Label>) -> JsResult<Item> {
		match self.peek() {
			Some(&Token::OpenBrace) => Ok(Item::Block(label, try!(self.parse_block()))),
			Some(&Token::Var) => self.parse_var_stmt(),
			Some(&Token::SemiColon) => {
				self.bump();
				Ok(Item::Empty)
			},
			Some(&Token::If) => self.parse_if(),
			Some(&Token::Do) => self.parse_do(label),
			Some(&Token::While) => self.parse_while(label),
			Some(&Token::For) => self.parse_for(label),
			Some(&Token::Continue) => self.parse_continue(),
			Some(&Token::Break) => self.parse_break(),
			Some(&Token::Return) => self.parse_return(),
			Some(&Token::With) => self.parse_with(),
			Some(&Token::Switch) => self.parse_switch(label),
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
	
	fn parse_stmt_list(&mut self) -> JsResult<Vec<Item>> {
		let mut stmts = Vec::new();
		
		loop {
			// Parse statements until we find an end condition. We also match Default and Case
			// so this can be used for Switch too.
			
			match self.peek() {
				None | Some(&Token::CloseBrace) | Some(&Token::Case) | Some(&Token::Default) => return Ok(stmts),
				_ => {}
			}
			
			stmts.push(try!(self.parse_stmt(None)));
		}
	}
	
	fn parse_var_stmt(&mut self) -> JsResult<Item> {
		self.bump();
		
		let var_decl = try!(self.parse_var_decl());
		
		try!(self.expect_eos());
		
		Ok(Item::VarDecl(var_decl))
	}
	
	fn parse_var_decl(&mut self) -> JsResult<Vec<Var>> {
		let mut vars = Vec::new();
		
		while !self.is_eof() {
			let ident = try!(self.parse_ident());
			
			// Don't register locals at the global scope.
			
			if !self.at_global() {
				self.register_local(ident.name, false);
			}
			
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
	
	fn parse_expr(&mut self) -> JsResult<Expr> {
		let mut expr = try!(match self.peek() {
			Some(&Token::Function) => {
				self.bump();
				
				Ok(Expr::Function(try!(self.parse_function())))
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
			Some(&Token::Plus) => self.parse_expr_unary_pre(Op::Positive),
			Some(&Token::Minus) => self.parse_expr_unary_pre(Op::Negative),
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
	
	fn parse_expr_member_index(&mut self, expr: Expr) -> JsResult<Expr> {
		self.bump();
		
		let index = try!(self.parse_expr_seq());
		
		try!(self.expect(&Token::CloseBracket));
		
		Ok(Expr::MemberIndex(Box::new(expr), index))
	}
	
	fn parse_expr_member_dot(&mut self, expr: Expr) -> JsResult<Expr> {
		self.bump();
		
		if let Some(name) = try!(self.parse_ident_name()) {
			Ok(Expr::MemberDot(Box::new(expr), name))
		} else {
			self.fatal("Expected identifier name")
		}
	}
	
	fn parse_expr_call(&mut self, expr: Expr) -> JsResult<Expr> {
		let args = try!(self.parse_arguments());
		
		Ok(Expr::Call(Box::new(expr), args))
	}
	
	fn parse_expr_binary(&mut self, expr: Expr, op: Op) -> JsResult<Expr> {
		self.bump();
		
		let right = try!(self.parse_expr());
		
		Ok(Expr::Binary(op, Box::new(expr), Box::new(right)))
	}
	
	fn parse_expr_binary_assign(&mut self, expr: Expr, op: Op) -> JsResult<Expr> {
		self.bump();
		
		let right = try!(self.parse_expr_seq());
		
		Ok(Expr::Assign(op, Box::new(expr), right))
	}
	
	fn parse_expr_ternary(&mut self, expr: Expr) -> JsResult<Expr> {
		self.bump();
		
		let then = try!(self.parse_expr());
		
		try!(self.expect(&Token::Colon));
		
		let else_ = try!(self.parse_expr());
		
		Ok(Expr::Ternary(Box::new(expr), Box::new(then), Box::new(else_)))
	}
	
	fn parse_expr_new(&mut self) -> JsResult<Expr> {
		self.bump();
		
		Ok(Expr::New(Box::new(try!(self.parse_expr()))))
	}
	
	fn parse_expr_unary_pre(&mut self, op: Op) -> JsResult<Expr> {
		self.bump();
		
		let expr = try!(self.parse_expr());
		
		// Rebalance binary expression because presendence of unary
		// operator is higher than that of a binary expresion.
		
		if let Expr::Binary(binop, lhs, rhs) = expr {
			Ok(Expr::Binary(
				binop,
				Box::new(Expr::Unary(op, lhs)),
				rhs
			))
		} else {
			Ok(Expr::Unary(op, Box::new(expr)))
		}
	}
	
	fn parse_expr_unary_post(&mut self, expr: Expr, op: Op) -> JsResult<Expr> {
		self.bump();
		
		Ok(Expr::Unary(op, Box::new(expr)))
	}
	
	fn parse_arguments(&mut self) -> JsResult<Vec<Expr>> {
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
	
	fn parse_expr_ident(&mut self) -> JsResult<Expr> {
		Ok(Expr::Ident(try!(self.parse_ident())))
	}
	
	fn parse_expr_literal(&mut self) -> JsResult<Expr> {
		if let &Token::Literal(ref lit) = self.next() {
			return Ok(Expr::Literal(lit.clone()));
		}
		
		self.fatal("Expected literal")
	}
	
	fn parse_expr_paren(&mut self) -> JsResult<Expr> {
		self.bump();
		
		let exprs = try!(self.parse_expr_seq());
		
		try!(self.expect(&Token::CloseParen));
		
		Ok(Expr::Paren(exprs))
	}
	
	fn parse_expr_array_literal(&mut self) -> JsResult<Expr> {
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
	
	fn parse_expr_object_literal(&mut self) -> JsResult<Expr> {
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
	
	fn parse_expr_object_literal_prop(&mut self) -> JsResult<Property> {
		if let Some(name) = try!(self.parse_ident_name()) {
			let prop_ident = try!(self.parse_ident_name());
			
			if self.consume(&Token::OpenParen) {
				if name == keywords::GET {
					try!(self.expect(&Token::CloseParen));
					
					let block = try!(self.parse_function_block(Vec::new()));
					
					let function = Box::new(Function {
						global: false,
						name: None,
						block: block
					});
					
					let function_ref = FunctionRef(self.context.functions.len() as u32);
					self.context.functions.push(function);
					
					Ok(Property::Getter(prop_ident, function_ref))
				} else if name == keywords::SET {
					let args = vec![try!(self.parse_name())];
					
					try!(self.expect(&Token::CloseParen));
					
					let block = try!(self.parse_function_block(args));
					
					let function = Box::new(Function {
						global: false,
						name: None,
						block: block
					});
					
					let function_ref = FunctionRef(self.context.functions.len() as u32);
					self.context.functions.push(function);
					
					Ok(Property::Setter(prop_ident, function_ref))
				} else {
					self.fatal("Property getter or setter must start with get or set")
				}
			} else {
				if prop_ident.is_some() {
					self.fatal("Unexpected identifier")
				} else {
					self.consume(&Token::Colon);
					
					let expr = try!(self.parse_expr());
					
					Ok(Property::Assignment(PropertyKey::Ident(name), Box::new(expr)))
				}
			}
		} else if let Some(&Token::Literal(..)) = self.peek() {
			let lit = try!(self.parse_lit());
			
			// Only strings and numbers are accepted as property keys.
			
			match *lit {
				Lit::Null | Lit::Boolean(..) | Lit::Regex(..) => return self.fatal("Expected property key literal"),
				Lit::String(..) | Lit::Integer(..) | Lit::Long(..) | Lit::Double(..) => {}
			}
			
			self.consume(&Token::Colon);
			
			let expr = try!(self.parse_expr());
			
			Ok(Property::Assignment(PropertyKey::Literal(lit), Box::new(expr)))
		} else {
			let message = { format!("Cannot parse object literal property, got {:?}", self.peek()) };
			
			self.fatal(&message)
		}
	}
	
	fn parse_if(&mut self) -> JsResult<Item> {
		self.bump();
		
		try!(self.expect(&Token::OpenParen));
		
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect(&Token::CloseParen));
		
		let then = try!(self.parse_stmt(None));
		
		let else_ = if self.consume(&Token::Else) {
			Some(Box::new(try!(self.parse_stmt(None))))
		} else {
			None
		};
		
		Ok(Item::If(expr, Box::new(then), else_))
	}
	
	fn parse_expr_seq(&mut self) -> JsResult<ExprSeq> {
		let mut exprs = Vec::new();
		
		exprs.push(try!(self.parse_expr()));
		
		while self.consume(&Token::Comma) {
			exprs.push(try!(self.parse_expr()));
		}
		
		Ok(ExprSeq {
			exprs: exprs
		})
	}
	
	fn parse_do(&mut self, label: Option<Label>) -> JsResult<Item> {
		self.bump();
		
		let stmt = try!(self.parse_stmt(None));
		
		try!(self.expect(&Token::While));
		try!(self.expect(&Token::OpenParen));
		
		let expr = try!(self.parse_expr());
		
		try!(self.expect(&Token::CloseParen));
		try!(self.expect_eos());
		
		Ok(Item::Do(label, Box::new(expr), Box::new(stmt)))
	}
	
	fn parse_while(&mut self, label: Option<Label>) -> JsResult<Item> {
		self.bump();
		
		try!(self.expect(&Token::OpenParen));
		
		let expr = try!(self.parse_expr());
		
		try!(self.expect(&Token::CloseParen));
		
		let stmt = try!(self.parse_stmt(None));
		
		Ok(Item::While(label, Box::new(expr), Box::new(stmt)))
	}
	
	fn parse_for(&mut self, label: Option<Label>) -> JsResult<Item> {
		self.bump();
		
		try!(self.expect(&Token::OpenParen));
		
		if self.consume(&Token::Var) {
			// Either ForVar or ForVarIn.
			
			let vars = try!(self.parse_var_decl());
			
			match *self.next() {
				Token::SemiColon => {
					let (test, incr, stmt) = try!(self.parse_for_tail());
					
					Ok(Item::ForVar(label, Some(vars), test, incr, Box::new(stmt)))
				},
				Token::In => {
					// A ForVarIn can only have a single var decl.
					
					if vars.len() != 1 {
						return self.fatal("For var in must have a single variable declaration");
					}
					let var = vars.single();
					if var.expr.is_some() {
						return self.fatal("Invalid variable declaration in for var in");
					}
					
					let expr = try!(self.parse_expr_seq());
					
					try!(self.expect(&Token::CloseParen));
					
					let stmt = try!(self.parse_stmt(None));
					
					Ok(Item::ForVarIn(label, var.ident, expr, Box::new(stmt)))
				},
				_ => self.fatal("Cannot parse for var")
			}
		} else {
			// Either For or ForIn.
			
			if self.consume(&Token::SemiColon) {
				// A For without an initial expression.
				
				let (test, incr, stmt) = try!(self.parse_for_tail());
				
				Ok(Item::For(label, None, test, incr, Box::new(stmt)))
			} else {
				let expr = try!(self.parse_expr_seq());
				
				// If the expression sequence is followed by a semi colon, we have a For.
				// Otherwise we have a ForIn.
				
				if self.consume(&Token::SemiColon) {
					let (test, incr, stmt) = try!(self.parse_for_tail());
					
					Ok(Item::For(label, Some(expr), test, incr, Box::new(stmt)))
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
							
							let stmt = try!(self.parse_stmt(None));
							
							let in_ = ExprSeq { exprs: vec![*right] };
							
							return Ok(Item::ForIn(label, left, in_, Box::new(stmt)));
						}
					}
					
					self.fatal("Expected in expression")
				}
			}
		}
	}
	
	fn parse_for_tail(&mut self) -> JsResult<(Option<ExprSeq>, Option<ExprSeq>, Item)> {
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
		
		let stmt = try!(self.parse_stmt(None));
		
		Ok((test, incr, stmt))
	}
	
	fn parse_continue(&mut self) -> JsResult<Item> {
		self.bump();
		
		let label = if let Some(name) = try!(self.parse_opt_name()) {
			Some(Label {
				name: name
			})
		} else {
			None
		};
		
		try!(self.expect_eos());
		
		Ok(Item::Continue(label))
	}
	
	fn parse_break(&mut self) -> JsResult<Item> {
		self.bump();
		
		let label = if let Some(name) = try!(self.parse_opt_name()) {
			Some(Label {
				name: name
			})
		} else {
			None
		};
		
		try!(self.expect_eos());
		
		Ok(Item::Break(label))
	}
	
	fn parse_return(&mut self) -> JsResult<Item> {
		self.bump();
		
		let expr = if self.is_eos() {
			None
		} else {
			Some(try!(self.parse_expr_seq()))
		};
		
		try!(self.expect_eos());
		
		Ok(Item::Return(expr))
	}
	
	fn parse_with(&mut self) -> JsResult<Item> {
		self.bump();
		
		try!(self.expect(&Token::OpenParen));
		
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect(&Token::CloseParen));
		
		let stmt = try!(self.parse_stmt(None));
		
		Ok(Item::With(expr, Box::new(stmt)))
	}
	
	fn parse_switch(&mut self, label: Option<Label>) -> JsResult<Item> {
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
		
		Ok(Item::Switch(label, expr, cases))
	}
	
	fn parse_throw(&mut self) -> JsResult<Item> {
		self.bump();
		
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect_eos());
		
		Ok(Item::Throw(expr))
	}
	
	fn parse_try(&mut self) -> JsResult<Item> {
		self.bump();
		
		let try = try!(self.parse_block());
		
		let catch = if self.consume(&Token::Catch) {
			self.push_block_scope();
			
			try!(self.expect(&Token::OpenParen));
			
			let ident = try!(self.parse_ident());
			
			self.register_local(ident.name, true);
			
			try!(self.expect(&Token::CloseParen));
			
			let mut catch = try!(self.parse_block());
			catch.locals = self.pop_block_scope();
			
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
	
	fn parse_debugger(&mut self) -> JsResult<Item> {
		self.bump();
		
		try!(self.expect_eos());
		
		Ok(Item::Debugger)
	}
	
	fn parse_labelled(&mut self) -> JsResult<Item> {
		let label = Label {
			name: try!(self.parse_name())
		};
		
		self.bump();
		
		self.parse_stmt(Some(label))
	}
	
	fn parse_expr_stmt(&mut self) -> JsResult<Item> {
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect_eos());
		
		Ok(Item::ExprStmt(expr))
	}
}
