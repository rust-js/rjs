use syntax::lexer::Lexer;
use syntax::Name;
use syntax::ast::*;
use syntax::token::{Token, Lit};
use syntax::Span;
use syntax::token::name;
use ::{JsResult, JsError};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use util::iter::*;
use util::interner::StrInterner;

mod locals;

pub struct Parser<'a> {
	lexer: &'a mut Lexer<'a>,
	context: &'a mut AstContext,
	interner: &'a StrInterner,
	scopes: Vec<Scope>
}

struct Scope {
	slots: Vec<Slot>,
	blocks: Vec<BlockScope>,
	has_arguments: bool,
	deopt: bool
}

struct BlockScope {
	locals: HashMap<Name, SlotRef>
}

impl RootBlock {
	fn new(stmts: Vec<Item>, args: Vec<Name>, strict: bool, scope: Scope, locals: HashMap<Name, SlotRef>) -> RootBlock {
		// When the scope is deopt, the scope builds a tick scope. This implies the scope taking
		// a scope (which is a thick scope too).
		
		RootBlock {
			block: Block {
				stmts: stmts,
				locals: locals
			},
			args: args,
			strict: strict,
			state: RefCell::new(RootBlockState {
				take_scope: scope.deopt,
				build_scope: if scope.deopt { ScopeType::Thick } else { ScopeType::None },
				slots: scope.slots
			})
		}
	}
}

impl<'a> Parser<'a> {
	fn at_global(&self) -> bool {
		self.scopes.len() == 1 && self.scopes[0].blocks.len() == 1
	}
	
	fn push_scope(&mut self) {
		self.scopes.push(Scope {
			slots: Vec::new(),
			blocks: Vec::new(),
			has_arguments: false,
			deopt: false
		})
	}
	
	fn top_scope(&mut self) -> &mut Scope {
		let len = self.scopes.len();
		&mut self.scopes[len - 1]
	}
	
	fn pop_scope(&mut self) -> Scope {
		self.scopes.pop().unwrap()
	}
	
	fn push_block_scope(&mut self) {
		self.top_scope().blocks.push(BlockScope {
			locals: HashMap::new()
		});
	}
	
	fn pop_block_scope(&mut self) -> BlockScope {
		self.top_scope().blocks.pop().unwrap()
	}
	
	fn register_local(&mut self, name: Name, throwarg: bool, global: bool) {
		// TODO: Validate performance. We expect that most locals set will be relatively small.
		// Changing this into a HashSet will very likely not make sense. However, this will
		// give issues in functions/scopes with many globals (probably more than 20 or 30).
		// An alternative is to leave duplicates in here and let the IR builder take care
		// of deduplicating then. It's already pushing the locals into a HashMap so that isn't
		// a problem.
				
		let scope = self.top_scope();
		
		let block = if name == name::ARGUMENTS {
			&mut scope.blocks[0]
		} else if throwarg {
			let len = scope.blocks.len();
			&mut scope.blocks[len - 1]
		} else {
			// Has this local already been declared?
			
			for block in &scope.blocks {
				if block.locals.contains_key(&name) {
					return;
				}
			}
			
			&mut scope.blocks[0]
		};
		
		// We need to declare a new variable. Create a slot for it.
		
		let slot = Slot {
			name: name,
			arg: None,
			state: if global { SlotState::Scoped } else { SlotState::Local }
		};
		
		let local_slot_ref = SlotRef(scope.slots.len());
		scope.slots.push(slot);

		// Register the local.
		
		block.locals.insert(name, local_slot_ref);
	}
	
	fn is_eof(&mut self) -> JsResult<bool> {
		self.lexer.is_eof()
	}
	
	fn peek(&mut self) -> JsResult<Option<Token>> {
		self.peek_at(0)
	}
	
	fn peek_at(&mut self, index: usize) -> JsResult<Option<Token>> {
		match try!(self.lexer.peek(index)) {
			None => Ok(None),
			Some(token) => Ok(Some(token.token))
		}
	}
	
	fn peek_any(&mut self) -> JsResult<Option<Token>> {
		self.peek_any_at(0)
	}
	
	fn peek_any_at(&mut self, index: usize) -> JsResult<Option<Token>> {
		match try!(self.lexer.peek_any(index)) {
			None => Ok(None),
			Some(token) => Ok(Some(token.token))
		}
	}
	
	fn next(&mut self) -> JsResult<Token> {
		Ok(try!(self.lexer.next()).token)
	}
	
	fn bump(&mut self) -> JsResult<()> {
		try!(self.lexer.bump());
		
		Ok(())
	}
	
	fn bump_any(&mut self) -> JsResult<()> {
		try!(self.lexer.bump_any());
		
		Ok(())
	}
	
	fn expect(&mut self, token: Token) -> JsResult<()> {
		let message = {
			let next = try!(self.next());
			
			if next == token {
				return Ok(());
			}
			
			format!("Unexpected token; expected {:?}, got {:?}", token, next)
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
		let span = match try!(self.lexer.peek(0)) {
			Some(token) => token.span,
			_ => Span::new(-1, -1, -1, -1, self.interner.intern(""))
		};
		
		Err(JsError::Parse(format!("{}:{}: {}", span.start_line, span.start_col, message.to_string())))
	}
	
	fn expect_eos(&mut self) -> JsResult<()> {
		// Find a valid end of statement. A valid end of statement is:
		//
		//   * A semi colon or newline, which is consumed;
		//   * A close brace, which is not consumed; or
		//   * The end of the file.
		//
		
		loop {
			let matched = match try!(self.peek_any()) {
				None => return Ok(()),
				Some(Token::SemiColon) | Some(Token::Newline) => Some(true),
				Some(Token::CloseBrace) => return Ok(()),
				Some(ref t) if t.is_hidden() => Some(false),
				_ => None
			};
			
			if !matched.is_some() {
				return self.fatal("Expected EOS");
			}
				
			try!(self.bump_any());
			
			if matched.unwrap() {
				return Ok(());
			}
		}
	}
	
	fn is_eos(&mut self) -> JsResult<bool> {
		self.is_eos_at(0)
	}
	
	fn is_eos_at(&mut self, offset: usize) -> JsResult<bool> {
		// Find a valid end of statement. A valid end of statement is:
		//
		//   * A semi colon or newline, which is consumed;
		//   * A close brace, which is not consumed; or
		//   * The end of the file.
		//
		
		let mut offset = offset;
		let mut index = 0;
		
		while offset > 0 {
			match try!(self.peek_any_at(index)) {
				None => return Ok(true),
				Some(token) if !token.is_hidden() => offset -= 1,
				_ => {}
			}
			
			index += 1;
		}
		
		loop {
			match try!(self.peek_any_at(index)) {
				None | Some(Token::SemiColon) | Some(Token::Newline) | Some(Token::CloseBrace) => return Ok(true),
				Some(ref t) if t.is_hidden() => {},
				_ => return Ok(false)
			}
				
			index += 1;
		}
	}
	
	pub fn parse_program(context: &'a mut AstContext, lexer: &'a mut Lexer<'a>, interner: &'a StrInterner) -> JsResult<FunctionRef> {
		let mut parser = Parser {
			lexer: lexer,
			context: context,
			scopes: Vec::new(),
			interner: interner
		};
		
		parser.push_scope();
		parser.push_block_scope();
		
		let mut items = Vec::new();
		
		let start = try!(parser.lexer.span());
		
		try!(parser.parse_strict());
		
		while !try!(parser.is_eof()) {
			items.push(try!(parser.parse_stmt(None)));
		}
		
		// If the last statement is an expression, turn it into a return.
		
		let len = items.len();
		if len > 0 {
			if let Item::ExprStmt(..) = items[len - 1] {
				if let Item::ExprStmt(exprs) = items.pop().unwrap() {
					items.push(Item::Return(Some(exprs)));
				}
			}
		}
		
		let end = parser.lexer.last_span().unwrap_or(start);
		
		let block = parser.pop_block_scope();
		let scope = parser.pop_scope();
		
		let program = Box::new(Function {
			name: None,
			global: true,
			block: RootBlock::new(items, Vec::new(), parser.lexer.strict(), scope, block.locals),
			args: 0,
			span: Span::from_range(start, end)
		});
		
		let program_ref = FunctionRef(parser.context.functions.len() as u32);
		parser.context.functions.push(program);
		
		try!(locals::LocalResolver::resolve(parser.context, program_ref));
		
		Ok(program_ref)
	}
	
	fn parse_strict(&mut self) -> JsResult<bool> {
		let was_strict = self.lexer.strict();
		
		let mut strict = false;
		let mut offset = 0;
		
		while let Some(Token::Literal(Lit::String(name, exact))) = try!(self.peek_at(offset)) {
			if !try!(self.is_eos_at(offset + 1)) {
				break;
			}
			
			offset += 1;
			
			if try!(self.peek_at(offset)) == Some(Token::SemiColon) {
				offset += 1;
			}
			
			if name == name::USE_STRICT && exact {
				strict = true;
			}
		}
		
		if strict {
			self.lexer.set_strict(true);
		}
		
		Ok(was_strict)
	}
	
	fn parse_function(&mut self) -> JsResult<FunctionRef> {
		let start = self.lexer.last_span().unwrap();
		
		let name = try!(self.parse_opt_name());
		
		let args = try!(self.parse_parameter_list());
		let arg_count = args.len();
		
		let block = try!(self.parse_function_block(args));
		
		let end = self.lexer.last_span().unwrap();
		
		let function = Box::new(Function {
			global: false,
			name: name,
			block: block,
			args: arg_count as u32,
			span: Span::from_range(start, end)
		});
		
		let function_ref = FunctionRef(self.context.functions.len() as u32);
		self.context.functions.push(function);
		
		Ok(function_ref)
	}
	
	fn parse_function_block(&mut self, args: Vec<Name>) -> JsResult<RootBlock> {
		self.push_scope();
		
		try!(self.expect(Token::OpenBrace));
		
		let was_strict = try!(self.parse_strict());
		let strict = self.lexer.strict();
		
		let mut stmts = Vec::new();
		
		self.push_block_scope();
		
		if args.len() > 0 {
			self.register_function_args(&args);
		}
		
		while !try!(self.consume(Token::CloseBrace)) {
			stmts.push(try!(self.parse_stmt(None)));
		}
		
		self.lexer.set_strict(was_strict);
		
		let block = self.pop_block_scope();
		let scope = self.pop_scope();
		
		Ok(RootBlock::new(stmts, args, strict, scope, block.locals))
	}
	
	fn register_function_args(&mut self, args: &Vec<Name>) {
		let scope = self.top_scope();
		
		for i in 0..args.len() {
			let name = args[i];
			
			// Create a slot for the parameter.
		
			let slot = Slot {
				name: name,
				arg: Some(i as u32),
				state: SlotState::Local
			};
			
			let local_slot_ref = SlotRef(scope.slots.len());
			scope.slots.push(slot);
	
			// Register the parameter.
			
			scope.blocks[0].locals.insert(name, local_slot_ref);
		}
	}
	
	fn parse_parameter_list(&mut self) -> JsResult<Vec<Name>> {
		try!(self.expect(Token::OpenParen));
		
		if try!(self.consume(Token::CloseParen)) {
			return Ok(Vec::new());
		}
		
		let mut args = Vec::new();
		
		while !try!(self.is_eof()) {
			args.push(try!(self.parse_name()));
			
			match try!(self.next()) {
				Token::Comma => continue,
				Token::CloseParen => return Ok(args),
				_ => break
			}
		}
		
		self.fatal("Cannot parse parameter list")
	}
	
	fn parse_name(&mut self) -> JsResult<Name> {
		if let Token::Identifier(name) = try!(self.next()) {
			Ok(name)
		} else {
			self.fatal("Expected identifier")
		}
	}
	
	fn parse_opt_name(&mut self) -> JsResult<Option<Name>> {
		let name = match try!(self.peek()) {
			Some(Token::Identifier(name)) => name,
			_ => return Ok(None)
		};
		
		try!(self.next());
		
		Ok(Some(name))
	}
	
	fn parse_ident(&mut self) -> JsResult<Ident> {
		let name = try!(self.parse_name());
		
		if name == name::ARGUMENTS && !self.top_scope().has_arguments {
			self.top_scope().has_arguments = true;
			
			let at_global = self.at_global();
			self.register_local(name, false, at_global);
		}
		
		Ok(Ident {
			name: name,
			state: Cell::new(IdentState::None)
		})
	}
	
	fn parse_ident_name(&mut self) -> JsResult<Option<Name>> {
		// This is a special version of parse_ident that also accepts keywords.
		
		let name = match try!(self.peek()) {
			Some(Token::Identifier(name)) => name,
			Some(Token::Literal(value)) => {
				match value {
					Lit::Boolean(value) => if value { name::TRUE } else { name::FALSE },
					Lit::Null => name::NULL,
					_ => return Ok(None)
				}
			},
			// Reserved words
			Some(Token::Break) => name::BREAK,
			Some(Token::Do) => name::DO,
			Some(Token::Instanceof) => name::INSTANCEOF,
			Some(Token::Typeof) => name::TYPEOF,
			Some(Token::Case) => name::CASE,
			Some(Token::Else) => name::ELSE,
			Some(Token::New) => name::NEW,
			Some(Token::Var) => name::VAR,
			Some(Token::Catch) => name::CATCH,
			Some(Token::Finally) => name::FINALLY,
			Some(Token::Return) => name::RETURN,
			Some(Token::Void) => name::VOID,
			Some(Token::Continue) => name::CONTINUE,
			Some(Token::For) => name::FOR,
			Some(Token::Switch) => name::SWITCH,
			Some(Token::While) => name::WHILE,
			Some(Token::Debugger) => name::DEBUGGER,
			Some(Token::Function) => name::FUNCTION,
			Some(Token::This) => name::THIS,
			Some(Token::With) => name::WHILE,
			Some(Token::Default) => name::DEFAULT,
			Some(Token::If) => name::IF,
			Some(Token::Throw) => name::THROW,
			Some(Token::Delete) => name::DELETE,
			Some(Token::In) => name::IN,
			Some(Token::Try) => name::TRY,
			// Future reserved words
			Some(Token::Class) => name::CLASS,
			Some(Token::Enum) => name::ENUM,
			Some(Token::Extends) => name::EXTENDS,
			Some(Token::Super) => name::SUPER,
			Some(Token::Const) => name::CONST,
			Some(Token::Export) => name::EXPORT,
			Some(Token::Import) => name::IMPORT,
			Some(Token::Implements) => name::IMPLEMENTS,
			Some(Token::Let) => name::LET,
			Some(Token::Private) => name::PRIVATE,
			Some(Token::Public) => name::PUBLIC,
			Some(Token::Interface) => name::INTERFACE,
			Some(Token::Package) => name::PACKAGE,
			Some(Token::Protected) => name::PROTECTED,
			Some(Token::Static) => name::STATIC,
			Some(Token::Yield) => name::YIELD,
			_ => return Ok(None)
		};
		
		try!(self.bump());
		
		Ok(Some(name))
	}
	
	fn parse_lit(&mut self) -> JsResult<Option<Lit>> {
		self.lexer.set_allow_regexp(true);
		
		let token = try!(self.peek());
		
		self.lexer.set_allow_regexp(false);
		
		if let Some(Token::Literal(lit)) = token {
			try!(self.bump());
			
			Ok(Some(lit))
		} else {
			Ok(None)
		}
	}
	
	fn parse_block(&mut self) -> JsResult<Block> {
		try!(self.expect(Token::OpenBrace));
		
		let stmts = try!(self.parse_stmt_list());
		
		try!(self.expect(Token::CloseBrace));
		
		Ok(Block {
			stmts: stmts,
			locals: HashMap::new()
		})
	}
	
	fn parse_stmt(&mut self, label: Option<Label>) -> JsResult<Item> {
		if let Some(Token::Function) = try!(self.peek()) {
			if let Some(Token::Identifier(..)) = try!(self.peek_at(1)) {
				try!(self.bump());
				
				let function_ref = try!(self.parse_function());
				let name = self.context.functions[function_ref.usize()].name.unwrap();
				
				let at_global = self.at_global();
				self.register_local(name, false, at_global);
				
				let ident = Ident {
					name: name,
					state: Cell::new(IdentState::None)
				};
				
				return Ok(Item::Function(ident, function_ref));
			}
		}
		
		match try!(self.peek()) {
			Some(Token::OpenBrace) => Ok(Item::Block(label, try!(self.parse_block()))),
			Some(Token::Var) => self.parse_var_stmt(),
			Some(Token::SemiColon) => {
				try!(self.bump());
				Ok(Item::Empty)
			},
			Some(Token::If) => self.parse_if(),
			Some(Token::Do) => self.parse_do(label),
			Some(Token::While) => self.parse_while(label),
			Some(Token::For) => self.parse_for(label),
			Some(Token::Continue) => self.parse_continue(),
			Some(Token::Break) => self.parse_break(),
			Some(Token::Return) => self.parse_return(),
			Some(Token::With) => self.parse_with(),
			Some(Token::Switch) => self.parse_switch(label),
			Some(Token::Throw) => self.parse_throw(),
			Some(Token::Try) => self.parse_try(),
			Some(Token::Debugger) => self.parse_debugger(),
			_ => {
				if let Some(Token::Identifier(..)) = try!(self.peek()) {
					if let Some(Token::Colon) = try!(self.peek_at(1)) {
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
			
			match try!(self.peek()) {
				None | Some(Token::CloseBrace) | Some(Token::Case) | Some(Token::Default) => return Ok(stmts),
				_ => {}
			}
			
			stmts.push(try!(self.parse_stmt(None)));
		}
	}
	
	fn parse_var_stmt(&mut self) -> JsResult<Item> {
		try!(self.bump());
		
		let var_decl = try!(self.parse_var_decl());
		
		try!(self.expect_eos());
		
		Ok(Item::VarDecl(var_decl))
	}
	
	fn parse_var_decl(&mut self) -> JsResult<Vec<Var>> {
		let mut vars = Vec::new();
		
		while !try!(self.is_eof()) {
			let ident = try!(self.parse_ident());
			
			let at_global = self.at_global();
			self.register_local(ident.name, false, at_global);
			
			let expr = if try!(self.consume(Token::Assign)) {
				Some(Box::new(try!(self.parse_expr())))
			} else {
				None
			};
			
			vars.push(Var {
				ident: ident,
				expr: expr
			});
			
			if !try!(self.consume(Token::Comma)) {
				return Ok(vars);
			}
		}
		
		self.fatal("Cannot parse variable declaration")
	}
	
	fn parse_expr(&mut self) -> JsResult<Expr> {
		let expr = try!(self.parse_expr_binary());
		
		match try!(self.peek()) {
			Some(Token::QuestionMark) => self.parse_expr_ternary(expr),
			Some(Token::Assign) => self.parse_expr_binary_assign(expr, Op::Assign),
			Some(Token::MultiplyAssign) => self.parse_expr_binary_assign(expr, Op::Multiply),
			Some(Token::DivideAssign) => self.parse_expr_binary_assign(expr, Op::Divide),
			Some(Token::ModulusAssign) => self.parse_expr_binary_assign(expr, Op::Modulus),
			Some(Token::PlusAssign) => self.parse_expr_binary_assign(expr, Op::Add),
			Some(Token::MinusAssign) => self.parse_expr_binary_assign(expr, Op::Subtract),
			Some(Token::LeftShiftArithmeticAssign) => self.parse_expr_binary_assign(expr, Op::LeftShiftArithmetic),
			Some(Token::RightShiftArithmeticAssign) => self.parse_expr_binary_assign(expr, Op::RightShiftArithmetic),
			Some(Token::RightShiftLogicalAssign) => self.parse_expr_binary_assign(expr, Op::RightShiftLogical),
			Some(Token::BitAndAssign) => self.parse_expr_binary_assign(expr, Op::BitAnd),
			Some(Token::BitXOrAssign) => self.parse_expr_binary_assign(expr, Op::BitXOr),
			Some(Token::BitOrAssign) => self.parse_expr_binary_assign(expr, Op::BitOr),
			_ => return Ok(expr)
		}
	}
	
	fn parse_expr_binary(&mut self) -> JsResult<Expr> {
		let mut expr = try!(self.parse_expr_unary());
		
		loop {
			let op = match try!(self.peek()) {
				Some(Token::Multiply) => Op::Multiply,
				Some(Token::Divide) => Op::Divide,
				Some(Token::Modulus) => Op::Modulus,
				Some(Token::Plus) => Op::Add,
				Some(Token::Minus) => Op::Subtract,
				Some(Token::LeftShiftArithmetic) => Op::LeftShiftArithmetic,
				Some(Token::RightShiftArithmetic) => Op::RightShiftArithmetic,
				Some(Token::RightShiftLogical) => Op::RightShiftLogical,
				Some(Token::LessThan) => Op::LessThan,
				Some(Token::GreaterThan) => Op::GreaterThan,
				Some(Token::LessThanEquals) => Op::LessThanEquals,
				Some(Token::GreaterThanEquals) => Op::GreaterThanEquals,
				Some(Token::Instanceof) => Op::InstanceOf,
				Some(Token::In) => Op::In,
				Some(Token::Equals) => Op::Equals,
				Some(Token::NotEquals) => Op::NotEquals,
				Some(Token::IdentityEquals) => Op::IdentityEquals,
				Some(Token::IdentityNotEquals) => Op::IdentityNotEquals,
				Some(Token::BitAnd) => Op::BitAnd,
				Some(Token::BitXOr) => Op::BitXOr,
				Some(Token::BitOr) => Op::BitOr,
				Some(Token::And) => Op::And,
				Some(Token::Or) => Op::Or,
				_ => break
			};
			
			try!(self.bump());
			
			let right = try!(self.parse_expr_unary());
			
			let rebalance = if let Expr::Binary(lop, _, _) = expr {
				lop.precedence() < op.precedence()
			} else {
				false
			};
			
			expr = if rebalance {
				if let Expr::Binary(lop, lleft, lright) = expr {
					Expr::Binary(
						lop,
						lleft,
						Box::new(Expr::Binary(
							op,
							lright,
							Box::new(right)
						))
					)
				} else {
					unreachable!();
				}
			} else {
				Expr::Binary(op, Box::new(expr), Box::new(right))
			};
		}
		
		Ok(expr)
	}
	
	fn parse_expr_unary(&mut self) -> JsResult<Expr> {
		let mut expr = try!(match try!(self.peek()) {
			Some(Token::Function) => {
				try!(self.bump());
				
				Ok(Expr::Function(try!(self.parse_function())))
			},
			Some(Token::New) => self.parse_expr_new(),
			Some(Token::Delete) => self.parse_expr_unary_pre(Op::Delete),
			Some(Token::Void) => self.parse_expr_unary_pre(Op::Void),
			Some(Token::Typeof) => self.parse_expr_unary_pre(Op::Typeof),
			Some(Token::This) => {
				try!(self.bump());
				Ok(Expr::This)
			},
			Some(Token::PlusPlus) => self.parse_expr_unary_pre(Op::PreIncr),
			Some(Token::MinusMinus) => self.parse_expr_unary_pre(Op::PreDecr),
			Some(Token::Plus) => self.parse_expr_unary_pre(Op::Positive),
			Some(Token::Minus) => self.parse_expr_unary_pre(Op::Negative),
			Some(Token::BitNot) => self.parse_expr_unary_pre(Op::BitNot),
			Some(Token::Not) => self.parse_expr_unary_pre(Op::Not),
			Some(Token::Identifier(..)) => self.parse_expr_ident(),
			Some(Token::Literal(..)) => self.parse_expr_literal(),
			Some(Token::OpenParen) => self.parse_expr_paren(),
			Some(Token::OpenBracket) => self.parse_expr_array_literal(),
			Some(Token::OpenBrace) => self.parse_expr_object_literal(),
			_ => {
				let message = { format!("Cannot parse expression, got {:?}", self.peek()) };
				
				return self.fatal(&message);
			}
		});
		
		loop {
			let is_eos = try!(self.is_eos());
			
			expr = try!(match try!(self.peek()) {
				Some(Token::OpenBracket) => self.parse_expr_member_index(expr),
				Some(Token::Dot) => self.parse_expr_member_dot(expr),
				Some(Token::OpenParen) => self.parse_expr_call(expr),
				Some(Token::PlusPlus) if !is_eos => self.parse_expr_unary_post(expr, Op::PostIncr),
				Some(Token::MinusMinus) if !is_eos => self.parse_expr_unary_post(expr, Op::PostDecr),
				_ => return Ok(expr)
			});
		}
	}
	
	fn parse_expr_member_index(&mut self, expr: Expr) -> JsResult<Expr> {
		try!(self.bump());
		
		let index = try!(self.parse_expr_seq());
		
		try!(self.expect(Token::CloseBracket));
		
		Ok(Expr::MemberIndex(Box::new(expr), index))
	}
	
	fn parse_expr_member_dot(&mut self, expr: Expr) -> JsResult<Expr> {
		try!(self.bump());
		
		if let Some(name) = try!(self.parse_ident_name()) {
			Ok(Expr::MemberDot(Box::new(expr), name))
		} else {
			self.fatal("Expected identifier name")
		}
	}
	
	fn parse_expr_call(&mut self, expr: Expr) -> JsResult<Expr> {
		let args = try!(self.parse_arguments());
		
		if let Expr::Ident(Ident { name, .. }) = expr {
			if name == name::EVAL {
				self.mark_deopt();
			}
		}
		
		Ok(Expr::Call(Box::new(expr), args))
	}
	
	fn mark_deopt(&mut self) {
		for scope in &mut self.scopes {
			scope.deopt = true;
		}
	}
	
	fn parse_expr_binary_assign(&mut self, expr: Expr, op: Op) -> JsResult<Expr> {
		try!(self.bump());
		
		let right = try!(self.parse_expr());
		
		Ok(Expr::Assign(op, Box::new(expr), Box::new(right)))
	}
	
	fn parse_expr_ternary(&mut self, expr: Expr) -> JsResult<Expr> {
		try!(self.bump());
		
		let then = try!(self.parse_expr());
		
		try!(self.expect(Token::Colon));
		
		let else_ = try!(self.parse_expr());
		
		Ok(Expr::Ternary(Box::new(expr), Box::new(then), Box::new(else_)))
	}
	
	fn parse_expr_new(&mut self) -> JsResult<Expr> {
		try!(self.bump());
		
		Ok(Expr::New(Box::new(try!(self.parse_expr_unary()))))
	}
	
	fn parse_expr_unary_pre(&mut self, op: Op) -> JsResult<Expr> {
		try!(self.bump());
		
		let expr = try!(self.parse_expr_unary());
		
		Ok(Expr::Unary(op, Box::new(expr)))
	}
	
	fn parse_expr_unary_post(&mut self, expr: Expr, op: Op) -> JsResult<Expr> {
		try!(self.bump());
		
		Ok(Expr::Unary(op, Box::new(expr)))
	}
	
	fn parse_arguments(&mut self) -> JsResult<Vec<Expr>> {
		try!(self.expect(Token::OpenParen));
		
		if try!(self.consume(Token::CloseParen)) {
			return Ok(Vec::new());
		}
		
		let mut args = Vec::new();
		
		loop {
			args.push(try!(self.parse_expr()));
			
			if try!(self.consume(Token::CloseParen)) {
				return Ok(args);
			}
			
			try!(self.expect(Token::Comma));
		}
	}
	
	fn parse_expr_ident(&mut self) -> JsResult<Expr> {
		Ok(Expr::Ident(try!(self.parse_ident())))
	}
	
	fn parse_expr_literal(&mut self) -> JsResult<Expr> {
		if let Token::Literal(lit) = try!(self.next()) {
			return Ok(Expr::Literal(lit));
		}
		
		self.fatal("Expected literal")
	}
	
	fn parse_expr_paren(&mut self) -> JsResult<Expr> {
		try!(self.bump());
		
		let exprs = try!(self.parse_expr_seq());
		
		try!(self.expect(Token::CloseParen));
		
		Ok(Expr::Paren(exprs))
	}
	
	fn parse_expr_array_literal(&mut self) -> JsResult<Expr> {
		try!(self.bump());
		
		let mut elems = Vec::new();
		
		if !try!(self.consume(Token::CloseBracket)) {
			loop {
				// After an element is parsed, we check for a CloseBracket or Comma. This means
				// that when we find a CloseBracket or Comma here, it means that we have
				// an elision.
				
				match try!(self.peek()) {
					Some(Token::CloseBracket) => {
						try!(self.bump());
						elems.push(Expr::Missing);
						break;
					},
					Some(Token::Comma) => {
						try!(self.bump());
						elems.push(Expr::Missing);
						continue;
					},
					_ => {
						elems.push(try!(self.parse_expr()));
						
						if try!(self.consume(Token::CloseBracket)) {
							break;
						}
						try!(self.expect(Token::Comma));
					}
				}
			}
		}
	
		return Ok(Expr::ArrayLiteral(elems));
	}
	
	fn parse_expr_object_literal(&mut self) -> JsResult<Expr> {
		try!(self.bump());
		
		let mut props = Vec::new();
		
		// First try to match an empty object literal with a comma, i.e. "{,}".
		
		if !try!(self.consume(Token::CloseBrace)) {
			if try!(self.consume(Token::Comma)) {
				try!(self.expect(Token::CloseBrace));
			} else {
				loop {
					props.push(try!(self.parse_expr_object_literal_prop()));
					
					// "}" or ",}" closes an object literal.
					
					if try!(self.consume(Token::CloseBrace)) {
						break;
					}
					try!(self.expect(Token::Comma));
					if try!(self.consume(Token::CloseBrace)) {
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
			
			if try!(self.consume(Token::OpenParen)) {
				if name == name::GET {
					try!(self.expect(Token::CloseParen));
					
					let start = try!(self.lexer.span());
					
					let block = try!(self.parse_function_block(Vec::new()));
					
					let end = self.lexer.last_span().unwrap();
					
					let function = Box::new(Function {
						global: false,
						name: None,
						block: block,
						args: 0,
						span: Span::from_range(start, end)
					});
					
					let function_ref = FunctionRef(self.context.functions.len() as u32);
					self.context.functions.push(function);
					
					Ok(Property::Getter(prop_ident, function_ref))
				} else if name == name::SET {
					let args = vec![try!(self.parse_name())];
					
					try!(self.expect(Token::CloseParen));
					
					let start = try!(self.lexer.span());
					
					let block = try!(self.parse_function_block(args));
					
					let end = self.lexer.last_span().unwrap();
					
					let function = Box::new(Function {
						global: false,
						name: None,
						block: block,
						args: 1,
						span: Span::from_range(start, end)
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
					try!(self.consume(Token::Colon));
					
					let expr = try!(self.parse_expr());
					
					Ok(Property::Assignment(PropertyKey::Ident(name), Box::new(expr)))
				}
			}
		} else if let Some(lit) = try!(self.parse_lit()) {
			// Only strings and numbers are accepted as property keys.
			
			match lit {
				Lit::Null | Lit::Boolean(..) | Lit::Regex(..) => return self.fatal("Expected property key literal"),
				Lit::String(..) | Lit::Integer(..) | Lit::Long(..) | Lit::Double(..) => {}
			}
			
			try!(self.consume(Token::Colon));
			
			let expr = try!(self.parse_expr());
			
			Ok(Property::Assignment(PropertyKey::Literal(lit), Box::new(expr)))
		} else {
			let message = { format!("Cannot parse object literal property, got {:?}", self.peek()) };
			
			self.fatal(&message)
		}
	}
	
	fn parse_if(&mut self) -> JsResult<Item> {
		try!(self.bump());
		
		try!(self.expect(Token::OpenParen));
		
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect(Token::CloseParen));
		
		let then = try!(self.parse_stmt(None));
		
		let else_ = if try!(self.consume(Token::Else)) {
			Some(Box::new(try!(self.parse_stmt(None))))
		} else {
			None
		};
		
		Ok(Item::If(expr, Box::new(then), else_))
	}
	
	fn parse_expr_seq(&mut self) -> JsResult<ExprSeq> {
		let mut exprs = Vec::new();
		
		exprs.push(try!(self.parse_expr()));
		
		while try!(self.consume(Token::Comma)) {
			exprs.push(try!(self.parse_expr()));
		}
		
		Ok(ExprSeq {
			exprs: exprs
		})
	}
	
	fn parse_do(&mut self, label: Option<Label>) -> JsResult<Item> {
		try!(self.bump());
		
		let stmt = try!(self.parse_stmt(None));
		
		try!(self.expect(Token::While));
		try!(self.expect(Token::OpenParen));
		
		let expr = try!(self.parse_expr());
		
		try!(self.expect(Token::CloseParen));
		try!(self.expect_eos());
		
		Ok(Item::Do(label, Box::new(expr), Box::new(stmt)))
	}
	
	fn parse_while(&mut self, label: Option<Label>) -> JsResult<Item> {
		try!(self.bump());
		
		try!(self.expect(Token::OpenParen));
		
		let expr = try!(self.parse_expr());
		
		try!(self.expect(Token::CloseParen));
		
		let stmt = try!(self.parse_stmt(None));
		
		Ok(Item::While(label, Box::new(expr), Box::new(stmt)))
	}
	
	fn parse_for(&mut self, label: Option<Label>) -> JsResult<Item> {
		try!(self.bump());
		
		try!(self.expect(Token::OpenParen));
		
		if try!(self.consume(Token::Var)) {
			// Either ForVar or ForVarIn.
			
			let vars = try!(self.parse_var_decl());
			
			match try!(self.next()) {
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
					
					try!(self.expect(Token::CloseParen));
					
					let stmt = try!(self.parse_stmt(None));
					
					Ok(Item::ForVarIn(label, var.ident, expr, Box::new(stmt)))
				},
				_ => self.fatal("Cannot parse for var")
			}
		} else {
			// Either For or ForIn.
			
			if try!(self.consume(Token::SemiColon)) {
				// A For without an initial expression.
				
				let (test, incr, stmt) = try!(self.parse_for_tail());
				
				Ok(Item::For(label, None, test, incr, Box::new(stmt)))
			} else {
				let expr = try!(self.parse_expr_seq());
				
				// If the expression sequence is followed by a semi colon, we have a For.
				// Otherwise we have a ForIn.
				
				if try!(self.consume(Token::SemiColon)) {
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
							try!(self.expect(Token::CloseParen));
							
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
		let test = if try!(self.consume(Token::SemiColon)) {
			None
		} else {
			let expr = try!(self.parse_expr_seq());
			
			try!(self.expect(Token::SemiColon));
			
			Some(expr)
		};
		
		let incr = if try!(self.consume(Token::CloseParen)) {
			None
		} else {
			let expr = try!(self.parse_expr_seq());
			
			try!(self.expect(Token::CloseParen));
			
			Some(expr)
		};
		
		let stmt = try!(self.parse_stmt(None));
		
		Ok((test, incr, stmt))
	}
	
	fn parse_continue(&mut self) -> JsResult<Item> {
		try!(self.bump());
		
		let label = if !try!(self.is_eos()) {
			if let Some(name) = try!(self.parse_opt_name()) {
				Some(Label {
					name: name
				})
			} else {
				None
			}
		} else {
			None
		};
		
		try!(self.expect_eos());
		
		Ok(Item::Continue(label))
	}
	
	fn parse_break(&mut self) -> JsResult<Item> {
		try!(self.bump());
		
		let label = if !try!(self.is_eos()) {
			if let Some(name) = try!(self.parse_opt_name()) {
				Some(Label {
					name: name
				})
			} else {
				None
			}
		} else {
			None
		};
		
		try!(self.expect_eos());
		
		Ok(Item::Break(label))
	}
	
	fn parse_return(&mut self) -> JsResult<Item> {
		try!(self.bump());
		
		let expr = if try!(self.is_eos()) {
			None
		} else {
			Some(try!(self.parse_expr_seq()))
		};
		
		try!(self.expect_eos());
		
		Ok(Item::Return(expr))
	}
	
	fn parse_with(&mut self) -> JsResult<Item> {
		if self.lexer.strict() {
			return self.fatal("With is not allowed in strict mode");
		}
		
		self.mark_deopt();
		
		try!(self.bump());
		
		try!(self.expect(Token::OpenParen));
		
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect(Token::CloseParen));
		
		let stmt = try!(self.parse_stmt(None));
		
		Ok(Item::With(expr, Box::new(stmt)))
	}
	
	fn parse_switch(&mut self, label: Option<Label>) -> JsResult<Item> {
		try!(self.bump());
		
		try!(self.expect(Token::OpenParen));
		
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect(Token::CloseParen));
		
		try!(self.expect(Token::OpenBrace));
		
		let mut cases: Vec<SwitchClause> = Vec::new();
		let mut have_default = false;
		
		loop {
			match try!(self.next()) {
				Token::CloseBrace => break,
				Token::Case => {
					let case_expr = try!(self.parse_expr_seq());
					
					try!(self.expect(Token::Colon));
					
					let stmts = try!(self.parse_stmt_list());
					
					cases.push(SwitchClause::Case(case_expr, stmts));
				},
				Token::Default => {
					try!(self.expect(Token::Colon));
					
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
		try!(self.bump());
		
		if try!(self.is_eos()) {
			Err(JsError::Parse("Illegal end of statement after throw".to_string()))
		} else {
			let expr = try!(self.parse_expr_seq());
			
			try!(self.expect_eos());
			
			Ok(Item::Throw(expr))
		}
	}
	
	fn parse_try(&mut self) -> JsResult<Item> {
		try!(self.bump());
		
		let try = try!(self.parse_block());
		
		let catch = if try!(self.consume(Token::Catch)) {
			self.push_block_scope();
			
			try!(self.expect(Token::OpenParen));
			
			let ident = try!(self.parse_ident());
			
			self.register_local(ident.name, true, false);
			
			try!(self.expect(Token::CloseParen));
			
			let mut catch = try!(self.parse_block());
			let block = self.pop_block_scope();
			catch.locals = block.locals;
			
			Some(Catch {
				ident: ident,
				block: catch
			})
		} else {
			None
		};
		
		let finally = if try!(self.consume(Token::Finally)) {
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
		try!(self.bump());
		
		try!(self.expect_eos());
		
		Ok(Item::Debugger)
	}
	
	fn parse_labelled(&mut self) -> JsResult<Item> {
		let label = Label {
			name: try!(self.parse_name())
		};
		
		try!(self.bump());
		
		self.parse_stmt(Some(label))
	}
	
	fn parse_expr_stmt(&mut self) -> JsResult<Item> {
		let expr = try!(self.parse_expr_seq());
		
		try!(self.expect_eos());
		
		Ok(Item::ExprStmt(expr))
	}
}
