pub mod builder;

use std::fs::File;
use syntax::{Name, Span};
use syntax::ast::*;
use syntax::token::Lit;
use syntax::ast::visitor::AstVisitor;
use self::builder::Ir;
use util::interner::StrInterner;
use syntax::token::name;
use syntax::lexer::Lexer;
use syntax::parser::{Parser, ParseMode};
use syntax::reader::StringReader;
use ::{JsResult, JsError};
use std::rc::Rc;
use std::fmt::Write;
use std::io::Read;

#[derive(Copy, Clone)]
struct NamedLabel {
	name: Option<Name>,
	label: builder::Label,
	try_catch_depth: i32
}

#[derive(Copy, Clone)]
struct ReturnTarget {
	local: builder::Local,
	label: builder::Label
}

pub struct IrContext {
	interner: StrInterner,
	ast: AstContext,
	functions: Vec<Option<Rc<builder::Block>>>,
	// TODO: Remove
	last_printed: usize
}

pub struct IrFunction {
	pub name: Option<Name>,
	pub args: u32,
	pub strict: bool,
	pub build_scope: ScopeType,
	pub take_scope: bool,
	pub span: Span
}

struct LhsRef<'a> {
	expr: &'a Expr
}

impl IrContext {
	pub fn new() -> IrContext {
		IrContext {
			interner: name::new_interner(),
			functions: Vec::new(),
			ast: AstContext::new(),
			last_printed: 0
		}
	}
	
	pub fn interner(&self) -> &StrInterner {
		&self.interner
	}
	
	pub fn parse_file(&mut self, file_name: &str, strict: bool) -> JsResult<FunctionRef> {
		let mut js = String::new();
		let mut file = match File::open(file_name) {
			Ok(ok) => ok,
			Err(err) => return Err(JsError::Io(err))
		};
		if let Err(err) = file.read_to_string(&mut js) {
			return Err(JsError::Io(err));
		}
		
		self.build_ir(&mut StringReader::new(file_name, &js), strict, ParseMode::Normal)
	}
	
	pub fn parse_string(&mut self, js: &str, strict: bool, mode: ParseMode) -> JsResult<FunctionRef> {
		self.build_ir(&mut StringReader::new("(global)", js), strict, mode)
	}
	
	pub fn get_function_ir(&mut self, function_ref: FunctionRef) -> JsResult<Rc<builder::Block>> {
		try!(self.build_function_ir(function_ref, ParseMode::Normal));
		
		Ok(self.functions[function_ref.usize()].as_ref().unwrap().clone())
	}
	
	fn build_ir(&mut self, reader: &mut StringReader, strict: bool, mode: ParseMode) -> JsResult<FunctionRef> {
		let offset = self.ast.functions.len();
		
		let program_ref = {
			let mut lexer = try!(Lexer::new(reader, &self.interner));
			
			if strict {
				lexer.set_strict(true);
			}
			
			try!(Parser::parse_program(&mut self.ast, &mut lexer, &self.interner, mode))
		};
		
		// Strict eval programs need to build a thick scope to isolate
		// their environment.
		
		// Append the new functions to our functions.
		
		for _ in offset..self.ast.functions.len() {
			self.functions.push(None);
		}
		
		try!(self.build_function_ir(program_ref, mode));
		
		Ok(program_ref)
	}
	
	fn build_function_ir(&mut self, function_ref: FunctionRef, mode: ParseMode) -> JsResult<()> {
		if self.functions[function_ref.usize()].is_some() {
			return Ok(());
		}

		let function = &self.ast.functions[function_ref.usize()];
		
		let block = {
			let block_state = &function.block.state.borrow();
			let mut generator = IrGenerator::new(self, block_state, mode);
			
			// Build the prolog to move the arguments into the scope and to
			// declare variables with undefined.
			
			for i in 0..block_state.slots.len() {
				let slot = &block_state.slots[i];
				if let Some(arg) = slot.arg {
					match slot.state {
						SlotState::Scoped => {
							generator.ir.emit(Ir::LoadEnvObject);
							generator.ir.emit(Ir::LoadParam(arg));
							generator.ir.emit(Ir::InitEnvName(slot.name));
						}
						SlotState::Lifted(index) => {
							generator.ir.emit(Ir::LoadParam(arg));
							generator.ir.emit(Ir::StoreLifted(index, 0));
						}
						_ => {}
					}
				} else if function.block.block.locals.contains_key(&slot.name) {
					let init = if slot.arguments {
						Ir::NewArguments
					} else {
						Ir::LoadUndefined
					};
					
					match slot.state {
						SlotState::Scoped => {
							if slot.arguments {
								generator.ir.emit(init);
								generator.ir.emit(Ir::StoreEnvArguments);
							} else {
								generator.ir.emit(Ir::LoadEnvObject);
								generator.ir.emit(init);
								generator.ir.emit(Ir::InitEnvName(slot.name));
							}
						}
						SlotState::Lifted(index) => {
							generator.ir.emit(init);
							generator.ir.emit(Ir::StoreLifted(index, 0));
						}
						SlotState::Local => {
							generator.ir.emit(init);
							generator.ir.emit(Ir::StoreLocal(generator.locals[i].unwrap()));
						}
					}
				}
			}
			
			generator.push_local_scope(&function.block.block);
			
			try!(generator.emit_functions(&function.block.block));
			
			try!(generator.emit_block(&function.block.block));
			
			// Build the return statement. We always have a return undefined
			// for when the function does not exit explicitly. If
			// we returned from a try block, we have a return target and
			// need to return from that.
			
			generator.ir.emit(Ir::LoadUndefined);
			generator.ir.emit(Ir::Return);
			
			if let Some(return_target) = generator.return_target {
				generator.ir.mark(return_target.label);
				generator.ir.emit(Ir::LoadLocal(return_target.local));
				generator.ir.emit(Ir::Return);
			}
			
			generator.ir.build()
		};
		
		self.functions[function_ref.usize()] = Some(Rc::new(block));
		
		Ok(())
	}
	
	pub fn get_function(&self, function_ref: FunctionRef) -> IrFunction {
		let function = &self.ast.functions[function_ref.usize()];
		
		let state = function.block.state.borrow();
		
		IrFunction {
			name: function.name,
			args: function.args,
			strict: function.block.strict,
			build_scope: state.build_scope,
			take_scope: state.take_scope,
			span: function.span
		}
	}
	
	pub fn print_ir(&mut self, ir: &mut String) -> JsResult<()> {
		for i in self.last_printed..self.functions.len() {
			try!(self.build_function_ir(FunctionRef(i as u32), ParseMode::Normal));
			
			let function = &self.ast.functions[i];
			
			ir.push_str("\n");
			ir.push_str(&*self.interner.get(function.span.file));
			write!(ir, "[{}:{}]", function.span.start_line, function.span.start_col).ok();
			ir.push_str(" ");
			
			if function.global {
				ir.push_str("(global)");
			} else {
				let name = match function.name {
					Some(ref name) => name.as_str(&mut self.interner),
					None => "(anonymous)"
				};
				
				ir.push_str("function '");
				ir.push_str(name);
				ir.push_str("'");
			}
			
			if function.block.strict {
				ir.push_str(" .strict");
			} 
			
			let block_state = function.block.state.borrow();
			
			match block_state.build_scope {
				ScopeType::Thick => ir.push_str(" .thick_scope"),
				ScopeType::Thin(size) => { write!(ir, " .thin_scope({})", size).ok(); }
				_ => {} 
			}
			
			if block_state.take_scope {
				ir.push_str(" .take_scope");
			}
			
			ir.push_str(":\n\n");
			
			self.functions[i].as_ref().unwrap().print(ir, true, &mut self.interner);
		}
		
		self.last_printed = self.functions.len();
		
		Ok(())
	}
	
	fn get_slot(&self, slot_ref: FunctionSlotRef) -> Slot {
		let function = &self.ast.functions[slot_ref.function().usize()];
		function.block.state.borrow().slots[slot_ref.slot().usize()]
	}
}

struct IrGenerator<'a> {
	ctx: &'a IrContext,
	block_state: &'a RootBlockState,
	ir: builder::IrBuilder,
	break_targets: Vec<NamedLabel>,
	continue_targets: Vec<NamedLabel>,
	scopes: Vec<&'a Block>,
	locals: Vec<Option<builder::Local>>,
	try_catch_depth: i32,
	return_target: Option<ReturnTarget>,
	env_count: usize,
	leave: Option<builder::Local>
}

impl<'a> IrGenerator<'a> {
	fn new(ctx: &'a IrContext, block_state: &'a RootBlockState, mode: ParseMode) -> IrGenerator<'a> {
		let mut generator = IrGenerator {
			ctx: ctx,
			block_state: block_state,
			ir: builder::IrBuilder::new(),
			break_targets: Vec::new(),
			continue_targets: Vec::new(),
			scopes: Vec::new(),
			locals: Vec::new(),
			try_catch_depth: 0,
			return_target: None,
			env_count: if mode == ParseMode::DirectEval || block_state.build_scope == ScopeType::Thick { 1 } else { 0 },
			leave: None
		};
		
		// Create locals for all local slots.
		
		for slot in &block_state.slots {
			generator.locals.push(
				if slot.arg.is_none() && slot.state == SlotState::Local {
					Some(generator.ir.local(Some(slot.name)))
				} else {
					None
				}
			);
		}
		
		generator
	}
	
	fn named_label(&mut self, label: &'a Option<Label>) -> NamedLabel {
		let name = match *label {
			Some(ref ident) => Some(ident.name),
			_ => None
		};
		
		NamedLabel {
			name: name,
			label: self.ir.label(),
			try_catch_depth: self.try_catch_depth
		}
	}
	
	fn fatal<T>(&self, message: &str) -> JsResult<T> {
		Err(JsError::Parse(message.to_string()))
	}
	
	fn get_target(&self, targets: &Vec<NamedLabel>, label: &Option<Label>) -> JsResult<(builder::Label, i32)> {
		match *label {
			Some(ref ident) => {
				for target in targets.iter().rev() {
					if target.name.is_some() && target.name.unwrap() == ident.name {
						return Ok((target.label, target.try_catch_depth));
					}
				}
				
				self.fatal(&format!("Cannot find break/continue target {:?}", ident.name.as_str(&self.ctx.interner)))
			},
			_ => {
				if targets.is_empty() {
					self.fatal("No break/continue target")
				} else {
					let target = targets[targets.len() - 1];
					
					Ok((target.label, target.try_catch_depth))
				}
			}
		}
	}
	
	fn push_local_scope(&mut self, block: &'a Block) {
		self.scopes.push(block);
	}
	
	fn pop_local_scope(&mut self) {
		self.scopes.pop();
	}
	
	fn unwrap_ident(&self, mut expr: &'a Expr) -> Option<&'a Ident> {
		loop {
			match *expr {
				Expr::Ident(ref ident) => return Some(ident),
				Expr::Paren(ref exprs) => expr = &exprs.exprs[exprs.exprs.len() - 1],
				_ => return None
			}
		}
	}
	
	fn emit_labeled_block(&mut self, label: &'a Option<Label>, block: &'a Block) -> JsResult<()> {
		let break_target = if label.is_some() {
			let break_target = self.named_label(label);
			self.break_targets.push(break_target);
			Some(break_target)
		} else {
			None
		};
		
		for stmt in &block.stmts {
			try!(self.emit_stmt(&stmt));
		}
		
		if let Some(break_target) = break_target {
			self.ir.mark(break_target.label);
		
			self.break_targets.pop();
		}
		
		Ok(())
	}
	
	fn emit_functions(&mut self, block: &'a Block) -> JsResult<()> {
		let mut finder = FunctionFinder {
			callback: &mut |item| {
				if let &Item::Function(ref ident, function_ref) = item {
					self.ir.emit(Ir::LoadFunction(function_ref));
					self.emit_store(ident);
				}
			}
		};
		
		finder.visit_block(&block);
		
		Ok(())
	}
	
	fn emit_block(&mut self, block: &'a Block) -> JsResult<()> {
		for stmt in &block.stmts {
			try!(self.emit_stmt(&stmt));
		}
		
		Ok(())
	}
	
	fn emit_stmts(&mut self, stmts: &'a Vec<Item>) -> JsResult<()> {
		for ref stmt in stmts {
			try!(self.emit_stmt(stmt));
		}
		
		Ok(())
	}
	
	fn emit_stmt(&mut self, stmt: &'a Item) -> JsResult<()> {
		match *stmt {
			Item::Block(ref label, ref block) => self.emit_labeled_block(label, block),
			Item::Break(ref label) => self.emit_break_continue(label, true),
			Item::Continue(ref label) => self.emit_break_continue(label, false),
			Item::Debugger => self.emit_debugger(),
			Item::Do(ref label, ref expr, ref stmt) => self.emit_do(label, expr, stmt),
			Item::Empty => Ok(()),
			Item::ExprStmt(ref exprs) => self.emit_expr_stmt(exprs),
			Item::For(ref label, ref init, ref test, ref incr, ref stmt) => self.emit_for(label, init, test, incr, stmt),
			Item::ForIn(ref label, ref expr, ref in_, ref stmt) => self.emit_for_in(label, expr, in_, stmt),
			Item::ForVar(ref label, ref init, ref test, ref incr, ref stmt) => self.emit_for_var(label, init, test, incr, stmt),
			Item::ForVarIn(ref label, ref var, ref in_, ref stmt) => self.emit_for_var_in(label, var, in_, stmt),
			Item::Function(ref ident, function_ref) => self.emit_function(ident, function_ref),
			Item::If(ref test, ref then, ref else_) => self.emit_if(test, then, if let Some(ref else_) = *else_ { Some(else_) } else { None }),
			Item::Leave(ref item) => self.emit_leave(item),
			Item::Return(ref exprs) => self.emit_return(exprs),
			Item::Switch(ref label, ref exprs, ref clauses) => self.emit_switch(label, exprs, clauses),
			Item::Throw(ref exprs) => self.emit_throw(exprs),
			Item::Try(ref try, ref catch, ref finally) => self.emit_try(try, catch, finally),
			Item::VarDecl(ref vars) => self.emit_var_decl(vars),
			Item::While(ref label, ref expr, ref stmt) => self.emit_while(label, expr, stmt),
			Item::With(ref exprs, ref stmt) => self.emit_with(exprs, stmt)
		}
	}
	
	fn emit_break_continue(&mut self, label: &'a Option<Label>, is_break: bool) -> JsResult<()> {
		let targets = if is_break { &self.break_targets } else { &self.continue_targets };
		
		let (target, try_catch_depth) = try!(self.get_target(targets, label));
		
		if try_catch_depth == self.try_catch_depth {
			self.ir.emit(Ir::Jump(target));
		} else {
			self.ir.emit(Ir::Leave(target));
		}
		
		Ok(())
	}
	
	fn emit_debugger(&mut self) -> JsResult<()> {
		self.ir.emit(Ir::Debugger);
		
		Ok(())
	}
	
	fn emit_do(&mut self, label: &'a Option<Label>, expr: &'a Expr, stmt: &'a Item) -> JsResult<()> {
		let break_target = self.named_label(label);
		self.break_targets.push(break_target);
		let continue_target = self.named_label(label);
		self.continue_targets.push(continue_target);
		
		let start = self.ir.label();
		
		self.ir.mark(start);
		
		try!(self.emit_stmt(stmt));
		
		self.ir.mark(continue_target.label);
		
		try!(self.emit_test(expr, start, false));
		
		self.ir.mark(break_target.label);
		
		self.break_targets.pop();
		self.continue_targets.pop();
		
		Ok(())
	}

	fn emit_expr_stmt(&mut self, exprs: &'a ExprSeq) -> JsResult<()> {
		let leave = self.leave;
		self.leave = None;
		
		try!(self.emit_exprs(exprs, leave.is_some()));
		
		if let Some(leave) = leave {
			self.ir.emit(Ir::StoreLocal(leave));
		}
		
		Ok(())
	}
	
	fn emit_for(&mut self, label: &'a Option<Label>, init: &'a Option<ExprSeq>, test: &'a Option<ExprSeq>, incr: &'a Option<ExprSeq>, stmt: &'a Item) -> JsResult<()> {
		self.emit_for_init(
			label,
			|generator| {
				if let Some(ref init) = *init {
					try!(generator.emit_exprs(init, false))
				}
				
				Ok(())
			},
			test,
			incr,
			stmt
		)
	}
	
	fn emit_for_var(&mut self, label: &'a Option<Label>, init: &'a Option<Vec<Var>>, test: &'a Option<ExprSeq>, incr: &'a Option<ExprSeq>, stmt: &'a Item) -> JsResult<()> {
		self.emit_for_init(
			label,
			|generator| {
				if let Some(ref init) = *init {
					try!(generator.emit_var_decl(init));
				}
				
				Ok(())
			},
			test,
			incr,
			stmt
		)
	}
	
	fn emit_for_init<F: Fn(&mut IrGenerator<'a>) -> JsResult<()>>(&mut self, label: &'a Option<Label>, init: F, test: &'a Option<ExprSeq>, incr: &'a Option<ExprSeq>, stmt: &'a Item) -> JsResult<()> {
		let break_target = self.named_label(label);
		self.break_targets.push(break_target);
		let continue_target = self.named_label(label);
		self.continue_targets.push(continue_target);
		
		let test_label = self.ir.label();
		let body_label = self.ir.label();
		
		try!(init(self));
		
		self.ir.emit(Ir::Jump(test_label));
		
		self.ir.mark(body_label);
		
		try!(self.emit_stmt(stmt));
		
		self.ir.mark(continue_target.label);
		
		if let Some(ref incr) = *incr {
			try!(self.emit_exprs(incr, false));
		}
		
		self.ir.mark(test_label);
		
		if let Some(ref test) = *test {
			try!(self.emit_tests(test, body_label, false));
		} else {
			self.ir.emit(Ir::Jump(body_label));
		}
		
		self.ir.mark(break_target.label);
		
		self.break_targets.pop();
		self.continue_targets.pop();
		
		Ok(())
	}
	
	fn emit_for_in(&mut self, label: &'a Option<Label>, expr: &'a Expr, in_: &'a ExprSeq, stmt: &'a Item) -> JsResult<()> {
		self.emit_for_in_init(
			label,
			|generator| generator.resolve_ident(expr),
			in_,
			stmt
		)
	}
	
	fn resolve_ident(&mut self, expr: &'a Expr) -> JsResult<&'a Ident> {
		if let Expr::Ident(ref ident) = *expr {
			Ok(ident)
		} else {
			self.fatal("Expected an identifier")
		}
	}
	
	fn emit_for_var_in(&mut self, label: &'a Option<Label>, var: &'a Ident, in_: &'a ExprSeq, stmt: &'a Item) -> JsResult<()> {
		self.emit_for_in_init(
			label,
			|_| Ok(var),
			in_,
			stmt
		)
	}
	
	fn emit_for_in_init<F: Fn(&mut IrGenerator<'a>) -> JsResult<&'a Ident>>(&mut self, label: &'a Option<Label>, expr: F, in_: &'a ExprSeq, stmt: &'a Item) -> JsResult<()> {
		let break_target = self.named_label(label);
		self.break_targets.push(break_target);
		let continue_target = self.named_label(label);
		self.continue_targets.push(continue_target);
		
		let local = try!(expr(self));
		let iter = self.ir.local(None);
		let next_label = self.ir.label();
		
		try!(self.emit_exprs(in_, true));
		self.ir.emit(Ir::IntoIter(iter));
		
		self.ir.start_exception_block();

		self.ir.emit(Ir::Jump(continue_target.label));
		
		self.ir.mark(next_label);
		
		self.ir.emit(Ir::CurrentIter(iter));
		self.emit_store(local);
		
		try!(self.emit_stmt(stmt));
		
		self.ir.mark(continue_target.label);
		
		self.ir.emit(Ir::NextIter(iter, next_label));
		
		self.ir.emit(Ir::Leave(break_target.label));
		
		self.ir.start_finally();
		
		self.ir.emit(Ir::EndIter(iter));
		
		self.ir.emit(Ir::EndFinally);
		
		self.ir.end_exception_block();
		
		self.ir.mark(break_target.label);
		
		self.break_targets.pop();
		self.continue_targets.pop();
		
		Ok(())
	}
	
	fn emit_function(&mut self, _ident: &'a Ident, _function_ref: FunctionRef) -> JsResult<()> {
		// No-op.
		
		Ok(())
	}
	
	fn emit_if(&mut self, test: &'a ExprSeq, then: &'a Item, else_: Option<&'a Item>) -> JsResult<()> {
		if let Some(else_) = else_ {
			let else_label = self.ir.label();
			let after_label = self.ir.label();
			
			try!(self.emit_tests(test, else_label, true));
			
			try!(self.emit_stmt(then));
			
			self.ir.emit(Ir::Jump(after_label));
			
			self.ir.mark(else_label);
			
			try!(self.emit_stmt(else_));
			
			self.ir.mark(after_label);
		} else {
			let after_label = self.ir.label();
			
			try!(self.emit_tests(test, after_label, true));
			
			try!(self.emit_stmt(then));
			
			self.ir.mark(after_label);
		}
		
		Ok(())
	}
	
	fn emit_leave(&mut self, item: &'a Item) -> JsResult<()> {
		let local = self.ir.local(None);
		self.leave = Some(local);
		
		try!(self.emit_stmt(item));
		
		self.ir.emit(Ir::LoadLocal(local));
		self.ir.emit(Ir::Return);
		
		Ok(())
	}
	
	fn emit_return(&mut self, exprs: &'a Option<ExprSeq>) -> JsResult<()> {
		if let Some(ref exprs) = *exprs {
			try!(self.emit_exprs(exprs, true));
		} else {
			self.ir.emit(Ir::LoadUndefined);
		}
		
		if self.try_catch_depth > 0 {
			let return_target = if let Some(return_target) = self.return_target {
				return_target
			} else {
				let return_target = ReturnTarget {
					label: self.ir.label(),
					local: self.ir.local(None)
				};
				
				self.return_target = Some(return_target);
				
				return_target
			};
			
			self.ir.emit(Ir::StoreLocal(return_target.local));
			self.ir.emit(Ir::Leave(return_target.label));
		} else {
			self.ir.emit(Ir::Return);
		}
		
		Ok(())
	}
	
	fn emit_switch(&mut self, label: &'a Option<Label>, exprs: &'a ExprSeq, clauses: &'a Vec<SwitchClause>) -> JsResult<()> {
		let break_target = self.named_label(label);
		self.break_targets.push(break_target);
		
		let local = self.ir.local(None);
		
		try!(self.emit_exprs(exprs, true));
		self.ir.emit(Ir::StoreLocal(local));
		
		let mut targets = Vec::new();
		let mut default = None;
		
		for clause in clauses {
			match *clause {
				SwitchClause::Case(ref exprs, _) => {
					let label = self.ir.label();
					targets.push(label);
					
					self.ir.emit(Ir::LoadLocal(local));
					try!(self.emit_exprs(exprs, true));
					self.ir.emit(Ir::JumpEq(label));
				}
				SwitchClause::Default(..) => {
					let label = self.ir.label();
					default = Some(label);
					targets.push(label);
				}
			}
		}
		
		if let Some(default) = default {
			self.ir.emit(Ir::Jump(default));
		} else {
			self.ir.emit(Ir::Jump(break_target.label));
		}
		
		for i in 0..clauses.len() {
			self.ir.mark(targets[i]);
			
			let stmts = match clauses[i] {
				SwitchClause::Case(_, ref stmts) => stmts,
				SwitchClause::Default(ref stmts) => stmts
			};
			
			try!(self.emit_stmts(stmts));
		}
		
		self.ir.mark(break_target.label);
		
		self.break_targets.pop();
		
		Ok(())
	}
	
	fn emit_throw(&mut self, exprs: &'a ExprSeq) -> JsResult<()> {
		try!(self.emit_exprs(exprs, true));
		
		self.ir.emit(Ir::Throw);
		
		Ok(())
	}
	
	fn emit_try(&mut self, try: &'a Block, catch: &'a Option<Catch>, finally: &'a Option<Block>) -> JsResult<()> {
		self.try_catch_depth += 1;
		
		let after_label = self.ir.label();
		
		self.ir.start_exception_block();
		
		try!(self.emit_block(try));
		
		self.ir.emit(Ir::Leave(after_label));
		
		if let Some(ref catch) = *catch {
			self.ir.start_catch();
			
			self.push_local_scope(&catch.block);
			
			// Build the catch block. In deopt, we're entering a scope.
			// Otherwise we can just emit the statements.
			
			let deopt = self.block_state.build_scope == ScopeType::Thick;
			
			if deopt {
				self.try_catch_depth += 1;
	
				self.ir.emit(Ir::EnterEnv);
				self.env_count += 1;
				self.ir.start_exception_block();
				
				self.ir.emit(Ir::LoadEnvObject);
				self.ir.emit(Ir::LoadException);
				self.ir.emit(Ir::StoreName(catch.ident.name));
				
				try!(self.emit_block(&catch.block));
				
				let label = self.ir.label();
				self.ir.emit(Ir::Leave(label));
				self.ir.start_finally();
				self.env_count -= 1;
				self.ir.emit(Ir::LeaveEnv);
				self.ir.emit(Ir::EndFinally);
				self.ir.end_exception_block();
				self.ir.mark(label);
	
				self.try_catch_depth -= 1;
			} else {
				self.ir.emit(Ir::LoadException);
				self.emit_store(&catch.ident);
				
				try!(self.emit_block(&catch.block));
			}
			
			self.pop_local_scope();
			
			self.ir.emit(Ir::Leave(after_label));
		}
		
		if let Some(ref finally) = *finally {
			self.ir.start_finally();
			
			try!(self.emit_block(finally));
			
			self.ir.emit(Ir::EndFinally);
		}
		
		self.ir.end_exception_block();
		
		self.ir.mark(after_label);
		
		self.try_catch_depth -= 1;
		
		Ok(())
	}
	
	fn emit_var_decl(&mut self, vars: &'a Vec<Var>) -> JsResult<()> {
		for var in vars {
			if let Some(ref expr) = var.expr {
				try!(self.emit_expr(expr, true));
				self.emit_store(&var.ident);
			}
		}
		
		Ok(())
	}
	
	fn emit_while(&mut self, label: &'a Option<Label>, expr: &'a Expr, stmt: &'a Item) -> JsResult<()> {
		let break_target = self.named_label(label);
		self.break_targets.push(break_target);
		let continue_target = self.named_label(label);
		self.continue_targets.push(continue_target);
		
		let start = self.ir.label();
		
		self.ir.emit(Ir::Jump(continue_target.label));
		
		self.ir.mark(start);
		
		try!(self.emit_stmt(stmt));
		
		self.ir.mark(continue_target.label);
		
		try!(self.emit_test(expr, start, false));
		
		self.ir.mark(break_target.label);
		
		self.break_targets.pop();
		self.continue_targets.pop();
		
		Ok(())
	}
	
	fn emit_with(&mut self, exprs: &'a ExprSeq, stmt: &'a Item) -> JsResult<()> {
		self.try_catch_depth += 1;
		
		try!(self.emit_exprs(exprs, true));
		self.ir.emit(Ir::EnterWithEnv);
		self.env_count += 1;
		self.ir.start_exception_block();
		
		try!(self.emit_stmt(stmt));
		
		let label = self.ir.label();
		self.ir.emit(Ir::Leave(label));
		self.ir.start_finally();
		self.env_count -= 1;
		self.ir.emit(Ir::LeaveEnv);
		self.ir.emit(Ir::EndFinally);
		self.ir.end_exception_block();
		self.ir.mark(label);

		self.try_catch_depth -= 1;
		
		Ok(())
	}
	
	fn emit_exprs(&mut self, exprs: &'a ExprSeq, leave: bool) -> JsResult<()> {
		let len = exprs.exprs.len();
		
		for i in 0..len {
			try!(self.emit_expr(&exprs.exprs[i], leave && i == len - 1));
		}
		
		Ok(())
	}
	
	fn emit_tests(&mut self, exprs: &'a ExprSeq, label: builder::Label, inverse: bool) -> JsResult<()> {
		let len = exprs.exprs.len();
		
		for i in 0..len {
			if i == len - 1 {
				try!(self.emit_test(&exprs.exprs[i], label, inverse));
			} else {
				try!(self.emit_expr(&exprs.exprs[i], false));
			}
		}
		
		Ok(())
	}
	
	fn emit_expr(&mut self, expr: &'a Expr, leave: bool) -> JsResult<()> {
		match *expr {
			Expr::ArrayLiteral(ref exprs) => self.emit_expr_array_literal(exprs, leave),
			Expr::Assign(op, ref lhs, ref rhs) => self.emit_expr_assign(op, lhs, rhs, leave),
			Expr::Binary(op, ref lhs, ref rhs) => self.emit_expr_binary(op, lhs, rhs, leave),
			Expr::Call(ref expr, ref args) => self.emit_expr_call(expr, args, leave),
			Expr::Function(function_ref) => self.emit_expr_function(function_ref, leave),
			Expr::Ident(ref ident) => self.emit_expr_ident(ident, leave),
			Expr::Literal(ref lit) => self.emit_expr_literal(&lit, leave),
			Expr::MemberDot(ref expr, ident) => self.emit_expr_member_dot(expr, ident, leave),
			Expr::MemberIndex(ref expr, ref index) => self.emit_expr_member_index(expr, index, leave),
			Expr::Missing => { panic!("missing must be handled at array creation"); },
			Expr::New(ref expr) => self.emit_expr_new(expr, leave),
			Expr::ObjectLiteral(ref props) => self.emit_expr_object_literal(props, leave),
			Expr::Paren(ref exprs) => self.emit_expr_paren(exprs, leave),
			Expr::Ternary(ref test, ref then, ref else_) => self.emit_expr_ternary(test, then, else_, leave),
			Expr::This => self.emit_expr_this(leave),
			Expr::Unary(op, ref expr) => self.emit_expr_unary(op, expr, leave)
		}
	}
	
	fn emit_test(&mut self, expr: &'a Expr, label: builder::Label, inverse: bool) -> JsResult<()> {
		// TODO: This can be optimized to use actual opcodes instead of just coercing to true/false.
		
		try!(self.emit_expr(expr, true));
		
		if inverse {
			self.ir.emit(Ir::JumpFalse(label));
		} else {
			self.ir.emit(Ir::JumpTrue(label));
		}
		
		Ok(())
	}
	
	// 11.1.4 Array Initialiser
	fn emit_expr_array_literal(&mut self, exprs: &'a Vec<Expr>, leave: bool) -> JsResult<()> {
		if leave {
			self.ir.emit(Ir::NewArray);
			
			if exprs.len() > 0 {
				let mut length = 0;
				let mut last_missing = false;
				
				for i in 0..exprs.len() {
					let expr = &exprs[i];
					
					let missing = if let Expr::Missing = *expr { true } else { false };
					if !missing {
						self.ir.emit(Ir::Dup);
						self.ir.emit(Ir::LoadI32(i as i32));
						try!(self.emit_expr(&exprs[i], true));
						// TODO: This is not conforming the specs. The specs state that
						// array initialization should use [[DefineOwnProperty]] should
						// be used but StoreIndex resolved to [[Put]].
						self.ir.emit(Ir::StoreIndex);
						
						length = i + 1;
					} else if i == exprs.len() - 1 {
						last_missing = true;
					}
				}
				
				let expected_length = if last_missing {
					exprs.len() - 1
				} else {
					exprs.len()
				};
				
				if length != expected_length {
					self.ir.emit(Ir::Dup);
					self.ir.emit(Ir::LoadI32(expected_length as i32));
					self.ir.emit(Ir::StoreName(name::LENGTH));
				}
			}
		} else {
			for expr in exprs {
				try!(self.emit_expr(expr, false));
			}
		}
		
		Ok(())
	}
	
	fn emit_op(&mut self, op: Op) {
		match op {
			Op::Add => self.ir.emit(Ir::Add),
			Op::BitAnd => self.ir.emit(Ir::BitAnd),
			Op::BitNot => self.ir.emit(Ir::BitNot),
			Op::BitOr => self.ir.emit(Ir::BitOr),
			Op::BitXOr => self.ir.emit(Ir::BitXOr),
			Op::Divide => self.ir.emit(Ir::Divide),
			Op::Equals => self.ir.emit(Ir::Eq),
			Op::GreaterThan => self.ir.emit(Ir::Gt),
			Op::GreaterThanEquals => self.ir.emit(Ir::Ge),
			Op::IdentityEquals => self.ir.emit(Ir::StrictEq),
			Op::IdentityNotEquals => self.ir.emit(Ir::StrictNe),
			Op::In => self.ir.emit(Ir::In),
			Op::InstanceOf => self.ir.emit(Ir::InstanceOf),
			Op::LeftShiftArithmetic => self.ir.emit(Ir::Lsh),
			Op::LessThan => self.ir.emit(Ir::Lt),
			Op::LessThanEquals => self.ir.emit(Ir::Le),
			Op::Modulus => self.ir.emit(Ir::Modulus),
			Op::Multiply => self.ir.emit(Ir::Multiply),
			Op::Negative => self.ir.emit(Ir::Negative),
			Op::Not => self.ir.emit(Ir::Not),
			Op::NotEquals => self.ir.emit(Ir::Ne),
			Op::Positive => self.ir.emit(Ir::Positive),
			Op::RightShiftArithmetic => self.ir.emit(Ir::Rsh),
			Op::RightShiftLogical => self.ir.emit(Ir::RshZeroFill),
			Op::Subtract => self.ir.emit(Ir::Subtract),
			_ => panic!("Invalid operator")
		}
	}
	
	fn unwrap_paren(&self, expr: &'a Expr) -> &'a Expr {
		if let Expr::Paren(ref exprs) = *expr {
			if exprs.exprs.len() == 1 {
				return &exprs.exprs[0];
			}
		}
		
		expr
	}
	
	fn emit_expr_assign(&mut self, op: Op, lhs: &'a Expr, rhs: &'a Expr, leave: bool) -> JsResult<()> {
		let load = |generator: &mut IrGenerator<'a>| {
			if op == Op::Assign {
				try!(generator.emit_expr(rhs, true));
			} else {
				try!(generator.emit_expr(lhs, true));
				try!(generator.emit_expr(rhs, true));
				
				generator.emit_op(op);
			}
			
			Ok(())
		};
		
		let store = |generator: &mut IrGenerator<'a>, ir: Ir| {
			// Store the result in a temporary if we need to leave something
			// on the stack.
			
			if leave {
				let local = generator.ir.local(None);
				
				generator.ir.emit(Ir::Dup);
				generator.ir.emit(Ir::StoreLocal(local));
				
				// Execute the IR that stores the result.
				generator.ir.emit(ir);
				
				// Load the result of the local.
				generator.ir.emit(Ir::LoadLocal(local));
			} else {
				// The target and index are still on the stack. Store the result.
				generator.ir.emit(ir);
			}
		};
		
		match *self.unwrap_paren(lhs) {
			Expr::Ident(ref ident) => {
				match ident.state.get() {
					IdentState::Arg(_, index) | IdentState::LiftedArg(_, index) | IdentState::ScopedArg(_, index) => {
						if leave {
							try!(load(self));
							self.ir.emit(Ir::Dup);
							self.emit_load_arguments(ident.state.get());
							self.ir.emit(Ir::Swap);
						} else {
							self.emit_load_arguments(ident.state.get());
							try!(load(self));
						}
						
						self.ir.emit(Ir::StoreName(Name::from_index(index as usize)));
					}
					IdentState::Scoped => {
						self.ir.emit(Ir::LoadEnvObjectFor(ident.name));
						
						try!(load(self));
						
						store(self, Ir::StoreName(ident.name));
					}
					_ => {
						try!(load(self));
						
						if leave {
							self.ir.emit(Ir::Dup);
						}
						
						self.emit_store(ident);
					}
				}
			},
			Expr::MemberDot(ref expr, ident) => {
				try!(self.emit_expr(expr, true));
				
				try!(load(self));
				
				store(self, Ir::StoreName(ident));
			},
			Expr::MemberIndex(ref expr, ref index) => {
				if op == Op::Assign {
					// Load the target and index onto the stack.
					try!(self.emit_member_target(expr, index, false));
					
					try!(load(self));
					
					store(self, Ir::StoreIndex);
				} else {
					// Load target and index onto the stack.
					try!(self.emit_member_target(expr, index, false));
					
					// Dup both member and index so we can re-use them for the store.
					self.ir.emit(Ir::Pick(1));
					self.ir.emit(Ir::Pick(1));
					
					// Load the current value.
					self.ir.emit(Ir::LoadIndex);
					
					// Execute the RHS of the assignment.
					try!(self.emit_expr(rhs, true));
					
					// Perform the operation.
					self.emit_op(op);
					
					store(self, Ir::StoreIndex);
				}
			}
			_ => return Err(JsError::Reference("Invalid assignment expression".to_string()))
		}
		
		Ok(())
	}
		
	fn emit_expr_binary(&mut self, op: Op, lhs: &'a Expr, rhs: &'a Expr, leave: bool) -> JsResult<()> {
		match op {
			Op::And | Op::Or => {
				let after = self.ir.label();
				
				try!(self.emit_expr(lhs, true));
				self.ir.emit(Ir::Dup);
				self.ir.emit(Ir::ToBoolean);
				if op == Op::And {
					self.ir.emit(Ir::JumpFalse(after));
				} else {
					self.ir.emit(Ir::JumpTrue(after));
				}
				self.ir.emit(Ir::Pop);
				try!(self.emit_expr(rhs, true));
				self.ir.mark(after);
				
				if !leave {
					self.ir.emit(Ir::Pop);
				}
				
				return Ok(());
			}
			_ => {}
		}

		try!(self.emit_expr(lhs, true));
		try!(self.emit_expr(rhs, true));
		self.emit_op(op);
		
		if !leave {
			self.ir.emit(Ir::Pop);
		}
		
		Ok(())
	}
	
	fn emit_expr_call(&mut self, expr: &'a Expr, args: &'a Vec<Expr>, leave: bool) -> JsResult<()> {
		let have_this = match *expr {
			Expr::MemberDot(ref expr, ident) => {
				try!(self.emit_expr(expr, true));
				self.ir.emit(Ir::Dup);
				self.ir.emit(Ir::LoadName(ident));
				true
			}
			Expr::MemberIndex(ref expr, ref index) => {
				try!(self.emit_member_target(expr, index, true));
				self.ir.emit(Ir::LoadIndex);
				true
			}
			_ => {
				try!(self.emit_expr(expr, true));
				false
			}
		};
		
		for arg in args {
			try!(self.emit_expr(arg, true));
		}
		
		let is_eval = if let Expr::Ident(ref ident) = *expr {
			ident.name == name::EVAL
		} else {
			false
		};
		
		if is_eval {
			self.ir.emit(Ir::CallEval(args.len() as u32));
		} else if have_this {
			self.ir.emit(Ir::CallThis(args.len() as u32));
		} else {
			self.ir.emit(Ir::Call(args.len() as u32));
		}
		
		if !leave {
			self.ir.emit(Ir::Pop);
		}
		
		Ok(())
	}
	
	fn emit_expr_function(&mut self, function_ref: FunctionRef, leave: bool) -> JsResult<()> {
		self.ir.emit(Ir::LoadFunction(function_ref));
		
		if !leave {
			self.ir.emit(Ir::Pop);
		}
		
		Ok(())
	}
	
	fn emit_expr_ident(&mut self, ident: &'a Ident, leave: bool) -> JsResult<()> {
		self.emit_load(ident);
		if !leave {
			self.ir.emit(Ir::Pop);
		}
		
		Ok(())
	}
	
	fn emit_expr_literal(&mut self, lit: &'a Lit, leave: bool) -> JsResult<()> {
		if leave {
			match *lit {
				Lit::Null => self.ir.emit(Ir::LoadNull),
				Lit::Boolean(value) => self.ir.emit(if value { Ir::LoadTrue } else { Ir::LoadFalse }),
				Lit::String(value, _) => self.ir.emit(Ir::LoadString(value)),
				Lit::Integer(value) => self.ir.emit(Ir::LoadI32(value)),
				Lit::Long(value) => self.ir.emit(Ir::LoadI64(value)),
				Lit::Double(value) => self.ir.emit(Ir::LoadF64(value)),
				Lit::Regex(body, flags) => self.ir.emit(Ir::LoadRegex(body, flags))
			}
		}
		
		Ok(())
	}
	
	fn emit_expr_member_dot(&mut self, expr: &'a Expr, ident: Name, leave: bool) -> JsResult<()> {
		try!(self.emit_expr(expr, true));
		self.ir.emit(Ir::LoadName(ident));
		
		if !leave {
			self.ir.emit(Ir::Pop);
		}
		
		Ok(())
	}
	
	fn emit_expr_member_index(&mut self, expr: &'a Expr, index: &'a ExprSeq, leave: bool) -> JsResult<()> {
		try!(self.emit_member_target(expr, index, false));
		self.ir.emit(Ir::LoadIndex);
		
		if !leave {
			self.ir.emit(Ir::Pop);
		}
		
		Ok(())
	}
	
	fn emit_expr_new(&mut self, expr: &'a Expr, leave: bool) -> JsResult<()> {
		if let Expr::Call(ref expr, ref args) = *expr {
			try!(self.emit_expr(expr, true));
		
			for arg in args {
				try!(self.emit_expr(arg, true));
			}
			
			self.ir.emit(Ir::New(args.len() as u32));
		} else {
			try!(self.emit_expr(expr, true));
		
			self.ir.emit(Ir::New(0));
		}
		
		if !leave {
			self.ir.emit(Ir::Pop);
		}
		
		Ok(())
	}
	
	fn emit_expr_object_literal(&mut self, props: &'a Vec<Property>, leave: bool) -> JsResult<()> {
		if leave {
			self.ir.emit(Ir::NewObject);
			
			for prop in props {
				self.ir.emit(Ir::Dup);
				
				match *prop {
					Property::Assignment(ref key, ref expr) => {
						match *key {
							PropertyKey::Ident(ident) => {
								try!(self.emit_expr(&*expr, true));
								self.ir.emit(Ir::StoreNameUnchecked(ident))
							}
							PropertyKey::Literal(ref lit) => {
								try!(self.emit_expr_literal(&*lit, true));
								try!(self.emit_expr(&*expr, true));
								self.ir.emit(Ir::StoreIndexUnchecked);
							}
						}
					}
					Property::Getter(ref ident, function_ref) => {
						if let Some(ident) = *ident {
							self.ir.emit(Ir::StoreNameGetterUnchecked(ident, function_ref));
						} else {
							self.ir.emit(Ir::StoreGetterUnchecked(function_ref));
						}
					}
					Property::Setter(ref ident, function_ref) => {
						if let Some(ident) = *ident {
							self.ir.emit(Ir::StoreNameSetterUnchecked(ident, function_ref));
						} else {
							self.ir.emit(Ir::StoreSetterUnchecked(function_ref));
						}
					}
				}
			}
		} else {
			for prop in props {
				match *prop {
					Property::Assignment(_, ref expr) => try!(self.emit_expr(expr, false)),
					_ => {}
				}
			}
		}
		
		Ok(())
	}
	
	fn emit_expr_paren(&mut self, exprs: &'a ExprSeq, leave: bool) -> JsResult<()> {
		self.emit_exprs(exprs, leave)
	}
	
	fn emit_expr_ternary(&mut self, test: &'a Expr, then: &'a Expr, else_: &'a Expr, leave: bool) -> JsResult<()> {
		let else_label = self.ir.label();
		let after_label = self.ir.label();
		
		try!(self.emit_test(test, else_label, true));
		
		try!(self.emit_expr(then, leave));
		
		self.ir.emit(Ir::Jump(after_label));
		
		self.ir.mark(else_label);
		
		try!(self.emit_expr(else_, leave));
		
		self.ir.mark(after_label);
		
		Ok(())
	}
	
	fn emit_expr_this(&mut self, leave: bool) -> JsResult<()> {
		if leave {
			self.ir.emit(Ir::LoadThis);
		}
		
		Ok(())
	}
	
	fn emit_lhs_load(&mut self, expr: &'a Expr) -> JsResult<LhsRef<'a>> {
		let expr = self.unwrap_paren(expr);
		
		match *expr {
			Expr::Ident(ref ident) => {
				match ident.state.get() {
					IdentState::Arg(_, index) | IdentState::LiftedArg(_, index) | IdentState::ScopedArg(_, index) => {
						self.emit_load_arguments(ident.state.get());
						self.ir.emit(Ir::Dup);
						self.ir.emit(Ir::LoadName(Name::from_index(index as usize)));
					}
					_ => {
						self.emit_load(ident);
					}
				}
			}
			Expr::MemberDot(ref expr, ident) => {
				try!(self.emit_expr(expr, true));
				self.ir.emit(Ir::Dup);
				self.ir.emit(Ir::LoadName(ident));
			}
			Expr::MemberIndex(ref expr, ref index) => {
				try!(self.emit_member_target(expr, index, false));
				self.ir.emit(Ir::Pick(1));
				self.ir.emit(Ir::Pick(1));
				self.ir.emit(Ir::LoadIndex);
			}
			_ => return Err(JsError::Reference("Unexpected left hand side expression".to_string()))
		}
		
		Ok(LhsRef {
			expr: expr
		})
	}
	
	fn emit_lhs_store(&mut self, lhs_ref: LhsRef<'a>) -> JsResult<()> {
		let result = match *lhs_ref.expr {
			Expr::Ident(ref ident) => {
				match ident.state.get() {
					IdentState::Arg(_, index) | IdentState::LiftedArg(_, index) | IdentState::ScopedArg(_, index) => {
						self.ir.emit(Ir::StoreName(Name::from_index(index as usize)));
					}
					_ => {
						self.emit_store(ident);
					}
				}
			}
			Expr::MemberDot(_, ident) => {
				self.ir.emit(Ir::StoreName(ident));
			}
			Expr::MemberIndex(..) => {
				self.ir.emit(Ir::StoreIndex);
			}
			_ => return Err(JsError::Reference("Unexpected left hand side expression".to_string()))
		};
		
		Ok(result)
	}
	
	fn emit_expr_unary(&mut self, op: Op, expr: &'a Expr, leave: bool) -> JsResult<()> {
		match op {
			Op::Delete => {
				match *expr {
					Expr::MemberDot(ref expr, name) => {
						try!(self.emit_expr(expr, true));
						self.ir.emit(Ir::DeleteName(name))
					}
					Expr::MemberIndex(ref expr, ref index) => {
						try!(self.emit_member_target(expr, index, false));
						self.ir.emit(Ir::DeleteIndex);
					}
					ref expr @ _ => {
						if let Some(ident) = self.unwrap_ident(expr) {
							match ident.state.get() {
								IdentState::Scoped | IdentState::Global(..) => {
									self.ir.emit(Ir::LoadEnvObjectFor(ident.name));
									self.ir.emit(Ir::DeleteEnvName(ident.name));
								}
								IdentState::Arg(_, index) | IdentState::LiftedArg(_, index) | IdentState::ScopedArg(_, index) => {
									self.emit_load_arguments(ident.state.get());
									self.ir.emit(Ir::DeleteName(Name::from_index(index as usize)));
								}
								IdentState::Slot(..) | IdentState::LiftedSlot(..) | IdentState::LoadFunction(..) => {
									self.ir.emit(Ir::LoadFalse);
								}
								IdentState::None => panic!()
							}
						} else {
							try!(self.emit_expr(expr, true));
							self.ir.emit(Ir::LoadTrue);
						};
					}
				}
				
				if !leave {
					self.ir.emit(Ir::Pop);
				}
				
				return Ok(());
			}
			Op::Typeof => {
				match *expr {
					Expr::MemberDot(ref expr, name) => {
						try!(self.emit_expr(expr, true));
						self.ir.emit(Ir::TypeofName(name))
					}
					Expr::MemberIndex(ref expr, ref index) => {
						try!(self.emit_member_target(expr, index, false));
						self.ir.emit(Ir::TypeofIndex);
					}
					ref expr @ _ => {
						let matched = if let Some(ident) = self.unwrap_ident(expr) {
							match ident.state.get() {
								IdentState::Scoped | IdentState::Global(..) => {
									self.ir.emit(Ir::LoadEnvObjectFor(ident.name));
									self.ir.emit(Ir::TypeofName(ident.name));
									true
								}
								IdentState::Arg(_, index) | IdentState::LiftedArg(_, index) | IdentState::ScopedArg(_, index) => {
									self.emit_load_arguments(ident.state.get());
									self.ir.emit(Ir::TypeofName(Name::from_index(index as usize)));
									true
								}
								_ => false
							}
						} else {
							false
						};
						
						if !matched {
							try!(self.emit_expr(expr, true));
							self.ir.emit(Ir::Typeof);
						}
					}
				}
				
				if !leave {
					self.ir.emit(Ir::Pop);
				}
				
				return Ok(());
			}
			Op::PreIncr => {
				let lhs_ref = try!(self.emit_lhs_load(expr));
				self.ir.emit(Ir::ToNumber);
				
				if leave {
					let local = self.ir.local(None);
					
					self.ir.emit(Ir::LoadI32(1));
					self.ir.emit(Ir::Add);
					self.ir.emit(Ir::Dup);
					self.ir.emit(Ir::StoreLocal(local));
					try!(self.emit_lhs_store(lhs_ref));
					self.ir.emit(Ir::LoadLocal(local));
				} else {
					self.ir.emit(Ir::LoadI32(1));
					self.ir.emit(Ir::Add);
					try!(self.emit_lhs_store(lhs_ref));
				}
				
				return Ok(());
			}
			Op::PostIncr => {
				let lhs_ref = try!(self.emit_lhs_load(expr));
				self.ir.emit(Ir::ToNumber);
				
				if leave {
					let local = self.ir.local(None);
					
					self.ir.emit(Ir::Dup);
					self.ir.emit(Ir::StoreLocal(local));
					self.ir.emit(Ir::LoadI32(1));
					self.ir.emit(Ir::Add);
					try!(self.emit_lhs_store(lhs_ref));
					self.ir.emit(Ir::LoadLocal(local));
				} else {
					self.ir.emit(Ir::LoadI32(1));
					self.ir.emit(Ir::Add);
					try!(self.emit_lhs_store(lhs_ref));
				}
				
				return Ok(());
			}
			Op::PreDecr => {
				let lhs_ref = try!(self.emit_lhs_load(expr));
				self.ir.emit(Ir::ToNumber);
				
				if leave {
					let local = self.ir.local(None);
					
					self.ir.emit(Ir::LoadI32(1));
					self.ir.emit(Ir::Subtract);
					self.ir.emit(Ir::Dup);
					self.ir.emit(Ir::StoreLocal(local));
					try!(self.emit_lhs_store(lhs_ref));
					self.ir.emit(Ir::LoadLocal(local));
				} else {
					self.ir.emit(Ir::LoadI32(1));
					self.ir.emit(Ir::Subtract);
					try!(self.emit_lhs_store(lhs_ref));
				}
				
				return Ok(());
			}
			Op::PostDecr => {
				let lhs_ref = try!(self.emit_lhs_load(expr));
				self.ir.emit(Ir::ToNumber);
				
				if leave {
					let local = self.ir.local(None);
					
					self.ir.emit(Ir::Dup);
					self.ir.emit(Ir::StoreLocal(local));
					self.ir.emit(Ir::LoadI32(1));
					self.ir.emit(Ir::Subtract);
					try!(self.emit_lhs_store(lhs_ref));
					self.ir.emit(Ir::LoadLocal(local));
				} else {
					self.ir.emit(Ir::LoadI32(1));
					self.ir.emit(Ir::Subtract);
					try!(self.emit_lhs_store(lhs_ref));
				}
				
				return Ok(());
			}
			_ => {}
		}
		
		try!(self.emit_expr(expr, true));
		
		match op {
			Op::BitNot => self.ir.emit(Ir::BitNot),
			Op::Negative => self.ir.emit(Ir::Negative),
			Op::Not => self.ir.emit(Ir::Not),
			Op::Positive => self.ir.emit(Ir::Positive),
			Op::Void => {
				self.ir.emit(Ir::Pop);
				
				if leave {
					self.ir.emit(Ir::LoadUndefined);
				} else {
					return Ok(())
				}
			}
			_ => panic!("Unexpected unary")
		}
		
		if !leave {
			self.ir.emit(Ir::Pop);
		}
		
		Ok(())
	}
	
	fn emit_load(&mut self, ident: &'a Ident) {
		match ident.state.get() {
			IdentState::Scoped | IdentState::Global(..) => {
				if self.env_count > 0 {
					if
						ident.arguments &&
						self.block_state.build_scope == ScopeType::Thick
					{
						self.ir.emit(Ir::LoadEnvArguments(0));
					} else {
						self.ir.emit(Ir::LoadEnv(ident.name));
					}
				} else {
					self.ir.emit(Ir::LoadGlobal(ident.name));
				}
			}
			IdentState::Slot(slot_ref) => self.emit_load_slot(slot_ref),
			IdentState::LiftedSlot(slot_ref) => self.emit_load_lifted_slot(slot_ref),
			IdentState::Arg(_, index) | IdentState::LiftedArg(_, index) | IdentState::ScopedArg(_, index) => {
				self.emit_load_arguments(ident.state.get());
				self.ir.emit(Ir::LoadName(Name::from_index(index as usize)));
			}
			IdentState::LoadFunction(function_ref) => self.ir.emit(Ir::LoadFunction(function_ref)),
			IdentState::None => panic!()
		}
	}
	
	fn emit_load_slot(&mut self, slot_ref: SlotRef) {
		let slot = &self.block_state.slots[slot_ref.usize()];
		if let SlotState::Lifted(index) = slot.state {
			self.ir.emit(Ir::LoadLifted(index, 0));
		} else if let Some(arg) = slot.arg {
			self.ir.emit(Ir::LoadParam(arg));
		} else {
			self.ir.emit(Ir::LoadLocal(self.locals[slot_ref.usize()].unwrap()));
		}
	}
	
	fn emit_load_lifted_slot(&mut self, slot_ref: FunctionSlotRef) {
		if slot_ref.depth() == 0 {
			self.emit_load_slot(slot_ref.slot());
		} else if let SlotState::Lifted(index) = self.ctx.get_slot(slot_ref).state {
			self.ir.emit(Ir::LoadLifted(index, slot_ref.depth()));
		} else {
			panic!();
		}
	}
	
	fn emit_store(&mut self, ident: &'a Ident) {
		match ident.state.get() {
			IdentState::Scoped | IdentState::Global(..) => {
				if self.env_count > 0 {
					self.ir.emit(Ir::StoreEnv(ident.name));
				} else {
					self.ir.emit(Ir::StoreGlobal(ident.name));
				}
			}
			IdentState::Slot(slot_ref) => self.emit_store_slot(slot_ref),
			IdentState::LiftedSlot(slot_ref) => self.emit_store_lifted_slot(slot_ref),
			IdentState::Arg(_, index) | IdentState::LiftedArg(_, index) | IdentState::ScopedArg(_, index) => {
				self.emit_load_arguments(ident.state.get());
				self.ir.emit(Ir::StoreName(Name::from_index(index as usize)));
			}
			IdentState::LoadFunction(..) => self.ir.emit(Ir::Pop),
			IdentState::None => panic!()
		}
	}
	
	fn emit_load_arguments(&mut self, state: IdentState) {
		match state {
			IdentState::Arg(slot_ref, _) => self.emit_load_slot(slot_ref),
			IdentState::LiftedArg(slot_ref, _) => self.emit_load_lifted_slot(slot_ref),
			IdentState::ScopedArg(depth, _) => self.ir.emit(Ir::LoadEnvArguments(depth)),
			_ => panic!()
		}
	}
	
	fn emit_store_slot(&mut self, slot_ref: SlotRef) {
		let slot = &self.block_state.slots[slot_ref.usize()];
		if let SlotState::Lifted(index) = slot.state {
			self.ir.emit(Ir::StoreLifted(index, 0));
		} else if let Some(arg) = slot.arg {
			self.ir.emit(Ir::StoreParam(arg));
		} else {
			self.ir.emit(Ir::StoreLocal(self.locals[slot_ref.usize()].unwrap()));
		}
	}
	
	fn emit_store_lifted_slot(&mut self, slot_ref: FunctionSlotRef) {
		if slot_ref.depth() == 0 {
			self.emit_store_slot(slot_ref.slot());
		} else if let SlotState::Lifted(index) = self.ctx.get_slot(slot_ref).state {
			self.ir.emit(Ir::StoreLifted(index, slot_ref.depth()));
		} else {
			panic!();
		}
	}
	
	fn emit_member_target(&mut self, expr: &'a Expr, index: &'a ExprSeq, dup_target: bool) -> JsResult<()> {
		try!(self.emit_expr(expr, true));
		if dup_target {
			self.ir.emit(Ir::Dup);
		}
		try!(self.emit_exprs(index, true));
		self.ir.emit(Ir::Pick(1));
		self.ir.emit(Ir::ValidateMemberTarget);
		self.ir.emit(Ir::ToPropertyKey);
		
		Ok(())
	}
}

struct FunctionFinder<F> {
	callback: F
}

impl<'a, F> AstVisitor<'a> for FunctionFinder<F>
	where F : FnMut(&'a Item) -> ()
{
	fn visit_item_function(&mut self, item: &'a Item) {
		(&mut self.callback)(item);
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test() {
		println!("{}", parse(r#"
		function f(a, b, c) {
			return a * b * c;
		}
		function f(a, b,c,d,e,f) {
			return b === c ? d : e === f;
		}
		function f(a, b, c) {
			return a + b * c;
		}
		function f(a, b, c) {
			return a * b + c;
		}
		function f(a) {
			return a + function () { };
		}
		function f(a, b) {
			return a + b['a'](1, 2, 3);
		}
"#));
	}
	
	fn parse(js: &str) -> String {
		let mut ctx = IrContext::new();
		ctx.parse_string(js, false, false).ok();
		
		let mut ir = String::new();
		ctx.print_ir(&mut ir).ok();
		
		ir
	}
}
