#![allow(unused_variables)]

pub mod ir;

use syntax::ast::*;
use syntax::token::Lit;
use self::ir::Ir;
use util::interner::StrInterner;
use std::collections::HashMap;

pub enum IrError {
	Message(String)
}

pub type IrResult<T> = Result<T, IrError>;

#[derive(Copy, Clone)]
struct NamedLabel {
	name: Option<Name>,
	label: ir::Label,
	try_catch_depth: i32
}

#[derive(Copy, Clone)]
struct ReturnTarget {
	local: ir::Local,
	label: ir::Label
}

struct IrLocalScope {
	locals: HashMap<Name, ir::Local>
}

struct IrGenerator<'a> {
	ir: ir::IrBuilder,
	break_targets: Vec<NamedLabel>,
	continue_targets: Vec<NamedLabel>,
	locals: Vec<IrLocalScope>,
	interner: &'a mut StrInterner,
	global: bool,
	try_catch_depth: i32,
	return_target: Option<ReturnTarget>
}

impl<'a> IrGenerator<'a> {
	fn new(global: bool, interner: &mut StrInterner) -> IrGenerator {
		let mut generator = IrGenerator {
			ir: ir::IrBuilder::new(),
			break_targets: Vec::new(),
			continue_targets: Vec::new(),
			locals: Vec::new(),
			interner: interner,
			global: global,
			try_catch_depth: 0,
			return_target: None
		};
		
		generator.push_local_scope();
		
		generator
	}
	
	fn named_label(&mut self, label: &Option<Ident>) -> NamedLabel {
		let name = match label {
			&Some(ref ident) => Some(ident.name),
			_ => None
		};
		
		NamedLabel {
			name: name,
			label: self.ir.label(),
			try_catch_depth: self.try_catch_depth
		}
	}
	
	fn fatal<T>(&self, message: &str) -> IrResult<T> {
		// Panic here under debug to get a stack trace.
		
		if cfg!(not(ndebug)) {
			panic!(message.to_string());
		}
		
		Err(IrError::Message(message.to_string()))
	}
	
	fn get_target(&self, targets: &Vec<NamedLabel>, ident: &Option<Ident>) -> IrResult<(ir::Label, i32)> {
		match ident {
			&Some(ref ident) => {
				for target in targets.iter().rev() {
					if target.name.is_some() && target.name.unwrap() == ident.name {
						return Ok((target.label, target.try_catch_depth));
					}
				}
				
				self.fatal(&format!("Cannot find break/continue target {:?}", ident.name.as_str(self.interner)))
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
	
	fn push_local_scope(&mut self) {
		self.locals.push(IrLocalScope {
			locals: HashMap::new()
		});
	}
	
	fn pop_local_scope(&mut self) {
		self.locals.pop();
	}
	
	fn find_local(&self, ident: &Ident) -> Option<ir::Local> {
		// Do we have a local by this name?
		
		for ref scope in self.locals.iter().rev() {
			if let Some(local) = scope.locals.get(&ident.name) {
				return Some(*local);
			}
		}
		
		None
	}
	
	fn new_local(&mut self, ident: &Ident) -> ir::Local {
		if let Some(local) = self.find_local(ident) {
			local
		} else {
			let local = self.ir.local(Some(ident.name));
			
			let top = self.locals.len() - 1;
			self.locals[top].locals.insert(ident.name, local);
			
			local
		}
	}
	
	fn emit_block(&mut self, block: &Block, outer: bool) -> IrResult<()> {
		for stmt in &block.stmts {
			try!(self.emit_stmt(&stmt));
		}
		
		if outer {
			self.ir.emit(Ir::LoadUndefined);
			self.ir.emit(Ir::Return);
			
			if let Some(return_target) = self.return_target {
				self.ir.mark(return_target.label);
				self.ir.emit(Ir::LoadLocal(return_target.local));
				self.ir.emit(Ir::Return);
			}
		}
		
		Ok(())
	}
	
	fn emit_stmts(&mut self, stmts: &Vec<Item>) -> IrResult<()> {
		for ref stmt in stmts {
			try!(self.emit_stmt(stmt));
		}
		
		Ok(())
	}
	
	fn emit_stmt(&mut self, stmt: &Item) -> IrResult<()> {
		match stmt {
			&Item::Block(ref block) => self.emit_block(block, false),
			&Item::Break(ref ident) => self.emit_break_continue(ident, true),
			&Item::Continue(ref ident) => self.emit_break_continue(ident, false),
			&Item::Debugger => self.emit_debugger(),
			&Item::Do(ref label, ref expr, ref stmt) => self.emit_do(label, expr, stmt),
			&Item::Empty => Ok(()),
			&Item::ExprStmt(ref exprs) => self.emit_expr_stmt(exprs),
			&Item::For(ref label, ref init, ref test, ref incr, ref stmt) => self.emit_for(label, init, test, incr, stmt),
			&Item::ForIn(ref label, ref expr, ref in_, ref stmt) => self.emit_for_in(label, expr, in_, stmt),
			&Item::ForVar(ref label, ref init, ref test, ref incr, ref stmt) => self.emit_for_var(label, init, test, incr, stmt),
			&Item::ForVarIn(ref label, ref var, ref in_, ref stmt) => self.emit_for_var_in(label, var, in_, stmt),
			&Item::Function(ref name, ref args, ref block) => self.emit_function(name, args, block),
			&Item::If(ref test, ref then, ref else_) => self.emit_if(test, then, if let &Some(ref else_) = else_ { Some(else_) } else { None }),
			&Item::Return(ref exprs) => self.emit_return(exprs),
			&Item::Switch(ref label, ref exprs, ref clauses) => self.emit_switch(label, exprs, clauses),
			&Item::Throw(ref exprs) => self.emit_throw(exprs),
			&Item::Try(ref try, ref catch, ref finally) => self.emit_try(try, catch, finally),
			&Item::VarDecl(ref vars) => self.emit_var_decl(vars),
			&Item::While(ref label, ref expr, ref stmt) => self.emit_while(label, expr, stmt),
			&Item::With(ref exprs, ref stmt) => self.emit_with(exprs, stmt)
		}
	}
	
	fn emit_break_continue(&mut self, ident: &Option<Ident>, is_break: bool) -> IrResult<()> {
		let targets = if is_break { &self.break_targets } else { &self.continue_targets };
		
		let (target, try_catch_depth) = try!(self.get_target(targets, ident));
		
		if try_catch_depth == self.try_catch_depth {
			self.ir.emit(Ir::Jump(target));
		} else {
			self.ir.emit(Ir::Leave(target));
		}
		
		Ok(())
	}
	
	fn emit_debugger(&mut self) -> IrResult<()> {
		self.ir.emit(Ir::Debugger);
		
		Ok(())
	}
	
	fn emit_do(&mut self, label: &Option<Ident>, expr: &Expr, stmt: &Item) -> IrResult<()> {
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

	fn emit_expr_stmt(&mut self, exprs: &ExprSeq) -> IrResult<()> {
		self.emit_exprs(exprs, false)
	}
	
	fn emit_for(&mut self, label: &Option<Ident>, init: &Option<ExprSeq>, test: &Option<ExprSeq>, incr: &Option<ExprSeq>, stmt: &Item) -> IrResult<()> {
		self.emit_for_init(
			label,
			|generator| {
				if let &Some(ref init) = init {
					try!(generator.emit_exprs(init, false))
				}
				
				Ok(())
			},
			test,
			incr,
			stmt
		)
	}
	
	fn emit_for_var(&mut self, label: &Option<Ident>, init: &Option<Vec<Var>>, test: &Option<ExprSeq>, incr: &Option<ExprSeq>, stmt: &Item) -> IrResult<()> {
		self.emit_for_init(
			label,
			|generator| {
				if let &Some(ref init) = init {
					try!(generator.emit_var_decl(init));
				}
				
				Ok(())
			},
			test,
			incr,
			stmt
		)
	}
	
	fn emit_for_init<F: Fn(&mut IrGenerator) -> IrResult<()>>(&mut self, label: &Option<Ident>, init: F, test: &Option<ExprSeq>, incr: &Option<ExprSeq>, stmt: &Item) -> IrResult<()> {
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
		
		if let &Some(ref incr) = incr {
			try!(self.emit_exprs(incr, false));
		}
		
		self.ir.mark(test_label);
		
		if let &Some(ref test) = test {
			try!(self.emit_tests(test, body_label, false));
		} else {
			self.ir.emit(Ir::Jump(body_label));
		}
		
		self.ir.mark(break_target.label);
		
		self.break_targets.pop();
		self.continue_targets.pop();
		
		Ok(())
	}
	
	fn emit_for_in(&mut self, label: &Option<Ident>, expr: &Expr, in_: &ExprSeq, stmt: &Item) -> IrResult<()> {
		self.emit_for_in_init(
			label,
			|generator| generator.emit_expr_get_local(expr),
			in_,
			stmt
		)
	}
	
	fn emit_expr_get_local(&mut self, expr: &Expr) -> IrResult<ir::Local> {
		match expr {
			&Expr::Ident(ref ident) => Ok(self.new_local(ident)),
			_ => self.fatal("Invalid expression for for in expression")
		}
	}
	
	fn emit_for_var_in(&mut self, label: &Option<Ident>, var: &Ident, in_: &ExprSeq, stmt: &Item) -> IrResult<()> {
		self.emit_for_in_init(
			label,
			|generator| Ok(generator.new_local(var)),
			in_,
			stmt
		)
	}
	
	fn emit_for_in_init<F: Fn(&mut IrGenerator) -> IrResult<ir::Local>>(&mut self, label: &Option<Ident>, expr: F, in_: &ExprSeq, stmt: &Item) -> IrResult<()> {
		let break_target = self.named_label(label);
		self.break_targets.push(break_target);
		let continue_target = self.named_label(label);
		self.continue_targets.push(continue_target);
		
		let local = try!(expr(self));
		let iter = self.ir.local(None);
		let next_label = self.ir.label();
		
		try!(self.emit_exprs(in_, true));
		self.ir.emit(Ir::IntoIter(iter));
		
		self.ir.start_try();

		self.ir.emit(Ir::Jump(continue_target.label));
		
		self.ir.mark(next_label);
		
		self.ir.emit(Ir::CurrentIter(iter));
		self.ir.emit(Ir::SetLocal(local));
		
		try!(self.emit_stmt(stmt));
		
		self.ir.mark(continue_target.label);
		
		self.ir.emit(Ir::NextIter(iter, next_label));
		
		self.ir.end_try();
		self.ir.start_finally();
		
		self.ir.emit(Ir::EndIter(iter));
		
		self.ir.end_finally();
		
		self.ir.mark(break_target.label);
		
		self.break_targets.pop();
		self.continue_targets.pop();
		
		Ok(())
	}
	
	fn emit_function(&mut self, name: &Option<Ident>, args: &Vec<Ident>, block: &Block) -> IrResult<()> {
		panic!();
	}
	
	fn emit_if(&mut self, test: &ExprSeq, then: &Item, else_: Option<&Item>) -> IrResult<()> {
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
	
	fn emit_return(&mut self, exprs: &Option<ExprSeq>) -> IrResult<()> {
		if let &Some(ref exprs) = exprs {
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
			
			self.ir.emit(Ir::SetLocal(return_target.local));
			self.ir.emit(Ir::Leave(return_target.label));
		} else {
			self.ir.emit(Ir::Return);
		}
		
		Ok(())
	}
	
	fn emit_switch(&mut self, label: &Option<Ident>, exprs: &ExprSeq, clauses: &Vec<SwitchClause>) -> IrResult<()> {
		let break_target = self.named_label(label);
		self.break_targets.push(break_target);
		
		let local = self.ir.local(None);
		
		try!(self.emit_exprs(exprs, true));
		self.ir.emit(Ir::SetLocal(local));
		
		let mut targets = Vec::new();
		let mut default = None;
		
		for clause in clauses {
			match clause {
				&SwitchClause::Case(ref exprs, _) => {
					let label = self.ir.label();
					targets.push(label);
					
					self.ir.emit(Ir::LoadLocal(local));
					try!(self.emit_exprs(exprs, true));
					self.ir.emit(Ir::JumpEq(label));
				}
				&SwitchClause::Default(ref stmts) => {
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
			
			let stmts = match &clauses[i] {
				&SwitchClause::Case(_, ref stmts) => stmts,
				&SwitchClause::Default(ref stmts) => stmts
			};
			
			try!(self.emit_stmts(stmts));
		}
		
		self.ir.mark(break_target.label);
		
		self.break_targets.pop();
		
		Ok(())
	}
	
	fn emit_throw(&mut self, exprs: &ExprSeq) -> IrResult<()> {
		try!(self.emit_exprs(exprs, true));
		
		self.ir.emit(Ir::Throw);
		
		Ok(())
	}
	
	fn emit_try(&mut self, try: &Block, catch: &Option<Catch>, finally: &Option<Block>) -> IrResult<()> {
		self.try_catch_depth += 1;
		
		let after_label = self.ir.label();
		
		self.ir.start_try();
		
		try!(self.emit_block(try, false));
		
		self.ir.emit(Ir::Leave(after_label));
		
		self.ir.end_try();
		
		if let &Some(ref catch) = catch {
			self.ir.start_catch();
			
			self.push_local_scope();
			
			let exception = self.new_local(&catch.ident);
			
			self.ir.emit(Ir::LoadException);
			self.ir.emit(Ir::SetLocal(exception));
			
			try!(self.emit_block(&catch.block, false));
			
			self.pop_local_scope();
			
			self.ir.end_catch();
		}
		
		if let &Some(ref finally) = finally {
			self.ir.start_finally();
			
			try!(self.emit_block(finally, false));
			
			self.ir.end_finally();
		}
		
		self.ir.mark(after_label);
		
		self.try_catch_depth -= 1;
		
		Ok(())
	}
	
	fn emit_var_decl(&mut self, vars: &Vec<Var>) -> IrResult<()> {
		for var in vars {
			if let Some(ref expr) = var.expr {
				try!(self.emit_expr(expr, true));
			} else {
				self.ir.emit(Ir::LoadUndefined);
			}
				
			let local = self.new_local(&var.ident);
			self.ir.emit(Ir::SetLocal(local));
		}
		
		Ok(())
	}
	
	fn emit_while(&mut self, label: &Option<Ident>, expr: &Expr, stmt: &Item) -> IrResult<()> {
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
	
	fn emit_with(&mut self, exprs: &ExprSeq, stmt: &Item) -> IrResult<()> {
		panic!();
	}
	
	fn emit_exprs(&mut self, exprs: &ExprSeq, leave: bool) -> IrResult<()> {
		let len = exprs.exprs.len();
		
		for i in 0..len {
			try!(self.emit_expr(&exprs.exprs[i], leave && i == len - 1));
		}
		
		Ok(())
	}
	
	fn emit_tests(&mut self, exprs: &ExprSeq, label: ir::Label, inverse: bool) -> IrResult<()> {
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
	
	fn emit_expr(&mut self, expr: &Expr, leave: bool) -> IrResult<()> {
		match expr {
			&Expr::ArrayLiteral(ref exprs) => self.emit_expr_array_literal(exprs, leave),
			&Expr::Assign(op, ref lhs, ref rhs) => self.emit_expr_assign(op, lhs, rhs, leave),
			&Expr::Binary(op, ref lhs, ref rhs) => self.emit_expr_binary(op, lhs, rhs, leave),
			&Expr::Call(ref expr, ref args) => self.emit_expr_call(expr, args, leave),
			&Expr::Function(ref name, ref args, ref block) => self.emit_expr_function(name, args, block, leave),
			&Expr::Ident(ref ident) => self.emit_expr_ident(ident, leave),
			&Expr::Literal(ref lit) => self.emit_expr_literal(&lit, leave),
			&Expr::MemberDot(ref expr, ref ident) => self.emit_expr_member_dot(expr, ident, leave),
			&Expr::MemberIndex(ref expr, ref index) => self.emit_expr_member_index(expr, index, leave),
			&Expr::Missing => self.emit_expr_missing(leave),
			&Expr::New(ref expr, ref args) => self.emit_expr_new(expr, args, leave),
			&Expr::ObjectLiteral(ref props) => self.emit_expr_object_literal(props, leave),
			&Expr::Paren(ref exprs) => self.emit_expr_paren(exprs, leave),
			&Expr::Ternary(ref test, ref then, ref else_) => self.emit_expr_ternary(test, then, else_, leave),
			&Expr::This => self.emit_expr_this(leave),
			&Expr::Unary(op, ref expr) => self.emit_expr_unary(op, expr, leave)
		}
	}
	
	fn emit_test(&mut self, expr: &Expr, label: ir::Label, inverse: bool) -> IrResult<()> {
		// TODO: This can be optimized to use actual opcodes instead of just coercing to true/false.
		
		try!(self.emit_expr(expr, true));
		
		if inverse {
			self.ir.emit(Ir::JumpFalse(label));
		} else {
			self.ir.emit(Ir::JumpTrue(label));
		}
		
		Ok(())
	}
	
	fn emit_expr_array_literal(&mut self, exprs: &Vec<Expr>, leave: bool) -> IrResult<()> {
		if leave {
			self.ir.emit(Ir::NewArray);
			
			if exprs.len() > 0 {
				let local = self.ir.local(None);
			
				self.ir.emit(Ir::SetLocal(local));
				
				for expr in exprs {
					self.ir.emit(Ir::LoadLocal(local));
					try!(self.emit_expr(expr, true));
					self.ir.emit(Ir::PushArray);
				}
				
				self.ir.emit(Ir::LoadLocal(local));
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
			Op::And => self.ir.emit(Ir::And),
			Op::BitAnd => self.ir.emit(Ir::BitAnd),
			Op::BitNot => self.ir.emit(Ir::BitNot),
			Op::BitOr => self.ir.emit(Ir::BitOr),
			Op::BitXOr => self.ir.emit(Ir::BitXOr),
			Op::Delete => self.ir.emit(Ir::Delete),
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
			Op::Or => self.ir.emit(Ir::Or),
			Op::Positive => self.ir.emit(Ir::Positive),
			Op::RightShiftArithmetic => self.ir.emit(Ir::Rsh),
			Op::RightShiftLogical => self.ir.emit(Ir::RshZeroFill),
			Op::Subtract => self.ir.emit(Ir::Subtract),
			Op::Typeof => self.ir.emit(Ir::Typeof),
			_ => panic!("Invalid operator")
		}
	}
	
	fn emit_expr_assign(&mut self, op: Op, lhs: &Expr, rhs: &ExprSeq, leave: bool) -> IrResult<()> {
		let load = |generator: &mut IrGenerator| {
			if op == Op::Assign {
				try!(generator.emit_exprs(rhs, true));
			} else {
				try!(generator.emit_expr(lhs, true));
				try!(generator.emit_exprs(rhs, true));
				
				generator.emit_op(op);
			}
			
			Ok(())
		};
		
		match lhs {
			&Expr::Ident(ref ident) => {
				try!(load(self));
				
				if let Some(local) = self.find_local(ident) {
					self.ir.emit(Ir::SetLocal(local));
				} else {
					self.ir.emit(Ir::SetGlobal(ident.name));
				}
			},
			&Expr::MemberDot(ref expr, ref ident) => {
				try!(self.emit_expr(expr, true));
				try!(load(self));
				
				self.ir.emit(Ir::SetName(ident.name));
			},
			&Expr::MemberIndex(ref expr, ref index) => {
				try!(self.emit_expr(expr, true));
				try!(self.emit_exprs(index, true));
				try!(load(self));
				
				self.ir.emit(Ir::SetIndex);
			}
			_ => return self.fatal("Invalid assignment expression")
		}
		
		Ok(())
	}
	
	fn emit_expr_binary(&mut self, op: Op, lhs: &Expr, rhs: &Expr, leave: bool) -> IrResult<()> {
		if leave {
			try!(self.emit_expr(lhs, true));
			try!(self.emit_expr(rhs, true));
			self.emit_op(op);
		} else {
			try!(self.emit_expr(lhs, false));
			try!(self.emit_expr(rhs, false));
		}
		
		Ok(())
	}
	
	fn emit_expr_call(&mut self, expr: &Expr, args: &Vec<Expr>, leave: bool) -> IrResult<()> {
		try!(self.emit_expr(expr, true));
		
		for arg in args {
			try!(self.emit_expr(arg, true));
		}
		
		self.ir.emit(Ir::Call(args.len() as u32));
		
		if !leave {
			self.ir.emit(Ir::Pop);
		}
		
		Ok(())
	}
	
	fn emit_expr_function(&mut self, name: &Option<Ident>, args: &Vec<Ident>, block: &Block, leave: bool) -> IrResult<()> {
		panic!();
	}
	
	fn emit_expr_ident(&mut self, ident: &Ident, leave: bool) -> IrResult<()> {
		if leave {
			if let Some(local) = self.find_local(ident) {
				self.ir.emit(Ir::LoadLocal(local))
			} else {
				self.ir.emit(Ir::LoadGlobal(ident.name))
			}
		}
		
		Ok(())
	}
	
	fn emit_expr_literal(&mut self, lit: &Lit, leave: bool) -> IrResult<()> {
		if leave {
			match lit {
				&Lit::Null => self.ir.emit(Ir::LoadNull),
				&Lit::Boolean(value) => self.ir.emit(if value { Ir::LoadTrue } else { Ir::LoadFalse }),
				&Lit::String(ref value) => self.ir.emit(Ir::LoadString(value.clone())),
				&Lit::Integer(value) => self.ir.emit(Ir::LoadI32(value)),
				&Lit::Long(value) => self.ir.emit(Ir::LoadI64(value)),
				&Lit::Double(value) => self.ir.emit(Ir::LoadF64(value)),
				&Lit::Regex(ref body, ref flags) => self.ir.emit(Ir::LoadRegex(body.clone(), flags.clone()))
			}
		}
		
		Ok(())
	}
	
	fn emit_expr_member_dot(&mut self, expr: &Expr, ident: &Ident, leave: bool) -> IrResult<()> {
		try!(self.emit_expr(expr, leave));
		
		if leave {
			self.ir.emit(Ir::LoadName(ident.name))
		}
		
		Ok(())
	}
	
	fn emit_expr_member_index(&mut self, expr: &Expr, index: &ExprSeq, leave: bool) -> IrResult<()> {
		try!(self.emit_expr(expr, leave));
		try!(self.emit_exprs(index, leave));
		
		if leave {
			self.ir.emit(Ir::LoadIndex);
		}
		
		Ok(())
	}
	
	fn emit_expr_missing(&mut self, leave: bool) -> IrResult<()> {
		if leave {
			self.ir.emit(Ir::LoadMissing);
		}
		
		Ok(())
	}
	
	fn emit_expr_new(&mut self, expr: &Expr, args: &Option<Vec<Expr>>, leave: bool) -> IrResult<()> {
		try!(self.emit_expr(expr, true));
		
		if let &Some(ref args) = args {
			for arg in args {
				try!(self.emit_expr(arg, true));
			}
			
			self.ir.emit(Ir::New(args.len() as u32));
		} else {
			self.ir.emit(Ir::New(0));
		}
		
		if !leave {
			self.ir.emit(Ir::Pop);
		}
		
		Ok(())
	}
	
	fn emit_expr_object_literal(&mut self, props: &Vec<Property>, leave: bool) -> IrResult<()> {
		panic!();
	}
	
	fn emit_expr_paren(&mut self, exprs: &ExprSeq, leave: bool) -> IrResult<()> {
		self.emit_exprs(exprs, leave)
	}
	
	fn emit_expr_ternary(&mut self, test: &Expr, then: &Expr, else_: &Expr, leave: bool) -> IrResult<()> {
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
	
	fn emit_expr_this(&mut self, leave: bool) -> IrResult<()> {
		if leave {
			self.ir.emit(Ir::LoadThis);
		}
		
		Ok(())
	}
	
	fn emit_expr_unary(&mut self, op: Op, expr: &Expr, leave: bool) -> IrResult<()> {
		panic!();
	}
}

pub fn build_ir(program: &Program, interner: &mut StrInterner) -> IrResult<ir::Block> {
	let mut generator = IrGenerator::new(true, interner);
	
	try!(generator.emit_block(&program.items, true));
	
	Ok(generator.ir.build())
}

#[cfg(test)]
mod test {
	use super::*;
	use syntax::lexer::Lexer;
	use syntax::parser::Parser;
	use syntax::reader::StringReader;
	use syntax::token::keywords;

	#[test]
	fn test() {
		println!("{}", parse("f(1 + 2);"));
		println!("{}", parse("
			if (i > a) {
				f(true);
			} else {
				g(false);
			}
		"));
		assert!(false);
	}
	
	fn parse(js: &str) -> String {
		let mut interner = keywords::new_interner();
		let mut reader = StringReader::new("global.js", js);
		let mut lexer = Lexer::new(&mut reader, &mut interner, false).ok().unwrap();
		let program = {
			let mut parser = Parser::new(&mut lexer, &mut interner);
			parser.parse_program().ok().unwrap()
		};
		let block = build_ir(&program, &mut interner).ok().unwrap();
		
		let mut parsed = String::new();
		block.print(&mut parsed, true, &mut interner);
		parsed
	}
}
