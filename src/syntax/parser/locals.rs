use syntax::ast::*;
use syntax::ast::visitor::AstVisitor;
use syntax::token::name;
use std::cell::RefCell;

pub struct LocalResolver<'a> {
	context: &'a AstContext,
	scopes: Vec<LocalResolverScope<'a>>
}

struct LocalResolverScope<'a> {
	function_ref: FunctionRef,
	blocks: Vec<&'a Block>,
	locals: &'a RefCell<Locals>,
	had_arguments: bool,
	build_scope: bool,
	takes_scope: bool,
	slot_count: u32
}

impl<'a> LocalResolver<'a> {
	pub fn resolve(context: &'a AstContext, program_ref: FunctionRef) {
		let mut resolver = LocalResolver {
			scopes: Vec::new(),
			context: context
		};
		
		let program = &context.functions[program_ref.usize()];
		
		resolver.resolve_scope(&program.block, program_ref);
	}

	fn resolve_function(&mut self, function_ref: FunctionRef) {
		let function = &self.context.functions[function_ref.usize()];
		
		self.resolve_scope(&function.block, function_ref);
	}

	fn resolve_scope(&mut self, block: &'a RootBlock, function_ref: FunctionRef) {
		self.scopes.push(LocalResolverScope {
			function_ref: function_ref,
			blocks: vec![&block.block],
			locals: &block.locals,
			had_arguments: false,
			build_scope: false,
			takes_scope: false,
			slot_count: 0
		});
		
		self.visit_block(&block.block);
		
		let scope = self.scopes.pop().unwrap();
		
		if scope.had_arguments {
			block.has_arguments.set(true);
		}
		if scope.takes_scope {
			block.takes_scope.set(true);
		}
		if scope.build_scope {
			block.scope.set(Some(scope.slot_count as u32));
		}
	}

	fn resolve_ident(&mut self, ident: &'a Ident) {
		// TODO: This is wrong. If we capture a catch local, it won't get registered
		// correctly. A way to fix this is to have all locals register with the outer
		// block and use something like slots which the locals refer to. At the moment
		// we have different sets of locals and these should/could be resolved into
		// a single set.
	
		// If we've already resolved this one, skip it.
		
		let state = ident.state.get();
		if !state.is_none() {
			return;
		}
		
		if ident.name == name::ARGUMENTS {
			ident.state.set(IdentState::Arguments);
			let len = self.scopes.len();
			self.scopes[len - 1].had_arguments = true;
			return;
		}
		
		// Walk over all scopes in reverse to find the scope the local is in.
		
		let scopes = self.scopes.len();
		
		'outer: for i in (0..scopes).rev() {
			let scope = &mut self.scopes[i];
			let blocks = &scope.blocks;
			
			// Walk over all blocks in reverse to find the block where the local is in.
			
			for j in (0..blocks.len()).rev() {
				// If we're at the top block of the top scope, it's a global.
				
				if i == 0 && j == 0 {
					break 'outer;
				}
				
				// Otherwise, see whether we have the local at this block of this scope.
				
				let locals = &blocks[j].locals;
				
				if let Some(local_slot_ref) = locals.get(&ident.name) {
					// Calculate the depth of the stack frame and update the ident state.
					
					let depth = scopes - i - 1;
					
					ident.state.set(if depth == 0 {
						IdentState::Slot(*local_slot_ref)
					} else {
						IdentState::LiftedSlot(scope.function_ref, *local_slot_ref, depth as u32)
					});
					
					// If the depth is greater than zero, it's lifted and we need to update the block state.
					
					if depth > 0 {
						scope.locals.borrow_mut().slots[local_slot_ref.usize()].lifted = Some(scope.slot_count);
						scope.slot_count += 1;
					}
					
					break 'outer;
				}
			}
		}
		
		match ident.state.get() {
			IdentState::None => ident.state.set(IdentState::Global),
			IdentState::LiftedSlot(_, _, depth) => self.mark_build_slot(depth),
			_ => {}
		};
	}
	
	fn mark_build_slot(&mut self, mut depth: u32) {
		let mut offset = self.scopes.len() - 1;
		self.scopes[offset].takes_scope = true;
		offset -= 1;
		while depth > 0 {
			self.scopes[offset].build_scope = true;
			if depth > 1 {
				self.scopes[offset].takes_scope = true;
			}
			depth -= 1;
			offset -= 1;
		}
	}

	fn resolve_set_ident(&mut self, ident: &'a Ident) {
		self.resolve_ident(ident);
	}
	
	fn resolve_get_ident(&mut self, ident: &'a Ident) {
		self.resolve_ident(ident);
	}
}

impl<'a> AstVisitor<'a> for LocalResolver<'a> {
	fn visit_expr_assign(&mut self, expr: &'a Expr) {
		if let &Expr::Assign(_, ref lhs, ref rhs) = expr {
			if let &Expr::Ident(ref ident) = &**lhs {
				self.resolve_set_ident(ident);
			} else {
				self.visit_expr(lhs);
			}
			
			self.visit_expr_seq(rhs);
		}
	}
	
	fn visit_expr_ident(&mut self, expr: &'a Expr) {
		if let &Expr::Ident(ref ident) = expr {
			self.resolve_get_ident(ident);
		}
	}
	
	fn visit_item_function(&mut self, item: &'a Item) {
		if let &Item::Function(ref ident, function_ref) = item {
			self.resolve_set_ident(ident);
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_property_getter(&mut self, property: &'a Property) {
		if let &Property::Getter(_, function_ref) = property {
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_property_setter(&mut self, property: &'a Property) {
		if let &Property::Setter(_, function_ref) = property {
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_expr_function(&mut self, expr: &'a Expr) {
		if let &Expr::Function(function_ref) = expr {
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_catch(&mut self, catch: &'a Catch) {
		let len = self.scopes.len();
		self.scopes[len - 1].blocks.push(&catch.block);
		
		self.resolve_set_ident(&catch.ident);
		self.visit_block(&catch.block);
		
		self.scopes[len - 1].blocks.pop();
	}
	
	fn visit_root_block(&mut self, _: &'a RootBlock) {
		panic!();
	}
	
	fn visit_item_for_var_in(&mut self, item: &'a Item) {
		if let &Item::ForVarIn(_, ref ident, ref in_, ref stmt) = item {
			self.resolve_set_ident(ident);
			self.visit_expr_seq(in_);
			self.visit_item(stmt);
		}
	}
	
	fn visit_var(&mut self, var: &'a Var) {
		self.resolve_set_ident(&var.ident);
		
		if let &Some(ref expr) = &var.expr {
			self.visit_expr(expr);
		}
	}
}
