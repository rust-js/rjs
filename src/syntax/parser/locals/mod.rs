use syntax::ast::*;
use syntax::ast::visitor::AstVisitor;
use self::block_walker::*;
use syntax::token::name;
use ::{JsResult, JsError};

// See https://github.com/pvginkel/rjs/wiki/Scopes-%28TD%29 for a description
// of the algorithm that is implemented here.

mod block_walker;

pub struct LocalResolver<'a> {
	context: &'a AstContext,
	walker: BlockWalker<'a, Scope>,
	illegal_arguments_eval: bool,
	illegal_delete: bool
}

struct Scope {
	function_ref: FunctionRef,
	lifted_slot_count: usize,
	arguments: Option<SlotRef>
}

impl<'a> LocalResolver<'a> {
	pub fn resolve(context: &'a AstContext, program_ref: FunctionRef) -> JsResult<()> {
		let mut resolver = LocalResolver {
			walker: BlockWalker::new(),
			context: context,
			illegal_arguments_eval: false,
			illegal_delete: false
		};
		
		let program = &context.functions[program_ref.usize()];
		
		resolver.resolve_scope(&program.block, program_ref, true);
		
		if resolver.illegal_arguments_eval {
			Err(JsError::Parse("Illegal arguments or eval in strict mode".to_string()))
		} else if resolver.illegal_delete {
			Err(JsError::Parse("Cannot delete declared variable".to_string()))
		} else {
			Ok(())
		}
	}
	
	fn resolve_function(&mut self, function_ref: FunctionRef) {
		let function = &self.context.functions[function_ref.usize()];
		
		self.resolve_scope(&function.block, function_ref, false);
	}

	fn resolve_scope(&mut self, block: &'a RootBlock, function_ref: FunctionRef, global: bool) {
		self.walker.push_scope(
			&block,
			Scope {
				function_ref: function_ref,
				lifted_slot_count: 0,
				arguments: block.block.locals.get(&name::ARGUMENTS).map(|p| *p)
			}
		);
		
		self.walker.push_block(Some(&block.block));
		
		self.visit_block(&block.block);
		
		let state = self.walker.pop_scope();
		
		let transform = {
			let mut block_state = block.state.borrow_mut();
			
			// Mark the scope as building a thin scope if it has lifted slots.
			
			if state.lifted_slot_count > 0 && block_state.build_scope != ScopeType::Thick {
				block_state.build_scope = ScopeType::Thin(state.lifted_slot_count as u32);
			}
			
			// If this scope is building a thick scope, make sure that all
			// idents are scoped and that all nested functions build a thick
			// scope if they were taking a thin scope.
			
			if block_state.build_scope == ScopeType::Thick {
				// Reset all slots that were marked as local. These have been
				// marked by the parser.
				
				for slot in &mut block_state.slots {
					slot.state = SlotState::Scoped;
				}
				
				true
			} else {
				false
			}
		};
		
		if transform {
			ThickScopeTransformer::transform(self.context, &block, global);
		}
	}

	fn resolve_ident(&mut self, ident: &'a Ident, store: bool) {
		// If we've already resolved this one, skip it.
		
		let state = ident.state.get();
		if !state.is_none() {
			return;
		}
		
		let strict = self.walker.top_scope().strict;
		
		if
			(ident.name == name::ARGUMENTS || ident.name == name::EVAL) &&
			store &&
			strict
		{
			self.illegal_arguments_eval = true;
		}
		
		// Walk over all scopes in reverse to find the scope the local is in.
		
		let scopes_len = self.walker.scopes_len();
		
		self.walker.block_walk_rev(|scope, scope_idx, block, block_idx, state| {
			let mut block_state = scope.state.borrow_mut();
			
			// If we're at the top block of the top scope, it's a global.
			
			if scope_idx == 0 && block_idx == 0 {
				if ident.state.get() == IdentState::None {
					if block_state.take_scope {
						ident.state.set(IdentState::Scoped);
					} else {
						ident.state.set(IdentState::Global(block.unwrap().locals.contains_key(&ident.name)));
					}
				}
				
				return false;
			}
			
			// If this scope builds a thick scope, the identifier is scoped
			// and we can stop the search.
			
			if block_state.build_scope == ScopeType::Thick {
				ident.state.set(IdentState::Scoped);
			}
			
			// See whether we have the local at this block of this scope.
			
			if let Some(block) = block {
				if let Some(local_slot_ref) = block.locals.get(&ident.name) {
					// Calculate the depth of the stack frame and update the ident state.
					
					let depth = scopes_len - scope_idx - 1;
					
					// Handle param access. If there is an arguments object,
					// we need to resolve access towards that object.
					
					let slot = &mut block_state.slots[local_slot_ref.usize()];
					
					if !strict && slot.arg.is_some() && state.arguments.is_some() {
						let index = slot.arg.unwrap();
						let arguments = state.arguments.unwrap();
						
						if ident.state.get() == IdentState::Scoped {
							ident.state.set(IdentState::ScopedArg(depth as u32, index));
						} else if depth > 0 {
							let slot_ref = FunctionSlotRef(state.function_ref, arguments, depth as u32);
							ident.state.set(IdentState::LiftedArg(slot_ref, index));
						} else {
							ident.state.set(IdentState::Arg(arguments, index));
						}
					} else if ident.state.get() != IdentState::Scoped {
						// Handle normal local access. Scoped variables don't
						// need a depth.
						
						if depth == 0 {
							ident.state.set(IdentState::Slot(*local_slot_ref));
						} else {
							ident.state.set(
								IdentState::LiftedSlot(FunctionSlotRef(state.function_ref, *local_slot_ref, depth as u32))
							);
							
							// If the depth is greater than zero, it's lifted and we need to update the block state.
							
							if slot.state == SlotState::Local {
								slot.state = SlotState::Lifted(state.lifted_slot_count as u32);
								state.lifted_slot_count += 1;
							}
						}
					}
					
					return false;
				}
			}
			
			true
		});
		
		assert!(ident.state.get() != IdentState::None);
		
		match ident.state.get() {
			IdentState::LiftedSlot(slot_ref) | IdentState::LiftedArg(slot_ref, _) => {
				self.mark_build_thin_scope(slot_ref.depth());
			},
			IdentState::Scoped | IdentState::ScopedArg(..) => {
				self.mark_build_thick_scope();
			}
			_ => {}
		};
	}
	
	fn mark_build_thin_scope(&mut self, depth: u32) {
		// When we're lifting a variable, the complete chain from the current
		// function until the function we're lifting from needs to participate
		// in building thin scopes.
		
		assert!(depth > 0);
		
		let scopes_len = self.walker.scopes_len();
		let target = scopes_len - depth as usize - 1;
		
		self.walker.scope_walk_rev(|scope, scope_idx, _| {
			let mut block_state = scope.state.borrow_mut();
			
			// Our level needs to take a scope.
			
			if scope_idx == scopes_len - 1 {
				block_state.take_scope = true;
			} else {
				// When building thin scopes, nothing in the chain can have
				// a thick scope.
				
				assert!(block_state.build_scope != ScopeType::Thick);
				
				// If this block already is building a scope, we can stop iterating.
				
				if block_state.build_scope != ScopeType::None {
					return false;
				}
				
				// Mark this scope as building an empty thin scope so it
				// can pass the matched scope to our scope.
				
				block_state.build_scope = ScopeType::Thin(0);
				
				// If this is not the last level, the scope also needs to take
				// a scope so the chain can be built.
				
				if scope_idx > target {
					block_state.take_scope = true;
				}
			}
			
			// Stop iterating when we're at the correct depth.
			
			scope_idx != target
		});
	}
	
	fn mark_build_thick_scope(&mut self) {
		// When the variable is a scoped variable (i.e. matches a deopt scope),
		// the complete chain needs to build a thick scope. The loop is in
		// reverse so that we start from our function and stop when we
		// find the first one that is already building a thick scope.
		// No sense in marking them again.
		
		self.walker.scope_walk_rev(|scope, _, _| {
			let mut block_state = scope.state.borrow_mut();
			
			// Stop marking scopes if it's already building a thick scope.
			
			if block_state.build_scope == ScopeType::Thick {
				return false;
			}
			
			// Mark the block as building a thick scope and taking a scope.
			// We don't have to check for depth here because a thick scope
			// chain always reaches the global scope.
			
			block_state.build_scope = ScopeType::Thick;
			block_state.take_scope = true;
			
			true
		});
	}
	
	fn unwrap_paren(&self, expr: &'a Expr) -> &'a Expr {
		if let Expr::Paren(ref exprs) = *expr {
			if exprs.exprs.len() == 1 {
				return &exprs.exprs[0];
			}
		}
		
		expr
	}
}

impl<'a> AstVisitor<'a> for LocalResolver<'a> {
	fn visit_expr_assign(&mut self, expr: &'a Expr) {
		if let Expr::Assign(_, ref lhs, ref rhs) = *expr {
			if let Expr::Ident(ref ident) = *self.unwrap_paren(lhs) {
				self.resolve_ident(ident, true);
			} else {
				self.visit_expr(lhs);
			}
			
			self.visit_expr(rhs);
		}
	}
	
	fn visit_expr_ident(&mut self, expr: &'a Expr) {
		if let Expr::Ident(ref ident) = *expr {
			self.resolve_ident(ident, false);
		}
	}
	
	fn visit_item_function(&mut self, item: &'a Item) {
		if let Item::Function(ref ident, function_ref) = *item {
			self.resolve_ident(ident, true);
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_property_getter(&mut self, property: &'a Property) {
		if let Property::Getter(_, function_ref) = *property {
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_property_setter(&mut self, property: &'a Property) {
		if let Property::Setter(_, function_ref) = *property {
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_expr_function(&mut self, expr: &'a Expr) {
		if let Expr::Function(function_ref) = *expr {
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_catch(&mut self, catch: &'a Catch) {
		self.walker.push_block(Some(&catch.block));
		
		self.resolve_ident(&catch.ident, true);
		self.visit_block(&catch.block);
		
		self.walker.pop_block();
	}
	
	fn visit_item_with(&mut self, item: &'a Item) {
		if let Item::With(ref exprs, ref stmt) = *item {
			self.visit_expr_seq(exprs);
			
			self.walker.push_block(None);
			
			self.visit_item(stmt);
			
			self.walker.pop_block();
		}
	}
	
	fn visit_root_block(&mut self, _: &'a RootBlock) {
		panic!();
	}
	
	fn visit_item_for_var_in(&mut self, item: &'a Item) {
		if let Item::ForVarIn(_, ref ident, ref in_, ref stmt) = *item {
			self.resolve_ident(ident, true);
			self.visit_expr_seq(in_);
			self.visit_item(stmt);
		}
	}
	
	fn visit_var(&mut self, var: &'a Var) {
		self.resolve_ident(&var.ident, true);
		
		if let Some(ref expr) = var.expr {
			self.visit_expr(expr);
		}
	}
	
	fn visit_expr_unary(&mut self, expr: &'a Expr) {
		if let Expr::Unary(op, ref expr) = *expr {
			self.visit_expr(expr);
			
			let strict = self.walker.top_scope().strict;
			
			if strict && op == Op::Delete {
				if let Expr::Ident(ref ident) = **expr {
					match ident.state.get() {
						IdentState::Global(true) | IdentState::Slot(..) | IdentState::LiftedSlot(..) => {
							self.illegal_delete = true;
						}
						IdentState::Global(false) | IdentState::Scoped => {
							match ident.name {
								name::OBJECT_CLASS | name::FUNCTION_CLASS | name::ARRAY_CLASS |
								name::STRING_CLASS | name::BOOLEAN_CLASS | name::NUMBER_CLASS |
								name::MATH_CLASS | name::DATE_CLASS | name::REGEXP_CLASS |
								name::ERROR_CLASS | name::EVAL_ERROR_CLASS | name::RANGE_ERROR_CLASS |
								name::REFERENCE_ERROR_CLASS | name::SYNTAX_ERROR_CLASS |
								name::TYPE_ERROR_CLASS | name::URI_ERROR_CLASS | name::NATIVE_ERROR_CLASS |
								name::JSON_CLASS => {
									self.illegal_delete = true;
								}
								_ => {}
							}
						}
						_ => {}
					}
				}
			}
		}
	}
}

struct ThickScopeTransformer<'a> {
	context: &'a AstContext,
	root_block: &'a RootBlock,
	global: bool
}

// While resolving, the scope type may have changed from a none/thin
// scope to a thick scope. Because of this, we may need to fix up the
// current scope. This is done here.
impl<'a> ThickScopeTransformer<'a> {
	fn transform(context: &'a AstContext, root_block: &'a RootBlock, global: bool) {
		let mut transformer = ThickScopeTransformer {
			context: context,
			root_block: root_block,
			global: global
		};
		
		transformer.visit_block(&root_block.block);
	}
	
	fn resolve_ident(&mut self, ident: &'a Ident) {
		match ident.state.get() {
			IdentState::Slot(slot_ref) => {
				self.root_block.state.borrow_mut().slots[slot_ref.usize()].state = SlotState::Scoped;
				
				ident.state.set(IdentState::Scoped);
			}
			IdentState::Scoped | IdentState::ScopedArg(..) => {},
			_ => ident.state.set(IdentState::Scoped)
		}
	}
	
	fn resolve_function(&mut self, function_ref: FunctionRef) {
		// If a nested function takes a scope but is building a thin scope,
		// switch it to building a thick scope and recurse into that function.
		
		let function = &self.context.functions[function_ref.usize()];
		
		let recurse = {
			let mut state = function.block.state.borrow_mut();
			
			if state.take_scope && state.build_scope != ScopeType::Thick {
				state.build_scope = ScopeType::Thick;
				
				true
			} else {
				false
			}
		};
		
		if recurse {
			self.visit_root_block(&function.block);
		}
	}
}

impl<'a> AstVisitor<'a> for ThickScopeTransformer<'a> {
	fn visit_expr_assign(&mut self, expr: &'a Expr) {
		if let Expr::Assign(_, ref lhs, ref rhs) = *expr {
			if let Expr::Ident(ref ident) = **lhs {
				self.resolve_ident(ident);
			} else {
				self.visit_expr(lhs);
			}
			
			self.visit_expr(rhs);
		}
	}
	
	fn visit_expr_ident(&mut self, expr: &'a Expr) {
		if let Expr::Ident(ref ident) = *expr {
			self.resolve_ident(ident);
		}
	}
	
	fn visit_item_function(&mut self, item: &'a Item) {
		if let Item::Function(ref ident, function_ref) = *item {
			self.resolve_ident(ident);
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_property_getter(&mut self, property: &'a Property) {
		if let Property::Getter(_, function_ref) = *property {
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_property_setter(&mut self, property: &'a Property) {
		if let Property::Setter(_, function_ref) = *property {
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_expr_function(&mut self, expr: &'a Expr) {
		if let Expr::Function(function_ref) = *expr {
			self.resolve_function(function_ref);
		}
	}
	
	fn visit_catch(&mut self, catch: &'a Catch) {
		self.resolve_ident(&catch.ident);
		let global = self.global;
		self.global = false;
		self.visit_block(&catch.block);
		self.global = global;
	}
	
	fn visit_root_block(&mut self, root_block: &'a RootBlock) {
		let previous = self.root_block;
		self.root_block = root_block;
		let global = self.global;
		self.global = false;
		
		self.visit_block(&root_block.block);
		
		self.root_block = previous;
		self.global = global;
	}
	
	fn visit_item_for_var_in(&mut self, item: &'a Item) {
		if let Item::ForVarIn(_, ref ident, ref in_, ref stmt) = *item {
			self.resolve_ident(ident);
			self.visit_expr_seq(in_);
			self.visit_item(stmt);
		}
	}
	
	fn visit_var(&mut self, var: &'a Var) {
		self.resolve_ident(&var.ident);
		
		if let Some(ref expr) = var.expr {
			self.visit_expr(expr);
		}
	}
	
	fn visit_item_with(&mut self, item: &'a Item) {
		if let Item::With(ref exprs, ref stmt) = *item {
			let global = self.global;
			self.global = false;
			self.visit_expr_seq(exprs);
			self.visit_item(stmt);
			self.global = global;
		}
	}
}
