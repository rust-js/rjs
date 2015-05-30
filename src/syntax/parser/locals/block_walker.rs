use syntax::ast::{RootBlock, Block};

pub struct BlockWalker<'a, T> {
	scopes: Vec<BlockScope<'a, T>>
}

struct BlockScope<'a, T> {
	root_block: &'a RootBlock,
	state: T,
	blocks: Vec<Option<&'a Block>>
}

impl<'a, T> BlockWalker<'a, T> {
	pub fn new() -> BlockWalker<'a, T> {
		BlockWalker {
			scopes: Vec::new()
		}
	}
	
	pub fn scopes_len(&self) -> usize {
		self.scopes.len()
	}
	
	pub fn top_scope(&self) -> &'a RootBlock {
		let len = self.scopes.len();
		self.scopes[len - 1].root_block
	}
	
	pub fn push_scope(&mut self, root_block: &'a RootBlock, state: T) {
		self.scopes.push(BlockScope {
			root_block: root_block,
			state: state,
			blocks: Vec::new()
		});
	}
	
	pub fn pop_scope(&mut self) -> T {
		self.scopes.pop().unwrap().state
	}
	
	pub fn push_block(&mut self, block: Option<&'a Block>) {
		let len = self.scopes.len();
		self.scopes[len - 1].blocks.push(block);
	}
	
	pub fn pop_block(&mut self) {
		let len = self.scopes.len();
		self.scopes[len - 1].blocks.pop().unwrap();
	}
	
	pub fn block_walk_rev<F>(&mut self, walk: F) -> bool
		where F : Fn(&'a RootBlock, usize, Option<&'a Block>, usize, &mut T) -> bool
	{
		let scope_len = self.scopes.len();
		for scope_idx in (0..scope_len).rev() {
			let block_len = self.scopes[scope_idx].blocks.len();
			
			for block_idx in (0..block_len).rev() {
				let result = walk(
					self.scopes[scope_idx].root_block,
					scope_idx,
					self.scopes[scope_idx].blocks[block_idx],
					block_idx,
					&mut self.scopes[scope_idx].state
				);
				
				if !result {
					return false;
				}
			}
		}
		
		true
	}
	
	pub fn scope_walk_rev<F: Fn(&'a RootBlock, usize, &mut T) -> bool>(&mut self, walk: F) -> bool {
		let scope_len = self.scopes.len();
		for scope_idx in (0..scope_len).rev() {
			let result = walk(
				&self.scopes[scope_idx].root_block,
				scope_idx,
				&mut self.scopes[scope_idx].state
			);
			
			if !result {
				return false;
			}
		}
		
		true
	}
}
