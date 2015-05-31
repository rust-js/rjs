pub mod copying;

extern crate libc;

use gc::{RootWalker, GcWalker, ptr_t};

pub trait Strategy {
	unsafe fn alloc_raw(&mut self, size: usize) -> ptr_t;
	
	fn mem_allocated(&self) -> usize;
	
	fn mem_used(&self) -> usize;
	
	fn gc(&mut self, walkers: Vec<Box<RootWalker>>, walker: &GcWalker);
}
