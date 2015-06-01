use gc::{Array, AsArray, GcHeap, ArrayRoot};
use std::ops::{Deref, DerefMut};

pub struct ArrayLocal<T> {
	handle: *const Array<T>
}

impl<T> ArrayLocal<T> {
	pub unsafe fn new(handle: *const Array<T>) -> ArrayLocal<T> {
		ArrayLocal {
			handle: handle
		}
	}
	
	pub fn as_root(&self, heap: &GcHeap) -> ArrayRoot<T> {
		unsafe { ArrayRoot::new(heap, *self) }
	}
}

impl<T> Copy for ArrayLocal<T> { }

impl<T> Clone for ArrayLocal<T> {
	fn clone(&self) -> ArrayLocal<T> {
		ArrayLocal {
			handle: self.handle
		}
	}
}

impl<T> Deref for ArrayLocal<T> {
	type Target = [T];
	
	fn deref(&self) -> &[T] {
		unsafe { &**self.handle }
	}
}

impl<T> DerefMut for ArrayLocal<T> {
	fn deref_mut(&mut self) -> &mut [T] {
		unsafe { &mut **(self.handle as *mut Array<T>) }
	}
}

impl<T> AsArray<T> for ArrayLocal<T> {
	fn as_ptr(&self) -> Array<T> {
		unsafe { *self.handle }
	}
}
