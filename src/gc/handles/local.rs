use gc::{Ptr, Root, AsPtr, GcHeap};
use std::ops::{Deref, DerefMut};

pub struct Local<T> {
	handle: *const Ptr<T>
}

impl<T> Local<T> {
	pub unsafe fn new(handle: *const Ptr<T>) -> Local<T> {
		Local {
			handle: handle
		}
	}
	
	pub fn as_root(&self, heap: &GcHeap) -> Root<T> {
		unsafe { Root::new(heap, *self) }
	}
}

impl<T> Copy for Local<T> {}

impl<T> Clone for Local<T> {
	fn clone(&self) -> Local<T> {
		Local {
			handle: self.handle
		}
	}
}

impl<T> Deref for Local<T> {
	type Target = T;
	
	fn deref(&self) -> &T {
		unsafe { &**self.handle }
	}
}

impl<T> DerefMut for Local<T> {
	fn deref_mut(&mut self) -> &mut T {
		unsafe { &mut **(self.handle as *mut Ptr<T>) }
	}
}

impl<T> AsPtr<T> for Local<T> {
	fn as_ptr(&self) -> Ptr<T> {
		unsafe { *self.handle }
	}
}
