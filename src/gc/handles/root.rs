use gc::{Ptr, Local, RootHandles, GcHeap, AsPtr};
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;
use std::mem::transmute;
use std::rc::Rc;

pub struct Root<T> {
	handles: Rc<RootHandles>,
	handle: u32,
	_type: PhantomData<T>
}

impl<T> Root<T> {
	pub unsafe fn new<U: AsPtr<T>>(heap: &GcHeap, ptr: U) -> Root<T> {
		Root {
			handles: heap.handles.clone(),
			handle: heap.handles.add(ptr.as_ptr().ptr()),
			_type: PhantomData
		}
	}
	
	pub fn as_local(&self, heap: &GcHeap) -> Local<T> {
		heap.alloc_local_from_ptr(self.as_ptr())
	}
}

impl<T> Deref for Root<T> {
	type Target = T;
	
	fn deref(&self) -> &T {
		unsafe {
			let ptr = self.handles.get_target(self.handle);
			transmute(ptr)
		}
	}
}

impl<T> DerefMut for Root<T> {
	fn deref_mut(&mut self) -> &mut T {
		unsafe { 
			let ptr = self.handles.get_target(self.handle);
			transmute(ptr)
		}
	}
}

impl<T> Clone for Root<T> {
	fn clone(&self) -> Root<T> {
		Root {
			handles: self.handles.clone(),
			handle: self.handles.clone_root(self.handle),
			_type: PhantomData
		}
	}
}

impl<T> Drop for Root<T> {
	fn drop(&mut self) {
		self.handles.remove(self.handle);
	}
}

impl<T> AsPtr<T> for Root<T> {
	fn as_ptr(&self) -> Ptr<T> {
		unsafe { Ptr::from_ptr(self.handles.get_target(self.handle)) }
	}
}
