use gc::{Array, ArrayLocal, RootHandles, GcHeap, AsArray, AsPtr};
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;
use std::mem::{size_of, transmute};
use std::slice;
use std::rc::Rc;

pub struct ArrayRoot<T> {
	handles: Rc<RootHandles>,
	handle: u32,
	_type: PhantomData<T>
}

impl<'a, T> ArrayRoot<T> {
	pub unsafe fn new<U: AsArray<T>>(heap: &'a GcHeap, ptr: U) -> ArrayRoot<T> {
		ArrayRoot {
			handles: heap.handles.clone(),
			handle: heap.handles.add(ptr.as_ptr().ptr()),
			_type: PhantomData
		}
	}
	
	pub fn as_local(&self, heap: &GcHeap) -> ArrayLocal<T> {
		heap.alloc_array_local_from_ptr(self.as_ptr())
	}
}

impl<T> Deref for ArrayRoot<T> {
	type Target = [T];
	
	fn deref(&self) -> &[T] {
		unsafe {
			let ptr = self.handles.get_target(self.handle);
			let size = *transmute::<_, *const usize>(ptr);
			let ptr = ptr.offset(size_of::<usize>() as isize);
			
			slice::from_raw_parts(
				transmute(ptr),
				size
			)
		}
	}
}

impl<T> DerefMut for ArrayRoot<T> {
	fn deref_mut(&mut self) -> &mut [T] {
		unsafe {
			let ptr = self.handles.get_target(self.handle);
			let size = *transmute::<_, *const usize>(ptr);
			let ptr = ptr.offset(size_of::<usize>() as isize);
			
			slice::from_raw_parts_mut(
				transmute(ptr),
				size
			)
		}
	}
}

impl<T> Clone for ArrayRoot<T> {
	fn clone(&self) -> ArrayRoot<T> {
		ArrayRoot {
			handles: self.handles.clone(),
			handle: self.handles.clone_root(self.handle),
			_type: PhantomData
		}
	}
}

impl<T> Drop for ArrayRoot<T> {
	fn drop(&mut self) {
		self.handles.remove(self.handle);
	}
}

impl<T> AsArray<T> for ArrayRoot<T> {
	fn as_ptr(&self) -> Array<T> {
		unsafe { Array::from_ptr(self.handles.get_target(self.handle)) }
	}
}
