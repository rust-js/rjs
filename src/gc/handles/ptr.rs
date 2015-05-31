use gc::ptr_t;
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;
use std::ptr;
use std::mem::transmute;
use std::fmt;

pub struct Ptr<T> {
	ptr: ptr_t,
	_type: PhantomData<T>
}

impl<T> PartialEq for Ptr<T> {
	fn eq(&self, other: &Ptr<T>) -> bool {
		self.ptr == other.ptr
	}
}

impl<T> Copy for Ptr<T> { }

impl<T> Clone for Ptr<T> {
	fn clone(&self) -> Ptr<T> {
		Self::from_ptr(self.ptr)
	}
}

impl<T> fmt::Debug for Ptr<T> {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		write!(fmt, "Ptr {{ ptr: {:?} }}", self.ptr)
	}
}

impl<T> Ptr<T> {
	pub fn ptr(&self) -> ptr_t {
		self.ptr
	}
	
	pub fn from_ptr(ptr: ptr_t) -> Ptr<T> {
		Ptr {
			ptr: ptr,
			_type: PhantomData
		}
	}
	
	pub fn null() -> Ptr<T> {
		Self::from_ptr(ptr::null())
	}
	
	pub fn is_null(&self) -> bool {
		self.ptr.is_null()
	}
}

impl<T> Deref for Ptr<T> {
	type Target = T;
	
	fn deref(&self) -> &T {
		unsafe { transmute(self.ptr) }
	}
}

impl<T> DerefMut for Ptr<T> {
	fn deref_mut(&mut self) -> &mut T {
		unsafe { transmute(self.ptr) }
	}
}

pub trait AsPtr<T> {
	fn as_ptr(&self) -> Ptr<T>;
}

impl<T> AsPtr<T> for Ptr<T> {
	fn as_ptr(&self) -> Ptr<T> {
		*self
	}
}
