extern crate libc;

use gc::os::Memory;
use self::libc::c_void;
use super::JsValue;
use std::mem::{size_of, transmute};

const STACK : usize = 8192;

pub struct Stack {
	stack: Memory,
	sp: *mut c_void,
}

impl Stack {
	pub fn new() -> Stack {
		let stack = Memory::alloc(STACK).unwrap();
		let sp = unsafe { stack.ptr() };
		
		Stack {
			stack: stack,
			sp: sp
		}
	}
	
	pub fn create_frame(&self, size: usize) -> StackFrame {
		StackFrame {
			sp: unsafe { self.sp.offset(-((size_of::<JsValue>() * size) as isize)) }
		}
	}
	
	pub fn drop_frame(&mut self, frame: StackFrame) {
		self.sp = frame.sp;
	}
	
	pub fn push(&mut self, value: JsValue) {
		unsafe {
			*transmute::<_, *mut JsValue>(self.sp) = value;
			self.sp = self.sp.offset(size_of::<JsValue>() as isize);
		}
	}
	
	pub fn pop(&mut self) -> JsValue {
		unsafe {
			self.sp = self.sp.offset(-(size_of::<JsValue>() as isize));
			*transmute::<_, *mut JsValue>(self.sp)
		}
	}
}

pub struct StackFrame {
	sp: *mut c_void
}

impl StackFrame {
	pub fn get(&self, offset: usize) -> JsValue {
		unsafe {
			*transmute::<_, *mut JsValue>(
				self.sp.offset((size_of::<JsValue>() * offset) as isize)
			)
		}
	}
	
	pub fn set(&self, offset: usize, value: JsValue) {
		unsafe {
			*transmute::<_, *mut JsValue>(
				self.sp.offset((size_of::<JsValue>() * offset) as isize)
			) = value
		}
	}
}
