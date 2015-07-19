use gc::{GcRootWalker, ptr_t};
use gc::os::Memory;
use rt::{JsEnv, JsRawValue, JsValue, JsType};
use std::mem::{size_of, transmute};
use std::ptr;
use std::cell::Cell;

const STACK : usize = 8192;

pub struct Stack {
    stack: Memory,
    sp: Cell<ptr_t>,
    end: ptr_t
}

impl Stack {
    pub fn new() -> Stack {
        let stack = Memory::alloc(STACK).unwrap();
        let (sp, end) = unsafe { (stack.ptr(), stack.ptr().offset(STACK as isize)) };
        
        Stack {
            stack: stack,
            sp: Cell::new(sp),
            end: end
        }
    }
    
    pub fn create_walker(&self) -> Box<GcRootWalker> {
        unsafe {
            let start = self.stack.ptr();
            let end = self.sp.get();
            
            tracegc!("creating stack walker from {:?} to {:?}", start, end);

            Box::new(StackWalker {
                ptr: transmute(start),
                end: transmute(end)
            })
        }
    }
    
    pub fn create_frame(&self, size: usize) -> StackFrame {
        StackFrame {
            sp: unsafe { self.sp.get().offset(-((size_of::<JsRawValue>() * size) as isize)) }
        }
    }
    
    pub fn drop_frame(&self, frame: StackFrame) {
        self.sp.set(frame.sp);
    }
    
    pub fn push(&self, value: JsRawValue) {
        if self.sp.get() == self.end {
            panic!("stack overflow");
        }
        
        unsafe {
            *transmute::<_, *mut JsRawValue>(self.sp.get()) = value;
            self.sp.set(self.sp.get().offset(size_of::<JsRawValue>() as isize));
        }
    }
    
    #[allow(dead_code)]
    pub fn pop(&self) -> JsRawValue {
        unsafe {
            let sp = self.sp.get().offset(-(size_of::<JsRawValue>() as isize));
            self.sp.set(sp);
            *transmute::<_, *mut JsRawValue>(sp)
        }
    }
}

#[derive(Copy)]
pub struct StackFrame {
    sp: ptr_t
}

impl StackFrame {
    pub fn get(&self, env: &JsEnv, offset: usize) -> JsValue {
        self.raw_get(offset).as_value(env)
    }
    
    pub fn raw_get(&self, offset: usize) -> JsRawValue {
        unsafe {
            *transmute::<_, *mut JsRawValue>(
                self.sp.offset((size_of::<JsRawValue>() * offset) as isize)
            )
        }
    }
    
    pub fn set(&self, offset: usize, value: JsRawValue) {
        unsafe {
            *transmute::<_, *mut JsRawValue>(
                self.sp.offset((size_of::<JsRawValue>() * offset) as isize)
            ) = value
        }
    }
}

impl Clone for StackFrame {
    fn clone(&self) -> StackFrame {
        StackFrame {
            sp: self.sp
        }
    }
}

struct StackWalker {
    ptr: *mut ptr_t,
    end: *mut ptr_t
}

impl GcRootWalker for StackWalker {
    unsafe fn next(&mut self) -> *mut ptr_t {
        while self.ptr < self.end {
            let ptr = transmute::<_, *mut usize>(self.ptr);
            self.ptr = transmute(self.ptr.offset(2));
            
            let ty = *transmute::<_, *const JsType>(ptr);
            if ty.is_ptr() {
                return transmute(ptr.offset(1));
            }
        }
        
        ptr::null_mut()
    }
}
