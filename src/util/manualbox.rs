use std::mem::transmute;
use std::ops::{Deref, DerefMut};
use std::ptr;

// HACK: This struct is used for manual memory management. The idea is
// that it behaves like a Box, except that it does not implement Drop.
// Instead, the exposed drop method must be called manually. This is
// used in the garbage collector. If we need a reference to a heap object
// in the garbage collector, we can't have a Drop implementation.
// Instead, we manually free the memory in the finalizer. This struct
// uses Box as a basis to implement allocation and freeing, but transmutes
// the box to a raw pointer in between.

pub struct ManualBox<T> {
    ptr: *mut T
}

impl<T> ManualBox<T> {
    pub fn new(value: T) -> ManualBox<T> {
        ManualBox {
            ptr: unsafe { transmute(Box::new(value)) }
        }
    }
    
    pub fn drop(&mut self) {
        unsafe { transmute::<_, Box<T>>(self.ptr); }
        
        self.ptr = ptr::null_mut();
    }
}

impl<T> Deref for ManualBox<T> {
    type Target = T;
    
    fn deref<'a>(&'a self) -> &'a T {
        if self.ptr.is_null() {
            panic!("dereferencing empty ManualBox");
        }
        
        unsafe { transmute(self.ptr) }
    }
}

impl<T> DerefMut for ManualBox<T> {
    fn deref_mut<'a>(&'a mut self) -> &'a mut T {
        if self.ptr.is_null() {
            panic!("dereferencing empty ManualBox");
        }
        
        unsafe { transmute(self.ptr) }
    }
}
