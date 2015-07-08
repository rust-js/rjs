use gc::{GcWalker, GcWalk, GcFinalize, GcRootWalker, ptr_t};
use rt::{JsType, JsRegExp, JsObject};
use rt::{GC_ARRAY_STORE, GC_ENTRY, GC_HASH_STORE, GC_ITERATOR, GC_OBJECT, GC_REGEXP};
use rt::{GC_SCOPE, GC_STRING, GC_U16, GC_U32, GC_VALUE, GC_SPARSE_ARRAY, GC_ARRAY_CHUNK};
use rt::stack::Stack;
use std::mem::transmute;
use std::rc::Rc;

#[inline(always)]
unsafe fn is_value_ptr(ptr: ptr_t, offset: usize) -> bool {
    let ptr = transmute::<_, *const usize>(ptr).offset(offset as isize);
    let ty = transmute::<_, *const JsType>(ptr);
    (*ty).is_ptr()
    
}

pub struct Walker {
    stack: Rc<Stack>
}

impl Walker {
    pub fn new(stack: Rc<Stack>) -> Walker {
        Walker {
            stack: stack
        }
    }
}

#[cfg(target_pointer_width = "64")]
impl GcWalker for Walker {
    fn walk(&self, ty: u32, ptr: ptr_t, index: u32) -> GcWalk {
        unsafe {
            match ty {
                GC_ARRAY_STORE => {
                    match index {
                        0 | 1 => GcWalk::Pointer,
                        _ => GcWalk::Skip
                    }
                }
                GC_ENTRY => {
                    match index {
                        3 if is_value_ptr(ptr, 2) => GcWalk::Pointer,
                        5 if is_value_ptr(ptr, 4) => GcWalk::Pointer,
                        _ => GcWalk::Skip
                    }
                }
                GC_HASH_STORE => {
                    match index {
                        0 => GcWalk::Pointer,
                        _ => GcWalk::Skip
                    }
                }
                GC_ITERATOR => {
                    match index {
                        0 | 3 => GcWalk::Pointer,
                        _ => GcWalk::Skip
                    }
                }
                GC_OBJECT => {
                    match index {
                        3 if is_value_ptr(ptr, 2) => GcWalk::Pointer,
                        6 | 7 | 9 => GcWalk::Pointer,
                        _ => GcWalk::Skip
                    }
                }
                GC_REGEXP => {
                    match index {
                        0 | 1 => GcWalk::Pointer,
                        _ => GcWalk::Skip
                    }
                }
                GC_SCOPE => {
                    match index {
                        0 => GcWalk::Pointer,
                        _ => GcWalk::Skip
                    }
                }
                GC_STRING => {
                    match index {
                        0 => GcWalk::Pointer,
                        _ => GcWalk::Skip
                    }
                }
                GC_U16 => {
                    GcWalk::EndArray
                }
                GC_U32 => {
                    GcWalk::EndArray
                }
                GC_VALUE => {
                    match index {
                        1 if is_value_ptr(ptr, 0) => GcWalk::Pointer,
                        _ => GcWalk::Skip
                    }
                }
                GC_SPARSE_ARRAY => {
                    match index {
                        0 | 1 => GcWalk::Pointer,
                        _ => GcWalk::Skip
                    }
                }
                GC_ARRAY_CHUNK => {
                    match index {
                        1 => GcWalk::Pointer,
                        _ => GcWalk::Skip
                    }
                }
                _ => panic!("unmapped GC type")
            }
        }
    }
    
    fn finalize(&self, ty: u32, ptr: ptr_t) -> GcFinalize {
        unsafe {
            match ty {
                GC_REGEXP => {
                    let regex = transmute::<_, *mut JsRegExp>(ptr);
                    (&mut *regex).finalize();
                    
                    GcFinalize::Finalized
                }
                GC_OBJECT => {
                    let object = transmute::<_, *mut JsObject>(ptr);
                    (&mut *object).finalize();
                    
                    GcFinalize::Finalized
                }
                _ => GcFinalize::NotFinalizable
            }
        }
    }
    
    fn create_root_walkers(&self) -> Vec<Box<GcRootWalker>> {
        vec![self.stack.create_walker()]
    }
}
