use gc::{GcWalker, GcWalk, ptr_t};
use rt::{JsType};
use rt::{GC_ARRAY_STORE, GC_ENTRY, GC_HASH_STORE, GC_ITERATOR, GC_OBJECT};
use rt::{GC_SCOPE, GC_STRING, GC_U16, GC_U32, GC_VALUE};
use std::mem::transmute;

pub struct Walker;

#[inline(always)]
unsafe fn is_value_ptr(ptr: ptr_t, offset: usize) -> bool {
	let ptr = transmute::<_, *const usize>(ptr).offset(offset as isize);
	let ty = transmute::<_, *const JsType>(ptr);
	(*ty).is_ptr()
	
}

#[cfg(target_pointer_width = "64")]
impl GcWalker for Walker {
	fn walk(&self, ty: u32, ptr: ptr_t, index: u32) -> GcWalk {
		unsafe {
			match ty {
				GC_ARRAY_STORE => {
					match index {
						2 | 3 => GcWalk::Pointer,
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
						2 if is_value_ptr(ptr, 1) => GcWalk::Pointer,
						9 | 10 | 12 => GcWalk::Pointer,
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
					GcWalk::Skip
				}
				GC_U32 => {
					GcWalk::Skip
				}
				GC_VALUE => {
					match index {
						1 if is_value_ptr(ptr, 0) => GcWalk::Pointer,
						_ => GcWalk::Skip
					}
				}
				_ => panic!("unmapped GC type")
			}
		}
	}
}
