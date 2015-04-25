// See https://github.com/jemalloc/jemalloc/blob/dev/src/chunk_mmap.c for how jemalloc allocates memory.

extern crate libc;

use self::libc::*;
use std::ptr;

#[cfg(target_os = "windows")]
unsafe fn map(addr: *const c_void, size: usize) -> *mut libc::c_void {
	assert!(size != 0);
	
	/*
	 * If VirtualAlloc can't allocate at the given address when one is
	 * given, it fails and returns NULL.
	 */
	let ret = VirtualAlloc(ptr::null_mut(), size as size_t, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
	
	assert!(
		ret == ptr::null_mut() ||
		(addr == ptr::null() && ret as *const c_void != addr) ||
		(addr != ptr::null() && ret as *const c_void == addr)
	);
	
	ret
}

/* LINUX VERSION
static void *
pages_map(void *addr, size_t size)
{
	void *ret;

	assert(size != 0);

	/ *
	 * We don't use MAP_FIXED here, because it can cause the *replacement*
	 * of existing mappings, and we only want to create new mappings.
	 * /
	ret = mmap(addr, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON,
	    -1, 0);
	assert(ret != NULL);

	if (ret == MAP_FAILED)
		ret = NULL;
	else if (addr != NULL && ret != addr) {
		/ *
		 * We succeeded in mapping memory, but not in the right place.
		 * /
		pages_unmap(ret, size);
		ret = NULL;
	}
	
	assert(ret == NULL || (addr == NULL && ret != addr)
	    || (addr != NULL && ret == addr));
	return (ret);
}
*/

#[cfg(target_os = "windows")]
unsafe fn unmap(addr: *mut c_void, _: usize) {
	if VirtualFree(addr, 0, MEM_RELEASE) == 0 {
		panic!("Error in VirtualFree");
	}
}

/* LINUX VERSION
static void
pages_unmap(void *addr, size_t size)
{

	if (munmap(addr, size) == -1)
	{
		char buf[BUFERROR_BUF];

		buferror(get_errno(), buf, sizeof(buf));
		malloc_printf("<jemalloc>: Error in "
		              "munmap"
		              "(): %s\n", buf);
		if (opt_abort)
			abort();
	}
}
*/

pub struct Memory {
	ptr: *mut libc::c_void,
	size: usize
}

impl Memory {
	pub fn empty() -> Memory {
		Memory {
			ptr: ptr::null_mut(),
			size: 0
		}
	}
	
	pub fn alloc(size: usize) -> Option<Memory> {
		let ptr = unsafe { map(ptr::null(), size) };
		if ptr.is_null() {
			None
		} else {
			Some(Memory {
				ptr: ptr,
				size: size
			})
		}
	}
	
	pub unsafe fn ptr(&self) -> *mut libc::c_void {
		self.ptr
	}
	
	pub fn size(&self) -> usize {
		self.size
	}
}

impl Drop for Memory {
	fn drop(&mut self) {
		if self.size > 0 {
			unsafe { unmap(self.ptr, self.size) };
		}
	}
}
