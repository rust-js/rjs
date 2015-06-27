extern crate libc;
extern crate time;

use gc::strategy::Strategy;
use gc::os::Memory;
use gc::{GcRootWalker, GcOpts, GcMemHeader, GcWalker, GcWalk, ptr_t};
use std::ptr;
use std::mem::{size_of, transmute, swap};
use std::cmp::max;

const PAGE_SIZE : usize = 4 * 1024;

struct Header {
    forward: ptr_t,
    size: usize
}

impl Header {
    fn new(size: usize) -> Header {
        Header {
            forward: ptr::null(),
            size: size
        }
    }
    
    unsafe fn from_ptr<'a>(ptr: ptr_t) -> &'a mut Header {
        transmute(ptr.offset(-((size_of::<Header>() + size_of::<GcMemHeader>()) as isize)))
    }
    
    unsafe fn offset_from_user(ptr: ptr_t) -> ptr_t {
        ptr.offset(-((size_of::<Header>() + size_of::<GcMemHeader>()) as isize))
    }
    
    unsafe fn offset_to_user(ptr: ptr_t) -> ptr_t {
        ptr.offset((size_of::<Header>() + size_of::<GcMemHeader>()) as isize)
    }
}

struct Block {
    memory: Memory,
    offset: usize,
    high_mark: usize
}

impl Block {
    unsafe fn alloc(&mut self, size: usize) -> ptr_t {
        let size = size + size_of::<Header>();
        
        if self.offset + size > self.high_mark {
            return ptr::null_mut();
        }
        
        let memory = self.memory.ptr().offset(self.offset as isize);
        
        (*(memory as *mut Header)) = Header::new(size);
        
        self.offset += size;
        
        memory.offset(size_of::<Header>() as isize)
    }
}

pub struct Copying {
    opts: GcOpts,
    from: Block,
    to: Memory,
    last_used: f64,
    last_failed: usize
}

impl Copying {
    pub fn new(opts: GcOpts) -> Copying {
        let memory = Memory::alloc(opts.initial_heap).unwrap();
        let high_mark = (opts.initial_heap * (opts.init_gc * 100.0) as usize) / 100;
        
        Copying {
            opts: opts,
            from: Block {
                memory: memory,
                offset: 0,
                high_mark: high_mark
            },
            to: Memory::empty(),
            last_used: 0.0,
            last_failed: 0
        }
    }
    
    unsafe fn copy(&mut self, mut walkers: Vec<Box<GcRootWalker>>, walker: &GcWalker) {
        // Calculate the new size of the heap. We use the fill factor of the previous
        // run as a basis and ensure that we have at least enough room to accept the
        // allocation that failed last (were we not able to reclaim any memory).
        //
        // The room we allocate comes down to the current allocated memory times the
        // fill factor times the growth factor. The growth factor is taken from
        // the configuration. 
        
        // We need at least enough room to fit the last allocation. If we're not
        // able to reclaim any memory, the current offset plus the last allocation
        // need to fit.
        let mut target_size = max(self.from.memory.size(), self.from.offset + self.last_failed);

        tracegc!("last offset {} last failed {} target size {}", self.from.offset, self.last_failed, target_size);
        
        // If we had a fill of more than 50% last time, adjust with the growth factor.
        // If the growth factor is over 85%, we choose the fast growth factor. Otherwise
        // the slow one.
        if self.last_used > 0.5 {
            let growth_factor = if self.last_used > 0.85 {
                self.opts.fast_growth_factor
            } else {
                self.opts.slow_growth_factor
            };
            
            target_size = (target_size * (growth_factor * 100.0) as usize) / 100;
            tracegc!("last used {} over 50% target size {} growth factor {}", self.last_used, target_size, growth_factor);
        }

        // The minimum is set to the last used size.
        target_size = max(target_size, (target_size * (self.last_used * 100.0) as usize) / 100);
        tracegc!("last used {} target size {}", self.last_used, target_size);
        
        // We don't shrink beyond the initial heap size.
        target_size = max(target_size, self.opts.initial_heap);
        tracegc!("initial heap {} target_size {}", self.opts.initial_heap, target_size);
        
        self.last_failed = 0;
        
        target_size = (target_size + (PAGE_SIZE - 1)) & !(PAGE_SIZE - 1);
        
        // Ensure that the target heap is large enough.
        
        if self.to.size() < target_size {
            // First set to empty to first release our allocated memory.
            self.to = Memory::empty();
            self.to = Memory::alloc(target_size).unwrap();
        }
        
        let mut forwarder = Forwarder {
            target: self.to.ptr()
        };
        
        // Walk all GC roots.
        
        for walker in &mut walkers {
            loop {
                let ptr = walker.next();
                if ptr.is_null() {
                    break;
                }
                
                let from = *ptr;
                *ptr = forwarder.forward(*ptr);
                tracegc!("forwarded {:?} to {:?}", from, *ptr);
            }
        }
        
        // Walk the to space.
        
        let mut ptr = Header::offset_to_user(self.to.ptr());
        
        while ptr < forwarder.target {
            let header = Header::from_ptr(ptr);
            let gc_header = GcMemHeader::from_ptr(ptr);
            let ty = gc_header.get_type_id();
            let size = gc_header.get_size();
            let ptrs = size / size_of::<usize>();
            
            if gc_header.is_array() {
                let count = *transmute::<_, *const usize>(ptr);

                let mut child = ptr.offset(size_of::<usize>() as isize);
                let end = child.offset((count * size) as isize);
                
                let mut index = 0;
                
                while child < end {
                    tracegc!("processing index {}", index);
                    index += 1;
                    process_block(child, ty, ptrs, &mut forwarder, walker);
                    
                    child = child.offset(size as isize);
                }
                
            } else {
                process_block(ptr, ty, ptrs, &mut forwarder, walker);
            }
            
            ptr = ptr.offset(header.size as isize);
        }
        
        // Swap the from and to space.
        
        self.from.offset = forwarder.target as usize - self.to.ptr() as usize;
        swap(&mut self.from.memory, &mut self.to);
        self.from.high_mark = (self.from.memory.size() * (self.opts.init_gc * 100.0) as usize) / 100;
        
        // Calculate the current fill rate.
        
        self.last_used = self.from.offset as f64 / self.from.memory.size() as f64;
        tracegc!("from offset {} from size {} last used {}", self.from.offset, self.from.memory.size(), self.last_used);
    }
}

struct Forwarder {
    target: ptr_t
}

impl Forwarder {
    unsafe fn forward(&mut self, ptr: ptr_t) -> ptr_t {
        let header = Header::from_ptr(ptr);
        
        if header.forward.is_null() {
            header.forward = self.target;
            
            *(self.target as *mut Header) = Header::new(header.size);
            
            let from = Header::offset_from_user(ptr).offset(size_of::<Header>() as isize);
            let to = transmute(self.target.offset(size_of::<Header>() as isize));
            let size = header.size - size_of::<Header>();
            
            ptr::copy(from, to, size);
            
            self.target = self.target.offset(header.size as isize);
        }
        
        Header::offset_to_user(header.forward)
    }
}

unsafe fn process_block(ptr: ptr_t, ty: u32, ptrs: usize, forwarder: &mut Forwarder, walker: &GcWalker) {
    for i in 0..ptrs {
        match walker.walk(ty, ptr, i as u32) {
            GcWalk::End => return,
            GcWalk::Skip => {},
            GcWalk::Pointer => {
                let offset = (ptr as *mut ptr_t).offset(i as isize);
                let child = *offset;
                tracegc!("forwarding {:?} of type {:?} at {:?} index {}", child, ty, ptr, i);
                if !child.is_null() {
                    *offset = forwarder.forward(child);
                }
            }
        }
    }
}

impl Strategy for Copying {
    unsafe fn alloc_raw(&mut self, size: usize) -> ptr_t {
        // Round the size to the next pointer.
        let size = (size + (size_of::<usize>() - 1)) & !(size_of::<usize>() - 1);
        
        let result = self.from.alloc(size);
        
        if result.is_null() {
            self.last_failed = size;
        } else {
            ptr::write_bytes(transmute::<_, *mut u8>(result), 0, size);
        }
        
        result
    }
    
    fn mem_allocated(&self) -> usize {
        self.from.memory.size() + self.to.size()
    }
    
    fn mem_used(&self) -> usize {
        self.from.offset
    }
    
    fn gc(&mut self, walkers: Vec<Box<GcRootWalker>>, walker: &GcWalker) {
        let start = time::precise_time_ns();
        
        tracegc!("=== GC === start");
        
        unsafe {
            self.copy(walkers, walker);
        }
        
        let elapsed = (time::precise_time_ns() - start) / 1_000_000;

        println!("=== GC === allocated {} ({}) used {} ms {}", nice_size(self.from.memory.size()), nice_size(self.mem_allocated()), nice_size(self.mem_used()), elapsed);
    }
}

fn nice_size(size: usize) -> String {
    if size < 1024 {
        format!("{} B", size)
    } else {
        let mut size = size as f64 / 1024.0;
        if size < 1024.0 {
            format!("{:.1} KB", size)
        } else {
            size /= 1024.0;
            if size < 1024.0 {
                format!("{:.1} MB", size)
            } else {
                size /= 1024.0;
                format!("{:.1} GB", size)
            }
        }
    }
}
