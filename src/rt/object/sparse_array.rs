use gc::{Array, Local, ptr_t, GcWalker};
use rt::JsEnv;
use rt::object::{JsStoreKey, Entry};
use std::cmp;
use rt::{GC_ENTRY, GC_ARRAY_CHUNK, GC_SPARSE_ARRAY, validate_walker_field};
use syntax::Name;
use std::usize;
use std::mem::{transmute, zeroed};

const CHUNK_SHIFT : usize = 5;
const CHUNK_SIZE : usize = 1 << CHUNK_SHIFT;
const INITIAL_VALUE_SIZE : usize = 20;
const INITIAL_CHUNK_COUNT : usize = 10;

pub struct SparseArray {
	items: Array<Entry>,
	chunks: Array<Chunk>,
	chunk_count: usize
}

#[derive(Copy, Clone)]
struct Chunk {
	offset: usize,
	items: Array<Entry>
}

impl Chunk {
	fn new(env: &JsEnv, offset: usize) -> Chunk {
		Chunk {
			offset: offset,
			items: unsafe { env.heap.alloc_array(GC_ENTRY, CHUNK_SIZE) }
		}
	}
}

#[derive(Copy, Clone, Debug)]
struct ChunkIndex(u32);

impl ChunkIndex {
	fn new(index: usize, found: bool) -> ChunkIndex {
		let mut store = (index & 0x7FFFFFFF) as u32;
		if found {
			store |= 0x80000000;
		}
		ChunkIndex(store)
	}
	
	fn found(&self) -> bool {
		(self.0 & 0x80000000) != 0
	}
	
	fn index(&self) -> usize {
		(self.0 & 0x7FFFFFFF) as usize
	}
}

impl SparseArray {
	pub fn new_local(env: &JsEnv) -> Local<SparseArray> {
		let mut array = env.heap.alloc_local::<SparseArray>(GC_SPARSE_ARRAY);
		
		*array = SparseArray {
			items: unsafe { env.heap.alloc_array(GC_ENTRY, INITIAL_VALUE_SIZE) },
			chunks: Array::null(),
			chunk_count: 0
		};
		
		array
	}
}

impl Local<SparseArray> {
	pub fn get_value(&self, index: usize) -> Entry {
		let items = self.items;
		
		if !items.is_null() {
			if index < items.len() {
				items[index]
			} else {
				Entry::empty()
			}
		} else {
			let offset = Self::get_offset_from_index(index);
			let chunk = self.find_chunk(offset);
			if chunk.found() {
				self.chunks[chunk.index()].items[index - offset]
			} else {
				Entry::empty()
			}
		}
	}
	
	pub fn set_value(&mut self, env: &JsEnv, index: usize, value: Entry) {
		if !self.items.is_null() {
			let len = self.items.len();
			
			if index < len {
				self.items[index] = value;
				return;
			}
			if index < len * 2 {
                // We allow the array to double in size every time
                // we grow it.
				
				self.grow_items(env);
				self.items[index] = value;
				return;
			}
			
            // We have a real array, but not enough room. Transfer the
            // values to chunks and continue.
            
            self.transfer_to_chunks(env);
		}
		
		let offset = Self::get_offset_from_index(index);
		let chunk = self.find_or_create_chunk(env, offset);
		self.chunks[chunk.index()].items[index - offset] = value;
	}
	
	fn get_offset_from_index(index: usize) -> usize {
		index & !(CHUNK_SIZE - 1)
	}
	
	fn transfer_to_chunks(&mut self, env: &JsEnv) {
		let chunk_count = (self.items.len() >> CHUNK_SHIFT) + 1;
		self.chunks = unsafe { env.heap.alloc_array(GC_ARRAY_CHUNK, INITIAL_CHUNK_COUNT) };
		
		for i in 0..chunk_count {
			let offset = i * CHUNK_SIZE;
			self.chunks[i] = Chunk::new(env, offset);
			
			let to_copy = if i < chunk_count - 1 {
				CHUNK_SIZE
			} else {
				cmp::min(self.items.len() - offset, CHUNK_SIZE)
			};
			
			Array::copy(self.items, offset, self.chunks[i].items, 0, to_copy);
		}
		
		self.items = Array::null();
		self.chunk_count = chunk_count;
	}
	
	fn grow_items(&mut self, env: &JsEnv) {
		let len = self.items.len();
		let items = unsafe { env.heap.alloc_array(GC_ENTRY, len * 2) };
		Array::copy(self.items, 0, items, 0, len);
		self.items = items;
	}
	
	fn find_or_create_chunk(&mut self, env: &JsEnv, offset: usize) -> ChunkIndex {
		let index = self.find_chunk(offset);
		
		if !index.found() {
			let chunk = Chunk::new(env, offset);
			self.insert_chunk(env, chunk, index.index());
		}
		
		index
	}
	
	fn insert_chunk(&mut self, env: &JsEnv, entry: Chunk, index: usize) {
		let chunk_count = self.chunk_count;
		
        // We never create the chunks here; they are created by TransferToChunks.
		
		assert!(chunk_count > 0);
		
		if self.chunks.len() == chunk_count {
			let new_size = cmp::max(
				(chunk_count as f64 * 1.2) as usize,
				chunk_count + 1
			);
			
			let mut destination = unsafe { env.heap.alloc_array(GC_ARRAY_CHUNK, new_size) };
			Array::copy(self.chunks, 0, destination, 0, index);
			destination[index] = entry;
			Array::copy(self.chunks, index, destination, index + 1, chunk_count - index);
			
			self.chunks = destination;
		} else {
			Array::copy(self.chunks, index, self.chunks, index + 1, chunk_count - index);
			self.chunks[index] = entry;
		}
		
		self.chunk_count += 1;
	}
	
	fn find_chunk(&self, offset: usize) -> ChunkIndex {
		let chunks = self.chunks;
		
		let mut lo = 0;
		let mut hi = self.chunk_count;
		
		if hi <= 0 {
			return ChunkIndex::new(0, false);
		}
		
		while hi - lo > 3 {
			let pv = (hi + lo) / 2;
			let check_offset = chunks[pv].offset;
			
			if offset == check_offset {
				return ChunkIndex::new(pv, true);
			}
			if offset <= check_offset {
				hi = pv;
			} else {
				lo = pv + 1;
			}
		}
		
		loop {
			let check_offset = chunks[lo].offset;
			
			if check_offset == offset {
				return ChunkIndex::new(lo, true);
			}
			if check_offset > offset {
				break;
			}
			lo += 1;
			
			if lo >= hi {
				break;
			}
		}
		
		ChunkIndex::new(lo, false)
	}
	
	pub fn get_key(&self, offset: usize) -> JsStoreKey {
		let items = self.items;
		if !items.is_null() {
			let entry = items[offset];
			if entry.is_valid() {
				JsStoreKey::Key(Name::from_index(offset), entry.is_enumerable())
			} else {
				JsStoreKey::Missing
			}
		} else {
			let chunk = offset >> CHUNK_SHIFT;
			let items = self.chunks[chunk].items;
			if items.is_null() {
				JsStoreKey::Missing
			} else {
				let entry = items[offset & (CHUNK_SIZE - 1)];
				if entry.is_valid() {
					JsStoreKey::Key(Name::from_index(offset), entry.is_enumerable())
				} else {
					JsStoreKey::Missing
				}
			}
		}
	}
	
	pub fn clear(&mut self, start: usize, end: usize) {
		let mut items = self.items;
		
		if !items.is_null() {
			for i in start..end {
				items[i] = Entry::empty();
			}
		} else {
			let chunks = self.chunks;
			
			let mut offset = usize::MAX;
			let mut items = Array::null();
			
			for i in start..end {
				let this_offset = Self::get_offset_from_index(i);
				
				if offset != this_offset {
					offset = this_offset;
					
					let chunk = self.find_chunk(offset);
					items = if chunk.found() {
						chunks[chunk.index()].items
					} else {
						Array::null()
					};
				}
				
				if !items.is_null() {
					items[i - offset] = Entry::empty()
				}
			}
		}
	}
}

pub unsafe fn validate_walker(walker: &GcWalker) {
	validate_walker_for_sparse_array(walker);
	validate_walker_for_array_chunk(walker);
}

unsafe fn validate_walker_for_sparse_array(walker: &GcWalker) {
	let mut object : Box<SparseArray> = Box::new(zeroed());
	let ptr = transmute::<_, ptr_t>(&*object);
	
	object.items = Array::from_ptr(transmute(1usize));
	validate_walker_field(walker, GC_SPARSE_ARRAY, ptr, true);
	object.items = Array::null();
	
	object.chunks = Array::from_ptr(transmute(1usize));
	validate_walker_field(walker, GC_SPARSE_ARRAY, ptr, true);
	object.chunks = Array::null();
	
	object.chunk_count = 1;
	validate_walker_field(walker, GC_SPARSE_ARRAY, ptr, false);
	object.chunk_count = 0;
}

unsafe fn validate_walker_for_array_chunk(walker: &GcWalker) {
	let mut object : Box<Chunk> = Box::new(zeroed());
	let ptr = transmute::<_, ptr_t>(&*object);
	
	object.offset = 1;
	validate_walker_field(walker, GC_ARRAY_CHUNK, ptr, false);
	object.offset = 0;

	object.items = Array::from_ptr(transmute(1usize));
	validate_walker_field(walker, GC_ARRAY_CHUNK, ptr, true);
	object.items = Array::null();
}
