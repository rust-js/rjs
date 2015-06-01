const INITIAL_OBJECT : usize = 20;

use rt::{JsEnv, JsDescriptor, GC_HASH_STORE, GC_ENTRY};
use rt::object::{Store, Entry, JsStoreKey};
use syntax::Name;
use gc::{Local, Array};

pub struct HashStore {
	entries: Array<Entry>,
	count: u32
}

impl HashStore {
	pub fn new_local(env: &JsEnv) -> Local<HashStore> {
		let mut store = env.heap.alloc_local::<HashStore>(GC_HASH_STORE);
		*store = Self::new(env);
		store
	}
	
	pub fn new(env: &JsEnv) -> HashStore {
		HashStore {
			entries: unsafe { env.heap.alloc_array::<Entry>(GC_ENTRY, primes::get_prime(INITIAL_OBJECT)) },
			count: 0
		}
	}
	
	fn find_entry(&self, name: Name) -> Option<usize> {
		let mut offset = self.hash(name) as usize;
		
		// If the first entry isn't valid, we don't have it in the list.
		
		if !self.entries[offset].is_valid() {
			return None;
		}
		
        // We don't check is_valid in the loop, because the entries are
        // maintained such that the chain is always valid.
		
		loop {
			// If the name is equal, we've found the correct entry.
			
			if self.entries[offset].name == name {
				return Some(offset);
			}
			
			// See whether this entry is changed to another entry.
			
			let next = self.entries[offset].next;
			if next < 0 {
				return None;
			}
			
			// If the next entry is valid, move the offset to that entry.
			
			offset = next as usize;
		}
	}
	
	pub fn len(&self) -> usize {
		self.count as usize
	}
	
	fn capacity(&self) -> usize {
		self.entries.len()
	}
	
	fn hash(&self, name: Name) -> u32 {
		name.value() as u32 % self.capacity() as u32
	}
	
	fn max_load_factor(&self) -> u32 {
		(self.capacity() * 7 / 10) as u32
	}
	
	fn grow_entries(&mut self, env: &JsEnv) {
		let entries = self.entries.as_local(&env.heap);
		
		unsafe {
			self.entries = env.heap.alloc_array(GC_ENTRY, primes::get_prime(entries.len() * 2));
		}
		
		self.count = 0;
		
		for i in 0..entries.len() {
			let entry = entries[i];
			if entry.is_valid() {
				self.add(env, entry.name, &entry.as_property(env));
			}
		}
	}
}

impl Store for HashStore {
	fn add(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor) {
		assert!(!self.find_entry(name).is_some());
		
		// Grow the entries when we have to.
		
		if self.count > self.max_load_factor() {
			self.grow_entries(env);
		}
		
        // If the entry at the ideal location doesn't have the correct has,
        // we're going to move that entry.
		
		let hash = self.hash(name);
		
		if
			self.entries[hash as usize].is_valid() &&
			self.hash(self.entries[hash as usize].name) != hash
		{
			// Create a copy of the current entry and remove it.
			
			let copy = self.entries[hash as usize];
			
			self.remove(env, copy.name);
			
			// Put the new entry at the ideal location.
			
			self.entries[hash as usize] = Entry::from_descriptor(value, name, -1);
			
			// Increment the count.
			
			self.count += 1;
			
			// And now add the previous entry.
			
			self.add(env, copy.name, &copy.as_property(env));
		} else {
			// Find the end of the chain currently at the entry.
			
			let mut entry = self.hash(name) as i32;
			let mut free;
			
			if self.entries[entry as usize].is_valid() {
				// Find the end of the chain.
				
				let mut next = self.entries[entry as usize].next;
				while next != -1 {
					entry = next;
					next = self.entries[entry as usize].next
				}
				
				// Find a free entry.
				
				free = entry as usize + 1;
				let length = self.entries.len();
				
				loop {
					if free == length {
						free = 0;
					}
					
					if !self.entries[free].is_valid() {
						break;
					}
					
					free += 1;
				}
			} else {
				free = entry as usize;
				entry = -1;
			}
			
			// Put the new entry into the free location.
			
			self.entries[free] = Entry::from_descriptor(value, name, -1);
			
			// Fixup the chain if we have one.
			
			if entry >= 0 {
				self.entries[entry as usize].next = free as i32;
			}
			
			// Increment the count.
			
			self.count += 1;
		}
	}
	
	fn remove(&mut self, _: &JsEnv, name: Name) -> bool {
		// Find the position of the element.
		
		let mut last = -1;
		let mut index = self.hash(name) as i32;
		
		while index != -1 && self.entries[index as usize].name != name {
			last = index;
			index = self.entries[index as usize].next;
		}
		
		if index < 0 {
			false
		} else {
        	// If this is not the tail of the chain, we need to fixup.
        	
        	let index = index as usize;
        	let next = self.entries[index].next;
        	
        	if last != -1 {
                // If this is not the head of the chain, the previous
                // entry must point to the next entry and this entry
                // becomes invalidated.
                
        		self.entries[last as usize].next = next;
        		
        		self.entries[index] = Entry::empty();
        	} else if next != -1 {
                // Otherwise, we replace the head of the chain with the
                // next entry and invalidate the next entry.
        		
        		self.entries[index] = self.entries[next as usize];
        		
        		self.entries[next as usize] = Entry::empty();
        	} else {
                // If we're the head and there is no next entry, just
                // invalidate this one.
        		
        		self.entries[index] = Entry::empty();
        	}
        	
        	// Decrement the count.
        	
        	self.count -= 1;
        	
        	true
        }
	}
	
	fn get_value(&self, env: &JsEnv, name: Name) -> Option<JsDescriptor> {
		if let Some(index) = self.find_entry(name) {
			Some(self.entries[index].as_property(env))
		} else {
			None
		}
	}
	
	fn replace(&mut self, _: &JsEnv, name: Name, value: &JsDescriptor) -> bool {
		if let Some(index) = self.find_entry(name) {
			let entry = self.entries[index];
			self.entries[index] = Entry::from_descriptor(value, entry.name, entry.next);
			
			true
		} else {
			false
		}
	}
	
	fn get_key(&self, _: &JsEnv, offset: usize) -> JsStoreKey {
		if offset >= self.entries.len() {
			JsStoreKey::End
		} else {
			let entry = self.entries[offset];
			if entry.is_valid() {
				JsStoreKey::Key(entry.name, entry.is_enumerable())
			} else {
				JsStoreKey::Missing
			}
		}
	}
}

mod primes {
	use std::u32;
	
	static PRIMES : [usize; 72] = [
        3, 7, 11, 17, 23, 29, 37, 47, 59, 71, 89, 107, 131, 163, 197, 239,
        293, 353, 431, 521, 631, 761, 919, 1103, 1327, 1597, 1931, 2333,
        2801, 3371, 4049, 4861, 5839, 7013, 8419, 10103, 12143, 14591,
        17519, 21023, 25229, 30293, 36353, 43627, 52361, 62851, 75431,
        90523, 108631, 130363, 156437, 187751, 225307, 270371, 324449,
        389357, 467237, 560689, 672827, 807403, 968897, 1162687, 1395263,
        1674319, 2009191, 2411033, 2893249, 3471899, 4166287, 4999559,
        5999471, 7199369
    ];
    
    fn is_prime(candidate: usize) -> bool {
    	if candidate & 1 != 0 {
    		let limit = (candidate as f64).sqrt() as usize;
    		
    		let mut divisor = 3;
    		while divisor <= limit {
    			if candidate % divisor == 0 {
    				return false;
    			}
    			
    			divisor += 2;
    		}
    		
    		return true;
    	}
    	
    	candidate == 2
    }
    
    pub fn get_prime(minimum: usize) -> usize {
    	for prime in PRIMES.iter() {
    		if *prime >= minimum {
    			return *prime;
    		}
    	}
    	
    	let mut prime = minimum | 1;
    	while prime < u32::MAX as usize {
    		if is_prime(prime) {
    			return prime;
    		}
    		
    		prime += 2;
    	}
    	
    	minimum
    }
}

/*
#[cfg(test)]
mod tests {
	use super::*;
	use rt::{JsType, JsValue, JsRawValue};
	use gc::*;
	use syntax::ast::Name;
	
	const A : Name = Name(1);
	const B : Name = Name(2);
	const C : Name = Name(3);
	const A1 : Name = Name(8);
	const A2 : Name = Name(15);

	struct Context {
		type_id: GcTypeId,
		heap: GcHeap
	}
	
	fn create_context() -> Context {
		let mut heap = GcHeap::new(GcOpts::default());
		
		Context {
			type_id: build_entry_gc_type(&mut heap),
			heap: heap
		}
	}
	
	#[test]
	fn single_entry() {
		let ctx = create_context();
		let mut hash = Hash::new(&ctx.heap, ctx.type_id, 7);
		
		hash.add(A, JsValue::new_number(1f64));
		
		assert_eq!(1, hash.count);
		assert!(hash.get_value(A).is_some());
		assert_eq!(JsValue::new_number(1f64), hash.get_value(A).unwrap());
	}
	
	#[test]
	fn clashed_entry() {
		let ctx = create_context();
		let mut hash = Hash::new(&ctx.heap, ctx.type_id, 7);
		
		hash.add(A, JsValue::new_number(1f64));
		hash.add(A1, JsValue::new_number(2f64));
		
		assert_eq!(2, hash.count);
		assert!(hash.get_value(A).is_some());
		assert_eq!(JsValue::new_number(1f64), hash.get_value(A).unwrap());
		assert!(hash.get_value(A1).is_some());
		assert_eq!(JsValue::new_number(2f64), hash.get_value(A1).unwrap());
	}
	
	#[test]
	fn requires_grow() {
		let ctx = create_context();
		let mut hash = Hash::new(&ctx.heap, ctx.type_id, 7);
		
		for i in 0..8 {
			hash.add(Name(i), JsValue::new_number(i as f64));
			dump_hash(&hash);
		}
		
		assert_eq!(8, hash.count);
		assert_eq!(17, unsafe { &*hash.entries }.len());
		
		for i in 0..8 {
			let value = hash.get_value(Name(i));
			assert!(value.is_some());
			assert_eq!(JsValue::new_number(i as f64), value.unwrap());
		}
	}
	
	#[test]
	fn add_and_remove_front() {
		let ctx = create_context();
		let mut hash = Hash::new(&ctx.heap, ctx.type_id, 7);
		
		hash.add(A, JsValue::new_number(1f64));
		hash.add(A1, JsValue::new_number(2f64));
		
		assert_eq!(2, hash.count);
		assert!(hash.get_value(A).is_some());
		assert_eq!(JsValue::new_number(1f64), hash.get_value(A).unwrap());
		assert!(hash.get_value(A1).is_some());
		assert_eq!(JsValue::new_number(2f64), hash.get_value(A1).unwrap());
		
		let removed = hash.remove(A);
		assert!(removed);
		
		let removed = hash.remove(A);
		assert!(!removed);
		
		assert_eq!(1, hash.count);
		assert!(!hash.get_value(A).is_some());
		assert!(hash.get_value(A1).is_some());
		assert_eq!(JsValue::new_number(2f64), hash.get_value(A1).unwrap());
	}
	
	#[test]
	fn add_and_remove_middle() {
		let ctx = create_context();
		let mut hash = Hash::new(&ctx.heap, ctx.type_id, 7);
		
		hash.add(A, JsValue::new_number(1f64));
		hash.add(A1, JsValue::new_number(2f64));
		hash.add(A2, JsValue::new_number(3f64));
		
		assert_eq!(3, hash.count);
		assert!(hash.get_value(A).is_some());
		assert_eq!(JsValue::new_number(1f64), hash.get_value(A).unwrap());
		assert!(hash.get_value(A1).is_some());
		assert_eq!(JsValue::new_number(2f64), hash.get_value(A1).unwrap());
		assert!(hash.get_value(A2).is_some());
		assert_eq!(JsValue::new_number(3f64), hash.get_value(A2).unwrap());
		
		let removed = hash.remove(A1);
		assert!(removed);
		
		let removed = hash.remove(A1);
		assert!(!removed);
		
		assert_eq!(2, hash.count);
		assert!(hash.get_value(A).is_some());
		assert_eq!(JsValue::new_number(1f64), hash.get_value(A).unwrap());
		assert!(!hash.get_value(A1).is_some());
		assert!(hash.get_value(A2).is_some());
		assert_eq!(JsValue::new_number(3f64), hash.get_value(A2).unwrap());
	}
	
	#[test]
	fn keys_iter() {
		let ctx = create_context();
		let mut hash = Hash::new(&ctx.heap, ctx.type_id, 7);
		
		for i in 0..8 {
			hash.add(Name(i), JsValue::new_number(i as f64));
			dump_hash(&hash);
		}
		
		assert_eq!(8, hash.count);
		
		let mut expected = 0;
		
		for name in hash.key_iter() {
			assert_eq!(expected, name.usize());
			expected += 1;
		}
		
		assert_eq!(8, expected);
	}
	
	fn dump_hash(hash: &Hash) {
		println!("\tcount: {}", hash.count);
		let entries = unsafe { &*hash.entries };
		println!("\tcapacity: {}", entries.len());
		for i in 0..entries.len() {
			let entry = &entries[i];
			println!("\t[{}]: {:?}, {:?}, {}, {}", i, entry.name.usize(), entry.tag.ty(), entry.next, entry.value.data);
		}
	}
}
*/
