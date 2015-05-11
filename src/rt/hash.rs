extern crate libc;

use gc::*;
use syntax::Name;
use super::{JsEnv, JsValue};
use std::mem::{transmute, size_of};

const VALID        : u32 = 0b00001;
const WRITABLE     : u32 = 0b00010;
const ENUMERABLE   : u32 = 0b00100;
const CONFIGURABLE : u32 = 0b01000;
const ACCESSOR     : u32 = 0b10000;

#[cfg(target_pointer_width = "64")]
const ENTRY_VALUE1_OFFSET : u32 = 2;
#[cfg(target_pointer_width = "32")]
const ENTRY_VALUE1_OFFSET : u32 = 3;
#[cfg(target_pointer_width = "64")]
const ENTRY_VALUE2_OFFSET : u32 = 4;
#[cfg(target_pointer_width = "32")]
const ENTRY_VALUE2_OFFSET : u32 = 5;

fn entry_walker(ptr: *const libc::c_void, index: u32) -> GcTypeWalk {
	if index < ENTRY_VALUE1_OFFSET {
		GcTypeWalk::Skip
	} else if index == ENTRY_VALUE1_OFFSET {
		let entry = unsafe { transmute::<_, &Entry>(ptr) };
		if entry.value1.ty().is_ptr() { GcTypeWalk::Pointer } else { GcTypeWalk::Skip }
	} else if index < ENTRY_VALUE2_OFFSET {
		GcTypeWalk::Skip
	} else if index == ENTRY_VALUE2_OFFSET {
		let entry = unsafe { transmute::<_, &Entry>(ptr) };
		if entry.value2.ty().is_ptr() { GcTypeWalk::Pointer } else { GcTypeWalk::Skip }
	} else {
		GcTypeWalk::End
	}
}

pub fn build_entry_gc_type(heap: &mut GcHeap) -> GcTypeId {
	heap.types().add(GcType::new(size_of::<Entry>(), GcTypeLayout::Callback(Box::new(entry_walker))))
}

pub enum PropertyValue {
	Value {
		value: Local<JsValue>
	},
	Accessor {
		get: Local<JsValue>,
		set: Local<JsValue>
	}
}

pub struct Property {
	pub value: PropertyValue,
	flags: u32
}

impl Property {
	pub fn new_value(value: Local<JsValue>, writable: bool, enumerable: bool, configurable: bool) -> Property {
		Property {
			value: PropertyValue::Value {
				value: value
			},
			flags:
				if writable { WRITABLE } else { 0 } |
				if enumerable { ENUMERABLE } else { 0 } |
				if configurable { CONFIGURABLE } else { 0 }
		}
	}
	
	pub fn new_simple_value(value: Local<JsValue>) -> Property {
		Self::new_value(value, true, true, true)
	}
	
	pub fn new_accessor(get: Local<JsValue>, set: Local<JsValue>, writable: bool, enumerable: bool, configurable: bool) -> Property {
		Property {
			value: PropertyValue::Accessor {
				get: get,
				set: set
			},
			flags:
				if writable { WRITABLE } else { 0 } |
				if enumerable { ENUMERABLE } else { 0 } |
				if configurable { CONFIGURABLE } else { 0 }
		}
	}
	
	pub fn new_simple_accessor(get: Local<JsValue>, set: Local<JsValue>) -> Property {
		Self::new_accessor(get, set, true, true, true)
	}
	
	fn as_entry(&self, name: Name, next: i32) -> Entry {
		match self.value {
			PropertyValue::Value { value } => {
				Entry {
					name: name,
					flags: self.flags | VALID,
					next: next,
					value1: *value,
					value2: JsValue::new_undefined()
				}
			}
			PropertyValue::Accessor { get, set } => {
				Entry {
					name: name,
					flags: self.flags | ACCESSOR | VALID,
					next: next,
					value1: *get,
					value2: *set
				}
			}
		}
	}
	
	pub fn is_writable(&self) -> bool {
		self.has_flag(WRITABLE)
	}
	
	pub fn set_writable(&mut self, value: bool) {
		self.set_flag(WRITABLE, value);
	}

	pub fn is_enumerable(&self) -> bool {
		self.has_flag(ENUMERABLE)
	}
	
	pub fn set_enumerable(&mut self, value: bool) {
		self.set_flag(ENUMERABLE, value);
	}

	pub fn is_configurable(&self) -> bool {
		self.has_flag(CONFIGURABLE)
	}
	
	pub fn set_configurable(&mut self, value: bool) {
		self.set_flag(CONFIGURABLE, value);
	}
	
	fn has_flag(&self, flag: u32) -> bool {
		(self.flags & flag) != 0
	}
	
	fn set_flag(&mut self, flag: u32, value: bool) {
		if value {
			self.flags |= flag;
		} else {
			self.flags &= !flag;
		}
	}
}

#[derive(Copy, Clone)]
pub struct Entry {
	name: Name,
	flags: u32,
	next: i32,
	value1: JsValue,
	value2: JsValue
}

impl Entry {
	fn is_valid(&self) -> bool {
		(self.flags & VALID) != 0
	}
	
	fn as_property(&self, env: &JsEnv) -> Property {
		Property {
			value: if (self.flags & ACCESSOR) != 0 {
				PropertyValue::Accessor {
					get: self.value1.as_local(env),
					set: self.value2.as_local(env)
				}
			} else {
				PropertyValue::Value {
					value: self.value1.as_local(env)
				}
			},
			flags: self.flags & (WRITABLE | ENUMERABLE | CONFIGURABLE)
		}
	}
}

pub struct Hash {
	entries: *mut [Entry],
	count: u32
}

impl Hash {
	pub fn new(env: &JsEnv, capacity: usize) -> Hash {
		let entries = unsafe {
			&mut *env.alloc_object_entry_array(primes::get_prime(capacity)) as *mut [Entry]
		};
		
		Hash {
			entries: entries,
			count: 0
		}
	}
	
	fn find_entry(&self, name: Name) -> Option<usize> {
		let entries = unsafe { &*self.entries };
		
		let mut offset = self.hash(name) as usize;
		
		// If the first entry isn't valid, we don't have it in the list.
		
		if !entries[offset].is_valid() {
			return None;
		}
		
        // We don't check is_valid in the loop, because the entries are
        // maintained such that the chain is always valid.
		
		loop {
			// If the name is equal, we've found the correct entry.
			
			if entries[offset].name == name {
				return Some(offset);
			}
			
			// See whether this entry is changed to another entry.
			
			let next = entries[offset].next;
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
		unsafe { &*self.entries }.len()
	}
	
	fn hash(&self, name: Name) -> u32 {
		name.value() as u32 % self.capacity() as u32
	}
	
	fn max_load_factor(&self) -> u32 {
		(self.capacity() * 7 / 10) as u32
	}
	
	pub fn add(&mut self, name: Name, value: &Property, env: &JsEnv) {
		let mut entries = unsafe { &mut *self.entries };
		
		assert!(!self.find_entry(name).is_some());
		
		// Grow the entries when we have to.
		
		if self.count > self.max_load_factor() {
			self.grow_entries(env);
			
			entries = unsafe { &mut *self.entries };
		}
		
        // If the entry at the ideal location doesn't have the correct has,
        // we're going to move that entry.
		
		let hash = self.hash(name);
		
		if
			entries[hash as usize].is_valid() &&
			self.hash(entries[hash as usize].name) != hash
		{
			// Create a copy of the current entry and remove it.
			
			let copy = entries[hash as usize];
			
			self.remove(copy.name);
			
			// Put the new entry at the ideal location.
			
			entries[hash as usize] = value.as_entry(name, -1);
			
			// Increment the count.
			
			self.count += 1;
			
			// And now add the previous entry.
			
			self.add(copy.name, &copy.as_property(env), env);
		} else {
			// Find the end of the chain currently at the entry.
			
			let mut entry = self.hash(name) as i32;
			let mut free;
			
			if entries[entry as usize].is_valid() {
				// Find the end of the chain.
				
				let mut next = entries[entry as usize].next;
				while next != -1 {
					entry = next;
					next = entries[entry as usize].next
				}
				
				// Find a free entry.
				
				free = entry as usize + 1;
				let length = entries.len();
				
				loop {
					if free == length {
						free = 0;
					}
					
					if !entries[free].is_valid() {
						break;
					}
					
					free += 1;
				}
			} else {
				free = entry as usize;
				entry = -1;
			}
			
			// Put the new entry into the free location.
			
			entries[free] = value.as_entry(name, -1);
			
			// Fixup the chain if we have one.
			
			if entry >= 0 {
				entries[entry as usize].next = free as i32;
			}
			
			// Increment the count.
			
			self.count += 1;
		}
	}
	
	fn grow_entries(&mut self, env: &JsEnv) {
		let entries;
		
		unsafe {
			entries = &*self.entries;
		
			self.entries = &mut *env.alloc_object_entry_array(primes::get_prime(entries.len() * 2));
		}
		
		self.count = 0;
		
		for entry in entries {
			if entry.is_valid() {
				self.add(entry.name, &entry.as_property(env), env);
			}
		}
	}
	
	pub fn remove(&mut self, name: Name) -> bool {
		let entries = unsafe { &mut *self.entries };
		
		// Find the position of the element.
		
		let mut last = -1;
		let mut index = self.hash(name) as i32;
		
		while index != -1 && entries[index as usize].name != name {
			last = index;
			index = entries[index as usize].next;
		}
		
		if index < 0 {
			false
		} else {
        	// If this is not the tail of the chain, we need to fixup.
        	
        	let index = index as usize;
        	let next = entries[index].next;
        	
        	if last != -1 {
                // If this is not the head of the chain, the previous
                // entry must point to the next entry and this entry
                // becomes invalidated.
                
        		entries[last as usize].next = next;
        		
        		entries[index].flags = 0;
        	} else if next != -1 {
                // Otherwise, we replace the head of the chain with the
                // next entry and invalidate the next entry.
        		
        		entries[index] = entries[next as usize];
        		
        		entries[next as usize].flags = 0;
        	} else {
                // If we're the head and there is no next entry, just
                // invalidate this one.
        		
        		entries[index].flags = 0;
        	}
        	
        	// Decrement the count.
        	
        	self.count -= 1;
        	
        	true
        }
	}
	
	pub fn get_value(&self, name: Name, env: &JsEnv) -> Option<Property> {
		if let Some(index) = self.find_entry(name) {
			let entry = &unsafe { &*self.entries }[index];
			Some(entry.as_property(env))
		} else {
			None
		}
	}
	
	pub fn replace(&self, name: Name, value: &Property) -> bool {
		if let Some(index) = self.find_entry(name) {
			let entry = &mut unsafe { &mut *self.entries }[index];
			*entry = value.as_entry(entry.name, entry.next);
			
			true
		} else {
			false
		}
	}
	
	pub fn key_iter(&self) -> HashIter {
		HashIter {
			entries: self.entries,
			offset: 0
		}
	}
}

pub struct HashIter {
	entries: *const [Entry],
	offset: usize
}

impl Iterator for HashIter {
	type Item = Name;
	
	fn next(&mut self) -> Option<Self::Item> {
		let entries = unsafe { &*self.entries };
		
		while self.offset < entries.len() {
			let entry = &entries[self.offset];
			self.offset += 1;
			
			if entry.is_valid() {
				return Some(entry.name);
			}
		}
		
		None
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

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::{JsType, JsValue, JsRawValue};
	use super::super::super::gc::*;
	use super::super::super::syntax::ast::Name;
	
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
