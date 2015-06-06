use rt::{JsEnv, JsDescriptor, GC_ENTRY, GC_ARRAY_STORE};
use rt::validate_walker_field;
use rt::object::{Store, Entry, JsStoreKey};
use rt::object::hash_store::{HashStore, validate_walker_for_embedded_hash_store};
use syntax::Name;
use gc::{Array, Local, GcWalker, ptr_t};
use std::cmp;
use syntax::token::name;
use std::mem::{transmute, zeroed};

const INITIAL_ARRAY_SIZE : usize = 8;

// Modifications to this struct must be synchronized with the GC walker.
pub struct ArrayStore {
	count: usize,
	capacity: usize,
	items: Array<Entry>,
	props: HashStore
}

impl ArrayStore {
	pub fn new_local(env: &JsEnv) -> Local<ArrayStore> {
		let mut store = env.heap.alloc_local::<ArrayStore>(GC_ARRAY_STORE);
		*store = Self::new(env);
		store
	}
	
	pub fn new(env: &JsEnv) -> ArrayStore {
		ArrayStore {
			items: Array::null(),
			props: HashStore::new(env),
			count: 0,
			capacity: 0
		}
	}
	
	fn grow_entries(&mut self, env: &JsEnv, count: usize) {
		if self.items.is_null() {
			self.capacity = cmp::max(INITIAL_ARRAY_SIZE, count);
			self.items = unsafe { env.heap.alloc_array(GC_ENTRY, self.capacity) };
		} else {
			self.capacity = cmp::max(self.capacity * 2, count);
			
			let mut copy = unsafe { env.heap.alloc_array(GC_ENTRY, self.capacity) };
			
			Array::copy(&self.items, &mut copy, self.count);
			
			self.items = copy;
		}
	}
	
	fn set_length(&mut self, env: &JsEnv, length: usize) {
		if length < self.count {
			for i in length..self.count {
				self.items[i] = Entry::empty();
			}
		} else if length > self.count {
			self.grow_entries(env, length);
		}

		self.count = length;
	}
}

impl Store for ArrayStore {
	fn add(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor) {
		if let Some(index) = name.index() {
			if self.capacity < index + 1 {
				self.grow_entries(env, index + 1);
			}
			
			self.items[index] = Entry::from_descriptor(value, name, -1);
			
			self.count = cmp::max(self.count, index + 1);
		} else {
			if name == name::LENGTH {
				// The special array define_own_property takes care of checking
				// that length is an u32.
				
				let length = value.value.unwrap().unwrap_number() as usize;
				self.set_length(env, length);
			}
			
			self.props.add(env, name, value);
		}
	}
	
	fn remove(&mut self, env: &JsEnv, name: Name) -> bool {
		if let Some(index) = name.index() {
			if index < self.count {
				self.items[index] = Entry::empty();
				true
			} else {
				false
			}
		} else {
			self.props.remove(env, name)
		}
	}
	
	fn get_value(&self, env: &JsEnv, name: Name) -> Option<JsDescriptor> {
		if let Some(index) = name.index() {
			if index < self.count {
				let entry = self.items[index];
				if entry.is_valid() {
					return Some(entry.as_property(env));
				}
			}
			
			None
		} else {
			self.props.get_value(env, name)
		}
	}
	
	fn replace(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor) -> bool {
		if let Some(index) = name.index() {
			if index < self.count {
				if self.items[index].is_valid() {
					self.items[index] = Entry::from_descriptor(value, name, -1);
					return true;
				}
			}
			
			false
		} else {
			if name == name::LENGTH {
				// The special array define_own_property takes care of checking
				// that length is an u32.
				
				let length = value.value.unwrap().unwrap_number() as usize;
				self.set_length(env, length);
			}
			
			self.props.replace(env, name, value)
		}
	}
	
	fn get_key(&self, _: &JsEnv, offset: usize) -> JsStoreKey {
		if offset == self.count {
			JsStoreKey::End
		} else {
			let entry = self.items[offset];
			if entry.is_valid() {
				JsStoreKey::Key(Name::from_index(offset), entry.is_enumerable())
			} else {
				JsStoreKey::Missing
			}
		}
	}
}

pub unsafe fn validate_walker_for_array_store(walker: &GcWalker) {
	let mut object : Box<ArrayStore> = Box::new(zeroed());
	let ptr = transmute::<_, ptr_t>(&*object);
	
	object.count = 1;
	validate_walker_field(walker, GC_ARRAY_STORE, ptr, false);
	object.count = 0;
	
	object.capacity = 1;
	validate_walker_field(walker, GC_ARRAY_STORE, ptr, false);
	object.capacity = 0;
	
	object.items = Array::from_ptr(transmute(1usize));
	validate_walker_field(walker, GC_ARRAY_STORE, ptr, true);
	object.items = Array::null();
	
	validate_walker_for_embedded_hash_store(walker, ptr, GC_ARRAY_STORE, &mut object.props);
}
