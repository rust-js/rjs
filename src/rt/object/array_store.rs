use super::Store;
use super::super::{JsEnv, JsDescriptor, GC_ENTRY, GC_ARRAY_STORE};
use syntax::Name;
use super::hash_store::HashStore;
use gc::Array;
use super::Entry;
use std::cmp;
use gc::Local;

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
		let len = cmp::max(self.capacity * 2, count);
		
		let mut copy = unsafe { env.heap.alloc_array(GC_ENTRY, len) };
		
		Array::copy(&self.items, &mut copy, self.count);
		
		self.items = copy;
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
			self.props.replace(env, name, value)
		}
	}
	
	fn key_iter(&self, _: &JsEnv) -> Box<Iterator<Item=Name>> {
		unimplemented!();
	}
}
