use rt::{JsItem, JsEnv, JsValue, JsType, JsObject, GC_ITERATOR, GC_U32};
use rt::object::JsStoreKey;
use gc::*;
use syntax::Name;

const INITIAL_SEEN : usize = 8;

pub struct JsIterator {
	target: Ptr<JsObject>,
	name: Option<Name>,
	offset: usize,
	seen: Array<Name>,
	len: usize
}

impl JsIterator {
	pub fn new_local(env: &JsEnv, target: Local<JsValue>) -> Local<JsIterator> {
		let mut result = env.heap.alloc_local::<JsIterator>(GC_ITERATOR);
		
		let target = if target.ty() == JsType::Object {
			target.unwrap_object().as_local(&env.heap).as_ptr()
		} else if let Some(prototype) = target.prototype(env) {
			prototype.unwrap_object().as_local(&env.heap).as_ptr()
		} else {
			Ptr::null()
		};
		
		let seen = env.heap.alloc_array_local::<Name>(GC_U32, INITIAL_SEEN);
		
		*result = JsIterator {
			target: target,
			name: None,
			offset: 0,
			seen: seen.as_ptr(),
			len: 0
		};
		
		result
	}
}

impl Local<JsIterator> {
	pub fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_iterator(self.as_ptr()).as_local(&env.heap)
	}
	
	pub fn next(&mut self, env: &JsEnv) -> bool {
		if self.target.is_null() {
			return false;
		}
		
		let mut target = self.target.as_local(&env.heap);
		
		loop {
			match target.get_key(env, self.offset) {
				JsStoreKey::Key(name, enumerable) => {
					self.offset += 1;
					
					if self.add(env, name) && enumerable { 
						self.name = Some(name);
						
						return true;
					}
				}
				JsStoreKey::Missing => {
					self.offset += 1;
				}
				JsStoreKey::End => {
					if let Some(prototype) = target.prototype(env) {
						target = prototype.unwrap_object().as_local(&env.heap);
						
						self.target = target.as_ptr();
						self.offset = 0;
					} else {
						self.target = Ptr::null();
						self.offset = 0;
						self.name = None;
						
						return false;
					}
				}
			}
		}
	}
	
	fn add(&mut self, env: &JsEnv, name: Name) -> bool {
		let mut seen = self.seen.as_local(&env.heap);
		
		for i in 0..self.len {
			if seen[i] == name {
				return false;
			}
		}
		
		if self.len == seen.len() {
			let mut new_seen = env.heap.alloc_array_local::<Name>(GC_U32, seen.len() * 2);
			
			for i in 0..self.len {
				new_seen[i] = seen[i];
			}
			
			self.seen = new_seen.as_ptr();
			seen = new_seen;
		}
		
		seen[self.len] = name;
		self.len += 1;
		
		true
	}
	
	pub fn current(&self) -> Name {
		if let Some(name) = self.name {
			name
		} else {
			panic!("iterator has been exhausted");
		}
	}
}
