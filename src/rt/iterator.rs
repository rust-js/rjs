use super::{JsItem, JsEnv, JsValue, JsType, JsObject, GC_ITERATOR};
use super::object::JsStoreKey;
use gc::{Local, Ptr};
use syntax::Name;

pub struct JsIterator {
	target: Ptr<JsObject>,
	name: Option<Name>,
	offset: usize
}

impl JsIterator {
	pub fn new_local(env: &JsEnv, target: Local<JsValue>) -> Local<JsIterator> {
		let mut result = env.heap.alloc_local::<JsIterator>(GC_ITERATOR);
		
		let target = if target.ty() == JsType::Object {
			target.as_object(env).as_ptr()
		} else if let Some(prototype) = target.prototype(env) {
			prototype.as_object(env).as_ptr()
		} else {
			Ptr::null()
		};
		
		*result = JsIterator {
			target: target,
			name: None,
			offset: 0
		};
		
		result
	}
}

impl Local<JsIterator> {
	pub fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_iterator(self.as_ptr()).as_local(env)
	}
	
	pub fn next(&mut self, env: &JsEnv) -> bool {
		if self.target.is_null() {
			return false;
		}
		
		let mut target = Local::from_ptr(self.target, &env.heap);
		
		loop {
			match target.get_key(env, self.offset) {
				JsStoreKey::Key(name) => {
					self.offset += 1;
					self.name = Some(name);
					
					return true;
				}
				JsStoreKey::Missing => {
					self.offset += 1;
				}
				JsStoreKey::End => {
					if let Some(prototype) = target.prototype(env) {
						target = prototype.as_object(env);
						
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
	
	pub fn current(&self) -> Name {
		if let Some(name) = self.name {
			name
		} else {
			panic!("iterator has been exhausted");
		}
	}
}
