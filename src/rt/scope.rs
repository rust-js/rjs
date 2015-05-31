use super::{JsEnv, JsValue, JsObject, JsItem, GC_SCOPE, GC_VALUE};
use gc::{Ptr, Array, Local};

pub struct JsScope {
	items: Array<JsValue>
}

impl JsScope {
	pub fn new_local_thin(env: &JsEnv, size: usize, parent: Option<Local<JsScope>>) -> Local<JsScope> {
		let mut result = env.heap.alloc_local::<JsScope>(GC_SCOPE);
		
		unsafe {
			result.items = env.heap.alloc_array(GC_VALUE, size + 1);
		}
		
		if let Some(parent) = parent {
			result.raw_set(0, parent.as_value(env));
		}
		
		result
	}
	
	pub fn new_local_thick(env: &JsEnv, scope_object: Local<JsObject>, parent: Option<Local<JsScope>>, arguments: bool) -> Local<JsScope> {
		let mut result = env.heap.alloc_local::<JsScope>(GC_SCOPE);
		
		let size = 2 + if arguments { 1 } else { 0 };
		
		unsafe {
			result.items = env.heap.alloc_array(GC_VALUE, size);
		}
		
		if let Some(parent) = parent {
			result.raw_set(0, parent.as_value(env));
		}
		result.raw_set(1, scope_object.as_value(env));
		
		result
	}
}

impl Ptr<JsScope> {
	pub fn as_local(&self, env: &JsEnv) -> Local<JsScope> {
		Local::from_ptr(*self, &env.heap)
	}
}

impl Local<JsScope> {
	pub fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_scope(self.as_ptr()).as_local(env)
	}
	
	pub fn parent(&self, env: &JsEnv) -> Option<Local<JsScope>> {
		let parent = self.raw_get(env, 0);
		
		if parent.is_undefined() { None } else { Some(parent.as_scope(env)) }
	}
	
	pub fn scope_object(&self, env: &JsEnv) -> Local<JsObject> {
		self.raw_get(env, 1).as_object(env)
	}
	
	pub fn arguments(&self, env: &JsEnv) -> Option<Local<JsValue>> {
		if self.items.len() == 2 {
			None
		} else {
			Some(self.raw_get(env, 2))
		}
	}
	
	pub fn set_arguments(&mut self, arguments: Local<JsValue>) {
		if self.items.len() == 2 {
			panic!("scope does not have a slot to store arguments");
		}
		
		self.raw_set(2, arguments);
	}
	
	pub fn get(&self, env: &JsEnv, index: usize) -> Local<JsValue> {
		self.raw_get(env, index + 1)
	}
	
	pub fn set(&mut self, index: usize, value: Local<JsValue>) {
		self.raw_set(index + 1, value)
	}
	
	fn raw_get(&self, env: &JsEnv, index: usize) -> Local<JsValue> {
		self.items[index].as_local(env)
	}
	
	fn raw_set(&mut self, index: usize, value: Local<JsValue>) {
		self.items[index] = *value;
	}
}
