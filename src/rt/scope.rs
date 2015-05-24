use super::{JsEnv, JsValue, GC_SCOPE, GC_VALUE};
use gc::{Ptr, Array, Local};

pub struct JsScope {
	parent: Ptr<JsScope>,
	items: Array<JsValue>
}

impl JsScope {
	pub fn new_local(env: &JsEnv, size: usize, parent: Option<Local<JsValue>>) -> Local<JsScope> {
		let mut result = env.heap.alloc_local::<JsScope>(GC_SCOPE);
		
		if let Some(parent) = parent {
			result.parent = parent.get_scope();
		}
		
		unsafe {
			if size > 0 {
				result.items = env.heap.alloc_array(GC_VALUE, size);
			}
		}
		
		result
	}
}

impl Local<JsScope> {
	pub fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_scope(self.as_ptr()).as_local(env)
	}
	
	pub fn parent(&self, env: &JsEnv) -> Option<Local<JsValue>> {
		if self.parent.is_null() {
			None
		} else {
			Some(Local::from_ptr(self.parent, &env.heap).as_value(env))
		}
	}
	
	pub fn get(&self, env: &JsEnv, index: usize) -> Local<JsValue> {
		self.items[index].as_local(env)
	}
	
	pub fn set(&mut self, index: usize, value: Local<JsValue>) {
		self.items[index] = *value;
	}
}
