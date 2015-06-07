use rt::{JsItem, JsEnv, JsValue};
use gc::Local;

pub struct JsBoolean {
	value: bool
}

impl JsBoolean {
	pub fn new(value: bool) -> JsBoolean {
		JsBoolean {
			value: value
		}
	}
}

impl JsItem for JsBoolean {
	fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_bool(&env.heap, self.value)
	}
	
	fn has_prototype(&self, _: &JsEnv) -> bool {
		true
	}
	
	fn prototype(&self, env: &JsEnv) -> Option<Local<JsValue>> {
		Some(env.boolean_prototype.as_value(env))
	}
}
