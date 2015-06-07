use rt::{JsItem, JsEnv, JsValue};
use gc::Local;

pub struct JsNumber {
	value: f64
}

impl JsNumber {
	pub fn new(value: f64) -> JsNumber {
		JsNumber {
			value: value
		}
	}
}

impl JsItem for JsNumber {
	fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_number(&env.heap, self.value)
	}
	
	fn has_prototype(&self, _: &JsEnv) -> bool {
		true
	}
	
	fn prototype(&self, env: &JsEnv) -> Option<Local<JsValue>> {
		Some(env.number_prototype.as_value(env))
	}
}
