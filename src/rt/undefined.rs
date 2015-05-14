use super::{JsItem, JsEnv, JsValue};
use gc::Local;

pub struct JsUndefined;

impl JsUndefined {
	pub fn new() -> JsUndefined {
		JsUndefined
	}
}

impl JsItem for JsUndefined {
	fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_undefined().as_local(env)
	}
}
