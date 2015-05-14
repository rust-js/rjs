use super::{JsItem, JsEnv, JsValue};
use gc::Local;

pub struct JsNull;

impl JsNull {
	pub fn new() -> JsNull {
		JsNull
	}
}

impl JsItem for JsNull {
	fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_null().as_local(env)
	}
}
