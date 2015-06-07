use rt::{JsItem, JsEnv, JsValue, JsDescriptor};
use gc::Local;
use ::{JsResult, JsError};
use syntax::Name;

pub struct JsNull;

impl JsItem for JsNull {
	fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_null(&env.heap)
	}
	
	fn get_property(&self, _: &JsEnv, _: Name) -> Option<JsDescriptor> {
		None
	}
	
	fn get(&self, env: &mut JsEnv, _: Name) -> JsResult<Local<JsValue>> {
		Err(JsError::new_type(env, ::errors::TYPE_NULL))
	}
	
	fn can_put(&self, _: &JsEnv, _: Name) -> bool {
		panic!("not supported");
	}
	
	fn put(&mut self, env: &mut JsEnv, _: Name, _: Local<JsValue>, _: bool) -> JsResult<()> {
		Err(JsError::new_type(env, ::errors::TYPE_NULL))
	}
	
	fn has_property(&self, _: &JsEnv, _: Name) -> bool {
		false
	}
	
	fn delete(&mut self, env: &mut JsEnv, _: Name, _: bool) -> JsResult<bool> {
		Err(JsError::new_type(env, ::errors::TYPE_NULL))
	}
	
	fn define_own_property(&mut self, env: &mut JsEnv, _: Name, _: JsDescriptor, _: bool) -> JsResult<bool> {
		Err(JsError::new_type(env, ::errors::TYPE_NULL))
	}
}
