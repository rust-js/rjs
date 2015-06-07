use rt::{JsItem, JsEnv, JsValue, JsDescriptor};
use gc::Local;
use ::{JsResult, JsError};
use syntax::Name;

pub struct JsUndefined;

impl JsItem for JsUndefined {
	fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		env.new_undefined()
	}
	
	fn get_property(&self, _: &JsEnv, _: Name) -> Option<JsDescriptor> {
		None
	}
	
	fn get(&self, env: &mut JsEnv, _: Name) -> JsResult<Local<JsValue>> {
		Err(JsError::new_type(env, ::errors::TYPE_UNDEFINED))
	}
	
	fn can_put(&self, _: &JsEnv, _: Name) -> bool {
		panic!("not supported");
	}
	
	fn put(&mut self, env: &mut JsEnv, _: Name, _: Local<JsValue>, _: bool) -> JsResult<()> {
		Err(JsError::new_type(env, ::errors::TYPE_UNDEFINED))
	}
	
	fn has_property(&self, _: &JsEnv, _: Name) -> bool {
		false
	}
	
	fn delete(&mut self, env: &mut JsEnv, _: Name, _: bool) -> JsResult<bool> {
		Err(JsError::new_type(env, ::errors::TYPE_UNDEFINED))
	}
	
	fn define_own_property(&mut self, env: &mut JsEnv, _: Name, _: JsDescriptor, _: bool) -> JsResult<bool> {
		Err(JsError::new_type(env, ::errors::TYPE_UNDEFINED))
	}
}
