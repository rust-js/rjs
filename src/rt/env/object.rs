use ::{JsResult, JsError};
use super::super::{JsEnv, JsObject, JsArgs, JsValue, JsType};
use gc::*;

pub fn Object_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Object_create(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	assert!(args.args.len() <= 1);
	
	let mut result = JsObject::new_local(env);

	result.prototype = if args.args.len() < 1 {
		return Err(JsError::Type);
	} else {
		let prototype = args.args[0];
		match prototype.ty() {
			JsType::Null => Ptr::null(),
			JsType::Object => prototype.get_object(),
			_ => return Err(JsError::Type)
		}
	};
	
	Ok(result.as_value(env))
}

pub fn Object_toString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Object_toLocaleString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Object_valueOf(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Object_hasOwnProperty(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Object_isPrototypeOf(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Object_propertyIsEnumerable(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Object_getPrototypeOf(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Object_defineProperty(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

// http://ecma-international.org/ecma-262/5.1/#sec-15.2.3.3
pub fn Object_getOwnPropertyDescriptor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.args.len() < 1 || args.args[0].ty() != JsType::Object {
		Err(JsError::Type)
	} else {
		let arg = if args.args.len() < 2 {
			JsValue::new_undefined().as_local(env)
		} else {
			args.args[1]
		};
		
		let property = env.to_string(arg).to_string();
		let property = env.intern(&property);
		
		let property = args.args[0].get_object().get_own_property(property, env);
		
		env.from_property_descriptor(property)
	}
}
