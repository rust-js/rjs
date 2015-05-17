use ::{JsResult, JsError};
use super::super::{JsEnv, JsObject, JsArgs, JsValue, JsType, JsItem};
use gc::*;

pub fn Object_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Object_create(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	assert!(args.args.len() <= 1);
	
	let mut result = JsObject::new_local(env);
	
	if args.args.len() < 1 {
		return Err(JsError::new_type(env));
	}

	result.set_prototype(env, Some(args.args[0]));
	
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
		Err(JsError::new_type(env))
	} else {
		let arg = if args.args.len() < 2 {
			JsValue::new_undefined().as_local(env)
		} else {
			args.args[1]
		};
		
		let property = env.to_string(arg).to_string();
		let property = env.intern(&property);
		
		let property = args.args[0].get_own_property(env, property);
		
		if let Some(property) = property {
			property.from_property_descriptor(env)
		} else {
			Ok(JsValue::new_undefined().as_local(env))
		}
	}
}
