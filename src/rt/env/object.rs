use ::{JsResult, JsError};
use rt::{JsEnv, JsObject, JsArgs, JsValue, JsType, JsItem, JsStoreType, JsString, JsFnMode, JsDescriptor};
use gc::*;
use syntax::token::name;

// 15.2.1 The Object Constructor Called as a Function
// 15.2.2 The Object Constructor
pub fn Object_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.mode == JsFnMode::Call {
		let arg = args.arg(env, 0);
		if arg.is_null() || arg.is_undefined() {
			Ok(env.new_object().as_value(env))
		} else {
			arg.to_object(env)
		}
	} else {
		if args.args.len() > 0 {
			let arg = args.args[0];
			
			match arg.ty() {
				JsType::Object => Ok(arg),
				JsType::String | JsType::Boolean | JsType::Number => arg.to_object(env),
				_ => Ok(env.new_object().as_value(env))
			}
		} else {
			Ok(env.new_object().as_value(env))
		}
	}
}

pub fn Object_create(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	assert!(args.args.len() <= 1);
	
	let mut result = JsObject::new_local(env, JsStoreType::Hash);
	
	if args.args.len() < 1 {
		return Err(JsError::new_type(env, ::errors::TYPE_INVALID));
	}

	result.set_prototype(env, Some(args.args[0]));
	
	Ok(result.as_value(env))
}

// 15.2.4.2 Object.prototype.toString ( )
pub fn Object_toString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let result = if args.this.is_undefined() {
		JsString::from_str(env, "[object Undefined]")
	} else if args.this.is_null() {
		JsString::from_str(env, "[object Null]")
	} else {
		let mut result = String::new();
		
		result.push_str("[object ");
		
		let object = try!(args.this.to_object(env));
		if let Some(class) = object.class(env) {
			result.push_str(&*env.ir.interner().get(class));
		} else {
			// TODO: Class should not be optional
			result.push_str("Unknown");
		}
		
		result.push_str("]");
		
		JsString::from_str(env, &result)
	};
	
	Ok(result.as_value(env))
}

// 15.2.4.3 Object.prototype.toLocaleString ( )
pub fn Object_toLocaleString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this = try!(args.this.to_object(env));
	let to_string = try!(this.get(env, name::TO_STRING));
	if to_string.is_callable(env) {
		to_string.call(env, this, Vec::new(), false)
	} else {
		Err(JsError::new_type(env, ::errors::TYPE_CANNOT_CALL_TO_STRING))
	}
}

// 15.2.4.4 Object.prototype.valueOf ( )
pub fn Object_valueOf(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	args.this.to_object(env)
}

// 15.2.4.5 Object.prototype.hasOwnProperty (V)
pub fn Object_hasOwnProperty(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = args.arg(env, 0);
	
	let name = try!(env.intern_value(arg));
	
	let object = try!(args.this.to_object(env));
	
	let desc = object.get_own_property(env, name);
	
	Ok(JsValue::new_bool(desc.is_some()).as_local(&env.heap))
}

// 15.2.4.6 Object.prototype.isPrototypeOf (V)
pub fn Object_isPrototypeOf(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = args.arg(env, 0);
	
	let result = if arg.ty() != JsType::Object {
		false
	} else {
		if let Some(prototype) = arg.prototype(env) {
			let object = try!(args.this.to_object(env));
			
			prototype.unwrap_object() == object.unwrap_object()
		} else {
			false
		}
	};
	
	Ok(JsValue::new_bool(result).as_local(&env.heap))
}

// 15.2.4.7 Object.prototype.propertyIsEnumerable (V)
pub fn Object_propertyIsEnumerable(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let name = args.arg(env, 0);
	let name = try!(env.intern_value(name));
	let object = try!(args.this.to_object(env));
	
	let result = if let Some(desc) = object.get_own_property(env, name) {
		desc.is_enumerable()
	} else {
		false
	};
	
	Ok(JsValue::new_bool(result).as_local(&env.heap))
}

// 15.2.3.2 Object.getPrototypeOf ( O )
pub fn Object_getPrototypeOf(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.this.ty() != JsType::Object {
		Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	} else if let Some(prototype) = args.this.prototype(env) {
		Ok(prototype)
	} else {
		Ok(JsValue::new_undefined().as_local(&env.heap))
	}
}

// 15.2.3.6 Object.defineProperty ( O, P, Attributes )
pub fn Object_defineProperty(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let mut object = args.arg(env, 0);
	
	if object.ty() != JsType::Object {
		Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	} else {
		let name = args.arg(env, 1);
		let name = try!(name.to_string(env));
		let name = env.ir.interner().intern(&name.to_string());
		
		let desc = args.arg(env, 2);
		let desc = try!(JsDescriptor::to_property_descriptor(env, desc));
		
		try!(object.define_own_property(env, name, desc, true));
		
		Ok(object)
	}
}

// http://ecma-international.org/ecma-262/5.1/#sec-15.2.3.3
pub fn Object_getOwnPropertyDescriptor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let object = args.arg(env, 0);
	
	if object.ty() != JsType::Object {
		Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	} else {
		let arg = args.arg(env, 1);
		let property = try!(arg.to_string(env)).to_string();
		let property = env.intern(&property);
		
		let property = object.get_own_property(env, property);
		
		if let Some(property) = property {
			property.from_property_descriptor(env)
		} else {
			Ok(JsValue::new_undefined().as_local(&env.heap))
		}
	}
}
