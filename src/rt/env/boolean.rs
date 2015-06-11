use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem, JsType, JsString};
use gc::*;
use syntax::token::name;

// 15.6.1 The Boolean Constructor Called as a Function
// 15.6.2 The Boolean Constructor
pub fn Boolean_constructor(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = if args.args.len() > 0 {
		args.args[0].to_boolean()
	} else {
		false
	};
	
	let arg = env.new_bool(arg);
	
	if mode == JsFnMode::Call {
		Ok(arg)
	} else {
		let mut this = args.this.unwrap_object(env);
		
		this.set_class(env, Some(name::BOOLEAN_CLASS));
		this.set_value(arg);
		
		Ok(args.this)
	}
}

fn get_bool_value(env: &mut JsEnv, this: Local<JsValue>) -> JsResult<bool> {
	match this.ty() {
		JsType::Boolean => Ok(this.unwrap_bool()),
		JsType::Object if this.class(env) == Some(name::BOOLEAN_CLASS) => {
			let this = this.unwrap_object(env);
			
			Ok(this.value(env).unwrap_bool())
		}
		_ => Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	}
}

// 15.6.4.3 Boolean.prototype.valueOf ( )
pub fn Boolean_valueOf(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	let value = try!(get_bool_value(env, args.this));
	
	Ok(env.new_bool(value))
}

// 15.6.4.2 Boolean.prototype.toString ( )
pub fn Boolean_toString(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	let value = try!(get_bool_value(env, args.this));
	
	let result = if value { "true" } else { "false" };
	
	Ok(JsString::from_str(env, result).as_value(env))
}
