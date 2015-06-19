use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem, JsType, JsString};
use gc::*;
use syntax::token::name;

pub fn Error_constructor(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	if !mode.construct() {
		let target_args = args.args(env);
		return args.function(env).construct(env, target_args);
	}
	
	let this = try!(args.this(env).to_object(env));
	let mut this_obj = this.unwrap_object(env);
	this_obj.set_class(env, Some(name::ERROR_CLASS));
	
	let message = args.arg(env, 0);
	if !message.is_undefined() {
		let message = try!(message.to_string(env)).as_value(env);
		try!(this_obj.put(env, name::MESSAGE, message, true));
	}
	
	Ok(this)
}

pub fn Error_toString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this = args.this(env);
	if this.ty() != JsType::Object {
		Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	} else {
		let name = try!(this.get(env, name::NAME));
		let name = if name.is_undefined() {
			JsString::from_str(env, "Error")
		} else {
			try!(name.to_string(env))
		};
		
		let message = try!(this.get(env, name::MESSAGE));
		let message = if message.is_undefined() {
			JsString::from_str(env, "")
		} else {
			try!(message.to_string(env))
		};
		
		let result = if name.chars().len() == 0 {
			message
		} else if message.chars().len() == 0 {
			name
		} else {
			let result = format!("{}: {}", name.to_string(), message.to_string());
			JsString::from_str(env, &result)
		};
		
		Ok(result.as_value(env))
	}
}
