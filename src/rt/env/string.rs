use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem, JsString, JsType, JsDescriptor};
use gc::*;
use syntax::token::name;

// 15.5.1 The String Constructor Called as a Function
// 15.5.2 The String Constructor
// 15.5.5.1 length
pub fn String_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = if args.args.len() > 0 {
		try!(args.args[0].to_string(env)).as_value(env)
	} else {
		JsString::from_str(env, "").as_value(env)
	};
	
	if args.mode == JsFnMode::Call {
		return Ok(arg);
	}
	
	let mut object = args.this.as_object(env);
	
	object.set_prototype(env, Some(env.string_prototype.as_value(env)));
	object.set_class(env, Some(name::STRING_CLASS));
	object.set_value(Some(arg));
	
	let value = JsValue::new_number(arg.get_string().chars.len() as f64).as_local(env);
	try!(object.define_own_property(env, name::LENGTH, JsDescriptor::new_value(value, false, false, false), false));
	
	Ok(args.this)
}

// 15.5.4.2 String.prototype.toString ( )
pub fn String_toString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	match args.this.ty() {
		JsType::String => Ok(args.this),
		JsType::Object => {
			let object = args.this.as_object(env);
			
			if object.class(env) == Some(name::STRING_CLASS) {
				// This is safe because the constructor always sets the value.
				Ok(object.value(env).unwrap())
			} else {
				Err(JsError::new_type(env, ::errors::TYPE_INVALID))
			}
		}
		_ => Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	}
}

pub fn String_substr(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}
