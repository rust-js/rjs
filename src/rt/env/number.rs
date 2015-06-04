use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem, JsType};
use gc::*;
use syntax::token::name;

// 15.7.1 The Number Constructor Called as a Function
// 15.7.2 The Number Constructor
pub fn Number_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = if args.args.len() > 0 {
		try!(args.args[0].to_number(env))
	} else {
		0f64
	};
	
	let arg = JsValue::new_number(arg).as_local(&env.heap);
	
	if args.mode == JsFnMode::Call {
		Ok(arg)
	} else {
		let mut this = args.this.unwrap_object().as_local(&env.heap);
		
		this.set_class(env, Some(name::NUMBER_CLASS));
		this.set_value(Some(arg));
		
		Ok(args.this)
	}
}

// 15.7.4.4 Number.prototype.valueOf ( )
pub fn Number_valueOf(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.this.class(env) != Some(name::NUMBER_CLASS) {
		Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	} else {
		let this = args.this.unwrap_object().as_local(&env.heap);
		Ok(this.value(env).unwrap())
	}
}

// 15.7.4.2 Number.prototype.toString ( [ radix ] )
// TODO: This is incomplete.
pub fn Number_toString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this = args.this;
	
	let value = match this.ty() {
		JsType::Object => {
			if this.class(env) != Some(name::NUMBER_CLASS) {
				return Err(JsError::new_type(env, ::errors::TYPE_INVALID))
			}
			this.unwrap_object().as_local(&env.heap).value(env).unwrap()
		}
		JsType::Number => this,
		_ => return Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	};
	
	let value = try!(value.to_string(env));
	Ok(value.as_value(env))
}
