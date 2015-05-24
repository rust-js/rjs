use ::JsResult;
use super::super::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem};
use gc::*;
use syntax::token::name;

// 15.6.1 The Boolean Constructor Called as a Function
// 15.6.2 The Boolean Constructor
pub fn Boolean_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = if args.args.len() > 0 {
		args.args[0].to_boolean()
	} else {
		false
	};
	
	let arg = JsValue::new_bool(arg).as_local(env);
	
	if args.mode == JsFnMode::Call {
		Ok(arg)
	} else {
		let mut this = args.this.as_object(env);
		
		this.set_class(env, Some(name::BOOLEAN_CLASS));
		this.set_value(Some(arg));
		
		Ok(args.this)
	}
}
