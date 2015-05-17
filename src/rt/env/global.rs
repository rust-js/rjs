use ::JsResult;
use super::super::{JsEnv, JsArgs, JsValue, JsType};
use gc::*;

pub fn Global_escape(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Global_unescape(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

// 15.1.2.1 eval (x)
// TODO: Execution context has not yet been implemented.
pub fn Global_eval(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let x = if args.args.len() > 0 {
		args.args[0]
	} else {
		JsValue::new_undefined().as_local(env)
	};
	
	if x.ty() != JsType::String {
		Ok(x)
	} else {
		let x = x.as_string(env).to_string();
		
		env.eval(&x).map(|result| Local::from_root(result, env.heap()))
	}
}
