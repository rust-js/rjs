use ::JsResult;
use super::super::{JsEnv, JsArgs, JsValue, JsType};
use gc::*;

pub fn Global_escape(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Global_unescape(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Global_parseInt(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Global_parseFloat(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

// 15.1.2.4 isNaN (number)
pub fn Global_isNaN(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let result = try!(args.arg(env, 0).to_number(env)).is_nan();
	
	Ok(JsValue::new_bool(result).as_local(env))
}

pub fn Global_isFinite(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

// 15.1.2.1 eval (x)
// TODO: Execution context has not yet been implemented.
pub fn Global_eval(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = args.arg(env, 0);
	
	if arg.ty() != JsType::String {
		Ok(arg)
	} else {
		let arg = arg.as_string(env).to_string();
		let global_scope = Local::from_root(&env.global_scope, &env.heap);
		
		env.eval_scoped(&arg, args.strict, global_scope, false).map(|result| Local::from_root(&result, env.heap()))
	}
}
