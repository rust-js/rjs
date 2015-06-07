use ::JsResult;
use rt::{JsEnv, JsArgs, JsValue, JsType};
use gc::*;
use syntax::parser::ParseMode;

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
	
	Ok(JsValue::new_bool(&env.heap, result))
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
		let arg = arg.unwrap_string(&env.heap).to_string();
		let global = env.global.as_value(env);
		let global_scope = env.global_scope.as_local(&env.heap);
		
		env.eval_scoped(&arg, args.strict, global, global_scope, ParseMode::Eval).map(|result| result.as_local(&env.heap))
	}
}
