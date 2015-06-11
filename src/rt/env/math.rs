use ::JsResult;
use rt::{JsEnv, JsArgs, JsValue, JsFnMode};
use gc::*;

// 15.8.2.13 pow (x, y)
pub fn Math_pow(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	let x = try!(args.arg(env, 0).to_number(env));
	let y = try!(args.arg(env, 1).to_number(env));
	
	let result = x.powf(y);
	
	Ok(env.new_number(result))
}

// 15.8.2.9 floor (x)
pub fn Math_floor(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = try!(args.arg(env, 0).to_number(env));
	
	let result = arg.floor();
	
	Ok(env.new_number(result))
}
