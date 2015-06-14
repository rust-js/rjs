use ::JsResult;
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsType};
use gc::*;
use std::f64;

// 15.8.2.13 pow (x, y)
pub fn Math_pow(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let x = try!(args.arg(env, 0).to_number(env));
	let y = try!(args.arg(env, 1).to_number(env));
	
	let result = x.powf(y);
	
	Ok(env.new_number(result))
}

// 15.8.2.9 floor (x)
pub fn Math_floor(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = try!(args.arg(env, 0).to_number(env));
	
	let result = arg.floor();
	
	Ok(env.new_number(result))
}

// 15.8.2.12 min ( [ value1 [ , value2 [ , … ] ] ] )
pub fn Math_min(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let mut result = None;
	
	for i in 0..args.argc {
		let arg = args.arg(env, i);
		if arg.ty() == JsType::Number && arg.unwrap_number().is_nan() {
			return Ok(env.new_number(f64::NAN));
		}
		
		if let Some(last) = result {
			if try!(env.compare_lt(arg, last)) {
				result = Some(arg);
			}
		} else {
			result = Some(arg);
		}
	}
	
	Ok(match result {
		Some(result) => result,
		_ => env.new_number(f64::NAN)
	})
}

// 15.8.2.11 max ( [ value1 [ , value2 [ , … ] ] ] )
pub fn Math_max(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let mut result = None;
	
	for i in 0..args.argc {
		let arg = args.arg(env, i);
		if arg.ty() == JsType::Number && arg.unwrap_number().is_nan() {
			return Ok(env.new_number(f64::NAN));
		}
		
		if let Some(last) = result {
			if try!(env.compare_gt(arg, last)) {
				result = Some(arg);
			}
		} else {
			result = Some(arg);
		}
	}
	
	Ok(match result {
		Some(result) => result,
		_ => env.new_number(f64::NAN)
	})
}
