#![allow(unused_variables)]

use ::JsResult;
use rt::{JsEnv, JsArgs, JsValue, JsType, JsFnMode};
use gc::*;
use syntax::parser::ParseMode;
use std::f64;

pub fn Global_escape(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Global_unescape(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

// 15.1.2.2 parseInt (string , radix)
pub fn Global_parseInt(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let string = try!(args.arg(env, 0).to_string(env));
	let mut radix = try!(args.arg(env, 1).to_int32(env)) as i64;
	
	let mut offset = 0;
	let chars = string.to_string().chars().collect::<Vec<_>>();
	
	while offset < chars.len() {
		if !chars[offset].is_whitespace() {
			break;
		}
		
		offset += 1;
	}
	
	let sign = if offset < chars.len() {
		match chars[offset] {
			'-' => {
				offset += 1;
				-1
			}
			'+' => {
				offset += 1;
				1
			}
			_ => {
				1
			}
		}
	} else {
		1
	};
	
	if radix != 0 {
		if radix < 2 || radix > 36 {
			return Ok(env.new_number(f64::NAN));
		}
	} else {
		radix = 10;
	}
	
	if radix == 16 && offset < chars.len() - 1 {
		if chars[offset] == '0' && (chars[offset + 1] == 'x' || chars[offset + 1] == 'X') {
			offset += 2;
		} 
	}
	
	let start = offset;
	let mut result = 0i64;
	
	while offset < chars.len() {
		let c = chars[offset];
		offset += 1;
		
		let digit = if c >= '0' && c <= '9' {
			c as i64 - '0' as i64
		} else if c >= 'a' && c <= 'z' {
			c as i64 - 'a' as i64 + 10
		} else if c >= 'A' && c <= 'Z' {
			c as i64 - 'A' as i64 + 10
		} else {
			break;
		};
		
		if digit >= radix {
			break;
		}
		
		result *= radix;
		result += digit;
	}
	
	if offset == start {
		Ok(env.new_number(f64::NAN))
	} else {
		Ok(env.new_number((result * sign) as f64))
	}
}

pub fn Global_parseFloat(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

// 15.1.2.4 isNaN (number)
pub fn Global_isNaN(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let result = try!(args.arg(env, 0).to_number(env)).is_nan();
	
	Ok(env.new_bool(result))
}

pub fn Global_isFinite(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

// 15.1.2.1 eval (x)
// TODO: Execution context has not yet been implemented.
pub fn Global_eval(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = args.arg(env, 0);
	
	if arg.ty() != JsType::String {
		Ok(arg)
	} else {
		let arg = arg.unwrap_string(env).to_string();
		let global = env.global.as_value(env);
		let global_scope = env.global_scope.as_local(env);
		
		env.eval_scoped(&arg, mode.strict(), global, global_scope, ParseMode::Eval).map(|result| result.as_local(env))
	}
}
