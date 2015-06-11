#![allow(unused_variables)]

use ::JsResult;
use rt::{JsEnv, JsArgs, JsValue, JsFnMode};
use gc::*;

pub fn Date_constructor(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.argc > 0 {
		unimplemented!();
	}
	
	Ok(args.this(env))
}

pub fn Date_getYear(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Date_setYear(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Date_toGMTString(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}
