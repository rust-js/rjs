#![allow(unused_variables)]

use ::JsResult;
use rt::{JsEnv, JsArgs, JsValue};
use gc::*;

pub fn Date_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.args.len() > 0 {
		unimplemented!();
	}
	
	Ok(args.this)
}

pub fn Date_getYear(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Date_setYear(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Date_toGMTString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}
