#![allow(unused_variables)]

use ::JsResult;
use rt::{JsEnv, JsArgs, JsValue, JsFnMode};
use gc::*;

pub fn RegExp_constructor(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.args.len() > 0 {
		unimplemented!();
	}
	
	Ok(args.this)
}
