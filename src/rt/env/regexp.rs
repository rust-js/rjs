#![allow(unused_variables)]

use ::JsResult;
use rt::{JsEnv, JsArgs, JsValue};
use gc::*;

pub fn RegExp_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.args.len() > 0 {
		unimplemented!();
	}
	
	Ok(args.this)
}
