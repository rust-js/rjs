use ::{JsResult, JsError};
use super::super::{JsEnv, JsString, JsFnMode, JsArgs, JsValue, JsItem};
use syntax::ast::FunctionRef;
use gc::*;

pub fn Function_baseConstructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	// Nothing to do. The default result already is undefined.
	Ok(JsValue::new_undefined().as_local(env))
}

pub fn Function_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.mode == JsFnMode::Call {
		return args.function.construct(env, args.args);
	}
	
	let body;
	
	let mut source = String::new();
	
	source.push_str("function (");
	
	if args.args.len() == 0 {
		body = JsString::from_str(env, "");
	} else {
		body = try!(args.args[args.args.len() - 1].to_string(env));
		
		for i in 0..args.args.len() - 1 {
			if i > 0 {
				source.push_str(", ");
			}
			let arg = try!(args.args[i].to_string(env));
			source.push_str(&arg.to_string());
		}
	}
	
	source.push_str(") { ");
	source.push_str(&body.to_string());
	source.push_str(" }");
	
	let function_ref = try!(env.ir.parse_string(&source));
	// The function returned is the program, but we need the function. The program
	// function is created last so we need the last but one.
	
	let function_ref = FunctionRef(function_ref.0 - 1);
	
	env.new_function(function_ref)
}

// 15.3.4.4 Function.prototype.call (thisArg [ , arg1 [ , arg2, … ] ] )
pub fn Function_call(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let func = args.this;
	if !func.is_callable(env) {
		Err(JsError::new_type(env))
	} else {
		let this_arg = if args.args.len() > 0 {
			args.args[0]
		} else {
			JsValue::new_undefined().as_local(env)
		};
		
		let call_args = args.args.into_iter().skip(1).collect::<Vec<_>>();
		
		func.call(env, this_arg, call_args)
	}
}

pub fn Function_apply(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Function_toString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Function_toLocaleString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Function_length_get(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

pub fn Function_length_set(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}
