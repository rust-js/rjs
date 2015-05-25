use ::{JsResult, JsError};
use super::super::{JsEnv, JsString, JsFnMode, JsArgs, JsValue, JsItem, JsFunction, JsType};
use syntax::ast::FunctionRef;
use gc::*;
use std::fmt::Write;

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
	
	let function_ref = try!(env.ir.parse_string(&source, args.strict));
	// The function returned is the program, but we need the function. The program
	// function is created last so we need the last but one.
	
	let function_ref = FunctionRef(function_ref.0 - 1);
	
	env.new_function(function_ref, None)
}

// 15.3.4.4 Function.prototype.call (thisArg [ , arg1 [ , arg2, … ] ] )
pub fn Function_call(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let func = args.this;
	if !func.is_callable(env) {
		Err(JsError::new_type(env, ::errors::TYPE_NOT_A_FUNCTION))
	} else {
		let this_arg = args.arg(env, 0);
		let call_args = args.args.into_iter().skip(1).collect::<Vec<_>>();
		
		func.call(env, this_arg, call_args, false)
	}
}

pub fn Function_apply(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

// 15.3.4.2 Function.prototype.toString ( )
// TODO: This can be greatly improved, e.g. by retaining/getting the real code.
pub fn Function_toString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.this.ty() == JsType::Object {
		if let Some(ref function) = args.this.as_object(env).function() {
			let name;
			let args;
			
			match *function {
				JsFunction::Ir(function_ref) => {
					let description = env.ir.get_function(function_ref);
					name = description.name;
					args = None;
				}
				JsFunction::Native(name_, args_, _, _) => {
					name = name_;
					args = Some(args_);
				}
			}
			
			let mut code = String::new();
		
			code.push_str("function ");
			
			if let Some(name) = name {
				code.push_str(&*env.ir.interner().get(name));
				code.push_str(" ");
			}
			
			code.push_str("(");
			
			if let Some(args) = args {
				for i in 0..args {
					if i > 0 {
						code.push_str(", ");
					}
					
					write!(code, "arg{}", i).ok();
				}
			} else {
				code.push_str(" /* ... */ ");
			}
			
			code.push_str(") {\n");
			code.push_str("    /* ... */\n");
			code.push_str("}\n");
			
			return Ok(JsString::from_str(env, &code).as_value(env));
		}
	}
	
	Err(JsError::new_type(env, ::errors::TYPE_INVALID))
}

pub fn Function_toLocaleString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}
