#![allow(unused_variables)]

use ::{JsResult, JsError};
use rt::{JsEnv, JsString, JsFnMode, JsArgs, JsValue, JsItem, JsFunction, JsType};
use rt::{JsScope, JsDescriptor, JsObject};
use syntax::ast::FunctionRef;
use syntax::parser::ParseMode;
use gc::*;
use std::fmt::Write;
use std::cmp::max;
use syntax::Name;
use syntax::token::name;

pub fn Function_baseConstructor(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	// Nothing to do. The default result already is undefined.
	Ok(env.new_undefined())
}

pub fn Function_constructor(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	if mode == JsFnMode::Call {
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
	
	let function_ref = try!(env.ir.parse_string(&source, strict, ParseMode::Normal));
	// The function returned is the program, but we need the function. The program
	// function is created last so we need the last but one.
	
	let function_ref = FunctionRef(function_ref.0 - 1);
	
	env.new_function(function_ref, None, strict)
}

// 15.3.4.4 Function.prototype.call (thisArg [ , arg1 [ , arg2, … ] ] )
pub fn Function_call(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	let func = args.this;
	if !func.is_callable(env) {
		Err(JsError::new_type(env, ::errors::TYPE_NOT_A_FUNCTION))
	} else {
		let this_arg = args.arg(env, 0);
		let call_args = args.args.into_iter().skip(1).collect::<Vec<_>>();
		
		func.call(env, this_arg, call_args, false)
	}
}

// 15.3.4.3 Function.prototype.apply (thisArg, argArray)
pub fn Function_apply(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	let func = args.this;
	if !func.is_callable(env) {
		Err(JsError::new_type(env, ::errors::TYPE_NOT_A_FUNCTION))
	} else {
		let this_arg = args.arg(env, 0);
		
		let call_args = args.arg(env, 1);
		let call_args = if call_args.is_null_or_undefined() {
			Vec::new()
		} else {
			let len = try!(call_args.get(env, name::LENGTH));
			let len = try!(len.to_uint32(env));
			let mut result = Vec::new();
			
			for i in 0..len {
				result.push(try!(call_args.get(env, Name::from_index(i as usize))));
			}
			
			result
		};
		
		func.call(env, this_arg, call_args, false)
	}
}

// 15.3.4.2 Function.prototype.toString ( )
// TODO: This can be greatly improved, e.g. by retaining/getting the real code.
pub fn Function_toString(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.this.ty() == JsType::Object {
		if let Some(ref function) = args.this.unwrap_object(env).function() {
			fn get_function_details(env: &mut JsEnv, this: Local<JsValue>, function: &JsFunction) -> JsResult<(Option<Name>, Option<u32>)> {
				match *function {
					JsFunction::Ir(function_ref) => {
						let description = env.ir.get_function(function_ref);
						Ok((description.name, None))
					}
					JsFunction::Native(name, args, _, _) => {
						Ok((name, Some(args)))
					}
					JsFunction::Bound => {
						let scope = this.scope(env).unwrap();
						let target = scope.get(env, 0);
						if let Some(ref function) = target.unwrap_object(env).function() {
							get_function_details(env, target, function)
						} else {
							Err(JsError::new_type(env, ::errors::TYPE_INVALID))
						}
					}
				}
			}
			
			let (name, args) = try!(get_function_details(env, args.this, function));
			
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

pub fn Function_toLocaleString(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

// 15.3.4.5 Function.prototype.bind (thisArg [, arg1 [, arg2, …]])
pub fn Function_bind(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	if !args.this.is_callable(env) {
		Err(JsError::new_type(env, ::errors::TYPE_NOT_CALLABLE))
	} else {
		let mut scope = JsScope::new_local_thin(env, max(args.args.len() + 1, 2), None);
		
		scope.set(0, args.this);
		scope.set(1, args.arg(env, 0));
		
		if args.args.len() > 1 {
			for i in 0..args.args.len() - 1 {
				scope.set(i + 2, args.args[i + 1]);
			}
		}
		
		let function_prototype = env.function_prototype.as_local(env);
		let mut result = JsObject::new_function(env, JsFunction::Bound, function_prototype, true);
		
		let length = if args.this.class(env) == Some(name::FUNCTION_CLASS) {
			let length = try!(args.this.get(env, name::LENGTH)).unwrap_number() as usize;
			if length > 0 && args.args.len() > 1 {
				length - (args.args.len() - 1)
			} else {
				length
			}
		} else {
			0
		};
		
		let length = env.new_number(length as f64);
		result.define_own_property(env, name::LENGTH, JsDescriptor::new_value(length, false, false, true), false).ok();
		
		result.set_scope(env, Some(scope));
		
		Ok(result.as_value(env))
	}
}
