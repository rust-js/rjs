#![allow(unused_variables)]

use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem, JsString, JsType, JsDescriptor};
use gc::*;
use syntax::token::name;
use std::char;
use std::f64;
use std::cmp::{min, max};

// 15.5.1 The String Constructor Called as a Function
// 15.5.2 The String Constructor
// 15.5.5.1 length
pub fn String_constructor(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = if args.argc > 0 {
		try!(args.arg(env, 0).to_string(env)).as_value(env)
	} else {
		JsString::from_str(env, "").as_value(env)
	};
	
	if !mode.construct() {
		return Ok(arg);
	}
	
	let this_arg = args.this(env);
	let mut object = this_arg.unwrap_object(env);
	
	object.set_prototype(env, Some(env.string_prototype.as_value(env)));
	object.set_class(env, Some(name::STRING_CLASS));
	object.set_value(arg);
	
	let value = env.new_number(arg.unwrap_string(env).chars().len() as f64);
	try!(object.define_own_property(env, name::LENGTH, JsDescriptor::new_value(value, false, false, false), false));
	
	Ok(this_arg)
}

// 15.5.4.2 String.prototype.toString ( )
pub fn String_toString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this_arg = args.this(env);
	
	match this_arg.ty() {
		JsType::String => Ok(this_arg),
		JsType::Object => {
			let object = this_arg.unwrap_object(env);
			
			if object.class(env) == Some(name::STRING_CLASS) {
				// This is safe because the constructor always sets the value.
				Ok(object.value(env))
			} else {
				Err(JsError::new_type(env, ::errors::TYPE_INVALID))
			}
		}
		_ => Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	}
}

// 15.5.4.3 String.prototype.valueOf ( )
pub fn String_valueOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this_arg = args.this(env);
	
	match this_arg.ty() {
		JsType::String => Ok(this_arg),
		JsType::Object => {
			let object = this_arg.unwrap_object(env);
			
			if object.class(env) == Some(name::STRING_CLASS) {
				// This is safe because the constructor always sets the value.
				Ok(object.value(env))
			} else {
				Err(JsError::new_type(env, ::errors::TYPE_INVALID))
			}
		}
		_ => Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	}
}

pub fn String_substr(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

// 15.5.3.2 String.fromCharCode ( [ char0 [ , char1 [ , … ] ] ] )
pub fn String_fromCharCode(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let mut chars = Vec::new();
	
	for i in 0..args.argc {
		chars.push(try!(args.arg(env, i).to_uint16(env)));
	}
	
	Ok(env.new_string(JsString::from_u16(env, &chars)))
}

// 15.5.4.4 String.prototype.charAt (pos)
pub fn String_charAt(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this = try!(args.this(env).to_string(env));
	let chars = this.chars();
	let position = try!(args.arg(env, 0).to_integer(env)) as i32;
	
	let result = if position < 0 || position >= chars.len() as i32 {
		"".to_string()
	} else {
		char::from_u32(chars[position as usize] as u32).unwrap().to_string()
	};
	
	Ok(JsString::from_str(env, &result).as_value(env))
}

// 15.5.4.5 String.prototype.charCodeAt (pos)
pub fn String_charCodeAt(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this = try!(args.this(env).to_string(env));
	let chars = this.chars();
	let position = try!(args.arg(env, 0).to_integer(env)) as i32;
	
	let result = if position < 0 || position >= chars.len() as i32 {
		f64::NAN
	} else {
		chars[position as usize] as f64
	};
	
	Ok(env.new_number(result))
}

// 15.5.4.7 String.prototype.indexOf (searchString, position)
// 15.5.4.8 String.prototype.lastIndexOf (searchString, position)
fn index_of(env: &mut JsEnv, args: JsArgs, reverse: bool) -> JsResult<Local<JsValue>> {
	let string = try!(args.this(env).to_string(env));
	let search = try!(args.arg(env, 0).to_string(env));
	
	let len = string.chars().len();
	
	let position = {
		let position = args.arg(env, 1);
		if !reverse {
			if position.is_undefined() {
				0
			} else {
				try!(position.to_integer(env)) as i32
			}
		} else {
			let position = try!(position.to_number(env));
			if position.is_nan() {
				len as i32
			} else {
				position as i32
			}
		}
	};
	
	let start = min(max(position, 0), len as i32) as usize;
	let string = string.chars();
	let search = search.chars();
	
	fn matches(string: &[u16], search: &[u16], index: usize) -> bool {
		for i in 0..search.len() {
			if string[index + i] != search[i] {
				return false;
			}
		}
		
		true
	}
	
	let end = string.len() - search.len() + 1;
	
	let index = if !reverse {
		let mut result = -1;
		
		for i in start..end {
			if matches(&string, &search, i) {
				result = i as i32;
				break;
			}
		}
		
		result
	} else {
		let mut result = -1;
		let start = min(start, end);
		
		for i in (0..start).rev() {
			if matches(&string, &search, i) {
				result = i as i32;
				break;
			}
		}
		
		result
	};
	
	Ok(env.new_number(index as f64))
}

// 15.5.4.7 String.prototype.indexOf (searchString, position)
pub fn String_indexOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	index_of(env, args, false)
}

// 15.5.4.8 String.prototype.lastIndexOf (searchString, position)
pub fn String_lastIndexOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	index_of(env, args, true)
}

// 15.5.4.15 String.prototype.substring (start, end)
pub fn String_substring(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this = args.this(env);
	try!(this.check_object_coercible(env));
	
	let string = try!(this.to_string(env));
	let len = string.chars().len();
	
	let start = try!(args.arg(env, 0).to_integer(env)) as usize;
	let end = args.arg(env, 1);
	let end = if end.is_undefined() {
		len
	} else {
		try!(end.to_integer(env)) as usize
	};
	
	let start = min(max(start, 0), len);
	let end = min(max(end, 0), len);
	
	let from = min(start, end);
	let to = max(start, end);
	
	let result = JsString::from_u16(env, &string.chars()[from..to]);
	
	Ok(result.as_value(env))
}

// 15.5.4.16 String.prototype.toLowerCase ( )
pub fn String_toLowerCase(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this = args.this(env);
	try!(this.check_object_coercible(env));
	
	let string = try!(this.to_string(env)).to_string().to_lowercase();
	let string = JsString::from_str(env, &string);
	
	Ok(string.as_value(env))
}

// 15.5.4.17 String.prototype.toLocaleLowerCase ( )
pub fn String_toLocaleLowerCase(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	String_toLowerCase(env, mode, args)
}

// 15.5.4.18 String.prototype.toUpperCase ( )
pub fn String_toUpperCase(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this = args.this(env);
	try!(this.check_object_coercible(env));
	
	let string = try!(this.to_string(env)).to_string().to_uppercase();
	let string = JsString::from_str(env, &string);
	
	Ok(string.as_value(env))
}

// 15.5.4.19 String.prototype.toLocaleUpperCase ( )
pub fn String_toLocaleUpperCase(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	String_toUpperCase(env, mode, args)
}
