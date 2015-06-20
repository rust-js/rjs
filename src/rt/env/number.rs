use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem, JsType, JsString};
use gc::*;
use syntax::token::name;

// 15.7.1 The Number Constructor Called as a Function
// 15.7.2 The Number Constructor
pub fn Number_constructor(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = if args.argc > 0 {
		try!(args.arg(env, 0).to_number(env))
	} else {
		0f64
	};
	
	let arg = env.new_number(arg);
	
	if mode.construct() {
		let this_arg = args.this(env);
		let mut this = this_arg.unwrap_object(env);
		
		this.set_class(env, Some(name::NUMBER_CLASS));
		this.set_value(arg);
		
		Ok(this_arg)
	} else {
		Ok(arg)
	}
}

// 15.7.4.4 Number.prototype.valueOf ( )
pub fn Number_valueOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this_arg = args.this(env);
	
	if this_arg.class(env) != Some(name::NUMBER_CLASS) {
		Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	} else {
		let this = this_arg.unwrap_object(env);
		Ok(this.value(env))
	}
}

fn get_number(env: &mut JsEnv, value: Local<JsValue>) -> JsResult<Local<JsValue>> {
	match value.ty() {
		JsType::Object => {
			if value.class(env) != Some(name::NUMBER_CLASS) {
				return Err(JsError::new_type(env, ::errors::TYPE_INVALID))
			}
			Ok(value.unwrap_object(env).value(env))
		}
		JsType::Number => Ok(value),
		_ => return Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	}
}

// 15.7.4.2 Number.prototype.toString ( [ radix ] )
// TODO: This is incomplete.
pub fn Number_toString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let value = args.this(env);
	let value = try!(get_number(env, value));
	let value = try!(value.to_string(env));
	
	Ok(value.as_value(env))
}

// 15.7.4.3 Number.prototype.toLocaleString()
pub fn Number_toLocaleString(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	Number_toString(env, mode, args)
}

// 15.7.4.5 Number.prototype.toFixed (fractionDigits)
// TODO: Thi isn't a very nice implementation. What we really need is proper
// formatting methods, which Rust doesn't have.
pub fn Number_toFixed(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let digits = args.arg(env, 0);
	let digits = if digits.is_undefined() {
		0f64
	} else {
		try!(args.arg(env, 0).to_integer(env))
	};
	
	let digits = digits as i32;
	
	if digits < 0 || digits > 20 {
		Err(JsError::new_range(env))
	} else {
		let value = try!(args.this(env).to_number(env));
		let result = if value.is_nan() {
			"NaN".to_string()
		} else {
			let pow = 10f64.powi(digits);
			let value = (value * pow).round();
			let value = value / pow;
			
			let mut result = value.to_string();
			
			if digits > 0 {
				let mut len = result.len();
				let offset = match result.find('.') {
					Some(offset) => offset + 1,
					None => {
						result.push('.');
						len += 1;
						len
					}
				};
				
				let current_digits = len - offset;
				for _ in current_digits..digits as usize {
					result.push('0');
				}
			}
			
			result
		};
		
		Ok(JsString::from_str(env, &result).as_value(env))
	}
}

// 15.7.4.6 Number.prototype.toExponential (fractionDigits)
pub fn Number_toExponential(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	let value = args.this(env);
	let value = try!(get_number(env, value)).unwrap_number();
	
	let result = if value.is_nan() {
		JsString::from_str(env, "NaN")
	} else if value == 0f64 {
		JsString::from_str(env, "0")
	} else if value.is_infinite() {
		JsString::from_str(env, if value.is_sign_negative() { "-Infinity" } else { "Infinity" })
	} else {
		// TODO: This is very wrong. See 9.8.1 ToString Applied to the Number Type
		// for the full specifications. A C# implementation can be found at
		// http://jurassic.codeplex.com/SourceControl/latest#Jurassic/Core/NumberFormatter.cs.
		
		let string = format!("{:e}", value);
		
		JsString::from_str(env, &string)
	};
	
	Ok(result.as_value(env))
}

// 15.7.4.7 Number.prototype.toPrecision (precision)
pub fn Number_toPrecision(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
	// TODO: This is very wrong. See 9.8.1 ToString Applied to the Number Type
	// for the full specifications. A C# implementation can be found at
	// http://jurassic.codeplex.com/SourceControl/latest#Jurassic/Core/NumberFormatter.cs.
	
	Number_toString(env, mode, args)
}
