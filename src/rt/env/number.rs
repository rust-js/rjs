use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem, JsType, JsString};
use gc::*;
use syntax::token::name;

// 15.7.1 The Number Constructor Called as a Function
// 15.7.2 The Number Constructor
pub fn Number_constructor(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = if args.args.len() > 0 {
		try!(args.args[0].to_number(env))
	} else {
		0f64
	};
	
	let arg = env.new_number(arg);
	
	if mode == JsFnMode::Call {
		Ok(arg)
	} else {
		let mut this = args.this.unwrap_object(env);
		
		this.set_class(env, Some(name::NUMBER_CLASS));
		this.set_value(arg);
		
		Ok(args.this)
	}
}

// 15.7.4.4 Number.prototype.valueOf ( )
pub fn Number_valueOf(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.this.class(env) != Some(name::NUMBER_CLASS) {
		Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	} else {
		let this = args.this.unwrap_object(env);
		Ok(this.value(env))
	}
}

// 15.7.4.2 Number.prototype.toString ( [ radix ] )
// TODO: This is incomplete.
pub fn Number_toString(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
	let this = args.this;
	
	let value = match this.ty() {
		JsType::Object => {
			if this.class(env) != Some(name::NUMBER_CLASS) {
				return Err(JsError::new_type(env, ::errors::TYPE_INVALID))
			}
			this.unwrap_object(env).value(env)
		}
		JsType::Number => this,
		_ => return Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	};
	
	let value = try!(value.to_string(env));
	Ok(value.as_value(env))
}

// 15.7.4.5 Number.prototype.toFixed (fractionDigits)
// TODO: Thi isn't a very nice implementation. What we really need is proper
// formatting methods, which Rust doesn't have.
pub fn Number_toFixed(env: &mut JsEnv, mode: JsFnMode, strict: bool, args: JsArgs) -> JsResult<Local<JsValue>> {
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
		let value = try!(args.this.to_number(env));
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
