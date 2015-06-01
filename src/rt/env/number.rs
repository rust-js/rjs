use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem};
use gc::*;
use syntax::token::name;

// 15.7.1 The Number Constructor Called as a Function
// 15.7.2 The Number Constructor
pub fn Number_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	let arg = if args.args.len() > 0 {
		try!(args.args[0].to_number(env))
	} else {
		0f64
	};
	
	let arg = JsValue::new_number(arg).as_local(&env.heap);
	
	if args.mode == JsFnMode::Call {
		Ok(arg)
	} else {
		let mut this = args.this.unwrap_object().as_local(&env.heap);
		
		this.set_class(env, Some(name::NUMBER_CLASS));
		this.set_value(Some(arg));
		
		Ok(args.this)
	}
}

// 15.7.4.4 Number.prototype.valueOf ( )
pub fn Number_valueOf(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.this.class(env) != Some(name::NUMBER_CLASS) {
		Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	} else {
		let this = args.this.unwrap_object().as_local(&env.heap);
		Ok(this.value(env).unwrap())
	}
}

// 15.7.4.2 Number.prototype.toString ( [ radix ] )
// ================================================
// 
// The optional radix should be an integer value in the inclusive range 2 to 36. If radix not present
// or is undefined the Number 10 is used as the value of radix. If ToInteger(radix) is the Number 10
// then this Number value is given as an argument to the ToString abstract operation; the resulting
// String value is returned.
// 
// If ToInteger(radix) is not an integer between 2 and 36 inclusive throw a RangeError exception. If
// ToInteger(radix) is an integer from 2 to 36, but not 10, the result is a String representation of
// this Number value using the specified radix. Letters a-z are used for digits with values 10 through
// 35. The precise algorithm is implementation-dependent if the radix is not 10, however the algorithm
// should be a generalisation of that specified in 9.8.1.
// 
// The toString function is not generic; it throws a TypeError exception if its this value is not a
// Number or a Number object. Therefore, it cannot be transferred to other kinds of objects for use as
// a method.
pub fn Number_toString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.this.class(env) != Some(name::NUMBER_CLASS) {
		Err(JsError::new_type(env, ::errors::TYPE_INVALID))
	} else {
		let this = args.this.unwrap_object().as_local(&env.heap);
		let value = this.value(env).unwrap();
		let value = try!(value.to_string(env));
		Ok(value.as_value(env))
	}
}
