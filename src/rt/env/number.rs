use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem, JsType, JsString};
use rt::fmt::*;
use gc::*;
use syntax::token::name;

// 15.7.1 The Number Constructor Called as a Function
// 15.7.2 The Number Constructor
pub fn Number_constructor(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = if args.argc > 0 {
        try!(args.arg(env, 0).to_number(env))
    } else {
        0.0
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
pub fn Number_toString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let value = args.this(env);
    let value = try!(get_number(env, value)).unwrap_number();
    
    let radix = args.arg(env, 0);
    let radix = if radix.is_undefined() {
        10
    } else {
        let radix = try!(radix.to_integer(env)) as i32;
        if radix < 2 || radix > 36 {
            return Err(JsError::new_range(env));
        }
        radix
    };
    
    let result = format_number(value, radix as u32, NumberFormatStyle::Regular, 0);
    
    Ok(JsString::from_str(env, &result).as_value(env))
}

// 15.7.4.3 Number.prototype.toLocaleString()
pub fn Number_toLocaleString(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    Number_toString(env, mode, args)
}

// 15.7.4.5 Number.prototype.toFixed (fractionDigits)
pub fn Number_toFixed(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let digits = args.arg(env, 0);
    let digits = if digits.is_undefined() {
        0.0
    } else {
        try!(args.arg(env, 0).to_integer(env))
    };
    
    let digits = digits as i32;
    
    if digits < 0 || digits > 20 {
        Err(JsError::new_range(env))
    } else {
        let value = try!(args.this(env).to_number(env));
        let result = format_number(value, 10, NumberFormatStyle::Fixed, digits);
        Ok(JsString::from_str(env, &result).as_value(env))
    }
}

// 15.7.4.6 Number.prototype.toExponential (fractionDigits)
pub fn Number_toExponential(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let value = args.this(env);
    let value = try!(get_number(env, value)).unwrap_number();
    
    let fraction_digits = args.arg(env, 0);
    let result = if fraction_digits.is_undefined() {
        format_number(value, 10, NumberFormatStyle::Regular, -1)
    } else {
        let fraction_digits = try!(fraction_digits.to_integer(env)) as i32;
        if fraction_digits < 0 || fraction_digits > 20 {
            return Err(JsError::new_range(env));
        }
        
        format_number(value, 10, NumberFormatStyle::Exponential, fraction_digits)
    };
    
    Ok(JsString::from_str(env, &result).as_value(env))
}

// 15.7.4.7 Number.prototype.toPrecision (precision)
pub fn Number_toPrecision(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let value = args.this(env);
    let value = try!(get_number(env, value)).unwrap_number();
    
    let precision = args.arg(env, 0);
    let result = if precision.is_undefined() {
        format_number(value, 10, NumberFormatStyle::Regular, -1)
    } else {
        let precision = try!(precision.to_integer(env)) as i32;
        if precision < 0 || precision > 21 {
            return Err(JsError::new_range(env));
        }
        
        format_number(value, 10, NumberFormatStyle::Precision, precision)
    };
    
    Ok(JsString::from_str(env, &result).as_value(env))
}
