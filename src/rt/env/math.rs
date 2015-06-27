extern crate rand;

use ::JsResult;
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsType};
use gc::*;
use std::f64;
use self::rand::random;

// 15.8.2.1 abs (x)
pub fn Math_abs(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    Ok(env.new_number(arg.abs()))
}

// 15.8.2.2 acos (x)
pub fn Math_acos(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    Ok(env.new_number(arg.acos()))
}

// 15.8.2.3 asin (x)
pub fn Math_asin(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    Ok(env.new_number(arg.asin()))
}

// 15.8.2.4 atan (x)
pub fn Math_atan(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    Ok(env.new_number(arg.atan()))
}

// 15.8.2.5 atan2 (y, x)
pub fn Math_atan2(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let y = try!(args.arg(env, 0).to_number(env));
    let x = try!(args.arg(env, 1).to_number(env));
    
    Ok(env.new_number(y.atan2(x)))
}

// 15.8.2.6 ceil (x)
pub fn Math_ceil(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    Ok(env.new_number(arg.ceil()))
}

// 15.8.2.7 cos (x)
pub fn Math_cos(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    Ok(env.new_number(arg.cos()))
}

// 15.8.2.8 exp (x)
pub fn Math_exp(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    Ok(env.new_number(arg.exp()))
}

// 15.8.2.9 floor (x)
pub fn Math_floor(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    Ok(env.new_number(arg.floor()))
}

// 15.8.2.10 log (x)
pub fn Math_log(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    Ok(env.new_number(arg.ln()))
}

// 15.8.2.11 max ( [ value1 [ , value2 [ , … ] ] ] )
pub fn Math_max(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let mut result = None;
    
    for i in 0..args.argc {
        let arg = args.arg(env, i);
        if arg.ty() == JsType::Number && arg.unwrap_number().is_nan() {
            return Ok(env.new_number(f64::NAN));
        }
        
        if let Some(last) = result {
            if try!(env.compare_gt(arg, last)) {
                result = Some(arg);
            }
        } else {
            result = Some(arg);
        }
    }
    
    Ok(match result {
        Some(result) => result,
        _ => env.new_number(f64::NEG_INFINITY)
    })
}

// 15.8.2.12 min ( [ value1 [ , value2 [ , … ] ] ] )
pub fn Math_min(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let mut result = None;
    
    for i in 0..args.argc {
        let arg = args.arg(env, i);
        if arg.ty() == JsType::Number && arg.unwrap_number().is_nan() {
            return Ok(env.new_number(f64::NAN));
        }
        
        if let Some(last) = result {
            if try!(env.compare_lt(arg, last)) {
                result = Some(arg);
            }
        } else {
            result = Some(arg);
        }
    }
    
    Ok(match result {
        Some(result) => result,
        _ => env.new_number(f64::INFINITY)
    })
}

// 15.8.2.13 pow (x, y)
pub fn Math_pow(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let x = try!(args.arg(env, 0).to_number(env));
    let y = try!(args.arg(env, 1).to_number(env));
    
    let result = if y.is_infinite() {
        let x = x.abs();
        
        if x > 1.0 {
            if y.is_sign_positive() { f64::INFINITY } else { 0.0 }
        } else if x < 1.0 {
            if y.is_sign_positive() { 0.0 } else { f64::INFINITY }
        } else {
            f64::NAN
        }
    } else {
        x.powf(y)
    };
    
    Ok(env.new_number(result))
}

// 15.8.2.14 random ( )
pub fn Math_random(env: &mut JsEnv, _mode: JsFnMode, _args: JsArgs) -> JsResult<Local<JsValue>> {
    Ok(env.new_number(random::<f64>()))
}

// 15.8.2.15 round (x)
pub fn Math_round(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    let result = if arg.is_finite() {
        let fract = arg.fract();
        let result = if arg > 0.0 {
            if fract >= 0.5 {
                arg.ceil()
            } else {
                arg.floor()
            }
        } else {
            if fract >= -0.5 {
                arg.ceil()
            } else {
                arg.floor()
            }
        };
        
        if result == 0.0 && arg.is_sign_negative() {
            -0.0
        } else {
            result
        }
    } else {
        arg
    };
    
    Ok(env.new_number(result))
}

// 15.8.2.16 sin (x)
pub fn Math_sin(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    Ok(env.new_number(arg.sin()))
}

// 15.8.2.17 sqrt (x)
pub fn Math_sqrt(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    Ok(env.new_number(arg.sqrt()))
}

// 15.8.2.18 tan (x)
pub fn Math_tan(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = try!(args.arg(env, 0).to_number(env));
    
    Ok(env.new_number(arg.tan()))
}
