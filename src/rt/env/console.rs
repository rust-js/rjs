use ::JsResult;
use rt::{JsEnv, JsArgs, JsValue, JsFnMode};

// TODO #65
pub fn console_log(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let string = try!(args.arg(env, 0).to_string(env)).to_string();
    
    println!("{}", string);
    
    Ok(JsValue::new_undefined())
}
