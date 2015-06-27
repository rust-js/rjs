use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem, JsType, JsString};
use gc::*;
use syntax::token::name;

// 15.6.1 The Boolean Constructor Called as a Function
// 15.6.2 The Boolean Constructor
pub fn Boolean_constructor(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = if args.argc > 0 {
        args.arg(env, 0).to_boolean()
    } else {
        false
    };
    
    let arg = env.new_bool(arg);
    
    if mode.construct() {
        let this_arg = args.this(env);
        let mut this = this_arg.unwrap_object(env);
        
        this.set_class(env, Some(name::BOOLEAN_CLASS));
        this.set_value(arg);
        
        Ok(this_arg)
    } else {
        Ok(arg)
    }
}

fn get_bool_value(env: &mut JsEnv, this: Local<JsValue>) -> JsResult<bool> {
    match this.ty() {
        JsType::Boolean => Ok(this.unwrap_bool()),
        JsType::Object if this.class(env) == Some(name::BOOLEAN_CLASS) => {
            let this = this.unwrap_object(env);
            
            Ok(this.value(env).unwrap_bool())
        }
        _ => Err(JsError::new_type(env, ::errors::TYPE_INVALID))
    }
}

// 15.6.4.3 Boolean.prototype.valueOf ( )
pub fn Boolean_valueOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let this_arg = args.this(env);
    let value = try!(get_bool_value(env, this_arg));
    
    Ok(env.new_bool(value))
}

// 15.6.4.2 Boolean.prototype.toString ( )
pub fn Boolean_toString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let this_arg = args.this(env);
    let value = try!(get_bool_value(env, this_arg));
    
    let result = if value { "true" } else { "false" };
    
    Ok(JsString::from_str(env, result).as_value(env))
}
