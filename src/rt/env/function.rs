use ::{JsResult, JsError};
use rt::{JsEnv, JsString, JsFnMode, JsArgs, JsValue, JsItem, JsFunction, JsType};
use rt::{JsScope, JsDescriptor, JsObject, JsHandle};
use syntax::ast::FunctionRef;
use syntax::parser::ParseMode;
use gc::*;
use std::fmt::Write;
use std::cmp::max;
use syntax::Name;
use syntax::token::name;

pub fn Function_baseConstructor(_env: &mut JsEnv, _mode: JsFnMode, _args: JsArgs) -> JsResult<JsValue> {
    Ok(JsValue::new_undefined())
}

pub fn Function_constructor(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    if !mode.construct() {
        let target_args = args.args(env);
        return args.function(env).construct(env, target_args);
    }
    
    let body;
    
    let mut source = String::new();
    
    source.push_str("function (");
    
    if args.argc == 0 {
        body = JsString::from_str(env, "");
    } else {
        for i in 0..args.argc - 1 {
            if i > 0 {
                source.push_str(", ");
            }
            let arg = try!(args.arg(env, i).to_string(env));
            source.push_str(&arg.to_string());
        }
        
        body = try!(args.arg(env, args.argc - 1).to_string(env));
    }
    
    source.push_str(") { ");
    source.push_str(&body.to_string());
    source.push_str(" }");
    
    let function_ref = try!(env.ir.parse_string(&source, mode.strict(), ParseMode::Normal, false));
    // The function returned is the program, but we need the function. The program
    // function is created last so we need the last but one.
    
    let function_ref = FunctionRef(function_ref.0 - 1);
    
    env.new_function(function_ref, None, mode.strict())
}

// 15.3.4.4 Function.prototype.call (thisArg [ , arg1 [ , arg2, … ] ] )
pub fn Function_call(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let func = args.this(env);
    if !func.is_callable() {
        Err(JsError::new_type(env, ::errors::TYPE_NOT_A_FUNCTION))
    } else {
        let this_arg = args.arg(env, 0);
        
        let mut call_args = Vec::new();
        for i in 1..args.argc {
            call_args.push(args.arg(env, i));
        }
        
        func.call(env, this_arg, call_args, false)
    }
}

// 15.3.4.3 Function.prototype.apply (thisArg, argArray)
pub fn Function_apply(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let func = args.this(env);
    if !func.is_callable() {
        Err(JsError::new_type(env, ::errors::TYPE_NOT_A_FUNCTION))
    } else {
        let this_arg = args.arg(env, 0);
        
        let call_args = args.arg(env, 1);
        let call_args = if call_args.is_null_or_undefined() {
            Vec::new()
        } else {
            match call_args.class() {
                Some(name::ARRAY_CLASS) | Some(name::ARGUMENTS_CLASS) => {},
                _ => {
                    // "Array" is a valid parameter to the apply method.
                    let prototype = try!(call_args.get(env, name::PROTOTYPE));
                    let array_prototype = env.handle(JsHandle::Array);
                    if prototype.ty() != JsType::Object || prototype.unwrap_object().as_ptr() != array_prototype.as_ptr() {
                        return Err(JsError::new_type(env, ::errors::TYPE_INVALID_ARGUMENTS_ARRAY))
                    }
                }
            }
            
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
// TODO #66: This can be greatly improved, e.g. by retaining/getting the real code.
pub fn Function_toString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let this_arg = args.this(env);
    
    if this_arg.ty() == JsType::Object {
        if let Some(function) = this_arg.unwrap_object().function() {
            fn get_function_details(env: &mut JsEnv, this: JsValue, function: &JsFunction) -> JsResult<(Option<Name>, Option<u32>)> {
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
                        if let Some(function) = target.unwrap_object().function() {
                            get_function_details(env, target, &function)
                        } else {
                            Err(JsError::new_type(env, ::errors::TYPE_INVALID))
                        }
                    }
                }
            }
            
            let (name, args) = try!(get_function_details(env, this_arg, &function));
            
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
            
            return Ok(JsString::from_str(env, &code).as_value());
        }
    }
    
    Err(JsError::new_type(env, ::errors::TYPE_INVALID))
}

// 15.3.4.5 Function.prototype.bind (thisArg [, arg1 [, arg2, …]])
pub fn Function_bind(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let this_arg = args.this(env);
    
    if !this_arg.is_callable() {
        Err(JsError::new_type(env, ::errors::TYPE_NOT_CALLABLE))
    } else {
        let mut scope = JsScope::new_local_thin(env, max(args.argc + 1, 2), None);
        
        scope.set(0, this_arg);
        scope.set(1, args.arg(env, 0));
        
        if args.argc > 1 {
            for i in 0..args.argc - 1 {
                scope.set(i + 2, args.arg(env, i + 1));
            }
        }
        
        let mut result = JsObject::new_function(env, JsFunction::Bound, true);
        
        let length = if this_arg.class() == Some(name::FUNCTION_CLASS) {
            let length = try!(this_arg.get(env, name::LENGTH)).unwrap_number() as usize;
            let argc = if args.argc > 1 { args.argc - 1 } else { 0 };
            if argc > length { 0 } else { length - argc }
        } else {
            0
        };
        
        let length = JsValue::new_number(length as f64);
        result.define_own_property(env, name::LENGTH, JsDescriptor::new_value(length, false, false, true), false).ok();
        
        result.set_scope(Some(scope));
        
        Ok(result.as_value())
    }
}
