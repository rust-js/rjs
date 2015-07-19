use ::{JsResult, JsError};
use rt::{JsEnv, JsObject, JsArgs, JsValue, JsType, JsItem, JsStoreType, JsString};
use rt::{JsFnMode, JsDescriptor};
use rt::object::JsStoreKey;
use gc::*;
use syntax::Name;
use syntax::token::name;

// 15.2.1 The Object Constructor Called as a Function
// 15.2.2 The Object Constructor
pub fn Object_constructor(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    if mode.construct() {
        if args.argc > 0 {
            let arg = args.arg(env, 0);
            
            match arg.ty() {
                JsType::Object => Ok(arg),
                JsType::String | JsType::Boolean | JsType::Number => arg.to_object(env),
                _ => Ok(env.create_object().as_value())
            }
        } else {
            Ok(env.create_object().as_value())
        }
    } else {
        let arg = args.arg(env, 0);
        if arg.is_null_or_undefined() {
            Ok(env.create_object().as_value())
        } else {
            arg.to_object(env)
        }
    }
}

// 15.2.3.5 Object.create ( O [, Properties] )
pub fn Object_create(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let object = args.arg(env, 0);
    match object.ty() {
        ty @ JsType::Object | ty @ JsType::Null => {
            let mut result = JsObject::new_local(env, JsStoreType::Hash);
            
            if ty == JsType::Null {
                result.set_prototype(None);
            } else {
                result.set_prototype(Some(args.arg(env, 0)));
            }
            
            let properties = args.arg(env, 1);
            if properties.ty() != JsType::Undefined {
                try!(define_properties(env, result, properties));
            }
            
            Ok(result.as_value())
        }
        _ => Err(JsError::new_type(env, ::errors::TYPE_INVALID))
    }
}

// 15.2.4.2 Object.prototype.toString ( )
pub fn Object_toString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let this_arg = args.this(env);
    
    let result = if this_arg.is_undefined() {
        JsString::from_str(env, "[object Undefined]")
    } else if this_arg.is_null() {
        JsString::from_str(env, "[object Null]")
    } else {
        let object = try!(this_arg.to_object(env));
        let ty = object.class().unwrap_or(name::OBJECT_CLASS);
        let result = format!("[object {}]", env.ir.interner().get(ty));
        
        JsString::from_str(env, &result)
    };
    
    Ok(result.as_value())
}

// 15.2.4.3 Object.prototype.toLocaleString ( )
pub fn Object_toLocaleString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let this = try!(args.this(env).to_object(env));
    let to_string = try!(this.get(env, name::TO_STRING));
    if to_string.is_callable() {
        to_string.call(env, this, Vec::new(), false)
    } else {
        Err(JsError::new_type(env, ::errors::TYPE_CANNOT_CALL_TO_STRING))
    }
}

// 15.2.4.4 Object.prototype.valueOf ( )
pub fn Object_valueOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    args.this(env).to_object(env)
}

// 15.2.4.5 Object.prototype.hasOwnProperty (V)
pub fn Object_hasOwnProperty(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let arg = args.arg(env, 0);
    
    let name = try!(env.intern_value(arg));
    
    let object = try!(args.this(env).to_object(env));
    
    let desc = object.get_own_property(env, name);
    
    Ok(JsValue::new_bool(desc.is_some()))
}

// 15.2.4.6 Object.prototype.isPrototypeOf (V)
pub fn Object_isPrototypeOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let mut arg = args.arg(env, 0);
    
    let result = if arg.ty() != JsType::Object {
        false
    } else {
        let object = try!(args.this(env).to_object(env));
        let mut result = false;
        
        loop {
            if let Some(prototype) = arg.prototype(env) {
                arg = prototype;
                if object == arg {
                    result = true;
                    break;
                }
            } else {
                break;
            }
        }
        
        result
    };
    
    Ok(JsValue::new_bool(result))
}

// 15.2.4.7 Object.prototype.propertyIsEnumerable (V)
pub fn Object_propertyIsEnumerable(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let name = args.arg(env, 0);
    let name = try!(env.intern_value(name));
    let object = try!(args.this(env).to_object(env));
    
    let result = if let Some(desc) = object.get_own_property(env, name) {
        desc.is_enumerable()
    } else {
        false
    };
    
    Ok(JsValue::new_bool(result))
}

// 15.2.3.2 Object.getPrototypeOf ( O )
pub fn Object_getPrototypeOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let arg = args.arg(env, 0);
    
    // [ES6] Argument is coerced to an object.
    let arg = try!(arg.to_object(env)).unwrap_object();
    
    if let Some(prototype) = arg.prototype(env) {
        Ok(prototype)
    } else {
        Ok(JsValue::new_null())
    }
}

// 15.2.3.6 Object.defineProperty ( O, P, Attributes )
pub fn Object_defineProperty(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let mut object = args.arg(env, 0);
    
    if object.ty() != JsType::Object {
        Err(JsError::new_type(env, ::errors::TYPE_INVALID))
    } else {
        let name = args.arg(env, 1);
        let name = try!(name.to_string(env));
        let name = env.ir.interner().intern(&name.to_string());
        
        let desc = args.arg(env, 2);
        let desc = try!(JsDescriptor::to_property_descriptor(env, desc));
        
        try!(object.define_own_property(env, name, desc, true));
        
        Ok(object)
    }
}

// 15.2.3.7 Object.defineProperties ( O, Properties )
pub fn Object_defineProperties(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let object = args.arg(env, 0);
    if object.ty() != JsType::Object {
        Err(JsError::new_type(env, ::errors::TYPE_INVALID))
    } else {
        let object_obj = object.unwrap_object();
        let properties = args.arg(env, 1);
        try!(define_properties(env, object_obj, properties));
        
        Ok(object)
    }
}

// 15.2.3.7 Object.defineProperties ( O, Properties )
fn define_properties(env: &mut JsEnv, mut object: Local<JsObject>, properties: JsValue) -> JsResult<()> {
    let properties = try!(properties.to_object(env)).unwrap_object();
    
    let mut descriptors = Vec::new();
    
    for offset in 0.. {
        match properties.get_key(env, offset) {
            JsStoreKey::Key(name, enumerable) => {
                if enumerable {
                    let value = try!(properties.get(env, name));
                    descriptors.push((name, try!(JsDescriptor::to_property_descriptor(env, value))));
                }
            }
            JsStoreKey::Missing => {}
            JsStoreKey::End => break
        }
    }
    
    for (name, descriptor) in descriptors {
        try!(object.define_own_property(
            env,
            name,
            descriptor,
            true
        ));
    }
    
    Ok(())
}

// 15.2.3.3 Object.getOwnPropertyDescriptor ( O, P )
pub fn Object_getOwnPropertyDescriptor(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let object = args.arg(env, 0);
    
    // [ES6] Argument is coerced to an object.
    let object = try!(object.to_object(env)).unwrap_object();
    
    let arg = args.arg(env, 1);
    let property = try!(arg.to_string(env)).to_string();
    let property = env.intern(&property);
    
    let property = object.get_own_property(env, property);
    
    if let Some(property) = property {
        property.from_property_descriptor(env)
    } else {
        Ok(JsValue::new_undefined())
    }
}

// 15.2.3.10 Object.preventExtensions ( O )
pub fn Object_preventExtensions(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let object = args.arg(env, 0);
    
    // [ES6] Instead of throwing a type error preventExtensions is a no-op for non-objects.
    if object.ty() == JsType::Object {
        object.unwrap_object().set_extensible(false);
    }
    
    Ok(object)
}

// 15.2.3.4 Object.getOwnPropertyNames ( O )
pub fn Object_getOwnPropertyNames(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let object = args.arg(env, 0);
    
    // [ES6] Argument is coerced to an object.
    let object = try!(object.to_object(env)).unwrap_object();

    let mut result = env.create_array();
    let mut offset = 0;
    
    for i in 0.. {
        match object.get_key(env, i) {
            JsStoreKey::Key(name, _) => {
                let name = env.ir.interner().get(name);
                let name = JsString::from_str(env, &*name).as_value();
                try!(result.define_own_property(
                    env,
                    Name::from_index(offset),
                    JsDescriptor::new_simple_value(name),
                    false
                ));
                
                offset += 1;
            }
            JsStoreKey::End => break,
            JsStoreKey::Missing => {}
        }
    }
    
    Ok(result.as_value())
}

// 15.2.3.9 Object.freeze ( O )
pub fn Object_freeze(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let object = args.arg(env, 0);
    
    // [ES6] Freezing non-object arguments is a no-op.
    if object.ty() == JsType::Object {
        let mut target = object.unwrap_object();
        
        for offset in 0.. {
            match target.get_key(env, offset) {
                JsStoreKey::Key(name, _) => {
                    // The GetOwnProperty call cannot fail because we got this
                    // name from the object itself.
                    let mut desc = target.get_own_property(env, name).unwrap();
                    if desc.is_data() {
                        desc.writable = Some(false);
                    }
                    desc.configurable = Some(false);
                    try!(target.define_own_property(env, name, desc, true));
                }
                JsStoreKey::Missing => {},
                JsStoreKey::End => break
            }
        }
        
        target.set_extensible(false);
    }
    
    Ok(object)
}

// 15.2.3.13 Object.isExtensible ( O )
pub fn Object_isExtensible(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let arg = args.arg(env, 0);

    let result = if arg.ty() != JsType::Object {
        // [ES6] Primitive values return false.
        false
    } else {
        arg.unwrap_object().extensible()
    };
    
    Ok(JsValue::new_bool(result))
}

// 15.2.3.8 Object.seal ( O )
pub fn Object_seal(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let object = args.arg(env, 0);
    
    // [ES6] Instead of throwing a type error seal is a no-op for non-objects.
    if object.ty() == JsType::Object {
        let mut object_obj = object.unwrap_object();
        
        for offset in 0.. {
            match object_obj.get_key(env, offset) {
                JsStoreKey::Key(name, _) => {
                    let mut desc = object_obj.get_own_property(env, name).unwrap();
                    if desc.is_configurable() {
                        desc.configurable = Some(false);
                        try!(object_obj.define_own_property(env, name, desc, true));
                    }
                }
                JsStoreKey::Missing => {}
                JsStoreKey::End => break
            }
        }
        
        object_obj.set_extensible(false);
    }
    
    Ok(object)
}

// 15.2.3.11 Object.isSealed ( O )
pub fn Object_isSealed(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let object = args.arg(env, 0);
    
    let result = if object.ty() != JsType::Object {
        // [ES6] Primitive values return true.
        true
    } else {
        let object_obj = object.unwrap_object();
        
        for offset in 0.. {
            match object_obj.get_key(env, offset) {
                JsStoreKey::Key(name, _) => {
                    let desc = object_obj.get_own_property(env, name).unwrap();
                    if desc.is_configurable() {
                        return Ok(JsValue::new_bool(false));
                    }
                }
                JsStoreKey::Missing => {}
                JsStoreKey::End => break
            }
        }
        
        !object_obj.extensible()
    };
    
    Ok(JsValue::new_bool(result))
}

// 15.2.3.12 Object.isFrozen ( O )
pub fn Object_isFrozen(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let object = args.arg(env, 0);
    
    let result = if object.ty() != JsType::Object {
        // [ES6] Primitive values return true.
        true
    } else {
        let object_obj = object.unwrap_object();
        
        for offset in 0.. {
            match object_obj.get_key(env, offset) {
                JsStoreKey::Key(name, _) => {
                    let desc = object_obj.get_own_property(env, name).unwrap();
                    if desc.is_data() {
                        if desc.is_writable() {
                            return Ok(JsValue::new_bool(false));
                        }
                    }
                    if desc.is_configurable() {
                        return Ok(JsValue::new_bool(false));
                    }
                }
                JsStoreKey::Missing => {}
                JsStoreKey::End => break
            }
        }
        
        !object_obj.extensible()
    };
    
    Ok(JsValue::new_bool(result))
}

// 15.2.3.14 Object.keys ( O )
pub fn Object_keys(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let object = args.arg(env, 0);

    // [ES6] Argument is coerced to an object.
    let object = try!(object.to_object(env)).unwrap_object();
    
    let mut array = env.create_array();
    let mut index = 0;
    
    for offset in 0.. {
        match object.get_key(env, offset) {
            JsStoreKey::Key(name, enumerable) => {
                if enumerable {
                    let value = &*env.ir.interner().get(name);
                    let value = JsString::from_str(env, value).as_value();
                    
                    try!(array.define_own_property(
                        env,
                        Name::from_index(index),
                        JsDescriptor::new_simple_value(value),
                        false
                    ));
                    
                    index += 1;
                }
            }
            JsStoreKey::Missing => {}
            JsStoreKey::End => break
        }
    }
    
    Ok(array.as_value())
}
