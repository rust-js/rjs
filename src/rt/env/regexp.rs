use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsRegExp, JsString, JsItem, JsDescriptor};
use gc::*;
use syntax::token::name;
use syntax::Name;

// 15.10.3 The RegExp Constructor Called as a Function
// 15.10.4 The RegExp Constructor
pub fn RegExp_constructor(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let pattern = args.arg(env, 0);
    let flags = args.arg(env, 1);
    
    if !mode.construct() {
        if pattern.class() == Some(name::REGEXP_CLASS) && flags.is_undefined() {
            Ok(pattern)
        } else {
            args.function(env).construct(env, vec![pattern, flags])
        }
    } else {
        let (pattern, flags) = if pattern.class() == Some(name::REGEXP_CLASS) {
            if flags.is_undefined() {
                let regexp = pattern.unwrap_object().value(env).unwrap_regexp();
                
                (regexp.pattern(env), regexp.flags(env))
            } else {
                return Err(JsError::new_type(env, ::errors::TYPE_INVALID_REGEXP_ARGS));
            }
        } else {
            let pattern = if pattern.is_undefined() {
                JsString::from_str(env, "")
            } else {
                try!(pattern.to_string(env))
            };
            let flags = if flags.is_undefined() {
                JsString::from_str(env, "")
            } else {
                try!(flags.to_string(env))
            };
            
            (pattern, flags)
        };
        
        let mut global = false;
        let mut ignore_case = false;
        let mut multiline = false;
        
        for c in flags.to_string().chars() {
            match c {
                'g' => {
                    if global {
                        return Err(JsError::new_syntax(env, ::errors::SYNTAX_INVALID_REGEXP_FLAGS));
                    }
                    global = true;
                }
                'i' => {
                    if ignore_case {
                        return Err(JsError::new_syntax(env, ::errors::SYNTAX_INVALID_REGEXP_FLAGS));
                    }
                    ignore_case = true;
                }
                'm' => {
                    if multiline {
                        return Err(JsError::new_syntax(env, ::errors::SYNTAX_INVALID_REGEXP_FLAGS));
                    }
                    multiline = true;
                }
                _ => return Err(JsError::new_syntax(env, ::errors::SYNTAX_INVALID_REGEXP_FLAGS))
            }
        }
        
        let regexp = try!(JsRegExp::new_local(
            env,
            pattern,
            flags,
            global,
            ignore_case,
            multiline
        ));
        
        let this_arg = args.this(env);
        let mut this = this_arg.unwrap_object();
        
        this.set_class(Some(name::REGEXP_CLASS));
        this.set_value(regexp.as_value());
        
        let value = JsDescriptor::new_value(pattern.as_value(), false, false, false);
        try!(this.define_own_property(env, name::SOURCE, value, true));
        let value = JsDescriptor::new_value(JsValue::new_bool(global), false, false, false);
        try!(this.define_own_property(env, name::GLOBAL, value, true));
        let value = JsDescriptor::new_value(JsValue::new_bool(ignore_case), false, false, false);
        try!(this.define_own_property(env, name::IGNORE_CASE, value, true));
        let value = JsDescriptor::new_value(JsValue::new_bool(multiline), false, false, false);
        try!(this.define_own_property(env, name::MULTILINE, value, true));
        let value = JsDescriptor::new_value(JsValue::new_number(0.0), true, false, false);
        try!(this.define_own_property(env, name::LAST_INDEX, value, true));
        
        Ok(this_arg)
    }
}

fn unwrap_regexp(env: &mut JsEnv, args: &JsArgs) -> JsResult<Local<JsRegExp>> {
    let this = args.this(env);
    
    if this.class() == Some(name::REGEXP_CLASS) {
        Ok(this.unwrap_object().value(env).unwrap_regexp())
    } else {
        Err(JsError::new_type(env, ::errors::TYPE_INVALID))
    }
}

// 15.10.6.2 RegExp.prototype.exec(string)
pub fn RegExp_exec(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let regexp = try!(unwrap_regexp(env, &args));
    let mut this = args.this(env);
    
    let input = try!(args.arg(env, 0).to_string(env));
    let string = input.to_string();
    let length = string.len() as isize;
    
    let last_index = try!(this.get(env, name::LAST_INDEX));
    let mut last_index = try!(last_index.to_number(env));
    
    let global = regexp.global();
    if !global {
        last_index = 0.0;
    }
    
    // The indexes we take from lastIndex and set as lastIndex and the index
    // of the captures is wrong. We set byte offsets instead UTF-16 code points.
    // The reason we do this is because it would be very costly to do this
    // right. This will be fixed through #79.
    
    if last_index < 0.0 || last_index > length as f64 {
        let value = JsValue::new_number(0.0);
        try!(this.put(env, name::LAST_INDEX, value, true));
        return Ok(JsValue::new_null());
    }
    
    let last_index = last_index as usize;
    
    if let Some(capture) = regexp.regex().captures(&string[last_index..]) {
        // The first capture is the complete match, so the end position marks
        // the end of the complete match.
        let (start_index, end_index) = capture.pos(0).unwrap();
        
        // Offset to adjust for the current index into the string.
        let start_index = start_index + last_index;
        let end_index = end_index + last_index;
        
        if global {
            let value = JsValue::new_number(end_index as f64);
            try!(this.put(env, name::LAST_INDEX, value, true));
        }
        
        let mut result = env.create_array();
        
        let value = JsDescriptor::new_simple_value(JsValue::new_number(start_index as f64));
        try!(result.define_own_property(env, name::INDEX, value, true));
        
        let value = JsDescriptor::new_simple_value(input.as_value());
        try!(result.define_own_property(env, name::INPUT, value, true));
        
        // Length is the same as capture.len() because that includes the
        // complete match as the first entry.
        let value = JsDescriptor {
            value: Some(JsValue::new_number(capture.len() as f64)),
            ..JsDescriptor::default()
        };
        try!(result.define_own_property(env, name::LENGTH, value, true));
        
        let matched_substr = JsString::from_str(env, &string[start_index..end_index]).as_value();
        let value = JsDescriptor::new_simple_value(matched_substr);
        try!(result.define_own_property(env, Name::from_index(0), value, true));
        
        for i in 1..capture.len() {
            if let Some((capture_start, capture_end)) = capture.pos(i) {
                let capture_i = JsString::from_str(env, &string[(capture_start + last_index)..(capture_end + last_index)]).as_value();
                let value = JsDescriptor::new_simple_value(capture_i);
                try!(result.define_own_property(env, Name::from_index(i), value, true));
            }
        }
        
        Ok(result.as_value())
    } else {
        let value = JsValue::new_number(0.0);
        try!(this.put(env, name::LAST_INDEX, value, true));
        Ok(JsValue::new_null())
    }
}

// 15.10.6.3 RegExp.prototype.test(string)
pub fn RegExp_test(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let result = try!(RegExp_exec(env, mode, args));
    
    Ok(JsValue::new_bool(!result.is_null()))
}

// 15.10.6.4 RegExp.prototype.toString()
pub fn RegExp_toString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let regexp = try!(unwrap_regexp(env, &args));
    
    let mut result = String::new();
    
    result.push('/');
    result.push_str(&*regexp.pattern(env).to_string());
    result.push('/');
    
    if regexp.global() {
        result.push('g');
    }
    if regexp.ignore_case() {
        result.push('i');
    }
    if regexp.multiline() {
        result.push('m');
    }
    
    Ok(JsString::from_str(env, &result).as_value())
}
