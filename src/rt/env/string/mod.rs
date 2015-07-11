use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsFnMode, JsItem, JsString, JsType, JsDescriptor, JsHandle};
use gc::*;
use syntax::Name;
use syntax::token::name;
use std::char;
use std::{f64, u32};
use std::cmp::{min, max};
use ::syntax::lexer::{is_line_terminator, is_whitespace};

mod replacer;

fn get_this_string(env: &mut JsEnv, args: &JsArgs) -> JsResult<Local<JsString>> {
    let this = args.this(env);
    
    try!(this.check_object_coercible(env));
    
    this.to_string(env)
}

// 15.5.1 The String Constructor Called as a Function
// 15.5.2 The String Constructor
// 15.5.5.1 length
pub fn String_constructor(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let arg = if args.argc > 0 {
        try!(args.arg(env, 0).to_string(env)).as_value()
    } else {
        JsString::from_str(env, "").as_value()
    };
    
    if !mode.construct() {
        return Ok(arg);
    }
    
    let this_arg = args.this(env);
    let mut object = this_arg.unwrap_object();
    
    object.set_prototype(Some(env.handle(JsHandle::String).as_value()));
    object.set_class(Some(name::STRING_CLASS));
    object.set_value(arg);
    
    let value = JsValue::new_number(arg.unwrap_string().chars().len() as f64);
    try!(object.define_own_property(env, name::LENGTH, JsDescriptor::new_value(value, false, false, false), false));
    
    Ok(this_arg)
}

// 15.5.4.2 String.prototype.toString ( )
pub fn String_toString(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let this_arg = args.this(env);
    
    match this_arg.ty() {
        JsType::String => Ok(this_arg),
        JsType::Object => {
            let object = this_arg.unwrap_object();
            
            if object.class() == Some(name::STRING_CLASS) {
                // This is safe because the constructor always sets the value.
                Ok(object.value(env))
            } else {
                Err(JsError::new_type(env, ::errors::TYPE_INVALID))
            }
        }
        _ => Err(JsError::new_type(env, ::errors::TYPE_INVALID))
    }
}

// 15.5.4.3 String.prototype.valueOf ( )
pub fn String_valueOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let this_arg = args.this(env);
    
    match this_arg.ty() {
        JsType::String => Ok(this_arg),
        JsType::Object => {
            let object = this_arg.unwrap_object();
            
            if object.class() == Some(name::STRING_CLASS) {
                // This is safe because the constructor always sets the value.
                Ok(object.value(env))
            } else {
                Err(JsError::new_type(env, ::errors::TYPE_INVALID))
            }
        }
        _ => Err(JsError::new_type(env, ::errors::TYPE_INVALID))
    }
}

// 15.5.3.2 String.fromCharCode ( [ char0 [ , char1 [ , … ] ] ] )
pub fn String_fromCharCode(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let mut chars = Vec::new();
    
    for i in 0..args.argc {
        chars.push(try!(args.arg(env, i).to_uint16(env)));
    }
    
    Ok(JsString::from_u16(env, &chars).as_value())
}

// 15.5.4.4 String.prototype.charAt (pos)
pub fn String_charAt(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let this = try!(get_this_string(env, &args));
    let chars = this.chars();
    let position = try!(args.arg(env, 0).to_integer(env)) as i32;
    
    let result = if position < 0 || position >= chars.len() as i32 {
        "".to_string()
    } else {
        char::from_u32(chars[position as usize] as u32).unwrap().to_string()
    };
    
    Ok(JsString::from_str(env, &result).as_value())
}

// 15.5.4.5 String.prototype.charCodeAt (pos)
pub fn String_charCodeAt(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let this = try!(get_this_string(env, &args));
    let chars = this.chars();
    let position = try!(args.arg(env, 0).to_integer(env)) as i32;
    
    let result = if position < 0 || position >= chars.len() as i32 {
        f64::NAN
    } else {
        chars[position as usize] as f64
    };
    
    Ok(JsValue::new_number(result))
}

// 15.5.4.7 String.prototype.indexOf (searchString, position)
// 15.5.4.8 String.prototype.lastIndexOf (searchString, position)
fn index_of(env: &mut JsEnv, args: JsArgs, reverse: bool) -> JsResult<JsValue> {
    let string = try!(get_this_string(env, &args));
    let search = try!(args.arg(env, 0).to_string(env));
    
    let len = string.chars().len();
    let search_len = search.chars().len();
    if len == 0 || search_len > len {
        return Ok(JsValue::new_number(-1.0));
    }
    
    let position = args.arg(env, 1);
    
    let (start, end) = if !reverse {
        let position = if position.is_undefined() {
            0
        } else {
            try!(position.to_integer(env)) as i32
        };
        
        if position >= len as i32 {
            return Ok(JsValue::new_number(-1.0));
        }
        
        (max(position, 0) as usize, len)
    } else {
        let position = if try!(position.to_number(env)).is_nan() {
            len as i32
        } else {
            try!(position.to_integer(env)) as i32
        };
        
        if position < 0 {
            return Ok(JsValue::new_number(-1.0));
        }
        
        (0, position as usize)
    };
    
    let end = min(end, len - search_len);
    
    let string = string.chars();
    let search = search.chars();
    
    fn matches(string: &[u16], search: &[u16], index: usize) -> bool {
        for i in 0..search.len() {
            if string[index + i] != search[i] {
                return false;
            }
        }
        
        true
    }
    
    let mut result = -1;
    
    if !reverse {
        for i in start..(end + 1) {
            if matches(&string, &search, i) {
                result = i as i32;
                break;
            }
        }
    } else {
        for i in (start..(end + 1)).rev() {
            if matches(&string, &search, i) {
                result = i as i32;
                break;
            }
        }
    }
    
    Ok(JsValue::new_number(result as f64))
}

// 15.5.4.7 String.prototype.indexOf (searchString, position)
pub fn String_indexOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    index_of(env, args, false)
}

// 15.5.4.8 String.prototype.lastIndexOf (searchString, position)
pub fn String_lastIndexOf(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    index_of(env, args, true)
}

// 15.5.4.15 String.prototype.substring (start, end)
pub fn String_substring(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let string = try!(get_this_string(env, &args));
    let len = string.chars().len() as f64;
    
    let start = try!(args.arg(env, 0).to_integer(env)) as f64;
    let end = args.arg(env, 1);
    let end = if end.is_undefined() {
        len as f64
    } else {
        try!(end.to_integer(env)) as f64
    };
    
    let start = start.max(0.0).min(len);
    let end = end.max(0.0).min(len);
    
    let from = start.min(end);
    let to = start.max(end);
    
    let result = JsString::from_u16(env, &string.chars()[(from as usize)..(to as usize)]);
    
    Ok(result.as_value())
}

// 15.5.4.16 String.prototype.toLowerCase ( )
pub fn String_toLowerCase(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let string = try!(get_this_string(env, &args)).to_string().to_lowercase();
    let string = JsString::from_str(env, &string);
    
    Ok(string.as_value())
}

// 15.5.4.17 String.prototype.toLocaleLowerCase ( )
pub fn String_toLocaleLowerCase(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    String_toLowerCase(env, mode, args)
}

// 15.5.4.18 String.prototype.toUpperCase ( )
pub fn String_toUpperCase(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let string = try!(get_this_string(env, &args)).to_string().to_uppercase();
    let string = JsString::from_str(env, &string);
    
    Ok(string.as_value())
}

// 15.5.4.19 String.prototype.toLocaleUpperCase ( )
pub fn String_toLocaleUpperCase(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    String_toUpperCase(env, mode, args)
}


// 15.5.4.6 String.prototype.concat ( [ string1 [ , string2 [ , … ] ] ] )
pub fn String_concat(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let string = try!(get_this_string(env, &args));
    
    let mut strings = Vec::with_capacity(args.argc + 1);
    strings.push(string);
    
    for i in 0..args.argc {
        let arg = try!(args.arg(env, i).to_string(env));
        strings.push(arg);
    }
    
    Ok(JsString::concat(env, &strings).as_value())
}


// 15.5.4.9 String.prototype.localeCompare (that)
pub fn String_localeCompare(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let string = try!(get_this_string(env, &args));
    let that = try!(args.arg(env, 0).to_string(env));
    
    let string = string.chars();
    let string_len = string.len();
    let that = that.chars();
    let that_len = that.len();
    let len = min(string_len, that_len);
    
    for i in 0..len {
        let string_c = string[i];
        let that_c = that[i];
        
        if string_c < that_c {
            return Ok(JsValue::new_number(-1.0));
        }
        if string_c > that_c {
            return Ok(JsValue::new_number(1.0));
        }
    }
    
    let result = if string_len < that_len {
        -1.0
    } else if string_len > that_len {
        1.0
    } else {
        0.0
    };
    
    Ok(JsValue::new_number(result))
}

// 15.5.4.10 String.prototype.match (regexp)
pub fn String_match(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let string = try!(get_this_string(env, &args)).as_value();
    
    let regex = args.arg(env, 0);
    let mut rx = if regex.class() == Some(name::REGEXP_CLASS) {
        regex
    } else {
        try!(env.handle(JsHandle::RegExpClass).construct(env, vec![regex]))
    };
    
    let global = rx.unwrap_object().value(env).unwrap_regexp().global();
    
    let exec = try!(rx.get(env, name::EXEC));
    
    let result = if !global {
        try!(exec.call(env, rx, vec![string], false))
    } else {
        let value = JsValue::new_number(0.0);
        try!(rx.put(env, name::LAST_INDEX, value, true));
        
        let mut array = env.create_array();
        
        let mut previous_last_index = 0;
        let mut n = 0;
        
        loop {
            let result = try!(exec.call(env, rx, vec![string], false));
            if result.is_null() {
                break;
            }
            
            let this_index = try!(try!(rx.get(env, name::LAST_INDEX)).to_integer(env)) as isize;
            if this_index == previous_last_index {
                let value = JsValue::new_number((this_index + 1) as f64);
                try!(rx.put(env, name::LAST_INDEX, value, true));
                previous_last_index = this_index + 1;
            } else {
                previous_last_index = this_index;
            }
            
            let match_str = try!(result.get(env, Name::from_index(0)));
            try!(array.define_own_property(
                env,
                Name::from_index(n),
                JsDescriptor::new_simple_value(match_str),
                false
            ));
            
            n += 1
        }
        
        if n == 0 {
            JsValue::new_null()
        } else {
            array.as_value()
        }
    };
    
    Ok(result)
}

// 15.5.4.11 String.prototype.replace (searchValue, replaceValue)
pub fn String_replace(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    replacer::Replacer::replace(env, args, mode.strict())
}

// 15.5.4.12 String.prototype.search (regexp)
pub fn String_search(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let string = try!(get_this_string(env, &args));
    
    let regex = args.arg(env, 0);
    let rx = if regex.class() == Some(name::REGEXP_CLASS) {
        regex
    } else {
        try!(env.handle(JsHandle::RegExpClass).construct(env, vec![regex]))
    };
    
    let regexp = rx.unwrap_object().value(env).unwrap_regexp();
    
    let string = string.to_string();
    
    let result = if let Some((start, _)) = regexp.regex().find(&string) {
        start as f64
    } else {
        -1.0
    };
    
    Ok(JsValue::new_number(result))
}

// 15.5.4.13 String.prototype.slice (start, end)
pub fn String_slice(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let string = try!(get_this_string(env, &args));
    
    let len = string.chars().len() as f64;
    
    let start = try!(args.arg(env, 0).to_integer(env));
    let end = {
        let end = args.arg(env, 1);
        if end.is_undefined() {
            len
        } else {
            try!(end.to_integer(env))
        }
    };
    
    let start = if start < 0.0 {
        (len + start).max(0.0)
    } else {
        start.min(len)
    };
    let end = if end < 0.0 {
        (len + end).max(0.0)
    } else {
        end.min(len)
    };
    
    if start >= len {
        Ok(JsString::from_str(env, "").as_value())
    } else {
        let span = (end - start).max(0.0);
        
        Ok(JsString::from_u16(env, &string.chars()[(start as usize)..((start + span) as usize)]).as_value())
    }
}

// 15.5.4.14 String.prototype.split (separator, limit)
pub fn String_split(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    struct MatchResult {
        index: usize,
        end_index: usize,
        captures: Vec<JsValue>
    }
    
    fn split_match(env: &mut JsEnv, string: JsValue, offset: usize, separator: JsValue) -> JsResult<Option<MatchResult>> {
        if separator.class() == Some(name::REGEXP_CLASS) {
            let exec = try!(separator.get(env, name::EXEC));
            let string = JsString::from_u16(env, &string.unwrap_string().chars()[offset..]).as_value();
            let matches = try!(exec.call(env, separator, vec![string], false));
            
            if matches.is_null() {
                Ok(None)
            } else {
                let index = try!(try!(matches.get(env, name::INDEX)).to_integer(env)) as usize;
                let length = try!(try!(matches.get(env, name::LENGTH)).to_integer(env)) as usize;
                // The value is coerced to a String because we need the byte length.
                let matched_length = try!(try!(matches.get(env, Name::from_index(0))).to_string(env)).to_string().len();
                
                let mut captures = Vec::new();
                
                for i in 1..length {
                    captures.push(try!(matches.get(env, Name::from_index(i))));
                }
                
                Ok(Some(MatchResult {
                    index: offset + index,
                    end_index: offset + index + matched_length,
                    captures: captures
                }))
            }
        } else {
            let separator = separator.unwrap_string();
            let separator = separator.chars();
            let length = separator.len();
            
            let string = string.unwrap_string();
            let string = string.chars();
            let string_length = string.len();
            
            if offset + length > string_length {
                Ok(None)
            } else {
                for i in 0..length {
                    if string[offset + i] != separator[i] {
                        return Ok(None);
                    }
                } 
                
                Ok(Some(MatchResult {
                    index: offset,
                    end_index: offset + length,
                    captures: Vec::new()
                }))
            }
        }
    }
    
    let string = try!(get_this_string(env, &args));
    let string_value = string.as_value();
    
    let mut array = env.create_array();
    let mut array_length = 0;
    
    let limit = {
        let limit = args.arg(env, 1);
        if limit.is_undefined() {
            u32::MAX - 1
        } else {
            try!(limit.to_uint32(env))
        }
    };
    
    let string_length = string.chars().len();
    
    let separator = {
        let separator = args.arg(env, 0);
        if separator.class() == Some(name::REGEXP_CLASS) {
            separator
        } else if separator.is_undefined() {
            separator
        } else {
            try!(separator.to_string(env)).as_value()
        }
    };
    
    if limit == 0 {
        Ok(array.as_value())
    } else {
        if separator.is_undefined() {
            try!(array.define_own_property(
                env,
                Name::from_index(0),
                JsDescriptor::new_simple_value(string_value),
                false
            ));
        } else if string_length == 0 {
            if try!(split_match(env, string_value, 0, separator)).is_none() {
                try!(array.define_own_property(
                    env,
                    Name::from_index(0),
                    JsDescriptor::new_simple_value(string_value),
                    false
                ));
            }
        } else {
            let mut offset = 0;
            let mut last_offset = 0;
            
            while offset < string_length {
                match try!(split_match(env, string_value, offset, separator)) {
                    None => {
                        offset += 1;
                    }
                    Some(match_result) => {
                        if match_result.end_index == last_offset {
                            offset += 1;
                        } else {
                            offset = match_result.index;
                            
                            let part = JsString::from_u16(env, &string.chars()[last_offset..offset]).as_value();
                            
                            try!(array.define_own_property(
                                env,
                                Name::from_index(array_length),
                                JsDescriptor::new_simple_value(part),
                                false
                            ));
                            
                            array_length += 1;
                            if array_length == limit as usize {
                                return Ok(array.as_value());
                            }
                            
                            last_offset = match_result.end_index;
                            
                            for capture in match_result.captures {
                                try!(array.define_own_property(
                                    env,
                                    Name::from_index(array_length),
                                    JsDescriptor::new_simple_value(capture),
                                    false
                                ));
                                
                                array_length += 1;
                                if array_length == limit as usize {
                                    return Ok(array.as_value())
                                }
                            }
                            
                            offset = last_offset;
                        }
                    }
                }
            }
            
            let remaining = JsString::from_u16(env, &string.chars()[last_offset..string_length]).as_value();
            try!(array.define_own_property(
                env,
                Name::from_index(array_length),
                JsDescriptor::new_simple_value(remaining),
                false
            ));
        }
        
        Ok(array.as_value())
    }
}

// 15.5.4.20 String.prototype.trim ( )
pub fn String_trim(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<JsValue> {
    let string = try!(get_this_string(env, &args));
    
    let chars = string.chars();
    let length = chars.len();
    let mut start = 0;
    
    while start < length {
        if let Some(c) = char::from_u32(chars[start] as u32) {
            if !(is_whitespace(c) || is_line_terminator(c)) {
                break;
            }
        } else {
            break;
        }
        
        start += 1;
    }
    
    if start < length {
        let mut end = length;
        
        loop {
            if let Some(c) = char::from_u32(chars[end - 1] as u32) {
                if !(is_whitespace(c) || is_line_terminator(c)) {
                    break;
                }
            } else {
                break;
            }
            
            end -= 1;
        }
        
        Ok(JsString::from_u16(env, &chars[start..end]).as_value())
    } else {
        Ok(JsString::from_str(env, "").as_value())
    }
}
