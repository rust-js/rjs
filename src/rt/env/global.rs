#![allow(unused_variables)]

use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsType, JsFnMode, JsString, JsHandle, JsItem};
use gc::*;
use syntax::parser::ParseMode;
use std::{char, f64};

pub fn Global_escape(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    unimplemented!();
}

pub fn Global_unescape(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    unimplemented!();
}

// 15.1.2.2 parseInt (string , radix)
pub fn Global_parseInt(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let string = try!(args.arg(env, 0).to_string(env));
    let mut radix = try!(args.arg(env, 1).to_int32(env)) as i64;
    
    let mut offset = 0;
    let chars = string.to_string().chars().collect::<Vec<_>>();
    
    while offset < chars.len() {
        if !chars[offset].is_whitespace() {
            break;
        }
        
        offset += 1;
    }
    
    let sign = if offset < chars.len() {
        match chars[offset] {
            '-' => {
                offset += 1;
                -1
            }
            '+' => {
                offset += 1;
                1
            }
            _ => {
                1
            }
        }
    } else {
        1
    };
    
    if radix != 0 {
        if radix < 2 || radix > 36 {
            return Ok(env.new_number(f64::NAN));
        }
    } else {
        radix = 10;
    }
    
    if radix == 16 && offset < chars.len() - 1 {
        if chars[offset] == '0' && (chars[offset + 1] == 'x' || chars[offset + 1] == 'X') {
            offset += 2;
        } 
    }
    
    let start = offset;
    let mut result = 0i64;
    
    while offset < chars.len() {
        let c = chars[offset];
        offset += 1;
        
        let digit = if c >= '0' && c <= '9' {
            c as i64 - '0' as i64
        } else if c >= 'a' && c <= 'z' {
            c as i64 - 'a' as i64 + 10
        } else if c >= 'A' && c <= 'Z' {
            c as i64 - 'A' as i64 + 10
        } else {
            break;
        };
        
        if digit >= radix {
            break;
        }
        
        result *= radix;
        result += digit;
    }
    
    if offset == start {
        Ok(env.new_number(f64::NAN))
    } else {
        Ok(env.new_number((result * sign) as f64))
    }
}

pub fn Global_parseFloat(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    unimplemented!();
}

// 15.1.2.4 isNaN (number)
pub fn Global_isNaN(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let result = try!(args.arg(env, 0).to_number(env)).is_nan();
    
    Ok(env.new_bool(result))
}

// 15.1.2.5 isFinite (number)
pub fn Global_isFinite(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let result = try!(args.arg(env, 0).to_number(env)).is_finite();
    
    Ok(env.new_bool(result))
}

// 15.1.2.1 eval (x)
// TODO: Execution context has not yet been implemented.
pub fn Global_eval(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    let arg = args.arg(env, 0);
    
    if arg.ty() != JsType::String {
        Ok(arg)
    } else {
        let arg = arg.unwrap_string(env).to_string();
        let global = env.handle(JsHandle::Global).as_value(env);
        let global_scope = env.global_scope.as_local(env);
        
        env.eval_scoped(&arg, mode.strict(), global, global_scope, ParseMode::Eval).map(|result| result.as_local(env))
    }
}

fn is_uri_unescape(c: char) -> bool {
    match c {
        'a'...'z' | 'A'...'Z' | '0'...'9' | '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' => true,
        _ => false
    }
}

fn is_uri_reserved(c: char) -> bool {
    match c {
        ';' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',' => true,
        _ => false
    }
}

fn decode<F: Fn(char) -> bool>(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs, is_reserved: F) -> JsResult<Local<JsValue>> {
    fn parse_hex(env: &mut JsEnv, c: char) -> JsResult<u8> {
        if c >= '0' && c <= '9' {
            Ok(((c as u32) - ('0' as u32)) as u8)
        } else if c >= 'a' && c <= 'f' {
            Ok(((c as u32) - ('a' as u32) + 10u32) as u8)
        } else if c >= 'A' && c <= 'F' {
            Ok(((c as u32) - ('A' as u32) + 10u32) as u8)
        } else {
            Err(JsError::new_uri(env))
        }
    }
    
    fn decode_part(env: &mut JsEnv, chars: &[u16], offset: usize) -> JsResult<Option<u8>> {
        if char::from_u32(chars[offset] as u32) == Some('%') {
            if offset + 2 >= chars.len() {
                Err(JsError::new_uri(env))
            } else {
                if let Some(c1) = char::from_u32(chars[offset + 1] as u32) {
                    if let Some(c2) = char::from_u32(chars[offset + 2] as u32) {
                        return Ok(Some(try!(parse_hex(env, c1)) * 16 + try!(parse_hex(env, c2))));
                    }
                }
                
                Err(JsError::new_uri(env))
            }
        } else {
            Ok(None)
        }
    }
    
    let string = try!(args.arg(env, 0).to_string(env));
    let chars = string.chars();
    let mut result = Vec::new();
    
    let len = chars.len();
    let mut i = 0;
    
    while i < len {
        if let Some(value) = try!(decode_part(env, chars, i)) {
            if value <= 127 {
                let c = value as char;
                if is_reserved(c) {
                    for j in 0..3 {
                        result.push(chars[i + j]);
                    }
                } else {
                    result.push(c as u16);
                }
                
                i += 3;
            } else {
                let start = i;
                
                let mut n = 1;
                while n <= 5 {
                    if (value << n) & 0x80 == 0 {
                        break;
                    }
                    n += 1;
                }
                
                if n == 1 || n > 4 {
                    return Err(JsError::new_uri(env));
                }
                
                let mut octets = Vec::with_capacity(n);
                octets.push(value);
                
                i += 3;
                
                if i + (3 * (n - 1)) > len {
                    return Err(JsError::new_uri(env));
                }
                
                for j in 1..n {
                    if let Some(value) = try!(decode_part(env, chars, i)) {
                        if value >> 6 != 0b10 {
                            return Err(JsError::new_uri(env));
                        }
                        
                        i += 3;
                        
                        octets.push(value);
                    } else {
                        return Err(JsError::new_uri(env));
                    }
                }
                
                let c = match String::from_utf8(octets) {
                    Ok(string) => {
                        let chars = string.chars().collect::<Vec<_>>();
                        assert_eq!(chars.len(), 1);
                        chars[0]
                    }
                    Err(..) => return Err(JsError::new_uri(env))
                };
                
                if is_reserved(c) {
                    for j in start..i {
                        result.push(chars[j]);
                    }
                } else {
                    let decoded = c as u32;
                    
                    if decoded < 0x10000 {
                        result.push(decoded as u16);
                    } else {
                        let low = ((decoded - 0x10000) & 0x3FF) + 0xDC00;
                        let high = (((decoded - 0x10000) >> 10) & 0x3FF) + 0xD800;
                        
                        result.push(high as u16);
                        result.push(low as u16);
                    }
                }
            }
        } else {
            result.push(chars[i]);
            
            i += 1;
        }
    }
    
    Ok(env.new_string(JsString::from_u16(env, &result)))
}

// 15.1.3 URI Handling Function Properties
fn encode<F: Fn(char) -> bool>(env: &mut JsEnv, _mode: JsFnMode, args: JsArgs, is_reserved: F) -> JsResult<Local<JsValue>> {
    fn to_hex(octet: u8) -> char {
        if octet < 10 {
            (('0' as u8) + octet) as char
        } else {
            (('A' as u8) + (octet - 10)) as char
        }
    }
    
    let string = try!(args.arg(env, 0).to_string(env));
    let chars = string.chars();
    let mut result = Vec::new();
    
    let len = chars.len();
    
    let mut i = 0;
    
    while i < len {
        let value = chars[i];
        
        let reserved = match char::from_u32(value as u32) {
            Some(c) => is_reserved(c),
            None => false
        };
        
        if reserved {
            result.push(value);
        } else {
            let code_point = if value >= 0xDC00 && value <= 0xDFFF {
                return Err(JsError::new_uri(env));
            } else if value < 0xD800 || value > 0xDBFF {
                value as u32
            } else {
                i += 1;
                if i == len {
                    return Err(JsError::new_uri(env));
                }
                
                let value2 = chars[i];
                if value2 < 0xDC00 || value2 > 0xDFFF {
                    return Err(JsError::new_uri(env));
                }
                
                ((value as u32 - 0xD800) * 0x400 + (value2 as u32 - 0xDC00) + 0x10000)
            };
            
            let c = match char::from_u32(code_point) {
                Some(c) => c,
                None => return Err(JsError::new_uri(env))
            };
            
            for octet in c.to_string().into_bytes() {
                result.push('%' as u16);
                result.push(to_hex(octet >> 4) as u16);
                result.push(to_hex(octet & 0xF) as u16);
            }
        }
        
        i += 1;
    }
    
    Ok(env.new_string(JsString::from_u16(env, &result)))
}

// 15.1.3.1 decodeURI (encodedURI)
pub fn Global_decodeURI(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    decode(env, mode, args, |c| is_uri_reserved(c) || c == '#')
}

// 15.1.3.2 decodeURIComponent (encodedURIComponent)
pub fn Global_decodeURIComponent(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    decode(env, mode, args, |_| false)
}

// 15.1.3.3 encodeURI (uri)
pub fn Global_encodeURI(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    encode(env, mode, args, |c| is_uri_reserved(c) || is_uri_unescape(c) || c == '#')
}

// 15.1.3.4 encodeURIComponent (uriComponent)
pub fn Global_encodeURIComponent(env: &mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<Local<JsValue>> {
    encode(env, mode, args, |c| is_uri_unescape(c))
}
