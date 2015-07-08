extern crate regex;

use rt::{JsEnv, JsValue, JsString, GC_REGEXP};
use rt::validate_walker_field;
use gc::*;
use self::regex::Regex;
use util::manualbox::ManualBox;
use ::{JsResult, JsError};
use std::mem::{transmute, zeroed, size_of};

// Modifications to this struct must be synchronized with the GC walker.
#[repr(C)]
pub struct JsRegExp {
    pattern: Ptr<JsString>,
    flags: Ptr<JsString>,
    regex: ManualBox<Regex>,
    global: bool,
    ignore_case: bool,
    multiline: bool
}

impl JsRegExp {
    pub fn new_local(env: &mut JsEnv, pattern: Local<JsString>, flags: Local<JsString>, global: bool, ignore_case: bool, multiline: bool) -> JsResult<Local<JsRegExp>> {
        fn is_hex_char(c: char) -> bool {
            (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
        }
        
        let mut result = env.heap.alloc_local::<JsRegExp>(GC_REGEXP);
        
        result.pattern = pattern.as_ptr();
        result.flags = flags.as_ptr();
        result.global = global;
        result.ignore_case = ignore_case;
        result.multiline = multiline;
        
        let pattern = pattern.to_string().chars().collect::<Vec<_>>();
        let mut translated = String::with_capacity(pattern.len());
        
        // Translate the JavaScript regular expression into a Rust regular expression.
        // Currently we support the following translations:
        //
        //   * Ignore case to (?i) and multiline to (?m);
        //   * \cA - \cZ to its corresponding control code, which is translated to \xXX;
        //   * \uXXXX to \x{XXXX};
        //   * All characters that can be escaped using \, which is translated to \xXX.
        //
        
        if ignore_case || multiline {
            if ignore_case {
                translated.push_str("(?i)");
            }
            if multiline {
                translated.push_str("(?m)");
            }
        }
        
        let mut i = 0;
        let mut in_class = false;
        
        while i < pattern.len() {
            match pattern[i] {
                '[' if !in_class => {
                    in_class = true;
                    translated.push('[');
                }
                ']' if in_class => {
                    in_class = false;
                    translated.push(']');
                }
                '\\' => {
                    if i + 2 < pattern.len() && pattern[i + 1] == 'c' {
                        let c = pattern[i + 2];
                        if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') {
                            i += 2;
                            translated.push_str(&format!("\\x{:02X}", (c as u32) % 32));
                        } else {
                            // If the letter of the control code is invalid, escape the \
                            // to make it a valid regular expression.
                            translated.push_str("\\\\");
                        }
                    } else if
                        i + 5 < pattern.len() &&
                        pattern[i + 1] == 'u' &&
                        is_hex_char(pattern[i + 2]) &&
                        is_hex_char(pattern[i + 3]) &&
                        is_hex_char(pattern[i + 4]) &&
                        is_hex_char(pattern[i + 5])
                    {
                        translated.push_str("\\x{");
                        i += 1;
                        for _ in 0..4 {
                            i += 1;
                            translated.push(pattern[i]);
                        }
                        translated.push('}');
                    } else if i + 1 < pattern.len() {
                        // Patterns like \[ match the raw [ character, which is supported by
                        // the regex crate. However, JavaScript supports more of these escapes.
                        // Here all such escapes are translated to their \xXX format.
                        
                        match pattern[i + 1] {
                            c @ '~' | c @ '`' | c @ '!' | c @ '@' | c @ '#' | c @ '$' | c @ '%' | c @ '^' | c @ '&' |
                            c @ '*' | c @ '(' | c @ ')' | c @ '-' | c @ '+' | c @ '=' | c @ '{' | c @ '[' | c @ '}' |
                            c @ ']' | c @ '|' | c @ '\\' | c @ ':' | c @ ';' | c @ '\'' | c @ '<' | c @ ',' | c @ '>' |
                            c @ '.' | c @ '/' | c @ '?' | c @ '"' => {
                                i += 1;
                                translated.push_str(&format!("\\x{:02X}", c as u32));
                            }
                            'b' => {
                                if in_class {
                                    // Backspace escape sequence (only applicable in a character class) is not supported by
                                    // regex; translate to \x08.
                                    i += 1;
                                    translated.push_str("\\x08");
                                } else {
                                    // Otherwise it's just a word boundary escape.
                                    translated.push('\\');
                                }
                            }
                            'd' | 'D' | 'w' | 'W' | 's' | 'S' | 't' | 'r' | 'n' | 'x' | 'B' => {
                                // These are supported escapes, which match assertions or character classes.
                                translated.push('\\');
                            }
                            'v' => {
                                // Vertical tab escape sequence is not supported by regex; translate to \x0B.
                                i += 1;
                                translated.push_str("\\x0B");
                            }
                            'f' => {
                                // Form feed escape sequence is not supported by regex; translate to \x0C.
                                i += 1;
                                translated.push_str("\\x0C");
                            }
                            '0' => {
                                // NUL escape sequence is not supported by regex; translate to \x00.
                                i += 1;
                                translated.push_str("\\x00");
                            }
                            _ => {
                                // When the character following the backslash doesn't match any of the
                                // supported characters, it's dropped.
                            }
                        }
                    } else {
                        translated.push('\\');
                    }
                }
                c @ _ => translated.push(c)
            }
            
            i += 1;
        }
        
        let regex = match Regex::new(&translated) {
            Ok(regex) => regex,
            Err(..) => return Err(JsError::new_syntax(env, ::errors::SYNTAX_INVALID_REGEX))
        };
        
        result.regex = ManualBox::new(regex);
        
        Ok(result)
    }
    
    pub fn finalize(&mut self) {
        self.regex.drop();
    }
}

impl Local<JsRegExp> {
    pub fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
        env.new_regexp(*self)
    }
    
    pub fn regex<'a>(&'a self) -> &'a Regex {
        &*self.regex
    }
    
    pub fn pattern(&self, env: &JsEnv) -> Local<JsString> {
        self.pattern.as_local(env)
    }
    
    pub fn flags(&self, env: &JsEnv) -> Local<JsString> {
        self.flags.as_local(env)
    }
    
    pub fn global(&self) -> bool {
        self.global
    }
    
    pub fn ignore_case(&self) -> bool {
        self.ignore_case
    }
    
    pub fn multiline(&self) -> bool {
        self.multiline
    }
}

pub unsafe fn validate_walker(walker: &GcWalker) {
    let mut object : Box<JsRegExp> = Box::new(zeroed());
    let ptr = transmute::<_, ptr_t>(&*object);
    
    object.pattern = Ptr::from_ptr(transmute(1usize));
    validate_walker_field(walker, GC_REGEXP, ptr, true);
    object.pattern = Ptr::null();
    
    object.flags = Ptr::from_ptr(transmute(1usize));
    validate_walker_field(walker, GC_REGEXP, ptr, true);
    object.flags = Ptr::null();
    
    object.regex = transmute(1usize);
    validate_walker_field(walker, GC_REGEXP, ptr, false);
    object.regex = transmute(0usize);
    
    object.global = true;
    validate_walker_field(walker, GC_REGEXP, ptr, false);
    object.global = false;
    
    object.ignore_case = true;
    validate_walker_field(walker, GC_REGEXP, ptr, false);
    object.ignore_case = false;
    
    object.multiline = true;
    validate_walker_field(walker, GC_REGEXP, ptr, false);
    object.multiline = false;
    
    assert_eq!(size_of::<JsRegExp>(), 32);
}
