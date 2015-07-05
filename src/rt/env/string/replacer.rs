use rt::{JsEnv, JsArgs, JsValue, JsItem, JsString, JsHandle, JsFunction};
use ::JsResult;
use gc::*;
use syntax::Name;
use syntax::token::name;
use std::char;

pub struct Replacer<'a> {
    env: &'a mut JsEnv,
    string: String,
    string_value: Local<JsValue>,
    replace_value: Local<JsValue>,
    replace_chars: Option<Vec<char>>,
    result: String,
    regexp_offset: usize,
    strict: bool
}

// 15.5.4.11 String.prototype.replace (searchValue, replaceValue)
impl<'a> Replacer<'a> {
    pub fn replace(env: &'a mut JsEnv, args: JsArgs, mut strict: bool) -> JsResult<Local<JsValue>> {
        let string = args.this(env);
        try!(string.check_object_coercible(env));
        let string = try!(string.to_string(env));
        
        let string_value = string.as_value(env);
        
        let search_value = args.arg(env, 0);
        
        // Search value is resolved before replace value is touched. We don't use
        // the result here; this is just for conformance.
        if search_value.class(env) != Some(name::REGEXP_CLASS) {
            try!(search_value.to_string(env));
        }
        
        let replace_value = args.arg(env, 1);
        let replace_chars = if replace_value.is_callable(env) {
            // TODO #86: This logic shouldn't be here, but in a central place so that
            // it can be re-used generically for callbacks.
            if !strict {
                strict = match replace_value.unwrap_object(env).function().unwrap() {
                    JsFunction::Ir(function_ref) => env.ir.get_function(function_ref).strict,
                    _ => false
                };
            }
            
            None
        } else {
            Some(try!(replace_value.to_string(env)).to_string().chars().collect::<Vec<_>>())
        };
        
        let mut replacer = Replacer {
            env: env,
            string: string.to_string(),
            string_value: string_value,
            replace_value: replace_value,
            replace_chars: replace_chars,
            result: String::new(),
            regexp_offset: 0,
            strict: strict
        };
        
        if search_value.class(replacer.env) == Some(name::REGEXP_CLASS) {
            let string = string.as_value(replacer.env);
            try!(replacer.replace_regexp(string, search_value));
        } else {
            let search_value = try!(search_value.to_string(replacer.env));
            try!(replacer.replace_string(string, search_value));
        }
        
        Ok(JsString::from_str(replacer.env, &replacer.result).as_value(replacer.env))
    }
    
    fn replace_regexp(&mut self, string: Local<JsValue>, mut search_value: Local<JsValue>) -> JsResult<()> {
        let global = search_value.unwrap_object(self.env).value(self.env).unwrap_regexp(self.env).global();
        
        let exec = try!(search_value.get(self.env, name::EXEC));
        
        if !global {
            let matches = try!(exec.call(self.env, search_value, vec![string], false));
            try!(self.process_regexp_match(matches));
        } else {
            let value = self.env.new_number(0.0);
            try!(search_value.put(self.env, name::LAST_INDEX, value, true));
            
            let mut previous_last_index = 0;
            
            loop {
                let matches = try!(exec.call(self.env, search_value, vec![string], false));
                if matches.is_null() {
                    break;
                }
                
                let this_index = try!(try!(search_value.get(self.env, name::LAST_INDEX)).to_integer(self.env)) as isize;
                if this_index == previous_last_index {
                    let value = self.env.new_number((this_index + 1) as f64);
                    try!(search_value.put(self.env, name::LAST_INDEX, value, true));
                    previous_last_index = this_index + 1;
                } else {
                    previous_last_index = this_index;
                }
                
                try!(self.process_regexp_match(matches));
            }
        }
        
        // Copy the remainder of string to the result.
        
        if self.regexp_offset < self.string.len() {
            self.result.push_str(&self.string[self.regexp_offset..]);
        }
        
        Ok(())
    }
    
    fn process_regexp_match(&mut self, matches: Local<JsValue>) -> JsResult<()> {
        let length = try!(try!(matches.get(self.env, name::LENGTH)).to_integer(self.env)) as usize;
        
        let matched = try!(try!(matches.get(self.env, Name::from_index(0))).to_string(self.env)).to_string();
        let mut captures = Vec::new();
        
        for i in 0..(length - 1) {
            captures.push(try!(try!(matches.get(self.env, Name::from_index(i + 1))).to_string(self.env)).to_string());
        }
        
        let index = try!(try!(matches.get(self.env, name::INDEX)).to_integer(self.env)) as usize;
        
        // Copy the part of string we've skipped over to the result.
        
        if index > self.regexp_offset {
            self.result.push_str(&self.string[self.regexp_offset..index]);
        }
        
        self.regexp_offset = index + matched.len();
        
        // Process the match.
        
        self.process_match(matched, captures, index)
    }
    
    fn replace_string(&mut self, string: Local<JsString>, search_value: Local<JsString>) -> JsResult<()> {
        if search_value.chars().len() == 0 {
            return Ok(());
        }
        
        let chars = string.chars();
        let string_len = chars.len();
        let search_value = search_value.chars();
        let search_value_len = search_value.len();
        
        let found = if search_value_len <= string_len {
            let mut offset = 0;
            let mut found = true;
            
            while offset <= (string_len - search_value_len) {
                found = true;
                
                for i in 0..search_value_len {
                    if chars[offset + i] != search_value[i] {
                        found = false;
                        break;
                    }
                }
                
                if found {
                    break;
                }
                
                offset += 1;
            }
            
            if found {
                // Copy the text we skipped over to the output.
                
                for i in 0..offset {
                    self.result.push(char::from_u32(chars[i] as u32).unwrap_or('�'));
                }
                
                // Process this match.
                
                let matched = ::rt::utf::utf16_to_string(&chars[offset..(offset + search_value_len)]);
                try!(self.process_match(matched, Vec::new(), offset));
                
                // Copy the rest of the search text.
                
                // Refetch chars after GC.
                let chars = string.chars();
                
                for i in (offset + search_value_len)..string_len {
                    self.result.push(char::from_u32(chars[i] as u32).unwrap_or('�'));
                }
            }
            
            found
        } else {
            false
        };
        
        // If there wasn't a match, the result is the search text.
        
        if !found {
            self.result = self.string.clone();
        }
        
        Ok(())
    }
    
    fn process_match(&mut self, matched: String, captures: Vec<String>, index: usize) -> JsResult<()> {
        if self.replace_chars.is_some() {
            self.process_match_string(matched, captures, index)
        } else {
            self.process_match_function(matched, captures, index)
        }
    }
    
    fn process_match_string(&mut self, matched: String, captures: Vec<String>, index: usize) -> JsResult<()> {
        let replace = self.replace_chars.as_ref().unwrap();
        
        let mut i = 0;
        while i < replace.len() {
            let c = replace[i];
            
            if c == '$' && i + 1 < replace.len() {
                match replace[i + 1] {
                    '$' => {
                        i += 2;
                        self.result.push('$');
                        continue;
                    }
                    '&' => {
                        i += 2;
                        self.result.push_str(&matched);
                        continue;
                    }
                    '`' => {
                        i += 2;
                        self.result.push_str(&self.string[0..index]);
                        continue;
                    }
                    '\'' => {
                        i += 2;
                        self.result.push_str(&self.string[(index + matched.len())..]);
                        continue;
                    }
                    c @ _ => {
                        if c >= '0' && c <= '9' {
                            let mut capture = c as u32 - '0' as u32;
                            i += 2;
                            
                            // The behavior of multi digit captures is implementation defined.
                            // When we won't have enough captures, we don't capture the second
                            // digit.
                            if captures.len() > 10 && i + 2 < replace.len() {
                                let c = replace[i + 2];
                                if c >= '0' && c <= '9' {
                                    capture = capture * 10 + (c as u32 - '0' as u32);
                                    i += 1;
                                }
                            }
                            
                            if capture == 0 {
                                // $0 is not a valid capture so falls under the rule that the
                                // escape is ignored and copied verbatim.
                                self.result.push_str("$0");
                            } else {
                                // The behavior here is implementation defined. I choose to not
                                // throw when the capture is out of range. Instead we silently
                                // ignore this error.
                                
                                let capture = (capture - 1) as usize;
                                if capture < captures.len() {
                                    self.result.push_str(&captures[capture]);
                                }
                            }
                        } else {
                            i += 1;
                            self.result.push('$');
                            self.result.push(c);
                        }
                    }
                }
            } else {
                i += 1;
                self.result.push(c);
            }
        }
        
        Ok(())
    }
    
    fn process_match_function(&mut self, matched: String, captures: Vec<String>, index: usize) -> JsResult<()> {
        let mut args = Vec::new();
        
        args.push(JsString::from_str(self.env, &matched).as_value(self.env));
        
        for capture in captures {
            args.push(JsString::from_str(self.env, &capture).as_value(self.env));
        }
        
        args.push(self.env.new_number(index as f64));
        args.push(self.string_value);
        
        let this = if self.strict {
            self.env.new_undefined()
        } else {
            self.env.handle(JsHandle::Global).as_value(self.env)
        };
        
        let result = try!(self.replace_value.call(self.env, this, args, false));
        
        self.result.push_str(&try!(result.to_string(self.env)).to_string());
        
        Ok(())
    }
}
