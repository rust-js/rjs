use ::{JsResult, JsError};
use rt::{JsEnv, JsArgs, JsValue, JsType, JsItem, JsObject, JsString, JsFnMode, JsDescriptor};
use rt::object::JsStoreKey;
use gc::*;
use syntax::token::name;
use syntax::Name;
use std::fmt::Write;
use std::collections::HashSet;
use std::rc::Rc;

pub struct JsonWriter<'a> {
    env: &'a mut JsEnv,
    stack: Vec<ptr_t>,
    value: JsValue,
    replacer: Option<JsValue>,
    property_list: Option<Rc<Vec<Name>>>,
    strict: bool
}

impl<'a> JsonWriter<'a> {
    pub fn new(env: &'a mut JsEnv, mode: JsFnMode, args: JsArgs) -> JsResult<JsonWriter<'a>> {
        let value = args.arg(env, 0);
        let replacer_arg = args.arg(env, 1);
        
        let (replacer, property_list) = if replacer_arg.is_callable() {
            (Some(replacer_arg), None)
        } else if replacer_arg.class() == Some(name::ARRAY_CLASS) {
            let mut property_list = Vec::new();
            
            let length = try!(value.get(env, name::LENGTH)).unwrap_number() as usize;
            
            for i in 0..length {
                let element = try!(value.get(env, Name::from_index(i)));
                
                let include = match element.ty() {
                    JsType::String | JsType::Number => true,
                    JsType::Object => {
                        match element.class() {
                            Some(name::STRING_CLASS) | Some(name::NUMBER_CLASS) => true,
                            _ => false
                        }
                    }
                    _ => false
                };
                
                if include {
                    let name = try!(element.to_string(env)).to_string();
                    let name = env.intern(&name);
                    if !property_list.contains(&name) {
                        property_list.push(name);
                    }
                }
            }
            
            (None, Some(Rc::new(property_list)))
        } else {
            (None, None)
        };
        
        Ok(JsonWriter {
            env: env,
            stack: Vec::new(),
            value: value,
            replacer: replacer,
            property_list: property_list,
            strict: mode.strict()
        })
    }
    
    pub fn write(&mut self) -> JsResult<JsValue> {
        let value = self.value;
        let value = try!(self.transform(value, None, None));
        
        if self.ignore(value) {
            Ok(JsValue::new_undefined())
        } else {
            let mut json = String::new();
            
            try!(self.write_value(&mut json, value));
            
            Ok(JsString::from_str(self.env, &json).as_value())
        }
    }
    
    fn ignore(&mut self, value: JsValue) -> bool {
        value.is_undefined() || value.is_callable()
    }
    
    fn write_string(&mut self, json: &mut String, value: &str) {
        json.push('"');
        
        for c in value.chars() {
            match c {
                '"' => json.push_str("\\\""),
                '\\' => json.push_str("\\\\"),
                '\u{8}' => json.push_str("\\b"),
                '\u{C}' => json.push_str("\\f"),
                '\n' => json.push_str("\\n"),
                '\r' => json.push_str("\\r"),
                '\t' => json.push_str("\\t"),
                _ => {
                    if c < ' ' {
                        write!(json, "\\u{:04x}", c as u32).ok().unwrap();
                    } else {
                        json.push(c);
                    }
                }
            }
        }
        
        json.push('"');
    }
    
    fn write_number(&mut self, json: &mut String, value: f64) -> JsResult<()> {
        if value.is_finite() {
            // TODO #61: This is very wrong. See 9.8.1 ToString Applied to the Number Type
            // for the full specifications. A C# implementation can be found at
            // http://jurassic.codeplex.com/SourceControl/latest#Jurassic/Core/NumberFormatter.cs.
            json.push_str(&value.to_string());
        } else {
            json.push_str("null");
        }
        
        Ok(())
    }
    
    fn write_element(&mut self, json: &mut String, object: Local<JsObject>, name: Name, value: JsValue, had_one: &mut bool) -> JsResult<()> {
        let element = try!(object.get(self.env, name));
        let element = try!(self.transform(element, Some(name), Some(value)));
        if !self.ignore(element) {
            if *had_one {
                json.push(',');
            } else {
                *had_one = true;
            }
            
            let string = &*self.env.ir.interner().get(name);
            self.write_string(json, string);
            json.push(':');
            try!(self.write_value(json, element));
        }
        
        Ok(())
    }
    
    fn write_object(&mut self, json: &mut String, value: JsValue) -> JsResult<()> {
        let mut object = value.unwrap_object();
        let ptr = object.as_ptr().ptr();
        
        if self.stack.contains(&ptr) {
            return Err(JsError::new_type(self.env, ::errors::TYPE_CYCLICAL_REFERENCE));
        }
        
        self.stack.push(ptr);
        
        json.push('{');
        
        let mut had_one = false;
        
        if let Some(property_list) = self.property_list.clone() {
            for name in property_list.iter() {
                try!(self.write_element(json, object, *name, value, &mut had_one));
            }
        } else {
            let mut offset = 0;
            let mut seen = HashSet::new();
            
            loop {
                match object.get_key(self.env, offset) {
                    JsStoreKey::Key(name, enumerable) => {
                        offset += 1;
                        
                        if seen.insert(name) && enumerable {
                            try!(self.write_element(json, object, name, value, &mut had_one));
                        }
                    }
                    JsStoreKey::Missing => {
                        offset += 1;
                    }
                    JsStoreKey::End => {
                        if let Some(prototype) = object.prototype(self.env) {
                            object = prototype.unwrap_object();
                            
                            offset = 0;
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        
        json.push('}');
        
        self.stack.pop();
        
        Ok(())
    }
    
    fn write_array(&mut self, json: &mut String, value: JsValue) -> JsResult<()> {
        let object = value.unwrap_object();
        let ptr = object.as_ptr().ptr();
        
        if self.stack.contains(&ptr) {
            return Err(JsError::new_type(self.env, ::errors::TYPE_CYCLICAL_REFERENCE))
        }
        
        self.stack.push(ptr);
        
        json.push('[');
        
        let length = try!(object.get(self.env, name::LENGTH)).unwrap_number() as usize;
        
        for i in 0..length {
            if i > 0 {
                json.push(',');
            }
            
            let index = Name::from_index(i);
            let element = try!(object.get(self.env, index));
            let element = try!(self.transform(element, Some(index), Some(value)));
            
            if self.ignore(element) {
                json.push_str("null");
            } else {
                try!(self.write_value(json, element));
            }
        }
        
        json.push(']');
        
        self.stack.pop();
        
        Ok(())
    }
    
    fn key_to_string(&mut self, key: Option<Name>) -> JsValue {
        match key {
            Some(name) => JsString::from_str(self.env, &*self.env.ir.interner().get(name)).as_value(),
            None => JsValue::new_undefined()
        }
    }
    
    fn transform(&mut self, mut value: JsValue, key: Option<Name>, holder: Option<JsValue>) -> JsResult<JsValue> {
        if value.ty() == JsType::Object {
            let to_json = try!(value.get(self.env, name::TO_JSON));
            if to_json.is_callable() {
                let args = vec![self.key_to_string(key)];
                value = try!(to_json.call(self.env, value, args, self.strict));
            }
        }
        
        if let Some(replacer) = self.replacer {
            let holder = match holder {
                Some(holder) => holder,
                None => {
                    let mut holder = self.env.create_object();
                    try!(holder.define_own_property(self.env, name::EMPTY, JsDescriptor::new_simple_value(value), false));
                    holder.as_value()
                }
            };
            
            let args = vec![self.key_to_string(key), value];
            value = try!(replacer.call(self.env, holder, args, self.strict));
        }
        
        Ok(value)
    }
    
    fn write_value(&mut self, json: &mut String, mut value: JsValue) -> JsResult<()> {
        // Ignored values must be handled higher up.
        assert!(!self.ignore(value));
        
        match value.class() {
            Some(name::NUMBER_CLASS) | Some(name::STRING_CLASS) | Some(name::BOOLEAN_CLASS)
                => value = value.unwrap_object().value(self.env),
            Some(name::ARRAY_CLASS) => return self.write_array(json, value),
            _ => {}
        }
        
        match value.ty() {
            JsType::Null => json.push_str("null"),
            JsType::Boolean => if value.unwrap_bool() {
                json.push_str("true");
            } else {
                json.push_str("false");
            },
            JsType::String => {
                let string = value.unwrap_string().to_string();
                self.write_string(json, &string);
            }
            JsType::Number => try!(self.write_number(json, value.unwrap_number())),
            _ => try!(self.write_object(json, value))
        }
        
        Ok(())
    }
}
