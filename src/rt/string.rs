use gc::{Array, Local};
use rt::{JsEnv, JsValue, JsItem, JsDescriptor, JsHandle, GC_STRING, GC_U16};
use rt::utf;
use syntax::Name;
use syntax::token::name;

// Modifications to this struct must be synchronized with the GC walker.
pub struct JsString {
    chars: Array<u16>
}

impl JsString {
    pub fn new_local(env: &JsEnv, size: usize) -> Local<JsString> {
        let mut result = env.heap.alloc_local::<JsString>(GC_STRING);
        
        unsafe {
            result.chars = env.heap.alloc_array(GC_U16, size);
        }
        
        result
    }
    
    pub fn from_str<'a>(env: &'a JsEnv, string: &str) -> Local<JsString> {
        let chars = utf::utf32_to_utf16(
            &string.chars().map(|c| c as u32).collect::<Vec<_>>()[..],
            false
        );
        
        let mut result = Self::new_local(env, chars.len());
        
        {
            let result_chars = &mut *result.chars;
            
            for i in 0..chars.len() {
                result_chars[i] = chars[i];
            }
        }
        
        result
    }
    
    pub fn from_u16(env: &JsEnv, chars: &[u16]) -> Local<JsString> {
        let result = JsString::new_local(env, chars.len());
        let mut result_chars = result.chars;
        
        for i in 0..chars.len() {
            result_chars[i] = chars[i];
        }
        
        result
    }
    
    pub fn chars(&self) -> &[u16] {
        &*self.chars
    }
    
    pub fn concat<'a>(env: &'a JsEnv, lhs: Local<JsString>, rhs: Local<JsString>) -> Local<JsString> {
        let lhs_chars = &*lhs.chars;
        let rhs_chars = &*rhs.chars;
        let len = lhs_chars.len() + rhs_chars.len();
        
        let mut result = Self::new_local(&env, len);
        
        {
            let chars = &mut *result.chars;
            
            // The below is optimized to two memcpy's.
            
            for i in 0..lhs_chars.len() {
                chars[i] = lhs_chars[i];
            }
            let offset = lhs_chars.len();
            for i in 0..rhs_chars.len() {
                chars[offset + i] = rhs_chars[i];
            }
        }
        
        result
    }
    
    pub fn equals(x: Local<JsString>, y: Local<JsString>) -> bool {
        let x_chars = &*x.chars;
        let y_chars = &*y.chars;
        
        if x_chars.len() != y_chars.len() {
            false
        } else {
            for i in 0..x_chars.len() {
                if x_chars[i] != y_chars[i] {
                    return false
                }
            }
            
            true
        }
    }
    
    pub fn to_string(&self) -> String {
        String::from_utf16(&*self.chars).ok().unwrap()
    }
}

impl JsItem for Local<JsString> {
    fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
        env.new_string(*self)
    }
    
    fn has_prototype(&self, _: &JsEnv) -> bool {
        true
    }
    
    fn prototype(&self, env: &JsEnv) -> Option<Local<JsValue>> {
        Some(env.handle(JsHandle::String).as_value(env))
    }
    
    fn get_own_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
        if property == name::LENGTH {
            let value = env.new_number(self.chars.len() as f64);
            return Some(JsDescriptor::new_simple_value(value));
        }
        
        if let Some(index) = property.index() {
            let chars = self.chars;
            if index < chars.len() {
                let char = chars[index];
                let mut string = JsString::new_local(env, 1);
                string.chars[0] = char;
                return Some(JsDescriptor::new_simple_value(string.as_value(env)));
            }
        }
        
        None
    }
}
