use gc::{Array, Local};
use super::{JsEnv, JsValue};
use super::utf;

pub struct JsString {
	// TODO: Make private.
	pub chars: Array<u16>
}

impl JsString {
	pub fn new_local(env: &JsEnv, size: usize) -> Local<JsString> {
		let mut result = env.alloc_local_string();
		unsafe {
			result.chars = env.alloc_char_array(size);
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
	
	pub fn to_string(&self) -> String {
		String::from_utf16(&*self.chars).ok().unwrap()
	}
}

impl Local<JsString> {
	pub fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_string(self.as_ptr()).as_local(env)
	}
}
