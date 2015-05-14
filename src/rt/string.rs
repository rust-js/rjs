use gc::{Array, Local};
use super::{JsEnv, JsValue, JsItem, GC_STRING, GC_CHAR};
use super::utf;
use gc::Ptr;

pub struct JsString {
	// TODO: Make private.
	pub chars: Array<u16>
}

impl JsString {
	pub fn new_local(env: &JsEnv, size: usize) -> Local<JsString> {
		let mut result = env.heap.alloc_local::<JsString>(GC_STRING);
		unsafe {
			result.chars = env.heap.alloc_array(GC_CHAR, size);
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
	
	pub fn equals(x: Ptr<JsString>, y: Ptr<JsString>) -> bool {
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
		JsValue::new_string(self.as_ptr()).as_local(env)
	}
	
	fn has_prototype(&self, _: &JsEnv) -> bool {
		true
	}
	
	fn prototype(&self, env: &JsEnv) -> Option<Local<JsValue>> {
		Some(env.string_prototype.as_value(env))
	}
}
