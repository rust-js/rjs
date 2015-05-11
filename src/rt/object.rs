const INITIAL_OBJECT : usize = 20;

use syntax::ast::Name;
use super::{JsEnv, JsFunction, JsValue};
use gc::{Local, Ptr};
use super::hash::{Hash, Property};

pub struct JsObject {
	// TODO: Make private!
	pub class: Option<Name>,
	pub value: Option<JsValue>,
	pub function: Option<JsFunction>,
	pub prototype: Ptr<JsObject>,
	pub props: Hash
}

impl JsObject {
	pub fn new(env: &JsEnv) -> JsObject {
		JsObject {
			class: None,
			value: None,
			function: None,
			prototype: Ptr::null(),
			props: Hash::new(&env, INITIAL_OBJECT)
		}
	}
	
	pub fn new_local(env: &JsEnv) -> Local<JsObject> {
		let mut result = env.alloc_local_object();
		*result = Self::new(env);
		result
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.12.1
	// TODO: INCOMPLETE (getters/setters)
	pub fn get_own_property(&self, name: Name, env: &JsEnv) -> Option<Property> {
		self.props.get_value(name, env)
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.12.2
	// TODO: INCOMPLETE
	pub fn get_property(&self, name: Name) -> Option<Property> {
		let prototype = self.prototype;
		if prototype.is_null() {
			None
		} else {
			prototype.get_property(name)
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.12.9
	// TODO: INCOMPLETE
	pub fn define_own_property(&mut self, name: Name, property: &Property, _throw: bool, env: &JsEnv) {
		self.props.add(name, property, env);
	}
}

impl Local<JsObject> {
	pub fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_object(self.as_ptr()).as_local(env)
	}
}
