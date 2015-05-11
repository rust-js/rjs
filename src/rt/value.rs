extern crate libc;

use super::{JsEnv, JsString, JsType, JsObject};
use ::{JsResult, JsError};
use super::hash::{Property, PropertyValue};
use syntax::ast::Name;
use syntax::token::name;
use gc::{Local, Ptr};
use std::fmt;
use std::mem::transmute;

#[derive(Copy, Clone, PartialEq)]
pub struct JsValue {
	ty: JsType,
	value: JsRawValue
}

impl fmt::Debug for JsValue {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		try!(write!(fmt, "JsValue {{ ty: {:?}, value: ", self.ty));
		match self.ty {
			JsType::None => try!(write!(fmt, "none")),
			JsType::Undefined => try!(write!(fmt, "undefined")),
			JsType::Null => try!(write!(fmt, "null")),
			JsType::Number => try!(write!(fmt, "{}", self.get_number())),
			JsType::Boolean => try!(write!(fmt, "{}", self.get_bool())),
			JsType::String => try!(write!(fmt, "string")),
			JsType::Object => try!(write!(fmt, "object"))
		}
		try!(write!(fmt, " }}"));
		
		Ok(())
	}
}

impl JsValue {
	fn new_local(env: &JsEnv, ty: JsType, value: JsRawValue) -> Local<JsValue> {
		Self::new(ty, value).as_local(env)
	}
	
	pub fn as_local(&self, env: &JsEnv) -> Local<JsValue> {
		let mut result = env.alloc_local_value();
		*result = *self;
		result
	}
	
	fn new(ty: JsType, value: JsRawValue) -> JsValue {
		JsValue {
			ty: ty,
			value: value
		}
	}
	
	pub fn new_none() -> JsValue {
		JsValue {
			ty: JsType::None,
			value: JsRawValue::new()
		}
	}
	
	pub fn new_undefined() -> JsValue {
		JsValue {
			ty: JsType::Undefined,
			value: JsRawValue::new()
		}
	}
	
	pub fn new_null() -> JsValue {
		JsValue {
			ty: JsType::Null,
			value: JsRawValue::new()
		}
	}
	
	pub fn new_number(value: f64) -> JsValue {
		JsValue {
			ty: JsType::Number,
			value: JsRawValue::new_number(value)
		}
	}
	
	pub fn new_bool(value: bool) -> JsValue {
		JsValue {
			ty: JsType::Boolean,
			value: JsRawValue::new_bool(value)
		}
	}
	
	pub fn new_true() -> JsValue {
		Self::new_bool(true)
	}
	
	pub fn new_false() -> JsValue {
		Self::new_bool(false)
	}
	
	pub fn new_string(value: Ptr<JsString>) -> JsValue {
		JsValue {
			ty: JsType::String,
			value: JsRawValue::new_ptr(value)
		}
	}
	
	pub fn new_object(value: Ptr<JsObject>) -> JsValue {
		JsValue {
			ty: JsType::Object,
			value: JsRawValue::new_ptr(value)
		}
	}
	
	pub fn ty(&self) -> JsType {
		self.ty
	}
	
	fn value(&self) -> JsRawValue {
		self.value
	}
	
	pub fn is_null(&self) -> bool {
		self.ty == JsType::Null
	}
	
	pub fn is_undefined(&self) -> bool {
		self.ty == JsType::Undefined
	}
	
	pub fn get_number(&self) -> f64 {
		assert!(self.ty == JsType::Number);
		
		self.value.get_number()
	}
	
	pub fn set_number(&mut self, value: f64) {
		*self = JsValue::new_number(value)
	}
	
	pub fn get_bool(&self) -> bool {
		assert!(self.ty == JsType::Boolean);
		
		self.value.get_bool()
	}
	
	pub fn set_bool(&mut self, value: bool) {
		*self = JsValue::new_bool(value)
	}
	
	pub fn get_string(&self) -> Ptr<JsString> {
		assert!(self.ty == JsType::String);
		
		self.value.get_ptr()
	}
	
	pub fn set_string(&mut self, value: Ptr<JsString>) {
		*self = JsValue::new_string(value);
	}
	
	pub fn get_object(&self) -> Ptr<JsObject> {
		assert_eq!(self.ty, JsType::Object);
		
		self.value.get_ptr()
	}
	
	pub fn set_object(&mut self, value: Ptr<JsObject>) {
		*self = JsValue::new_object(value);
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.12.1
	// TODO: INCOMPLETE (getters/setters)
	pub fn get_own_property(&self, name: Name, env: &JsEnv) -> JsResult<Option<Property>> {
		match self.ty {
			JsType::Null | JsType::Undefined => Err(JsError::Type),
			JsType::Object => Ok(self.get_object().get_own_property(name, env)),
			JsType::String => Ok(env.string_prototype.get_own_property(name, env)),
			_ => panic!("{:?}", self.ty)
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.12.2
	// TODO: INCOMPLETE
	pub fn get_property(&self, name: Name, env: &JsEnv) -> JsResult<Option<Property>> {
		let value = try!(self.get_own_property(name, env));
		if value.is_some() {
			Ok(value)
		} else {
			match self.ty {
				JsType::Null | JsType::Undefined => Err(JsError::Type),
				JsType::Object => Ok(self.get_object().get_property(name)),
				JsType::String => Ok(env.string_prototype.get_property(name)),
				_ => panic!("{:?}", self.ty)
			}
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.12.9
	// TODO: INCOMPLETE
	pub fn define_own_property(&self, _name: Name, _value: Local<JsValue>) {
		unimplemented!();
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.12.3
	pub fn get(&self, name: Name, env: &JsEnv) -> JsResult<Local<JsValue>> {
		if let Some(property) = try!(self.get_property(name, env)) {
			match property.value {
				PropertyValue::Value { value } => Ok(value),
				PropertyValue::Accessor { get, .. } => {
					if get.is_undefined() {
						Ok(get)
					} else {
						unimplemented!();
					}
				}
			}
		} else {
			Ok(JsValue::new_undefined().as_local(env))
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.12.4
	// TODO: INCOMPLETE ([[Extensible]] is not checked)
	pub fn can_put(&self, name: Name, env: &JsEnv) -> JsResult<bool> {
		if let Some(property) = try!(self.get_own_property(name, env)) {
			match property.value {
				PropertyValue::Accessor { set, .. } => Ok(!set.is_undefined()),
				PropertyValue::Value { .. } => Ok(property.is_writable())
			}
		} else {
			match self.ty {
				JsType::Object => {
					let prototype = self.get_object().prototype;
					if prototype.is_null() {
						Ok(true)
					} else {
						if let Some(property) = prototype.get_property(name) {
							match property.value {
								PropertyValue::Accessor { set, .. } => Ok(!set.is_undefined()),
								PropertyValue::Value { .. } => Ok(property.is_writable())
							}
						} else {
							Ok(true)
						}
					}
				}
				_ => Ok(false)
			}
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.12.5
	pub fn put(&self, name: Name, value: Local<JsValue>, throw: bool, env: &JsEnv) -> JsResult<()> {
		if !try!(self.can_put(name, env)) {
			return if throw {
				Err(JsError::Type)
			} else {
				Ok(())
			};
		}
		
		if let Some(mut property) = try!(self.get_own_property(name, env)) {
			if let PropertyValue::Value { .. } = property.value {
				property.value = PropertyValue::Value {
					value: value
				};
				
				self.get_object().props.replace(name, &property);
				
				return Ok(())
			}
		}
		
		if let Some(property) = try!(self.get_property(name, env)) {
			if let PropertyValue::Accessor { .. } = property.value {
				unimplemented!();
			}
		}
		
		let property = Property::new_simple_value(value);
		
		if !self.get_object().props.replace(name, &property) {
			self.get_object().props.add(name, &property, env);
		}
		
		Ok(())
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-15.3.5.3
	pub fn has_instance(&self, val: Local<JsValue>, env: &JsEnv) -> JsResult<bool> {
		if self.ty() != JsType::Object || val.ty() != JsType::Object {
			Ok(false)
		} else {
			let obj = try!(self.get(name::PROTOTYPE, env));
			if obj.ty() != JsType::Object {
				Err(JsError::Type)
			} else {
				let obj = obj.get_object();
				let mut prototype = val.get_object().prototype;
				loop {
					if prototype.is_null() {
						return Ok(false);
					} else if prototype == obj {
						return Ok(true);
					}
					
					prototype = prototype.prototype;
				}
			}
		}
	}
}

impl Local<JsValue> {
	pub fn as_object(&self, env: &JsEnv) -> Local<JsObject> {
		Local::from_ptr(self.get_object(), &env.heap)
	}
}

#[derive(Copy, Clone, PartialEq, Debug)]
struct JsRawValue {
	data: u64
}

impl JsRawValue {
	fn clear(&mut self) {
		self.data = 0
	}
	
	fn new() -> JsRawValue {
		JsRawValue {
			data: 0
		}
	}
	
	fn new_number(value: f64) -> JsRawValue {
		let mut result = JsRawValue::new();
		result.set_number(value);
		result
	}
	
	fn new_bool(value: bool) -> JsRawValue {
		JsRawValue {
			data: if value { 1 } else { 0 }
		}
	}
	
	fn new_ptr<T>(value: Ptr<T>) -> JsRawValue {
		let mut result = JsRawValue::new();
		result.set_ptr(value);
		result
	}
	
	fn get_number(&self) -> f64 {
		unsafe { *transmute::<_, &f64>(&self.data) }
	}
	
	fn set_number(&mut self, value: f64) {
		unsafe { *transmute::<_, &mut f64>(&self.data) = value; }
	}
	
	fn get_bool(&self) -> bool {
		self.data != 0
	}
	
	fn set_bool(&mut self, value: bool) {
		self.data = if value { 1 } else { 0 }
	}
	
	fn get_ptr<T>(&self) -> Ptr<T> {
		Ptr::from_ptr(self.data as *const libc::c_void)
	}
	
	fn set_ptr<T>(&mut self, value: Ptr<T>) {
		self.data = value.as_ptr() as u64
	}
}
