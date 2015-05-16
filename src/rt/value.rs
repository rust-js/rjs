extern crate libc;

use super::{JsEnv, JsString, JsType, JsObject, JsItem, JsDescriptor, JsScope, JsDefaultValueHint};
use super::{JsNull, JsUndefined, JsNumber, JsBoolean, GC_VALUE};
use ::JsResult;
use syntax::Name;
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
		let mut result = env.heap.alloc_local(GC_VALUE);
		*result = *self;
		result
	}
	
	fn new(ty: JsType, value: JsRawValue) -> JsValue {
		JsValue {
			ty: ty,
			value: value
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
}

macro_rules! delegate {
	( $target:expr, $env:expr, $method:ident ( $( $arg:expr ),* ) ) => {
		match $target.ty() {
			JsType::Undefined => JsUndefined::new().$method( $( $arg ),* ),
			JsType::Null => JsNull::new().$method( $( $arg ),* ),
			JsType::Number => JsNumber::new($target.get_number()).$method( $( $arg ),* ),
			JsType::Boolean => JsBoolean::new($target.get_bool()).$method( $( $arg ),* ),
			JsType::Object => $target.as_object($env).$method( $( $arg ),* ),
			JsType::String => $target.as_string($env).$method( $( $arg ),* )
		}
	}
}

impl JsItem for Local<JsValue> {
	fn as_value(&self, _: &JsEnv) -> Local<JsValue> {
		*self
	}
	
	fn get_own_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
		delegate!(self, env, get_own_property(env, property))
	}
	
	fn get_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
		delegate!(self, env, get_property(env, property))
	}
	
	fn get(&self, env: &mut JsEnv, property: Name) -> JsResult<Local<JsValue>> {
		delegate!(self, env, get(env, property))
	}
	
	fn can_put(&self, env: &JsEnv, property: Name) -> bool {
		delegate!(self, env, can_put(env, property))
	}
	
	fn put(&mut self, env: &mut JsEnv, property: Name, value: Local<JsValue>, throw: bool) -> JsResult<()> {
		delegate!(self, env, put(env, property, value, throw))
	}
	
	fn has_property(&self, env: &JsEnv, property: Name) -> bool {
		delegate!(self, env, has_property(env, property))
	}
	
	fn delete(&mut self, env: &JsEnv, property: Name, throw: bool) -> JsResult<bool> {
		delegate!(self, env, delete(env, property, throw))
	}
	
	fn default_value(&self, env: &mut JsEnv, hint: JsDefaultValueHint) -> JsResult<Local<JsValue>> {
		delegate!(self, env, default_value(env, hint))
	}
	
	fn define_own_property(&mut self, env: &JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
		delegate!(self, env, define_own_property(env, property, descriptor, throw))
	}
	
	fn is_callable(&self, env: &JsEnv) -> bool  {
		delegate!(self, env, is_callable(env))
	}
	
	fn call(&mut self, env: &mut JsEnv, this: Local<JsValue>, args: Vec<Local<JsValue>>) -> JsResult<Local<JsValue>>  {
		delegate!(self, env, call(env, this, args))
	}
	
	fn can_construct(&self, env: &JsEnv) -> bool  {
		delegate!(self, env, can_construct(env))
	}
	
	fn construct(&self, env: &mut JsEnv, args: Vec<Local<JsValue>>) -> JsResult<Local<JsValue>>  {
		delegate!(self, env, construct(env, args))
	}
	
	fn has_prototype(&self, env: &JsEnv) -> bool  {
		delegate!(self, env, has_prototype(env))
	}
	
	fn prototype(&self, env: &JsEnv) -> Option<Local<JsValue>>  {
		delegate!(self, env, prototype(env))
	}
	
	fn set_prototype(&mut self, env: &JsEnv, prototype: Option<Local<JsValue>>)  {
		delegate!(self, env, set_prototype(env, prototype))
	}
	
	fn has_class(&self, env: &JsEnv) -> bool  {
		delegate!(self, env, has_class(env))
	}
	
	fn class(&self, env: &JsEnv) -> Option<Name>  {
		delegate!(self, env, class(env))
	}
	
	fn set_class(&mut self, env: &JsEnv, class: Option<Name>)  {
		delegate!(self, env, set_class(env, class))
	}
	
	fn is_extensible(&self, env: &JsEnv) -> bool  {
		delegate!(self, env, is_extensible(env))
	}
	
	fn has_instance(&self, env: &mut JsEnv, object: Local<JsValue>) -> JsResult<bool>  {
		delegate!(self, env, has_instance(env, object))
	}
	
	fn scope(&self, env: &JsEnv) -> Option<Local<JsScope>>  {
		delegate!(self, env, scope(env))
	}
	
	fn formal_parameters(&self, env: &JsEnv) -> Option<Vec<Name>>  {
		delegate!(self, env, formal_parameters(env))
	}
	
	fn code(&self, env: &JsEnv) -> Option<String>  {
		delegate!(self, env, code(env))
	}
	
	fn target_function(&self, env: &JsEnv) -> Option<Local<JsValue>>  {
		delegate!(self, env, target_function(env))
	}
	
	fn bound_this(&self, env: &JsEnv) -> Option<Local<JsValue>>  {
		delegate!(self, env, bound_this(env))
	}
	
	fn bound_arguments(&self, env: &JsEnv) -> Option<Local<JsValue>>  {
		delegate!(self, env, bound_arguments(env))
	}
}

impl Local<JsValue> {
	pub fn as_object(&self, env: &JsEnv) -> Local<JsObject> {
		Local::from_ptr(self.get_object(), &env.heap)
	}

	pub fn as_string(&self, env: &JsEnv) -> Local<JsString> {
		Local::from_ptr(self.get_string(), &env.heap)
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
		unsafe { self.data = *transmute::<_, &u64>(&value); }
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
