extern crate libc;

use rt::{JsEnv, JsString, JsType, JsObject, JsItem, JsDescriptor, JsScope, JsPreferredType};
use rt::{JsNull, JsUndefined, JsNumber, JsBoolean, JsIterator, GC_VALUE};
use ::{JsResult, JsError};
use syntax::Name;
use syntax::lexer::Lexer;
use syntax::reader::StringReader;
use syntax::token::Token;
use syntax::token::name;
use gc::{Local, Ptr};
use std::fmt;
use std::mem::transmute;
use std::f64;

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
			JsType::Object => try!(write!(fmt, "object")),
			_ => panic!("unexpected type")
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
	
	pub fn new_iterator(value: Ptr<JsIterator>) -> JsValue {
		JsValue {
			ty: JsType::Iterator,
			value: JsRawValue::new_ptr(value)
		}
	}
	
	pub fn new_scope(value: Ptr<JsScope>) -> JsValue {
		JsValue {
			ty: JsType::Scope,
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

	pub fn as_string(&self, env: &JsEnv) -> Local<JsString> {
		Local::from_ptr(self.get_string(), &env.heap)
	}
	
	pub fn get_object(&self) -> Ptr<JsObject> {
		assert_eq!(self.ty, JsType::Object);
		
		self.value.get_ptr()
	}
	
	pub fn set_object(&mut self, value: Ptr<JsObject>) {
		*self = JsValue::new_object(value);
	}
	
	pub fn as_object(&self, env: &JsEnv) -> Local<JsObject> {
		Local::from_ptr(self.get_object(), &env.heap)
	}
	
	pub fn get_iterator(&self) -> Ptr<JsIterator> {
		assert_eq!(self.ty, JsType::Iterator);
		
		self.value.get_ptr()
	}
	
	pub fn set_iterator(&mut self, value: Ptr<JsIterator>) {
		*self = JsValue::new_iterator(value);
	}
	
	pub fn as_iterator(&self, env: &JsEnv) -> Local<JsIterator> {
		Local::from_ptr(self.get_iterator(), &env.heap)
	}
	
	pub fn get_scope(&self) -> Ptr<JsScope> {
		assert_eq!(self.ty, JsType::Scope);
		
		self.value.get_ptr()
	}
	
	pub fn set_scope(&mut self, value: Ptr<JsScope>) {
		*self = JsValue::new_scope(value);
	}
	
	pub fn as_scope(&self, env: &JsEnv) -> Local<JsScope> {
		self.get_scope().as_local(&env)
	}
}

macro_rules! delegate {
	( $target:expr, $env:expr, $method:ident ( $( $arg:expr ),* ) ) => {
		match $target.ty() {
			JsType::Undefined => JsUndefined.$method( $( $arg ),* ),
			JsType::Null => JsNull.$method( $( $arg ),* ),
			JsType::Number => JsNumber::new($target.get_number()).$method( $( $arg ),* ),
			JsType::Boolean => JsBoolean::new($target.get_bool()).$method( $( $arg ),* ),
			JsType::Object => $target.as_object($env).$method( $( $arg ),* ),
			JsType::String => $target.as_string($env).$method( $( $arg ),* ),
			_ => panic!("unexpected type")
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
	
	fn delete(&mut self, env: &mut JsEnv, property: Name, throw: bool) -> JsResult<bool> {
		delegate!(self, env, delete(env, property, throw))
	}
	
	fn default_value(&self, env: &mut JsEnv, hint: JsPreferredType) -> JsResult<Local<JsValue>> {
		delegate!(self, env, default_value(env, hint))
	}
	
	fn define_own_property(&mut self, env: &mut JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
		delegate!(self, env, define_own_property(env, property, descriptor, throw))
	}
	
	fn is_callable(&self, env: &JsEnv) -> bool  {
		delegate!(self, env, is_callable(env))
	}
	
	fn call(&self, env: &mut JsEnv, this: Local<JsValue>, args: Vec<Local<JsValue>>, strict: bool) -> JsResult<Local<JsValue>>  {
		delegate!(self, env, call(env, this, args, strict))
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
	
	fn set_scope(&mut self, env: &JsEnv, scope: Option<Local<JsScope>>) {
		delegate!(self, env, set_scope(env, scope))
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
	// 9.1 ToPrimitive
	pub fn to_primitive(&self, env: &mut JsEnv, hint: JsPreferredType) -> JsResult<Local<JsValue>> {
		match self.ty() {
			JsType::Object => self.default_value(env, hint),
			_ => Ok(*self)
		}
	}
	
	// 9.2 ToBoolean
	pub fn to_boolean(&self) -> bool {
		match self.ty() {
			JsType::Undefined | JsType::Null => false,
			JsType::Boolean => self.get_bool(),
			JsType::Number => {
				let value = self.get_number();
				!(value == 0f64 || value.is_nan())
			}
			JsType::String => self.get_string().chars.len() > 0,
			JsType::Object => true,
			_ => panic!("unexpected type")
		}
	}
	
	// 9.3 ToNumber
	pub fn to_number(&self, env: &mut JsEnv) -> JsResult<f64> {
		let result = match self.ty() {
			JsType::Undefined => f64::NAN,
			JsType::Null => 0f64,
			JsType::Boolean => if self.get_bool() { 1f64 } else { 0f64 },
			JsType::Number => self.get_number(),
			JsType::String => {
				let mut reader = StringReader::new("", &self.get_string().to_string());
				if let Ok(mut lexer) = Lexer::new(&mut reader, env.ir.interner()) {
					if let Some(token) = try!(lexer.peek(0)) {
						if try!(lexer.peek(1)).is_none() {
							if let Token::Literal(lit) = token.token {
								if let Some(value) = lit.to_number() {
									return Ok(value);
								}
							}
						}
					}
				}
				
				f64::NAN
			},
			JsType::Object => {
				let value = try!(self.to_primitive(env, JsPreferredType::Number));
				try!(value.to_number(env))
			}
			_ => panic!("unexpected type")
		};
		
		Ok(result)
	}
	
	// 9.4 ToInteger
	pub fn to_integer(&self, env: &mut JsEnv) -> JsResult<f64> {
		let number = try!(self.to_number(env));
		let result = if number.is_nan() {
			0f64
		} else if number == 0f64 || number.is_infinite() {
			number
		} else {
			number.round()
		};
		
		Ok(result)
	}
	
	// 9.5 ToInt32: (Signed 32 Bit Integer)
	// TODO: This does not adhere to the full specs.
	pub fn to_int32(&self, env: &mut JsEnv) -> JsResult<i32> {
		let number = try!(self.to_number(env));
		let result = if number.is_nan() || number == 0f64 || number.is_infinite() {
			0
		} else {
			number as i32
		};
		
		Ok(result)
	}
	
	// 9.6 ToUint32: (Unsigned 32 Bit Integer)
	// TODO: This does not adhere to the full specs.
	pub fn to_uint32(&self, env: &mut JsEnv) -> JsResult<u32> {
		let number = try!(self.to_number(env));
		let result = if number.is_nan() || number == 0f64 || number.is_infinite() {
			0
		} else {
			number as u32
		};
		
		Ok(result)
	}
	
	// 9.6 ToUint32: (Unsigned 32 Bit Integer)
	// TODO: This does not adhere to the full specs.
	pub fn to_uint32_exact(&self, env: &mut JsEnv) -> JsResult<Option<u32>> {
		let number = try!(self.to_number(env));
		let result = if number.is_nan() || number == 0f64 || number.is_infinite() {
			0
		} else {
			number as u32
		};
		
		if result as f64 == number {
			Ok(Some(result))
		} else {
			Ok(None)
		}
	}
	
	// 9.7 ToUint16: (Unsigned 16 Bit Integer)
	// TODO: This does not adhere to the full specs.
	pub fn to_uint16(&self, env: &mut JsEnv) -> JsResult<u16> {
		let number = try!(self.to_number(env));
		let result = if number.is_nan() || number == 0f64 || number.is_infinite() {
			0
		} else {
			number as u16
		};
		
		Ok(result)
	}
	
	// 9.8 ToString
	pub fn to_string(&self, env: &mut JsEnv) -> JsResult<Local<JsString>> {
		let result = match self.ty {
			JsType::Undefined => JsString::from_str(env, "undefined"),
			JsType::Null => JsString::from_str(env, "null"),
			JsType::Boolean => JsString::from_str(env, if self.get_bool() { "true" } else { "false" }),
			JsType::Number => {
				let number = self.get_number();
				
				if number.is_nan() {
					JsString::from_str(env, "NaN")
				} else if number == 0f64 {
					JsString::from_str(env, "0")
				} else if number.is_infinite() {
					JsString::from_str(env, "Infinity")
				} else {
					// TODO: This is very wrong. See 9.8.1 ToString Applied to the Number Type
					// for the full specifications. A C# implementation can be found at
					// http://jurassic.codeplex.com/SourceControl/latest#Jurassic/Core/NumberFormatter.cs.
					JsString::from_str(env, &number.to_string())
				}
			}
			JsType::String => Local::from_ptr(self.get_string(), &mut env.heap),
			JsType::Object => {
				let result = try!(self.to_primitive(env, JsPreferredType::String));
				try!(result.to_string(env))
			}
			_ => panic!("unexpected type")
		};
		
		Ok(result)
	}
	
	// 9.9 ToObject
	pub fn to_object(&self, env: &mut JsEnv) -> JsResult<Local<JsValue>> {
		match self.ty {
			JsType::Null | JsType::Undefined => Err(JsError::new_type(env, ::errors::TYPE_INVALID)),
			JsType::String => {
				let constructor = try!(env.global().as_local(env).get(env, name::STRING_CLASS));
				let value = self.as_local(env);
				let object = try!(constructor.construct(env, vec![value]));
				Ok(object)
			}
			JsType::Boolean | JsType::Number => {
				let class = match self.ty {
					JsType::Boolean => name::BOOLEAN_CLASS,
					JsType::Number => name::NUMBER_CLASS,
					JsType::String => name::STRING_CLASS,
					_ => unreachable!()
				};
				
				let constructor = try!(env.global().as_local(env).get(env, class));
				let object = try!(constructor.construct(env, Vec::new()));
				object.as_object(env).set_value(Some(*self));
				Ok(object)
			}
			JsType::Object => Ok(*self),
			_ => panic!("unexpected type")
		}
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
		unsafe { transmute(self.data) }
	}
	
	fn set_number(&mut self, value: f64) {
		unsafe { self.data = transmute(value); }
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
