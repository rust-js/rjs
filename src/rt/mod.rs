#![allow(dead_code)]

extern crate libc;

use gc::*;
use ir::IrContext;
use syntax::ast::{Name, FunctionRef};
use std::mem;
use std::fmt;
use self::hash::{Property, PropertyValue};
use ::{JsResult, JsError};
use syntax::token::name;

const INITIAL_OBJECT : usize = 20;

mod hash;
mod interpreter;
mod utf;
mod env;
mod runtime;
mod stack;

struct Types {
	object: GcTypeId,
	object_entry: GcTypeId,
	string: GcTypeId,
	char: GcTypeId,
	value: GcTypeId
}

const VALUE_VALUE_OFFSET : u32 = 1;

fn value_walker(ptr: *const libc::c_void, index: u32) -> GcTypeWalk {
	if index < VALUE_VALUE_OFFSET {
		GcTypeWalk::Skip
	} else if index == VALUE_VALUE_OFFSET {
		let value = unsafe { mem::transmute::<_, &JsValue>(ptr) };
		
		match value.ty {
			JsType::String | JsType::Object => GcTypeWalk::Pointer,
			_ => GcTypeWalk::End
		}
	} else {
		GcTypeWalk::End
	}
}

pub struct JsEnv {
	heap: GcHeap,
	types: Types,
	global: UnsafeRoot<JsObject>,
	object_prototype: UnsafeRoot<JsObject>,
	function_prototype: UnsafeRoot<JsObject>,
	string_prototype: UnsafeRoot<JsObject>,
	ir: IrContext,
	stack: stack::Stack
}

impl JsEnv {
	pub fn new() -> JsResult<JsEnv> {
		let mut heap = GcHeap::new(GcOpts::default());
		
		let types = Types { 
			object: heap.types().add(GcType::new(mem::size_of::<JsObject>(), GcTypeLayout::None)),
			object_entry: hash::build_entry_gc_type(&mut heap),
			string: heap.types().add(GcType::new(mem::size_of::<JsString>(), GcTypeLayout::None)),
			char: heap.types().add(GcType::new(mem::size_of::<u16>(), GcTypeLayout::None)),
			value: heap.types().add(GcType::new(mem::size_of::<JsValue>(), GcTypeLayout::Callback(Box::new(value_walker)))),
		};
		
		let global = heap.alloc_root::<JsObject>(types.object).into_unsafe();
		let object_prototype = heap.alloc_root::<JsObject>(types.object).into_unsafe();
		let function_prototype = heap.alloc_root::<JsObject>(types.object).into_unsafe();
		let string_prototype = heap.alloc_root::<JsObject>(types.object).into_unsafe();
		
		let mut env = JsEnv {
			heap: heap,
			types: types,
			global: global,
			object_prototype: object_prototype,
			function_prototype: function_prototype,
			string_prototype: string_prototype,
			ir: IrContext::new(),
			stack: stack::Stack::new()
		};
		
		*env.global = JsObject::new(&env);
		
		try!(env::setup(&mut env));
		
		Ok(env)
	}
	
	pub fn run(&mut self, file_name: &str) -> JsResult<Root<JsValue>> {
		let function_ref = try!(self.ir.parse_file(file_name));
		
		let mut ir = String::new();
		try!(self.ir.print_ir(&mut ir));
		println!("{}", ir);
		
		self.call(function_ref)
	}
	
	pub fn eval(&mut self, js: &str) -> JsResult<Root<JsValue>> {
		let function_ref = try!(self.ir.parse_string(js));
		
		let mut ir = String::new();
		try!(self.ir.print_ir(&mut ir));
		println!("{}", ir);
		
		self.call(function_ref)
	}
	
	fn call(&mut self, function_ref: FunctionRef) -> JsResult<Root<JsValue>> {
		println!("ENTER {}", if let Some(name) = self.ir.get_function_description(function_ref).name { self.ir.interner().get(name).to_string() } else { "(anonymous)".to_string() });
		
		let block = try!(self.ir.get_function_ir(function_ref));
		
		let mut result = self.alloc_root_value().into_unsafe();
		*result = try!(self.call_block(block, None, Vec::new()));
		
		println!("EXIT {}", if let Some(name) = self.ir.get_function_description(function_ref).name { self.ir.interner().get(name).to_string() } else { "(anonymous)".to_string() });
		
		Ok(Root::from_unsafe(&self.heap, result))
	}
	
	/// Returns a new GC handle to the global object.
	pub fn global(&self) -> Root<JsObject> {
		Root::from_unsafe(&self.heap, self.global.clone())
	}
	
	fn alloc_root_value(&self) -> Root<JsValue> {
		self.heap.alloc_root::<JsValue>(self.types.value)
	}
	
	fn alloc_local_value(&self) -> Local<JsValue> {
		self.heap.alloc_local::<JsValue>(self.types.value)
	}
	
	fn alloc_local_object(&self) -> Local<JsObject> {
		self.heap.alloc_local::<JsObject>(self.types.object)
	}
	
	fn alloc_local_string(&self) -> Local<JsString> {
		self.heap.alloc_local::<JsString>(self.types.string)
	}
	
	unsafe fn alloc_char_array(&self, size: usize) -> Array<u16> {
		self.heap.alloc_array::<u16>(self.types.char, size)
	}
	
	unsafe fn alloc_object_entry_array(&self, size: usize) -> Array<hash::Entry> {
		self.heap.alloc_array::<hash::Entry>(self.types.object_entry, size)
	}
	
	pub fn intern(&self, name: &str) -> Name {
		self.ir.interner().intern(name)
	}
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum JsType {
	None = 0,
	Undefined = 1,
	Null = 2,
	Number = 3,
	Boolean = 4,
	String = 5,
	Object = 6
}

impl JsType {
	fn is_ptr(&self) -> bool {
		match *self {
			JsType::Object | JsType::String => true,
			_ => false
		}
	}
}

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
	
	fn as_local(&self, env: &JsEnv) -> Local<JsValue> {
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
	
	fn new_none() -> JsValue {
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
		unsafe { *mem::transmute::<_, &f64>(&self.data) }
	}
	
	fn set_number(&mut self, value: f64) {
		unsafe { *mem::transmute::<_, &mut f64>(&self.data) = value; }
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

pub struct JsString {
	chars: Array<u16>
}

impl JsString {
	fn new_local(env: &JsEnv, size: usize) -> Local<JsString> {
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
	
	fn concat<'a>(env: &'a JsEnv, lhs: Local<JsString>, rhs: Local<JsString>) -> Local<JsString> {
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
	
	fn to_string(&self) -> String {
		String::from_utf16(&*self.chars).ok().unwrap()
	}
}

pub struct JsObject {
	class: Option<Name>,
	value: Option<JsValue>,
	function: Option<JsFunction>,
	prototype: Ptr<JsObject>,
	props: hash::Hash
}

impl JsObject {
	fn new(env: &JsEnv) -> JsObject {
		JsObject {
			class: None,
			value: None,
			function: None,
			prototype: Ptr::null(),
			props: hash::Hash::new(&env, INITIAL_OBJECT)
		}
	}
	
	fn new_local(env: &JsEnv) -> Local<JsObject> {
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
	fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_object(self.as_ptr()).as_local(env)
	}
}

impl Local<JsString> {
	fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_string(self.as_ptr()).as_local(env)
	}
}

#[derive(Copy, Clone, PartialEq)]
pub enum JsFnMode {
	Call,
	New
}

pub struct JsArgs {
	function: Local<JsValue>,
	this: Local<JsValue>,
	args: Vec<Local<JsValue>>,
	mode: JsFnMode
}

pub type JsFn = Fn(&mut JsEnv, JsArgs) -> JsResult<Local<JsValue>>;

pub enum JsFunction {
	Ir(FunctionRef),
	Native(Option<Name>, u32, Box<JsFn>)
}
