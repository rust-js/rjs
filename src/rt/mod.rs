#![allow(dead_code)]

extern crate libc;

use gc::*;
use ir::IrContext;
use syntax::Name;
use syntax::token::name;
use syntax::ast::FunctionRef;
use ::{JsResult, JsError};
use std::i32;
pub use self::value::JsValue;
pub use self::object::{JsObject, JsStoreType};
pub use self::string::JsString;
pub use self::null::JsNull;
pub use self::undefined::JsUndefined;
pub use self::number::JsNumber;
pub use self::boolean::JsBoolean;
pub use self::iterator::JsIterator;
pub use self::scope::JsScope;

mod interpreter;
mod utf;
mod env;
mod runtime;
mod stack;
mod value;
mod object;
mod string;
mod number;
mod boolean;
mod undefined;
mod null;
mod iterator;
mod scope;

const GC_OBJECT : u32 = 1;
const GC_ENTRY : u32 = 2;
const GC_STRING : u32 = 3;
const GC_U16 : u32 = 2;
const GC_U32 : u32 = 4;
const GC_VALUE : u32 = 5;
const GC_HASH_STORE : u32 = 6;
const GC_ARRAY_STORE : u32 = 7;
const GC_ITERATOR : u32 = 8;
const GC_SCOPE : u32 = 8;

impl Root<JsObject> {
	pub fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_object(self.as_ptr()).as_local(&env.heap)
	}
}

pub struct JsEnv {
	heap: GcHeap,
	global: Root<JsObject>,
	global_scope: Root<JsScope>,
	object_prototype: Root<JsObject>,
	array_prototype: Root<JsObject>,
	function_prototype: Root<JsObject>,
	string_prototype: Root<JsObject>,
	number_prototype: Root<JsObject>,
	boolean_prototype: Root<JsObject>,
	date_prototype: Root<JsObject>,
	regexp_prototype: Root<JsObject>,
	ir: IrContext,
	stack: stack::Stack
}

impl JsEnv {
	pub fn new() -> JsResult<JsEnv> {
		let heap = GcHeap::new(Box::new(Walker), GcOpts::default());
		
		let global = heap.alloc_root::<JsObject>(GC_OBJECT);
		let global_scope = heap.alloc_root::<JsScope>(GC_SCOPE);
		let object_prototype = heap.alloc_root::<JsObject>(GC_OBJECT);
		let array_prototype = heap.alloc_root::<JsObject>(GC_OBJECT);
		let function_prototype = heap.alloc_root::<JsObject>(GC_OBJECT);
		let string_prototype = heap.alloc_root::<JsObject>(GC_OBJECT);
		let number_prototype = heap.alloc_root::<JsObject>(GC_OBJECT);
		let boolean_prototype = heap.alloc_root::<JsObject>(GC_OBJECT);
		let date_prototype = heap.alloc_root::<JsObject>(GC_OBJECT);
		let regexp_prototype = heap.alloc_root::<JsObject>(GC_OBJECT);
		
		let mut env = JsEnv {
			heap: heap,
			global: global,
			global_scope: global_scope,
			object_prototype: object_prototype,
			array_prototype: array_prototype,
			function_prototype: function_prototype,
			string_prototype: string_prototype,
			number_prototype: number_prototype,
			boolean_prototype: boolean_prototype,
			date_prototype: date_prototype,
			regexp_prototype: regexp_prototype,
			ir: IrContext::new(),
			stack: stack::Stack::new()
		};
		
		try!(env::setup(&mut env));
		
		Ok(env)
	}
	
	pub fn run(&mut self, file_name: &str) -> JsResult<Root<JsValue>> {
		self.run_strict(file_name, false)
	}
	
	pub fn run_strict(&mut self, file_name: &str, strict: bool) -> JsResult<Root<JsValue>> {
		let function_ref = try!(self.ir.parse_file(file_name, strict));
		
		let mut ir = String::new();
		try!(self.ir.print_ir(&mut ir));
		debugln!("{}", ir);
		
		let _scope = self.heap.new_local_scope();
		
		let global_scope = self.global_scope.as_local(&self.heap);
		
		self.call(function_ref, global_scope)
	}
	
	pub fn eval(&mut self, js: &str) -> JsResult<Root<JsValue>> {
		let _scope = self.heap.new_local_scope();
		
		let global_scope = self.global_scope.as_local(&self.heap);
		
		self.eval_scoped(js, false, global_scope, false)
	}
	
	fn eval_scoped(&mut self, js: &str, strict: bool, scope: Local<JsScope>, direct_eval: bool) -> JsResult<Root<JsValue>> {
		let function_ref = try!(self.ir.parse_string(js, strict, direct_eval));
		
		let mut ir = String::new();
		try!(self.ir.print_ir(&mut ir));
		debugln!("{}", ir);
		
		self.call(function_ref, scope)
	}
	
	fn call(&mut self, function_ref: FunctionRef, scope: Local<JsScope>) -> JsResult<Root<JsValue>> {
		let function = self.ir.get_function(function_ref);
		let name = if let Some(name) = function.name {
			self.ir.interner().get(name).to_string()
		} else {
			"(anonymous)".to_string()
		};
		let location = format!("{}[{}:{}] {}", self.ir.interner().get(function.span.file), function.span.start_line, function.span.start_col, name);
		
		debugln!("ENTER {}", location);
		
		let block = try!(self.ir.get_function_ir(function_ref));
		
		// TODO: Validate. Function is just coerced to undefined because
		// we don't have it here.
		let args = JsArgs {
			function: JsValue::new_undefined().as_local(&self.heap),
			this: self.global.as_value(self),
			args: Vec::new(),
			strict: function.strict,
			mode: JsFnMode::Call
		};
		
		let mut result = self.heap.alloc_root::<JsValue>(GC_VALUE);
		*result = try!(self.call_block(block, args, &function, scope));
		
		debugln!("EXIT {}", location);
		
		Ok(result)
	}
	
	pub fn heap(&self) -> &GcHeap {
		&self.heap
	}
	
	/// Returns a new GC handle to the global object.
	pub fn global(&self) -> &Root<JsObject> {
		&self.global
	}
	
	pub fn intern(&self, name: &str) -> Name {
		self.ir.interner().intern(name)
	}
	
	pub fn intern_value(&mut self, value: Local<JsValue>) -> JsResult<Name> {
		if value.ty() == JsType::Number {
			let index = value.unwrap_number();
			if index >= 0f64 && index <= i32::MAX as f64 && index as i32 as f64 == index {
				return Ok(Name::from_index(index as usize));
			}
		}
		
		let index = try!(value.to_string(self));
		Ok(self.intern(&index.to_string()))
	}
	
	fn new_native_function<'a>(&mut self, name: Option<Name>, args: u32, function: &JsFn, prototype: Local<JsObject>) -> Local<JsValue> {
		let mut result = JsObject::new_function(self, JsFunction::Native(name, args, function as *const JsFn, true), prototype).as_value(self);
		
		let mut proto = self.new_object();
		let value = proto.as_value(self);
		result.define_own_property(self, name::PROTOTYPE, JsDescriptor::new_value(value, true, false, true), false).ok();
		proto.define_own_property(self, name::CONSTRUCTOR, JsDescriptor::new_value(result, true, false, true), false).ok();
		
		result
	}
}

struct Walker;

impl GcWalker for Walker {
	fn walk(&self, _ty: u32, _ptr: ptr_t, _index: u32) -> GcWalk {
		panic!();
	}
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum JsPreferredType {
	None,
	String,
	Number
}

#[allow(unused_variables)]
pub trait JsItem {
	fn as_value(&self, env: &JsEnv) -> Local<JsValue>;
	
	fn get_own_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
		None
	}
	
	// 8.12.2 [[GetProperty]] (P)
	fn get_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
		if let Some(descriptor) = self.get_own_property(env, property) {
			Some(descriptor)
		} else {
			if let Some(proto) = self.prototype(env) {
				proto.get_property(env, property)
			} else {
				None
			}
		}
	}
	
	// 8.12.3 [[Get]] (P)
	fn get(&self, env: &mut JsEnv, property: Name) -> JsResult<Local<JsValue>> {
		if let Some(desc) = self.get_property(env, property) {
			return if desc.is_data() {
				Ok(desc.value(env))
			} else {
				let get = desc.get(env);
				if get.is_undefined() {
					Ok(JsValue::new_undefined().as_local(&env.heap))
				} else {
					let this = self.as_value(env);
					get.call(env, this, Vec::new(), false)
				}
			}
		}

		Ok(JsValue::new_undefined().as_local(&env.heap))
	}
	
	// 8.12.4 [[CanPut]] (P)
	fn can_put(&self, env: &JsEnv, property: Name) -> bool {
		if let Some(desc) = self.get_own_property(env, property) {
			return if desc.is_accessor() {
				if let Some(set) = desc.set {
					!set.is_undefined()
				} else {
					false
				}
			} else {
				desc.is_writable()
			}
		}
		
		if let Some(proto) = self.prototype(env) {
			if let Some(inherited) = proto.get_property(env, property) {
				return if inherited.is_accessor() {
					if let Some(set) = inherited.set {
						!set.is_undefined()
					} else {
						false
					}
				} else {
					if !self.is_extensible(env) {
						false
					} else {
						inherited.is_writable()
					}
				}
			}
		}

		self.is_extensible(env)
	}
	
	// 8.12.5 [[Put]] ( P, V, Throw )
	fn put(&mut self, env: &mut JsEnv, property: Name, value: Local<JsValue>, throw: bool) -> JsResult<()> {
		// BUG #18: This is wrong but I can't figure out how to solve this. The specs state that
		// [[CanPut]] will reject the mutation of the property is not writable. However, if
		// Array.prototype has a not writable 0, the write still succeeds.
		
		if self.class(env) != Some(name::ARRAY_CLASS) || !property.is_index() {
			if !self.can_put(env, property) {
				return if throw {
					Err(JsError::new_type(env, ::errors::TYPE_CANNOT_PUT))
				} else {
					Ok(())
				};
			}
		}
		
		if let Some(own_desc) = self.get_own_property(env, property) {
			if own_desc.is_data() {
				let value_desc = JsDescriptor {
					value: Some(value),
					..JsDescriptor::default()
				};
				try!(self.define_own_property(env, property, value_desc, throw));
				
				return Ok(());
			}
		}
		
		if let Some(desc) = self.get_property(env, property) {
			if desc.is_accessor() {
				let this = self.as_value(env);
				try!(desc.set(env).call(env, this, vec![value], false));
				return Ok(());
			}
		}
		
		try!(self.define_own_property(env, property, JsDescriptor::new_simple_value(value), throw));
		
		Ok(())
	}
	
	// 8.12.6 [[HasProperty]] (P)
	fn has_property(&self, env: &JsEnv, property: Name) -> bool {
		self.get_property(env, property).is_some()
	}
	
	// 8.12.7 [[Delete]] (P, Throw)
	fn delete(&mut self, env: &mut JsEnv, property: Name, throw: bool) -> JsResult<bool> {
		// If get_own_property returns None, delete returns true.
		Ok(true)
	}
	
	// 8.12.8 [[DefaultValue]] (hint)
	fn default_value(&self, env: &mut JsEnv, hint: JsPreferredType) -> JsResult<Local<JsValue>> {
		let hint = if hint == JsPreferredType::None {
			let date_class = try!(env.global.as_value(env).get(env, name::DATE_CLASS));
			
			let object = self.as_value(env);
			if try!(date_class.has_instance(env, object)) {
				JsPreferredType::String
			} else {
				JsPreferredType::Number
			}
		} else {
			hint
		};
		
		fn try_call(env: &mut JsEnv, this: Local<JsValue>, method: Local<JsValue>) -> JsResult<Option<Local<JsValue>>> {
			if method.is_callable(env) {
				let this = this.as_value(env);
				let val = try!(method.call(env, this, Vec::new(), false));
				if val.ty().is_primitive() {
					return Ok(Some(val));
				}
			}
			
			Ok(None)
		}
		
		let this = self.as_value(env);
		
		if hint == JsPreferredType::String {
			let to_string = try!(this.get(env, name::TO_STRING));
			if let Some(str) = try!(try_call(env, this, to_string)) {
				return Ok(str);
			}
			
			let value_of = try!(this.get(env, name::VALUE_OF));
			if let Some(val) = try!(try_call(env, this, value_of)) {
				return Ok(val);
			}
			
			Err(JsError::new_type(env, ::errors::TYPE_INVALID))
		} else {
			let value_of = try!(this.get(env, name::VALUE_OF));
			if let Some(val) = try!(try_call(env, this, value_of)) {
				return Ok(val);
			}
			
			let to_string = try!(this.get(env, name::TO_STRING));
			if let Some(str) = try!(try_call(env, this, to_string)) {
				return Ok(str);
			}
			
			Err(JsError::new_type(env, ::errors::TYPE_INVALID))
		}
	}
	
	// 8.12.9 [[DefineOwnProperty]] (P, Desc, Throw)
	fn define_own_property(&mut self, env: &mut JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
		// If get_own_property returns None and self is not extensible, the below happens.
		if throw { Err(JsError::new_type(env, ::errors::TYPE_CANNOT_PUT)) } else { Ok(false) }
	}
	
	fn is_callable(&self, env: &JsEnv) -> bool {
		false
	}
	
	fn call(&self, env: &mut JsEnv, this: Local<JsValue>, args: Vec<Local<JsValue>>, strict: bool) -> JsResult<Local<JsValue>> {
		let args = JsArgs {
			function: self.as_value(env),
			this: this,
			args: args,
			strict: strict,
			mode: JsFnMode::Call
		};
		
		env.call_function(args)
	}
	
	fn can_construct(&self, env: &JsEnv) -> bool {
		false
	}
	
	// 13.2.2 [[Construct]]
	fn construct(&self, env: &mut JsEnv, args: Vec<Local<JsValue>>) -> JsResult<Local<JsValue>> {
		let mut obj = JsObject::new_local(env, JsStoreType::Hash);
		obj.set_class(env, Some(name::OBJECT_CLASS));
		let proto = try!(self.get(env, name::PROTOTYPE));
		if proto.ty() == JsType::Object {
			obj.set_prototype(env, Some(proto));
		} else {
			let proto = env.object_prototype.as_local(&env.heap).as_value(env);
			obj.set_prototype(env, Some(proto));
		}
		
		let obj = obj.as_value(env);

		let args = JsArgs {
			function: self.as_value(env),
			this: obj,
			args: args,
			strict: false,
			mode: JsFnMode::New
		};
		
		let result = try!(env.call_function(args));
		
		Ok(if result.ty() == JsType::Object {
			result
		} else {
			obj
		})
	}
	
	fn has_prototype(&self, env: &JsEnv) -> bool {
		false
	}
	
	fn prototype(&self, env: &JsEnv) -> Option<Local<JsValue>> {
		panic!("prototype not supported on {:?}", self.as_value(env).ty());
	}
	
	fn set_prototype(&mut self, env: &JsEnv, prototype: Option<Local<JsValue>>) {
		panic!("prototype not supported on {:?}", self.as_value(env).ty());
	}
	
	fn has_class(&self, env: &JsEnv) -> bool {
		false
	}
	
	fn class(&self, env: &JsEnv) -> Option<Name> {
		None
	}
	
	fn set_class(&mut self, env: &JsEnv, class: Option<Name>) {
		panic!("class not supported");
	}
	
	fn is_extensible(&self, env: &JsEnv) -> bool {
		true
	}
	
	fn has_instance(&self, env: &mut JsEnv, object: Local<JsValue>) -> JsResult<bool> {
		Err(JsError::new_type(env, ::errors::TYPE_CANNOT_HAS_INSTANCE))
	}
	
	fn scope(&self, env: &JsEnv) -> Option<Local<JsScope>> {
		panic!("scope not supported");
	}
	
	fn set_scope(&mut self, env: &JsEnv, scope: Option<Local<JsScope>>) {
		panic!("scope not supported");
	}
	
	fn formal_parameters(&self, env: &JsEnv) -> Option<Vec<Name>> {
		None
	}
	
	fn code(&self, env: &JsEnv) -> Option<String> {
		None
	}
	
	fn target_function(&self, env: &JsEnv) -> Option<Local<JsValue>> {
		None
	}
	
	fn bound_this(&self, env: &JsEnv) -> Option<Local<JsValue>> {
		None
	}
	
	fn bound_arguments(&self, env: &JsEnv) -> Option<Local<JsValue>> {
		None
	}
	
	// fn can_match(&self) -> bool;
	
	// fn match_(&self, env: &JsEnv, pattern: String, index: u32) -> JsMatchResult;
	
	// fn parameter_map(&self) -> JsParameterMap;
}

#[derive(Copy, Clone)]
pub struct JsDescriptor {
	pub value: Option<Local<JsValue>>,
	pub get: Option<Local<JsValue>>,
	pub set: Option<Local<JsValue>>,
	pub writable: Option<bool>,
	pub enumerable: Option<bool>,
	pub configurable: Option<bool>
}

impl JsDescriptor {
	pub fn default() -> JsDescriptor {
		JsDescriptor {
			value: None,
			get: None,
			set: None,
			writable: None,
			enumerable: None,
			configurable: None
		}
	}
	
	pub fn new_value(value: Local<JsValue>, writable: bool, enumerable: bool, configurable: bool) -> JsDescriptor {
		JsDescriptor {
			value: Some(value),
			get: None,
			set: None,
			writable: Some(writable),
			enumerable: Some(enumerable),
			configurable: Some(configurable)
		}
	}
	
	pub fn new_simple_value(value: Local<JsValue>) -> JsDescriptor {
		Self::new_value(value, true, true, true)
	}
	
	pub fn new_accessor(get: Local<JsValue>, set: Local<JsValue>, enumerable: bool, configurable: bool) -> JsDescriptor {
		JsDescriptor {
			value: None,
			get: Some(get),
			set: Some(set),
			writable: None,
			enumerable: Some(enumerable),
			configurable: Some(configurable)
		}
	}
	
	pub fn new_simple_accessor(get: Local<JsValue>, set: Local<JsValue>) -> JsDescriptor {
		Self::new_accessor(get, set, true, true)
	}
	
	pub fn is_same(&self, env: &JsEnv, other: &JsDescriptor) -> bool {
		fn is_same(env: &JsEnv, x: &Option<Local<JsValue>>, y: &Option<Local<JsValue>>) -> bool{
			(x.is_none() && y.is_none()) || (x.is_some() && y.is_some() && env.same_value(x.unwrap(), y.unwrap()))
		}
		
		is_same(env, &self.value, &other.value) &&
			is_same(env, &self.get, &other.get) &&
			is_same(env, &self.set, &other.set) &&
			self.writable == other.writable &&
			self.enumerable == other.enumerable &&
			self.configurable == other.configurable
	}
	
	// 8.10.1 IsAccessorDescriptor ( Desc )
	pub fn is_accessor(&self) -> bool {
		self.get.is_some() || self.set.is_some()
	}
	
	// 8.10.2 IsDataDescriptor ( Desc )
	pub fn is_data(&self) -> bool {
		self.writable.is_some() || self.value.is_some()
	}
	
	// 8.10.3 IsGenericDescriptor ( Desc )
	pub fn is_generic(&self) -> bool {
		!(self.is_accessor() || self.is_data())
	}
	
	pub fn value(&self, env: &JsEnv) -> Local<JsValue> {
		self.value.unwrap_or_else(|| JsValue::new_undefined().as_local(&env.heap))
	}
	
	pub fn get(&self, env: &JsEnv) -> Local<JsValue> {
		self.get.unwrap_or_else(|| JsValue::new_undefined().as_local(&env.heap))
	}
	
	pub fn set(&self, env: &JsEnv) -> Local<JsValue> {
		self.set.unwrap_or_else(|| JsValue::new_undefined().as_local(&env.heap))
	}
	
	pub fn is_writable(&self) -> bool {
		self.writable.unwrap_or(false)
	}
	
	pub fn is_enumerable(&self) -> bool {
		self.enumerable.unwrap_or(false)
	}
	
	pub fn is_configurable(&self) -> bool {
		self.configurable.unwrap_or(false)
	}
	
	pub fn is_empty(&self) -> bool {
		self.value.is_none() && self.get.is_none() && self.set.is_none() && self.writable.is_none() && self.enumerable.is_none() && self.configurable.is_none()
	}
	
	// 8.10.4 FromPropertyDescriptor ( Desc )
	pub fn from_property_descriptor(&self, env: &mut JsEnv) -> JsResult<Local<JsValue>> {
		let mut obj = env.new_object();
		
		if self.is_data() {
			let value = self.value(env);
			let writable = JsValue::new_bool(self.is_writable()).as_local(&env.heap);
			
			try!(obj.define_own_property(env, name::VALUE, JsDescriptor::new_simple_value(value), false));
			try!(obj.define_own_property(env, name::WRITABLE, JsDescriptor::new_simple_value(writable), false));
		} else if self.is_accessor() {
			let get = self.get(env);
			let set = self.set(env);
			
			try!(obj.define_own_property(env, name::GET, JsDescriptor::new_simple_value(get), false));
			try!(obj.define_own_property(env, name::SET, JsDescriptor::new_simple_value(set), false));
		}
		
		let enumerable = JsValue::new_bool(self.is_enumerable()).as_local(&env.heap);
		let configurable = JsValue::new_bool(self.is_configurable()).as_local(&env.heap);
		
		try!(obj.define_own_property(env, name::ENUMERABLE, JsDescriptor::new_simple_value(enumerable), false));
		try!(obj.define_own_property(env, name::CONFIGURABLE, JsDescriptor::new_simple_value(configurable), false));
		
		Ok(obj.as_value(env))
	}
	
	// 8.10.5 ToPropertyDescriptor ( Obj )
	pub fn to_property_descriptor(env: &mut JsEnv, obj: Local<JsValue>) -> JsResult<JsDescriptor> {
		if obj.ty() != JsType::Object {
			Err(JsError::new_type(env, ::errors::TYPE_INVALID))
		} else {
			let enumerable = if obj.has_property(env, name::ENUMERABLE) {
				let enumerable = try!(obj.get(env, name::ENUMERABLE));
				Some(enumerable.to_boolean())
			} else {
				None
			};
			let configurable = if obj.has_property(env, name::CONFIGURABLE) {
				let configurable = try!(obj.get(env, name::CONFIGURABLE));
				Some(configurable.to_boolean())
			} else {
				None
			};
			let value = if obj.has_property(env, name::VALUE) {
				Some(try!(obj.get(env, name::VALUE)))
			} else {
				None
			};
			let writable = if obj.has_property(env, name::WRITABLE) {
				let writable = try!(obj.get(env, name::WRITABLE));
				Some(writable.to_boolean())
			} else {
				None
			};
			let getter = if obj.has_property(env, name::GET) {
				let getter = try!(obj.get(env, name::GET));
				if getter.ty() != JsType::Undefined && !getter.is_callable(env) {
					return Err(JsError::new_type(env, ::errors::TYPE_ACCESSOR_NOT_CALLABLE));
				}
				Some(getter)
			} else {
				None
			};
			let setter = if obj.has_property(env, name::SET) {
				let setter = try!(obj.get(env, name::SET));
				if setter.ty() != JsType::Undefined && !setter.is_callable(env) {
					return Err(JsError::new_type(env, ::errors::TYPE_ACCESSOR_NOT_CALLABLE));
				}
				Some(setter)
			} else {
				None
			};
			if (getter.is_some() || setter.is_some()) && writable.is_some() {
				return Err(JsError::new_type(env, ::errors::TYPE_WRITABLE_INVALID_ON_ACCESSOR));
			}
			
			Ok(JsDescriptor {
				value: value,
				get: getter,
				set: setter,
				writable: writable,
				enumerable: enumerable,
				configurable: configurable
			})
		}
	}
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum JsType {
	Undefined = 0,
	Null = 1,
	Number = 2,
	Boolean = 3,
	String = 4,
	Object = 5,
	Iterator = 6,
	Scope = 7,
}

impl JsType {
	fn is_ptr(&self) -> bool {
		match *self {
			JsType::Object | JsType::String => true,
			_ => false
		}
	}
	
	fn is_primitive(&self) -> bool {
		match *self {
			JsType::Object => false,
			_ => true
		}
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
	strict: bool,
	mode: JsFnMode
}

impl JsArgs {
	pub fn arg(&self, env: &JsEnv, index: usize) -> Local<JsValue> {
		if self.args.len() > index {
			self.args[index]
		} else {
			JsValue::new_undefined().as_local(&env.heap)
		}
	}
	
	pub fn arg_or(&self, index: usize, def: Local<JsValue>) -> Local<JsValue> {
		if self.args.len() > index {
			self.args[index]
		} else {
			def
		}
	}
}

pub type JsFn = Fn(&mut JsEnv, JsArgs) -> JsResult<Local<JsValue>>;


pub enum JsFunction {
	Ir(FunctionRef),
	Native(Option<Name>, u32, *const JsFn, bool)
}

impl Clone for JsFunction {
	fn clone(&self) -> JsFunction {
		match *self {
			JsFunction::Ir(function_ref) => JsFunction::Ir(function_ref),
			JsFunction::Native(name, args, callback, can_construct) => JsFunction::Native(name, args, callback, can_construct)
		}
	}
}
