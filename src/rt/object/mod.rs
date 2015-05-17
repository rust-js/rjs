extern crate libc;

use syntax::Name;
use syntax::token::name;
use super::{JsEnv, JsFunction, JsValue, JsItem, JsDescriptor, JsScope, JsType, GC_OBJECT};
use gc::{Local, Ptr};
use ::{JsResult, JsError};
use self::hash_store::HashStore;
use self::array_store::ArrayStore;
use std::mem;
use self::libc::c_void;
use std::str::FromStr;
use std::u32;

mod hash_store;
mod array_store;

pub struct JsObject {
	class: Option<Name>,
	value: Option<JsValue>,
	function: Option<JsFunction>,
	prototype: Ptr<JsObject>,
	store: StorePtr,
	extensible: bool
}

impl JsObject {
	pub fn new(env: &JsEnv, ty: JsStoreType) -> JsObject {
		let store = unsafe {
			match ty {
				JsStoreType::Hash => StorePtr::new(HashStore::new_local(env).as_ptr(), ty),
				JsStoreType::Array => StorePtr::new(ArrayStore::new_local(env).as_ptr(), ty)
			}
		};
		
		JsObject {
			class: None,
			value: None,
			function: None,
			prototype: Ptr::null(),
			store: store,
			extensible: true
		}
	}
	
	pub fn new_local(env: &JsEnv, ty: JsStoreType) -> Local<JsObject> {
		let mut result = env.heap.alloc_local(GC_OBJECT);
		*result = Self::new(env, ty);
		result
	}
	
	pub fn new_function(env: &JsEnv, function: JsFunction, prototype: Local<JsObject>) -> Local<JsObject> {
		let mut result = Self::new_local(env, JsStoreType::Hash);
		
		result.prototype = prototype.as_ptr();
		result.class = Some(name::FUNCTION_CLASS);
		result.function = Some(function);
		
		result
	}
	
	pub fn function(&self) -> &Option<JsFunction> {
		&self.function
	}
}

impl Local<JsObject> {
	pub fn value(&self, env: &JsEnv) -> Option<Local<JsValue>> {
		if let Some(value) = self.value {
			Some(value.as_local(env))
		} else {
			None
		}
	}
	
	pub fn set_value(&mut self, value: Option<Local<JsValue>>) {
		self.value = value.map(|value| *value);
	}

	// 8.12.9 [[DefineOwnProperty]] (P, Desc, Throw)
	fn define_own_object_property(&mut self, env: &mut JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
		let current = self.get_own_property(env, property);
		let extensible = self.is_extensible(env);
		
		match current {
			None => {
				return if !extensible {
					if throw { Err(JsError::Type) } else { Ok(false) }
				} else {
					if descriptor.is_generic() || descriptor.is_data() {
						JsDescriptor {
							value: Some(descriptor.value(env)),
							get: None,
							set: None,
							writable: Some(descriptor.is_writable()),
							enumerable: Some(descriptor.is_enumerable()),
							configurable: Some(descriptor.is_configurable())
						}
					} else {
						JsDescriptor {
							value: None,
							get: Some(descriptor.get(env)),
							set: Some(descriptor.set(env)),
							writable: Some(descriptor.is_writable()),
							enumerable: Some(descriptor.is_enumerable()),
							configurable: Some(descriptor.is_configurable())
						}
					};
					
					self.store.add(env, property, &descriptor);
					
					Ok(true)
				}
			}
			Some(current) => {
				if descriptor.is_empty() {
					return Ok(true);
				}
				if current.is_same(env, &descriptor) {
					return Ok(true);
				}
				
				fn can_write(env: &JsEnv, current: &JsDescriptor, desc: &JsDescriptor) -> bool {
					if 
						!current.is_configurable() &&
						(desc.is_configurable() || (desc.is_enumerable() == !current.is_enumerable()))
					{
						return false;
					}
					
					if current.is_generic() {
						return true;
					}

					if current.is_data() != desc.is_data() {
						if !current.is_configurable() {
							return false;
						}
						// TODO: Preservation of configurable, enumerable.
						return true;
					}
					
					if current.is_data() && desc.is_data() {
						if !current.is_configurable() {
							if !current.is_writable() && desc.is_writable() {
								return false;
							}
							if !current.is_writable() {
								if let Some(value) = desc.value {
									if !env.same_value(current.value(env), value) {
										return false;
									}
								}
							}
						}
						
						return true;
					}
					
					if current.is_accessor() && desc.is_accessor() {
						if !current.is_configurable() {
							if let Some(set) = desc.set {
								if !env.same_value(current.set(env), set) {
									return false;
								}
							}
							if let Some(get) = desc.get {
								if !env.same_value(current.get(env), get) {
									return false;
								}
							}
						}
					}
					
					true
				}
				
				if !can_write(env, &current, &descriptor) {
					if throw { Err(JsError::Type) } else { Ok(false) }
				} else {
					self.store.replace(env, property, &descriptor);
					Ok(true)
				}
			}
		}
	}
	
	// 15.4.5.1 [[DefineOwnProperty]] ( P, Desc, Throw )
	// =================================================
	// 
	// Array objects use a variation of the [[DefineOwnProperty]] internal method used for other native
	// ECMAScript objects (8.12.9).
	// 
	// Assume A is an Array object, Desc is a Property Descriptor, and Throw is a Boolean flag.
	// 
	// In the following algorithm, the term “Reject” means “If Throw is true, then throw a TypeError
	// exception, otherwise return false.”
	// 
	// When the [[DefineOwnProperty]] internal method of A is called with property P, Property Descriptor
	// Desc, and Boolean flag Throw, the following steps are taken:
	// 
	// 1.  Let oldLenDesc be the result of calling the [[GetOwnProperty]] internal method of A passing
	//     "length" as the argument. The result will never be undefined or an accessor descriptor because
	//     Array objects are created with a length data property that cannot be deleted or reconfigured.
	// 2.  Let oldLen be oldLenDesc.[[Value]].
	// 3.  If P is "length", then
	//     1.  If the [[Value]] field of Desc is absent, then
	//         1.  Return the result of calling the default [[DefineOwnProperty]] internal method (8.12.9)
	//             on A passing "length", Desc, and Throw as arguments.
	// 
	//     2.  Let newLenDesc be a copy of Desc.
	//     3.  Let newLen be ToUint32(Desc.[[Value]]).
	//     4.  If newLen is not equal to ToNumber( Desc.[[Value]]), throw a RangeError exception.
	//     5.  Set newLenDesc.[[Value] to newLen.
	//     6.  If newLen ≥oldLen, then
	//         1.  Return the result of calling the default [[DefineOwnProperty]] internal method (8.12.9)
	//             on A passing "length", newLenDesc, and Throw as arguments.
	// 
	//     7.  Reject if oldLenDesc.[[Writable]] is false.
	//     8.  If newLenDesc.[[Writable]] is absent or has the value true, let newWritable be true.
	//     9.  Else,
	//         1.  Need to defer setting the [[Writable]] attribute to false in case any elements cannot be
	//             deleted.
	//         2.  Let newWritable be false.
	//         3.  Set newLenDesc.[[Writable] to true.
	// 
	//     10. Let succeeded be the result of calling the default [[DefineOwnProperty]] internal method
	//         (8.12.9) on A passing "length", newLenDesc, and Throw as arguments.
	//     11. If succeeded is false, return false.
	//     12. While newLen < oldLen repeat,
	//         1.  Set oldLen to oldLen – 1.
	//         2.  Let deleteSucceeded be the result of calling the [[Delete]] internal method of A passing
	//             ToString(oldLen) and false as arguments.
	//         3.  If deleteSucceeded is false, then
	//             1.  Set newLenDesc.[[Value] to oldLen+1.
	//             2.  If newWritable is false, set newLenDesc.[[Writable] to false.
	//             3.  Call the default [[DefineOwnProperty]] internal method (8.12.9) on A passing
	//                 "length", newLenDesc, and false as arguments.
	//             4.  Reject.
	// 
	//     13. If newWritable is false, then
	//         1.  Call the default [[DefineOwnProperty]] internal method (8.12.9) on A passing "length",
	//             Property Descriptor{[[Writable]]: false}, and false as arguments. This call will always
	//             return true.
	// 
	//     14. Return true.
	// 
	// 4.  Else if P is an array index (15.4), then
	//     1.  Let index be ToUint32(P).
	//     2.  Reject if index ≥ oldLen and oldLenDesc.[[Writable]] is false.
	//     3.  Let succeeded be the result of calling the default [[DefineOwnProperty]] internal method
	//         (8.12.9) on A passing P, Desc, and false as arguments.
	//     4.  Reject if succeeded is false.
	//     5.  If index ≥ oldLen
	//         1.  Set oldLenDesc.[[Value]] to index + 1.
	//         2.  Call the default [[DefineOwnProperty]] internal method (8.12.9) on A passing "length",
	//             oldLenDesc, and false as arguments. This call will always return true.
	// 
	//     6.  Return true.
	// 
	// 5.  Return the result of calling the default [[DefineOwnProperty]] internal method (8.12.9) on A
	//     passing P, Desc, and Throw as arguments.
	fn define_own_array_property(&mut self, env: &mut JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
		let mut old_len_desc = self.get_own_property(env, name::LENGTH).unwrap();
		// This is safe because we control the value of length.
		let mut old_len = old_len_desc.value.unwrap().get_number() as usize;
		
		if property == name::LENGTH {
			return match descriptor.value {
				None => {
					self.define_own_object_property(
						env,
						name::LENGTH,
						descriptor,
						throw
					)
				}
				Some(desc_value) => {
					let mut new_len_desc = descriptor.clone();
					let new_len = try!(desc_value.to_uint32(env)) as usize;
					
					if new_len as f64 != try!(desc_value.to_number(env)) {
						Err(JsError::Range)
					} else {
						new_len_desc.value = Some(JsValue::new_number(new_len as f64).as_local(env));
						
						if new_len >= old_len {
							self.define_own_object_property(
								env,
								name::LENGTH,
								new_len_desc,
								throw
							)
						} else if !old_len_desc.is_writable() {
							if throw { Err(JsError::Type) } else { Ok(false) }
						} else {
							let new_writable = if new_len_desc.is_writable() {
								true
							} else {
								new_len_desc.writable = Some(true);
								false
							};
							
							let succeeded = try!(self.define_own_object_property(
								env,
								name::LENGTH,
								new_len_desc,
								throw
							));
							if !succeeded {
								return if throw { Err(JsError::Type) } else { Ok(false) };
							}
							
							while new_len < old_len {
								old_len -= 1;
								
								let delete_succeeded = try!(self.delete(
									env,
									Name::from_index(old_len),
									false
								));
								
								if !delete_succeeded {
									new_len_desc.value = Some(JsValue::new_number((old_len + 1) as f64).as_local(env));
									if !new_writable {
										new_len_desc.writable = Some(false);
									}
									try!(self.define_own_object_property(
										env,
										name::LENGTH,
										new_len_desc,
										false
									));
									
									return if throw { Err(JsError::Type) } else { Ok(false) };
								}
							}
							
							if !new_writable {
								try!(self.define_own_object_property(
									env,
									name::LENGTH,
									JsDescriptor {
										writable: Some(false),
										..JsDescriptor::default()
									},
									false
								));
							}
							
							Ok(true)
						}
					}
				}
			}
		} else {
			match Self::parse_array_index(env, property) {
				Some(index) => {
					if index >= old_len && !old_len_desc.is_writable() {
						if throw { Err(JsError::Type) } else { Ok(false) }
					} else {
						let succeeded = try!(self.define_own_object_property(
							env,
							property,
							descriptor,
							false
						));
						
						if !succeeded {
							if throw { Err(JsError::Type) } else { Ok(false) }
						} else if index >= old_len {
							old_len_desc.value = Some(JsValue::new_number((index + 1) as f64).as_local(env));
							try!(self.define_own_object_property(
								env,
								name::LENGTH,
								old_len_desc,
								false
							));
							
							Ok(true)
						} else {
							self.define_own_object_property(
								env,
								property,
								descriptor,
								throw
							)
						}
					}
				}
				None => {
					self.define_own_object_property(
						env,
						property,
						descriptor,
						throw
					)
				}
			}
		}
	}
	
	// 15.4 Array Objects
	fn is_array_index(env: &JsEnv, name: Name) -> bool {
		Self::parse_array_index(env, name).is_some()
	}
	
	// 15.4 Array Objects
	fn parse_array_index(env: &JsEnv, name: Name) -> Option<usize> {
		match name.index() {
			Some(index) => Some(index),
			None => {
				match u32::from_str(&env.ir.interner().get(name)) {
					Ok(value) => if value != u32::MAX { Some(value as usize) } else { None },
					Err(..) => None
				}
			}
		}
	}
}

impl JsItem for Local<JsObject> {
	fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_object(self.as_ptr()).as_local(env)
	}

	// 8.12.1 [[GetOwnProperty]] (P)
	fn get_own_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
		self.store.get_value(env, property)
	}
	
	// 8.12.7 [[Delete]] (P, Throw)
	fn delete(&mut self, env: &JsEnv, property: Name, throw: bool) -> JsResult<bool> {
		if let Some(desc) = self.get_own_property(env, property) {
			if desc.is_configurable() {
				self.store.remove(env, property);
				Ok(true)
			} else if throw {
				Err(JsError::Type)
			} else {
				Ok(false)
			}
		} else {
			Ok(true)
		}
	}
	
	// 8.12.9 [[DefineOwnProperty]] (P, Desc, Throw)
	// 15.4.5.1 [[DefineOwnProperty]] ( P, Desc, Throw )
	fn define_own_property(&mut self, env: &mut JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
		if self.class == Some(name::ARRAY_CLASS) {
			self.define_own_array_property(env, property, descriptor, throw)
		} else {
			self.define_own_object_property(env, property, descriptor, throw)
		}
	}
	
	fn is_callable(&self, _: &JsEnv) -> bool {
		self.function.is_some()
	}
	
	fn can_construct(&self, _: &JsEnv) -> bool {
		self.function.is_some()
	}
	
	fn has_prototype(&self, _: &JsEnv) -> bool {
		!self.prototype.is_null()
	}
	
	fn prototype(&self, env: &JsEnv) -> Option<Local<JsValue>> {
		if self.prototype.is_null() {
			None
		} else {
			Some(JsValue::new_object(self.prototype).as_local(env))
		}
	}
	
	fn set_prototype(&mut self, _: &JsEnv, prototype: Option<Local<JsValue>>) {
		if let Some(prototype) = prototype {
			if prototype.ty() == JsType::Object {
				self.prototype = prototype.get_object()
			}
		} else {
			self.prototype = Ptr::null()
		}
	}
	
	fn has_class(&self, _: &JsEnv) -> bool {
		self.class.is_some()
	}
	
	fn class(&self, _: &JsEnv) -> Option<Name> {
		self.class
	}
	
	fn set_class(&mut self, _: &JsEnv, class: Option<Name>) {
		self.class = class
	}
	
	fn is_extensible(&self, _: &JsEnv) -> bool {
		self.extensible
	}
	
	// 15.3.5.3 [[HasInstance]] (V)
	fn has_instance(&self, env: &mut JsEnv, object: Local<JsValue>) -> JsResult<bool> {
		if self.function.is_none() {
			Err(JsError::Type)
		} else if object.ty() != JsType::Object {
			Ok(false)
		} else {
			let prototype = try!(self.get(env, name::PROTOTYPE));
			if prototype.ty() != JsType::Object {
				Err(JsError::Type)
			} else {
				let mut object = object;
				
				loop {
					if let Some(object_) = object.prototype(env) {
						object = object_;
						if prototype.get_object().as_ptr() == object.get_object().as_ptr() {
							return Ok(true)
						}
					} else {
						return Ok(false)
					}
				}
			}
		}
	}
	
	fn scope(&self, _: &JsEnv) -> Option<Local<JsScope>> {
		unimplemented!();
	}
	
	fn formal_parameters(&self, _: &JsEnv) -> Option<Vec<Name>> {
		unimplemented!();
	}
	
	fn code(&self, _: &JsEnv) -> Option<String> {
		unimplemented!();
	}
	
	fn target_function(&self, _: &JsEnv) -> Option<Local<JsValue>> {
		unimplemented!();
	}
	
	fn bound_this(&self, _: &JsEnv) -> Option<Local<JsValue>> {
		unimplemented!();
	}
	
	fn bound_arguments(&self, _: &JsEnv) -> Option<Local<JsValue>> {
		unimplemented!();
	}
}

trait Store {
	fn add(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor);
	
	fn remove(&mut self, env: &JsEnv, name: Name) -> bool;
	
	fn get_value(&self, env: &JsEnv, name: Name) -> Option<JsDescriptor>;
	
	fn replace(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor) -> bool;
	
	fn key_iter(&self, env: &JsEnv) -> Box<Iterator<Item=Name>>;
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum JsStoreType {
	Hash = 1,
	Array = 2
}

struct StorePtr {
	ty: JsStoreType,
	ptr: usize
}

impl StorePtr {
	unsafe fn new<T>(ptr: Ptr<T>, ty: JsStoreType) -> StorePtr {
		StorePtr {
			ty: ty,
			ptr: ptr.as_ptr() as usize
		}
	}
	
	fn get_ptr<T>(&self) -> Ptr<T> {
		Ptr::from_ptr(self.ptr as *const c_void)
	}
	
	fn as_hash(&self, env: &JsEnv) -> Local<HashStore> {
		assert_eq!(self.ty, JsStoreType::Hash);
		
		Local::from_ptr(self.get_ptr(), &env.heap)
	}
	
	fn as_array(&self, env: &JsEnv) -> Local<ArrayStore> {
		assert_eq!(self.ty, JsStoreType::Array);
		
		Local::from_ptr(self.get_ptr(), &env.heap)
	}
}

macro_rules! delegate {
	( $target:expr, $env:expr, $method:ident ( $( $arg:expr ),* ) ) => {
		match $target.ty {
			JsStoreType::Hash => $target.as_hash($env).$method( $( $arg ),* ),
			JsStoreType::Array => $target.as_array($env).$method( $( $arg ),* ),
		}
	}
}

impl Store for StorePtr {
	fn add(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor) {
		delegate!(self, env, add(env, name, value))
	}
	
	fn remove(&mut self, env: &JsEnv, name: Name) -> bool {
		delegate!(self, env, remove(env, name))
	}
	
	fn get_value(&self, env: &JsEnv, name: Name) -> Option<JsDescriptor> {
		delegate!(self, env, get_value(env, name))
	}
	
	fn replace(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor) -> bool {
		delegate!(self, env, replace(env, name, value))
	}
	
	fn key_iter(&self, env: &JsEnv) -> Box<Iterator<Item=Name>> {
		delegate!(self, env, key_iter(env))
	}
}

const VALID        : u32 = 0b00001;
const WRITABLE     : u32 = 0b00010;
const ENUMERABLE   : u32 = 0b00100;
const CONFIGURABLE : u32 = 0b01000;
const ACCESSOR     : u32 = 0b10000;

#[derive(Copy, Clone)]
pub struct Entry {
	name: Name,
	flags: u32,
	next: i32,
	value1: JsValue,
	value2: JsValue
}

impl Entry {
	fn empty() -> Entry {
		unsafe { mem::zeroed() }
	}
	
	fn is_valid(&self) -> bool {
		(self.flags & VALID) != 0
	}
	
	fn as_property(&self, env: &JsEnv) -> JsDescriptor {
		if (self.flags & ACCESSOR) != 0 {
			JsDescriptor {
				value: None,
				get: Some(self.value1.as_local(env)),
				set: Some(self.value2.as_local(env)),
				writable: None,
				enumerable: Some((self.flags & ENUMERABLE) != 0),
				configurable: Some((self.flags & CONFIGURABLE) != 0)
			}
		} else {
			JsDescriptor {
				value: Some(self.value1.as_local(env)),
				get: None,
				set: None,
				writable: Some((self.flags & WRITABLE) != 0),
				enumerable: Some((self.flags & ENUMERABLE) != 0),
				configurable: Some((self.flags & CONFIGURABLE) != 0)
			}
		}
	}
	
	
	fn from_descriptor(descriptor: &JsDescriptor, name: Name, next: i32) -> Entry {
		let flags = VALID |
			if descriptor.writable.unwrap_or(true) { WRITABLE } else { 0 } |
			if descriptor.configurable.unwrap_or(true) { CONFIGURABLE } else { 0 } |
			if descriptor.enumerable.unwrap_or(true) { ENUMERABLE } else { 0 } |
			if descriptor.is_accessor() { ACCESSOR } else { 0 };
		
		let value1;
		let value2;
		
		if descriptor.is_accessor() {
			value1 = if let Some(get) = descriptor.get {
				*get
			} else {
				JsValue::new_undefined()
			};
			value2 = if let Some(set) = descriptor.set {
				*set
			} else {
				JsValue::new_undefined()
			};
		} else {
			value1 = if let Some(value) = descriptor.value {
				*value
			} else {
				JsValue::new_undefined()
			};
			value2 = JsValue::new_undefined();
		}
		
		Entry {
			name: name,
			flags: flags,
			next: next,
			value1: value1,
			value2: value2
		}
	}
}
