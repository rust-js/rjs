const INITIAL_OBJECT : usize = 20;

use syntax::Name;
use syntax::token::name;
use super::{JsEnv, JsFunction, JsValue, JsItem, JsDescriptor, JsScope, JsType, GC_OBJECT};
use gc::{Local, Ptr};
use super::hash::Hash;
use ::{JsResult, JsError};

pub struct JsObject {
	class: Option<Name>,
	value: Option<JsValue>,
	function: Option<JsFunction>,
	prototype: Ptr<JsObject>,
	props: Hash,
	extensible: bool
}

impl JsObject {
	pub fn new(env: &JsEnv) -> JsObject {
		JsObject {
			class: None,
			value: None,
			function: None,
			prototype: Ptr::null(),
			props: Hash::new(&env, INITIAL_OBJECT),
			extensible: true
		}
	}
	
	pub fn new_local(env: &JsEnv) -> Local<JsObject> {
		let mut result = env.heap.alloc_local(GC_OBJECT);
		*result = Self::new(env);
		result
	}
	
	pub fn new_function(env: &JsEnv, function: JsFunction, prototype: Local<JsObject>) -> Local<JsObject> {
		let mut result = Self::new_local(env);
		
		result.prototype = prototype.as_ptr();
		result.class = Some(name::FUNCTION_CLASS);
		result.function = Some(function);
		
		result
	}
	
	pub fn function(&self) -> &Option<JsFunction> {
		&self.function
	}
}

impl JsItem for Local<JsObject> {
	fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
		JsValue::new_object(self.as_ptr()).as_local(env)
	}

	// 8.12.1 [[GetOwnProperty]] (P)
	fn get_own_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
		self.props.get_value(property, env)
	}
	
	// 8.12.7 [[Delete]] (P, Throw)
	fn delete(&mut self, env: &JsEnv, property: Name, throw: bool) -> JsResult<bool> {
		if let Some(desc) = self.get_own_property(env, property) {
			if desc.is_configurable() {
				self.props.remove(property);
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
	fn define_own_property(&mut self, env: &JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
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
					
					self.props.add(property, &descriptor, env);
					
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
					self.props.replace(property, &descriptor);
					Ok(true)
				}
			}
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
