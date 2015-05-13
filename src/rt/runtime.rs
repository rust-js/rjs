use super::*;
use gc::*;
use ::{JsResult, JsError};
use syntax::ast::FunctionRef;
use super::hash::{Property, PropertyValue};
use syntax::token::name;
use std::f64;

pub enum ToPrimitiveHint {
	None,
	Number,
	String
}

pub enum ComparisonResult {
	Undefined,
	True,
	False
}

impl JsEnv {
	// http://ecma-international.org/ecma-262/5.1/#sec-11.6.1
	pub fn add(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> Local<JsValue> {
		let lhs = self.get_value(lhs);
		let rhs = self.get_value(rhs);
		let lprim = self.to_primitive(lhs, ToPrimitiveHint::None);
		let rprim = self.to_primitive(rhs, ToPrimitiveHint::None);
		
		if lprim.ty() == JsType::String || rprim.ty() == JsType::String {
			let lhs = self.to_string(lprim);
			let rhs = self.to_string(rprim);
			let result = JsString::concat(self, lhs, rhs);
			
			JsValue::new_string(result.as_ptr()).as_local(self)
		} else {
			JsValue::new_number(
				self.to_number(lprim) + self.to_number(rprim)
			).as_local(self)
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.2.3
	pub fn call_function(&mut self, args: JsArgs) -> JsResult<Local<JsValue>> {
		if args.function.ty() != JsType::Object {
			return Err(JsError::Type);
		};
		
		let function = &args.function.get_object().function;
		if !function.is_some() {
			return Err(JsError::Type);
		}
		
		let function = function.as_ref().unwrap();
		
		Ok(match function {
			&JsFunction::Ir(function_ref) => {
				let function = try!(self.ir.get_function_ir(function_ref));
				
				println!("ENTER {}", if let Some(name) = self.ir.get_function_description(function_ref).name { self.ir.interner().get(name).to_string() } else { "(anonymous)".to_string() });
				
				let result = try!(self.call_block(function, Some(args.this), args.args)).as_local(self);
				
				println!("EXIT {}", if let Some(name) = self.ir.get_function_description(function_ref).name { self.ir.interner().get(name).to_string() } else { "(anonymous)".to_string() });
				
				result
			}
			&JsFunction::Native(_, _, ref callback) => {
				try!(callback(self, args))
			}
		})
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.7.1
	pub fn get_value(&mut self, value: Local<JsValue>) -> Local<JsValue> {
		value
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-9.1
	pub fn to_primitive(&mut self, value: Local<JsValue>, _hint: ToPrimitiveHint) -> Local<JsValue> {
		if value.ty() == JsType::Object {
			panic!();
		} else {
			value
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-9.8
	pub fn to_string(&mut self, value: Local<JsValue>) -> Local<JsString> {
		match value.ty() {
			JsType::Undefined => JsString::from_str(self, "undefined"),
			JsType::Null => JsString::from_str(self, "null"),
			JsType::Boolean => JsString::from_str(self, if value.get_bool() { "true" } else { "false" }),
			JsType::Number => {
				// http://ecma-international.org/ecma-262/5.1/#sec-9.8.1
				panic!();
			}
			JsType::String => Local::from_ptr(value.get_string(), &mut self.heap),
			JsType::Object => {
				let result = self.to_primitive(value, ToPrimitiveHint::String);
				self.to_string(result)
			}
			JsType::None => panic!()
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.4.3
	pub fn type_of(&mut self, value: Local<JsValue>) -> Local<JsString> {
		JsString::from_str(self, match value.ty() {
			JsType::Undefined => "undefined",
			JsType::Null => "object",
			JsType::Boolean => "boolean",
			JsType::Number => "number",
			JsType::String => "string",
			JsType::Object => if value.get_object().function.is_some() { "function" } else { "object" },
			JsType::None => panic!()
		})
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-9.3
	pub fn to_number(&mut self, value: Local<JsValue>) -> f64 {
		match value.ty() {
			JsType::Undefined => f64::NAN,
			JsType::Null => 0f64,
			JsType::Boolean => if value.get_bool() { 1f64 } else { 0f64 },
			JsType::Number => value.get_number(),
			JsType::String => panic!(),
			JsType::Object => {
				let value = self.to_primitive(value, ToPrimitiveHint::Number);
				self.to_number(value)
			},
			JsType::None => panic!()
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.5
	pub fn compare(&mut self, x: Local<JsValue>, y: Local<JsValue>, left_first: bool) -> ComparisonResult {
		let px;
		let py;
		
		if left_first {
			px = self.to_primitive(x, ToPrimitiveHint::Number);
			py = self.to_primitive(y, ToPrimitiveHint::Number);
		} else {
			py = self.to_primitive(y, ToPrimitiveHint::Number);
			px = self.to_primitive(x, ToPrimitiveHint::Number);
		}
		
		if !(px.ty() == JsType::String && py.ty() == JsType::String) {
			let nx = self.to_number(px);
			let ny = self.to_number(py);
			if nx.is_nan() || ny.is_nan() {
				ComparisonResult::Undefined
			} else if nx == ny {
				ComparisonResult::False
			} else if nx == 0f64 && ny == -0f64 {
				ComparisonResult::False
			} else if nx == -0f64 && ny == 0f64 {
				ComparisonResult::False
			} else if nx == f64::INFINITY {
				ComparisonResult::False
			} else if ny == f64::INFINITY {
				ComparisonResult::True
			} else if ny == f64::NEG_INFINITY {
				ComparisonResult::False
			} else if nx == f64::NEG_INFINITY {
				ComparisonResult::True
			} else {
				if nx < ny { ComparisonResult::True } else { ComparisonResult::False }
			}
		} else {
			panic!();
		}
	}
	
	fn compare_any(&mut self, x: Local<JsValue>, y: Local<JsValue>, left_first: bool) -> ComparisonResult {
		self.compare(x, y, left_first)
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.1
	pub fn compare_lt(&mut self, x: Local<JsValue>, y: Local<JsValue>) -> bool {
		let result = self.compare_any(x, y, false);
		
		match result {
			ComparisonResult::True => true,
			_ => false
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.2
	pub fn compare_gt(&mut self, x: Local<JsValue>, y: Local<JsValue>) -> bool  {
		let result = self.compare_any(x, y, true);
		
		match result {
			ComparisonResult::True => true,
			_ => false
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.3
	pub fn compare_le(&mut self, x: Local<JsValue>, y: Local<JsValue>) -> bool  {
		let result = self.compare_any(x, y, false);
		
		match result {
			ComparisonResult::True | ComparisonResult::Undefined => false,
			_ => true
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.4
	pub fn compare_ge(&mut self, x: Local<JsValue>, y: Local<JsValue>) -> bool  {
		let result = self.compare_any(x, y, true);
		
		match result {
			ComparisonResult::True | ComparisonResult::Undefined => false,
			_ => true
		}
	}
	
	pub fn new_function(&mut self, function_ref: FunctionRef) -> Local<JsValue> {
		let mut proto = JsObject::new_local(self);
	
		proto.class = Some(name::FUNCTION_CLASS);
		
		let mut result = JsObject::new_local(self);
		
		result.prototype = self.function_prototype.as_ptr();
		result.class = Some(name::FUNCTION_CLASS);
		result.function = Some(JsFunction::Ir(function_ref));
		
		let result_value = result.as_value(self);
		
		result.props.add(name::PROTOTYPE, &Property::new_value(proto.as_value(self), true, false, true), self);
		proto.props.add(name::CONSTRUCTOR, &Property::new_value(result_value, true, false, true), self);
		
		result_value
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.6
	pub fn instanceof(&mut self, lref: Local<JsValue>, rref: Local<JsValue>) -> JsResult<Local<JsValue>> {
		let lval = self.get_value(lref);
		let rval = self.get_value(rref);
		
		if rval.ty() != JsType::Object {
			Err(JsError::Type)
		} else{
			let result = if lval.ty() != JsType::Object {
				false
			} else {
				try!(rval.has_instance(lval, self))
			};
				
			Ok(JsValue::new_bool(result).as_local(self))
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-13.2.2
	pub fn construct(&mut self, function: Local<JsValue>, args: Vec<Local<JsValue>>) -> JsResult<Local<JsValue>> {
		if function.ty() != JsType::Object {
			Err(JsError::Type)
		} else {
			let mut obj = JsObject::new_local(self);
			obj.class = Some(name::OBJECT_CLASS);
			let proto = try!(function.get(name::PROTOTYPE, self));
			obj.prototype = if proto.ty() == JsType::Object {
				proto.get_object()
			} else {
				self.object_prototype.as_ptr()
			};
			
			let obj = obj.as_value(self);
			
			let args = JsArgs {
				function: function,
				this: obj,
				args: args,
				mode: JsFnMode::New
			};
			
			let result = try!(self.call_function(args));
			
			if result.ty() == JsType::Object {
				Ok(result)
			} else {
				Ok(obj)
			}
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.4.9
	pub fn logical_not(&mut self, arg: Local<JsValue>) -> Local<JsValue> {
		let value = self.get_value(arg);
		let value = self.to_boolean(value);
		JsValue::new_bool(!value).as_local(self)
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-9.2
	pub fn to_boolean(&mut self, value: Local<JsValue>) -> bool {
		match value.ty() {
			JsType::Undefined | JsType::Null => false,
			JsType::Boolean => value.get_bool(),
			JsType::Number => {
				let value = value.get_number();
				!(value == 0f64 || value.is_nan())
			}
			JsType::String => value.get_string().chars.len() > 0,
			JsType::Object => true,
			JsType::None => panic!()
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.9.4
	// http://ecma-international.org/ecma-262/5.1/#sec-11.9.5
	// http://ecma-international.org/ecma-262/5.1/#sec-11.9.6
	pub fn strict_eq(&mut self, lref: Local<JsValue>, rref: Local<JsValue>) -> bool {
		let lval = self.get_value(lref);
		let rval = self.get_value(rref);
		
		if lval.ty() != rval.ty() {
			false
		} else {
			match lval.ty() {
				JsType::Undefined | JsType::Null => false,
				JsType::Number => {
					let x = lval.get_number();
					let y = rval.get_number();
					
					if x.is_nan() || y.is_nan() {
						false
					} else {
						// -0 checks ommitted because these return true
						x == y 
					}
				}
				JsType::String => {
					let x = &*lval.get_string().chars;
					let y = &*rval.get_string().chars;
					
					if x.len() != y.len() {
						false
					} else {
						for i in 0..x.len() {
							if x[i] != y[i] {
								return false;
							}
						}
						true
					}
				}
				JsType::Boolean => lval.get_bool() == rval.get_bool(),
				JsType::Object => lval.get_object() == rval.get_object(),
				JsType::None => panic!()
			}
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-15.2.2
	// TODO: Wrapping value not yet implemented.
	pub fn new_object(&mut self) -> Local<JsObject> {
		let mut obj = JsObject::new_local(self);
		obj.prototype = self.object_prototype.as_ptr();
		obj.class = Some(name::OBJECT_CLASS);
		obj
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.10.4
	pub fn from_property_descriptor(&mut self, property: Option<Property>) -> JsResult<Local<JsValue>> {
		match property {
			Some(property) => {
				let mut obj = self.new_object();
				
				match property.value {
					PropertyValue::Value { value } => {
						obj.define_own_property(
							name::VALUE,
							&Property::new_simple_value(value),
							false,
							self
						);
						obj.define_own_property(
							name::WRITABLE,
							&Property::new_simple_value(JsValue::new_bool(property.is_writable()).as_local(self)),
							false,
							self
						);
					}
					PropertyValue::Accessor { get, set } => {
						obj.define_own_property(
							name::GET,
							&Property::new_simple_value(get),
							false,
							self
						);
						obj.define_own_property(
							name::SET,
							&Property::new_simple_value(set),
							false,
							self
						);
					}
				}
				
				obj.define_own_property(
					name::ENUMERABLE,
					&Property::new_simple_value(JsValue::new_bool(property.is_enumerable()).as_local(self)),
					false,
					self
				);
				obj.define_own_property(
					name::CONFIGURABLE,
					&Property::new_simple_value(JsValue::new_bool(property.is_configurable()).as_local(self)),
					false,
					self
				);
				
				Ok(obj.as_value(self))
			}
			None => Ok(JsValue::new_undefined().as_local(self))
		}
	}
}
