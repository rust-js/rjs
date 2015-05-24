use super::*;
use gc::*;
use ::{JsResult, JsError};
use syntax::Name;
use syntax::ast::FunctionRef;
use syntax::token::name;
use std::f64;
use std::cmp;

pub enum ComparisonResult {
	Undefined,
	True,
	False
}

impl JsEnv {
	// http://ecma-international.org/ecma-262/5.1/#sec-11.6.1
	pub fn add(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<Local<JsValue>> {
		let lhs = self.get_value(lhs);
		let rhs = self.get_value(rhs);
		let lprim = try!(lhs.to_primitive(self, JsPreferredType::None));
		let rprim = try!(rhs.to_primitive(self, JsPreferredType::None));
		
		if lprim.ty() == JsType::String || rprim.ty() == JsType::String {
			let lhs = try!(lprim.to_string(self));
			let rhs = try!(rprim.to_string(self));
			let result = JsString::concat(self, lhs, rhs);
			
			Ok(JsValue::new_string(result.as_ptr()).as_local(self))
		} else {
			Ok(JsValue::new_number(
				try!(lprim.to_number(self)) + try!(rprim.to_number(self))
			).as_local(self))
		}
	}
	
	// 11.6.2 The Subtraction Operator ( - )
	pub fn subtract(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<f64> {
		let lhs = self.get_value(lhs);
		let rhs = self.get_value(rhs);
		let lnum = try!(lhs.to_number(self));
		let rnum = try!(rhs.to_number(self));
		
		Ok(lnum - rnum)
	}
	
	pub fn negative(&mut self, expr: Local<JsValue>) -> JsResult<f64> {
		let old_value = try!(expr.to_number(self));
		let result = if old_value.is_nan() {
			old_value
		} else {
			-old_value
		};
		
		Ok(result)
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.2.3
	pub fn call_function(&mut self, args: JsArgs) -> JsResult<Local<JsValue>> {
		if args.function.ty() != JsType::Object {
			return Err(JsError::new_type(self, ::errors::TYPE_INVALID));
		};
		
		let function = args.function.as_object(self).function();
		if !function.is_some() {
			return Err(JsError::new_type(self, ::errors::TYPE_INVALID));
		}
		
		let function = function.as_ref().unwrap();
		
		Ok(match function {
			&JsFunction::Ir(function_ref) => {
				let block = try!(self.ir.get_function_ir(function_ref));
				
				let function = self.ir.get_function_description(function_ref);
				let name = if let Some(name) = function.name {
					self.ir.interner().get(name).to_string()
				} else {
					"(anonymous)".to_string()
				};
				let location = format!("{}[{}:{}] {}", *function.span.file, function.span.start_line, function.span.start_col, name);
				
				debugln!("ENTER {}", location);
				
				let scope = args.function.scope(self);
				
				let result = try!(self.call_block(block, Some(args.this), args.args, &function, scope)).as_local(self);
				
				debugln!("EXIT {}", location);
				
				result
			}
			&JsFunction::Native(_, _, ref callback, can_construct) => {
				if !can_construct && args.mode == JsFnMode::New {
					return Err(JsError::new_type(self, ::errors::TYPE_NOT_A_CONSTRUCTOR));
				}
				
				try!((*callback as &JsFn)(self, args))
			}
		})
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-8.7.1
	pub fn get_value(&mut self, value: Local<JsValue>) -> Local<JsValue> {
		value
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.4.3
	pub fn type_of(&mut self, value: Local<JsValue>) -> Local<JsString> {
		JsString::from_str(self, match value.ty() {
			JsType::Undefined => "undefined",
			JsType::Null => "object",
			JsType::Boolean => "boolean",
			JsType::Number => "number",
			JsType::String => "string",
			JsType::Object => if value.as_object(self).function().is_some() { "function" } else { "object" },
			_ => panic!("unexpected type")
		})
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.5
	pub fn compare(&mut self, x: Local<JsValue>, y: Local<JsValue>, left_first: bool) -> JsResult<ComparisonResult> {
		let px;
		let py;
		
		if left_first {
			px = try!(x.to_primitive(self, JsPreferredType::Number));
			py = try!(y.to_primitive(self, JsPreferredType::Number));
		} else {
			py = try!(y.to_primitive(self, JsPreferredType::Number));
			px = try!(x.to_primitive(self, JsPreferredType::Number));
		}
		
		if !(px.ty() == JsType::String && py.ty() == JsType::String) {
			let nx = try!(px.to_number(self));
			let ny = try!(py.to_number(self));
			
			let result = if nx.is_nan() || ny.is_nan() {
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
			};
			
			Ok(result)
		} else {
			let px = try!(px.to_string(self)).to_string();
			let py = try!(py.to_string(self)).to_string();
			Ok(self.compare_string(&px, &py))
		}
	}
	
	// 11.8.5 The Abstract Relational Comparison Algorithm
	pub fn compare_string(&mut self, x: &str, y: &str) -> ComparisonResult {
		if x.starts_with(y) {
			ComparisonResult::False
		} else if y.starts_with(x) {
			ComparisonResult::True
		} else {
			let x = x.chars().collect::<Vec<_>>();
			let y = y.chars().collect::<Vec<_>>();
			
			let len = cmp::min(x.len(), y.len());
			let mut same = 0usize;
			
			for i in 0..len {
				if x[i] != y[i] {
					break;
				}
				same = i;
			}
			
			let x_char = x[same];
			let y_char = y[same];
			
			if x_char < y_char { ComparisonResult::True } else { ComparisonResult::False }
		}
	}
	
	fn compare_any(&mut self, x: Local<JsValue>, y: Local<JsValue>, left_first: bool) -> JsResult<ComparisonResult> {
		self.compare(x, y, left_first)
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.1
	pub fn compare_lt(&mut self, x: Local<JsValue>, y: Local<JsValue>) -> JsResult<bool> {
		let result = try!(self.compare_any(x, y, false));
		
		Ok(match result {
			ComparisonResult::True => true,
			_ => false
		})
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.2
	pub fn compare_gt(&mut self, x: Local<JsValue>, y: Local<JsValue>) -> JsResult<bool>  {
		let result = try!(self.compare_any(x, y, true));
		
		Ok(match result {
			ComparisonResult::True => true,
			_ => false
		})
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.3
	pub fn compare_le(&mut self, x: Local<JsValue>, y: Local<JsValue>) -> JsResult<bool>  {
		let result = try!(self.compare_any(x, y, false));
		
		Ok(match result {
			ComparisonResult::True | ComparisonResult::Undefined => false,
			_ => true
		})
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.4
	pub fn compare_ge(&mut self, x: Local<JsValue>, y: Local<JsValue>) -> JsResult<bool>  {
		let result = try!(self.compare_any(x, y, true));
		
		Ok(match result {
			ComparisonResult::True | ComparisonResult::Undefined => false,
			_ => true
		})
	}
	
	pub fn new_function(&mut self, function_ref: FunctionRef, scope: Option<Local<JsValue>>) -> JsResult<Local<JsValue>> {
		let mut proto = JsObject::new_local(self, JsStoreType::Hash);
	
		proto.set_class(self, Some(name::OBJECT_CLASS));
		
		let function_prototype = self.function_prototype.as_local(self);
		let mut result = JsObject::new_function(self, JsFunction::Ir(function_ref), function_prototype).as_value(self);
		
		if self.ir.get_function_description(function_ref).takes_scope {
			result.set_scope(self, scope);
		}
		
		let value = proto.as_value(self);
		try!(result.define_own_property(self, name::PROTOTYPE, JsDescriptor::new_value(value, true, false, true), false));
		try!(proto.define_own_property(self, name::CONSTRUCTOR, JsDescriptor::new_value(result, true, false, true), false));
		
		Ok(result)
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.6
	pub fn instanceof(&mut self, lref: Local<JsValue>, rref: Local<JsValue>) -> JsResult<Local<JsValue>> {
		let lval = self.get_value(lref);
		let rval = self.get_value(rref);
		
		let result = try!(rval.has_instance(self, lval));
		Ok(JsValue::new_bool(result).as_local(self))
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.4.9
	pub fn logical_not(&mut self, arg: Local<JsValue>) -> Local<JsValue> {
		let value = self.get_value(arg);
		let value = value.to_boolean();
		JsValue::new_bool(!value).as_local(self)
	}
	
	// 11.9.1 The Equals Operator ( == )
	// 11.9.3 The Abstract Equality Comparison Algorithm
	pub fn eq(&mut self, lref: Local<JsValue>, rref: Local<JsValue>) -> JsResult<bool> {
		let lval = self.get_value(lref);
		let rval = self.get_value(rref);
		
		let lty = lval.ty();
		let rty = rval.ty();
		
		if lty == rty {
			Ok(self.strict_eq(lref, rref))
		} else if
			(lty == JsType::Null && rty == JsType::Undefined) ||
			(lty == JsType::Undefined && rty == JsType::Null)
		{
			Ok(true)
		} else if lty == JsType::Number && rty == JsType::String {
			let rval = try!(rval.to_number(self));
			let rval = JsValue::new_number(rval).as_local(self);
			self.eq(lval, rval)
		} else if lty == JsType::String && rty == JsType::Number {
			let lval = try!(lval.to_number(self));
			let lval = JsValue::new_number(lval).as_local(self);
			self.eq(lval, rval)
		} else if lty == JsType::Boolean {
			let lval = try!(lval.to_number(self));
			let lval = JsValue::new_number(lval).as_local(self);
			self.eq(lval, rval)
		} else if rty == JsType::Boolean {
			let rval = try!(rval.to_number(self));
			let rval = JsValue::new_number(rval).as_local(self);
			self.eq(lval, rval)
		} else if (lty == JsType::String || lty == JsType::Number) && rty == JsType::Object {
			let rval = try!(rval.to_primitive(self, JsPreferredType::None));
			self.eq(lval, rval)
		} else if lty == JsType::Object && (rty == JsType::String || rty == JsType::Number) {
			let lval = try!(lval.to_primitive(self, JsPreferredType::None));
			self.eq(lval, rval)
		} else {
			Ok(false)
		}
	}
	
	// 11.9.2 The Does-not-equals Operator ( != )
	pub fn ne(&mut self, lref: Local<JsValue>, rref: Local<JsValue>) -> JsResult<bool> {
		return Ok(!try!(self.eq(lref, rref)))
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
				JsType::Undefined | JsType::Null => true,
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
				_ => panic!("unexpected type")
			}
		}
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-15.2.2
	// TODO: Wrapping value not yet implemented.
	pub fn new_object(&self) -> Local<JsObject> {
		let mut obj = JsObject::new_local(self, JsStoreType::Hash);
		
		obj.set_prototype(self, Some(self.object_prototype.as_value(self)));
		obj.set_class(self, Some(name::OBJECT_CLASS));
		
		obj
	}
	
	// 15.4.5.2 length
	pub fn new_array(&mut self) -> Local<JsObject> {
		let mut obj = JsObject::new_local(self, JsStoreType::Array);
		
		// This must be called before the class is set to get the
		// define_own_property implementation of Object.
		// We don't propagate the JsError because this define_own_property
		// will not fail.
		
		let length = JsValue::new_number(0f64).as_local(self);
		obj.define_own_property(
			self,
			name::LENGTH,
			JsDescriptor::new_value(length, true, false, false),
			false
		).ok();
		
		obj.set_prototype(self, Some(self.array_prototype.as_value(self)));
		obj.set_class(self, Some(name::ARRAY_CLASS));
		
		obj
	}
	
	// 9.12 The SameValue Algorithm
	pub fn same_value(&self, x: Local<JsValue>, y: Local<JsValue>) -> bool {
		let x_ty = x.ty();
		let y_ty = y.ty();
		
		if x_ty != y_ty {
			false
		} else {
			match x_ty {
				JsType::Undefined | JsType::Null => true,
				JsType::Number => {
					let x_number = x.get_number();
					let y_number = y.get_number();
					
					if x_number.is_nan() && y_number.is_nan() {
						true
					} else if x_number == y_number {
						if (x_number == -0f64 && y_number != -0f64) || (x_number != -0f64 && y_number == -0f64) {
							false
						} else {
							true
						}
					} else {
						false
					}
				}
				JsType::String => JsString::equals(x.get_string(), y.get_string()),
				JsType::Boolean => x.get_bool() == y.get_bool(),
				JsType::Object => x.get_object().as_ptr() == y.get_object().as_ptr(),
				_ => panic!("unexpected type")
			}
		}
	}
	
	// 10.6 Arguments Object
	// TODO: Incomplete.
	pub fn new_arguments(&mut self, args: &Vec<Local<JsValue>>) -> JsResult<Local<JsValue>> {
		let mut result = self.new_object();
		
		result.set_class(self, Some(name::ARGUMENTS_CLASS));
		
		let value = JsValue::new_number(args.len() as f64).as_local(self);
		try!(result.define_own_property(self, name::LENGTH, JsDescriptor::new_value(value, true, false, true), false));
		
		for i in 0..args.len() {
			try!(result.define_own_property(self, Name::from_index(i), JsDescriptor::new_simple_value(args[i]), false));
		}
		
		Ok(result.as_value(self))
	}
}
