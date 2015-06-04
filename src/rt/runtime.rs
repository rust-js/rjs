use rt::*;
use gc::*;
use ::{JsResult, JsError};
use syntax::Name;
use syntax::ast::FunctionRef;
use syntax::token::name;
use std::f64;
use std::cmp;

trait SignedZero {
	#[inline(always)]
	fn is_positive_zero(&self) -> bool;
	
	#[inline(always)]
	fn is_negative_zero(&self) -> bool;
}

impl SignedZero for f64 {
	#[inline(always)]
	fn is_positive_zero(&self) -> bool {
		*self == 0f64 && self.is_sign_positive()
	}
	
	#[inline(always)]
	fn is_negative_zero(&self) -> bool {
		*self == 0f64 && self.is_sign_negative()
	}
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ComparisonResult {
	Undefined,
	True,
	False
}

impl JsEnv {
	// http://ecma-international.org/ecma-262/5.1/#sec-11.6.1
	pub fn add(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<Local<JsValue>> {
		let lprim = try!(lhs.to_primitive(self, JsPreferredType::None));
		let rprim = try!(rhs.to_primitive(self, JsPreferredType::None));
		
		if lprim.ty() == JsType::String || rprim.ty() == JsType::String {
			let lhs = try!(lprim.to_string(self));
			let rhs = try!(rprim.to_string(self));
			let result = JsString::concat(self, lhs, rhs);
			
			Ok(JsValue::new_string(result.as_ptr()).as_local(&self.heap))
		} else {
			Ok(JsValue::new_number(
				try!(lprim.to_number(self)) + try!(rprim.to_number(self))
			).as_local(&self.heap))
		}
	}
	
	// 11.6.2 The Subtraction Operator ( - )
	pub fn subtract(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<f64> {
		let lnum = try!(lhs.to_number(self));
		let rnum = try!(rhs.to_number(self));
		
		Ok(lnum - rnum)
	}
	
	// 11.4.7 Unary - Operator
	pub fn negative(&mut self, expr: Local<JsValue>) -> JsResult<f64> {
		let old_value = try!(expr.to_number(self));
		let result = if old_value.is_nan() {
			old_value
		} else {
			-old_value
		};
		
		Ok(result)
	}
	
	// 11.4.6 Unary + Operator
	pub fn positive(&mut self, expr: Local<JsValue>) -> JsResult<f64> {
		expr.to_number(self)
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.2.3
	// 10.4.3 Entering Function Code
	pub fn call_function(&mut self, args: JsArgs) -> JsResult<Local<JsValue>> {
		if args.function.ty() != JsType::Object {
			return Err(JsError::new_type(self, ::errors::TYPE_NOT_A_FUNCTION));
		};
		
		let function = args.function.unwrap_object().as_local(&self.heap).function();
		if !function.is_some() {
			return Err(JsError::new_type(self, ::errors::TYPE_NOT_A_FUNCTION));
		}
		
		let function = function.as_ref().unwrap();
		
		Ok(match *function {
			JsFunction::Ir(function_ref) => {
				let block = try!(self.ir.get_function_ir(function_ref));
				
				let function = self.ir.get_function(function_ref);
				let name = if let Some(name) = function.name {
					self.ir.interner().get(name).to_string()
				} else {
					"(anonymous)".to_string()
				};
				let location = format!("{}[{}:{}] {}", self.ir.interner().get(function.span.file), function.span.start_line, function.span.start_col, name);
				
				debugln!("ENTER {}", location);
				
				let mut args = args;
				
				if !function.strict {
					if args.this.is_null() || args.this.is_undefined() {
						args.this = self.global.as_value(self);
					} else {
						args.this = try!(args.this.to_object(self));
					}
				}
				
				let scope = args.function.scope(self)
					.unwrap_or_else(|| self.global_scope.as_local(&self.heap));
				
				let result = try!(self.call_block(block, args, &function, scope)).as_local(&self.heap);
				
				debugln!("EXIT {}", location);
				
				result
			}
			JsFunction::Native(_, _, ref callback, can_construct) => {
				if !can_construct && args.mode == JsFnMode::New {
					return Err(JsError::new_type(self, ::errors::TYPE_NOT_A_CONSTRUCTOR));
				}
				
				try!((*callback as &JsFn)(self, args))
			}
		})
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.4.3
	pub fn type_of(&mut self, value: Local<JsValue>) -> Local<JsString> {
		JsString::from_str(self, match value.ty() {
			JsType::Undefined => "undefined",
			JsType::Null => "object",
			JsType::Boolean => "boolean",
			JsType::Number => "number",
			JsType::String => "string",
			JsType::Object => if value.unwrap_object().as_local(&self.heap).function().is_some() { "function" } else { "object" },
			_ => panic!("unexpected type")
		})
	}
	
	// 11.8.5 The Abstract Relational Comparison Algorithm
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
		
		if px.ty() == JsType::String && py.ty() == JsType::String {
			Ok(self.compare_string(px, py))
		} else {
			let nx = try!(px.to_number(self));
			let ny = try!(py.to_number(self));
			
			let result = if nx.is_nan() || ny.is_nan() {
				ComparisonResult::Undefined
			} else if nx == ny {
				ComparisonResult::False
			} else if nx.is_positive_zero() && ny.is_negative_zero() {
				ComparisonResult::False
			} else if nx.is_negative_zero() && ny.is_positive_zero() {
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
		}
	}
	
	// 11.8.5 The Abstract Relational Comparison Algorithm
	pub fn compare_string(&mut self, x: Local<JsValue>, y: Local<JsValue>) -> ComparisonResult {
		assert_eq!(x.ty(), JsType::String);
		assert_eq!(y.ty(), JsType::String);
		
		let x = x.unwrap_string().chars;
		let y = y.unwrap_string().chars;
		
		let x_len = x.len();
		let y_len = y.len();
		let len = cmp::min(x_len, y_len);
		
		// If there is a character that  differs, return based on that
		// character.
		
		for i in 0..len {
			if x[i] != y[i] {
				return if x[i] < y[i] {
					ComparisonResult::True
				} else {
					ComparisonResult::False
				};
			}
		}
		
		// Otherwise, the strings are either the same or differ in length.
		
		if x_len >= y_len {
			ComparisonResult::False
		} else {
			ComparisonResult::True
		}
	}
	
	// 11.8.1 The Less-than Operator ( < )
	pub fn compare_lt(&mut self, lval: Local<JsValue>, rval: Local<JsValue>) -> JsResult<bool> {
		let result = try!(self.compare(lval, rval, true));
		
		Ok(match result {
			ComparisonResult::True => true,
			_ => false
		})
	}
	
	// 11.8.2 The Greater-than Operator ( > )
	pub fn compare_gt(&mut self, lval: Local<JsValue>, rval: Local<JsValue>) -> JsResult<bool>  {
		let result = try!(self.compare(rval, lval, false));
		
		Ok(match result {
			ComparisonResult::True => true,
			_ => false
		})
	}
	
	// 11.8.3 The Less-than-or-equal Operator ( <= )
	pub fn compare_le(&mut self, lval: Local<JsValue>, rval: Local<JsValue>) -> JsResult<bool>  {
		let result = try!(self.compare(rval, lval, false));
		
		Ok(match result {
			ComparisonResult::True | ComparisonResult::Undefined => false,
			_ => true
		})
	}
	
	// 11.8.4 The Greater-than-or-equal Operator ( >= )
	pub fn compare_ge(&mut self, lval: Local<JsValue>, rval: Local<JsValue>) -> JsResult<bool>  {
		let result = try!(self.compare(lval, rval, true));
		
		Ok(match result {
			ComparisonResult::True | ComparisonResult::Undefined => false,
			_ => true
		})
	}
	
	pub fn new_function(&mut self, function_ref: FunctionRef, scope: Option<Local<JsScope>>) -> JsResult<Local<JsValue>> {
		let function_prototype = self.function_prototype.as_local(&self.heap);
		let mut result = JsObject::new_function(self, JsFunction::Ir(function_ref), function_prototype).as_value(self);
		
		let function = self.ir.get_function(function_ref);
		if function.take_scope {
			result.set_scope(self, scope);
		}
		
		let mut proto = self.new_object();
		let value = proto.as_value(self);
		try!(result.define_own_property(self, name::PROTOTYPE, JsDescriptor::new_value(value, true, false, true), false));
		try!(proto.define_own_property(self, name::CONSTRUCTOR, JsDescriptor::new_value(result, true, false, true), false));
		
		Ok(result)
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.8.6
	pub fn instanceof(&mut self, lval: Local<JsValue>, rval: Local<JsValue>) -> JsResult<Local<JsValue>> {
		let result = try!(rval.has_instance(self, lval));
		Ok(JsValue::new_bool(result).as_local(&self.heap))
	}
	
	// http://ecma-international.org/ecma-262/5.1/#sec-11.4.9
	pub fn logical_not(&mut self, value: Local<JsValue>) -> Local<JsValue> {
		let value = value.to_boolean();
		JsValue::new_bool(!value).as_local(&self.heap)
	}
	
	// 11.9.1 The Equals Operator ( == )
	// 11.9.3 The Abstract Equality Comparison Algorithm
	pub fn eq(&mut self, lval: Local<JsValue>, rval: Local<JsValue>) -> JsResult<bool> {
		let lty = lval.ty();
		let rty = rval.ty();
		
		if lty == rty {
			Ok(self.strict_eq(lval, rval))
		} else if
			(lty == JsType::Null && rty == JsType::Undefined) ||
			(lty == JsType::Undefined && rty == JsType::Null)
		{
			Ok(true)
		} else if lty == JsType::Number && rty == JsType::String {
			let rval = try!(rval.to_number(self));
			let rval = JsValue::new_number(rval).as_local(&self.heap);
			self.eq(lval, rval)
		} else if lty == JsType::String && rty == JsType::Number {
			let lval = try!(lval.to_number(self));
			let lval = JsValue::new_number(lval).as_local(&self.heap);
			self.eq(lval, rval)
		} else if lty == JsType::Boolean {
			let lval = try!(lval.to_number(self));
			let lval = JsValue::new_number(lval).as_local(&self.heap);
			self.eq(lval, rval)
		} else if rty == JsType::Boolean {
			let rval = try!(rval.to_number(self));
			let rval = JsValue::new_number(rval).as_local(&self.heap);
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
	pub fn strict_eq(&mut self, lval: Local<JsValue>, rval: Local<JsValue>) -> bool {
		if lval.ty() != rval.ty() {
			false
		} else {
			match lval.ty() {
				JsType::Undefined | JsType::Null => true,
				JsType::Number => {
					let x = lval.unwrap_number();
					let y = rval.unwrap_number();
					
					if x.is_nan() || y.is_nan() {
						false
					} else {
						// -0 checks ommitted because these return true
						x == y 
					}
				}
				JsType::String => {
					let x = &*lval.unwrap_string().chars;
					let y = &*rval.unwrap_string().chars;
					
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
				JsType::Boolean => lval.unwrap_bool() == rval.unwrap_bool(),
				JsType::Object => lval.unwrap_object() == rval.unwrap_object(),
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
		
		let length = JsValue::new_number(0f64).as_local(&self.heap);
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
					let x_number = x.unwrap_number();
					let y_number = y.unwrap_number();
					
					if x_number.is_nan() && y_number.is_nan() {
						true
					} else if x_number == y_number {
						if
							(x_number.is_negative_zero() && !y_number.is_negative_zero()) ||
							(!x_number.is_negative_zero() && y_number.is_negative_zero())
						{
							false
						} else {
							true
						}
					} else {
						false
					}
				}
				JsType::String => JsString::equals(x.unwrap_string(), y.unwrap_string()),
				JsType::Boolean => x.unwrap_bool() == y.unwrap_bool(),
				JsType::Object => x.unwrap_object() == y.unwrap_object(),
				_ => panic!("unexpected type")
			}
		}
	}
	
	// 10.6 Arguments Object
	// TODO: Incomplete.
	pub fn new_arguments(&mut self, args: &JsArgs, strict: bool) -> JsResult<Local<JsValue>> {
		let mut result = self.new_object();
		
		result.set_class(self, Some(name::ARGUMENTS_CLASS));
		
		let value = JsValue::new_number(args.args.len() as f64).as_local(&self.heap);
		try!(result.define_own_property(self, name::LENGTH, JsDescriptor::new_value(value, true, false, true), false));
		
		for i in 0..args.args.len() {
			try!(result.define_own_property(self, Name::from_index(i), JsDescriptor::new_simple_value(args.args[i]), false));
		}
		
		if !strict {
			try!(result.define_own_property(self, name::CALLEE, JsDescriptor::new_value(args.function, true, false, true), false));
		} else {
			let prototype = self.function_prototype.as_local(&self.heap);
			let thrower = self.new_native_function(None, 0, &throw_type_error, prototype);
			
			try!(result.define_own_property(self, name::CALLEE, JsDescriptor::new_accessor(Some(thrower), Some(thrower), false, false), false));
			try!(result.define_own_property(self, name::CALLER, JsDescriptor::new_accessor(Some(thrower), Some(thrower), false, false), false));
		}
		
		Ok(result.as_value(self))
	}
	
	// 11.8.7 The in operator
	pub fn in_(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<Local<JsValue>> {
		if rhs.ty() != JsType::Object {
			Err(JsError::new_type(self, ::errors::TYPE_IN_RHS_NOT_OBJECT))
		} else {
			let name = try!(lhs.to_string(self));
			let name = self.ir.interner().intern(&name.to_string());
			
			let result = rhs.has_property(self, name);
			
			Ok(JsValue::new_bool(result).as_local(&self.heap))
		}
	}
	
	// 11.5.1 Applying the * Operator
	pub fn multiply(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<f64> {
		self.multiplicative(lhs, rhs, |lhs, rhs| lhs * rhs)
	}
	
	// 11.5.2 Applying the / Operator
	pub fn divide(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<f64> {
		self.multiplicative(lhs, rhs, |lhs, rhs| {
			if rhs == 0f64 {
				if lhs == 0f64 || lhs.is_nan() {
					f64::NAN
				} else if lhs.is_sign_positive() == rhs.is_sign_positive() {
					f64::INFINITY
				} else {
					f64::NEG_INFINITY
				}
			} else {
				lhs / rhs
			}
		})
	}
	
	// 11.5.3 Applying the % Operator
	pub fn modulus(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<f64> {
		self.multiplicative(lhs, rhs, |lhs, rhs| lhs % rhs)
	}
	
	// 11.5 Multiplicative Operators
	fn multiplicative<F: FnOnce(f64, f64) -> f64>(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>, func: F) -> JsResult<f64> {
		let left = try!(lhs.to_number(self));
		let right = try!(rhs.to_number(self));
		Ok(func(left, right))
	}
	
	// 11.10 Binary Bitwise Operators
	pub fn bit_and(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<f64> {
		self.bitwise(lhs, rhs, |lhs, rhs| lhs & rhs)
	}
	
	// 11.10 Binary Bitwise Operators
	pub fn bit_or(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<f64> {
		self.bitwise(lhs, rhs, |lhs, rhs| lhs | rhs)
	}
	
	// 11.10 Binary Bitwise Operators
	pub fn bit_xor(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<f64> {
		self.bitwise(lhs, rhs, |lhs, rhs| lhs ^ rhs)
	}
	
	// 11.10 Binary Bitwise Operators
	fn bitwise<F: FnOnce(i32, i32) -> i32>(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>, func: F) -> JsResult<f64> {
		let left = try!(lhs.to_int32(self));
		let right = try!(rhs.to_int32(self));
		Ok(func(left, right) as f64)
	}
	
	// 11.4.8 Bitwise NOT Operator ( ~ )
	pub fn bit_not(&mut self, arg: Local<JsValue>) -> JsResult<f64> {
		let arg = try!(arg.to_int32(self));
		Ok((!arg) as f64)
	}
	
	// 11.7.1 The Left Shift Operator ( << )
	pub fn lsh(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<f64> {
		let lnum = try!(lhs.to_int32(self));
		let rnum = try!(rhs.to_uint32(self));
		let shift_count = rnum & 0x1f;
		let result = lnum << shift_count;
		Ok(result as f64)
	}
	
	// 11.7.2 The Signed Right Shift Operator ( >> )
	pub fn rsh(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<f64> {
		let lnum = try!(lhs.to_int32(self));
		let rnum = try!(rhs.to_uint32(self));
		let shift_count = rnum & 0x1f;
		let result = lnum >> shift_count;
		Ok(result as f64)
	}
	
	// 11.7.3 The Unsigned Right Shift Operator ( >>> )
	pub fn unsigned_rsh(&mut self, lhs: Local<JsValue>, rhs: Local<JsValue>) -> JsResult<f64> {
		let lnum = try!(lhs.to_uint32(self));
		let rnum = try!(rhs.to_uint32(self));
		let shift_count = rnum & 0x1f;
		let result = lnum >> shift_count;
		Ok(result as f64)
	}
}

fn throw_type_error(env: &mut JsEnv, _: JsArgs) -> JsResult<Local<JsValue>> {
	Err(JsError::new_type(env, ::errors::TYPE_CANNOT_ACCESS_ARGUMENTS_PROPERTY))
}
