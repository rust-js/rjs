#![allow(unused_variables)]

use super::{JsEnv, JsValue, JsString, JsArgs, JsFnMode};
use gc::*;
use ::JsResult;
use ir::builder::{Block, Ir};
use std::rc::Rc;

impl JsEnv {
	pub fn call_block(&mut self, block: Rc<Block>, this: Option<Local<JsValue>>, args: Vec<Local<JsValue>>) -> JsResult<JsValue> {
		for i in 0..block.locals.len() {
			self.stack.push(JsValue::new_undefined());
		}
		
		let locals = self.stack.create_frame(block.locals.len());
		
		let ir = &block.ir[..];
		let mut ip = 0;
		
		loop {
			println!("IP: {}", ip);
			
			match &ir[ip] {
				&Ir::Add => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(2);
					let arg1 = frame.get(0).as_local(self);
					let arg2 = frame.get(1).as_local(self);
					let result = self.add(arg1, arg2);
					self.stack.drop_frame(frame);
					self.stack.push(*result);
				}
				&Ir::BitAnd => { panic!(); },
				&Ir::BitNot => { panic!(); },
				&Ir::BitOr => { panic!(); },
				&Ir::BitXOr => { panic!(); },
				&Ir::Call(count) => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(count as usize + 2);
					let mut args = Vec::new();
					for i in 0..count as usize {
						args.push(frame.get(i + 2).as_local(self));
					}
					
					let this = frame.get(0).as_local(self);
					let function = frame.get(1).as_local(self);
					let function = self.get_value(function);
					
					let args = JsArgs {
						function: function,
						this: this,
						args: args,
						mode: JsFnMode::Call
					};
					
					let result = try!(self.call_function(args));
					
					self.stack.drop_frame(frame);
					self.stack.push(*result);
				}
				&Ir::CurrentIter(local) => { panic!(); },
				&Ir::Debugger => { panic!(); },
				&Ir::Delete => { panic!(); },
				&Ir::Divide => { panic!(); },
				&Ir::Dup => {
					let frame = self.stack.create_frame(1);
					self.stack.push(frame.get(0));
				}
				&Ir::EndIter(local) => { panic!(); },
				&Ir::EnterWith => { panic!(); },
				&Ir::Eq => { panic!(); },
				&Ir::Ge => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(2);
					let arg1 = frame.get(0).as_local(self);
					let arg2 = frame.get(1).as_local(self);
					let result = self.compare_ge(arg1, arg2);
					self.stack.drop_frame(frame);
					self.stack.push(JsValue::new_bool(result));
				}
				&Ir::Gt => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(2);
					let arg1 = frame.get(0).as_local(self);
					let arg2 = frame.get(1).as_local(self);
					let result = self.compare_gt(arg1, arg2);
					self.stack.drop_frame(frame);
					self.stack.push(JsValue::new_bool(result));
				},
				&Ir::In => { panic!(); },
				&Ir::InstanceOf => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(2);
					let arg1 = frame.get(0).as_local(self);
					let arg2 = frame.get(1).as_local(self);
					let result = try!(self.instanceof(arg1, arg2));
					self.stack.drop_frame(frame);
					self.stack.push(*result);
				},
				&Ir::IntoIter(local) => { panic!(); },
				&Ir::Jump(label) => { panic!(); },
				&Ir::JumpEq(label) => { panic!(); },
				&Ir::JumpFalse(label) => {
					let frame = self.stack.create_frame(1);
					let jump = !frame.get(0).get_bool();
					self.stack.drop_frame(frame);
					if jump {
						ip  = label.offset();
						continue;
					}
				}
				&Ir::JumpTrue(label) => {
					let frame = self.stack.create_frame(1);
					let jump = frame.get(0).get_bool();
					self.stack.drop_frame(frame);
					if jump {
						ip  = label.offset();
						continue;
					}
				}
				&Ir::Le => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(2);
					let arg1 = frame.get(0).as_local(self);
					let arg2 = frame.get(1).as_local(self);
					let result = self.compare_le(arg1, arg2);
					self.stack.drop_frame(frame);
					self.stack.push(JsValue::new_bool(result));
				}
				&Ir::Leave(label) => { panic!(); },
				&Ir::LeaveWith => { panic!(); },
				&Ir::LoadArguments => { panic!(); },
				&Ir::LoadException => { panic!(); },
				&Ir::LoadF64(value) => self.stack.push(JsValue::new_number(value)),
				&Ir::LoadFalse => self.stack.push(JsValue::new_false()),
				&Ir::LoadFunction(function) => {
					let _scope = self.heap.new_local_scope();
					
					let function = *self.new_function(function);
					self.stack.push(function);
				}
				&Ir::LoadGlobal(name) => {
					let _scope = self.heap.new_local_scope();
					
					let value = try!(JsValue::new_object(self.global.as_ptr()).as_local(self).get(name, self));
					self.stack.push(*value);
				}
				&Ir::LoadGlobalThis => {
					self.stack.push(JsValue::new_object(self.global.as_ptr()));
				}
				&Ir::LoadI32(value) => self.stack.push(JsValue::new_number(value as f64)),
				&Ir::LoadI64(value) => self.stack.push(JsValue::new_number(value as f64)),
				&Ir::LoadIndex => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(2);
					
					let index = frame.get(1).as_local(self);
					let index = self.to_string(index);
					let index = self.intern(&index.to_string());
					
					let result = try!(frame.get(0).get(index, self));
					
					self.stack.drop_frame(frame);
					
					self.stack.push(*result);
				}
				&Ir::LoadLifted(name, depth) => { panic!(); },
				&Ir::LoadLocal(local) => self.stack.push(locals.get(local.offset())),
				&Ir::LoadMissing => { panic!(); },
				&Ir::LoadName(name) => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(1);
					let result = try!(frame.get(0).get(name, self));
					self.stack.drop_frame(frame);
					self.stack.push(*result);
				}
				&Ir::LoadNameLit => { panic!(); },
				&Ir::LoadNull => self.stack.push(JsValue::new_null()),
				&Ir::LoadParam(index) => {
					if index < args.len() as u32 {
						self.stack.push(*args[index as usize]);
					} else {
						self.stack.push(JsValue::new_undefined());
					}
				}
				&Ir::LoadRegex(ref pattern, ref modifiers) => { panic!(); },
				&Ir::LoadString(ref string) => {
					let _scope = self.heap.new_local_scope();
					
					let result = JsString::from_str(self, &string).as_value(self);
					self.stack.push(*result);
				}
				&Ir::LoadThis => {
					if let Some(this) = this {
						self.stack.push(*this);
					} else {
						self.stack.push(JsValue::new_object(self.global.as_ptr()));
					}
				},
				&Ir::LoadTrue => self.stack.push(JsValue::new_true()),
				&Ir::LoadUndefined => self.stack.push(JsValue::new_undefined()),
				&Ir::Lsh => { panic!(); },
				&Ir::Lt => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(2);
					let arg1 = frame.get(0).as_local(self);
					let arg2 = frame.get(1).as_local(self);
					let result = self.compare_gt(arg1, arg2);
					self.stack.drop_frame(frame);
					self.stack.push(JsValue::new_bool(result));
				},
				&Ir::Modulus => { panic!(); },
				&Ir::Multiply => { panic!(); },
				&Ir::Ne => { panic!(); },
				&Ir::Negative => { panic!(); },
				&Ir::New(count) => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(count as usize + 1);
					let mut args = Vec::new();
					for i in 0..count as usize {
						args.push(frame.get(i + 1).as_local(self));
					}
					
					let constructor = frame.get(0).as_local(self);
					
					let result = try!(self.construct(constructor, args));
					
					self.stack.drop_frame(frame);
					self.stack.push(*result);
				},
				&Ir::NewArray => { panic!(); },
				&Ir::NewObject => { panic!(); },
				&Ir::NextIter(local, label) => { panic!(); },
				&Ir::Not => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(1);
					let arg = frame.get(0).as_local(self);
					let result = self.logical_not(arg);
					self.stack.drop_frame(frame);
					self.stack.push(*result);
				},
				&Ir::Pick(depth) => { panic!(); },
				&Ir::Pop => {
					let frame = self.stack.create_frame(1);
					self.stack.drop_frame(frame);
				},
				&Ir::Positive => { panic!(); },
				&Ir::PushArray => { panic!(); },
				&Ir::Return => {
					let frame = self.stack.create_frame(1);
					let result = frame.get(0);
					self.stack.drop_frame(frame);
					return Ok(result);
				}
				&Ir::Rsh => { panic!(); },
				&Ir::RshZeroFill => { panic!(); },
				&Ir::StoreArguments => { panic!(); },
				&Ir::StoreGlobal(name) => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(1);
					try!(JsValue::new_object(self.global.as_ptr()).as_local(self).put(name, frame.get(0).as_local(self), true, self));
					self.stack.drop_frame(frame);
				}
				&Ir::StoreIndex => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(3);
					
					let index = frame.get(1).as_local(self);
					let index = self.to_string(index);
					let index = self.intern(&index.to_string());
					
					try!(frame.get(0).put(index, frame.get(2).as_local(self), true, self));
					
					self.stack.drop_frame(frame);
				}
				&Ir::StoreLifted(name, depth) => { panic!(); },
				&Ir::StoreLocal(local) => {
					let frame = self.stack.create_frame(1);
					locals.set(local.offset(), frame.get(0));
					self.stack.drop_frame(frame);
				}
				&Ir::StoreName(name) => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(2);
					try!(frame.get(0).put(name, frame.get(1).as_local(self), true, self));
					self.stack.drop_frame(frame);
				}
				&Ir::StoreNameLit => { panic!(); },
				&Ir::StoreGetter(function) => { panic!(); },
				&Ir::StoreNameGetter(name, function) => { panic!(); },
				&Ir::StoreSetter(function) => { panic!(); },
				&Ir::StoreNameSetter(name, function) => { panic!(); },
				&Ir::StoreParam(u32) => { panic!(); },
				&Ir::StrictEq => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(2);
					let arg1 = frame.get(0).as_local(self);
					let arg2 = frame.get(1).as_local(self);
					let result = self.strict_eq(arg1, arg2);
					self.stack.drop_frame(frame);
					
					self.stack.push(JsValue::new_bool(result));
				},
				&Ir::StrictNe => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(2);
					let arg1 = frame.get(0).as_local(self);
					let arg2 = frame.get(1).as_local(self);
					let result = self.strict_eq(arg1, arg2);
					self.stack.drop_frame(frame);
					
					self.stack.push(JsValue::new_bool(!result));
				},
				&Ir::Subtract => { panic!(); },
				&Ir::Swap => { panic!(); },
				&Ir::Throw => { panic!(); },
				&Ir::ToBoolean => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(1);
					let arg = frame.get(0).as_local(self);
					let result = self.to_boolean(arg);
					self.stack.drop_frame(frame);
					
					self.stack.push(JsValue::new_bool(result));
				}
				&Ir::Typeof => {
					let _scope = self.heap.new_local_scope();
					
					let frame = self.stack.create_frame(1);
					let arg = frame.get(0).as_local(self);
					let result = self.type_of(arg);
					self.stack.drop_frame(frame);
					
					self.stack.push(JsValue::new_string(result.as_ptr()));
				}
			}
			
			ip += 1;
		}
	}
}
