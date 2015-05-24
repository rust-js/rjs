#![allow(unused_variables)]

use super::{JsEnv, JsValue, JsString, JsItem, JsIterator};
use gc::*;
use ::{JsResult, JsError};
use ir::IrFunction;
use ir::builder::{Block, Ir};
use std::rc::Rc;
use super::stack::StackFrame;

enum Next {
	Next,
	Return(JsValue),
	Throw(JsError),
	Leave(usize),
	EndFinally
}

macro_rules! local_try {
	( $expr:expr ) => {
		match $expr {
			Ok(result) => result,
			Err(error) => return Next::Throw(error)
		}
	}
}

impl JsEnv {
	pub fn call_block(&mut self, block: Rc<Block>, this: Option<Local<JsValue>>, args: Vec<Local<JsValue>>, function: &IrFunction) -> JsResult<JsValue> {
		for i in 0..block.locals.len() {
			self.stack.push(JsValue::new_undefined());
		}
		
		let arguments = if function.has_arguments {
			let arguments = self.new_arguments(&args);
			self.stack.push(*try!(arguments));
			block.locals.len()
		} else {
			0
		};
		
		let locals =
			block.locals.len() +
			if function.has_arguments { 1 } else { 0 };
		
		let locals = self.stack.create_frame(locals);
		
		let ir = &block.ir[..];
		let mut ip = 0;
		let mut thrown = None;
		let mut leave = None;
		let strict = function.strict;
		
		loop {
			debugln!("IP: {}", ip);
			
			let ir = &ir[ip];
			
			match self.call_stmt(&mut ip, ir, &locals, &this, &args, &mut thrown, strict, arguments) {
				Next::Next => {},
				Next::Return(result) => return Ok(result),
				Next::Throw(error) => {
					thrown = Some(error.as_runtime(self));
					
					// Find the try/catch block that belongs to the current instruction.
					
					let mut found = false;
					
					for frame in &block.try_catches {
						// If the ip is in the try range, jump to the catch or
						// finally block.
						
						if frame.try.contains(ip) {
							ip = if let Some(catch) = frame.catch {
								catch.start().offset()
							} else if let Some(finally) = frame.finally {
								finally.start().offset()
							} else {
								panic!("Expected either a catch or finally block");
							};
							
							found = true;
							break;
						}
						
						// If the ip is in the catch block, jump to the finally
						// block if there is one.
						
						if let Some(catch) = frame.catch {
							if catch.contains(ip) {
								if let Some(finally) = frame.finally {
									ip = finally.start().offset();
									
									found = true;
									break;
								}
							}
						}
					}
					
					if !found {
						return Err(error);
					}
				}
				Next::Leave(leave_) => {
					// Leave occurs in a try or catch block and is always the last
					// statement of it. The purpose of the leave is to process
					// finally blocks. If the frame the leave belongs to contains
					// a finally block, queue the leave and jump into the finally
					// block. Otherwise jump to the leave instruction.
					// The end finally instruction takes the leave in flight and
					// jumps to it.
					
					let mut found = false;
					let mut next = false;
					
					for frame in &block.try_catches {
						if next {
							// If the leave instruction falls inside this frame, we
							// stop processing frames because we can actually jump to it.
							// Otherwise we check to see whether we need to process
							// this finally block.
							
							if frame.contains(leave_) {
								if let Some(finally) = frame.finally {
									// We have another finally block to process. Jump into
									// it.
									
									ip = finally.start().offset();
									
									found = true;
									break;
								}
							} else {
								// There are no matching frames left. We can just jump to
								// the leave.
								break;
							}
						}
						if frame.try.contains(ip) || frame.catch.map_or(false, |catch| catch.contains(ip)) {
							// If the leave is in a try or catch block, jump to the finally block.
							
							ip = if let Some(finally) = frame.finally {
								leave = Some(leave_);
								finally.start().offset()
							} else {
								leave = None;
								leave_
							};
							found = true;
							break;
						}
						if frame.finally.map_or(false, |finally| finally.contains(ip)) {
							// If the leave is in a finally block, treat it as an end finally
							// but with a specific leave.
							
							leave = Some(leave_);
							next = true;
						}
					}
					
					if !found {
						if next {
							// If the leave was in a finally block and we're treating this as
							// an end finally, and we couldn't find another finally block,
							// just jump to the leave.
							
							ip = leave_;
							leave = None;
						} else {
							panic!("Cannot find try/catch frame of leave");
						}
					}
				}
				Next::EndFinally => {
					// The end finally statement marks the end of a finally block.
					//
					// If we have an error in flight, we need to find the next catch
					// or finally block. If we find a catch, the catch handles the
					// exception. If we find a finally we leave the error in flight
					// and we'll get back here.
					//
					// If we don't have an error in flight, we process the pending
					// leave.
					
					if let Some(ref error) = thrown {
						// We have an error in flight and are exiting the finally
						// block. We need to find the next frame that can process
						// the error.
						//
						// If there is another frame present, and it contains a catch
						// block, let the catch block process the error. Otherwise,
						// it has a finally and we jump into that. We'll get back
						// here after that.
						
						let mut next = false;
						let mut found = false;
						
						for frame in &block.try_catches {
							if next {
								// If the frame has a catch block, let the catch
								// block handle the error. Otherwise it must have
								// a finally block and we enter that.
								
								if let Some(catch) = frame.catch {
									ip = catch.start().offset();
									
									found = true;
									break;
								} else if let Some(finally) = frame.finally {
									ip = finally.start().offset();
									
									found = true;
									break;
								} else {
									panic!("Expected at least a catch or finally block");
								}
							} else if frame.finally.map_or(false, |finally| finally.contains(ip)) {
								// If the end finally is part of this frame, start processing
								// frames to find the next finally.
								
								next = true;
							}
						}
						
						// If we didn't find another frame to process the error, move
						// up the stack.
						
						if !found {
							return Err(JsError::Runtime(error.clone()));
						}
					} else if let Some(leave_) = leave {
						// There are two cases for a pending leave. In the simple case,
						// the finally is being executed because we're leaving a try or
						// catch and the leave is the first instruction after the finally
						// block.
						
						if leave_ == ip + 1 {
							ip = leave_;
							continue;
						}
						
						// Otherwise, we're processing e.g. a break or continue which may
						// skip a few frames. If this is the case, we need to find the
						// block the end finally is in and check the next block for
						// a finally. If there is a finally that starts before the leave,
						// we start with that block and let the leave in place. We'll
						// get back here. Otherwise we jump to the pending leave.
						
						let mut next = false;
						let mut found = false;
						
						for frame in &block.try_catches {
							if next {
								// If the leave instruction falls inside this frame, we
								// stop processing frames because we can actually jump to it.
								// Otherwise we check to see whether we need to process
								// this finally block.
								
								if frame.contains(leave_) {
									if let Some(finally) = frame.finally {
										// We have another finally block to process. Jump into
										// it.
										
										ip = finally.start().offset();
										
										found = true;
										break;
									}
								} else {
									// There are no matching frames left.
									break;
								}
							} else if frame.finally.map_or(false, |finally| finally.contains(ip)) {
								// If the end finally is part of this frame, start processing
								// frames to find the next finally.
								
								next = true;
							}
						}
						
						// If we didn't find a frame, jump to the leave.
						
						if !found {
							ip = leave_;
						}
					} else {
						panic!("End finaly without pending error or leave");
					}
				}
			}
		}
	}
	
	#[inline(always)]
	fn call_stmt(&mut self, ip: &mut usize, ir: &Ir, locals: &StackFrame, this: &Option<Local<JsValue>>, args: &Vec<Local<JsValue>>, thrown: &mut Option<Root<JsValue>>, strict: bool, arguments: usize) -> Next {
		match ir {
			&Ir::Add => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self);
				let arg2 = frame.get(1).as_local(self);
				let result = local_try!(self.add(arg1, arg2));
				self.stack.drop_frame(frame);
				self.stack.push(*result);
			}
			&Ir::BitAnd => { unimplemented!(); },
			&Ir::BitNot => { unimplemented!(); },
			&Ir::BitOr => { unimplemented!(); },
			&Ir::BitXOr => { unimplemented!(); },
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
				
				let result = local_try!(function.call(self, this, args));
				
				self.stack.drop_frame(frame);
				self.stack.push(*result);
			}
			&Ir::CurrentIter(local) => {
				let _scope = self.heap.new_local_scope();
				
				let iterator = Local::from_ptr(locals.get(local.offset()).get_iterator(), &self.heap);
				let name = iterator.current();
				
				let result = if let Some(index) = name.index() {
					JsString::from_str(self, &index.to_string()).as_value(self)
				} else {
					JsString::from_str(self, &*self.ir.interner().get(name)).as_value(self)
				};
				
				self.stack.push(*result);
			},
			&Ir::Debugger => { unimplemented!(); },
			&Ir::DeleteIndex => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				
				let index = frame.get(1).as_local(self);
				let index = local_try!(self.intern_value(index));
				
				let result = local_try!(frame.get(0).as_local(self).delete(self, index, true));
				
				self.stack.drop_frame(frame);
				self.stack.push(JsValue::new_bool(result));
			}
			&Ir::DeleteName(name) => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(1);
				
				let result = local_try!(frame.get(0).as_local(self).delete(self, name, true));
				
				self.stack.drop_frame(frame);
				self.stack.push(JsValue::new_bool(result));
			}
			&Ir::Divide => { unimplemented!(); },
			&Ir::Dup => {
				let frame = self.stack.create_frame(1);
				self.stack.push(frame.get(0));
			}
			&Ir::EndFinally => return Next::EndFinally,
			&Ir::EndIter(local) => { /* no-op */ },
			&Ir::EnterWith => { unimplemented!(); },
			&Ir::Eq => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self);
				let arg2 = frame.get(1).as_local(self);
				let result = local_try!(self.eq(arg1, arg2));
				self.stack.drop_frame(frame);
				
				self.stack.push(JsValue::new_bool(result));
			}
			&Ir::Ge => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self);
				let arg2 = frame.get(1).as_local(self);
				let result = local_try!(self.compare_ge(arg1, arg2));
				self.stack.drop_frame(frame);
				self.stack.push(JsValue::new_bool(result));
			}
			&Ir::Gt => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self);
				let arg2 = frame.get(1).as_local(self);
				let result = local_try!(self.compare_gt(arg1, arg2));
				self.stack.drop_frame(frame);
				self.stack.push(JsValue::new_bool(result));
			},
			&Ir::In => { unimplemented!(); },
			&Ir::InstanceOf => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self);
				let arg2 = frame.get(1).as_local(self);
				let result = local_try!(self.instanceof(arg1, arg2));
				self.stack.drop_frame(frame);
				
				self.stack.push(*result);
			},
			&Ir::IntoIter(local) => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(1);
				
				let arg = frame.get(0).as_local(self);
				let result = JsIterator::new_local(self, arg).as_value(self);
				locals.set(local.offset(), *result);
				
				self.stack.drop_frame(frame);
			},
			&Ir::Jump(label) => {
				*ip = label.offset();
				return Next::Next;
			}
			&Ir::JumpEq(label) => { unimplemented!(); },
			&Ir::JumpFalse(label) => {
				let frame = self.stack.create_frame(1);
				let jump = !frame.get(0).get_bool();
				self.stack.drop_frame(frame);
				if jump {
					*ip  = label.offset();
					return Next::Next;
				}
			}
			&Ir::JumpTrue(label) => {
				let frame = self.stack.create_frame(1);
				let jump = frame.get(0).get_bool();
				self.stack.drop_frame(frame);
				if jump {
					*ip  = label.offset();
					return Next::Next;
				}
			}
			&Ir::Le => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self);
				let arg2 = frame.get(1).as_local(self);
				let result = local_try!(self.compare_le(arg1, arg2));
				self.stack.drop_frame(frame);
				self.stack.push(JsValue::new_bool(result));
			}
			&Ir::Leave(label) => return Next::Leave(label.offset()),
			&Ir::LeaveWith => { unimplemented!(); },
			&Ir::LoadArguments => self.stack.push(locals.get(arguments)),
			&Ir::LoadException => {
				if let Some(ref exception) = *thrown {
					self.stack.push(**exception)
				} else {
					panic!("Load exception statement without exception in flight");
				}

				// We clear the exception in flight here. Every catch
				// block begins with a load exception statement so
				// we're guarenteed to correctly clear the exception
				// in flight when we enter one.
				
				*thrown = None;
			}
			&Ir::LoadF64(value) => self.stack.push(JsValue::new_number(value)),
			&Ir::LoadFalse => self.stack.push(JsValue::new_false()),
			&Ir::LoadFunction(function) => {
				let _scope = self.heap.new_local_scope();
				
				let function = *local_try!(self.new_function(function));
				self.stack.push(function);
			}
			&Ir::LoadGlobal(name) => {
				let _scope = self.heap.new_local_scope();
				
				let global = self.global.as_local(self);
				if !global.has_property(self, name) {
					return Next::Throw(JsError::new_reference(self));
				}
				let value = local_try!(global.get(self, name));
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
				let index = local_try!(self.intern_value(index));
				
				let result = local_try!(frame.get(0).as_local(self).get(self, index));
				
				self.stack.drop_frame(frame);
				
				self.stack.push(*result);
			}
			&Ir::LoadLifted(name, depth) => { panic!("lifted not yet implemented"); },
			&Ir::LoadLocal(local) => self.stack.push(locals.get(local.offset())),
			&Ir::LoadMissing => { unimplemented!(); },
			&Ir::LoadName(name) => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(1);
				let result = local_try!(frame.get(0).as_local(self).get(self, name));
				self.stack.drop_frame(frame);
				self.stack.push(*result);
			}
			&Ir::LoadNameLit => { unimplemented!(); },
			&Ir::LoadNull => self.stack.push(JsValue::new_null()),
			&Ir::LoadParam(index) => {
				if index < args.len() as u32 {
					self.stack.push(*args[index as usize]);
				} else {
					self.stack.push(JsValue::new_undefined());
				}
			}
			&Ir::LoadRegex(ref pattern, ref modifiers) => { unimplemented!(); },
			&Ir::LoadString(ref string) => {
				let _scope = self.heap.new_local_scope();
				
				let result = JsString::from_str(self, &string).as_value(self);
				self.stack.push(*result);
			}
			&Ir::LoadThis => {
				if let Some(ref this) = *this {
					self.stack.push(**this);
				} else {
					self.stack.push(JsValue::new_object(self.global.as_ptr()));
				}
			},
			&Ir::LoadTrue => self.stack.push(JsValue::new_true()),
			&Ir::LoadUndefined => self.stack.push(JsValue::new_undefined()),
			&Ir::Lsh => { unimplemented!(); },
			&Ir::Lt => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self);
				let arg2 = frame.get(1).as_local(self);
				let result = local_try!(self.compare_gt(arg1, arg2));
				self.stack.drop_frame(frame);
				self.stack.push(JsValue::new_bool(result));
			},
			&Ir::Modulus => { unimplemented!(); },
			&Ir::Multiply => { unimplemented!(); },
			&Ir::Ne => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self);
				let arg2 = frame.get(1).as_local(self);
				let result = local_try!(self.ne(arg1, arg2));
				self.stack.drop_frame(frame);
				
				self.stack.push(JsValue::new_bool(result));
			},
			&Ir::Negative => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(1);
				let arg = frame.get(0).as_local(self);
				let result = local_try!(self.negative(arg));
				self.stack.drop_frame(frame);
				
				self.stack.push(JsValue::new_number(result));
			},
			&Ir::New(count) => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(count as usize + 1);
				let mut args = Vec::new();
				for i in 0..count as usize {
					args.push(frame.get(i + 1).as_local(self));
				}
				
				let constructor = frame.get(0).as_local(self);
				
				let result = local_try!(constructor.construct(self, args));
				
				self.stack.drop_frame(frame);
				self.stack.push(*result);
			},
			&Ir::NewArray => {
				let _scope = self.heap.new_local_scope();
					
				let result = self.new_array().as_value(self);
				self.stack.push(*result);
			}
			&Ir::NewObject => {
				let _scope = self.heap.new_local_scope();
					
				let result = self.new_object().as_value(self);
				self.stack.push(*result);
			}
			&Ir::NextIter(local, label) => {
				let _scope = self.heap.new_local_scope();
				
				let mut iterator = Local::from_ptr(locals.get(local.offset()).get_iterator(), &self.heap);
				
				if iterator.next(self) {
					*ip  = label.offset();
					return Next::Next;
				}
			},
			&Ir::Not => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(1);
				let arg = frame.get(0).as_local(self);
				let result = self.logical_not(arg);
				self.stack.drop_frame(frame);
				self.stack.push(*result);
			},
			&Ir::Pick(depth) => { unimplemented!(); },
			&Ir::Pop => {
				let frame = self.stack.create_frame(1);
				self.stack.drop_frame(frame);
			},
			&Ir::Positive => { unimplemented!(); },
			&Ir::Return => {
				let frame = self.stack.create_frame(1);
				let result = frame.get(0);
				self.stack.drop_frame(frame);
				return Next::Return(result);
			}
			&Ir::Rsh => { unimplemented!(); },
			&Ir::RshZeroFill => { unimplemented!(); },
			&Ir::StoreArguments => { unimplemented!(); },
			&Ir::StoreGlobal(name) => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(1);
				let value = frame.get(0).as_local(self);
				local_try!(self.global.as_local(self).put(self, name, value, strict));
				self.stack.drop_frame(frame);
			}
			&Ir::StoreIndex => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(3);
				
				let index = frame.get(1).as_local(self);
				let index = local_try!(self.intern_value(index));
				
				let value = frame.get(2).as_local(self);
				local_try!(frame.get(0).as_local(self).put(self, index, value, strict));
				
				self.stack.drop_frame(frame);
			}
			&Ir::StoreLifted(name, depth) => { panic!("lifted not yet implemented"); },
			&Ir::StoreLocal(local) => {
				let frame = self.stack.create_frame(1);
				locals.set(local.offset(), frame.get(0));
				self.stack.drop_frame(frame);
			}
			&Ir::StoreName(name) => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				let value = frame.get(1).as_local(self);
				local_try!(frame.get(0).as_local(self).put(self, name, value, strict));
				self.stack.drop_frame(frame);
			}
			&Ir::StoreGetter(function) => { unimplemented!(); },
			&Ir::StoreNameGetter(name, function) => { unimplemented!(); },
			&Ir::StoreSetter(function) => { unimplemented!(); },
			&Ir::StoreNameSetter(name, function) => { unimplemented!(); },
			&Ir::StoreParam(u32) => { unimplemented!(); },
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
			&Ir::Subtract => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self);
				let arg2 = frame.get(1).as_local(self);
				let result = local_try!(self.subtract(arg1, arg2));
				self.stack.drop_frame(frame);
				
				self.stack.push(JsValue::new_number(result));
			},
			&Ir::Swap => { unimplemented!(); },
			&Ir::Throw => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(1);
				let error = Root::from_local(&self.heap, frame.get(0).as_local(self));
				self.stack.drop_frame(frame);
				
				return Next::Throw(JsError::Runtime(error));
			}
			&Ir::ToBoolean => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(1);
				let arg = frame.get(0).as_local(self);
				let result = arg.to_boolean();
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
			&Ir::TypeofName(name) => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(1);
				let base = frame.get(0).as_local(self);
				let result = if base.is_undefined() {
					JsString::from_str(self, "undefined")
				} else {
					let arg = local_try!(base.get(self, name));
					self.type_of(arg)
				};
				
				self.stack.drop_frame(frame);
				
				self.stack.push(JsValue::new_string(result.as_ptr()));
			}
			&Ir::TypeofIndex => {
				let _scope = self.heap.new_local_scope();
				
				let frame = self.stack.create_frame(2);
				let base = frame.get(0).as_local(self);
				let result = if base.is_undefined() {
					JsString::from_str(self, "undefined")
				} else {
					let index = frame.get(1).as_local(self);
					let index = local_try!(self.intern_value(index));
					
					let arg = local_try!(base.get(self, index));
					self.type_of(arg)
				};
				
				self.stack.drop_frame(frame);
				
				self.stack.push(JsValue::new_string(result.as_ptr()));
			}
		}
		
		*ip += 1;
		
		Next::Next
	}
}
