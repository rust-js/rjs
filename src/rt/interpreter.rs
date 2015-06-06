#![allow(unused_variables)]

use rt::{JsEnv, JsValue, JsString, JsItem, JsIterator, JsScope, JsType, JsArgs};
use rt::{JsDescriptor, JsPreferredType};
use gc::*;
use ::{JsResult, JsError};
use ir::IrFunction;
use ir::builder::{Block, Ir};
use std::rc::Rc;
use rt::stack::StackFrame;
use syntax::Name;
use syntax::ast::ScopeType;
use syntax::token::name;
use syntax::parser::ParseMode;

enum Next {
	Next,
	Return(JsValue),
	Throw(JsError),
	Leave(usize),
	EndFinally
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum CallMode {
	Global,
	Eval,
	This
}

macro_rules! local_try {
	( $expr:expr ) => {
		match $expr {
			Ok(result) => result,
			Err(error) => return Next::Throw(error)
		}
	}
}

macro_rules! numeric_op {
	( $frame:expr , $method:ident ) => { {
		let _scope = $frame.env.heap.new_local_scope();
		
		let frame = $frame.env.stack.create_frame(1);
		let arg = frame.get(0).as_local(&$frame.env.heap);
		let result = local_try!($frame.env.$method(arg));
		$frame.env.stack.drop_frame(frame);
		
		$frame.env.stack.push(JsValue::new_number(result));
	} }
}

macro_rules! numeric_bin_op {
	( $frame:expr , $method:ident ) => { {
		let _scope = $frame.env.heap.new_local_scope();
		
		let frame = $frame.env.stack.create_frame(2);
		let arg1 = frame.get(0).as_local(&$frame.env.heap);
		let arg2 = frame.get(1).as_local(&$frame.env.heap);
		let result = local_try!($frame.env.$method(arg1, arg2));
		$frame.env.stack.drop_frame(frame);
		
		$frame.env.stack.push(JsValue::new_number(result));
	} }
}

struct Frame<'a> {
	env: &'a mut JsEnv,
	ip: usize,
	locals: StackFrame,
	args: JsArgs,
	thrown: Option<Root<JsValue>>,
	strict: bool,
	scope: usize,
	built_scope: bool
}

impl JsEnv {
	pub fn call_block(&mut self, block: Rc<Block>, args: JsArgs, function: &IrFunction, scope: Local<JsScope>) -> JsResult<JsValue> {
		let mut locals = block.locals.len();
		
		for i in 0..block.locals.len() {
			self.stack.push(JsValue::new_undefined());
		}
		
		// Build the environment context.
		
		let scope = {
			let _scope = self.heap.new_local_scope();
			
			let scope = if let ScopeType::Thin(size) = function.build_scope {
				JsScope::new_local_thin(self, size as usize, Some(scope)).as_value(self)
			} else if function.build_scope == ScopeType::Thick {
				let mut scope_object = self.new_object();
				scope_object.set_prototype(&self, None);
				
				JsScope::new_local_thick(self, scope_object, Some(scope), true).as_value(self)
			} else {
				scope.as_value(self)
			};
			
			self.stack.push(*scope);
			
			locals += 1;
			locals - 1
		};
		
		let ir = &block.ir[..];
		let mut leave = None;
		
		let locals = self.stack.create_frame(locals);
		
		let mut frame = Frame {
			env: self,
			ip: 0,
			locals: locals,
			args: args,
			thrown: None,
			strict: function.strict,
			scope: scope,
			built_scope: function.build_scope != ScopeType::None
		};
		
		loop {
			debugln!("IP: {}", frame.ip);
			
			let ir = &ir[frame.ip];
			
			match frame.call_stmt(ir) {
				Next::Next => {}
				Next::Return(result) => {
					frame.env.stack.drop_frame(frame.locals);
					
					return Ok(result);
				}
				Next::Throw(error) => {
					frame.thrown = Some(error.as_runtime(frame.env));
					
					// Find the try/catch block that belongs to the current instruction.
					
					let mut found = false;
					
					for try_catch in &block.try_catches {
						// If the ip is in the try range, jump to the catch or
						// finally block.
						
						if try_catch.try.contains(frame.ip) {
							frame.ip = if let Some(catch) = try_catch.catch {
								catch.start().offset()
							} else if let Some(finally) = try_catch.finally {
								finally.start().offset()
							} else {
								panic!("Expected either a catch or finally block");
							};
							
							found = true;
							break;
						}
						
						// If the ip is in the catch block, jump to the finally
						// block if there is one.
						
						if let Some(catch) = try_catch.catch {
							if catch.contains(frame.ip) {
								if let Some(finally) = try_catch.finally {
									frame.ip = finally.start().offset();
									
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
					
					// A leave from anywhere will suppress the thrown exception.
					
					frame.thrown = None;
					
					for try_catch in &block.try_catches {
						if next {
							// If the leave instruction falls inside this frame, we
							// stop processing frames because we can actually jump to it.
							// Otherwise we check to see whether we need to process
							// this finally block.
							
							if try_catch.contains(leave_) {
								if let Some(finally) = try_catch.finally {
									// We have another finally block to process. Jump into
									// it.
									
									frame.ip = finally.start().offset();
									
									found = true;
									break;
								}
							} else {
								// There are no matching frames left. We can just jump to
								// the leave.
								break;
							}
						}
						if try_catch.try.contains(frame.ip) || try_catch.catch.map_or(false, |catch| catch.contains(frame.ip)) {
							// If the leave is in a try or catch block, jump to the finally block.
							
							frame.ip = if let Some(finally) = try_catch.finally {
								leave = Some(leave_);
								finally.start().offset()
							} else {
								leave = None;
								leave_
							};
							found = true;
							break;
						}
						if try_catch.finally.map_or(false, |finally| finally.contains(frame.ip)) {
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
							
							frame.ip = leave_;
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
					
					if let Some(ref error) = frame.thrown {
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
						
						for try_catch in &block.try_catches {
							if next {
								if try_catch.try.contains(frame.ip) {
									// If we're propagating the exception from a try block
									// and the frame has a catch block, let the catch
									// block handle the error. Otherwise it must have
									// a finally block and we enter that.
									
									if let Some(catch) = try_catch.catch {
										frame.ip = catch.start().offset();
									} else if let Some(finally) = try_catch.finally {
										frame.ip = finally.start().offset();
									} else {
										panic!("expected at least a catch or finally block");
									}
									
									found = true;
									break;
								} else if try_catch.catch.map_or(false, |catch| catch.contains(frame.ip)) {
									// If we're propagating the exception from a catch block
									// and the frame has a finally block, enter that. Otherwise
									// look at the next frame.
									
									if let Some(finally) = try_catch.finally {
										frame.ip = finally.start().offset();
										
										found = true;
										break;
									}
								}
							} else if try_catch.finally.map_or(false, |finally| finally.contains(frame.ip)) {
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
						
						if leave_ == frame.ip + 1 {
							frame.ip = leave_;
							leave = None;
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
						
						for try_catch in &block.try_catches {
							if next {
								// If the leave instruction falls inside this frame, we
								// stop processing frames because we can actually jump to it.
								// Otherwise we check to see whether we need to process
								// this finally block.
								
								if try_catch.contains(leave_) {
									if let Some(finally) = try_catch.finally {
										// We have another finally block to process. Jump into
										// it.
										
										frame.ip = finally.start().offset();
										
										found = true;
										break;
									}
								} else {
									// There are no matching frames left.
									break;
								}
							} else if try_catch.finally.map_or(false, |finally| finally.contains(frame.ip)) {
								// If the end finally is part of this frame, start processing
								// frames to find the next finally.
								
								next = true;
							}
						}
						
						// If we didn't find a frame, jump to the leave.
						
						if !found {
							frame.ip = leave_;
							leave = None;
						}
					} else {
						// If we don't have a pending leave, we're leaving the finally block
						// because an exception was thrown in this block that was suppressed
						// by a caught exception in the finally block. In that case, we can
						// just continue with the next statement, like:
						//
						// try {
						//   throw '';
						// } finally {
						//   try {
						//     throw '';
						//   catch (e) {
						//   }
						// }
						//
						
						frame.ip += 1;
					}
				}
			}
		}
	}
}

impl<'a> Frame<'a> {
	#[inline(always)]
	fn call_stmt(&mut self, ir: &Ir) -> Next {
		match *ir {
			Ir::Add => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let result = local_try!(self.env.add(arg1, arg2));
				
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(*result);
			}
			Ir::BitAnd => numeric_bin_op!(self, bit_and),
			Ir::BitNot => numeric_op!(self, bit_not),
			Ir::BitOr => numeric_bin_op!(self, bit_or),
			Ir::BitXOr => numeric_bin_op!(self, bit_xor),
			Ir::Call(count) => local_try!(self.call(count, CallMode::Global)),
			Ir::CallEval(count) => local_try!(self.call(count, CallMode::Eval)),
			Ir::CallThis(count) => local_try!(self.call(count, CallMode::This)),
			Ir::CurrentIter(local) => {
				let _scope = self.env.heap.new_local_scope();
				
				let iterator = self.locals.get(local.offset()).unwrap_iterator().as_local(&self.env.heap);
				let name = iterator.current();
				
				let result = if let Some(index) = name.index() {
					JsString::from_str(self.env, &index.to_string()).as_value(self.env)
				} else {
					JsString::from_str(self.env, &*self.env.ir.interner().get(name)).as_value(self.env)
				};
				
				self.env.stack.push(*result);
			}
			Ir::Debugger => unimplemented!(),
			Ir::Delete => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(false));
			}
			Ir::DeleteIndex => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				
				let mut target = frame.get(0).as_local(&self.env.heap);
				let index = frame.get(1).as_local(&self.env.heap);
				let index = local_try!(self.env.intern_value(index));
				
				let result = local_try!(target.delete(self.env, index, self.strict));
				
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(result));
			}
			Ir::DeleteName(name) => {
				match local_try!(self.delete(name, false)) {
					Some(next) => return next,
					_ => {}
				}
			}
			Ir::DeleteEnvName(name) => {
				match local_try!(self.delete(name, true)) {
					Some(next) => return next,
					_ => {}
				}
			}
			Ir::Divide => numeric_bin_op!(self, divide),
			Ir::Dup => {
				let frame = self.env.stack.create_frame(1);
				self.env.stack.push(frame.get(0));
			}
			Ir::EndFinally => return Next::EndFinally,
			Ir::EndIter(local) => { /* no-op */ }
			Ir::EnterEnv => {
				let _scope = self.env.heap.new_local_scope();
				
				let mut scope_object = self.env.new_object();
				scope_object.set_prototype(self.env, None);
				
				let scope = JsScope::new_local_thick(
					self.env,
					scope_object,
					self.get_scope(),
					false
				);
				
				self.locals.set(self.scope, *scope.as_value(self.env));
			}
			Ir::EnterWithEnv => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				
				let scope_object = frame.get(0).as_local(&self.env.heap);
				let scope_object = local_try!(scope_object.to_object(&mut self.env));
				
				let scope = JsScope::new_local_thick(
					self.env,
					scope_object.unwrap_object().as_local(&self.env.heap),
					self.get_scope(),
					false
				);
				
				self.locals.set(self.scope, *scope.as_value(self.env));
				
				self.env.stack.drop_frame(frame);
			}
			Ir::Eq => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let result = local_try!(self.env.eq(arg1, arg2));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(result));
			}
			Ir::Ge => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let result = local_try!(self.env.compare_ge(arg1, arg2));
				self.env.stack.drop_frame(frame);
				self.env.stack.push(JsValue::new_bool(result));
			}
			Ir::Gt => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let result = local_try!(self.env.compare_gt(arg1, arg2));
				self.env.stack.drop_frame(frame);
				self.env.stack.push(JsValue::new_bool(result));
			}
			Ir::In => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let result = local_try!(self.env.in_(arg1, arg2));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(*result);
			}
			Ir::InitEnvName(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let mut target = frame.get(0).as_local(&self.env.heap);
				let value = frame.get(1).as_local(&self.env.heap);
				if !target.has_property(self.env, name) {
					local_try!(target.define_own_property(
						self.env,
						name,
						JsDescriptor::new_value(value, true, true, false),
						true
					));
				}
				self.env.stack.drop_frame(frame);
			}
			Ir::InstanceOf => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let result = local_try!(self.env.instanceof(arg1, arg2));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(*result);
			}
			Ir::IntoIter(local) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				
				let arg = frame.get(0).as_local(&self.env.heap);
				let result = JsIterator::new_local(self.env, arg).as_value(self.env);
				self.locals.set(local.offset(), *result);
				
				self.env.stack.drop_frame(frame);
			}
			Ir::Jump(label) => {
				self.ip = label.offset();
				return Next::Next;
			}
			Ir::JumpStrictEq(label) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let jump = self.env.strict_eq(arg1, arg2);
				self.env.stack.drop_frame(frame);
				
				if jump {
					self.ip  = label.offset();
					return Next::Next;
				}
			}
			Ir::JumpFalse(label) => {
				let frame = self.env.stack.create_frame(1);
				let jump = !frame.get(0).as_local(&self.env.heap).to_boolean();
				self.env.stack.drop_frame(frame);
				if jump {
					self.ip  = label.offset();
					return Next::Next;
				}
			}
			Ir::JumpTrue(label) => {
				let frame = self.env.stack.create_frame(1);
				let jump = frame.get(0).as_local(&self.env.heap).to_boolean();
				self.env.stack.drop_frame(frame);
				if jump {
					self.ip  = label.offset();
					return Next::Next;
				}
			}
			Ir::Le => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let result = local_try!(self.env.compare_le(arg1, arg2));
				self.env.stack.drop_frame(frame);
				self.env.stack.push(JsValue::new_bool(result));
			}
			Ir::Leave(label) => return Next::Leave(label.offset()),
			Ir::LeaveEnv => {
				let _scope = self.env.heap.new_local_scope();
				
				let scope = self.get_scope().unwrap();
				
				let parent = if let Some(parent) = scope.parent(self.env) {
					*parent.as_value(self.env)
				} else {
					JsValue::new_undefined()
				};
				
				self.locals.set(self.scope, parent);
			}
			Ir::LoadArguments => unimplemented!(),
			Ir::LoadException => {
				if let Some(ref exception) = self.thrown {
					self.env.stack.push(**exception)
				} else {
					panic!("Load exception statement without exception in flight");
				}

				// We clear the exception in flight here. Every catch
				// block begins with a load exception statement so
				// we're guarenteed to correctly clear the exception
				// in flight when we enter one.
				
				self.thrown = None;
			}
			Ir::LoadF64(value) => self.env.stack.push(JsValue::new_number(value)),
			Ir::LoadFalse => self.env.stack.push(JsValue::new_false()),
			Ir::LoadFunction(function) => {
				let _scope = self.env.heap.new_local_scope();
				
				let scope = self.get_scope();
				
				let function = *local_try!(self.env.new_function(function, scope, self.strict));
				self.env.stack.push(function);
			}
			Ir::LoadEnvObject => {
				let _scope = self.env.heap.new_local_scope();
				
				let scope = self.get_scope().unwrap();
				let scope_object = scope.scope_object(self.env).as_value(self.env);
				
				self.env.stack.push(*scope_object);
			}
			Ir::LoadGlobal(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let global = self.env.global.as_value(self.env);
				
				if !global.has_property(self.env, name) {
					return Next::Throw(JsError::new_reference(self.env));
				}
				
				let result = local_try!(global.get(self.env, name));
				
				self.env.stack.push(*result);
			}
			Ir::LoadGlobalObject => {
				let _scope = self.env.heap.new_local_scope();
				
				let global = self.env.global.as_value(self.env);
				
				self.env.stack.push(*global);
			}
			Ir::LoadI32(value) => self.env.stack.push(JsValue::new_number(value as f64)),
			Ir::LoadI64(value) => self.env.stack.push(JsValue::new_number(value as f64)),
			Ir::LoadIndex => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				
				let target = frame.get(0).as_local(&self.env.heap);
				let index = frame.get(1).as_local(&self.env.heap);
				let index = local_try!(self.env.intern_value(index));
				
				let result = local_try!(target.get(self.env, index));
				
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(*result);
			}
			Ir::LoadLifted(index, depth) => {
				let _scope = self.env.heap.new_local_scope();
				
				let scope = self.find_scope(depth, false);
				let result = scope.get(self.env, index as usize);
				self.env.stack.push(*result);
			}
			Ir::LoadLocal(local) => {
				self.env.stack.push(self.locals.get(local.offset()));
			}
			Ir::LoadMissing => unimplemented!(),
			Ir::LoadName(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let result = local_try!(frame.get(0).as_local(&self.env.heap).get(self.env, name));
				self.env.stack.drop_frame(frame);
				self.env.stack.push(*result);
			}
			Ir::LoadNameLit => unimplemented!(),
			Ir::LoadNull => self.env.stack.push(JsValue::new_null()),
			Ir::LoadParam(index) => {
				if index < self.args.args.len() as u32 {
					self.env.stack.push(*self.args.args[index as usize]);
				} else {
					self.env.stack.push(JsValue::new_undefined());
				}
			}
			Ir::LoadRegex(ref pattern, ref modifiers) => unimplemented!(),
			Ir::LoadEnv(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let scope_object = self.find_scope_object(name);
				if !scope_object.has_property(self.env, name) {
					return Next::Throw(JsError::new_reference(self.env));
				}
				
				let value = local_try!(scope_object.get(self.env, name));
				self.env.stack.push(*value);
			}
			Ir::LoadEnvObjectFor(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let scope_object = self.find_scope_object(name);
				
				self.env.stack.push(*scope_object);
			}
			Ir::LoadEnvArguments(depth) => {
				let scope = self.find_scope(depth, true);
				
				let arguments = scope.arguments(self.env).unwrap();
				
				self.env.stack.push(*arguments);
			}
			Ir::LoadString(string) => {
				let _scope = self.env.heap.new_local_scope();
				
				let result = JsString::from_str(self.env, &*self.env.ir.interner().get(string)).as_value(self.env);
				self.env.stack.push(*result);
			}
			Ir::LoadThis => self.env.stack.push(*self.args.this),
			Ir::LoadTrue => self.env.stack.push(JsValue::new_true()),
			Ir::LoadUndefined => self.env.stack.push(JsValue::new_undefined()),
			Ir::Lsh => numeric_bin_op!(self, lsh),
			Ir::Lt => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let result = local_try!(self.env.compare_lt(arg1, arg2));
				self.env.stack.drop_frame(frame);
				self.env.stack.push(JsValue::new_bool(result));
			}
			Ir::Modulus => numeric_bin_op!(self, modulus),
			Ir::Multiply => numeric_bin_op!(self, multiply),
			Ir::Ne => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let result = local_try!(self.env.ne(arg1, arg2));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(result));
			}
			Ir::Negative => numeric_op!(self, negative),
			Ir::New(count) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(count as usize + 1);
				let mut args = Vec::new();
				for i in 0..count as usize {
					args.push(frame.get(i + 1).as_local(&self.env.heap));
				}
				
				let constructor = frame.get(0).as_local(&self.env.heap);
				
				let result = local_try!(constructor.construct(self.env, args));
				
				self.env.stack.drop_frame(frame);
				self.env.stack.push(*result);
			}
			Ir::NewArguments => {
				let _scope = self.env.heap.new_local_scope();
					
				let result = local_try!(self.env.new_arguments(&self.args, self.strict)).as_value(self.env);
				self.env.stack.push(*result);
			}
			Ir::NewArray => {
				let _scope = self.env.heap.new_local_scope();
					
				let result = self.env.new_array().as_value(self.env);
				self.env.stack.push(*result);
			}
			Ir::NewObject => {
				let _scope = self.env.heap.new_local_scope();
					
				let result = self.env.new_object().as_value(self.env);
				self.env.stack.push(*result);
			}
			Ir::NextIter(local, label) => {
				let _scope = self.env.heap.new_local_scope();
				
				let mut iterator = self.locals.get(local.offset()).unwrap_iterator().as_local(&self.env.heap);
				
				if iterator.next(self.env) {
					self.ip  = label.offset();
					return Next::Next;
				}
			}
			Ir::Not => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let arg = frame.get(0).as_local(&self.env.heap);
				let result = self.env.logical_not(arg);
				self.env.stack.drop_frame(frame);
				self.env.stack.push(*result);
			}
			Ir::Pick(depth) => {
				let frame = self.env.stack.create_frame(depth as usize + 1);
				self.env.stack.push(frame.get(0));
			}
			Ir::Pop => {
				let frame = self.env.stack.create_frame(1);
				self.env.stack.drop_frame(frame);
			}
			Ir::Positive => numeric_op!(self, positive),
			Ir::Return => {
				let frame = self.env.stack.create_frame(1);
				let result = frame.get(0);
				self.env.stack.drop_frame(frame);
				return Next::Return(result);
			}
			Ir::Rsh => numeric_bin_op!(self, rsh),
			Ir::RshZeroFill => numeric_bin_op!(self, unsigned_rsh),
			Ir::StoreIndex => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(3);
				let mut target = frame.get(0).as_local(&self.env.heap);
				let index = frame.get(1).as_local(&self.env.heap);
				let value = frame.get(2).as_local(&self.env.heap);

				let index = local_try!(self.env.intern_value(index));
				
				local_try!(target.put(self.env, index, value, self.strict));
				
				self.env.stack.drop_frame(frame);
			}
			Ir::StoreIndexUnchecked => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(3);
				
				let mut target = frame.get(0).as_local(&self.env.heap);
				let index = frame.get(1).as_local(&self.env.heap);
				let value = frame.get(2).as_local(&self.env.heap);

				let index = local_try!(self.env.intern_value(index));

				local_try!(target.define_own_property(self.env, index, JsDescriptor::new_simple_value(value), true));
				
				self.env.stack.drop_frame(frame);
			}
			Ir::StoreLifted(index, depth) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				
				let mut scope = self.find_scope(depth, false);
				let result = scope.set(index as usize, frame.get(0).as_local(&self.env.heap));
				
				self.env.stack.drop_frame(frame);
			}
			Ir::StoreLocal(local) => {
				let frame = self.env.stack.create_frame(1);
				self.locals.set(local.offset(), frame.get(0));
				self.env.stack.drop_frame(frame);
			}
			Ir::StoreName(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let mut target = frame.get(0).as_local(&self.env.heap);
				let value = frame.get(1).as_local(&self.env.heap);
				local_try!(target.put(self.env, name, value, self.strict));
				
				self.env.stack.drop_frame(frame);
			}
			Ir::StoreNameUnchecked(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let mut target = frame.get(0).as_local(&self.env.heap);
				let value = frame.get(1).as_local(&self.env.heap);
				
				local_try!(target.define_own_property(self.env, name, JsDescriptor::new_simple_value(value), true));
				
				self.env.stack.drop_frame(frame);
			}
			Ir::StoreGetterUnchecked(function) => unimplemented!(),
			Ir::StoreNameGetterUnchecked(name, function) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let mut object = frame.get(0).as_local(&self.env.heap);
				
				let scope = self.get_scope();
				
				let function = local_try!(self.env.new_function(function, scope, self.strict));
				
				local_try!(object.define_own_property(self.env, name, JsDescriptor::new_simple_accessor(Some(function), None), true));
				
				self.env.stack.drop_frame(frame);
			}
			Ir::StoreEnv(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let value = frame.get(0).as_local(&self.env.heap);
				let mut scope_object = self.find_scope_object(name);
				
				local_try!(scope_object.put(self.env, name, value, self.strict));
				
				self.env.stack.drop_frame(frame);
			}
			Ir::StoreEnvArguments => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let arguments = frame.get(0).as_local(&self.env.heap);
				
				let mut scope = self.get_scope().unwrap();
				scope.set_arguments(arguments);
				
				self.env.stack.drop_frame(frame);
			}
			Ir::StoreGlobal(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let value = frame.get(0).as_local(&self.env.heap);
				let mut global = self.env.global.as_value(self.env);
				
				local_try!(global.put(self.env, name, value, self.strict));
				
				self.env.stack.drop_frame(frame);
			}
			Ir::StoreSetterUnchecked(function) => unimplemented!(),
			Ir::StoreNameSetterUnchecked(name, function) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let mut object = frame.get(0).as_local(&self.env.heap);
				
				let scope = self.get_scope();
				
				let function = local_try!(self.env.new_function(function, scope, self.strict));
				
				local_try!(object.define_own_property(self.env, name, JsDescriptor::new_simple_accessor(None, Some(function)), true));
				
				self.env.stack.drop_frame(frame);
			}
			Ir::StoreParam(index)  => {
				let _scope = self.env.heap.new_local_scope();
				
				let index = index as usize;
				
				while self.args.args.len() <= index {
					self.args.args.push(JsValue::new_undefined().as_local(&self.env.heap));
				}
				
				let frame = self.env.stack.create_frame(1);
				let value = frame.get(0).as_local(&self.env.heap);
				self.env.stack.drop_frame(frame);
				
				self.args.args[index] = value;
			}
			Ir::StrictEq => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let result = self.env.strict_eq(arg1, arg2);
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(result));
			}
			Ir::StrictNe => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(&self.env.heap);
				let arg2 = frame.get(1).as_local(&self.env.heap);
				let result = self.env.strict_eq(arg1, arg2);
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(!result));
			}
			Ir::Subtract => numeric_bin_op!(self, subtract),
			Ir::Swap => {
				let frame = self.env.stack.create_frame(2);
				let tmp = frame.get(0);
				frame.set(0, frame.get(1));
				frame.set(1, tmp);
			}
			Ir::Throw => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let error = frame.get(0).as_local(&self.env.heap).as_root(&self.env.heap);
				self.env.stack.drop_frame(frame);
				
				return Next::Throw(JsError::Runtime(error));
			}
			Ir::ToBoolean => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let arg = frame.get(0).as_local(&self.env.heap);
				let result = arg.to_boolean();
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(result));
			}
			Ir::ToNumber => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let arg = frame.get(0).as_local(&self.env.heap);
				let result = local_try!(arg.to_number(self.env));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_number(result));
			}
			Ir::ToPropertyKey => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let arg = frame.get(0).as_local(&self.env.heap);
				let result = local_try!(arg.to_primitive(self.env, JsPreferredType::String));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(*result);
			}
			Ir::Typeof => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let arg = frame.get(0).as_local(&self.env.heap);
				let result = self.env.type_of(arg);
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_string(result.as_ptr()));
			}
			Ir::TypeofName(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let base = frame.get(0).as_local(&self.env.heap);
				let result = if base.is_undefined() {
					JsString::from_str(self.env, "undefined")
				} else {
					let arg = local_try!(base.get(self.env, name));
					self.env.type_of(arg)
				};
				
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_string(result.as_ptr()));
			}
			Ir::TypeofIndex => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let base = frame.get(0).as_local(&self.env.heap);
				let result = if base.is_undefined() {
					JsString::from_str(self.env, "undefined")
				} else {
					let index = frame.get(1).as_local(&self.env.heap);
					let index = local_try!(self.env.intern_value(index));
					
					let arg = local_try!(base.get(self.env, index));
					self.env.type_of(arg)
				};
				
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_string(result.as_ptr()));
			}
			Ir::ValidateMemberTarget => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let target = frame.get(0).as_local(&self.env.heap);
				
				let invalid = target.is_null() || target.is_undefined();
				
				self.env.stack.drop_frame(frame);
				
				if invalid {
					return Next::Throw(JsError::new_type(self.env, ::errors::TYPE_INVALID));
				}
			}
		}
		
		self.ip += 1;
		
		Next::Next
	}
	
	fn get_scope(&self) -> Option<Local<JsScope>> {
		let scope = self.locals.get(self.scope);
		if scope.is_undefined() {
			return None;
		}
		
		Some(scope.unwrap_scope().as_local(&self.env.heap))
	}
	
	fn find_scope(&self, mut depth: u32, root: bool) -> Local<JsScope> {
		let mut scope = self.get_scope().unwrap();
		
		// Skip over non-root scopes.
		
		while root && scope.arguments(self.env).is_none() {
			scope = scope.parent(self.env).unwrap();
		}
		
		if !self.built_scope {
			depth -= 1;
		}
		
		while depth > 0 {
			scope = scope.parent(self.env).unwrap();
			
			// Skip over non-root scopes.
			
			while root && scope.arguments(self.env).is_none() {
				scope = scope.parent(self.env).unwrap();
			}
			
			depth -= 1;
		}
		
		scope
	}
	
	fn find_scope_object(&self, name: Name) -> Local<JsValue> {
		if let Some(mut scope) = self.get_scope() {
			loop {
				let object = scope.scope_object(self.env);
				
				if object.get_own_property(self.env, name).is_some() {
					return object.as_value(self.env);
				}
				
				if let Some(parent) = scope.parent(self.env) {
					scope = parent;
				} else {
					break;
				}
			}
		}
		
		self.env.global.as_value(self.env)
	}
	
	fn is_any_scope_object(&self, target: Local<JsValue>) -> bool {
		if target.ty() != JsType::Object {
			false
		} else {
			if let Some(mut scope) = self.get_scope() {
				loop {
					let object = scope.scope_object(self.env);
					
					if object.as_ptr() == target.unwrap_object() {
						return true;
					}
					
					if let Some(parent) = scope.parent(self.env) {
						scope = parent;
					} else {
						break;
					}
				}
			}
			
			self.env.global.as_ptr() == target.unwrap_object()
		}
	}
	
	fn call(&mut self, count: u32, mode: CallMode) -> JsResult<()> {
		let _scope = self.env.heap.new_local_scope();
		
		let offset = if mode == CallMode::This { 2 } else { 1 };
		
		let frame = self.env.stack.create_frame(count as usize + offset);
		let mut args = Vec::new();
		for i in 0..count as usize {
			args.push(frame.get(i + offset).as_local(&self.env.heap));
		}
		
		let this = if mode == CallMode::This {
			frame.get(0).as_local(&self.env.heap)
		} else {
			JsValue::new_undefined().as_local(&self.env.heap)
		};
		let function = frame.get(offset - 1).as_local(&self.env.heap);
		
		// If this is a scoped call (i.e. eval) we need to double check to make
		// sure that this actually is a direct eval call. Otherwise the
		// scope is not inherited.
		
		let is_eval = if function.ty() == JsType::Object && mode == CallMode::Eval {
			let global = self.env.global.as_local(&self.env.heap);
			if let Ok(eval) = global.get(&mut self.env, name::EVAL) {
				function.unwrap_object() == eval.unwrap_object()
			} else {
				false
			}
		} else {
			false
		};
		
		// If this actually is an eval, we go directly into eval. Otherwise
		// we let the normal call mechanism handle the call.
		
		let result = if is_eval {
			let js = if args.len() > 0 {
				args[0]
			} else {
				JsValue::new_undefined().as_local(&self.env.heap)
			};
			
			if js.ty() != JsType::String {
				*js
			} else {
				let js = js.unwrap_string().as_local(&self.env.heap).to_string();
				let scope = self.get_scope().unwrap();
				
				*try!(self.env.eval_scoped(&js, self.strict, self.args.this, scope, ParseMode::DirectEval)).as_local(&self.env.heap)
			}
		} else {
			*try!(function.call(self.env, this, args, self.strict))
		};
		
		self.env.stack.drop_frame(frame);
		self.env.stack.push(result);
		
		Ok(())
	}
	
	// 11.4.1 The delete Operator
	fn delete(&mut self, name: Name, env: bool) -> JsResult<Option<Next>> {
		let _scope = self.env.heap.new_local_scope();
		
		let frame = self.env.stack.create_frame(1);
		
		let mut target = frame.get(0).as_local(&self.env.heap);
		
		self.env.stack.drop_frame(frame);
		
		if env && self.strict {
			return Ok(Some(Next::Throw(JsError::new_syntax(self.env, ::errors::SYNTAX_CANNOT_RESOLVE_PROPERTY))));
		}
		
		let result = if env && !target.has_property(self.env, name) {
			true
		} else {
			match target.delete(self.env, name, self.strict) {
				Ok(result) => result,
				Err(error) => return Ok(Some(Next::Throw(error)))
			}
		};
		
		self.env.stack.push(JsValue::new_bool(result));
		
		Ok(None)
	}
}
