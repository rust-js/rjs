#![allow(unused_variables)]

use super::{JsEnv, JsValue, JsString, JsItem, JsIterator, JsScope};
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

struct Frame<'a> {
	env: &'a mut JsEnv,
	ip: usize,
	locals: StackFrame,
	this: Option<Local<JsValue>>,
	args: Vec<Local<JsValue>>,
	thrown: Option<Root<JsValue>>,
	strict: bool,
	arguments: Option<usize>,
	parent_scope: Option<usize>,
	scope: Option<usize>
}

impl JsEnv {
	pub fn call_block(&mut self, block: Rc<Block>, this: Option<Local<JsValue>>, args: Vec<Local<JsValue>>, function: &IrFunction, scope: Option<Local<JsValue>>) -> JsResult<JsValue> {
		let mut locals = block.locals.len();
		
		for i in 0..block.locals.len() {
			self.stack.push(JsValue::new_undefined());
		}
		
		let arguments = if function.has_arguments {
			let arguments = self.new_arguments(&args);
			self.stack.push(*try!(arguments));
			locals += 1;
			Some(locals - 1)
		} else {
			None
		};
		
		let parent_scope = if let Some(scope) = scope {
			self.stack.push(*scope);
			locals += 1;
			Some(locals - 1)
		} else {
			None
		};
		
		let scope = if let Some(size) = function.scope {
			let _scope = self.heap.new_local_scope();
			
			let scope = JsScope::new_local(self, size as usize, scope).as_value(self);
			self.stack.push(*scope);
			locals += 1;
			Some(locals - 1)
		} else {
			None
		};
		
		let ir = &block.ir[..];
		let mut leave = None;
		
		let locals = self.stack.create_frame(locals);
		
		let mut frame = Frame {
			env: self,
			ip: 0,
			locals: locals,
			this: this,
			args: args,
			thrown: None,
			strict: function.strict,
			arguments: arguments,
			parent_scope: parent_scope,
			scope: scope
		};
		
		loop {
			debugln!("IP: {}", frame.ip);
			
			let ir = &ir[frame.ip];
			
			match frame.call_stmt(ir) {
				Next::Next => {},
				Next::Return(result) => return Ok(result),
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
								// If the frame has a catch block, let the catch
								// block handle the error. Otherwise it must have
								// a finally block and we enter that.
								
								if let Some(catch) = try_catch.catch {
									frame.ip = catch.start().offset();
									
									found = true;
									break;
								} else if let Some(finally) = try_catch.finally {
									frame.ip = finally.start().offset();
									
									found = true;
									break;
								} else {
									panic!("Expected at least a catch or finally block");
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
						}
					} else {
						panic!("End finaly without pending error or leave");
					}
				}
			}
		}
	}
}

impl<'a> Frame<'a> {
	fn find_scope(&self, depth: u32) -> Local<JsScope> {
		if depth == 0 {
			self.locals.get(self.scope.unwrap()).as_scope(self.env)
		} else {
			let mut scope = self.locals.get(self.parent_scope.unwrap()).as_scope(self.env);
			let mut depth = depth - 1;
			while depth > 0 {
				scope = scope.parent(self.env).unwrap().as_scope(self.env);
				depth -= 1;
			}
			scope
		}
	}
	
	#[inline(always)]
	fn call_stmt(&mut self, ir: &Ir) -> Next {
		match ir {
			&Ir::Add => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self.env);
				let arg2 = frame.get(1).as_local(self.env);
				let result = local_try!(self.env.add(arg1, arg2));
				self.env.stack.drop_frame(frame);
				self.env.stack.push(*result);
			}
			&Ir::BitAnd => { unimplemented!(); },
			&Ir::BitNot => { unimplemented!(); },
			&Ir::BitOr => { unimplemented!(); },
			&Ir::BitXOr => { unimplemented!(); },
			&Ir::Call(count) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(count as usize + 2);
				let mut args = Vec::new();
				for i in 0..count as usize {
					args.push(frame.get(i + 2).as_local(self.env));
				}
				
				let this = frame.get(0).as_local(self.env);
				let function = frame.get(1).as_local(self.env);
				let function = self.env.get_value(function);
				
				let result = local_try!(function.call(self.env, this, args, self.strict));
				
				self.env.stack.drop_frame(frame);
				self.env.stack.push(*result);
			}
			&Ir::CastObject => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let arg = frame.get(0).as_local(self.env);
				let result = local_try!(arg.to_object(self.env));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(*result);
			}
			&Ir::CurrentIter(local) => {
				let _scope = self.env.heap.new_local_scope();
				
				let iterator = self.locals.get(local.offset()).as_iterator(self.env);
				let name = iterator.current();
				
				let result = if let Some(index) = name.index() {
					JsString::from_str(self.env, &index.to_string()).as_value(self.env)
				} else {
					JsString::from_str(self.env, &*self.env.ir.interner().get(name)).as_value(self.env)
				};
				
				self.env.stack.push(*result);
			},
			&Ir::Debugger => { unimplemented!(); },
			&Ir::Delete => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_true());
			}
			&Ir::DeleteIndex => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				
				let index = frame.get(1).as_local(self.env);
				let index = local_try!(self.env.intern_value(index));
				
				let result = local_try!(frame.get(0).as_local(self.env).delete(self.env, index, true));
				
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(result));
			}
			&Ir::DeleteName(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				
				let result = local_try!(frame.get(0).as_local(self.env).delete(self.env, name, true));
				
				self.env.stack.drop_frame(frame);
				self.env.stack.push(JsValue::new_bool(result));
			}
			&Ir::DeleteNameJump(name, label) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				
				let mut object = frame.get(0).as_local(self.env);
				
				if object.has_property(self.env, name) {
					let result = local_try!(object.delete(self.env, name, true));
					
					self.env.stack.drop_frame(frame);
					self.env.stack.push(JsValue::new_bool(result));
					
					self.ip = label.offset();
					return Next::Next;
				}
				
				self.env.stack.drop_frame(frame);
			}
			&Ir::Divide => { unimplemented!(); },
			&Ir::Dup => {
				let frame = self.env.stack.create_frame(1);
				self.env.stack.push(frame.get(0));
			}
			&Ir::EndFinally => return Next::EndFinally,
			&Ir::EndIter(local) => { /* no-op */ },
			&Ir::Eq => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self.env);
				let arg2 = frame.get(1).as_local(self.env);
				let result = local_try!(self.env.eq(arg1, arg2));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(result));
			}
			&Ir::Ge => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self.env);
				let arg2 = frame.get(1).as_local(self.env);
				let result = local_try!(self.env.compare_ge(arg1, arg2));
				self.env.stack.drop_frame(frame);
				self.env.stack.push(JsValue::new_bool(result));
			}
			&Ir::Gt => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self.env);
				let arg2 = frame.get(1).as_local(self.env);
				let result = local_try!(self.env.compare_gt(arg1, arg2));
				self.env.stack.drop_frame(frame);
				self.env.stack.push(JsValue::new_bool(result));
			},
			&Ir::In => { unimplemented!(); },
			&Ir::InstanceOf => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self.env);
				let arg2 = frame.get(1).as_local(self.env);
				let result = local_try!(self.env.instanceof(arg1, arg2));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(*result);
			},
			&Ir::IntoIter(local) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				
				let arg = frame.get(0).as_local(self.env);
				let result = JsIterator::new_local(self.env, arg).as_value(self.env);
				self.locals.set(local.offset(), *result);
				
				self.env.stack.drop_frame(frame);
			},
			&Ir::Jump(label) => {
				self.ip = label.offset();
				return Next::Next;
			}
			&Ir::JumpEq(label) => { unimplemented!(); },
			&Ir::JumpFalse(label) => {
				let frame = self.env.stack.create_frame(1);
				let jump = !frame.get(0).get_bool();
				self.env.stack.drop_frame(frame);
				if jump {
					self.ip  = label.offset();
					return Next::Next;
				}
			}
			&Ir::JumpTrue(label) => {
				let frame = self.env.stack.create_frame(1);
				let jump = frame.get(0).get_bool();
				self.env.stack.drop_frame(frame);
				if jump {
					self.ip  = label.offset();
					return Next::Next;
				}
			}
			&Ir::Le => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self.env);
				let arg2 = frame.get(1).as_local(self.env);
				let result = local_try!(self.env.compare_le(arg1, arg2));
				self.env.stack.drop_frame(frame);
				self.env.stack.push(JsValue::new_bool(result));
			}
			&Ir::Leave(label) => return Next::Leave(label.offset()),
			&Ir::LoadArguments => self.env.stack.push(self.locals.get(self.arguments.unwrap())),
			&Ir::LoadException => {
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
			&Ir::LoadF64(value) => self.env.stack.push(JsValue::new_number(value)),
			&Ir::LoadFalse => self.env.stack.push(JsValue::new_false()),
			&Ir::LoadFunction(function) => {
				let _scope = self.env.heap.new_local_scope();
				
				let scope = if let Some(index) = self.scope {
					Some(self.locals.get(index).as_local(self.env))
				} else {
					None
				};
				
				let function = *local_try!(self.env.new_function(function, scope));
				self.env.stack.push(function);
			}
			&Ir::LoadGlobal(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let global = self.env.global.as_local(self.env);
				if !global.has_property(self.env, name) {
					return Next::Throw(JsError::new_reference(self.env));
				}
				let value = local_try!(global.get(self.env, name));
				self.env.stack.push(*value);
			}
			&Ir::LoadGlobalThis => {
				self.env.stack.push(JsValue::new_object(self.env.global.as_ptr()));
			}
			&Ir::LoadI32(value) => self.env.stack.push(JsValue::new_number(value as f64)),
			&Ir::LoadI64(value) => self.env.stack.push(JsValue::new_number(value as f64)),
			&Ir::LoadIndex => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				
				let index = frame.get(1).as_local(self.env);
				let index = local_try!(self.env.intern_value(index));
				
				let result = local_try!(frame.get(0).as_local(self.env).get(self.env, index));
				
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(*result);
			}
			&Ir::LoadLifted(index, depth) => {
				let _scope = self.env.heap.new_local_scope();
				
				let scope = self.find_scope(depth);
				let result = scope.get(self.env, index as usize);
				self.env.stack.push(*result);
			},
			&Ir::LoadLocal(local) => {
				self.env.stack.push(self.locals.get(local.offset()));
			},
			&Ir::LoadMissing => { unimplemented!(); },
			&Ir::LoadName(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let result = local_try!(frame.get(0).as_local(self.env).get(self.env, name));
				self.env.stack.drop_frame(frame);
				self.env.stack.push(*result);
			}
			&Ir::LoadNameJump(name, label) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let object = frame.get(0).as_local(self.env);
				
				if object.has_property(self.env, name) {
					let result = local_try!(object.get(self.env, name));
					self.env.stack.drop_frame(frame);
					self.env.stack.push(*result);
					self.ip = label.offset();
					return Next::Next;
				}
				
				self.env.stack.drop_frame(frame);
			}
			&Ir::LoadNameLit => { unimplemented!(); },
			&Ir::LoadNull => self.env.stack.push(JsValue::new_null()),
			&Ir::LoadParam(index) => {
				if index < self.args.len() as u32 {
					self.env.stack.push(*self.args[index as usize]);
				} else {
					self.env.stack.push(JsValue::new_undefined());
				}
			}
			&Ir::LoadRegex(ref pattern, ref modifiers) => { unimplemented!(); },
			&Ir::LoadString(string) => {
				let _scope = self.env.heap.new_local_scope();
				
				let result = JsString::from_str(self.env, &*self.env.ir.interner().get(string)).as_value(self.env);
				self.env.stack.push(*result);
			}
			&Ir::LoadThis => {
				if let Some(ref this) = self.this {
					self.env.stack.push(**this);
				} else {
					self.env.stack.push(JsValue::new_object(self.env.global.as_ptr()));
				}
			},
			&Ir::LoadTrue => self.env.stack.push(JsValue::new_true()),
			&Ir::LoadUndefined => self.env.stack.push(JsValue::new_undefined()),
			&Ir::Lsh => { unimplemented!(); },
			&Ir::Lt => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self.env);
				let arg2 = frame.get(1).as_local(self.env);
				let result = local_try!(self.env.compare_gt(arg1, arg2));
				self.env.stack.drop_frame(frame);
				self.env.stack.push(JsValue::new_bool(result));
			},
			&Ir::Modulus => { unimplemented!(); },
			&Ir::Multiply => { unimplemented!(); },
			&Ir::Ne => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self.env);
				let arg2 = frame.get(1).as_local(self.env);
				let result = local_try!(self.env.ne(arg1, arg2));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(result));
			},
			&Ir::Negative => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let arg = frame.get(0).as_local(self.env);
				let result = local_try!(self.env.negative(arg));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_number(result));
			},
			&Ir::New(count) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(count as usize + 1);
				let mut args = Vec::new();
				for i in 0..count as usize {
					args.push(frame.get(i + 1).as_local(self.env));
				}
				
				let constructor = frame.get(0).as_local(self.env);
				
				let result = local_try!(constructor.construct(self.env, args));
				
				self.env.stack.drop_frame(frame);
				self.env.stack.push(*result);
			},
			&Ir::NewArray => {
				let _scope = self.env.heap.new_local_scope();
					
				let result = self.env.new_array().as_value(self.env);
				self.env.stack.push(*result);
			}
			&Ir::NewObject => {
				let _scope = self.env.heap.new_local_scope();
					
				let result = self.env.new_object().as_value(self.env);
				self.env.stack.push(*result);
			}
			&Ir::NextIter(local, label) => {
				let _scope = self.env.heap.new_local_scope();
				
				let mut iterator = self.locals.get(local.offset()).as_iterator(self.env);
				
				if iterator.next(self.env) {
					self.ip  = label.offset();
					return Next::Next;
				}
			},
			&Ir::Not => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let arg = frame.get(0).as_local(self.env);
				let result = self.env.logical_not(arg);
				self.env.stack.drop_frame(frame);
				self.env.stack.push(*result);
			},
			&Ir::Pick(depth) => {
				let frame = self.env.stack.create_frame(depth as usize + 1);
				self.env.stack.push(frame.get(0));
			},
			&Ir::Pop => {
				let frame = self.env.stack.create_frame(1);
				self.env.stack.drop_frame(frame);
			},
			&Ir::Positive => { unimplemented!(); },
			&Ir::Return => {
				let frame = self.env.stack.create_frame(1);
				let result = frame.get(0);
				self.env.stack.drop_frame(frame);
				return Next::Return(result);
			}
			&Ir::Rsh => { unimplemented!(); },
			&Ir::RshZeroFill => { unimplemented!(); },
			&Ir::StoreArguments => { unimplemented!(); },
			&Ir::StoreGlobal(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let value = frame.get(0).as_local(self.env);
				local_try!(self.env.global.as_local(self.env).put(self.env, name, value, self.strict));
				self.env.stack.drop_frame(frame);
			}
			&Ir::StoreIndex => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(3);
				
				let index = frame.get(1).as_local(self.env);
				let index = local_try!(self.env.intern_value(index));
				
				let value = frame.get(2).as_local(self.env);
				local_try!(frame.get(0).as_local(self.env).put(self.env, index, value, self.strict));
				
				self.env.stack.drop_frame(frame);
			}
			&Ir::StoreLifted(index, depth) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				
				let mut scope = self.find_scope(depth);
				let result = scope.set(index as usize, frame.get(0).as_local(self.env));
				
				self.env.stack.drop_frame(frame);
			},
			&Ir::StoreLocal(local) => {
				let frame = self.env.stack.create_frame(1);
				self.locals.set(local.offset(), frame.get(0));
				self.env.stack.drop_frame(frame);
			}
			&Ir::StoreName(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let value = frame.get(1).as_local(self.env);
				local_try!(frame.get(0).as_local(self.env).put(self.env, name, value, self.strict));
				self.env.stack.drop_frame(frame);
			}
			&Ir::StoreNameJump(name, label) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let mut object = frame.get(0).as_local(self.env);
				
				if object.has_property(self.env, name) {
					let value = frame.get(1).as_local(self.env);
					local_try!(object.put(self.env, name, value, self.strict));
					self.env.stack.drop_frame(frame);
					self.ip = label.offset();
					return Next::Next;
				}
				
				self.env.stack.drop_frame(frame);
			}
			&Ir::StoreGetter(function) => { unimplemented!(); },
			&Ir::StoreNameGetter(name, function) => { unimplemented!(); },
			&Ir::StoreSetter(function) => { unimplemented!(); },
			&Ir::StoreNameSetter(name, function) => { unimplemented!(); },
			&Ir::StoreParam(u32) => { unimplemented!(); },
			&Ir::StrictEq => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self.env);
				let arg2 = frame.get(1).as_local(self.env);
				let result = self.env.strict_eq(arg1, arg2);
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(result));
			},
			&Ir::StrictNe => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self.env);
				let arg2 = frame.get(1).as_local(self.env);
				let result = self.env.strict_eq(arg1, arg2);
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(!result));
			},
			&Ir::Subtract => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let arg1 = frame.get(0).as_local(self.env);
				let arg2 = frame.get(1).as_local(self.env);
				let result = local_try!(self.env.subtract(arg1, arg2));
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_number(result));
			},
			&Ir::Swap => { unimplemented!(); },
			&Ir::Throw => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let error = Root::from_local(&self.env.heap, frame.get(0).as_local(self.env));
				self.env.stack.drop_frame(frame);
				
				return Next::Throw(JsError::Runtime(error));
			}
			&Ir::ToBoolean => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let arg = frame.get(0).as_local(self.env);
				let result = arg.to_boolean();
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_bool(result));
			}
			&Ir::Typeof => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let arg = frame.get(0).as_local(self.env);
				let result = self.env.type_of(arg);
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_string(result.as_ptr()));
			}
			&Ir::TypeofName(name) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let base = frame.get(0).as_local(self.env);
				let result = if base.is_undefined() {
					JsString::from_str(self.env, "undefined")
				} else {
					let arg = local_try!(base.get(self.env, name));
					self.env.type_of(arg)
				};
				
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_string(result.as_ptr()));
			}
			&Ir::TypeofNameJump(name, label) => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(1);
				let object = frame.get(0).as_local(self.env);
				
				if object.has_property(self.env, name) {
					let arg = local_try!(object.get(self.env, name));
					let result = self.env.type_of(arg);
					
					self.env.stack.drop_frame(frame);
					
					self.env.stack.push(JsValue::new_string(result.as_ptr()));
					
					self.ip = label.offset();
					return Next::Next;
				}
				
				self.env.stack.drop_frame(frame);
			}
			&Ir::TypeofIndex => {
				let _scope = self.env.heap.new_local_scope();
				
				let frame = self.env.stack.create_frame(2);
				let base = frame.get(0).as_local(self.env);
				let result = if base.is_undefined() {
					JsString::from_str(self.env, "undefined")
				} else {
					let index = frame.get(1).as_local(self.env);
					let index = local_try!(self.env.intern_value(index));
					
					let arg = local_try!(base.get(self.env, index));
					self.env.type_of(arg)
				};
				
				self.env.stack.drop_frame(frame);
				
				self.env.stack.push(JsValue::new_string(result.as_ptr()));
			}
		}
		
		self.ip += 1;
		
		Next::Next
	}
}
