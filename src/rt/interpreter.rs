use rt::{JsEnv, JsRawValue, JsValue, JsString, JsItem, JsIterator, JsScope, JsType, JsArgs};
use rt::{JsDescriptor, JsPreferredType, JsFnMode, JsHandle};
use rt::{GC_VALUE};
use gc::*;
use ::{JsResult, JsError};
use ir::IrFunction;
use ir::builder::{Block, Ir};
use std::rc::Rc;
use rt::stack::StackFrame;
use syntax::Name;
use syntax::ast::{CastType, ScopeType};
use syntax::token::name;
use syntax::parser::ParseMode;

const CALL_PROLOG : usize = 2;

enum Next {
    Next,
    Return,
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

macro_rules! numeric_op {
    ( $frame:expr , $method:ident ) => { {
        let _scope = $frame.env.new_local_scope();
        
        let frame = $frame.env.stack.create_frame(1);
        let arg = frame.get(&$frame.env, 0);
        let result = local_try!($frame.env.$method(arg));
        $frame.env.stack.drop_frame(frame);
        
        $frame.env.stack.push(JsRawValue::new_number(result));
    } }
}

macro_rules! numeric_bin_op {
    ( $frame:expr , $method:ident ) => { {
        let _scope = $frame.env.new_local_scope();
        
        let frame = $frame.env.stack.create_frame(2);
        let arg1 = frame.get(&$frame.env, 0);
        let arg2 = frame.get(&$frame.env, 1);
        let result = local_try!($frame.env.$method(arg1, arg2));
        $frame.env.stack.drop_frame(frame);
        
        $frame.env.stack.push(JsRawValue::new_number(result));
    } }
}

struct Frame<'a> {
    env: &'a mut JsEnv,
    ip: usize,
    locals: StackFrame,
    args: JsArgs,
    thrown: Option<Root<JsRawValue>>,
    strict: bool,
    scope: usize,
    built_scope: bool
}

impl JsEnv {
    pub fn call_block(&mut self, block: Rc<Block>, args: JsArgs, function: &IrFunction, scope: Local<JsScope>) -> JsResult<()> {
        // Ensure that we have enough arguments on the stack. We will have the number
        // of arguments already on the stack that the caller pushed. However,
        // our load/store param calls will write directly into these locations.
        // The problem is when the caller pushed too few arguments. When this is
        // the case, we'll be missing them on the stack. Instead we're adding them
        // here now.
        
        for _ in args.argc..function.args as usize {
            self.stack.push(JsRawValue::new_undefined());
        }
        
        let mut locals = block.locals.len();
        
        for _ in 0..block.locals.len() {
            self.stack.push(JsRawValue::new_undefined());
        }
        
        // Build the environment context.
        
        let scope = {
            let _scope = self.new_local_scope();
            
            let scope = if let ScopeType::Thin(size) = function.build_scope {
                JsScope::new_local_thin(self, size as usize, Some(scope)).as_value()
            } else if function.build_scope == ScopeType::Thick {
                let mut scope_object = self.create_object();
                scope_object.set_prototype(None);
                
                JsScope::new_local_thick(self, scope_object, Some(scope), true).as_value()
            } else {
                scope.as_value()
            };
            
            self.stack.push(scope.as_raw());
            
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
                Next::Return => {
                    return Ok(());
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
                                panic!("expected either a catch or finally block");
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
                        frame.env.stack.drop_frame(frame.args.frame);
                        
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
                            panic!("cannot find try/catch frame of leave");
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
                            frame.env.stack.drop_frame(frame.args.frame);
                            
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
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let result = local_try!(self.env.add(arg1, arg2));
                
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(result.as_raw());
            }
            Ir::BitAnd => numeric_bin_op!(self, bit_and),
            Ir::BitNot => numeric_op!(self, bit_not),
            Ir::BitOr => numeric_bin_op!(self, bit_or),
            Ir::BitXOr => numeric_bin_op!(self, bit_xor),
            Ir::Call(count) => local_try!(self.call(count, false)),
            Ir::CallEval(count) => local_try!(self.call(count, true)),
            Ir::CurrentIter(local) => {
                let _scope = self.env.new_local_scope();
                
                let iterator = self.locals.get(&self.env, local.offset()).unwrap_iterator();
                let name = iterator.current();
                
                let result = if let Some(index) = name.index() {
                    JsString::from_str(self.env, &index.to_string()).as_value()
                } else {
                    JsString::from_str(self.env, &*self.env.ir.interner().get(name)).as_value()
                };
                
                self.env.stack.push(result.as_raw());
            }
            Ir::Debugger => {
                // Not implemented. Debugger support is not yet part of rjs.
            }
            Ir::DeleteIndex => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                
                let mut target = frame.get(&self.env, 0);
                let index = frame.get(&self.env, 1);
                let index = local_try!(self.env.intern_value(index));
                
                let result = local_try!(target.delete(self.env, index, self.strict));
                
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(JsRawValue::new_bool(result));
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
                self.env.stack.push(frame.raw_get(0));
            }
            Ir::EndFinally => return Next::EndFinally,
            Ir::EndIter(..) => { /* no-op */ }
            Ir::EnterEnv => {
                let _scope = self.env.new_local_scope();
                
                let mut scope_object = self.env.create_object();
                scope_object.set_prototype(None);
                
                let scope = JsScope::new_local_thick(
                    self.env,
                    scope_object,
                    self.get_scope(),
                    false
                );
                
                self.locals.set(self.scope, scope.as_value().as_raw());
            }
            Ir::EnterWithEnv => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                
                let scope_object = frame.get(&self.env, 0);
                let scope_object = local_try!(scope_object.to_object(&mut self.env));
                
                let scope = JsScope::new_local_thick(
                    self.env,
                    scope_object.unwrap_object(),
                    self.get_scope(),
                    false
                );
                
                self.locals.set(self.scope, scope.as_value().as_raw());
                
                self.env.stack.drop_frame(frame);
            }
            Ir::Eq => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let result = local_try!(self.env.eq(arg1, arg2));
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(JsRawValue::new_bool(result));
            }
            Ir::Ge => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let result = local_try!(self.env.compare_ge(arg1, arg2));
                self.env.stack.drop_frame(frame);
                self.env.stack.push(JsRawValue::new_bool(result));
            }
            Ir::Gt => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let result = local_try!(self.env.compare_gt(arg1, arg2));
                self.env.stack.drop_frame(frame);
                self.env.stack.push(JsRawValue::new_bool(result));
            }
            Ir::In => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let result = local_try!(self.env.in_(arg1, arg2));
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(result.as_raw());
            }
            Ir::InitEnvName(name) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let mut target = frame.get(&self.env, 0);
                let value = frame.get(&self.env, 1);
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
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let result = local_try!(self.env.instanceof(arg1, arg2));
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(result.as_raw());
            }
            Ir::IntoIter(local) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                
                let arg = frame.get(&self.env, 0);
                let result = JsIterator::new_local(self.env, arg).as_value();
                self.locals.set(local.offset(), result.as_raw());
                
                self.env.stack.drop_frame(frame);
            }
            Ir::Jump(label) => {
                self.ip = label.offset();
                return Next::Next;
            }
            Ir::JumpStrictEq(label) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let jump = self.env.strict_eq(arg1, arg2);
                self.env.stack.drop_frame(frame);
                
                if jump {
                    self.ip  = label.offset();
                    return Next::Next;
                }
            }
            Ir::JumpFalse(label) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let jump = !frame.get(&self.env, 0).to_boolean();
                self.env.stack.drop_frame(frame);
                if jump {
                    self.ip  = label.offset();
                    return Next::Next;
                }
            }
            Ir::JumpTrue(label) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let jump = frame.get(&self.env, 0).to_boolean();
                self.env.stack.drop_frame(frame);
                if jump {
                    self.ip  = label.offset();
                    return Next::Next;
                }
            }
            Ir::Le => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let result = local_try!(self.env.compare_le(arg1, arg2));
                self.env.stack.drop_frame(frame);
                self.env.stack.push(JsRawValue::new_bool(result));
            }
            Ir::Leave(label) => return Next::Leave(label.offset()),
            Ir::LeaveEnv => {
                let _scope = self.env.new_local_scope();
                
                let scope = self.get_scope().unwrap();
                
                let parent = if let Some(parent) = scope.parent(self.env) {
                    parent.as_value().as_raw()
                } else {
                    JsRawValue::new_undefined()
                };
                
                self.locals.set(self.scope, parent);
            }
            Ir::LoadException => {
                if let Some(ref exception) = self.thrown {
                    self.env.stack.push(**exception)
                } else {
                    panic!("load exception statement without exception in flight");
                }

                // We clear the exception in flight here. Every catch
                // block begins with a load exception statement so
                // we're guarenteed to correctly clear the exception
                // in flight when we enter one.
                
                self.thrown = None;
            }
            Ir::LoadF64(value) => self.env.stack.push(JsRawValue::new_number(value)),
            Ir::LoadFalse => self.env.stack.push(JsRawValue::new_bool(false)),
            Ir::LoadFunction(function) => {
                let _scope = self.env.new_local_scope();
                
                let scope = self.get_scope();
                
                let function = local_try!(self.env.new_function(function, scope, self.strict));
                self.env.stack.push(function.as_raw());
            }
            Ir::LoadEnvObject => {
                let _scope = self.env.new_local_scope();
                
                let scope = self.get_scope().unwrap();
                let scope_object = scope.scope_object(self.env).as_value();
                
                self.env.stack.push(scope_object.as_raw());
            }
            Ir::LoadGlobal(name) => {
                let _scope = self.env.new_local_scope();
                
                let global = self.env.handle(JsHandle::Global).as_value();
                
                if !global.has_property(self.env, name) {
                    return Next::Throw(JsError::new_reference(self.env));
                }
                
                let result = local_try!(global.get(self.env, name));
                
                self.env.stack.push(result.as_raw());
            }
            Ir::LoadI32(value) => self.env.stack.push(JsRawValue::new_number(value as f64)),
            Ir::LoadI64(value) => self.env.stack.push(JsRawValue::new_number(value as f64)),
            Ir::LoadIndex => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                
                let target = frame.get(&self.env, 0);
                let index = frame.get(&self.env, 1);
                let index = local_try!(self.env.intern_value(index));
                
                let result = local_try!(target.get(self.env, index));
                
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(result.as_raw());
            }
            Ir::LoadLifted(index, depth) => {
                let _scope = self.env.new_local_scope();
                
                let scope = self.find_scope(depth, false);
                let result = scope.get(self.env, index as usize);
                
                self.env.stack.push(result.as_raw());
            }
            Ir::LoadLocal(local) => {
                self.env.stack.push(self.locals.raw_get(local.offset()));
            }
            Ir::LoadName(name) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                
                let result = local_try!(frame.get(&self.env, 0).get(self.env, name));
                
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(result.as_raw());
            }
            Ir::LoadNull => self.env.stack.push(JsRawValue::new_null()),
            Ir::LoadParam(index) => self.env.stack.push(self.args.frame.raw_get(index as usize + CALL_PROLOG)),
            Ir::LoadRegex(pattern, flags) => {
                let _scope = self.env.new_local_scope();
                
                let pattern = self.env.ir.interner().get(pattern);
                let pattern = JsString::from_str(self.env, &*pattern).as_value();
                
                let flags = self.env.ir.interner().get(flags);
                let flags = JsString::from_str(self.env, &*flags).as_value();
                
                let regexp = self.env.handle(JsHandle::RegExpClass);
                
                let result = local_try!(regexp.construct(self.env, vec![pattern, flags]));
                
                self.env.stack.push(result.as_raw());
            }
            Ir::LoadEnv(name) => {
                let _scope = self.env.new_local_scope();
                
                let scope_object = local_try!(self.find_scope_object(name, true));
                
                let value = local_try!(scope_object.get(self.env, name));
                self.env.stack.push(value.as_raw());
            }
            Ir::FindEnvObjectFor(name) => {
                let _scope = self.env.new_local_scope();
                
                let scope_object = local_try!(self.find_scope_object(name, false));
                
                self.env.stack.push(scope_object.as_raw());
            }
            Ir::LoadEnvObjectFor(name) => {
                let _scope = self.env.new_local_scope();
                
                let strict = self.strict;
                let scope_object = local_try!(self.find_scope_object(name, strict));
                
                self.env.stack.push(scope_object.as_raw());
            }
            Ir::LoadEnvArguments(depth) => {
                let scope = self.find_scope(depth, true);
                
                let arguments = scope.arguments(self.env).unwrap();
                
                self.env.stack.push(arguments.as_raw());
            }
            Ir::LoadString(string) => {
                let _scope = self.env.new_local_scope();
                
                let result = JsString::from_str(self.env, &*self.env.ir.interner().get(string)).as_value();
                self.env.stack.push(result.as_raw());
            }
            Ir::LoadThis => self.env.stack.push(self.args.raw_this()),
            Ir::LoadTrue => self.env.stack.push(JsRawValue::new_bool(true)),
            Ir::LoadUndefined => self.env.stack.push(JsRawValue::new_undefined()),
            Ir::Lsh => numeric_bin_op!(self, lsh),
            Ir::Lt => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let result = local_try!(self.env.compare_lt(arg1, arg2));
                
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(JsRawValue::new_bool(result));
            }
            Ir::Modulus => numeric_bin_op!(self, modulus),
            Ir::Multiply => numeric_bin_op!(self, multiply),
            Ir::Ne => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let result = local_try!(self.env.ne(arg1, arg2));
                
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(JsRawValue::new_bool(result));
            }
            Ir::Negative => numeric_op!(self, negative),
            Ir::New(count) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(count as usize + CALL_PROLOG);
                let args = JsArgs {
                    frame: frame,
                    argc: count as usize
                };
                
                local_try!(self.env.construct(args));
            }
            Ir::NewArguments => {
                let _scope = self.env.new_local_scope();
                    
                let result = local_try!(self.env.new_arguments(&self.args, self.strict)).as_value();
                self.env.stack.push(result.as_raw());
            }
            Ir::NewArray => {
                let _scope = self.env.new_local_scope();
                    
                let result = self.env.create_array().as_value();
                self.env.stack.push(result.as_raw());
            }
            Ir::NewObject => {
                let _scope = self.env.new_local_scope();
                    
                let result = self.env.create_object().as_value();
                self.env.stack.push(result.as_raw());
            }
            Ir::NextIter(local, label) => {
                let _scope = self.env.new_local_scope();
                
                let mut iterator = self.locals.get(&self.env, local.offset()).unwrap_iterator();
                
                if iterator.next(self.env) {
                    self.ip  = label.offset();
                    return Next::Next;
                }
            }
            Ir::Not => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let arg = frame.get(&self.env, 0);
                let result = self.env.logical_not(arg);
                self.env.stack.drop_frame(frame);
                self.env.stack.push(result.as_raw());
            }
            Ir::Pick(depth) => {
                let frame = self.env.stack.create_frame(depth as usize + 1);
                self.env.stack.push(frame.raw_get(0));
            }
            Ir::Pop => {
                let frame = self.env.stack.create_frame(1);
                self.env.stack.drop_frame(frame);
            }
            Ir::Positive => numeric_op!(self, positive),
            Ir::Return => {
                let frame = self.env.stack.create_frame(1);
                let result = frame.raw_get(0);
                self.env.stack.drop_frame(self.args.frame);
                self.env.stack.push(result);
                return Next::Return;
            }
            Ir::Rsh => numeric_bin_op!(self, rsh),
            Ir::RshZeroFill => numeric_bin_op!(self, unsigned_rsh),
            Ir::StoreIndex => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(3);
                let mut target = frame.get(&self.env, 0);
                let index = frame.get(&self.env, 1);
                let value = frame.get(&self.env, 2);

                let index = local_try!(self.env.intern_value(index));
                
                local_try!(target.put(self.env, index, value, self.strict));
                
                self.env.stack.drop_frame(frame);
            }
            Ir::StoreIndexUnchecked => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(3);
                
                let mut target = frame.get(&self.env, 0);
                let index = frame.get(&self.env, 1);
                let value = frame.get(&self.env, 2);

                let index = local_try!(self.env.intern_value(index));

                local_try!(target.define_own_property(self.env, index, JsDescriptor::new_simple_value(value), true));
                
                self.env.stack.drop_frame(frame);
            }
            Ir::StoreLifted(index, depth) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                
                let mut scope = self.find_scope(depth, false);
                scope.set(index as usize, frame.get(&self.env, 0));
                
                self.env.stack.drop_frame(frame);
            }
            Ir::StoreLocal(local) => {
                let frame = self.env.stack.create_frame(1);
                self.locals.set(local.offset(), frame.raw_get(0));
                self.env.stack.drop_frame(frame);
            }
            Ir::StoreName(name) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let mut target = frame.get(&self.env, 0);
                let value = frame.get(&self.env, 1);
                local_try!(target.put(self.env, name, value, self.strict));
                
                self.env.stack.drop_frame(frame);
            }
            Ir::StoreNameUnchecked(name) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let mut target = frame.get(&self.env, 0);
                let value = frame.get(&self.env, 1);
                
                local_try!(target.define_own_property(self.env, name, JsDescriptor::new_simple_value(value), true));
                
                self.env.stack.drop_frame(frame);
            }
            Ir::StoreNameGetterUnchecked(name, function) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let mut object = frame.get(&self.env, 0);
                
                let scope = self.get_scope();
                
                let function = local_try!(self.env.new_function(function, scope, self.strict));
                
                local_try!(object.define_own_property(self.env, name, JsDescriptor::new_simple_accessor(Some(function), None), true));
                
                self.env.stack.drop_frame(frame);
            }
            Ir::StoreEnv(name) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let value = frame.get(&self.env, 0);
                let mut scope_object = local_try!(self.find_scope_object(name, false));
                
                local_try!(scope_object.put(self.env, name, value, self.strict));
                
                self.env.stack.drop_frame(frame);
            }
            Ir::StoreEnvArguments => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let arguments = frame.get(&self.env, 0);
                
                let mut scope = self.get_scope().unwrap();
                scope.set_arguments(arguments);
                
                self.env.stack.drop_frame(frame);
            }
            Ir::StoreGlobal(name) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let value = frame.get(&self.env, 0);
                let mut global = self.env.handle(JsHandle::Global).as_value();
                
                if self.strict && !global.has_property(self.env, name) {
                    return Next::Throw(JsError::new_reference(self.env));
                }
                
                local_try!(global.put(self.env, name, value, self.strict));
                
                self.env.stack.drop_frame(frame);
            }
            Ir::StoreNameSetterUnchecked(name, function) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let mut object = frame.get(&self.env, 0);
                
                let scope = self.get_scope();
                
                let function = local_try!(self.env.new_function(function, scope, self.strict));
                
                local_try!(object.define_own_property(self.env, name, JsDescriptor::new_simple_accessor(None, Some(function)), true));
                
                self.env.stack.drop_frame(frame);
            }
            Ir::StoreParam(index)  => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let value = frame.get(&self.env, 0);
                self.env.stack.drop_frame(frame);
                
                self.args.frame.set(index as usize + CALL_PROLOG, value.as_raw());
            }
            Ir::StrictEq => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let result = self.env.strict_eq(arg1, arg2);
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(JsRawValue::new_bool(result));
            }
            Ir::StrictNe => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let arg1 = frame.get(&self.env, 0);
                let arg2 = frame.get(&self.env, 1);
                let result = self.env.strict_eq(arg1, arg2);
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(JsRawValue::new_bool(!result));
            }
            Ir::Subtract => numeric_bin_op!(self, subtract),
            Ir::Swap => {
                let frame = self.env.stack.create_frame(2);
                let tmp = frame.raw_get(0);
                frame.set(0, frame.raw_get(1));
                frame.set(1, tmp);
            }
            Ir::Throw => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                
                let mut error = self.env.heap.alloc_root::<JsRawValue>(GC_VALUE);
                *error = frame.get(&self.env, 0).as_raw();
                
                self.env.stack.drop_frame(frame);
                
                return Next::Throw(JsError::Runtime(error));
            }
            Ir::ToPrimitive(JsPreferredType::String) => local_try!(self.cast(CastType::StringPrimitive)),
            Ir::ToPrimitive(JsPreferredType::Number) => local_try!(self.cast(CastType::NumberPrimitive)),
            Ir::ToPrimitive(JsPreferredType::None) => local_try!(self.cast(CastType::Primitive)),
            Ir::ToBoolean => local_try!(self.cast(CastType::Boolean)),
            Ir::ToNumber => local_try!(self.cast(CastType::Number)),
            Ir::ToInteger => local_try!(self.cast(CastType::Integer)),
            Ir::ToInt32 => local_try!(self.cast(CastType::Int32)),
            Ir::ToUInt32 => local_try!(self.cast(CastType::UInt32)),
            Ir::ToUInt16 => local_try!(self.cast(CastType::UInt16)),
            Ir::ToString => local_try!(self.cast(CastType::String)),
            Ir::ToObject => local_try!(self.cast(CastType::Object)),
            Ir::ToPropertyKey => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let arg = frame.get(&self.env, 0);
                let result = local_try!(arg.to_primitive(self.env, JsPreferredType::String));
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(result.as_raw());
            }
            Ir::Typeof => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let arg = frame.get(&self.env, 0);
                let result = self.env.type_of(arg).as_value();
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(result.as_raw());
            }
            Ir::TypeofName(name) => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let base = frame.get(&self.env, 0);
                let result = if base.is_undefined() {
                    JsString::from_str(self.env, "undefined")
                } else {
                    let arg = local_try!(base.get(self.env, name));
                    self.env.type_of(arg)
                };
                
                let result = result.as_value();
                
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(result.as_raw());
            }
            Ir::TypeofIndex => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(2);
                let base = frame.get(&self.env, 0);
                let result = if base.is_undefined() {
                    JsString::from_str(self.env, "undefined")
                } else {
                    let index = frame.get(&self.env, 1);
                    let index = local_try!(self.env.intern_value(index));
                    
                    let arg = local_try!(base.get(self.env, index));
                    self.env.type_of(arg)
                };
                
                let result = result.as_value();
                
                self.env.stack.drop_frame(frame);
                
                self.env.stack.push(result.as_raw());
            }
            Ir::ValidateMemberTarget => {
                let _scope = self.env.new_local_scope();
                
                let frame = self.env.stack.create_frame(1);
                let target = frame.get(&self.env, 0);
                
                let invalid = target.is_null_or_undefined();
                
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
        let scope = self.locals.get(&self.env, self.scope);
        if scope.is_undefined() {
            return None;
        }
        
        Some(scope.unwrap_scope())
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
    
    fn find_scope_object(&mut self, name: Name, must_exist: bool) -> JsResult<JsValue> {
        if let Some(mut scope) = self.get_scope() {
            loop {
                let object = scope.scope_object(self.env);
                
                if object.get_own_property(self.env, name).is_some() {
                    return Ok(object.as_value());
                }
                
                if let Some(parent) = scope.parent(self.env) {
                    scope = parent;
                } else {
                    break;
                }
            }
        }
        
        if must_exist {
            Err(JsError::new_reference(self.env))
        } else {
            Ok(self.env.handle(JsHandle::Global).as_value())
        }
    }
    
    fn call(&mut self, count: u32, eval: bool) -> JsResult<()> {
        let _scope = self.env.new_local_scope();
        
        let frame = self.env.stack.create_frame(count as usize + CALL_PROLOG);
        
        let args = JsArgs {
            frame: frame,
            argc: count as usize
        };
        
        // If this is a scoped call (i.e. eval) we need to double check to make
        // sure that this actually is a direct eval call. Otherwise the
        // scope is not inherited.
        
        let mut is_eval = false;
        
        if eval {
            let function = args.function(self.env);
            if function.ty() == JsType::Object {
                let global = self.env.handle(JsHandle::Global);
                if let Ok(eval) = global.get(&mut self.env, name::EVAL) {
                    is_eval = function == eval;
                }
            }
        }
        
        // If this actually is an eval, we go directly into eval. Otherwise
        // we let the normal call mechanism handle the call.
        
        if is_eval {
            let js = if args.argc > 0 {
                args.arg(self.env, 0)
            } else {
                JsValue::new_undefined()
            };
            
            let result = if js.ty() != JsType::String {
                js.as_raw()
            } else {
                let js = js.unwrap_string().to_string();
                let scope = self.get_scope().unwrap();
                
                let this_arg = self.args.this(self.env);
                try!(self.env.eval_scoped(&js, self.strict, this_arg, scope, ParseMode::DirectEval))
                    .as_value(self.env)
                    .as_raw()
            };
            
            self.env.stack.drop_frame(frame);
            self.env.stack.push(result);
        } else {
            try!(self.env.call(JsFnMode::new(false, self.strict), args));
        };
        
        Ok(())
    }
    
    // 11.4.1 The delete Operator
    fn delete(&mut self, name: Name, env: bool) -> JsResult<Option<Next>> {
        let _scope = self.env.new_local_scope();
        
        let frame = self.env.stack.create_frame(1);
        
        let mut target = frame.get(&self.env, 0);
        
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
        
        self.env.stack.push(JsRawValue::new_bool(result));
        
        Ok(None)
    }
    
    fn cast(&mut self, cast_ty: CastType) -> JsResult<()> {
        let _scope = self.env.new_local_scope();
        
        let frame = self.env.stack.create_frame(1);
        let arg = frame.get(&self.env, 0);
        
        let result = match cast_ty {
            CastType::Primitive => try!(arg.to_primitive(self.env, JsPreferredType::None)).as_raw(),
            CastType::StringPrimitive => try!(arg.to_primitive(self.env, JsPreferredType::String)).as_raw(),
            CastType::NumberPrimitive => try!(arg.to_primitive(self.env, JsPreferredType::Number)).as_raw(),
            CastType::Boolean => JsRawValue::new_bool(arg.to_boolean()),
            CastType::Number => JsRawValue::new_number(try!(arg.to_number(self.env))),
            CastType::Integer => JsRawValue::new_number(try!(arg.to_integer(self.env))),
            CastType::Int32 => JsRawValue::new_number(try!(arg.to_int32(self.env)) as f64),
            CastType::UInt32 => JsRawValue::new_number(try!(arg.to_uint32(self.env)) as f64),
            CastType::UInt16 => JsRawValue::new_number(try!(arg.to_uint16(self.env)) as f64),
            CastType::String => try!(arg.to_string(self.env)).as_value().as_raw(),
            CastType::Object => try!(arg.to_object(self.env)).as_raw()
        };
        
        self.env.stack.drop_frame(frame);
        
        self.env.stack.push(result);
        
        Ok(())
     }
}
