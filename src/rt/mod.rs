extern crate libc;

use gc::*;
use ir::IrContext;
use syntax::Name;
use syntax::token::name;
use syntax::ast::FunctionRef;
use syntax::parser::ParseMode;
use std::i32;
use std::mem::transmute;
use std::rc::Rc;
use std::io;

pub use self::value::{JsRawValue, JsValue};
pub use self::object::{JsObject, JsStoreType};
pub use self::string::JsString;
pub use self::null::JsNull;
pub use self::undefined::JsUndefined;
pub use self::number::JsNumber;
pub use self::boolean::JsBoolean;
pub use self::iterator::JsIterator;
pub use self::scope::JsScope;
pub use self::regexp::JsRegExp;

mod interpreter;
mod utf;
mod env;
mod runtime;
mod stack;
mod value;
mod object;
mod string;
mod number;
mod boolean;
mod undefined;
mod null;
mod iterator;
mod scope;
mod walker;
mod allocators;
mod regexp;
mod fmt;

const GC_ARRAY_STORE : u32 = 1;
const GC_ENTRY : u32 = 2;
const GC_HASH_STORE : u32 = 3;
const GC_ITERATOR : u32 = 4;
const GC_OBJECT : u32 = 5;
const GC_SCOPE : u32 = 6;
const GC_STRING : u32 = 7;
const GC_U16 : u32 = 8;
const GC_U32 : u32 = 9;
const GC_VALUE : u32 = 10;
const GC_ARRAY_CHUNK : u32 = 11;
const GC_SPARSE_ARRAY : u32 = 12;
const GC_REGEXP : u32 = 13;

#[repr(usize)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum JsHandle {
    Global = 0,
    Object = 1,
    Function = 2,
    Array = 3,
    String = 4,
    Date = 5,
    Number = 6,
    Boolean = 7,
    RegExpClass = 8,
    RegExp = 9,
    Error = 10,
    EvalError = 11,
    RangeError = 12,
    ReferenceError = 13,
    SyntaxError = 14,
    TypeError = 15,
    URIError = 16,
    NativeError = 17
}

pub struct JsEnv {
    heap: GcHeap,
    handles: Vec<Root<JsObject>>,
    global_scope: Root<JsScope>,
    ir: IrContext,
    stack: Rc<stack::Stack>,
    privileged: bool
}

impl JsEnv {
    pub fn new() -> JsResult<JsEnv> {
        let stack = Rc::new(stack::Stack::new());
        
        let walker = Box::new(walker::Walker::new(stack.clone()));
        validate_walker(&*walker);
        
        let heap = GcHeap::new(walker, GcOpts::default());
        
        let global_scope = heap.alloc_root::<JsScope>(GC_SCOPE);
        
        let mut env = JsEnv {
            heap: heap,
            global_scope: global_scope,
            ir: IrContext::new(),
            stack: stack,
            privileged: true,
            handles: Vec::new()
        };
        
        if let Err(error) = env::setup(&mut env) {
            let _scope = env.new_local_scope();
            
            let error = error.as_runtime(&mut env).as_value(&env);
            
            let error = if let Ok(error) = error.to_string(&mut env) {
                error.to_string()
            } else {
                "(cannot convert error to string)".to_string()
            };
            
            panic!("(setup): uncaught {}", error);
        }
        
        // Turn of privileged mode for normal code (i.e. not setup code).
        env.privileged = false;
        
        Ok(env)
    }
    
    pub fn handle(&self, handle: JsHandle) -> Local<JsObject> {
        self.handles[unsafe { transmute::<_, usize>(handle) }].as_local(self)
    }
    
    fn add_handle(&mut self, handle: JsHandle, local: Local<JsObject>) {
        let index = unsafe { transmute(handle) };
        if self.handles.len() != index {
            panic!("unexpected handle order; got {:?} expected {:?}", handle, unsafe { transmute::<_, JsHandle>(self.handles.len()) });
        }
        let root = local.as_root(self);
        self.handles.push(root);
    }
    
    pub fn run(&mut self, file_name: &str) -> JsResult<Root<JsRawValue>> {
        self.run_strict(file_name, false)
    }
    
    pub fn run_strict(&mut self, file_name: &str, strict: bool) -> JsResult<Root<JsRawValue>> {
        let function_ref = try!(self.ir.parse_file(file_name, strict, self.privileged));
        
        let mut ir = String::new();
        try!(self.ir.print_ir(&mut ir));
        debugln!("{}", ir);
        
        let _scope = self.new_local_scope();
        
        let global = self.handle(JsHandle::Global).as_value();
        let global_scope = self.global_scope.as_local(self);
        
        self.run_program(function_ref, global, global_scope)
    }
    
    pub fn eval(&mut self, js: &str) -> JsResult<Root<JsRawValue>> {
        let _scope = self.new_local_scope();
        
        let global = self.handle(JsHandle::Global).as_value();
        let global_scope = self.global_scope.as_local(self);
        
        self.eval_scoped(js, false, global, global_scope, ParseMode::Normal)
    }
    
    fn eval_scoped(&mut self, js: &str, strict: bool, this: JsValue, scope: Local<JsScope>, mode: ParseMode) -> JsResult<Root<JsRawValue>> {
        let function_ref = try!(self.ir.parse_string(js, strict, mode, self.privileged));
        
        let mut ir = String::new();
        try!(self.ir.print_ir(&mut ir));
        debugln!("{}", ir);
        
        self.run_program(function_ref, this, scope)
    }
    
    fn run_program(&mut self, function_ref: FunctionRef, this: JsValue, scope: Local<JsScope>) -> JsResult<Root<JsRawValue>> {
        let function = self.ir.get_function(function_ref);
        
        let this = if !function.strict && this.is_undefined() {
            self.handle(JsHandle::Global).as_value()
        } else {
            this
        };
        
        let function = try!(self.new_function(function_ref, Some(scope), false));
        
        let mut result = self.heap.alloc_root::<JsRawValue>(GC_VALUE);
        *result = try!(function.call(self, this, Vec::new(), false)).as_raw();
        
        Ok(result)
    }
    
    pub fn intern(&self, name: &str) -> Name {
        self.ir.interner().intern(name)
    }
    
    pub fn intern_value(&mut self, value: JsValue) -> JsResult<Name> {
        if value.ty() == JsType::Number {
            let index = value.unwrap_number();
            if index >= 0.0 && index <= i32::MAX as f64 && index as i32 as f64 == index {
                return Ok(Name::from_index(index as usize));
            }
        }
        
        let index = try!(value.to_string(self));
        Ok(self.intern(&index.to_string()))
    }
    
    fn new_native_function<'a>(&mut self, name: Option<Name>, args: u32, function: JsFn) -> JsValue {
        let mut result = JsObject::new_function(self, JsFunction::Native(name, args, function, true), false).as_value();
        
        let mut proto = self.create_object();
        let value = proto.as_value();
        result.define_own_property(self, name::PROTOTYPE, JsDescriptor::new_value(value, false, false, false), false).ok();
        proto.define_own_property(self, name::CONSTRUCTOR, JsDescriptor::new_value(result, true, false, true), false).ok();
        
        result
    }
    
    pub fn new_local_scope(&self) -> LocalScope {
        self.heap.new_local_scope()
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum JsPreferredType {
    None,
    String,
    Number
}

#[allow(unused_variables)]
pub trait JsItem {
    fn as_value(&self) -> JsValue;
    
    fn get_own_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
        None
    }
    
    // 8.12.2 [[GetProperty]] (P)
    fn get_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
        if let Some(descriptor) = self.get_own_property(env, property) {
            Some(descriptor)
        } else {
            if let Some(proto) = self.prototype(env) {
                proto.get_property(env, property)
            } else {
                None
            }
        }
    }
    
    // 8.12.3 [[Get]] (P)
    fn get(&self, env: &mut JsEnv, property: Name) -> JsResult<JsValue> {
        if let Some(desc) = self.get_property(env, property) {
            return if desc.is_data() {
                Ok(desc.value())
            } else {
                let get = desc.get();
                if get.is_undefined() {
                    Ok(JsValue::new_undefined())
                } else {
                    let this = self.as_value();
                    get.call(env, this, Vec::new(), false)
                }
            }
        }

        Ok(JsValue::new_undefined())
    }
    
    // 8.12.4 [[CanPut]] (P)
    fn can_put(&self, env: &JsEnv, property: Name) -> bool {
        if let Some(desc) = self.get_own_property(env, property) {
            return if desc.is_accessor() {
                if let Some(set) = desc.set {
                    !set.is_undefined()
                } else {
                    false
                }
            } else {
                desc.is_writable()
            }
        }
        
        if let Some(proto) = self.prototype(env) {
            if let Some(inherited) = proto.get_property(env, property) {
                return if inherited.is_accessor() {
                    if let Some(set) = inherited.set {
                        !set.is_undefined()
                    } else {
                        false
                    }
                } else {
                    if !self.is_extensible() {
                        false
                    } else {
                        inherited.is_writable()
                    }
                }
            }
        }

        self.is_extensible()
    }
    
    // 8.12.5 [[Put]] ( P, V, Throw )
    fn put(&mut self, env: &mut JsEnv, property: Name, value: JsValue, throw: bool) -> JsResult<()> {
        if !self.can_put(env, property) {
            return if throw {
                Err(JsError::new_type(env, ::errors::TYPE_CANNOT_PUT))
            } else {
                Ok(())
            };
        }
        
        if let Some(own_desc) = self.get_own_property(env, property) {
            if own_desc.is_data() {
                let value_desc = JsDescriptor {
                    value: Some(value),
                    ..JsDescriptor::default()
                };
                try!(self.define_own_property(env, property, value_desc, throw));
                
                return Ok(());
            }
        }
        
        if let Some(desc) = self.get_property(env, property) {
            if desc.is_accessor() {
                let this = self.as_value();
                let set = desc.set();
                // TODO #71: This is not conform the specs. The specs state that
                // this cannot be undefined. However nothing is stopping
                // you from only specifying a getter.
                return if !set.is_undefined() {
                    try!(set.call(env, this, vec![value], false));
                    Ok(())
                } else {
                    Err(JsError::new_type(env, ::errors::TYPE_PROPERTY_ONLY_HAS_GETTER))
                };
            }
        }
        
        try!(self.define_own_property(env, property, JsDescriptor::new_simple_value(value), throw));
        
        Ok(())
    }
    
    // 8.12.6 [[HasProperty]] (P)
    fn has_property(&self, env: &JsEnv, property: Name) -> bool {
        self.get_property(env, property).is_some()
    }
    
    // 8.12.7 [[Delete]] (P, Throw)
    fn delete(&mut self, env: &mut JsEnv, property: Name, throw: bool) -> JsResult<bool> {
        // If get_own_property returns None, delete returns true.
        Ok(true)
    }
    
    // 8.12.8 [[DefaultValue]] (hint)
    fn default_value(&self, env: &mut JsEnv, hint: JsPreferredType) -> JsResult<JsValue> {
        let hint = if hint == JsPreferredType::None {
            let date_class = try!(env.handle(JsHandle::Global).as_value().get(env, name::DATE_CLASS));
            
            let object = self.as_value();
            if try!(date_class.has_instance(env, object)) {
                JsPreferredType::String
            } else {
                JsPreferredType::Number
            }
        } else {
            hint
        };
        
        fn try_call(env: &mut JsEnv, this: JsValue, method: JsValue) -> JsResult<Option<JsValue>> {
            if method.is_callable() {
                let this = this.as_value();
                let val = try!(method.call(env, this, Vec::new(), false));
                if val.ty().is_primitive() {
                    return Ok(Some(val));
                }
            }
            
            Ok(None)
        }
        
        let this = self.as_value();
        
        if hint == JsPreferredType::String {
            let to_string = try!(this.get(env, name::TO_STRING));
            if let Some(str) = try!(try_call(env, this, to_string)) {
                return Ok(str);
            }
            
            let value_of = try!(this.get(env, name::VALUE_OF));
            if let Some(val) = try!(try_call(env, this, value_of)) {
                return Ok(val);
            }
            
            Err(JsError::new_type(env, ::errors::TYPE_INVALID))
        } else {
            let value_of = try!(this.get(env, name::VALUE_OF));
            if let Some(val) = try!(try_call(env, this, value_of)) {
                return Ok(val);
            }
            
            let to_string = try!(this.get(env, name::TO_STRING));
            if let Some(str) = try!(try_call(env, this, to_string)) {
                return Ok(str);
            }
            
            Err(JsError::new_type(env, ::errors::TYPE_INVALID))
        }
    }
    
    // 8.12.9 [[DefineOwnProperty]] (P, Desc, Throw)
    fn define_own_property(&mut self, env: &mut JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
        // If get_own_property returns None and self is not extensible, the below happens.
        if throw { Err(JsError::new_type(env, ::errors::TYPE_CANNOT_PUT)) } else { Ok(false) }
    }
    
    fn is_callable(&self) -> bool {
        false
    }
    
    fn call(&self, env: &mut JsEnv, this: JsValue, args: Vec<JsValue>, strict: bool) -> JsResult<JsValue> {
        let args = JsArgs::new(env, this, self.as_value(), &args);
        
        try!(env.call(JsFnMode::new(false, strict), args));
        
        let result = env.stack.pop().as_value(env);
        
        Ok(result)
    }
    
    fn can_construct(&self) -> bool {
        false
    }
    
    // 13.2.2 [[Construct]]
    // 15.3.4.5.2 [[Construct]]
    fn construct(&self, env: &mut JsEnv, args: Vec<JsValue>) -> JsResult<JsValue> {
        let args = JsArgs::new(env, JsValue::new_undefined(), self.as_value(), &args);
        
        try!(env.construct(args));
        
        let result = env.stack.pop().as_value(env);
        
        Ok(result)
    }
    
    fn has_prototype(&self) -> bool {
        false
    }
    
    fn prototype(&self, env: &JsEnv) -> Option<JsValue> {
        panic!("prototype not supported on {:?}", self.as_value().ty());
    }
    
    fn set_prototype(&mut self, prototype: Option<JsValue>) {
        panic!("prototype not supported on {:?}", self.as_value().ty());
    }
    
    fn has_class(&self) -> bool {
        false
    }
    
    fn class(&self) -> Option<Name> {
        None
    }
    
    fn set_class(&mut self, class: Option<Name>) {
        panic!("class not supported");
    }
    
    fn is_extensible(&self) -> bool {
        true
    }
    
    fn has_instance(&self, env: &mut JsEnv, object: JsValue) -> JsResult<bool> {
        Err(JsError::new_type(env, ::errors::TYPE_CANNOT_HAS_INSTANCE))
    }
    
    fn scope(&self, env: &JsEnv) -> Option<Local<JsScope>> {
        panic!("scope not supported");
    }
    
    fn set_scope(&mut self, scope: Option<Local<JsScope>>) {
        panic!("scope not supported");
    }
}

#[derive(Copy, Clone)]
pub struct JsDescriptor {
    pub value: Option<JsValue>,
    pub get: Option<JsValue>,
    pub set: Option<JsValue>,
    pub writable: Option<bool>,
    pub enumerable: Option<bool>,
    pub configurable: Option<bool>
}

impl JsDescriptor {
    pub fn default() -> JsDescriptor {
        JsDescriptor {
            value: None,
            get: None,
            set: None,
            writable: None,
            enumerable: None,
            configurable: None
        }
    }
    
    pub fn new_value(value: JsValue, writable: bool, enumerable: bool, configurable: bool) -> JsDescriptor {
        JsDescriptor {
            value: Some(value),
            get: None,
            set: None,
            writable: Some(writable),
            enumerable: Some(enumerable),
            configurable: Some(configurable)
        }
    }
    
    pub fn new_simple_value(value: JsValue) -> JsDescriptor {
        Self::new_value(value, true, true, true)
    }
    
    pub fn new_accessor(get: Option<JsValue>, set: Option<JsValue>, enumerable: bool, configurable: bool) -> JsDescriptor {
        JsDescriptor {
            value: None,
            get: get,
            set: set,
            writable: None,
            enumerable: Some(enumerable),
            configurable: Some(configurable)
        }
    }
    
    pub fn new_simple_accessor(get: Option<JsValue>, set: Option<JsValue>) -> JsDescriptor {
        Self::new_accessor(get, set, true, true)
    }
    
    pub fn is_same(&self, env: &JsEnv, other: &JsDescriptor) -> bool {
        fn is_same(env: &JsEnv, x: &Option<JsValue>, y: &Option<JsValue>) -> bool{
            (x.is_none() && y.is_none()) || (x.is_some() && y.is_some() && env.same_value(x.unwrap(), y.unwrap()))
        }
        
        is_same(env, &self.value, &other.value) &&
            is_same(env, &self.get, &other.get) &&
            is_same(env, &self.set, &other.set) &&
            self.writable == other.writable &&
            self.enumerable == other.enumerable &&
            self.configurable == other.configurable
    }
    
    // 8.10.1 IsAccessorDescriptor ( Desc )
    pub fn is_accessor(&self) -> bool {
        self.get.is_some() || self.set.is_some()
    }
    
    // 8.10.2 IsDataDescriptor ( Desc )
    pub fn is_data(&self) -> bool {
        self.writable.is_some() || self.value.is_some()
    }
    
    // 8.10.3 IsGenericDescriptor ( Desc )
    pub fn is_generic(&self) -> bool {
        !(self.is_accessor() || self.is_data())
    }
    
    pub fn value(&self) -> JsValue {
        self.value.unwrap_or_else(|| JsValue::new_undefined())
    }
    
    pub fn get(&self) -> JsValue {
        self.get.unwrap_or_else(|| JsValue::new_undefined())
    }
    
    pub fn set(&self) -> JsValue {
        self.set.unwrap_or_else(|| JsValue::new_undefined())
    }
    
    pub fn is_writable(&self) -> bool {
        self.writable.unwrap_or(false)
    }
    
    pub fn is_enumerable(&self) -> bool {
        self.enumerable.unwrap_or(false)
    }
    
    pub fn is_configurable(&self) -> bool {
        self.configurable.unwrap_or(false)
    }
    
    pub fn is_empty(&self) -> bool {
        self.value.is_none() && self.get.is_none() && self.set.is_none() && self.writable.is_none() && self.enumerable.is_none() && self.configurable.is_none()
    }
    
    // 8.10.4 FromPropertyDescriptor ( Desc )
    pub fn from_property_descriptor(&self, env: &mut JsEnv) -> JsResult<JsValue> {
        let mut obj = env.create_object();
        
        if self.is_data() {
            let value = self.value();
            let writable = JsValue::new_bool(self.is_writable());
            
            try!(obj.define_own_property(env, name::VALUE, JsDescriptor::new_simple_value(value), false));
            try!(obj.define_own_property(env, name::WRITABLE, JsDescriptor::new_simple_value(writable), false));
        } else if self.is_accessor() {
            let get = self.get();
            let set = self.set();
            
            try!(obj.define_own_property(env, name::GET, JsDescriptor::new_simple_value(get), false));
            try!(obj.define_own_property(env, name::SET, JsDescriptor::new_simple_value(set), false));
        }
        
        let enumerable = JsValue::new_bool(self.is_enumerable());
        let configurable = JsValue::new_bool(self.is_configurable());
        
        try!(obj.define_own_property(env, name::ENUMERABLE, JsDescriptor::new_simple_value(enumerable), false));
        try!(obj.define_own_property(env, name::CONFIGURABLE, JsDescriptor::new_simple_value(configurable), false));
        
        Ok(obj.as_value())
    }
    
    // 8.10.5 ToPropertyDescriptor ( Obj )
    pub fn to_property_descriptor(env: &mut JsEnv, obj: JsValue) -> JsResult<JsDescriptor> {
        if obj.ty() != JsType::Object {
            Err(JsError::new_type(env, ::errors::TYPE_INVALID))
        } else {
            let enumerable = if obj.has_property(env, name::ENUMERABLE) {
                let enumerable = try!(obj.get(env, name::ENUMERABLE));
                Some(enumerable.to_boolean())
            } else {
                None
            };
            let configurable = if obj.has_property(env, name::CONFIGURABLE) {
                let configurable = try!(obj.get(env, name::CONFIGURABLE));
                Some(configurable.to_boolean())
            } else {
                None
            };
            let value = if obj.has_property(env, name::VALUE) {
                Some(try!(obj.get(env, name::VALUE)))
            } else {
                None
            };
            let writable = if obj.has_property(env, name::WRITABLE) {
                let writable = try!(obj.get(env, name::WRITABLE));
                Some(writable.to_boolean())
            } else {
                None
            };
            let getter = if obj.has_property(env, name::GET) {
                let getter = try!(obj.get(env, name::GET));
                if getter.ty() != JsType::Undefined && !getter.is_callable() {
                    return Err(JsError::new_type(env, ::errors::TYPE_ACCESSOR_NOT_CALLABLE));
                }
                Some(getter)
            } else {
                None
            };
            let setter = if obj.has_property(env, name::SET) {
                let setter = try!(obj.get(env, name::SET));
                if setter.ty() != JsType::Undefined && !setter.is_callable() {
                    return Err(JsError::new_type(env, ::errors::TYPE_ACCESSOR_NOT_CALLABLE));
                }
                Some(setter)
            } else {
                None
            };
            if (getter.is_some() || setter.is_some()) && (value.is_some() || writable.is_some()) {
                return Err(JsError::new_type(env, ::errors::TYPE_WRITABLE_VALUE_INVALID_ON_ACCESSOR));
            }
            
            Ok(JsDescriptor {
                value: value,
                get: getter,
                set: setter,
                writable: writable,
                enumerable: enumerable,
                configurable: configurable
            })
        }
    }
    
    pub fn merge(&self, other: JsDescriptor) -> JsDescriptor {
        JsDescriptor {
            value: self.value.or(other.value),
            get: self.get.or(other.get),
            set: self.set.or(other.set),
            writable: self.writable.or(other.writable),
            enumerable: self.enumerable.or(other.enumerable),
            configurable: self.configurable.or(other.configurable)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
#[repr(usize)]
pub enum JsType {
    Undefined = 0,
    Null = 1,
    Number = 2,
    Boolean = 3,
    String = 4,
    Object = 5,
    Iterator = 6,
    Scope = 7,
    RegExp = 8
}

impl JsType {
    fn is_ptr(&self) -> bool {
        match *self {
            JsType::String | JsType::Object | JsType::Iterator | JsType::Scope | JsType::RegExp => true,
            _ => false
        }
    }
    
    fn is_primitive(&self) -> bool {
        match *self {
            JsType::Object => false,
            _ => true
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub struct JsFnMode(u8);

impl JsFnMode {
    fn new(construct: bool, strict: bool) -> JsFnMode {
        JsFnMode(
            if construct { 1 } else { 0 } |
            if strict { 2 } else { 0 }
        )
    }
    
    fn construct(&self) -> bool {
        (self.0 & 1) != 0
    }
    
    fn strict(&self) -> bool {
        (self.0 & 2) != 0
    }
}

pub struct JsArgs {
    frame: stack::StackFrame,
    argc: usize
}

impl JsArgs {
    pub fn new(env: &JsEnv, this: JsValue, function: JsValue, args: &[JsValue]) -> JsArgs {
        let stack = &*env.stack;
        
        let frame = stack.create_frame(0);
        
        stack.push(this.as_raw());
        stack.push(function.as_raw());
        
        for arg in args {
            stack.push(arg.as_raw());
        }
        
        JsArgs {
            frame: frame,
            argc: args.len()
        }
    }
    
    pub fn arg(&self, env: &JsEnv, index: usize) -> JsValue {
        if self.argc > index {
            self.frame.get(env, index + 2)
        } else {
            JsValue::new_undefined()
        }
    }
    
    pub fn arg_or(&self, env: &JsEnv, index: usize, def: JsValue) -> JsValue {
        if self.argc > index {
            self.frame.get(env, index + 2)
        } else {
            def
        }
    }
    
    pub fn map_or<U, F: FnOnce(&mut JsEnv, JsValue) -> U>(&self, env: &mut JsEnv, index: usize, def: U, f: F) -> U {
        if self.argc > index {
            let value = self.frame.get(env, index + 2);
            f(env, value)
        } else {
            def
        }
    }
    
    pub fn map_or_else<U, D: FnOnce(&mut JsEnv) -> U, F: FnOnce(&mut JsEnv, JsValue) -> U>(&self, env: &mut JsEnv, index: usize, def: D, f: F) -> U {
        if self.argc > index {
            let value = self.frame.get(env, index + 2);
            f(env, value)
        } else {
            def(env)
        }
    }
    
    pub fn args(&self, env: &JsEnv) -> Vec<JsValue> {
        let mut args = Vec::new();
        
        for i in 0..self.argc {
            args.push(self.arg(env, i));
        }
        
        args
    }
    
    pub fn this(&self, env: &JsEnv) -> JsValue {
        self.frame.get(env, 0)
    }
    
    fn set_this(&self, value: JsRawValue) {
        self.frame.set(0, value);
    }
    
    fn raw_this(&self) -> JsRawValue {
        self.frame.raw_get(0)
    }
    
    pub fn function(&self, env: &JsEnv) -> JsValue {
        self.frame.get(env, 1)
    }
}

pub type JsFn = fn(&mut JsEnv, JsFnMode, JsArgs) -> JsResult<JsValue>;

pub enum JsFunction {
    Ir(FunctionRef),
    Native(Option<Name>, u32, JsFn, bool),
    Bound
}

pub type JsResult<T> = Result<T, JsError>;

pub enum JsError {
    Io(io::Error),
    Lex(String),
    Parse(String),
    Reference(String),
    Runtime(Root<JsRawValue>)
}

impl JsError {
    fn new_error(env: &mut JsEnv, handle: JsHandle, message: Option<&str>, file_name: Option<&str>, line_number: Option<usize>) -> JsResult<Root<JsRawValue>> {
        // If construction of the error fails, we simply propagate the error itself.
        
        let _scope = env.new_local_scope();
        
        let class = env.handle(handle);
        
        let mut args = Vec::new();
        
        args.push(match message {
            Some(message) => JsString::from_str(env, message).as_value(),
            None => JsValue::new_undefined()
        });
        args.push(match file_name {
            Some(file_name) => JsString::from_str(env, file_name).as_value(),
            None => JsValue::new_undefined()
        });
        args.push(match line_number {
            Some(line_number) => JsValue::new_number(line_number as f64),
            None => JsValue::new_undefined()
        });
        
        let mut result = env.heap.alloc_root::<JsRawValue>(GC_VALUE);
        *result = try!(class.construct(env, args)).as_raw();
        
        Ok(result)
    }
    
    pub fn new_runtime(env: &mut JsEnv, handle: JsHandle, message: Option<&str>, file_name: Option<&str>, line_number: Option<usize>) -> JsError {
        match Self::new_error(env, handle, message, file_name, line_number) {
            Ok(error) => JsError::Runtime(error),
            Err(error) => error
        }
    }
    
    pub fn new_type(env: &mut JsEnv, message: &str) -> JsError {
        Self::new_runtime(env, JsHandle::TypeError, Some(message), None, None)
    }
    
    pub fn new_range(env: &mut JsEnv) -> JsError {
        Self::new_runtime(env, JsHandle::RangeError, None, None, None)
    }
    
    pub fn new_uri(env: &mut JsEnv) -> JsError {
        Self::new_runtime(env, JsHandle::URIError, None, None, None)
    }
    
    pub fn new_reference(env: &mut JsEnv) -> JsError {
        Self::new_runtime(env, JsHandle::ReferenceError, None, None, None)
    }
    
    pub fn new_syntax(env: &mut JsEnv, message: &str) -> JsError {
        Self::new_runtime(env, JsHandle::SyntaxError, Some(message), None, None)
    }
    
    pub fn as_runtime(&self, env: &mut JsEnv) -> Root<JsRawValue> {
        match *self {
            JsError::Lex(ref message) | JsError::Parse(ref message) => {
                match Self::new_error(env, JsHandle::SyntaxError, Some(&message), None, None) {
                    Ok(error) => error,
                    Err(error) => error.as_runtime(env)
                }
            }
            JsError::Reference(ref message) => {
                match Self::new_error(env, JsHandle::ReferenceError, Some(&message), None, None) {
                    Ok(error) => error,
                    Err(error) => error.as_runtime(env)
                }
            }
            JsError::Runtime(ref error) => error.clone(),
            ref error @ _ => {
                // TODO #73: This could be nicer.
                let mut result = env.heap.alloc_root::<JsRawValue>(GC_VALUE);
                *result = JsString::from_str(env, &format!("{:?}", error)).as_value().as_raw();
                
                result
            }
        }
    }
}

impl ::std::fmt::Debug for JsError {
    fn fmt(&self, formatter: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        try!(write!(formatter, "JsError {{ "));
        match *self {
            JsError::Io(ref err) => try!(err.fmt(formatter)),
            JsError::Lex(ref message) => try!(write!(formatter, "Lex {{ {} }}", message)),
            JsError::Parse(ref message) => try!(write!(formatter, "Parse {{ {} }}", message)),
            JsError::Reference(ref message) => try!(write!(formatter, "Reference {{ {} }}", message)),
            JsError::Runtime(..) => try!(write!(formatter, "Runtime {{ .. }}"))
        }
        write!(formatter, " }}")
    }
}

fn validate_walker(walker: &GcWalker) {
    unsafe {
        object::validate_walker(walker);
        value::validate_walker_for_value(walker);
        regexp::validate_walker(walker);
    }
}

unsafe fn validate_walker_field(walker: &GcWalker, ty: u32, ptr: ptr_t, expect_pointer: bool) -> u32 {
    validate_walker_field_at(walker, ty, ptr, expect_pointer, 0)
}

unsafe fn validate_walker_field_at(walker: &GcWalker, ty: u32, ptr: ptr_t, expect_pointer: bool, index: u32) -> u32 {
    let mut index = index;
    let mut offset = transmute::<_, *const usize>(ptr).offset(index as isize);
    
    loop {
        if *offset != 0 {
            let is_pointer = walker.walk(ty, ptr, index) == GcWalk::Pointer;
            
            if expect_pointer != is_pointer {
                if is_pointer {
                    panic!("GC walker is invalid; found pointer at {} but expected none", index);
                } else {
                    panic!("GC walker is invalid; expected a pointer at {} but found none", index);
                }
            }
            
            return index as u32;
        }
        
        index += 1;
        offset = offset.offset(1);
    }
}
