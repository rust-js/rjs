use syntax::Name;
use syntax::ast::FunctionRef;
use syntax::token::name;
use rt::{JsEnv, JsFunction, JsValue, JsItem, JsDescriptor, JsScope, JsType, JsString, JsArgs, JsFnMode, JsHandle};
use rt::{GC_OBJECT, GC_ENTRY};
use rt::validate_walker_field;
use rt::value::validate_walker_for_embedded_value;
use gc::{Local, Ptr, AsPtr, ptr_t, GcWalker};
use ::{JsResult, JsError};
use self::hash_store::HashStore;
use self::array_store::ArrayStore;
use std::str::FromStr;
use std::mem::{zeroed, transmute, size_of};

mod hash_store;
mod array_store;
mod sparse_array;

// Modifications to this struct must be synchronized with the GC walker.
pub struct JsObject {
    class: Option<Name>,
    value: JsValue,
    function: Option<JsFunction>,
    prototype: Ptr<JsObject>,
    scope: Ptr<JsScope>,
    store: StorePtr,
    extensible: bool
}

impl JsObject {
    pub fn new(env: &JsEnv, ty: JsStoreType) -> JsObject {
        let store = unsafe {
            match ty {
                JsStoreType::Hash => StorePtr::new(HashStore::new_local(env).as_ptr(), ty),
                JsStoreType::Array => StorePtr::new(ArrayStore::new_local(env).as_ptr(), ty)
            }
        };
        
        JsObject {
            class: None,
            value: JsValue::new_undefined(),
            function: None,
            prototype: Ptr::null(),
            scope: Ptr::null(),
            store: store,
            extensible: true
        }
    }
    
    pub fn new_local(env: &JsEnv, ty: JsStoreType) -> Local<JsObject> {
        let mut result = env.heap.alloc_local(GC_OBJECT);
        
        *result = Self::new(env, ty);
        
        result
    }
    
    pub fn new_function(env: &mut JsEnv, function: JsFunction, strict: bool) -> Local<JsObject> {
        let prototype = env.handle(JsHandle::Function);
        Self::new_function_with_prototype(env, function, prototype, strict)
    }
    
    pub fn new_function_with_prototype(env: &mut JsEnv, function: JsFunction, prototype: Local<JsObject>, strict: bool) -> Local<JsObject> {
        let mut result = Self::new_local(env, JsStoreType::Hash);
        
        let (name, args, strict) = match function {
            JsFunction::Native(name, args, _, _) => (name, args, strict),
            JsFunction::Ir(function_ref) => {
                let function = env.ir.get_function(function_ref);
                (function.name, function.args, strict || function.strict)
            }
            JsFunction::Bound => {
                (None, 0, strict)
            }
        };
        
        result.prototype = prototype.as_ptr();
        result.class = Some(name::FUNCTION_CLASS);
        
        let value = env.new_number(args as f64);
        
        // TODO #68: This does not seem to be conform the specs. Value should not be configurable.
        // Don't set the length on bound functions. The caller will take care of this.
        
        if function != JsFunction::Bound {
            result.define_own_property(env, name::LENGTH, JsDescriptor::new_value(value, false, false, true), false).ok();
        }
        
        result.function = Some(function);

        let name = name.unwrap_or(name::EMPTY);
        let name = JsString::from_str(env, &*env.ir.interner().get(name)).as_value(env);
        
        result.define_own_property(env, name::NAME, JsDescriptor::new_value(name, false, false, true), false).ok();
        
        if strict {
            let thrower = env.new_native_function(None, 0, throw_type_error);
            
            result.define_own_property(env, name::CALLER, JsDescriptor::new_accessor(Some(thrower), Some(thrower), false, false), false).ok();
            result.define_own_property(env, name::ARGUMENTS, JsDescriptor::new_accessor(Some(thrower), Some(thrower), false, false), false).ok();
        }
        
        result
    }
}

fn throw_type_error(env: &mut JsEnv, _mode: JsFnMode, _args: JsArgs) -> JsResult<Local<JsValue>> {
    Err(JsError::new_type(env, ::errors::TYPE_CANNOT_ACCESS_FUNCTION_PROPERTY))
}

impl Local<JsObject> {
    pub fn value(&self, env: &JsEnv) -> Local<JsValue> {
        let mut value = env.new_value();
        
        *value = self.value;
        
        value
    }
    
    pub fn set_value(&mut self, value: Local<JsValue>) {
        self.value = *value;
    }
    
    pub fn extensible(&self) -> bool {
        self.extensible
    }
    
    pub fn set_extensible(&mut self, extensible: bool) {
        self.extensible = extensible;
    }
    
    pub fn function(&self) -> Option<JsFunction> {
        self.function.clone()
    }
    
    pub fn get_key(&self, env: &JsEnv, offset: usize) -> JsStoreKey {
        match self.store.get_key(env, offset) {
            StoreKey::Key(name, enumerable) => JsStoreKey::Key(name, enumerable),
            StoreKey::Missing => JsStoreKey::Missing,
            StoreKey::End(len) => {
                if self.value.ty() == JsType::String {
                    let offset = offset - len;
                    let string = self.value(env).unwrap_string(env);
                    let chars = string.chars();
                    
                    if offset < chars.len() {
                        JsStoreKey::Key(Name::from_index(offset), true)
                    } else if offset == chars.len() {
                        JsStoreKey::Key(name::LENGTH, false)
                    } else {
                        JsStoreKey::End
                    }
                } else {
                    JsStoreKey::End
                }
            }
        }
    }

    // 8.12.9 [[DefineOwnProperty]] (P, Desc, Throw)
    fn define_own_object_property(&mut self, env: &mut JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
        fn reject(env: &mut JsEnv, throw: bool) -> JsResult<bool> {
            if throw { Err(JsError::new_type(env, ::errors::TYPE_NOT_EXTENSIBLE)) } else { Ok(false) }
        }

        fn is_same(env: &JsEnv, x: &Option<Local<JsValue>>, y: &Option<Local<JsValue>>) -> bool{
            (x.is_none() && y.is_none()) || (x.is_some() && y.is_some() && env.same_value(x.unwrap(), y.unwrap()))
        }
        
        let current = self.get_own_property(env, property);
        let extensible = self.is_extensible(env);
        
        match current {
            None => {
                if !extensible {
                    reject(env, throw)
                } else {
                    let descriptor = if descriptor.is_generic() || descriptor.is_data() {
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
                    
                    self.store.add(env, property, &descriptor);
                    
                    Ok(true)
                }
            }
            Some(mut current) => {
                if descriptor.is_empty() {
                    return Ok(true);
                }
                if current.is_same(env, &descriptor) {
                    return Ok(true)
                }
                if !current.is_configurable() {
                    if descriptor.is_configurable() {
                        return reject(env, throw);
                    } else if descriptor.enumerable.is_some() && descriptor.is_enumerable() != current.is_enumerable() {
                        return reject(env, throw);
                    }
                }
                if !descriptor.is_generic() {
                    if current.is_data() != descriptor.is_data() {
                        if !current.is_configurable() {
                            return reject(env, throw);
                        }
                        
                        current = JsDescriptor {
                            enumerable: current.enumerable,
                            configurable: current.configurable,
                            ..JsDescriptor::default()
                        };
                    } else if current.is_data() {
                        if !current.is_configurable() {
                            if !current.is_writable() {
                                if descriptor.is_writable() {
                                    return reject(env, throw);
                                } else {
                                    if descriptor.value.is_some() && !is_same(env, &descriptor.value, &current.value) {
                                        return reject(env, throw);
                                    }
                                }
                            }
                        }
                    } else {
                        if !current.is_configurable() {
                            if
                                (descriptor.set.is_some() && !is_same(env, &descriptor.set, &current.set)) ||
                                (descriptor.get.is_some() && !is_same(env, &descriptor.get, &current.get))
                            {
                                return reject(env, throw);
                            }
                        }
                    }
                }
                
                self.store.replace(env, property, &descriptor.merge(current));
                
                Ok(true)
            }
        }
    }
    
    // 15.4.5.1 [[DefineOwnProperty]] ( P, Desc, Throw )
    fn define_own_array_property(&mut self, env: &mut JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
        let mut old_len_desc = self.get_own_property(env, name::LENGTH).unwrap();
        // This is safe because we control the value of length.
        let mut old_len = old_len_desc.value.unwrap().unwrap_number() as u32;
        
        if property == name::LENGTH {
            return match descriptor.value {
                None => {
                    self.define_own_object_property(
                        env,
                        name::LENGTH,
                        descriptor,
                        throw
                    )
                }
                Some(desc_value) => {
                    let mut new_len_desc = descriptor.clone();
                    let new_len = try!(desc_value.to_uint32(env));
                    
                    if new_len as f64 != try!(desc_value.to_number(env)) {
                        Err(JsError::new_range(env))
                    } else {
                        new_len_desc.value = Some(env.new_number(new_len as f64));
                        
                        if new_len >= old_len {
                            self.define_own_object_property(
                                env,
                                name::LENGTH,
                                new_len_desc,
                                throw
                            )
                        } else if !old_len_desc.is_writable() {
                            if throw { Err(JsError::new_type(env, ::errors::TYPE_CANNOT_WRITE)) } else { Ok(false) }
                        } else {
                            let new_writable = if new_len_desc.writable == Some(false) {
                                new_len_desc.writable = Some(true);
                                false
                            } else {
                                true
                            };
                            
                            let succeeded = try!(self.define_own_object_property(
                                env,
                                name::LENGTH,
                                new_len_desc,
                                throw
                            ));
                            if !succeeded {
                                return if throw { Err(JsError::new_type(env, ::errors::TYPE_CANNOT_WRITE)) } else { Ok(false) };
                            }
                            
                            // The mechanism below ensures that we don't iterate over entries
                            // that do not exist in the array. This is specifically important
                            // for sparse arrays. This mechanism depends on the fact that
                            // entries are stored ordered, even for sparse arrays.
                            //
                            // TODO #69: This however can still be optimized. For one, when we find
                            // an index that matches i, we know that all names we get after that
                            // are equal to their name. We then don't have to call get_key
                            // anymore because we know what it's gonna say.
                            
                            let end = self.store.capacity(env);
                            
                            for i in (0..end).rev() {
                                let index = match self.store.get_key(env, i) {
                                    StoreKey::Missing => continue,
                                    StoreKey::Key(index, _) => index.index().unwrap(),
                                    _ => panic!("did not expect store key end")
                                };
                                
                                if index < new_len as usize {
                                    break;
                                }
                                
                                old_len = index as u32;
                                
                                let delete_succeeded = try!(self.delete(
                                    env,
                                    Name::from_index(old_len as usize),
                                    false
                                ));
                                
                                if !delete_succeeded {
                                    new_len_desc.value = Some(env.new_number((old_len + 1) as f64));
                                    if !new_writable {
                                        new_len_desc.writable = Some(false);
                                    }
                                    try!(self.define_own_object_property(
                                        env,
                                        name::LENGTH,
                                        new_len_desc,
                                        false
                                    ));
                                    
                                    return if throw { Err(JsError::new_type(env, ::errors::TYPE_CANNOT_WRITE)) } else { Ok(false) };
                                }
                            }
                            
                            if !new_writable {
                                try!(self.define_own_object_property(
                                    env,
                                    name::LENGTH,
                                    JsDescriptor {
                                        writable: Some(false),
                                        ..JsDescriptor::default()
                                    },
                                    false
                                ));
                            }
                            
                            Ok(true)
                        }
                    }
                }
            }
        } else {
            match property.index() {
                Some(index) => {
                    if index >= old_len as usize && !old_len_desc.is_writable() {
                        if throw { Err(JsError::new_type(env, ::errors::TYPE_CANNOT_WRITE)) } else { Ok(false) }
                    } else {
                        let succeeded = try!(self.define_own_object_property(
                            env,
                            property,
                            descriptor,
                            false
                        ));
                        
                        if !succeeded {
                            if throw { Err(JsError::new_type(env, ::errors::TYPE_CANNOT_WRITE)) } else { Ok(false) }
                        } else if index >= old_len as usize {
                            old_len_desc.value = Some(env.new_number((index + 1) as f64));
                            try!(self.define_own_object_property(
                                env,
                                name::LENGTH,
                                old_len_desc,
                                false
                            ));
                            
                            Ok(true)
                        } else {
                            self.define_own_object_property(
                                env,
                                property,
                                descriptor,
                                throw
                            )
                        }
                    }
                }
                None => {
                    self.define_own_object_property(
                        env,
                        property,
                        descriptor,
                        throw
                    )
                }
            }
        }
    }
}

impl JsItem for Local<JsObject> {
    fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
        env.new_object(*self)
    }

    // 8.12.1 [[GetOwnProperty]] (P)
    // 15.5.5.2 [[GetOwnProperty]] ( P )
    fn get_own_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
        let desc = self.store.get_value(env, property);
        
        if !desc.is_some() && self.value.ty() == JsType::String {
            self.value(env).get_own_property(env, property)
        } else {
            desc
        }
    }
    
    // 8.12.7 [[Delete]] (P, Throw)
    fn delete(&mut self, env: &mut JsEnv, property: Name, throw: bool) -> JsResult<bool> {
        if let Some(desc) = self.get_own_property(env, property) {
            if desc.is_configurable() {
                self.store.remove(env, property);
                Ok(true)
            } else if throw {
                Err(JsError::new_type(env, ::errors::TYPE_CANNOT_DELETE))
            } else {
                Ok(false)
            }
        } else {
            Ok(true)
        }
    }
    
    // 8.12.9 [[DefineOwnProperty]] (P, Desc, Throw)
    // 15.4.5.1 [[DefineOwnProperty]] ( P, Desc, Throw )
    fn define_own_property(&mut self, env: &mut JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
        if self.class == Some(name::ARRAY_CLASS) {
            self.define_own_array_property(env, property, descriptor, throw)
        } else {
            self.define_own_object_property(env, property, descriptor, throw)
        }
    }
    
    fn is_callable(&self, _: &JsEnv) -> bool {
        self.function.is_some()
    }
    
    fn can_construct(&self, _: &JsEnv) -> bool {
        match self.function {
            Some(JsFunction::Native(_, _, _, can_construct)) => can_construct,
            Some(..) => true,
            _ => false
        }
    }
    
    fn has_prototype(&self, _: &JsEnv) -> bool {
        !self.prototype.is_null()
    }
    
    fn prototype(&self, env: &JsEnv) -> Option<Local<JsValue>> {
        if self.prototype.is_null() {
            None
        } else {
            Some(env.new_object(self.prototype.as_local(env)))
        }
    }
    
    fn set_prototype(&mut self, _: &JsEnv, prototype: Option<Local<JsValue>>) {
        if let Some(prototype) = prototype {
            if prototype.ty() == JsType::Object {
                self.prototype = prototype.get_ptr();
            }
        } else {
            self.prototype = Ptr::null();
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
    // 15.3.4.5.3 [[HasInstance]] (V)
    fn has_instance(&self, env: &mut JsEnv, mut object: Local<JsValue>) -> JsResult<bool> {
        if self.function.is_none() {
            Err(JsError::new_type(env, ::errors::TYPE_CANNOT_HAS_INSTANCE))
        } else if object.ty() != JsType::Object {
            Ok(false)
        } else {
            let prototype = if *self.function.as_ref().unwrap() == JsFunction::Bound {
                let scope = self.scope(env).unwrap();
                let target = scope.get(env, 0);
                
                try!(target.get(env, name::PROTOTYPE))
            } else {
                try!(self.get(env, name::PROTOTYPE))
            };
            
            if prototype.ty() != JsType::Object {
                Err(JsError::new_type(env, ::errors::TYPE_CANNOT_HAS_INSTANCE))
            } else {
                loop {
                    if let Some(object_) = object.prototype(env) {
                        object = object_;
                        if prototype == object {
                            return Ok(true)
                        }
                    } else {
                        return Ok(false)
                    }
                }
            }
        }
    }
    
    fn scope(&self, env: &JsEnv) -> Option<Local<JsScope>> {
        if self.scope.is_null() {
            None
        } else {
            Some(env.new_scope(self.scope.as_local(env)).unwrap_scope(env))
        }
    }
    
    fn set_scope(&mut self, _: &JsEnv, scope: Option<Local<JsScope>>) {
        if let Some(scope) = scope {
            self.scope = scope.as_ptr();
        } else {
            self.scope = Ptr::null();
        }
    }
}

trait Store {
    fn add(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor);
    
    fn remove(&mut self, env: &JsEnv, name: Name);
    
    fn get_value(&self, env: &JsEnv, name: Name) -> Option<JsDescriptor>;
    
    fn replace(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor) -> bool;
    
    fn get_key(&self, env: &JsEnv, offset: usize) -> StoreKey;
    
    fn capacity(&self, env: &JsEnv) -> usize;
}

pub enum StoreKey {
    Key(Name, bool),
    Missing,
    End(usize)
}

pub enum JsStoreKey {
    Key(Name, bool),
    Missing,
    End
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum JsStoreType {
    Hash = 1,
    Array = 2
}

struct StorePtr {
    ty: JsStoreType,
    ptr: usize
}

impl StorePtr {
    unsafe fn new<T>(ptr: Ptr<T>, ty: JsStoreType) -> StorePtr {
        StorePtr {
            ty: ty,
            ptr: ptr.ptr() as usize
        }
    }
    
    fn get_ptr<T>(&self) -> Ptr<T> {
        Ptr::from_ptr(self.ptr as ptr_t)
    }
    
    fn as_hash(&self, env: &JsEnv) -> Local<HashStore> {
        assert_eq!(self.ty, JsStoreType::Hash);
        
        self.get_ptr::<HashStore>().as_local(env)
    }
    
    fn as_array(&self, env: &JsEnv) -> Local<ArrayStore> {
        assert_eq!(self.ty, JsStoreType::Array);
        
        self.get_ptr::<ArrayStore>().as_local(env)
    }
}

macro_rules! delegate {
    ( $target:expr, $env:expr, $method:ident ( $( $arg:expr ),* ) ) => {
        match $target.ty {
            JsStoreType::Hash => $target.as_hash($env).$method( $( $arg ),* ),
            JsStoreType::Array => $target.as_array($env).$method( $( $arg ),* ),
        }
    }
}

impl Store for StorePtr {
    fn add(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor) {
        delegate!(self, env, add(env, name, value))
    }
    
    fn remove(&mut self, env: &JsEnv, name: Name) {
        delegate!(self, env, remove(env, name))
    }
    
    fn get_value(&self, env: &JsEnv, name: Name) -> Option<JsDescriptor> {
        delegate!(self, env, get_value(env, name))
    }
    
    fn replace(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor) -> bool {
        delegate!(self, env, replace(env, name, value))
    }
    
    fn get_key(&self, env: &JsEnv, offset: usize) -> StoreKey {
        delegate!(self, env, get_key(env, offset))
    }
    
    fn capacity(&self, env: &JsEnv) -> usize {
        delegate!(self, env, capacity(env))
    }
}

const VALID        : u32 = 0b00001;
const WRITABLE     : u32 = 0b00010;
const ENUMERABLE   : u32 = 0b00100;
const CONFIGURABLE : u32 = 0b01000;
const ACCESSOR     : u32 = 0b10000;

#[derive(Copy, Clone)]
// Modifications to this struct must be synchronized with the GC walker.
pub struct Entry {
    name: Name,
    flags: u32,
    next: i32,
    value1: JsValue,
    value2: JsValue
}

impl Entry {
    fn empty() -> Entry {
        unsafe { zeroed() }
    }
    
    fn is_valid(&self) -> bool {
        (self.flags & VALID) != 0
    }
    
    fn is_writable(&self) -> bool {
        (self.flags & WRITABLE) != 0
    }
    
    fn is_enumerable(&self) -> bool {
        (self.flags & ENUMERABLE) != 0
    }
    
    fn is_configurable(&self) -> bool {
        (self.flags & CONFIGURABLE) != 0
    }
    
    fn is_accessor(&self) -> bool {
        (self.flags & ACCESSOR) != 0
    }
    
    fn from_descriptor(descriptor: &JsDescriptor, name: Name, next: i32) -> Entry {
        let flags = VALID |
            if descriptor.is_writable() { WRITABLE } else { 0 } |
            if descriptor.is_configurable() { CONFIGURABLE } else { 0 } |
            if descriptor.is_enumerable() { ENUMERABLE } else { 0 } |
            if descriptor.is_accessor() { ACCESSOR } else { 0 };
        
        let value1;
        let value2;
        
        if descriptor.is_accessor() {
            value1 = if let Some(get) = descriptor.get {
                *get
            } else {
                JsValue::new_undefined()
            };
            value2 = if let Some(set) = descriptor.set {
                *set
            } else {
                JsValue::new_undefined()
            };
        } else {
            value1 = if let Some(value) = descriptor.value {
                *value
            } else {
                JsValue::new_undefined()
            };
            value2 = JsValue::new_undefined();
        }
        
        Entry {
            name: name,
            flags: flags,
            next: next,
            value1: value1,
            value2: value2
        }
    }
}

impl Local<Entry> {
    fn as_property(&self, env: &JsEnv) -> JsDescriptor {
        if self.is_accessor() {
            let mut value1 = env.new_value();
            *value1 = self.value1;
            let mut value2 = env.new_value();
            *value2 = self.value2;
            
            JsDescriptor {
                value: None,
                get: Some(value1),
                set: Some(value2),
                writable: None,
                enumerable: Some(self.is_enumerable()),
                configurable: Some(self.is_configurable())
            }
        } else {
            let mut value = env.new_value();
            *value = self.value1;
            
            JsDescriptor {
                value: Some(value),
                get: None,
                set: None,
                writable: Some(self.is_writable()),
                enumerable: Some(self.is_enumerable()),
                configurable: Some(self.is_configurable())
            }
        }
    }
}

pub unsafe fn validate_walker(walker: &GcWalker) {
    validate_walker_for_object(walker);
    validate_walker_for_entry(walker);
    array_store::validate_walker_for_array_store(walker);
    hash_store::validate_walker_for_hash_store(walker);
    sparse_array::validate_walker(walker);
}

unsafe fn validate_walker_for_object(walker: &GcWalker) {
    let mut object : Box<JsObject> = Box::new(zeroed());
    let ptr = transmute::<_, ptr_t>(&*object);
    
    object.class = Some(Name::from_index(1));
    validate_walker_field(walker, GC_OBJECT, ptr, false);
    object.class = None;
    
    object.value = JsValue::new_bool(true);
    let value_offset = validate_walker_field(walker, GC_OBJECT, ptr, false);
    object.value = transmute(zeroed::<JsValue>());
    
    validate_walker_for_embedded_value(walker, ptr, GC_OBJECT, value_offset, &mut object.value);
    
    object.function = Some(JsFunction::Ir(FunctionRef(1)));
    validate_walker_field(walker, GC_OBJECT, ptr, false);
    object.function = None;
    
    object.prototype = Ptr::from_ptr(transmute(1usize));
    validate_walker_field(walker, GC_OBJECT, ptr, true);
    object.prototype = Ptr::null();
    
    object.scope = Ptr::from_ptr(transmute(1usize));
    validate_walker_field(walker, GC_OBJECT, ptr, true);
    object.scope = Ptr::null();
    
    object.store = StorePtr { ty: JsStoreType::Hash, ptr: 0 };
    validate_walker_field(walker, GC_OBJECT, ptr, false);
    object.store = transmute(zeroed::<StorePtr>());
    
    object.store.ptr = 1;
    validate_walker_field(walker, GC_OBJECT, ptr, true);
    object.store = transmute(zeroed::<StorePtr>());
    
    object.extensible = true;
    validate_walker_field(walker, GC_OBJECT, ptr, false);
    object.extensible = false;
    
    assert_eq!(size_of::<JsObject>(), 128);
}

unsafe fn validate_walker_for_entry(walker: &GcWalker) {
    let mut object : Box<Entry> = Box::new(zeroed());
    let ptr = transmute::<_, ptr_t>(&*object);
    
    object.name = Name::from_index(1);
    validate_walker_field(walker, GC_ENTRY, ptr, false);
    object.name = Name::from_index(0);
    
    object.flags = 1;
    validate_walker_field(walker, GC_ENTRY, ptr, false);
    object.flags = 0;
    
    object.next = 1;
    validate_walker_field(walker, GC_ENTRY, ptr, false);
    object.next = 0;
    
    object.value1 = JsValue::new_bool(true);
    let value_offset = validate_walker_field(walker, GC_ENTRY, ptr, false);
    object.value1 = transmute(zeroed::<JsValue>());
    
    validate_walker_for_embedded_value(walker, ptr, GC_ENTRY, value_offset, &mut object.value1);
    
    object.value2 = JsValue::new_bool(true);
    let value_offset = validate_walker_field(walker, GC_ENTRY, ptr, false);
    object.value2 = transmute(zeroed::<JsValue>());
    
    validate_walker_for_embedded_value(walker, ptr, GC_ENTRY, value_offset, &mut object.value2);
    
    assert_eq!(size_of::<Entry>(), 48);
}
