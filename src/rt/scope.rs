use rt::{JsEnv, JsRawValue, JsValue, JsObject, JsItem, GC_SCOPE, GC_VALUE};
use gc::*;

// Modifications to this struct must be synchronized with the GC walker.
pub struct JsScope {
    items: Array<JsRawValue>
}

impl JsScope {
    pub fn new_local_thin(env: &JsEnv, size: usize, parent: Option<Local<JsScope>>) -> Local<JsScope> {
        let mut result = env.heap.alloc_local::<JsScope>(GC_SCOPE);
        
        unsafe {
            result.items = env.heap.alloc_array(GC_VALUE, size + 1);
        }
        
        if let Some(parent) = parent {
            result.raw_set(0, parent.as_value());
        }
        
        result
    }
    
    pub fn new_local_thick(env: &JsEnv, scope_object: Local<JsObject>, parent: Option<Local<JsScope>>, arguments: bool) -> Local<JsScope> {
        let mut result = env.heap.alloc_local::<JsScope>(GC_SCOPE);
        
        let size = 2 + if arguments { 1 } else { 0 };
        
        unsafe {
            result.items = env.heap.alloc_array(GC_VALUE, size);
        }
        
        if let Some(parent) = parent {
            result.raw_set(0, parent.as_value());
        }
        result.raw_set(1, scope_object.as_value());
        
        result
    }
}

impl Local<JsScope> {
    pub fn as_value(&self) -> JsValue {
        JsValue::new_scope(*self)
    }
    
    pub fn parent(&self, env: &JsEnv) -> Option<Local<JsScope>> {
        let parent = self.raw_get(env, 0);
        
        if parent.is_undefined() { None } else { Some(parent.unwrap_scope()) }
    }
    
    pub fn scope_object(&self, env: &JsEnv) -> Local<JsObject> {
        self.raw_get(env, 1).unwrap_object()
    }
    
    pub fn arguments(&self, env: &JsEnv) -> Option<JsValue> {
        if self.items.len() == 2 {
            None
        } else {
            Some(self.raw_get(env, 2))
        }
    }
    
    pub fn set_arguments(&mut self, arguments: JsValue) {
        if self.items.len() == 2 {
            panic!("scope does not have a slot to store arguments");
        }
        
        self.raw_set(2, arguments);
    }
    
    pub fn len(&self) -> usize {
        self.items.len() - 1
    }
    
    pub fn get(&self, env: &JsEnv, index: usize) -> JsValue {
        self.raw_get(env, index + 1)
    }
    
    pub fn set(&mut self, index: usize, value: JsValue) {
        self.raw_set(index + 1, value)
    }
    
    fn raw_get(&self, env: &JsEnv, index: usize) -> JsValue {
        self.items[index].as_value(env)
    }
    
    fn raw_set(&mut self, index: usize, value: JsValue) {
        self.items[index] = value.as_raw();
    }
}
