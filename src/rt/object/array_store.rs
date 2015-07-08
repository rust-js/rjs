use rt::{JsEnv, JsDescriptor, GC_ARRAY_STORE, GC_ENTRY};
use rt::validate_walker_field;
use rt::object::{Store, StoreKey, Entry};
use rt::object::hash_store::HashStore;
use syntax::Name;
use gc::{Local, GcWalker, GcAllocator, AsPtr, Ptr, ptr_t};
use std::mem::{transmute, zeroed, size_of};
use rt::object::sparse_array::SparseArray;

// Modifications to this struct must be synchronized with the GC walker.
#[repr(C)]
pub struct ArrayStore {
    array: Ptr<SparseArray>,
    props: Ptr<HashStore>
}

impl ArrayStore {
    pub fn new_local(env: &JsEnv) -> Local<ArrayStore> {
        let mut store = env.heap.alloc_local::<ArrayStore>(GC_ARRAY_STORE);
        let props = HashStore::new_local(env);
        let array = SparseArray::new_local(env);

        *store = ArrayStore {
            array: array.as_ptr(),
            props: props.as_ptr()
        };
        
        store
    }
}

impl Local<ArrayStore> {
    fn props<T: GcAllocator>(&self, allocator: &T) -> Local<HashStore> {
        self.props.as_local(allocator)
    }
    
    fn array<T: GcAllocator>(&self, allocator: &T) -> Local<SparseArray> {
        self.array.as_local(allocator)
    }
}

impl Store for Local<ArrayStore> {
    fn add(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor) {
        if let Some(index) = name.index() {
            let mut entry = env.heap.alloc_local(GC_ENTRY);
            
            *entry = Entry::from_descriptor(value, name, -1);
            
            self.array(env).set_value(env, index, entry);
        } else {
            self.props(env).add(env, name, value);
        }
    }
    
    fn remove(&mut self, env: &JsEnv, name: Name) {
        if let Some(index) = name.index() {
            let mut entry = env.heap.alloc_local(GC_ENTRY);
            
            *entry = Entry::empty();
            
            self.array(env).set_value(env, index, entry);
        } else {
            self.props(env).remove(env, name);
        }
    }
    
    fn get_value(&self, env: &JsEnv, name: Name) -> Option<JsDescriptor> {
        if let Some(index) = name.index() {
            let mut entry = env.heap.alloc_local(GC_ENTRY);
            
            *entry = self.array(env).get_value(index);
            
            if entry.is_valid() {
                Some(entry.as_property(env))
            } else {
                None
            }
        } else {
            self.props(env).get_value(env, name)
        }
    }
    
    fn replace(&mut self, env: &JsEnv, name: Name, value: &JsDescriptor) -> bool {
        if let Some(index) = name.index() {
            let mut array = self.array(env);
            
            if array.get_value(index).is_valid() {
                let mut entry = env.heap.alloc_local(GC_ENTRY);
                *entry = Entry::from_descriptor(value, name, -1);
                array.set_value(env, index, entry);
                return true;
            }
            
            false
        } else {
            self.props(env).replace(env, name, value)
        }
    }
    
    fn get_key(&self, env: &JsEnv, offset: usize) -> StoreKey {
        let array = self.array(env);
        let count = array.capacity();
        
        if offset < count {
            self.array(env).get_key(offset)
        } else {
            match self.props(env).get_key(env, offset - count) {
                StoreKey::End(end) => StoreKey::End(end + count),
                result @ _ => result
            }
        }
    }
    
    fn capacity(&self, env: &JsEnv) -> usize {
        self.array(env).capacity()
    }
}

pub unsafe fn validate_walker_for_array_store(walker: &GcWalker) {
    let mut object : Box<ArrayStore> = Box::new(zeroed());
    let ptr = transmute::<_, ptr_t>(&*object);
    
    object.array = Ptr::from_ptr(transmute(1usize));
    validate_walker_field(walker, GC_ARRAY_STORE, ptr, true);
    object.array = Ptr::null();
    
    object.props = Ptr::from_ptr(transmute(1usize));
    validate_walker_field(walker, GC_ARRAY_STORE, ptr, true);
    object.props = Ptr::null();
    
    assert_eq!(size_of::<ArrayStore>(), 16);
}
