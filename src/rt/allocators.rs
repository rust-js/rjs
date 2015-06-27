use gc::{GcAllocator, Local, Root, Ptr, ArrayLocal, ArrayRoot, Array};
use rt::JsEnv;

impl GcAllocator for JsEnv {
    fn alloc_array_local_from_root<T>(&self, root: &ArrayRoot<T>) -> ArrayLocal<T> {
        self.heap.alloc_array_local_from_root(root)
    }
    
    fn alloc_array_root_from_local<T>(&self, local: ArrayLocal<T>) -> ArrayRoot<T> {
        self.heap.alloc_array_root_from_local(local)
    }
    
    fn alloc_array_local_from_array<T>(&self, array: Array<T>) -> ArrayLocal<T> {
        self.heap.alloc_array_local_from_array(array)
    }
    
    fn alloc_root_from_local<T>(&self, local: Local<T>) -> Root<T> {
        self.heap.alloc_root_from_local(local)
    }
    
    fn alloc_local_from_ptr<T>(&self, ptr: Ptr<T>) -> Local<T> {
        self.heap.alloc_local_from_ptr(ptr)
    }
    
    fn alloc_local_from_root<T>(&self, root: &Root<T>) -> Local<T> {
        self.heap.alloc_local_from_root(root)
    }
}
