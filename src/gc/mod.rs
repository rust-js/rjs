// TODO: The handles field of UnsafeRoot currently is a Rc. This is not preferable
// because of performance. However there is a problem. If the field is changed to
// a *const and the Rc is changed to a Box, a segmentation fault will occur.

const INITIAL_LOCAL_SCOPE_CAPACITY : usize = 8;

extern crate libc;
extern crate time;

use std::ops::{Deref, DerefMut, Index};
use std::marker::PhantomData;
use std::ptr;
use std::mem::{size_of, transmute, swap};
use std::cell::RefCell;
use std::slice;
use self::strategy::Strategy;
use self::strategy::copying::Copying;
use self::libc::c_void;
use std::fmt;
use std::rc::Rc;

pub mod os;
mod strategy;

#[macro_export]
macro_rules! field_offset {
	( $ty:ty, $ident:ident ) => {
		unsafe {
			use std::ptr;
			((&(& *(ptr::null::<$ty>() as *const $ty)).$ident) as *const _) as usize
		}
	}
}

pub struct Root<'a, T: 'a> {
	root: UnsafeRoot<T>,
	_type: PhantomData<&'a T>
}

impl<'a, T: 'a> Root<'a, T> {
	pub fn as_ptr(&self) -> Ptr<T> {
		self.root.as_ptr()
	}
	
	fn from_raw_parts(heap: &'a GcHeap, ptr: *const c_void) -> Root<'a, T> {
		Root {
			root: UnsafeRoot::from_raw_parts(heap, ptr),
			_type: PhantomData
		}
	}
	
	pub fn from_local(heap: &'a GcHeap, local: Local<T>) -> Root<'a, T> {
		Root::from_raw_parts(heap, unsafe { (*local.handle).ptr })
	}
	
	pub fn into_unsafe(self) -> UnsafeRoot<T> {
		self.root
	}
	
	pub fn from_unsafe(_heap: &'a GcHeap, root: UnsafeRoot<T>) -> Root<'a, T> {
		Root {
			root: root,
			_type: PhantomData
		}
	}
}

pub struct UnsafeRoot<T> {
	handles: Rc<RootHandles>,
	handle: u32,
	_type: PhantomData<T>
}

impl<T> UnsafeRoot<T> {
	pub fn as_ptr(&self) -> Ptr<T> {
		unsafe { Ptr::from_ptr(self.handles.get_target(self.handle)) }
	}
	
	fn from_raw_parts(heap: &GcHeap, ptr: *const c_void) -> UnsafeRoot<T> {
		UnsafeRoot {
			handles: heap.handles.clone(),
			handle: heap.handles.add(ptr),
			_type: PhantomData
		}
	}
}

impl<T> Deref for UnsafeRoot<T> {
	type Target = T;
	
	fn deref(&self) -> &T {
		unsafe {
			let ptr = self.handles.get_target(self.handle);
			transmute(ptr)
		}
	}
}

impl<T> DerefMut for UnsafeRoot<T> {
	fn deref_mut(&mut self) -> &mut T {
		unsafe { 
			let ptr = self.handles.get_target(self.handle);
			transmute(ptr)
		}
	}
}

impl<T> Clone for UnsafeRoot<T> {
	fn clone(&self) -> UnsafeRoot<T> {
		UnsafeRoot {
			handles: self.handles.clone(),
			handle: self.handles.clone_root(self.handle),
			_type: PhantomData
		}
	}
}

impl<T> Drop for UnsafeRoot<T> {
	fn drop(&mut self) {
		self.handles.remove(self.handle);
	}
}

impl<'a, T> Deref for Root<'a, T> {
	type Target = T;
	
	fn deref(&self) -> &T {
		&*self.root
	}
}

impl<'a, T> DerefMut for Root<'a, T> {
	fn deref_mut(&mut self) -> &mut T {
		&mut *self.root
	}
}

impl<'a, T> Clone for Root<'a, T> {
	fn clone(&self) -> Root<'a, T> {
		Root {
			root: self.root.clone(),
			_type: PhantomData
		}
	}
}

pub struct ArrayRoot<'a, T> {
	handles: &'a RootHandles,
	handle: u32,
	_type: PhantomData<T>
}

impl<'a, T> ArrayRoot<'a, T> {
	pub fn as_ptr(&self) -> Array<T> {
		unsafe {
			Array::from_ptr(self.handles.get_target(self.handle))
		}
	}
	
	fn from_raw_parts(heap: &'a GcHeap, ptr: *const c_void) -> ArrayRoot<'a, T> {
		ArrayRoot {
			handles: &heap.handles,
			handle: heap.handles.add(ptr),
			_type: PhantomData
		}
	}
	
	pub fn from_local(heap: &'a GcHeap, local: ArrayLocal<T>) -> ArrayRoot<'a, T> {
		ArrayRoot::from_raw_parts(heap, unsafe { (*local.handle).ptr })
	}
}

impl<'a, T> Deref for ArrayRoot<'a, T> {
	type Target = [T];
	
	fn deref(&self) -> &[T] {
		unsafe {
			let ptr = self.handles.get_target(self.handle);
			let size = *transmute::<_, *const usize>(ptr);
			let ptr = ptr.offset(size_of::<usize>() as isize);
			
			slice::from_raw_parts(
				transmute(ptr),
				size
			)
		}
	}
}

impl<'a, T> DerefMut for ArrayRoot<'a, T> {
	fn deref_mut(&mut self) -> &mut [T] {
		unsafe {
			let ptr = self.handles.get_target(self.handle);
			let size = *transmute::<_, *const usize>(ptr);
			let ptr = ptr.offset(size_of::<usize>() as isize);
			
			slice::from_raw_parts_mut(
				transmute(ptr),
				size
			)
		}
	}
}

impl<'a, T> Clone for ArrayRoot<'a, T> {
	fn clone(&self) -> ArrayRoot<'a, T> {
		ArrayRoot {
			handles: self.handles,
			handle: self.handles.clone_root(self.handle),
			_type: PhantomData
		}
	}
}

impl<'a, T> Drop for ArrayRoot<'a, T> {
	fn drop(&mut self) {
		self.handles.remove(self.handle);
	}
}

pub struct Local<T> {
	handle: *const Ptr<T>
}

impl<T> Local<T> {
	pub fn from_ptr(ptr: Ptr<T>, heap: &GcHeap) -> Local<T> {
		heap.alloc_local_from_ptr(ptr)
	}
	
	pub fn as_ptr(&self) -> Ptr<T> {
		unsafe { *self.handle }
	}
}

impl<T> Copy for Local<T> { }

impl<T> Clone for Local<T> {
	fn clone(&self) -> Local<T> {
		Local {
			handle: self.handle
		}
	}
}

impl<T> Deref for Local<T> {
	type Target = T;
	
	fn deref(&self) -> &T {
		unsafe { &**self.handle }
	}
}

impl<T> DerefMut for Local<T> {
	fn deref_mut(&mut self) -> &mut T {
		unsafe { &mut **(self.handle as *mut Ptr<T>) }
	}
}

pub struct ArrayLocal<T> {
	handle: *const Array<T>
}

impl<T> ArrayLocal<T> {
	pub fn from_ptr(ptr: Array<T>, heap: &GcHeap) -> ArrayLocal<T> {
		heap.alloc_array_local_from_ptr(ptr)
	}
	
	pub fn as_ptr(&self) -> Array<T> {
		unsafe { *self.handle }
	}
}

impl<T> Copy for ArrayLocal<T> { }

impl<T> Clone for ArrayLocal<T> {
	fn clone(&self) -> ArrayLocal<T> {
		ArrayLocal {
			handle: self.handle
		}
	}
}

impl<T> Deref for ArrayLocal<T> {
	type Target = [T];
	
	fn deref(&self) -> &[T] {
		unsafe { &**self.handle }
	}
}

impl<T> DerefMut for ArrayLocal<T> {
	fn deref_mut(&mut self) -> &mut [T] {
		unsafe { &mut **(self.handle as *mut Array<T>) }
	}
}

pub struct LocalScope {
	heap: *const GcHeap,
	index: usize
}

impl Drop for LocalScope {
	fn drop(&mut self) {
		unsafe { &*self.heap }.drop_current_scope(self.index);
	}
}

struct LocalScopeData {
	current: Vec<*const c_void>,
	handles: Vec<Vec<*const c_void>>
}

impl LocalScopeData {
	fn new() -> LocalScopeData {
		LocalScopeData {
			current: Vec::with_capacity(INITIAL_LOCAL_SCOPE_CAPACITY),
			handles: Vec::new()
		}
	}
	
	fn add(&mut self, ptr: *const c_void) -> *const *const c_void {
		if self.current.len() == self.current.capacity() {
			self.grow();
		}
		
		let index = self.current.len();
		self.current.push(ptr);
		
		unsafe { (*self.current).as_ptr().offset(index as isize) }
	}
	
	fn grow(&mut self) {
		let mut new = Vec::with_capacity(self.current.capacity() * 2);
		swap(&mut new, &mut self.current);
		self.handles.push(new);
	}
}

pub struct Ptr<T> {
	ptr: *const c_void,
	_type: PhantomData<T>
}

impl<T> PartialEq for Ptr<T> {
	fn eq(&self, other: &Ptr<T>) -> bool {
		self.ptr == other.ptr
	}
}

impl<T> Copy for Ptr<T> { }

impl<T> Clone for Ptr<T> {
	fn clone(&self) -> Ptr<T> {
		Self::from_ptr(self.ptr)
	}
}

impl<T> fmt::Debug for Ptr<T> {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		write!(fmt, "Ptr {{ ptr: {:?} }}", self.ptr)
	}
}

impl<T> Ptr<T> {
	pub fn as_ptr(&self) -> *const c_void {
		self.ptr
	}
	
	pub fn from_ptr(ptr: *const c_void) -> Ptr<T> {
		Ptr {
			ptr: ptr,
			_type: PhantomData
		}
	}
	
	pub fn null() -> Ptr<T> {
		Self::from_ptr(ptr::null())
	}
	
	pub fn is_null(&self) -> bool {
		self.ptr.is_null()
	}
}

impl<T> Deref for Ptr<T> {
	type Target = T;
	
	fn deref(&self) -> &T {
		unsafe { transmute(self.ptr) }
	}
}

impl<T> DerefMut for Ptr<T> {
	fn deref_mut(&mut self) -> &mut T {
		unsafe { transmute(self.ptr) }
	}
}

pub struct Array<T> {
	ptr: *const c_void,
	_type: PhantomData<T>
}

impl<T> Copy for Array<T> { }

impl<T> Clone for Array<T> {
	fn clone(&self) -> Array<T> {
		Self::from_ptr(self.ptr)
	}
}

impl<T> fmt::Debug for Array<T> {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		write!(fmt, "Ptr {{ ptr: {:?} }}", self.ptr)
	}
}

impl<T> Array<T> {
	pub fn as_ptr(&self) -> *const c_void {
		self.ptr
	}
	
	pub fn from_ptr(ptr: *const c_void) -> Array<T> {
		Array {
			ptr: ptr,
			_type: PhantomData
		}
	}
	
	pub fn null() -> Array<T> {
		Self::from_ptr(ptr::null())
	}
	
	pub fn is_null(&self) -> bool {
		self.ptr.is_null()
	}
}

impl<T> Deref for Array<T> {
	type Target = [T];
	
	fn deref(&self) -> &[T] {
		unsafe {
			let size = *transmute::<_, *const usize>(self.ptr);
			let ptr = self.ptr.offset(size_of::<usize>() as isize);
			
			slice::from_raw_parts(
				transmute(ptr),
				size
			)
		}
	}
}

impl<T> DerefMut for Array<T> {
	fn deref_mut(&mut self) -> &mut [T] {
		unsafe {
			let size = *transmute::<_, *const usize>(self.ptr);
			let ptr = self.ptr.offset(size_of::<usize>() as isize);
			
			slice::from_raw_parts_mut(
				transmute(ptr),
				size
			)
		}
	}
}

pub struct GcOpts {
	pub initial_heap: usize,
	pub slow_growth_factor: f64,
	pub fast_growth_factor: f64
}

impl GcOpts {
	pub fn default() -> GcOpts {
		GcOpts {
			initial_heap: 16 * 1024 * 1024, // 16M
			slow_growth_factor: 1.5f64,
			fast_growth_factor: 3f64
		}
	}
}

struct RootHandles {
	data: RefCell<RootHandlesData>
}

struct RootHandlesData {
	ptrs: Vec<*const c_void>,
	free: Vec<u32>
}

impl RootHandles {
	fn new() -> RootHandles {
		RootHandles {
			data: RefCell::new(RootHandlesData {
				ptrs: Vec::new(),
				free: Vec::new()
			})
		}
	}
	
	fn add(&self, ptr: *const c_void) -> u32 {
		let mut data = self.data.borrow_mut();
		
		let index = if let Some(index) = data.free.pop() {
			assert_eq!(data.ptrs[index as usize], ptr::null());
			
			data.ptrs[index as usize] = ptr;
			index
		} else {
			let index = data.ptrs.len() as u32;
			data.ptrs.push(ptr);
			index
		};
		
		index
	}
	
	fn remove(&self, handle: u32) -> *const c_void {
		let mut data = self.data.borrow_mut();
		
		data.free.push(handle);
		let ptr = data.ptrs[handle as usize];
		data.ptrs[handle as usize] = ptr::null();
		
		ptr
	}
	
	fn clone_root(&self, handle: u32) -> u32 {
		let ptr = self.data.borrow().ptrs[handle as usize];
		self.add(ptr)
	}
	
	unsafe fn get_target(&self, handle: u32) -> *const c_void {
		self.data.borrow().ptrs[handle as usize]
	}
}

struct GcMemHeader {
	header: usize
}

impl GcMemHeader {
	fn new(ty: u32, size: usize, is_array: bool) -> GcMemHeader {
		let mut header =
			(ty & 0x7f) << 1 |
			(size as u32 & 0xffffff) << 8;
		
		if is_array {
			header |= 1;
		}
		
		GcMemHeader {
			header: header as usize
		}
	}
	
	#[inline(always)]
	fn get_type_id(&self) -> u32 {
		(self.header >> 1) as u32 & 0x7f
	}
	
	fn get_size(&self) -> usize {
		self.header >> 8 & 0xffffff
	}
	
	fn is_array(&self) -> bool {
		self.header & 1 != 0
	}
	
	unsafe fn from_ptr<'a>(ptr: *const c_void) -> &'a mut GcMemHeader {
		transmute(ptr.offset(-(size_of::<GcMemHeader>() as isize)))
	}
}

pub struct GcHeap {
	handles: Rc<RootHandles>,
	heap: RefCell<Copying>,
	scopes: RefCell<Vec<LocalScopeData>>,
	walker: Box<GcWalker>
}

impl GcHeap {
	pub fn new(walker: Box<GcWalker>, opts: GcOpts) -> GcHeap {
		if opts.fast_growth_factor <= 1f64 {
			panic!("fast_growth_factor must be more than 1");
		}
		if opts.slow_growth_factor <= 1f64 {
			panic!("slow_growth_factor must be more than 1");
		}
		
		GcHeap {
			handles: Rc::new(RootHandles::new()),
			heap: RefCell::new(Copying::new(opts)),
			scopes: RefCell::new(Vec::new()),
			walker: walker
		}
	}
	
	unsafe fn alloc_raw(&self, size: usize) -> *const c_void {
		let mut ptr = self.heap.borrow_mut().alloc_raw(size);
		if ptr.is_null() {
			self.gc();
			
			ptr = self.heap.borrow_mut().alloc_raw(size);
			if ptr.is_null() {
				panic!("Could not allocate memory after GC");
			}
		}
		
		if ptr.is_null() {
			ptr
		} else {
			ptr.offset(size_of::<GcMemHeader>() as isize)
		}
	}
	
	pub unsafe fn alloc<T>(&self, ty: u32) -> Ptr<T> {
		let size = (size_of::<T>() + size_of::<usize>() - 1) / size_of::<usize>() * size_of::<usize>();
		
		let ptr = self.alloc_raw(
			size +
			size_of::<GcMemHeader>()
		);
		
		*GcMemHeader::from_ptr(ptr) = GcMemHeader::new(ty, size, false);
		
		Ptr::from_ptr(ptr)
	}
	
	pub fn alloc_root<T>(&self, ty: u32) -> Root<T> {
		Root::from_raw_parts(self, unsafe { self.alloc::<T>(ty).ptr })
	}
	
	pub fn alloc_local<T>(&self, ty: u32) -> Local<T> {
		self.alloc_local_from_ptr(unsafe { self.alloc::<T>(ty) })
	}
	
	fn alloc_local_from_ptr<T>(&self, ptr: Ptr<T>) -> Local<T> {
		let mut scopes = self.scopes.borrow_mut();
		let len = scopes.len();
		if len == 0 {
			panic!("No local scope present");
		}
		
		Local {
			handle: unsafe { transmute(scopes[len - 1].add(ptr.ptr)) }
		}
	}
	
	pub fn alloc_array_root<T>(&self, ty: u32, size: usize) -> ArrayRoot<T> {
		ArrayRoot::from_raw_parts(self, unsafe { self.alloc_array::<T>(ty, size).ptr })
	}
	
	pub fn alloc_array_local<T>(&self, ty: u32, size: usize) -> ArrayLocal<T> {
		self.alloc_array_local_from_ptr(unsafe { self.alloc_array::<T>(ty, size) })
	}
	
	fn alloc_array_local_from_ptr<T>(&self, ptr: Array<T>) -> ArrayLocal<T> {
		let mut scopes = self.scopes.borrow_mut();
		let len = scopes.len();
		if len == 0 {
			panic!("No local scope present");
		}
		
		ArrayLocal {
			handle: unsafe { transmute(scopes[len - 1].add(ptr.ptr)) }
		}
	}
	
	pub unsafe fn alloc_array<T>(&self, ty: u32, size: usize) -> Array<T> {
		let item_size = (size_of::<T>() + size_of::<usize>() - 1) / size_of::<usize>() * size_of::<usize>();
		
		let ptr = self.alloc_raw(
			size_of::<usize>() +
			(item_size * size) +
			size_of::<GcMemHeader>()
		);
		
		*GcMemHeader::from_ptr(ptr) = GcMemHeader::new(ty, item_size, true);
		*transmute::<_, *mut usize>(ptr) = size;
		
		Array::from_ptr(ptr)
	}
	
	pub fn gc(&self) {
		let mut handles = self.handles.data.borrow_mut();
		let scopes = self.scopes.borrow_mut();
		
		let mut walkers : Vec<Box<RootWalker>> = Vec::new();
		
		// Add the root handles walker if there are root handles.
		
		if handles.ptrs.len() != handles.free.len() {
			let ptr = (*handles.ptrs).as_mut_ptr();
			let end = unsafe { ptr.offset(handles.ptrs.len() as isize) };
			
			walkers.push(Box::new(RootHandlesWalker {
				ptr: ptr,
				end: end
			}));
		}
		
		// Add the local scopes walker if there are any.
		
		if scopes.len() > 0 {
			walkers.push(Box::new(LocalScopesWalker {
				scopes: &scopes,
				scope: 0,
				vec: 0,
				index: 0
			}));
		}
		
		self.heap.borrow_mut().gc(&mut walkers, &*self.walker);
	}
	
	pub fn mem_allocated(&self) -> usize {
		self.heap.borrow().mem_allocated()
	}
	
	pub fn mem_used(&self) -> usize {
		self.heap.borrow().mem_used()
	}
	
	pub fn new_local_scope(&self) -> LocalScope {
		let mut scopes = self.scopes.borrow_mut();
		
		let index = scopes.len();
		scopes.push(LocalScopeData::new());
		
		LocalScope {
			heap: self as *const GcHeap,
			index: index
		}
	}
	
	fn drop_current_scope(&self, index: usize) {
		let mut scopes = self.scopes.borrow_mut();
		
		if scopes.len() != index + 1 {
			panic!("Local scopes must be destoryed in the order they are created");
		}
		
		scopes.pop();
	}
}

trait RootWalker {
	unsafe fn next(&mut self) -> *mut *const c_void;
}
struct RootHandlesWalker {
	ptr: *mut *const c_void,
	end: *mut *const c_void
}

impl RootWalker for RootHandlesWalker {
	unsafe fn next(&mut self) -> *mut *const c_void {
		while self.ptr < self.end {
			let ptr = self.ptr;
			self.ptr = self.ptr.offset(1);
			
			if !(*ptr).is_null() {
				return ptr;
			}
		}
		
		ptr::null_mut()
	}
}

struct LocalScopesWalker<'a> {
	scopes: &'a [LocalScopeData],
	scope: usize,
	vec: usize,
	index: usize
}

impl<'a> RootWalker for LocalScopesWalker<'a> {
	unsafe fn next(&mut self) -> *mut *const c_void {
		if self.scope == self.scopes.len() {
			return ptr::null_mut();
		}
		
		let scope = &self.scopes[self.scope];
		
		let vec = if self.vec == 0 {
			&scope.current
		} else {
			&scope.handles[self.vec - 1]
		};
		
		let ptr = (*vec).as_ptr().offset(self.index as isize) as *mut *const c_void;
		
		self.index += 1;
		
		if self.index == vec.len() {
			self.vec += 1;
			self.index = 0;
			
			if self.vec - 1 == scope.handles.len() {
				self.vec = 0;
				self.scope += 1;
			}
		}
		
		ptr
	}
}

pub trait GcWalker {
	fn walk(&self, ty: u32, ptr: *const c_void, index: u32) -> GcWalk;
}

#[derive(Debug)]
pub enum GcWalk {
	Pointer,
	Skip,
	End
}
