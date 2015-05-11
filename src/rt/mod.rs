#![allow(dead_code)]

extern crate libc;

use gc::*;
use ir::IrContext;
use syntax::Name;
use syntax::ast::FunctionRef;
use std::mem;
use ::JsResult;
pub use self::value::JsValue;
pub use self::object::JsObject;
pub use self::string::JsString;

mod hash;
mod interpreter;
mod utf;
mod env;
mod runtime;
mod stack;
mod value;
mod object;
mod string;

struct Types {
	object: GcTypeId,
	object_entry: GcTypeId,
	string: GcTypeId,
	char: GcTypeId,
	value: GcTypeId
}

const VALUE_VALUE_OFFSET : u32 = 1;

fn value_walker(ptr: *const libc::c_void, index: u32) -> GcTypeWalk {
	if index < VALUE_VALUE_OFFSET {
		GcTypeWalk::Skip
	} else if index == VALUE_VALUE_OFFSET {
		let value = unsafe { mem::transmute::<_, &JsValue>(ptr) };
		
		match value.ty() {
			JsType::String | JsType::Object => GcTypeWalk::Pointer,
			_ => GcTypeWalk::End
		}
	} else {
		GcTypeWalk::End
	}
}

pub struct JsEnv {
	heap: GcHeap,
	types: Types,
	global: UnsafeRoot<JsObject>,
	object_prototype: UnsafeRoot<JsObject>,
	function_prototype: UnsafeRoot<JsObject>,
	string_prototype: UnsafeRoot<JsObject>,
	ir: IrContext,
	stack: stack::Stack
}

impl JsEnv {
	pub fn new() -> JsResult<JsEnv> {
		let mut heap = GcHeap::new(GcOpts::default());
		
		let types = Types { 
			object: heap.types().add(GcType::new(mem::size_of::<JsObject>(), GcTypeLayout::None)),
			object_entry: hash::build_entry_gc_type(&mut heap),
			string: heap.types().add(GcType::new(mem::size_of::<JsString>(), GcTypeLayout::None)),
			char: heap.types().add(GcType::new(mem::size_of::<u16>(), GcTypeLayout::None)),
			value: heap.types().add(GcType::new(mem::size_of::<JsValue>(), GcTypeLayout::Callback(Box::new(value_walker)))),
		};
		
		let global = heap.alloc_root::<JsObject>(types.object).into_unsafe();
		let object_prototype = heap.alloc_root::<JsObject>(types.object).into_unsafe();
		let function_prototype = heap.alloc_root::<JsObject>(types.object).into_unsafe();
		let string_prototype = heap.alloc_root::<JsObject>(types.object).into_unsafe();
		
		let mut env = JsEnv {
			heap: heap,
			types: types,
			global: global,
			object_prototype: object_prototype,
			function_prototype: function_prototype,
			string_prototype: string_prototype,
			ir: IrContext::new(),
			stack: stack::Stack::new()
		};
		
		*env.global = JsObject::new(&env);
		
		try!(env::setup(&mut env));
		
		Ok(env)
	}
	
	pub fn run(&mut self, file_name: &str) -> JsResult<Root<JsValue>> {
		let function_ref = try!(self.ir.parse_file(file_name));
		
		let mut ir = String::new();
		try!(self.ir.print_ir(&mut ir));
		println!("{}", ir);
		
		self.call(function_ref)
	}
	
	pub fn eval(&mut self, js: &str) -> JsResult<Root<JsValue>> {
		let function_ref = try!(self.ir.parse_string(js));
		
		let mut ir = String::new();
		try!(self.ir.print_ir(&mut ir));
		println!("{}", ir);
		
		self.call(function_ref)
	}
	
	fn call(&mut self, function_ref: FunctionRef) -> JsResult<Root<JsValue>> {
		println!("ENTER {}", if let Some(name) = self.ir.get_function_description(function_ref).name { self.ir.interner().get(name).to_string() } else { "(anonymous)".to_string() });
		
		let block = try!(self.ir.get_function_ir(function_ref));
		
		let mut result = self.alloc_root_value().into_unsafe();
		*result = try!(self.call_block(block, None, Vec::new()));
		
		println!("EXIT {}", if let Some(name) = self.ir.get_function_description(function_ref).name { self.ir.interner().get(name).to_string() } else { "(anonymous)".to_string() });
		
		Ok(Root::from_unsafe(&self.heap, result))
	}
	
	/// Returns a new GC handle to the global object.
	pub fn global(&self) -> Root<JsObject> {
		Root::from_unsafe(&self.heap, self.global.clone())
	}
	
	fn alloc_root_value(&self) -> Root<JsValue> {
		self.heap.alloc_root::<JsValue>(self.types.value)
	}
	
	fn alloc_local_value(&self) -> Local<JsValue> {
		self.heap.alloc_local::<JsValue>(self.types.value)
	}
	
	fn alloc_local_object(&self) -> Local<JsObject> {
		self.heap.alloc_local::<JsObject>(self.types.object)
	}
	
	fn alloc_local_string(&self) -> Local<JsString> {
		self.heap.alloc_local::<JsString>(self.types.string)
	}
	
	unsafe fn alloc_char_array(&self, size: usize) -> Array<u16> {
		self.heap.alloc_array::<u16>(self.types.char, size)
	}
	
	unsafe fn alloc_object_entry_array(&self, size: usize) -> Array<hash::Entry> {
		self.heap.alloc_array::<hash::Entry>(self.types.object_entry, size)
	}
	
	pub fn intern(&self, name: &str) -> Name {
		self.ir.interner().intern(name)
	}
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum JsType {
	Undefined = 0,
	Null = 1,
	Number = 2,
	Boolean = 3,
	String = 4,
	Object = 5
}

impl JsType {
	fn is_ptr(&self) -> bool {
		match *self {
			JsType::Object | JsType::String => true,
			_ => false
		}
	}
}

#[derive(Copy, Clone, PartialEq)]
pub enum JsFnMode {
	Call,
	New
}

pub struct JsArgs {
	function: Local<JsValue>,
	this: Local<JsValue>,
	args: Vec<Local<JsValue>>,
	mode: JsFnMode
}

pub type JsFn = Fn(&mut JsEnv, JsArgs) -> JsResult<Local<JsValue>>;

pub enum JsFunction {
	Ir(FunctionRef),
	Native(Option<Name>, u32, Box<JsFn>)
}
