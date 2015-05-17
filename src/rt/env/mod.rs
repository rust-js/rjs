#![allow(non_snake_case)]
#![allow(unused_variables)]

use super::{JsEnv, JsObject, JsFunction, JsFn, JsValue, JsDescriptor, JsItem};
use ::JsResult;
use syntax::Name;
use syntax::token::name;
use gc::*;

use self::global::*;
use self::object::*;
use self::function::*;
use self::string::*;
use self::date::*;

mod global;
mod object;
mod function;
mod string;
mod date;

macro_rules! function {
	( $target:expr, $name:expr , $function:ident , $arity:expr , $prototype:expr , $env:expr ) => {
		{
			let function = new_function($env, Some($name), $arity, Box::new($function), $prototype);
			$target.define_own_property($env, $name, JsDescriptor::new_value(function, true, false, true), false).ok();
		}
	}
}

macro_rules! accessor {
	( $target:expr, $name:expr , $get:ident , $set:ident , $prototype:expr , $env:expr ) => {
		{
			let get_function = new_function($env, Some($name), 0, Box::new($get), $prototype);
			let set_function = new_function($env, Some($name), 1, Box::new($set), $prototype);
			$target.define_own_property($env, $name, JsDescriptor::new_accessor(get_function, set_function, false, true), false).ok();
		}
	}
}

pub fn setup(ctx: &mut JsEnv) -> JsResult<()> {
	// Setup the native environment.
	
	setup_global(ctx);
	
	// Run setup to initialize the rest.
	
	try!(ctx.eval(include_str!("setup.js")));
	
	Ok(())
}

fn setup_global(env: &mut JsEnv) {
	let _scope = env.heap.new_local_scope();
	
	let mut global = env.global().as_value(env);
	
	let mut object_prototype = JsObject::new_local(&env);
	env.object_prototype = Root::from_local(&env.heap, object_prototype);
	
	// Build Function
	
	let mut function_prototype = new_function(env, None, 0, Box::new(Function_baseConstructor), &object_prototype).as_object(env);
	env.function_prototype = Root::from_local(&env.heap, function_prototype);
	
	let function_class = new_function(env, Some(name::FUNCTION_CLASS), 0, Box::new(Function_constructor), &function_prototype);
	let mut function_class_object = function_class.as_object(env);
	function_class_object.set_class(env, Some(name::FUNCTION_CLASS));
	
	setup_function(env, &mut function_prototype, &function_class_object);
	
	global.define_own_property(env, name::FUNCTION_CLASS, JsDescriptor::new_value(function_class, true, false, true), false).ok();
	
	// Build Object
	
	let object_class = setup_object(env, &mut object_prototype, &function_prototype);
	
	global.define_own_property(env, name::OBJECT_CLASS, JsDescriptor::new_value(object_class, true, false, true), false).ok();
	
	// Build String
	
	setup_string(env, global, &function_prototype);
	
	// Build Date
	
	setup_date(env, global, &function_prototype);
	
	// Build global functions
	
	function!(global, name::ESCAPE, Global_escape, 1, &function_prototype, env);
	function!(global, name::UNESCAPE, Global_unescape, 1, &function_prototype, env);
	function!(global, name::EVAL, Global_eval, 1, &function_prototype, env);
}

fn setup_function(env: &mut JsEnv, prototype: &mut Local<JsObject>, class: &Local<JsObject>) {
	let value = class.as_value(env);
	prototype.define_own_property(env, name::CONSTRUCTOR, JsDescriptor::new_value(value, true, false, true), false).ok();
	
	function!(prototype, name::CALL, Function_call, 1, prototype, env);
	function!(prototype, name::APPLY, Function_apply, 2, prototype, env);
	function!(prototype, name::TO_STRING, Function_toString, 0, prototype, env);
	function!(prototype, name::TO_LOCALE_STRING, Function_toLocaleString, 0, prototype, env);
	accessor!(prototype, name::LENGTH, Function_length_get, Function_length_set, prototype, env);
}

fn setup_object<'a>(env: &mut JsEnv, prototype: &mut Local<JsObject>, function_prototype: &Local<JsObject>) -> Local<JsValue> {
	function!(prototype, name::TO_STRING, Object_toString, 0, function_prototype, env);
	function!(prototype, name::TO_LOCALE_STRING, Object_valueOf, 0, function_prototype, env);
	function!(prototype, name::VALUE_OF, Object_toString, 0, function_prototype, env);
	function!(prototype, name::HAS_OWN_PROPERTY, Object_hasOwnProperty, 1, function_prototype, env);
	function!(prototype, name::IS_PROTOTYPE_OF, Object_isPrototypeOf, 1, function_prototype, env);
	function!(prototype, name::PROPERTY_IS_ENUMERABLE, Object_propertyIsEnumerable, 1, function_prototype, env);
	function!(prototype, name::GET_PROTOTYPE_OF, Object_getPrototypeOf, 1, function_prototype, env);
	function!(prototype, name::DEFINE_PROPERTY, Object_defineProperty, 1, function_prototype, env);
	
	let mut class = new_function(env, Some(name::OBJECT_CLASS), 0, Box::new(Object_constructor), prototype);
	
	function!(class, name::CREATE, Object_create, 1, function_prototype, env);
	function!(class, name::GET_OWN_PROPERTY_DESCRIPTOR, Object_getOwnPropertyDescriptor, 2, function_prototype, env);
	
	class
}

fn setup_string<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: &Local<JsObject>) {
	let mut class = new_function(env, Some(name::STRING_CLASS), 0, Box::new(String_constructor), &function_prototype);	

	global.define_own_property(env, name::STRING_CLASS, JsDescriptor::new_value(class, true, false, true), false).ok();

	let mut prototype = class.get(env, name::PROTOTYPE).ok().unwrap().as_object(env);
	
	env.string_prototype = Root::from_local(&env.heap, prototype);
	
	class.set_class(env, Some(name::STRING_CLASS));
	
	function!(&mut prototype, name::SUBSTR, String_substr, 1, function_prototype, env);
}

fn setup_date<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: &Local<JsObject>) {
	let mut class = new_function(env, Some(name::DATE_CLASS), 0, Box::new(Date_constructor), &function_prototype);	

	global.define_own_property(env, name::DATE_CLASS, JsDescriptor::new_value(class, true, false, true), false).ok();

	let mut prototype = class.get(env, name::PROTOTYPE).ok().unwrap().as_object(env);
	
	class.set_class(env, Some(name::DATE_CLASS));
	
	function!(&mut prototype, name::GET_YEAR, Date_getYear, 0, function_prototype, env);
	function!(&mut prototype, name::SET_YEAR, Date_setYear, 1, function_prototype, env);
	function!(&mut prototype, name::TO_GMT_STRING, Date_toGMTString, 0, function_prototype, env);
}

fn new_naked_function<'a>(env: &JsEnv, name: Option<Name>, args: u32, function: Box<JsFn>, prototype: &Local<JsObject>) -> Local<JsObject> {
	JsObject::new_function(env, JsFunction::Native(name, args, function), *prototype)
}

// http://ecma-international.org/ecma-262/5.1/#sec-13.2
// TODO: INCOMPLETE
fn new_function<'a>(env: &mut JsEnv, name: Option<Name>, args: u32, function: Box<JsFn>, prototype: &Local<JsObject>) -> Local<JsValue> {
	let mut proto = JsObject::new_local(env);
	
	proto.set_class(env, Some(name::FUNCTION_CLASS));
	
	let mut result = new_naked_function(env, name, args, function, &prototype);
	let result_value = result.as_value(env);
	
	let value = proto.as_value(env);
	result.define_own_property(env, name::PROTOTYPE, JsDescriptor::new_value(value, true, false, true), false).ok();
	proto.define_own_property(env, name::CONSTRUCTOR, JsDescriptor::new_value(result_value, true, false, true), false).ok();
	
	result_value
}
