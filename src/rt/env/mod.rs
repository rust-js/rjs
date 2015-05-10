#![allow(non_snake_case)]
#![allow(unused_variables)]

use super::{JsEnv, JsObject, JsFunction, JsFn, JsValue};
use super::hash::Property;
use ::JsResult;
use syntax::ast::Name;
use syntax::token::keywords;
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
			$target.props.add($name, &Property::new_value(function, true, false, true), $env);
		}
	}
}

macro_rules! accessor {
	( $target:expr, $name:expr , $get:ident , $set:ident , $prototype:expr , $env:expr ) => {
		{
			let get_function = new_function($env, Some($name), 0, Box::new($get), $prototype);
			let set_function = new_function($env, Some($name), 1, Box::new($set), $prototype);
			$target.props.add($name, &Property::new_accessor(get_function, set_function, true, false, true), $env);
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
	
	let mut object_prototype = JsObject::new_local(&env);
	env.object_prototype = Root::from_local(&env.heap, object_prototype).into_unsafe();
	
	// Build Function
	
	let mut function_prototype = new_function(env, None, 0, Box::new(Function_baseConstructor), &object_prototype).as_object(env);
	env.function_prototype = Root::from_local(&env.heap, function_prototype).into_unsafe();
	
	let function_class = new_function(env, Some(keywords::FUNCTION_CLASS), 0, Box::new(Function_constructor), &function_prototype);
	let mut function_class_object = function_class.as_object(env);
	function_class_object.class = Some(keywords::FUNCTION_CLASS);
	
	setup_function(env, &mut function_prototype, &function_class_object);
	
	env.global().props.add(keywords::FUNCTION_CLASS, &Property::new_value(function_class, true, false, true), env);
	
	// Build Object
	
	let object_class = setup_object(env, &mut object_prototype, &function_prototype);
	
	env.global().props.add(keywords::OBJECT_CLASS, &Property::new_value(object_class, true, false, true), env);
	
	// Build String
	
	setup_string(env, &function_prototype);
	
	// Build Date
	
	setup_date(env, &function_prototype);
	
	// Build global functions
	
	function!(env.global(), keywords::ESCAPE, Global_escape, 1, &function_prototype, env);
	function!(env.global(), keywords::UNESCAPE, Global_unescape, 1, &function_prototype, env);
}

fn setup_function(env: &JsEnv, prototype: &mut Local<JsObject>, class: &Local<JsObject>) {
	prototype.props.add(keywords::CONSTRUCTOR, &Property::new_value(class.as_value(env), true, false, true), env);
	
	function!(prototype, keywords::CALL, Function_call, 1, prototype, env);
	function!(prototype, keywords::APPLY, Function_apply, 2, prototype, env);
	function!(prototype, keywords::TO_STRING, Function_toString, 0, prototype, env);
	function!(prototype, keywords::TO_LOCALE_STRING, Function_toLocaleString, 0, prototype, env);
	accessor!(prototype, keywords::LENGTH, Function_length_get, Function_length_set, prototype, env);
}

fn setup_object<'a>(env: &JsEnv, prototype: &mut Local<JsObject>, function_prototype: &Local<JsObject>) -> Local<JsValue> {
	function!(prototype, keywords::TO_STRING, Object_toString, 0, function_prototype, env);
	function!(prototype, keywords::TO_LOCALE_STRING, Object_valueOf, 0, function_prototype, env);
	function!(prototype, keywords::VALUE_OF, Object_toString, 0, function_prototype, env);
	function!(prototype, keywords::HAS_OWN_PROPERTY, Object_hasOwnProperty, 1, function_prototype, env);
	function!(prototype, keywords::IS_PROTOTYPE_OF, Object_isPrototypeOf, 1, function_prototype, env);
	function!(prototype, keywords::PROPERTY_IS_ENUMERABLE, Object_propertyIsEnumerable, 1, function_prototype, env);
	function!(prototype, keywords::GET_PROTOTYPE_OF, Object_getPrototypeOf, 1, function_prototype, env);
	function!(prototype, keywords::DEFINE_PROPERTY, Object_defineProperty, 1, function_prototype, env);
	
	let class = new_function(env, Some(keywords::OBJECT_CLASS), 0, Box::new(Object_constructor), prototype);
	
	function!(class.get_object(), keywords::CREATE, Object_create, 1, function_prototype, env);
	function!(class.get_object(), keywords::GET_OWN_PROPERTY_DESCRIPTOR, Object_getOwnPropertyDescriptor, 2, function_prototype, env);
	
	class
}

fn setup_string<'a>(env: &mut JsEnv, function_prototype: &Local<JsObject>) {
	let class = new_function(env, Some(keywords::STRING_CLASS), 0, Box::new(String_constructor), &function_prototype);	

	env.global().props.add(keywords::STRING_CLASS, &Property::new_value(class, true, false, true), env);

	let mut prototype = class.get(keywords::PROTOTYPE, env).ok().unwrap().as_object(env);
	
	env.string_prototype = Root::from_local(&env.heap, prototype).into_unsafe();
	
	class.as_object(env).class = Some(keywords::STRING_CLASS);
	
	function!(&mut prototype, keywords::SUBSTR, String_substr, 1, function_prototype, env);
}

fn setup_date<'a>(env: &mut JsEnv, function_prototype: &Local<JsObject>) {
	let class = new_function(env, Some(keywords::DATE_CLASS), 0, Box::new(Date_constructor), &function_prototype);	

	env.global().props.add(keywords::DATE_CLASS, &Property::new_value(class, true, false, true), env);

	let mut prototype = class.get(keywords::PROTOTYPE, env).ok().unwrap().as_object(env);
	
	class.as_object(env).class = Some(keywords::DATE_CLASS);
	
	function!(&mut prototype, keywords::GET_YEAR, Date_getYear, 0, function_prototype, env);
	function!(&mut prototype, keywords::SET_YEAR, Date_setYear, 1, function_prototype, env);
	function!(&mut prototype, keywords::TO_GMT_STRING, Date_toGMTString, 0, function_prototype, env);
}

fn new_naked_function<'a>(env: &JsEnv, name: Option<Name>, args: u32, function: Box<JsFn>, prototype: &Local<JsObject>) -> Local<JsObject> {
	let mut result = JsObject::new_local(env);
	
	result.prototype = prototype.as_ptr();
	result.class = Some(keywords::FUNCTION_CLASS);
	result.function = Some(JsFunction::Native(name, args, function));
	
	result
}

// http://ecma-international.org/ecma-262/5.1/#sec-13.2
// TODO: INCOMPLETE
fn new_function<'a>(env: &JsEnv, name: Option<Name>, args: u32, function: Box<JsFn>, prototype: &Local<JsObject>) -> Local<JsValue> {
	let mut proto = JsObject::new_local(env);
	
	proto.class = Some(keywords::FUNCTION_CLASS);
	
	let mut result = new_naked_function(env, name, args, function, &prototype);
	let result_value = result.as_value(env);
	
	result.props.add(keywords::PROTOTYPE, &Property::new_value(proto.as_value(env), true, false, true), env);
	proto.props.add(keywords::CONSTRUCTOR, &Property::new_value(result_value, true, false, true), env);
	
	result_value
}
