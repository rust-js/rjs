#![allow(non_snake_case)]
#![allow(unused_variables)]

use super::{JsEnv, JsObject, JsFunction, JsFn, JsValue, JsDescriptor, JsItem, JsStoreType};
use ::JsResult;
use syntax::Name;
use syntax::token::name;
use gc::*;
use std::f64;

use self::global::*;
use self::object::*;
use self::function::*;
use self::array::*;
use self::string::*;
use self::date::*;
use self::number::*;
use self::boolean::*;
use self::regexp::*;

mod global;
mod object;
mod array;
mod function;
mod string;
mod date;
mod number;
mod boolean;
mod regexp;

macro_rules! function {
	( $target:expr , $name:expr , $function:ident , $arity:expr , $prototype:expr , $env:expr ) => {
		{
			let function = new_naked_function($env, Some($name), $arity, &$function, $prototype, false);
			$target.define_own_property($env, $name, JsDescriptor::new_value(function, true, false, true), false).ok();
		}
	}
}

macro_rules! accessor {
	( $target:expr , $name:expr , $get:ident , $set:ident , $prototype:expr , $env:expr ) => {
		{
			let get_function = new_naked_function($env, Some($name), 0, &$get, $prototype, false);
			let set_function = new_naked_function($env, Some($name), 1, &$set, $prototype, false);
			$target.define_own_property($env, $name, JsDescriptor::new_accessor(get_function, set_function, false, true), false).ok();
		}
	}
}

macro_rules! property {
	( $target:expr , $name:expr , $value:expr , $writable:expr , $enumerable:expr , $configurable:expr , $env:expr ) => {
		{
			let value = JsDescriptor::new_value($value, $writable, $enumerable, $configurable);
			$target.define_own_property($env, $name, value, false).ok();
		}
	}
}

macro_rules! value {
	( $target:expr , $name:expr , $value:expr , $writable:expr , $enumerable:expr , $configurable:expr , $env:expr ) => {
		property!($target, $name, $value.as_local($env), $writable, $enumerable, $configurable, $env)
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
	
	*env.global = JsObject::new(&env, JsStoreType::Hash);
	
	let mut global = env.global().as_value(env);
	
	let mut object_prototype = JsObject::new_local(&env, JsStoreType::Hash);
	env.object_prototype = Root::from_local(&env.heap, object_prototype);
	
	// Constructor setup.
	
	let function_prototype = setup_function(env, global, object_prototype);
	setup_object(env, global, &mut object_prototype, function_prototype);
	setup_array(env, global, function_prototype);
	setup_string(env, global, function_prototype);
	setup_date(env, global, function_prototype);
	setup_number(env, global, function_prototype);
	setup_boolean(env, global, function_prototype);
	setup_math(env, global, function_prototype);
	setup_regexp(env, global, function_prototype);
	setup_json(env, global, function_prototype);
	
	// Build global functions
	
	function!(global, name::ESCAPE, Global_escape, 1, function_prototype, env);
	function!(global, name::UNESCAPE, Global_unescape, 1, function_prototype, env);
	function!(global, name::PARSE_INT, Global_parseInt, 1, function_prototype, env);
	function!(global, name::PARSE_FLOAT, Global_parseFloat, 1, function_prototype, env);
	function!(global, name::IS_NAN, Global_isNaN, 1, function_prototype, env);
	function!(global, name::IS_FINITE, Global_isFinite, 1, function_prototype, env);
	function!(global, name::EVAL, Global_eval, 1, function_prototype, env);
	
	value!(global, name::NAN, JsValue::new_number(f64::NAN), false, false, false, env);
	value!(global, name::INFINITY, JsValue::new_number(f64::INFINITY), false, false, false, env);
	value!(global, name::UNDEFINED, JsValue::new_undefined(), true, false, true, env);
}

fn setup_function(env: &mut JsEnv, mut global: Local<JsValue>, object_prototype: Local<JsObject>) -> Local<JsObject> {
	let mut prototype = new_naked_function(env, None, 0, &Function_baseConstructor, object_prototype, false).as_object(env);
	env.function_prototype = Root::from_local(&env.heap, prototype);
	
	let class = new_naked_function(env, Some(name::FUNCTION_CLASS), 0, &Function_constructor, prototype, true);
	
	let mut class_object = class.as_object(env);

	let value = prototype.as_value(env);
	class_object.define_own_property(env, name::PROTOTYPE, JsDescriptor::new_value(value, true, false, true), false).ok();
	prototype.define_own_property(env, name::CONSTRUCTOR, JsDescriptor::new_value(class, true, false, true), false).ok();
	
	class_object.set_class(env, Some(name::FUNCTION_CLASS));
	
	property!(prototype, name::CONSTRUCTOR, class.as_value(env), true, false, true, env);
	
	function!(prototype, name::CALL, Function_call, 1, prototype, env);
	function!(prototype, name::APPLY, Function_apply, 2, prototype, env);
	function!(prototype, name::TO_STRING, Function_toString, 0, prototype, env);
	function!(prototype, name::TO_LOCALE_STRING, Function_toLocaleString, 0, prototype, env);
	
	property!(global, name::FUNCTION_CLASS, class.as_value(env), true, false, true, env);
	
	prototype
}

fn setup_object<'a>(env: &mut JsEnv, mut global: Local<JsValue>, prototype: &mut Local<JsObject>, function_prototype: Local<JsObject>) {
	function!(prototype, name::TO_STRING, Object_toString, 0, function_prototype, env);
	function!(prototype, name::TO_LOCALE_STRING, Object_valueOf, 0, function_prototype, env);
	function!(prototype, name::VALUE_OF, Object_toString, 0, function_prototype, env);
	function!(prototype, name::HAS_OWN_PROPERTY, Object_hasOwnProperty, 1, function_prototype, env);
	function!(prototype, name::IS_PROTOTYPE_OF, Object_isPrototypeOf, 1, function_prototype, env);
	function!(prototype, name::PROPERTY_IS_ENUMERABLE, Object_propertyIsEnumerable, 1, function_prototype, env);
	function!(prototype, name::GET_PROTOTYPE_OF, Object_getPrototypeOf, 1, function_prototype, env);
	function!(prototype, name::DEFINE_PROPERTY, Object_defineProperty, 1, function_prototype, env);
	
	let constructor = &Object_constructor;
	let class = JsObject::new_function(env, JsFunction::Native(Some(name::OBJECT_CLASS), 0, constructor as *const JsFn, true), function_prototype);
	let mut class = class.as_value(env);
	
	property!(global, name::OBJECT_CLASS, class, true, false, true, env);
		
	property!(class, name::PROTOTYPE, prototype.as_value(env), false, false, false, env);
	property!(prototype, name::CONSTRUCTOR, class, true, false, true, env);
	value!(class, name::LENGTH, JsValue::new_number(0f64), true, false, true, env);
	
	function!(class, name::CREATE, Object_create, 1, function_prototype, env);
	function!(class, name::GET_OWN_PROPERTY_DESCRIPTOR, Object_getOwnPropertyDescriptor, 2, function_prototype, env);
}

fn setup_array<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let mut class = env.new_native_function(Some(name::ARRAY_CLASS), 0, &Array_constructor, function_prototype);	
	
	property!(global, name::ARRAY_CLASS, class, true, false, true, env);

	let mut prototype = class.get(env, name::PROTOTYPE).ok().unwrap().as_object(env);

	function!(prototype, name::TO_STRING, Array_toString, 0, function_prototype, env);
	function!(prototype, name::TO_LOCALE_STRING, Array_toLocaleString, 0, function_prototype, env);
	function!(prototype, name::CONCAT, Array_concat, 1, function_prototype, env);
	function!(prototype, name::JOIN, Array_join, 1, function_prototype, env);
	function!(prototype, name::POP, Array_pop, 0, function_prototype, env);
	function!(prototype, name::PUSH, Array_push, 1, function_prototype, env);
	function!(prototype, name::REVERSE, Array_reverse, 0, function_prototype, env);
	function!(prototype, name::SHIFT, Array_shift, 0, function_prototype, env);
	function!(prototype, name::SLICE, Array_slice, 2, function_prototype, env);
	function!(prototype, name::SORT, Array_sort, 1, function_prototype, env);
	function!(prototype, name::SPLICE, Array_splice, 2, function_prototype, env);
	function!(prototype, name::UNSHIFT, Array_unshift, 1, function_prototype, env);
	function!(prototype, name::INDEX_OF, Array_indexOf, 1, function_prototype, env);
	function!(prototype, name::LAST_INDEX_OF, Array_lastIndexOf, 1, function_prototype, env);
	function!(prototype, name::EVERY, Array_every, 1, function_prototype, env);
	function!(prototype, name::SOME, Array_some, 1, function_prototype, env);
	function!(prototype, name::FOR_EACH, Array_forEach, 1, function_prototype, env);
	function!(prototype, name::MAP, Array_map, 1, function_prototype, env);
	function!(prototype, name::FILTER, Array_filter, 1, function_prototype, env);
	function!(prototype, name::REDUCE, Array_reduce, 1, function_prototype, env);
	function!(prototype, name::REDUCE_RIGHT, Array_reduceRight, 1, function_prototype, env);
	
	env.array_prototype = Root::from_local(&env.heap, prototype);
	
	function!(class, name::IS_ARRAY, Array_isArray, 1, function_prototype, env);
}

fn setup_string<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let class = env.new_native_function(Some(name::STRING_CLASS), 0, &String_constructor, function_prototype);	
	
	property!(global, name::STRING_CLASS, class, true, false, true, env);

	let mut prototype = class.get(env, name::PROTOTYPE).ok().unwrap().as_object(env);

	env.string_prototype = Root::from_local(&env.heap, prototype);
	
	function!(&mut prototype, name::SUBSTR, String_substr, 1, function_prototype, env);
	function!(&mut prototype, name::TO_STRING, String_toString, 0, function_prototype, env);
}

fn setup_date<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let class = env.new_native_function(Some(name::DATE_CLASS), 0, &Date_constructor, function_prototype);	
	
	property!(global, name::DATE_CLASS, class, true, false, true, env);

	let mut prototype = class.get(env, name::PROTOTYPE).ok().unwrap().as_object(env);
	
	env.date_prototype = Root::from_local(&env.heap, prototype);
	
	function!(&mut prototype, name::GET_YEAR, Date_getYear, 0, function_prototype, env);
	function!(&mut prototype, name::SET_YEAR, Date_setYear, 1, function_prototype, env);
	function!(&mut prototype, name::TO_GMT_STRING, Date_toGMTString, 0, function_prototype, env);
}

fn setup_number<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let mut class = env.new_native_function(Some(name::NUMBER_CLASS), 0, &Number_constructor, function_prototype);
	
	value!(class, name::MAX_VALUE, JsValue::new_number(f64::MAX), false, false, false, env);
	value!(class, name::MIN_VALUE, JsValue::new_number(f64::MIN), false, false, false, env);
	value!(class, name::NAN, JsValue::new_number(f64::NAN), false, false, false, env);
	value!(class, name::NEGATIVE_INFINITY, JsValue::new_number(f64::NEG_INFINITY), false, false, false, env);
	value!(class, name::POSITIVE_INFINITY, JsValue::new_number(f64::INFINITY), false, false, false, env);
	
	property!(global, name::NUMBER_CLASS, class, true, false, true, env);

	let mut prototype = class.get(env, name::PROTOTYPE).ok().unwrap().as_object(env);
	
	env.number_prototype = Root::from_local(&env.heap, prototype);

	function!(&mut prototype, name::VALUE_OF, Number_valueOf, 0, function_prototype, env);
	function!(&mut prototype, name::TO_STRING, Number_toString, 0, function_prototype, env);
}

fn setup_boolean<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let class = env.new_native_function(Some(name::BOOLEAN_CLASS), 0, &Boolean_constructor, function_prototype);
	
	property!(global, name::BOOLEAN_CLASS, class, true, false, true, env);
	
	let prototype = class.get(env, name::PROTOTYPE).ok().unwrap().as_object(env);
	
	env.boolean_prototype = Root::from_local(&env.heap, prototype);
}

fn setup_math<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let mut class = JsObject::new_local(env, JsStoreType::Hash);
	
	class.set_class(env, Some(name::MATH_CLASS));
	
	property!(global, name::MATH_CLASS, class.as_value(env), true, false, true, env);
}

fn setup_regexp<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let class = env.new_native_function(Some(name::REGEXP_CLASS), 0, &RegExp_constructor, function_prototype);
	
	property!(global, name::REGEXP_CLASS, class, true, false, true, env);
	
	let prototype = class.get(env, name::PROTOTYPE).ok().unwrap().as_object(env);
	
	env.regexp_prototype = Root::from_local(&env.heap, prototype);
}

fn setup_json<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let mut class = JsObject::new_local(env, JsStoreType::Hash);
	
	class.set_class(env, Some(name::JSON_CLASS));
	
	property!(global, name::JSON_CLASS, class.as_value(env), true, false, true, env);
}

fn new_naked_function<'a>(env: &mut JsEnv, name: Option<Name>, args: u32, function: &JsFn, prototype: Local<JsObject>, can_construct: bool) -> Local<JsValue> {
	JsObject::new_function(env, JsFunction::Native(name, args, function as *const JsFn, can_construct), prototype).as_value(env)
}
