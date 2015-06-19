#![allow(non_snake_case)]

use rt::{JsEnv, JsObject, JsFunction, JsFn, JsFnRef, JsValue, JsDescriptor, JsItem};
use rt::{JsStoreType, JsScope, JsString, JsHandle};
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
use self::math::*;
use self::console::*;
use self::error::*;

mod global;
mod object;
mod array;
mod function;
mod string;
mod date;
mod number;
mod boolean;
mod regexp;
mod math;
mod console;
mod error;

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
		property!($target, $name, $value, $writable, $enumerable, $configurable, $env)
	}
}

pub fn setup(env: &mut JsEnv) -> JsResult<()> {
	setup_global(env);
	
	Ok(())
}

fn setup_global(env: &mut JsEnv) {
	let _scope = env.new_local_scope();
	
	let global = JsObject::new_local(&env, JsStoreType::Hash);
	env.add_handle(JsHandle::Global, global);
	
	env.global_scope = {
		let global = env.handle(JsHandle::Global);
		let global_scope = JsScope::new_local_thick(&env, global, None, true);
		global_scope.as_root(env)
	};
	
	let mut global = env.handle(JsHandle::Global).as_value(env);
	
	let mut object_prototype = JsObject::new_local(&env, JsStoreType::Hash);
	env.add_handle(JsHandle::Object, object_prototype);
	
	global.set_prototype(env, Some(object_prototype.as_value(env)));
	global.set_class(env, Some(name::OBJECT_CLASS));
	
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
	setup_console(env, global, function_prototype);
	setup_error(env, global, function_prototype);
	
	// Build global functions
	
	function!(global, name::ESCAPE, Global_escape, 1, function_prototype, env);
	function!(global, name::UNESCAPE, Global_unescape, 1, function_prototype, env);
	function!(global, name::PARSE_INT, Global_parseInt, 1, function_prototype, env);
	function!(global, name::PARSE_FLOAT, Global_parseFloat, 1, function_prototype, env);
	function!(global, name::IS_NAN, Global_isNaN, 1, function_prototype, env);
	function!(global, name::IS_FINITE, Global_isFinite, 1, function_prototype, env);
	function!(global, name::EVAL, Global_eval, 1, function_prototype, env);
	function!(global, name::DECODE_URI, Global_decodeURI, 1, function_prototype, env);
	function!(global, name::DECODE_URI_COMPONENT, Global_decodeURIComponent, 1, function_prototype, env);
	function!(global, name::ENCODE_URI, Global_encodeURI, 1, function_prototype, env);
	function!(global, name::ENCODE_URI_COMPONENT, Global_encodeURIComponent, 1, function_prototype, env);
	
	value!(global, name::NAN, env.new_number(f64::NAN), false, false, false, env);
	value!(global, name::INFINITY, env.new_number(f64::INFINITY), false, false, false, env);
	value!(global, name::UNDEFINED, env.new_undefined(), false, false, false, env);
}

fn setup_function(env: &mut JsEnv, mut global: Local<JsValue>, object_prototype: Local<JsObject>) -> Local<JsObject> {
	let mut prototype = new_naked_function(env, None, 0, &Function_baseConstructor, object_prototype, false).unwrap_object(env);
	env.add_handle(JsHandle::Function, prototype);
	
	let class = new_naked_function(env, Some(name::FUNCTION_CLASS), 1, &Function_constructor, prototype, true);
	
	let mut class_object = class.unwrap_object(env);

	let value = prototype.as_value(env);
	class_object.define_own_property(env, name::PROTOTYPE, JsDescriptor::new_value(value, false, false, false), false).ok();
	prototype.define_own_property(env, name::CONSTRUCTOR, JsDescriptor::new_value(class, true, false, true), false).ok();
	
	class_object.set_class(env, Some(name::FUNCTION_CLASS));
	
	property!(prototype, name::CONSTRUCTOR, class.as_value(env), true, false, true, env);
	
	function!(prototype, name::CALL, Function_call, 1, prototype, env);
	function!(prototype, name::APPLY, Function_apply, 2, prototype, env);
	function!(prototype, name::BIND, Function_bind, 1, prototype, env);
	function!(prototype, name::TO_STRING, Function_toString, 0, prototype, env);
	function!(prototype, name::TO_LOCALE_STRING, Function_toLocaleString, 0, prototype, env);
	
	property!(global, name::FUNCTION_CLASS, class.as_value(env), true, false, true, env);
	
	prototype
}

fn setup_object<'a>(env: &mut JsEnv, mut global: Local<JsValue>, prototype: &mut Local<JsObject>, function_prototype: Local<JsObject>) {
	function!(prototype, name::TO_STRING, Object_toString, 0, function_prototype, env);
	function!(prototype, name::TO_LOCALE_STRING, Object_toLocaleString, 0, function_prototype, env);
	function!(prototype, name::VALUE_OF, Object_valueOf, 0, function_prototype, env);
	function!(prototype, name::HAS_OWN_PROPERTY, Object_hasOwnProperty, 1, function_prototype, env);
	function!(prototype, name::IS_PROTOTYPE_OF, Object_isPrototypeOf, 1, function_prototype, env);
	function!(prototype, name::PROPERTY_IS_ENUMERABLE, Object_propertyIsEnumerable, 1, function_prototype, env);
	function!(prototype, name::GET_PROTOTYPE_OF, Object_getPrototypeOf, 1, function_prototype, env);
	function!(prototype, name::DEFINE_PROPERTY, Object_defineProperty, 1, function_prototype, env);
	function!(prototype, name::PREVENT_EXTENSIONS, Object_preventExtensions, 1, function_prototype, env);
	
	let class = JsObject::new_function(env, JsFunction::Native(Some(name::OBJECT_CLASS), 1, JsFnRef::new(&Object_constructor), true), function_prototype, false);
	let mut class = class.as_value(env);
	
	property!(global, name::OBJECT_CLASS, class, true, false, true, env);
		
	property!(class, name::PROTOTYPE, prototype.as_value(env), false, false, false, env);
	property!(prototype, name::CONSTRUCTOR, class, true, false, true, env);
	
	function!(class, name::CREATE, Object_create, 1, function_prototype, env);
	function!(class, name::GET_OWN_PROPERTY_DESCRIPTOR, Object_getOwnPropertyDescriptor, 2, function_prototype, env);
	function!(class, name::GET_OWN_PROPERTY_NAMES, Object_getOwnPropertyNames, 1, function_prototype, env);
	function!(class, name::FREEZE, Object_freeze, 1, function_prototype, env);
	function!(class, name::IS_EXTENSIBLE, Object_isExtensible, 1, function_prototype, env);
}

fn setup_array<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	// This is a bit of a mess. Array.prototype is itself an array.
	// To get this to work, we duplicate JsEnv.new_native_function here.
	// There are two changes compared to the normal implementation.
	// First, the prototype property is set to a new array. However,
	// this is not a completely normal array since we set [[Prototype]]
	// of this array to Object.[[Prototype]]. Next we create the class
	// the same was as we normally do except that the prototype
	// property is set to our array object. [[Prototype]] is still
	// the function prototype.
	
	// Create the array instance used as the value for the prototype
	// property.
	
	let mut prototype = JsObject::new_local(env, JsStoreType::Array);
	
	let length = env.new_number(0f64);
	prototype.define_own_property(
		env,
		name::LENGTH,
		JsDescriptor::new_value(length, true, false, false),
		false
	).ok();
	
	let array_prototype = prototype.as_value(env);
	// Set the [[Prototype]] value as usual.
	prototype.set_prototype(env, Some(env.handle(JsHandle::Object).as_value(env)));
	prototype.set_class(env, Some(name::ARRAY_CLASS));
	
	// Create the class as usual.
	let mut class = JsObject::new_function(env, JsFunction::Native(Some(name::ARRAY_CLASS), 1, JsFnRef::new(&Array_constructor), true), function_prototype, false).as_value(env);
	
	// But set the prototype to our array intance.
	class.define_own_property(env, name::PROTOTYPE, JsDescriptor::new_value(array_prototype, false, false, false), false).ok();
	prototype.define_own_property(env, name::CONSTRUCTOR, JsDescriptor::new_value(class, true, false, true), false).ok();
	
	// And we have a valid Array class.
	
	property!(global, name::ARRAY_CLASS, class, true, false, true, env);

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
	
	env.add_handle(JsHandle::Array, prototype);
	
	function!(class, name::IS_ARRAY, Array_isArray, 1, function_prototype, env);
}

fn setup_string<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let mut class = env.new_native_function(Some(name::STRING_CLASS), 1, &String_constructor, function_prototype);	
	
	function!(class, name::FROM_CHAR_CODE, String_fromCharCode, 0, function_prototype, env);
	
	property!(global, name::STRING_CLASS, class, true, false, true, env);

	let mut prototype = class.get(env, name::PROTOTYPE).ok().unwrap().unwrap_object(env);

	env.add_handle(JsHandle::String, prototype);
	
	function!(&mut prototype, name::SUBSTR, String_substr, 1, function_prototype, env);
	function!(&mut prototype, name::TO_STRING, String_toString, 0, function_prototype, env);
	function!(&mut prototype, name::VALUE_OF, String_valueOf, 0, function_prototype, env);
	function!(&mut prototype, name::CHAR_AT, String_charAt, 1, function_prototype, env);
	function!(&mut prototype, name::CHAR_CODE_AT, String_charCodeAt, 1, function_prototype, env);
	function!(&mut prototype, name::INDEX_OF, String_indexOf, 1, function_prototype, env);
	function!(&mut prototype, name::LAST_INDEX_OF, String_lastIndexOf, 1, function_prototype, env);
	function!(&mut prototype, name::SUBSTRING, String_substring, 2, function_prototype, env);
	function!(&mut prototype, name::TO_LOWER_CASE, String_toLowerCase, 0, function_prototype, env);
	function!(&mut prototype, name::TO_LOCALE_LOWER_CASE, String_toLocaleLowerCase, 0, function_prototype, env);
	function!(&mut prototype, name::TO_UPPER_CASE, String_toUpperCase, 0, function_prototype, env);
	function!(&mut prototype, name::TO_LOCALE_UPPER_CASE, String_toLocaleUpperCase, 0, function_prototype, env);
}

fn setup_date<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let mut class = env.new_native_function(Some(name::DATE_CLASS), 7, &Date_constructor, function_prototype);	
	
	function!(class, name::PARSE, Date_parse, 1, function_prototype, env);
	function!(class, name::UTC, Date_UTC, 7, function_prototype, env);
	function!(class, name::NOW, Date_now, 0, function_prototype, env);

	property!(global, name::DATE_CLASS, class, true, false, true, env);

	let mut prototype = class.get(env, name::PROTOTYPE).ok().unwrap().unwrap_object(env);
	
	env.add_handle(JsHandle::Date, prototype);
	
	function!(&mut prototype, name::TO_STRING, Date_toString, 0, function_prototype, env);
	function!(&mut prototype, name::TO_DATE_STRING, Date_toDateString, 0, function_prototype, env);
	function!(&mut prototype, name::TO_TIME_STRING, Date_toTimeString, 0, function_prototype, env);
	function!(&mut prototype, name::TO_LOCALE_STRING, Date_toLocaleString, 0, function_prototype, env);
	function!(&mut prototype, name::TO_LOCALE_DATE_STRING, Date_toLocaleDateString, 0, function_prototype, env);
	function!(&mut prototype, name::TO_LOCALE_TIME_STRING, Date_toLocaleTimeString, 0, function_prototype, env);
	function!(&mut prototype, name::VALUE_OF, Date_valueOf, 0, function_prototype, env);
	function!(&mut prototype, name::GET_TIME, Date_getTime, 0, function_prototype, env);
	function!(&mut prototype, name::GET_FULL_YEAR, Date_getFullYear, 0, function_prototype, env);
	function!(&mut prototype, name::GET_UTC_FULL_YEAR, Date_getUTCFullYear, 0, function_prototype, env);
	function!(&mut prototype, name::GET_MONTH, Date_getMonth, 0, function_prototype, env);
	function!(&mut prototype, name::GET_UTC_MONTH, Date_getUTCMonth, 0, function_prototype, env);
	function!(&mut prototype, name::GET_DATE, Date_getDate, 0, function_prototype, env);
	function!(&mut prototype, name::GET_UTC_DATE, Date_getUTCDate, 0, function_prototype, env);
	function!(&mut prototype, name::GET_DAY, Date_getDay, 0, function_prototype, env);
	function!(&mut prototype, name::GET_UTC_DAY, Date_getUTCDay, 0, function_prototype, env);
	function!(&mut prototype, name::GET_HOURS, Date_getHours, 0, function_prototype, env);
	function!(&mut prototype, name::GET_UTC_HOURS, Date_getUTCHours, 0, function_prototype, env);
	function!(&mut prototype, name::GET_MINUTES, Date_getMinutes, 0, function_prototype, env);
	function!(&mut prototype, name::GET_UTC_MINUTES, Date_getUTCMinutes, 0, function_prototype, env);
	function!(&mut prototype, name::GET_SECONDS, Date_getSeconds, 0, function_prototype, env);
	function!(&mut prototype, name::GET_UTC_SECONDS, Date_getUTCSeconds, 0, function_prototype, env);
	function!(&mut prototype, name::GET_MILLISECONDS, Date_getMilliseconds, 0, function_prototype, env);
	function!(&mut prototype, name::GET_UTC_MILLISECONDS, Date_getUTCMilliseconds, 0, function_prototype, env);
	function!(&mut prototype, name::GET_TIMEZONE_OFFSET, Date_getTimezoneOffset, 0, function_prototype, env);
	function!(&mut prototype, name::SET_TIME, Date_setTime, 1, function_prototype, env);
	function!(&mut prototype, name::SET_MILLISECONDS, Date_setMilliseconds, 1, function_prototype, env);
	function!(&mut prototype, name::SET_UTC_MILLISECONDS, Date_setUTCMilliseconds, 1, function_prototype, env);
	function!(&mut prototype, name::SET_SECONDS, Date_setSeconds, 2, function_prototype, env);
	function!(&mut prototype, name::SET_UTC_SECONDS, Date_setUTCSeconds, 2, function_prototype, env);
	function!(&mut prototype, name::SET_MINUTES, Date_setMinutes, 3, function_prototype, env);
	function!(&mut prototype, name::SET_UTC_MINUTES, Date_setUTCMinutes, 3, function_prototype, env);
	function!(&mut prototype, name::SET_HOURS, Date_setHours, 4, function_prototype, env);
	function!(&mut prototype, name::SET_UTC_HOURS, Date_setUTCHours, 4, function_prototype, env);
	function!(&mut prototype, name::SET_DATE, Date_setDate, 1, function_prototype, env);
	function!(&mut prototype, name::SET_UTC_DATE, Date_setUTCDate, 1, function_prototype, env);
	function!(&mut prototype, name::SET_MONTH, Date_setMonth, 2, function_prototype, env);
	function!(&mut prototype, name::SET_UTC_MONTH, Date_setUTCMonth, 2, function_prototype, env);
	function!(&mut prototype, name::SET_FULL_YEAR, Date_setFullYear, 3, function_prototype, env);
	function!(&mut prototype, name::SET_UTC_FULL_YEAR, Date_setUTCFullYear, 3, function_prototype, env);
	function!(&mut prototype, name::TO_UTC_STRING, Date_toUTCString, 0, function_prototype, env);
	function!(&mut prototype, name::TO_ISO_STRING, Date_toISOString, 0, function_prototype, env);
	function!(&mut prototype, name::TO_JSON, Date_toJSON, 1, function_prototype, env);
	function!(&mut prototype, name::GET_YEAR, Date_getYear, 0, function_prototype, env);
	function!(&mut prototype, name::SET_YEAR, Date_setYear, 1, function_prototype, env);
	function!(&mut prototype, name::TO_GMT_STRING, Date_toGMTString, 0, function_prototype, env);
}

fn setup_number<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let mut class = env.new_native_function(Some(name::NUMBER_CLASS), 1, &Number_constructor, function_prototype);
	
	value!(class, name::MAX_VALUE, env.new_number(f64::MAX), false, false, false, env);
	value!(class, name::MIN_VALUE, env.new_number(f64::MIN_POSITIVE), false, false, false, env);
	value!(class, name::NAN, env.new_number(f64::NAN), false, false, false, env);
	value!(class, name::NEGATIVE_INFINITY, env.new_number(f64::NEG_INFINITY), false, false, false, env);
	value!(class, name::POSITIVE_INFINITY, env.new_number(f64::INFINITY), false, false, false, env);
	
	property!(global, name::NUMBER_CLASS, class, true, false, true, env);

	let mut prototype = class.get(env, name::PROTOTYPE).ok().unwrap().unwrap_object(env);
	
	env.add_handle(JsHandle::Number, prototype);

	function!(&mut prototype, name::VALUE_OF, Number_valueOf, 0, function_prototype, env);
	function!(&mut prototype, name::TO_STRING, Number_toString, 0, function_prototype, env);
	function!(&mut prototype, name::TO_FIXED, Number_toFixed, 1, function_prototype, env);
}

fn setup_boolean<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let class = env.new_native_function(Some(name::BOOLEAN_CLASS), 1, &Boolean_constructor, function_prototype);
	
	property!(global, name::BOOLEAN_CLASS, class, true, false, true, env);
	
	let mut prototype = class.get(env, name::PROTOTYPE).ok().unwrap().unwrap_object(env);
	
	env.add_handle(JsHandle::Boolean, prototype);
	
	function!(&mut prototype, name::VALUE_OF, Boolean_valueOf, 0, function_prototype, env);
	function!(&mut prototype, name::TO_STRING, Boolean_toString, 0, function_prototype, env);
}

fn setup_math<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let mut class = JsObject::new_local(env, JsStoreType::Hash);
	
	value!(class, name::PI, env.new_number(f64::consts::PI), false, false, false, env);
	value!(class, name::E, env.new_number(f64::consts::E), false, false, false, env);
	value!(class, name::LN10, env.new_number(f64::consts::LN_10), false, false, false, env);
	value!(class, name::LN2, env.new_number(f64::consts::LN_2), false, false, false, env);
	value!(class, name::LOG2E, env.new_number(f64::consts::LOG2_E), false, false, false, env);
	value!(class, name::LOG10E, env.new_number(f64::consts::LOG10_E), false, false, false, env);
	value!(class, name::SQRT1_2, env.new_number(f64::consts::FRAC_1_SQRT_2), false, false, false, env);
	value!(class, name::SQRT2, env.new_number(f64::consts::SQRT_2), false, false, false, env);
	
	class.set_class(env, Some(name::MATH_CLASS));
	
	property!(global, name::MATH_CLASS, class.as_value(env), true, false, true, env);
	
	function!(class, name::POW, Math_pow, 2, function_prototype, env);
	function!(class, name::FLOOR, Math_floor, 1, function_prototype, env);
	function!(class, name::MIN, Math_min, 2, function_prototype, env);
	function!(class, name::MAX, Math_max, 2, function_prototype, env);
	function!(class, name::ABS, Math_abs, 1, function_prototype, env);
}

fn setup_regexp<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let class = env.new_native_function(Some(name::REGEXP_CLASS), 1, &RegExp_constructor, function_prototype);
	
	property!(global, name::REGEXP_CLASS, class, true, false, true, env);
	
	let prototype = class.get(env, name::PROTOTYPE).ok().unwrap().unwrap_object(env);
	
	env.add_handle(JsHandle::RegExp, prototype);
}

fn setup_json<'a>(env: &mut JsEnv, mut global: Local<JsValue>, _function_prototype: Local<JsObject>) {
	let mut class = JsObject::new_local(env, JsStoreType::Hash);
	
	class.set_class(env, Some(name::JSON_CLASS));
	
	property!(global, name::JSON_CLASS, class.as_value(env), true, false, true, env);
}

fn setup_console<'a>(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>) {
	let mut class = JsObject::new_local(env, JsStoreType::Hash);
	
	class.set_class(env, Some(name::CONSOLE_CLASS));
	
	property!(global, name::CONSOLE, class.as_value(env), true, false, true, env);
	
	function!(class, name::LOG, console_log, 1, function_prototype, env);
}

fn setup_error<'a>(env: &mut JsEnv, global: Local<JsValue>, function_prototype: Local<JsObject>) {
	fn register_error(env: &mut JsEnv, mut global: Local<JsValue>, function_prototype: Local<JsObject>, name: Name, handle: JsHandle) {
		let class = env.new_native_function(Some(name), 1, &Error_constructor, function_prototype);
		
		let class_obj = class.unwrap_object(env);
		env.add_handle(handle, class_obj);
		
		property!(global, name, class, true, false, true, env);
		
		let mut prototype = class.get(env, name::PROTOTYPE).ok().unwrap().unwrap_object(env);
		
		let value = JsString::from_str(env, "").as_value(env);
		value!(&mut prototype, name::MESSAGE, value, true, false, true, env);
		let value = JsString::from_str(env, &*env.ir.interner().get(name)).as_value(env);
		value!(&mut prototype, name::NAME, value, true, false, true, env);
		function!(&mut prototype, name::TO_STRING, Error_toString, 0, function_prototype, env);
	}
	
	register_error(env, global, function_prototype, name::ERROR_CLASS, JsHandle::Error);
	register_error(env, global, function_prototype, name::EVAL_ERROR_CLASS, JsHandle::EvalError);
	register_error(env, global, function_prototype, name::RANGE_ERROR_CLASS, JsHandle::RangeError);
	register_error(env, global, function_prototype, name::REFERENCE_ERROR_CLASS, JsHandle::ReferenceError);
	register_error(env, global, function_prototype, name::SYNTAX_ERROR_CLASS, JsHandle::SyntaxError);
	register_error(env, global, function_prototype, name::TYPE_ERROR_CLASS, JsHandle::TypeError);
	register_error(env, global, function_prototype, name::URI_ERROR_CLASS, JsHandle::URIError);
	register_error(env, global, function_prototype, name::NATIVE_ERROR_CLASS, JsHandle::NativeError);
}

fn new_naked_function<'a>(env: &mut JsEnv, name: Option<Name>, args: u32, function: &JsFn, prototype: Local<JsObject>, can_construct: bool) -> Local<JsValue> {
	JsObject::new_function(env, JsFunction::Native(name, args, JsFnRef::new(function), can_construct), prototype, false).as_value(env)
}
