#![allow(non_snake_case)]
#![allow(unused_variables)]

use super::{JsEnv, JsObject, JsString, JsFunction, JsFn, JsFnMode, JsArgs, JsValue, JsType};
use super::hash::Property;
use ::{JsResult, JsError};
use syntax::ast::{Name, FunctionRef};
use syntax::token::keywords;
use gc::*;

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
	
	env.global().props.add(keywords::FUNCTION_CLASS, &Property::new_value(function_class, false, true, false), env);
	
	// Build Object
	
	let object_class = setup_object(env, &mut object_prototype, &function_prototype);
	
	env.global().props.add(keywords::OBJECT_CLASS, &Property::new_value(object_class, false, true, false), env);
}

fn setup_function(env: &JsEnv, prototype: &mut Local<JsObject>, class: &Local<JsObject>) {
	prototype.props.add(keywords::CONSTRUCTOR, &Property::new_value(class.as_value(env), false, true, false), env);
	let call_function = new_function(env, Some(keywords::CALL), 1, Box::new(Function_call), prototype);
	prototype.props.add(keywords::CALL, &Property::new_value(call_function, false, true, false), env);
	let apply_function = new_function(env, Some(keywords::APPLY), 2, Box::new(Function_apply), prototype);
	prototype.props.add(keywords::APPLY, &Property::new_value(apply_function, false, true, false), env);
	let to_string_function = new_function(env, Some(keywords::TO_STRING), 0, Box::new(Function_toString), prototype);
	prototype.props.add(keywords::TO_STRING, &Property::new_value(to_string_function, false, true, false), env);
	let to_locale_string_function = new_function(env, Some(keywords::TO_LOCALE_STRING), 0, Box::new(Function_toLocaleString), prototype);
	prototype.props.add(keywords::TO_LOCALE_STRING, &Property::new_value(to_locale_string_function, false, true, false), env);
	let length_get_function = new_function(env, Some(keywords::LENGTH), 2, Box::new(Function_length_get), prototype);
	let length_set_function = new_function(env, Some(keywords::LENGTH), 2, Box::new(Function_length_set), prototype);
	prototype.props.add(keywords::LENGTH, &Property::new_accessor(length_get_function, length_set_function, false, true, false), env);
}

fn setup_object<'a>(env: &JsEnv, prototype: &mut Local<JsObject>, function_prototype: &Local<JsObject>) -> Local<JsValue> {
	let to_string_function = new_function(env, Some(keywords::TO_STRING), 0, Box::new(Object_toString), function_prototype);
	prototype.props.add(keywords::TO_STRING, &Property::new_value(to_string_function, false, true, false), env);
	let to_locale_string_function = new_function(env, Some(keywords::TO_LOCALE_STRING), 0, Box::new(Object_toLocaleString), function_prototype);
	prototype.props.add(keywords::TO_LOCALE_STRING, &Property::new_value(to_locale_string_function, false, true, false), env);
	let value_of_function = new_function(env, Some(keywords::VALUE_OF), 0, Box::new(Object_valueOf), function_prototype);
	prototype.props.add(keywords::VALUE_OF, &Property::new_value(value_of_function, false, true, false), env);
	let has_own_property_function = new_function(env, Some(keywords::HAS_OWN_PROPERTY), 1, Box::new(Object_hasOwnProperty), function_prototype);
	prototype.props.add(keywords::HAS_OWN_PROPERTY, &Property::new_value(has_own_property_function, false, true, false), env);
	let is_prototype_of_function = new_function(env, Some(keywords::IS_PROTOTYPE_OF), 1, Box::new(Object_isPrototypeOf), function_prototype);
	prototype.props.add(keywords::IS_PROTOTYPE_OF, &Property::new_value(is_prototype_of_function, false, true, false), env);
	let property_is_enumerable_function = new_function(env, Some(keywords::PROPERTY_IS_ENUMERABLE), 1, Box::new(Object_propertyIsEnumerable), function_prototype);
	prototype.props.add(keywords::PROPERTY_IS_ENUMERABLE, &Property::new_value(property_is_enumerable_function, false, true, false), env);
	let get_prototype_of_function = new_function(env, Some(keywords::GET_PROTOTYPE_OF), 1, Box::new(Object_getPrototypeOf), function_prototype);
	prototype.props.add(keywords::GET_PROTOTYPE_OF, &Property::new_value(get_prototype_of_function, false, true, false), env);
	let define_property_function = new_function(env, Some(keywords::DEFINE_PROPERTY), 1, Box::new(Object_defineProperty), function_prototype);
	prototype.props.add(keywords::DEFINE_PROPERTY, &Property::new_value(define_property_function, false, true, false), env);
	
	let object = new_function(env, Some(keywords::OBJECT_CLASS), 0, Box::new(Object_constructor), prototype);
	
	let create_function = new_function(env, Some(keywords::CREATE), 1, Box::new(Object_create), function_prototype);
	object.get_object().props.add(keywords::CREATE, &Property::new_value(create_function, false, true, false), env);
	
	object
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
	
	result.props.add(keywords::PROTOTYPE, &Property::new_value(proto.as_value(env), false, true, false), env);
	proto.props.add(keywords::CONSTRUCTOR, &Property::new_value(result_value, false, true, false), env);
	
	result_value
}

fn Function_baseConstructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	// Nothing to do. The default result already is undefined.
	Ok(JsValue::new_undefined().as_local(env))
}

fn Function_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	if args.mode == JsFnMode::Call {
		return env.construct(args.function, args.args);
	}
	
	let body;
	
	let mut source = String::new();
	
	source.push_str("function (");
	
	if args.args.len() == 0 {
		body = JsString::from_str(env, "");
	} else {
		body = env.to_string(args.args[args.args.len() - 1]);
		
		for i in 0..args.args.len() - 1 {
			if i > 0 {
				source.push_str(", ");
			}
			source.push_str(&env.to_string(args.args[i]).to_string());
		}
	}
	
	source.push_str(") { ");
	source.push_str(&body.to_string());
	source.push_str(" }");
	
	let function_ref = try!(env.ir.parse_string(&source));
	// The function returned is the program, but we need the function. The program
	// function is created last so we need the last but one.
	
	let function_ref = FunctionRef(function_ref.0 - 1);
	
	Ok(env.new_function(function_ref))
}

fn Function_call(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Function_apply(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Function_toString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Function_toLocaleString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Function_length_get(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Function_length_set(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Object_constructor(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Object_create(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	assert!(args.args.len() <= 1);
	
	let mut result = JsObject::new_local(env);

	result.prototype = if args.args.len() < 1 {
		return Err(JsError::Type);
	} else {
		let prototype = args.args[0];
		match prototype.ty() {
			JsType::Null => Ptr::null(),
			JsType::Object => prototype.get_object(),
			_ => return Err(JsError::Type)
		}
	};
	
	Ok(result.as_value(env))
}

fn Object_toString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Object_toLocaleString(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Object_valueOf(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Object_hasOwnProperty(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Object_isPrototypeOf(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Object_propertyIsEnumerable(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Object_getPrototypeOf(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}

fn Object_defineProperty(env: &mut JsEnv, args: JsArgs) -> JsResult<Local<JsValue>> {
	unimplemented!();
}
