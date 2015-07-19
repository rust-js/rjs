extern crate libc;

use rt::{JsEnv, JsString, JsType, JsObject, JsItem, JsDescriptor, JsScope, JsPreferredType};
use rt::{JsNull, JsUndefined, JsNumber, JsBoolean, JsIterator, JsHandle, JsRegExp, GC_VALUE};
use rt::{validate_walker_field, validate_walker_field_at};
use rt::fmt::{format_number, NumberFormatStyle};
use ::{JsResult, JsError};
use syntax::Name;
use syntax::lexer::{Lexer, LexerMode};
use syntax::reader::StringReader;
use syntax::token::{Token, LitNumber};
use syntax::token::name;
use gc::{Local, Ptr, AsPtr, ptr_t, GcWalker, GcAllocator};
use std::fmt;
use std::mem::{transmute, zeroed, size_of};
use std::f64;
use std::cmp::PartialEq;
use std::num::Wrapping;

#[allow(dead_code)]
const FLOAT_EXPONENT_BIAS    : u32 = 127;
#[allow(dead_code)]
const FLOAT_EXPONENT_SHIFT   : u32 = 23;
#[allow(dead_code)]
const FLOAT_SIGN_BIT         : u32 = 0x80000000u32;
#[allow(dead_code)]
const FLOAT_EXPONENT_BITS    : u32 = 0x7F800000u32;
#[allow(dead_code)]
const FLOAT_SIGNIFICANT_BITS : u32 = 0x007FFFFFu32;

const DOUBLE_EXPONENT_BIAS    : u32 = 1023;
const DOUBLE_EXPONENT_SHIFT   : u32 = 52;
const DOUBLE_SIGN_BIT         : u64 = 0x8000000000000000u64;
const DOUBLE_EXPONENT_BITS    : u64 = 0x7ff0000000000000u64;
#[allow(dead_code)]
const DOUBLE_SIGNIFICANT_BITS : u64 = 0x000fffffffffffffu64;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct JsRawValue {
    ty: JsType,
    value: Data
}

impl fmt::Debug for JsRawValue {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "JsRawValue {{ ty: {:?}, value: ", self.ty));
        match self.ty {
            JsType::Undefined => try!(write!(fmt, "undefined")),
            JsType::Null => try!(write!(fmt, "null")),
            JsType::Number => try!(write!(fmt, "{}", self.unwrap_number())),
            JsType::Boolean => try!(write!(fmt, "{}", self.unwrap_bool())),
            JsType::String => try!(write!(fmt, "string")),
            JsType::Object => try!(write!(fmt, "object")),
            _ => panic!("unexpected type")
        }
        try!(write!(fmt, " }}"));
        
        Ok(())
    }
}

impl JsRawValue {
    pub fn new_undefined() -> JsRawValue {
        JsRawValue {
            ty: JsType::Undefined,
            value: Data::new()
        }
    }
    
    pub fn new_null() -> JsRawValue {
        JsRawValue {
            ty: JsType::Null,
            value: Data::new()
        }
    }
    
    pub fn new_number(value: f64) -> JsRawValue {
        JsRawValue {
            ty: JsType::Number,
            value: Data::new_number(value)
        }
    }
    
    pub fn new_bool(value: bool) -> JsRawValue {
        JsRawValue {
            ty: JsType::Boolean,
            value: Data::new_bool(value)
        }
    }
    
    pub fn ty(&self) -> JsType {
        self.ty
    }
    
    fn unwrap_number(&self) -> f64 {
        assert_eq!(self.ty, JsType::Number);
        
        self.value.get_number()
    }
    
    fn unwrap_bool(&self) -> bool {
        assert_eq!(self.ty, JsType::Boolean);
        
        self.value.get_bool()
    }
    
    fn get_ptr<T>(&self) -> Ptr<T> {
        self.value.get_ptr()
    }
    
    pub fn as_value<T: GcAllocator>(&self, allocator: &T) -> JsValue {
        let value = if self.ty.is_ptr() {
            LocalData::new_local(allocator.alloc_local_from_ptr::<()>(self.get_ptr()))
        } else {
            LocalData {
                data: self.value.data
            }
        };
        
        JsValue {
            ty: self.ty,
            value: value
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
struct Data {
    data: u64
}

impl Data {
    fn new() -> Data {
        Data {
            data: 0
        }
    }
    
    fn new_number(value: f64) -> Data {
        Data {
            data: unsafe { transmute(value) }
        }
    }
    
    fn new_bool(value: bool) -> Data {
        Data {
            data: unsafe { transmute::<_, u8>(value) as u64 }
        }
    }
    
    fn new_ptr<T, U: AsPtr<T>>(value: U) -> Data {
        Data {
            data: value.as_ptr().ptr() as u64
        }
    }
    
    fn get_number(&self) -> f64 {
        unsafe { transmute(self.data) }
    }
    
    fn get_bool(&self) -> bool {
        unsafe { transmute(self.data as u8) }
    }
    
    fn get_ptr<T>(&self) -> Ptr<T> {
        Ptr::from_ptr(self.data as ptr_t)
    }
}

#[derive(Copy, Clone)]
pub struct JsValue {
    ty: JsType,
    value: LocalData
}

impl PartialEq for JsValue {
    fn eq(&self, other: &JsValue) -> bool {
        if self.ty != other.ty {
            false
        } else {
            if self.ty.is_ptr() {
                self.value.get_local::<()>().as_ptr() == other.value.get_local::<()>().as_ptr()
            } else {
                self.value.data == other.value.data
            }
        }
    }
}

impl fmt::Debug for JsValue {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "JsRawValue {{ ty: {:?}, value: ", self.ty));
        match self.ty {
            JsType::Undefined => try!(write!(fmt, "undefined")),
            JsType::Null => try!(write!(fmt, "null")),
            JsType::Number => try!(write!(fmt, "{}", self.unwrap_number())),
            JsType::Boolean => try!(write!(fmt, "{}", self.unwrap_bool())),
            JsType::String => try!(write!(fmt, "string")),
            JsType::Object => try!(write!(fmt, "object")),
            _ => panic!("unexpected type")
        }
        try!(write!(fmt, " }}"));
        
        Ok(())
    }
}

impl JsValue {
    pub fn new_undefined() -> JsValue {
        JsValue {
            ty: JsType::Undefined,
            value: LocalData::new()
        }
    }
    
    pub fn new_null() -> JsValue {
        JsValue {
            ty: JsType::Null,
            value: LocalData::new()
        }
    }
    
    pub fn new_number(value: f64) -> JsValue {
        JsValue {
            ty: JsType::Number,
            value: LocalData::new_number(value)
        }
    }
    
    pub fn new_bool(value: bool) -> JsValue {
        JsValue {
            ty: JsType::Boolean,
            value: LocalData::new_bool(value)
        }
    }
    
    pub fn new_string(value: Local<JsString>) -> JsValue {
        JsValue {
            ty: JsType::String,
            value: LocalData::new_local(value)
        }
    }
    
    pub fn new_object(value: Local<JsObject>) -> JsValue {
        JsValue {
            ty: JsType::Object,
            value: LocalData::new_local(value)
        }
    }
    
    pub fn new_iterator(value: Local<JsIterator>) -> JsValue {
        JsValue {
            ty: JsType::Iterator,
            value: LocalData::new_local(value)
        }
    }
    
    pub fn new_scope(value: Local<JsScope>) -> JsValue {
        JsValue {
            ty: JsType::Scope,
            value: LocalData::new_local(value)
        }
    }
    
    pub fn new_regexp(value: Local<JsRegExp>) -> JsValue {
        JsValue {
            ty: JsType::RegExp,
            value: LocalData::new_local(value)
        }
    }
    
    pub fn ty(&self) -> JsType {
        self.ty
    }
    
    pub fn is_null(&self) -> bool {
        self.ty == JsType::Null
    }
    
    pub fn is_undefined(&self) -> bool {
        self.ty == JsType::Undefined
    }
    
    pub fn is_null_or_undefined(&self) -> bool {
        let ty = self.ty;
        ty == JsType::Null || ty == JsType::Undefined
    }
    
    pub fn unwrap_number(&self) -> f64 {
        assert_eq!(self.ty, JsType::Number);
        
        self.value.get_number()
    }
    
    pub fn unwrap_bool(&self) -> bool {
        assert_eq!(self.ty, JsType::Boolean);
        
        self.value.get_bool()
    }
    
    pub fn unwrap_string(&self) -> Local<JsString> {
        assert_eq!(self.ty, JsType::String);
        
        self.value.get_local::<JsString>()
    }
    
    pub fn unwrap_object(&self) -> Local<JsObject> {
        assert_eq!(self.ty, JsType::Object);
        
        self.value.get_local::<JsObject>()
    }
    
    pub fn unwrap_iterator(&self) -> Local<JsIterator> {
        assert_eq!(self.ty, JsType::Iterator);
        
        self.value.get_local::<JsIterator>()
    }
    
    pub fn unwrap_scope(&self) -> Local<JsScope> {
        assert_eq!(self.ty, JsType::Scope);
        
        self.value.get_local::<JsScope>()
    }
    
    pub fn unwrap_regexp(&self) -> Local<JsRegExp> {
        assert_eq!(self.ty, JsType::RegExp);
        
        self.value.get_local::<JsRegExp>()
    }
    
    pub fn as_raw(&self) -> JsRawValue {
        let value = if self.ty.is_ptr() {
            Data::new_ptr(self.value.get_local::<()>())
        } else {
            Data {
                data: self.value.data
            }
        };
        
        JsRawValue {
            ty: self.ty,
            value: value
        }
    }
    
    // 9.1 ToPrimitive
    pub fn to_primitive(&self, env: &mut JsEnv, hint: JsPreferredType) -> JsResult<JsValue> {
        match self.ty() {
            JsType::Object => self.default_value(env, hint),
            _ => Ok(*self)
        }
    }
    
    // 9.2 ToBoolean
    pub fn to_boolean(&self) -> bool {
        match self.ty() {
            JsType::Undefined | JsType::Null => false,
            JsType::Boolean => self.unwrap_bool(),
            JsType::Number => {
                let value = self.unwrap_number();
                !(value == 0.0 || value.is_nan())
            }
            JsType::String => self.value.get_local::<JsString>().chars().len() > 0,
            JsType::Object => true,
            _ => panic!("unexpected type")
        }
    }
    
    // 9.3 ToNumber
    pub fn to_number(&self, env: &mut JsEnv) -> JsResult<f64> {
        let result = match self.ty() {
            JsType::Undefined => f64::NAN,
            JsType::Null => 0.0,
            JsType::Boolean => if self.unwrap_bool() { 1.0 } else { 0.0 },
            JsType::Number => self.unwrap_number(),
            JsType::String => try!(self.get_number_from_string(env)),
            JsType::Object => {
                let value = try!(self.to_primitive(env, JsPreferredType::Number));
                try!(value.to_number(env))
            }
            _ => panic!("unexpected type")
        };
        
        Ok(result)
    }
    
    fn get_number_from_string(&self, env: &JsEnv) -> JsResult<f64> {
        // TODO #67: Use DecimalMatcher.
        
        let value = self.unwrap_string().to_string();
        let mut reader = StringReader::new("", &value);
        
        if let Ok(mut lexer) = Lexer::new(&mut reader, env.ir.interner(), LexerMode::Runtime) {
            // Skip over the + or - sign if we have one.
            
            let sign = match try!(lexer.peek(0)).map(|token| token.token) {
                Some(Token::Plus) => {
                    try!(lexer.bump());
                    false
                }
                Some(Token::Minus) => {
                    try!(lexer.bump());
                    true
                }
                None => {
                    // The empty string results in 0.
                    
                    return Ok(0.0);
                }
                _ => false
            };
            
            // Verify that the next token is a number literal.
            
            let mut result = None;
            
            if let Some(token) = try!(lexer.peek(0)) {
                match token.token {
                    Token::Literal(lit) => {
                        if let Some(value) = LitNumber::from_lit(env.ir.interner(), &lit, sign) {
                            match value {
                                LitNumber::Float(value) => result = Some(value),
                                LitNumber::Long(value) => result = Some(value as f64)
                            }
                            
                            try!(lexer.next());
                        }
                    }
                    Token::Identifier(name::INFINITY) => {
                        result = Some(if sign { -f64::INFINITY } else { f64::INFINITY });
                        
                        try!(lexer.next());
                    }
                    _ => {}
                }
            }
            
            // Verify that we're at the end of the stream.
            
            if let Some(result) = result {
                if try!(lexer.peek(0)).is_none() {
                    return Ok(result);
                }
            }
        }
        
        Ok(f64::NAN)
    }
    
    // 9.4 ToInteger
    pub fn to_integer(&self, env: &mut JsEnv) -> JsResult<f64> {
        let number = try!(self.to_number(env));
        let result = if number.is_nan() {
            0.0
        } else if number == 0.0 || number.is_infinite() {
            number
        } else {
            number.trunc()
        };
        
        Ok(result)
    }
    
    // 9.5 ToInt32: (Signed 32 Bit Integer)
    // TODO #74: This does not adhere to the full specs.
    pub fn to_int32(&self, env: &mut JsEnv) -> JsResult<i32> {
        let number = try!(self.to_number(env));
        let result = if number.is_nan() || number == 0.0 || number.is_infinite() {
            0
        } else {
            number as i64 as i32
        };
        
        Ok(result)
    }
    
    // 9.6 ToUint32: (Unsigned 32 Bit Integer)
    pub fn to_uint32(&self, env: &mut JsEnv) -> JsResult<u32> {
        let number = try!(self.to_number(env));
        
        Ok(Self::f64_to_u32(number))
    }
    
    // Taken from http://mxr.mozilla.org/mozilla-central/source/js/public/Conversions.h#255.
    pub fn f64_to_u32(number: f64) -> u32 {
        let bits = unsafe { transmute::<_, u64>(number) };
        
        // Extract the exponent component.  (Be careful here!  It's not technically
        // the exponent in NaN, infinities, and subnormals.)
        
        let exp =
            ((bits & DOUBLE_EXPONENT_BITS) >> DOUBLE_EXPONENT_SHIFT) as i16 -
            DOUBLE_EXPONENT_BIAS as i16;
        
        // If the exponent's less than zero, abs(d) < 1, so the result is 0.  (This
        // also handles subnormals.)
        
        if exp < 0 {
            return 0;
        }
        
        let exponent = exp as u32;
        
        // If the exponent is greater than or equal to the bits of precision of a
        // double plus ResultType's width, the number is either infinite, NaN, or
        // too large to have lower-order bits in the congruent value.  (Example:
        // 2**84 is exactly representable as a double.  The next exact double is
        // 2**84 + 2**32.  Thus if ResultType is int32_t, an exponent >= 84 implies
        // floor(abs(d)) == 0 mod 2**32.)  Return 0 in all these cases.
        
        let result_width = (8 * size_of::<u32>()) as u32;
        if exponent >= DOUBLE_EXPONENT_SHIFT + result_width {
            return 0;
        }
        
        // The significand contains the bits that will determine the final result.
        // Shift those bits left or right, according to the exponent, to their
        // locations in the unsigned binary representation of floor(abs(d)).
        
        let mut result = if exponent > DOUBLE_EXPONENT_SHIFT {
            (bits << (exponent - DOUBLE_EXPONENT_SHIFT)) as u32
        } else {
            (bits >> (DOUBLE_EXPONENT_SHIFT - exponent)) as u32
        };
        
        // Two further complications remain.  First, |result| may contain bogus
        // sign/exponent bits.  Second, IEEE-754 numbers' significands (excluding
        // subnormals, but we already handled those) have an implicit leading 1
        // which may affect the final result.
        //
        // It may appear that there's complexity here depending on how ResultWidth
        // and DoubleExponentShift relate, but it turns out there's not.
        //
        // Assume ResultWidth < DoubleExponentShift:
        //   Only right-shifts leave bogus bits in |result|.  For this to happen,
        //   we must right-shift by > |DoubleExponentShift - ResultWidth|, implying
        //   |exponent < ResultWidth|.
        //   The implicit leading bit only matters if it appears in the final
        //   result -- if |2**exponent mod 2**ResultWidth != 0|.  This implies
        //   |exponent < ResultWidth|.
        // Otherwise assume ResultWidth >= DoubleExponentShift:
        //   Any left-shift less than |ResultWidth - DoubleExponentShift| leaves
        //   bogus bits in |result|.  This implies |exponent < ResultWidth|.  Any
        //   right-shift less than |ResultWidth| does too, which implies
        //   |DoubleExponentShift - ResultWidth < exponent|.  By assumption, then,
        //   |exponent| is negative, but we excluded that above.  So bogus bits
        //   need only |exponent < ResultWidth|.
        //   The implicit leading bit matters identically to the other case, so
        //   again, |exponent < ResultWidth|.
        
        if exponent < result_width {
            let implicit_one = 1u32 << exponent;
            result &= implicit_one - 1; // remove bogus bits
            result += implicit_one; // add the implicit bit
        }
        
        // Compute the congruent value in the signed range.
        if (bits & DOUBLE_SIGN_BIT) != 0 { (Wrapping(!result) + Wrapping(1)).0 } else { result }
    }
    
    // 9.6 ToUint32: (Unsigned 32 Bit Integer)
    // TODO #74: This does not adhere to the full specs.
    pub fn to_uint32_exact(&self, env: &mut JsEnv) -> JsResult<Option<u32>> {
        let number = try!(self.to_number(env));
        let result = if number.is_nan() || number == 0.0 || number.is_infinite() {
            0
        } else {
            number as u32
        };
        
        if result as f64 == number {
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }
    
    // 9.7 ToUint16: (Unsigned 16 Bit Integer)
    pub fn to_uint16(&self, env: &mut JsEnv) -> JsResult<u16> {
        let number = try!(self.to_number(env));
        
        let result = if !number.is_finite() {
            0
        } else {
            let result = number as u16;
            
            if result as f64 == number {
                result
            } else {
                let mut number = if number < 0.0 {
                    -((-number).floor())
                } else {
                    number.floor()
                };
                
                number %= 0x10000 as f64;
                if number < 0.0 {
                    number += 0x10000 as f64;
                }
                
                number as u16
            }
        };
        
        Ok(result)
    }
    
    // 9.8 ToString
    pub fn to_string(&self, env: &mut JsEnv) -> JsResult<Local<JsString>> {
        let result = match self.ty {
            JsType::Undefined => JsString::from_str(env, "undefined"),
            JsType::Null => JsString::from_str(env, "null"),
            JsType::Boolean => JsString::from_str(env, if self.unwrap_bool() { "true" } else { "false" }),
            JsType::Number => {
                let number = self.unwrap_number();
                let result = format_number(number, 10, NumberFormatStyle::Regular, 0);
                JsString::from_str(env, &result)
            }
            JsType::String => self.unwrap_string(),
            JsType::Object => {
                let result = try!(self.to_primitive(env, JsPreferredType::String));
                try!(result.to_string(env))
            }
            _ => panic!("unexpected type")
        };
        
        Ok(result)
    }
    
    // 9.9 ToObject
    pub fn to_object(&self, env: &mut JsEnv) -> JsResult<JsValue> {
        match self.ty {
            JsType::Null | JsType::Undefined => Err(JsError::new_type(env, ::errors::TYPE_INVALID)),
            JsType::String => {
                let constructor = try!(env.handle(JsHandle::Global).get(env, name::STRING_CLASS));
                let object = try!(constructor.construct(env, vec![*self]));
                Ok(object)
            }
            JsType::Boolean | JsType::Number => {
                let class = match self.ty {
                    JsType::Boolean => name::BOOLEAN_CLASS,
                    JsType::Number => name::NUMBER_CLASS,
                    JsType::String => name::STRING_CLASS,
                    _ => unreachable!()
                };
                
                let constructor = try!(env.handle(JsHandle::Global).get(env, class));
                let object = try!(constructor.construct(env, Vec::new()));
                object.unwrap_object().set_value(*self);
                Ok(object)
            }
            JsType::Object => Ok(*self),
            _ => panic!("unexpected type")
        }
    }
    
    // 9.10 CheckObjectCoercible
    pub fn check_object_coercible(&self, env: &mut JsEnv) -> JsResult<()> {
        if self.is_null_or_undefined() {
            Err(JsError::new_type(env, ::errors::TYPE_NOT_COERCIBLE))
        } else {
            Ok(())
        }
    }
}

macro_rules! delegate {
    ( $target:expr, $method:ident ( $( $arg:expr ),* ) ) => {
        match $target.ty() {
            JsType::Undefined => JsUndefined.$method( $( $arg ),* ),
            JsType::Null => JsNull.$method( $( $arg ),* ),
            JsType::Number => JsNumber::new($target.unwrap_number()).$method( $( $arg ),* ),
            JsType::Boolean => JsBoolean::new($target.unwrap_bool()).$method( $( $arg ),* ),
            JsType::Object => $target.unwrap_object().$method( $( $arg ),* ),
            JsType::String => $target.unwrap_string().$method( $( $arg ),* ),
            _ => panic!("unexpected type")
        }
    };
    ( $target:expr, $env:expr, $method:ident ( $( $arg:expr ),* ) ) => {
        match $target.ty() {
            JsType::Undefined => JsUndefined.$method( $( $arg ),* ),
            JsType::Null => JsNull.$method( $( $arg ),* ),
            JsType::Number => JsNumber::new($target.unwrap_number()).$method( $( $arg ),* ),
            JsType::Boolean => JsBoolean::new($target.unwrap_bool()).$method( $( $arg ),* ),
            JsType::Object => $target.unwrap_object().$method( $( $arg ),* ),
            JsType::String => $target.unwrap_string().$method( $( $arg ),* ),
            _ => panic!("unexpected type")
        }
    }
}

impl JsItem for JsValue {
    fn as_value(&self) -> JsValue {
        *self
    }
    
    fn get_own_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
        delegate!(self, env, get_own_property(env, property))
    }
    
    fn get_property(&self, env: &JsEnv, property: Name) -> Option<JsDescriptor> {
        delegate!(self, env, get_property(env, property))
    }
    
    fn get(&self, env: &mut JsEnv, property: Name) -> JsResult<JsValue> {
        delegate!(self, env, get(env, property))
    }
    
    fn can_put(&self, env: &JsEnv, property: Name) -> bool {
        delegate!(self, env, can_put(env, property))
    }
    
    fn put(&mut self, env: &mut JsEnv, property: Name, value: JsValue, throw: bool) -> JsResult<()> {
        delegate!(self, env, put(env, property, value, throw))
    }
    
    fn has_property(&self, env: &JsEnv, property: Name) -> bool {
        delegate!(self, env, has_property(env, property))
    }
    
    fn delete(&mut self, env: &mut JsEnv, property: Name, throw: bool) -> JsResult<bool> {
        delegate!(self, env, delete(env, property, throw))
    }
    
    fn default_value(&self, env: &mut JsEnv, hint: JsPreferredType) -> JsResult<JsValue> {
        delegate!(self, env, default_value(env, hint))
    }
    
    fn define_own_property(&mut self, env: &mut JsEnv, property: Name, descriptor: JsDescriptor, throw: bool) -> JsResult<bool> {
        delegate!(self, env, define_own_property(env, property, descriptor, throw))
    }
    
    fn is_callable(&self) -> bool  {
        delegate!(self, is_callable())
    }
    
    fn call(&self, env: &mut JsEnv, this: JsValue, args: Vec<JsValue>, strict: bool) -> JsResult<JsValue>  {
        delegate!(self, env, call(env, this, args, strict))
    }
    
    fn can_construct(&self) -> bool  {
        delegate!(self, can_construct())
    }
    
    fn construct(&self, env: &mut JsEnv, args: Vec<JsValue>) -> JsResult<JsValue>  {
        delegate!(self, env, construct(env, args))
    }
    
    fn has_prototype(&self) -> bool  {
        delegate!(self, has_prototype())
    }
    
    fn prototype(&self, env: &JsEnv) -> Option<JsValue>  {
        delegate!(self, env, prototype(env))
    }
    
    fn set_prototype(&mut self, prototype: Option<JsValue>)  {
        delegate!(self, set_prototype(prototype))
    }
    
    fn has_class(&self) -> bool  {
        delegate!(self, has_class())
    }
    
    fn class(&self) -> Option<Name>  {
        delegate!(self, class())
    }
    
    fn set_class(&mut self, class: Option<Name>)  {
        delegate!(self, set_class(class))
    }
    
    fn is_extensible(&self) -> bool  {
        delegate!(self, is_extensible())
    }
    
    fn has_instance(&self, env: &mut JsEnv, object: JsValue) -> JsResult<bool>  {
        delegate!(self, env, has_instance(env, object))
    }
    
    fn scope(&self, env: &JsEnv) -> Option<Local<JsScope>>  {
        delegate!(self, env, scope(env))
    }
    
    fn set_scope(&mut self, scope: Option<Local<JsScope>>) {
        delegate!(self, set_scope(scope))
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
struct LocalData {
    data: u64
}

impl LocalData {
    fn new() -> LocalData {
        LocalData {
            data: 0
        }
    }
    
    fn new_number(value: f64) -> LocalData {
        LocalData {
            data: unsafe { transmute(value) }
        }
    }
    
    fn new_bool(value: bool) -> LocalData {
        LocalData {
            data: unsafe { transmute::<_, u8>(value) as u64 }
        }
    }
    
    fn new_local<T>(value: Local<T>) -> LocalData {
        LocalData {
            data: unsafe { transmute(value) }
        }
    }
    
    fn get_number(&self) -> f64 {
        unsafe { transmute(self.data) }
    }
    
    fn get_bool(&self) -> bool {
        unsafe { transmute(self.data as u8) }
    }
    
    fn get_local<T>(&self) -> Local<T> {
        unsafe { transmute(self.data) }
    }
}

pub unsafe fn validate_walker_for_value(walker: &GcWalker) {
    let mut object : Box<JsRawValue> = Box::new(zeroed());
    let ptr = transmute::<_, ptr_t>(&*object);
    
    validate_walker_for_embedded_value(walker, ptr, GC_VALUE, 0, &mut *object);
    
    assert_eq!(size_of::<JsRawValue>(), 16);
}

pub unsafe fn validate_walker_for_embedded_value(walker: &GcWalker, ptr: ptr_t, ty: u32, offset: u32, object: *mut JsRawValue) {
    (*object).ty = JsType::Boolean;
    validate_walker_field(walker, ty, ptr, false);
    (*object).ty = JsType::Undefined;
    
    (*object).value = Data { data: 1 };
    validate_walker_field_at(walker, ty, ptr, false, offset + 1);
    (*object).value = Data { data: 0 };
    
    (*object).ty = JsType::String;
    (*object).value = Data { data: 1 };
    validate_walker_field_at(walker, ty, ptr, true, offset + 1);
    
    *object = transmute(zeroed::<JsRawValue>());
}
