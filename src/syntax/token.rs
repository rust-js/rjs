use syntax::Span;
use syntax::Name;

#[derive(Copy, Clone)]
pub struct TokenAndSpan {
	pub token: Token,
	pub span: Span
}

impl TokenAndSpan {
	pub fn new(token: Token, span: Span) -> TokenAndSpan {
		TokenAndSpan {
			token: token,
			span: span
		}
	}
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Token {
	And,
	Assign,
	BitAnd,
	BitAndAssign,
	BitNot,
	BitOr,
	BitOrAssign,
	BitXOr,
	BitXOrAssign,
	Break,
	Case,
	Catch,
	Class,
	CloseBrace,
	CloseBracket,
	CloseParen,
	Colon,
	Comma,
	Comment,
	Const,
	Continue,
	Debugger,
	Default,
	Delete,
	Divide,
	DivideAssign,
	Do,
	Dot,
	Else,
	Enum,
	Equals,
	Export,
	Extends,
	Finally,
	For,
	Function,
	GreaterThan,
	GreaterThanEquals,
	Identifier(Name),
	IdentityEquals,
	IdentityNotEquals,
	If,
	Implements,
	Import,
	In,
	Instanceof,
	Interface,
	LeftShiftArithmetic,
	LeftShiftArithmeticAssign,
	LessThan,
	LessThanEquals,
	Let,
	LineTerminator,
	Literal(Lit),
	Minus,
	MinusAssign,
	MinusMinus,
	Modulus,
	ModulusAssign,
	Multiply,
	MultiplyAssign,
	New,
	Newline,
	Not,
	NotEquals,
	OpenBrace,
	OpenBracket,
	OpenParen,
	Or,
	Package,
	Plus,
	PlusAssign,
	PlusPlus,
	Private,
	Protected,
	Public,
	QuestionMark,
	Return,
	RightShiftArithmetic,
	RightShiftArithmeticAssign,
	RightShiftLogical,
	RightShiftLogicalAssign,
	SemiColon,
	Static,
	Super,
	Switch,
	This,
	Throw,
	Try,
	Typeof,
	Var,
	Void,
	While,
	Whitespace,
	With,
	Yield
}

impl Token {
	pub fn is_hidden(&self) -> bool {
		match *self {
			Token::Whitespace | Token::Newline | Token::Comment => true,
			_ => false
		}
	}
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Lit {
	Null,
	Boolean(bool),
	String(Name, bool),
	Integer(i32),
	Long(i64),
	Double(f64),
	Regex(Name, Name)
}

impl Lit {
	pub fn to_number(&self) -> Option<f64> {
		match *self {
			Lit::Null | Lit::Boolean(..) | Lit::String(..) | Lit::Regex(..) => None,
			Lit::Integer(value) => Some(value as f64),
			Lit::Long(value) => Some(value as f64),
			Lit::Double(value) => Some(value)
		}
	}
}

macro_rules! declare_idents {(
	$( ($str:expr, $name:ident, $num:expr); )*
) => {
	pub mod name {
		use syntax;
		use util::interner::StrInterner;
		
		$(
			pub const $name: syntax::Name = syntax::Name(-($num as i64 + 1));
		)*
		
		pub fn new_interner() -> StrInterner {
			let interner = StrInterner::new();
			
			$(
				let interned = interner.intern($str);
				assert_eq!(interned, $name);
			)*
			
			interner
		}
	}
}}

declare_idents! {
	( "null", NULL, 0 );
	( "true", TRUE, 1 );
	( "false", FALSE, 2 );
	( "break", BREAK, 3 );
	( "do", DO, 4 );
	( "instanceof", INSTANCEOF, 5 );
	( "typeof", TYPEOF, 6 );
	( "case", CASE, 7 );
	( "else", ELSE, 8 );
	( "new", NEW, 9 );
	( "var", VAR, 10 );
	( "catch", CATCH, 11 );
	( "finally", FINALLY, 12 );
	( "return", RETURN, 13 );
	( "void", VOID, 14 );
	( "continue", CONTINUE, 15 );
	( "for", FOR, 16 );
	( "switch", SWITCH, 17 );
	( "while", WHILE, 18 );
	( "debugger", DEBUGGER, 19 );
	( "function", FUNCTION, 20 );
	( "this", THIS, 21 );
	( "with", WITH, 22 );
	( "default", DEFAULT, 23 );
	( "if", IF, 24 );
	( "throw", THROW, 25 );
	( "delete", DELETE, 26 );
	( "in", IN, 27 );
	( "try", TRY, 28 );
	( "class", CLASS, 29 );
	( "enum", ENUM, 30 );
	( "extends", EXTENDS, 31 );
	( "super", SUPER, 32 );
	( "const", CONST, 33 );
	( "export", EXPORT, 34 );
	( "import", IMPORT, 35 );
	( "implements", IMPLEMENTS, 36 );
	( "let", LET, 37 );
	( "private", PRIVATE, 38 );
	( "public", PUBLIC, 39 );
	( "interface", INTERFACE, 40 );
	( "package", PACKAGE, 41 );
	( "protected", PROTECTED, 42 );
	( "static", STATIC, 43 );
	( "yield", YIELD, 44 );
	( "get", GET, 45 );
	( "set", SET, 46 );
	( "arguments", ARGUMENTS, 47 );
	( "Function", FUNCTION_CLASS, 48 );
	( "constructor", CONSTRUCTOR, 49 );
	( "call", CALL, 50 );
	( "apply", APPLY, 51 );
	( "toString", TO_STRING, 52 );
	( "toLocaleString", TO_LOCALE_STRING, 53 );
	( "length", LENGTH, 54 );
	( "Object", OBJECT_CLASS, 55 );
	( "valueOf", VALUE_OF, 56 );
	( "hasOwnProperty", HAS_OWN_PROPERTY, 57 );
	( "isPrototypeOf", IS_PROTOTYPE_OF, 58 );
	( "propertyIsEnumerable", PROPERTY_IS_ENUMERABLE, 59 );
	( "getPrototypeOf", GET_PROTOTYPE_OF, 60 );
	( "defineProperty", DEFINE_PROPERTY, 61 );
	( "prototype", PROTOTYPE, 62 );
	( "toGMTString", TO_GMT_STRING, 63 );
	( "create", CREATE, 64 );
	( "getOwnPropertyDescriptor", GET_OWN_PROPERTY_DESCRIPTOR, 65 );
	( "value", VALUE, 66 );
	( "writable", WRITABLE, 67 );
	( "enumerable", ENUMERABLE, 68 );
	( "configurable", CONFIGURABLE, 69 );
	( "escape", ESCAPE, 70 );
	( "unescape", UNESCAPE, 71 );
	( "String", STRING_CLASS, 72 );
	( "substr", SUBSTR, 73 );
	( "Date", DATE_CLASS, 74 );
	( "getYear", GET_YEAR, 75 );
	( "setYear", SET_YEAR, 76 );
	( "Array", ARRAY_CLASS, 77 );
	( "concat", CONCAT, 78 );
	( "join", JOIN, 79 );
	( "pop", POP, 80 );
	( "push", PUSH, 81 );
	( "reverse", REVERSE, 82 );
	( "shift", SHIFT, 83 );
	( "slice", SLICE, 84 );
	( "sort", SORT, 85 );
	( "splice", SPLICE, 86 );
	( "unshift", UNSHIFT, 87 );
	( "indexOf", INDEX_OF, 88 );
	( "lastIndexOf", LAST_INDEX_OF, 89 );
	( "every", EVERY, 90 );
	( "some", SOME, 91 );
	( "forEach", FOR_EACH, 92 );
	( "map", MAP, 93 );
	( "filter", FILTER, 94 );
	( "reduce", REDUCE, 95 );
	( "reduceRight", REDUCE_RIGHT, 96 );
	( "isArray", IS_ARRAY, 97 );
	( "Boolean", BOOLEAN_CLASS, 98 );
	( "Number", NUMBER_CLASS, 99 );
	( "TypeError", TYPE_ERROR_CLASS, 100 );
	( "RangeError", RANGE_ERROR_CLASS, 101 );
	( "SyntaxError", SYNTAX_ERROR_CLASS, 102 );
	( "eval", EVAL, 103 );
	( "ReferenceError", REFERENCE_ERROR_CLASS, 104 );
	( "undefined", UNDEFINED, 105 );
	( "NaN", NAN, 106 );
	( "Infinity", INFINITY, 107 );
	( "MAX_VALUE", MAX_VALUE, 108 );
	( "MIN_VALUE", MIN_VALUE, 109 );
	( "NEGATIVE_INFINITY", NEGATIVE_INFINITY, 110 );
	( "POSITIVE_INFINITY", POSITIVE_INFINITY, 111 );
	( "Math", MATH_CLASS, 112 );
	( "RegExp", REGEXP_CLASS, 113 );
	( "JSON", JSON_CLASS, 114 );
	( "Arguments", ARGUMENTS_CLASS, 115 );
	( "use strict", USE_STRICT, 116 );
	( "parseInt", PARSE_INT, 117 );
	( "parseFloat", PARSE_FLOAT, 118 );
	( "isNaN", IS_NAN, 119 );
	( "isFinite", IS_FINITE, 120 );
	( "callee", CALLEE, 121 );
	( "caller", CALLER, 122 );
	( "fromCharCode", FROM_CHAR_CODE, 123 );
	( "pow", POW, 124 );
	( "floor", FLOOR, 125 );
	( "Console", CONSOLE_CLASS, 126 );
	( "console", CONSOLE, 127 );
	( "log", LOG, 128 );
	( "preventExtensions", PREVENT_EXTENSIONS, 129 );
	( "PI", PI, 130 );
	( "E", E, 131 );
	( "LN10", LN10, 132 );
	( "LN2", LN2, 133 );
	( "LOG2E", LOG2E, 134 );
	( "LOG10E", LOG10E, 135 );
	( "SQRT1_2", SQRT1_2, 136 );
	( "SQRT2", SQRT2, 137 );
	( "Error", ERROR_CLASS, 138 );
	( "EvalError", EVAL_ERROR_CLASS, 139 );
	( "URIError", URI_ERROR_CLASS, 140 );
	( "NativeError", NATIVE_ERROR_CLASS, 141 );
	( "getOwnPropertyNames", GET_OWN_PROPERTY_NAMES, 142 );
	( "", EMPTY, 143 );
	( "name", NAME, 144 );
	( "toFixed", TO_FIXED, 145 );
	( "charAt", CHAR_AT, 146 );
	( "charCodeAt", CHAR_CODE_AT, 147 );
	( "bind", BIND, 148 );
	( "prim", PRIM, 149 );
	( "sprim", SPRIM, 150 );
	( "nprim", NPRIM, 151 );
	( "bool", BOOL, 152 );
	( "number", NUMBER, 153 );
	( "int", INT, 154 );
	( "i32", I32, 155 );
	( "u32", U32, 156 );
	( "u16", U16, 157 );
	( "string", STRING, 158 );
	( "object", OBJECT, 159 );
	( "Intrinsics", INTRINSICS_CLASS, 160 );
	( "isCallable", IS_CALLABLE, 161 );
	( "hasProperty", HAS_PROPERTY, 162 );
	( "min", MIN, 163 );
	( "max", MAX, 164 );
	( "registerFunction", REGISTER_FUNCTION, 165 );
	( "freeze", FREEZE, 166 );
	( "toISO", TO_ISO, 167 );
	( "parse", PARSE, 168 );
	( "UTC", UTC, 169 );
	( "now", NOW, 170 );
	( "toDateString", TO_DATE_STRING, 171 );
	( "toTimeString", TO_TIME_STRING, 172 );
	( "toLocaleDateString", TO_LOCALE_DATE_STRING, 173 );
	( "toLocaleTimeString", TO_LOCALE_TIME_STRING, 174 );
	( "getTime", GET_TIME, 175 );
	( "getFullYear", GET_FULL_YEAR, 176 );
	( "getUTCFullYear", GET_UTC_FULL_YEAR, 177 );
	( "getMonth", GET_MONTH, 178 );
	( "getUTCMonth", GET_UTC_MONTH, 179 );
	( "getDate", GET_DATE, 180 );
	( "getUTCDate", GET_UTC_DATE, 181 );
	( "getDay", GET_DAY, 182 );
	( "getUTCDay", GET_UTC_DAY, 183 );
	( "getHours", GET_HOURS, 184 );
	( "getUTCHours", GET_UTC_HOURS, 185 );
	( "getMinutes", GET_MINUTES, 186 );
	( "getUTCMinutes", GET_UTC_MINUTES, 187 );
	( "getSeconds", GET_SECONDS, 188 );
	( "getUTCSeconds", GET_UTC_SECONDS, 189 );
	( "getMilliseconds", GET_MILLISECONDS, 190 );
	( "getUTCMilliseconds", GET_UTC_MILLISECONDS, 191 );
	( "getTimezoneOffset", GET_TIMEZONE_OFFSET, 192 );
	( "setMilliseconds", SET_MILLISECONDS, 193 );
	( "setUTCMilliseconds", SET_UTC_MILLISECONDS, 194 );
	( "setSeconds", SET_SECONDS, 195 );
	( "setUTCSeconds", SET_UTC_SECONDS, 196 );
	( "setMinutes", SET_MINUTES, 197 );
	( "setUTCMinutes", SET_UTC_MINUTES, 198 );
	( "setHours", SET_HOURS, 199 );
	( "setUTCHours", SET_UTC_HOURS, 200 );
	( "setDate", SET_DATE, 201 );
	( "setUTCDate", SET_UTC_DATE, 202 );
	( "setMonth", SET_MONTH, 203 );
	( "setUTCMonth", SET_UTC_MONTH, 204 );
	( "setFullYear", SET_FULL_YEAR, 205 );
	( "setUTCFullYear", SET_UTC_FULL_YEAR, 206 );
	( "toUTCString", TO_UTC_STRING, 207 );
	( "toISOString", TO_ISO_STRING, 208 );
	( "toJSON", TO_JSON, 209 );
	( "setTime", SET_TIME, 210 );
	( "abs", ABS, 211 );
	( "decodeURI", DECODE_URI, 212 );
	( "substring", SUBSTRING, 213 );
	( "decodeURIComponent", DECODE_URI_COMPONENT, 214 );
	( "encodeURI", ENCODE_URI, 215 );
	( "encodeURIComponent", ENCODE_URI_COMPONENT, 216 );
	( "toLowerCase", TO_LOWER_CASE, 217 );
	( "toLocaleLowerCase", TO_LOCALE_LOWER_CASE, 218 );
	( "toUpperCase", TO_UPPER_CASE, 219 );
	( "toLocaleUpperCase", TO_LOCALE_UPPER_CASE, 220 );
	( "message", MESSAGE, 221 );
	( "isExtensible", IS_EXTENSIBLE, 222 );
}
