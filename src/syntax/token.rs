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
			pub const $name: syntax::Name = syntax::Name(-($num as i32 + 1));
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
}
