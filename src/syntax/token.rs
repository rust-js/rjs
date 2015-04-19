#![allow(dead_code)]

use syntax::Span;
use syntax::ast::Name;
use std::rc::Rc;

pub struct TokenAndSpan {
	token: Token,
	span: Span
}

impl TokenAndSpan {
	pub fn new(token: Token, span: Span) -> TokenAndSpan {
		TokenAndSpan {
			token: token,
			span: span
		}
	}
	
	pub fn token(&self) -> &Token {
		&self.token
	}
	
	pub fn span(&self) -> &Span {
		&self.span
	}
}

#[derive(PartialEq, Debug)]
pub enum Token {
	Comment,
	Whitespace,
	Newline,
	LineTerminator,
	OpenBracket,
	CloseBracket,
	OpenParen,
	CloseParen,
	OpenBrace,
	CloseBrace,
	SemiColon,
	Comma,
	Assign,
	QuestionMark,
	Colon,
	Dot,
	PlusPlus,
	MinusMinus,
	Plus,
	Minus,
	BitNot,
	Not,
	Multiply,
	Divide,
	Modulus,
	RightShiftArithmetic,
	LeftShiftArithmetic,
	RightShiftLogical,
	LessThan,
	GreaterThan,
	LessThanEquals,
	GreaterThanEquals,
	Equals,
	NotEquals,
	IdentityEquals,
	IdentityNotEquals,
	BitAnd,
	BitXOr,
	BitOr,
	And,
	Or,
	MultiplyAssign,
	DivideAssign,
	ModulusAssign,
	PlusAssign,
	MinusAssign,
	LeftShiftArithmeticAssign,
	RightShiftArithmeticAssign,
	RightShiftLogicalAssign,
	BitAndAssign,
	BitXOrAssign,
	BitOrAssign,
	Break,
	Do,
	Instanceof,
	Typeof,
	Case,
	Else,
	New,
	Var,
	Catch,
	Finally,
	Return,
	Void,
	Continue,
	For,
	Switch,
	While,
	Debugger,
	Function,
	This,
	With,
	Default,
	If,
	Throw,
	Delete,
	In,
	Try,
	Class,
	Enum,
	Extends,
	Super,
	Const,
	Export,
	Import,
	Implements,
	Let,
	Private,
	Public,
	Interface,
	Package,
	Protected,
	Static,
	Yield,
	Identifier(Name),
	Literal(Rc<Lit>),
}

impl Token {
	pub fn is_hidden(&self) -> bool {
		match *self {
			Token::Whitespace | Token::Newline | Token::Comment => true,
			_ => false
		}
	}
}

#[derive(PartialEq, Debug)]
pub enum Lit {
	Null,
	Boolean(bool),
	String(String),
	Integer(i32),
	Long(i64),
	Double(f64),
	Regex(String, String)
}

macro_rules! declare_idents {(
	$( ($str:expr, $name:ident, $num:expr); )*
) => {
	pub mod keywords {
		use syntax::ast;
		use util::interner::StrInterner;
		
		$(
			pub const $name: ast::Name = ast::Name($num);
		)*
		
		pub fn new_interner() -> StrInterner {
			let interner = StrInterner::new();
			
			$(
				interner.intern($str);
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
}
