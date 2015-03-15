#![allow(dead_code)]

use syntax::Span;
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
	Identifier(String),
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
	HexInteger(String),
	OctalInteger(String),
	Decimal(String),
	Regex(String, String)
}
