#![allow(dead_code)]

use syntax::token::Lit;
use util::interner::{StrInterner, RcStr};
use std::rc::Rc;
use std::ops::Deref;
use std::fmt;

#[derive(Clone, PartialEq, Hash, PartialOrd, Eq, Ord)]
pub struct InternedString {
    string: RcStr,
}

/// Represents a string stored in the task-local interner. Because the
/// interner lives for the life of the task, this can be safely treated as an
/// immortal string, as long as it never crosses between tasks.
///
/// FIXME(pcwalton): You must be careful about what you do in the destructors
/// of objects stored in TLS, because they may run after the interner is
/// destroyed. In particular, they must not access string contents. This can
/// be fixed in the future by just leaking all strings until task death
/// somehow.
impl InternedString {
    #[inline]
    pub fn new(string: &'static str) -> InternedString {
        InternedString {
            string: RcStr::new(string),
        }
    }

    #[inline]
    fn new_from_rc_str(string: RcStr) -> InternedString {
        InternedString {
            string: string,
        }
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &str { &*self.string }
}

/*
#[allow(deprecated)]
impl BytesContainer for InternedString {
    fn container_as_bytes<'a>(&'a self) -> &'a [u8] {
        // FIXME #12938: This is a workaround for the incorrect signature
        // of `BytesContainer`, which is itself a workaround for the lack of
        // DST.
        unsafe {
            let this = &self[..];
            mem::transmute::<&[u8],&[u8]>(this.container_as_bytes())
        }
    }
}
*/

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.string, f)
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.string, f)
    }
}

impl<'a> PartialEq<&'a str> for InternedString {
    #[inline(always)]
    fn eq(&self, other: & &'a str) -> bool {
        PartialEq::eq(&self.string[..], *other)
    }
    #[inline(always)]
    fn ne(&self, other: & &'a str) -> bool {
        PartialEq::ne(&self.string[..], *other)
    }
}

impl<'a> PartialEq<InternedString > for &'a str {
    #[inline(always)]
    fn eq(&self, other: &InternedString) -> bool {
        PartialEq::eq(*self, &other.string[..])
    }
    #[inline(always)]
    fn ne(&self, other: &InternedString) -> bool {
        PartialEq::ne(*self, &other.string[..])
    }
}

/*
impl Decodable for InternedString {
    fn decode<D: Decoder>(d: &mut D) -> Result<InternedString, D::Error> {
        Ok(get_name(get_ident_interner().intern(&try!(d.read_str())[..])))
    }
}

impl Encodable for InternedString {
    fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
        s.emit_str(&self.string)
    }
}
*/

#[derive(Eq, Ord, PartialEq, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct Name(pub u32);

impl Name {
	pub fn as_str<'a>(&'a self, interner: &StrInterner) -> &'a str {
		unsafe {
			// FIXME #12938: can't use copy_lifetime since &str isn't a &T
			::std::mem::transmute::<&str,&str>(&InternedString::new_from_rc_str(interner.get(*self)))
		}
	}

	pub fn usize(&self) -> usize {
		let Name(nm) = *self;
		nm as usize
	}
	
	pub fn ident(&self) -> Ident {
		Ident { name: *self }
	}
}

pub struct Program {
	pub items: Block
}

pub enum Item {
	Block(Block),
	Break(Option<Ident>),
	Continue(Option<Ident>),
	Debugger,
	Do(Option<Ident>, Box<Expr>, Box<Item>),
	Empty,
	ExprStmt(ExprSeq),
	For(Option<Ident>, Option<ExprSeq>, Option<ExprSeq>, Option<ExprSeq>, Box<Item>),
	ForIn(Option<Ident>, Box<Expr>, ExprSeq, Box<Item>),
	ForVar(Option<Ident>, Option<Vec<Var>>, Option<ExprSeq>, Option<ExprSeq>, Box<Item>),
	ForVarIn(Option<Ident>, Ident, ExprSeq, Box<Item>),
	Function(Option<Ident>, Vec<Ident>, Block),
	If(ExprSeq, Box<Item>, Option<Box<Item>>),
	Return(Option<ExprSeq>),
	Switch(Option<Ident>, ExprSeq, Vec<SwitchClause>),
	Throw(ExprSeq),
	Try(Block, Option<Catch>, Option<Block>),
	VarDecl(Vec<Var>),
	While(Option<Ident>, Box<Expr>, Box<Item>),
	With(ExprSeq, Box<Item>)
}

pub struct Block {
	pub stmts: Vec<Item>
}

pub struct Ident {
	pub name: Name
}

pub struct Var {
	pub ident: Ident,
	pub expr: Option<Box<Expr>>
}

pub struct ExprSeq {
	pub exprs: Vec<Expr>
}

pub enum SwitchClause {
	Case(ExprSeq, Vec<Item>),
	Default(Vec<Item>)
}

pub struct Catch {
	pub ident: Ident,
	pub block: Block
}

pub enum Expr {
	ArrayLiteral(Vec<Expr>),
	Assign(Op, Box<Expr>, ExprSeq),
	Binary(Op, Box<Expr>, Box<Expr>),
	Call(Box<Expr>, Vec<Expr>),
	Function(Option<Ident>, Vec<Ident>, Block),
	Ident(Ident),
	Literal(Rc<Lit>),
	MemberDot(Box<Expr>, Ident),
	MemberIndex(Box<Expr>, ExprSeq),
	Missing,
	New(Box<Expr>, Option<Vec<Expr>>),
	ObjectLiteral(Vec<Property>),
	Paren(ExprSeq),
	Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
	This,
	Unary(Op, Box<Expr>)
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Op {
	Add,
	And,
	Assign,
	BitAnd,
	BitNot,
	BitOr,
	BitXOr,
	Delete,
	Divide,
	Equals,
	GreaterThan,
	GreaterThanEquals,
	IdentityEquals,
	IdentityNotEquals,
	In,
	InstanceOf,
	LeftShiftArithmetic,
	LessThan,
	LessThanEquals,
	Modulus,
	Multiply,
	Negative,
	Not,
	NotEquals,
	Or,
	Positive,
	PostDecr,
	PostIncr,
	PreDecr,
	PreIncr,
	RightShiftArithmetic,
	RightShiftLogical,
	Subtract,
	Typeof,
	Void
}

pub enum Property {
	Assignment(PropertyKey, Box<Expr>),
	Getter(Option<Ident>, Block),
	Setter(Option<Ident>, Ident, Block)
}

pub enum PropertyKey {
	Ident(Ident),
	Literal(Rc<Lit>)
}
