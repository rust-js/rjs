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
	pub fn as_str<'a>(&'a self, interner: StrInterner) -> &'a str {
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
	pub items: Vec<Box<Item>>
}

pub enum Item {
	Function(Box<Function>),
	Block(Box<Block>),
	VarDecl(Box<VarDecl>),
	Empty,
	If(Box<If>),
	Do(Box<Do>),
	While(Box<While>),
	For(Box<For>),
	ForVar(Box<ForVar>),
	ForIn(Box<ForIn>),
	ForVarIn(Box<ForVarIn>),
	Continue(Box<Continue>),
	Break(Box<Break>),
	Return(Box<Return>),
	With(Box<With>),
	Switch(Box<Switch>),
	Throw(Box<Throw>),
	Try(Box<Try>),
	Debugger,
	Labelled(Box<Labelled>),
	ExprStmt(Box<ExprStmt>)
}

pub struct Function {
	pub ident: Option<Ident>,
	pub args: Vec<Ident>,
	pub block: Block
}

pub struct Block {
	pub stmts: Vec<Item>
}

pub struct Ident {
	pub name: Name
}

pub struct VarDecl {
	pub vars: Vec<Var>
}

pub struct Var {
	pub ident: Ident,
	pub expr: Option<Expr>
}

pub struct ExprSeq {
	pub exprs: Vec<Expr>
}

pub struct If {
	pub expr: ExprSeq,
	pub then: Item,
	pub else_: Option<Item>
}

pub struct Do {
	pub expr: Expr,
	pub stmt: Item
}

pub struct While {
	pub expr: Expr,
	pub stmt: Item
}

pub struct For {
	pub init: Option<ExprSeq>,
	pub test: Option<ExprSeq>,
	pub incr: Option<ExprSeq>,
	pub stmt: Item
}

pub struct ForVar {
	pub init: Option<VarDecl>,
	pub test: Option<ExprSeq>,
	pub incr: Option<ExprSeq>,
	pub stmt: Item
}

pub struct ForIn {
	pub in_: Expr,
	pub expr: ExprSeq,
	pub stmt: Item
}

pub struct ForVarIn {
	pub in_: Var,
	pub expr: ExprSeq,
	pub stmt: Item
}

pub struct Continue {
	pub ident: Option<Ident>
}

pub struct Break {
	pub ident: Option<Ident>
}

pub struct Return {
	pub expr: Option<ExprSeq>
}

pub struct With {
	pub expr: ExprSeq,
	pub stmt: Item
}

pub struct Switch {
	pub expr: ExprSeq,
	pub cases: Vec<CaseClause>
}

pub struct CaseClause {
	pub expr: Option<ExprSeq>,
	pub stmts: Vec<Item>
}

pub struct Throw {
	pub expr: ExprSeq
}

pub struct Try {
	pub try: Block,
	pub catch: Option<CatchClause>,
	pub finally: Option<Block>
}

pub struct CatchClause {
	pub ident: Ident,
	pub block: Block
}

pub struct Labelled {
	pub ident: Ident,
	pub stmt: Item
}

pub struct ExprStmt {
	pub expr: ExprSeq
}

pub enum Expr {
	Function(Box<Function>),
	New(Box<New>),
	This,
	Missing,
	Ident(Box<Ident>),
	Unary(Box<Unary>),
	Literal(Rc<Lit>),
	Paren(Box<ExprSeq>),
	ArrayLiteral(Box<ArrayLit>),
	ObjectLiteral(Box<ObjectLit>),
	MemberIndex(Box<MemberIndex>),
	MemberDot(Box<MemberDot>),
	Call(Box<Call>),
	Binary(Box<Binary>),
	Ternary(Box<Ternary>),
	Assign(Box<Assign>)
}

pub struct New {
	pub expr: Expr,
	pub args: Option<Vec<Expr>>
}

#[derive(PartialEq, Debug)]
pub enum Op {
	Delete,
	Void,
	Typeof,
	Assign,
	PreIncr,
	PostIncr,
	PreDecr,
	PostDecr,
	Plus,
	Minus,
	BitNot,
	Not,
	Multiply,
	Divide,
	Modulus,
	Add,
	Subtract,
	LeftShiftArithmetic,
	RightShiftArithmetic,
	RightShiftLogical,
	LessThan,
	GreaterThan,
	LessThanEquals,
	GreaterThanEquals,
	InstanceOf,
	In,
	Equals,
	NotEquals,
	IdentityEquals,
	IdentityNotEquals,
	BitAnd,
	BitXOr,
	BitOr,
	And,
	Or
}

pub struct Unary {
	pub op: Op,
	pub expr: Expr
}

pub struct Binary {
	pub op: Op,
	pub left: Expr,
	pub right: Expr
}

pub struct Assign {
	pub op: Op,
	pub left: Expr,
	pub right: ExprSeq
}

pub struct ArrayLit {
	pub elems: Vec<Expr>
}

pub struct ObjectLit {
	pub props: Vec<Property>
}

pub enum Property {
	Assignment(Box<PropertyAssignment>),
	Getter(Box<PropertyGetter>),
	Setter(Box<PropertySetter>)
}

pub struct PropertyAssignment {
	pub key: PropertyKey,
	pub value: Expr
}

pub enum PropertyKey {
	Ident(Ident),
	Literal(Rc<Lit>)
}

pub struct PropertyGetter {
	pub name: Option<Ident>,
	pub block: Block
}

pub struct PropertySetter {
	pub name: Option<Ident>,
	pub param: Ident,
	pub block: Block
}

pub struct MemberIndex {
	pub expr: Expr,
	pub index: ExprSeq
}

pub struct MemberDot {
	pub expr: Expr,
	pub ident: Ident
}

pub struct Call {
	pub expr: Expr,
	pub args: Vec<Expr>
}

pub struct Ternary {
	pub test: Expr,
	pub then: Expr,
	pub else_: Expr
}
