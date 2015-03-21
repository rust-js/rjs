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
	pub items: Vec<Item>
}

pub enum Item {
	Function(Function),
	Block(Block),
	VarDecl(VarDecl),
	Empty,
	If(If),
	Do(Do),
	While(While),
	For(For),
	ForVar(ForVar),
	ForIn(ForIn),
	ForVarIn(ForVarIn),
	Continue(Continue),
	Break(Break),
	Return(Return),
	With(With),
	Switch(Switch),
	Throw(Throw),
	Try(Try),
	Debugger,
	Labelled(Labelled),
	ExprStmt(ExprStmt)
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
	pub expr: Option<Box<Expr>>
}

pub struct ExprSeq {
	pub exprs: Vec<Expr>
}

pub struct If {
	pub expr: ExprSeq,
	pub then: Box<Item>,
	pub else_: Option<Box<Item>>
}

pub struct Do {
	pub expr: Box<Expr>,
	pub stmt: Box<Item>
}

pub struct While {
	pub expr: Box<Expr>,
	pub stmt: Box<Item>
}

pub struct For {
	pub init: Option<ExprSeq>,
	pub test: Option<ExprSeq>,
	pub incr: Option<ExprSeq>,
	pub stmt: Box<Item>
}

pub struct ForVar {
	pub init: Option<VarDecl>,
	pub test: Option<ExprSeq>,
	pub incr: Option<ExprSeq>,
	pub stmt: Box<Item>
}

pub struct ForIn {
	pub in_: Box<Expr>,
	pub expr: ExprSeq,
	pub stmt: Box<Item>
}

pub struct ForVarIn {
	pub in_: Var,
	pub expr: ExprSeq,
	pub stmt: Box<Item>
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
	pub stmt: Box<Item>
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
	pub stmt: Box<Item>
}

pub struct ExprStmt {
	pub expr: ExprSeq
}

pub enum Expr {
	Function(Function),
	New(New),
	This,
	Missing,
	Ident(Ident),
	Unary(Unary),
	Literal(Rc<Lit>),
	Paren(ExprSeq),
	ArrayLiteral(ArrayLit),
	ObjectLiteral(ObjectLit),
	MemberIndex(MemberIndex),
	MemberDot(MemberDot),
	Call(Call),
	Binary(Binary),
	Ternary(Ternary),
	Assign(Assign)
}

pub struct New {
	pub expr: Box<Expr>,
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
	pub expr: Box<Expr>
}

pub struct Binary {
	pub op: Op,
	pub left: Box<Expr>,
	pub right: Box<Expr>
}

pub struct Assign {
	pub op: Op,
	pub left: Box<Expr>,
	pub right: ExprSeq
}

pub struct ArrayLit {
	pub elems: Vec<Expr>
}

pub struct ObjectLit {
	pub props: Vec<Property>
}

pub enum Property {
	Assignment(PropertyAssignment),
	Getter(PropertyGetter),
	Setter(PropertySetter)
}

pub struct PropertyAssignment {
	pub key: PropertyKey,
	pub value: Box<Expr>
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
	pub expr: Box<Expr>,
	pub index: ExprSeq
}

pub struct MemberDot {
	pub expr: Box<Expr>,
	pub ident: Ident
}

pub struct Call {
	pub expr: Box<Expr>,
	pub args: Vec<Expr>
}

pub struct Ternary {
	pub test: Box<Expr>,
	pub then: Box<Expr>,
	pub else_: Box<Expr>
}
