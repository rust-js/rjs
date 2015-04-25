pub mod visitor;

use syntax::token::Lit;
use util::interner::{StrInterner, RcStr};
use std::rc::Rc;
use std::ops::Deref;
use std::fmt;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

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

impl<'a> PartialEq<InternedString> for &'a str {
    #[inline(always)]
    fn eq(&self, other: &InternedString) -> bool {
        PartialEq::eq(*self, &other.string[..])
    }
    
    #[inline(always)]
    fn ne(&self, other: &InternedString) -> bool {
        PartialEq::ne(*self, &other.string[..])
    }
}

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
}

pub struct AstContext {
	pub functions: Vec<Box<Function>>
}

impl AstContext {
	pub fn new() -> AstContext {
		AstContext {
			functions: Vec::new()
		}
	}
}

#[derive(Copy, Clone, Debug)]
pub struct FunctionRef(pub usize);

impl FunctionRef {
	pub fn usize(&self) -> usize {
		let &FunctionRef(index) = self;
		index
	}
}

pub struct Function {
	pub global: bool,
	pub name: Option<Name>,
	pub block: RootBlock
}

pub struct Locals {
	pub slots: Vec<Slot>,
	/// Whether to de-optimize this function. This applies when there is an eval
	/// somewhere in the stack.
	pub deopt: bool
}

impl Locals {
	pub fn new(slots: Vec<Slot>) -> Locals {
		Locals {
			slots: slots,
			deopt: false
		}
	}
}

/// A local slot is a declaration of a local that is known within the current
/// scope (function). This only applies to functions and when optimizations are
/// enabled. When optimizations are disabled (i.e. when there is an eval
/// somewhere in the stack), local slots are not tracked.
pub struct Slot {
	pub name: Name,
	pub arg: Option<u32>,
	pub lifted: bool
}

#[derive(Copy, Clone, Debug)]
pub struct SlotRef(pub usize);

impl SlotRef {
	pub fn usize(&self) -> usize {
		let &SlotRef(index) = self;
		index
	}
}

pub struct RootBlock {
	pub args: Vec<Name>,
	pub block: Block,
	pub locals: RefCell<Locals>
}

pub struct Block {
	pub stmts: Vec<Item>,
	pub locals: HashMap<Name, SlotRef>
}

pub enum Item {
	Block(Option<Label>, Block),
	Break(Option<Label>),
	Continue(Option<Label>),
	Debugger,
	Do(Option<Label>, Box<Expr>, Box<Item>),
	Empty,
	ExprStmt(ExprSeq),
	For(Option<Label>, Option<ExprSeq>, Option<ExprSeq>, Option<ExprSeq>, Box<Item>),
	ForIn(Option<Label>, Box<Expr>, ExprSeq, Box<Item>),
	ForVar(Option<Label>, Option<Vec<Var>>, Option<ExprSeq>, Option<ExprSeq>, Box<Item>),
	ForVarIn(Option<Label>, Ident, ExprSeq, Box<Item>),
	Function(Ident, FunctionRef),
	If(ExprSeq, Box<Item>, Option<Box<Item>>),
	Return(Option<ExprSeq>),
	Switch(Option<Label>, ExprSeq, Vec<SwitchClause>),
	Throw(ExprSeq),
	Try(Block, Option<Catch>, Option<Block>),
	VarDecl(Vec<Var>),
	While(Option<Label>, Box<Expr>, Box<Item>),
	With(ExprSeq, Box<Item>)
}

pub struct Label {
	pub name: Name
}

#[derive(Debug)]
pub struct Ident {
	pub name: Name,
	pub state: Cell<IdentState>
}

/// Resolve state of an identifier. Variables can:
///
/// * Reference a global,
/// * Reference a local (either a parameter or a local); or
/// * Be be scoped.
///
/// Scoped identifiers apply when optimizations are disabled (i.e. when there is
/// an eval somewhere in the stack).
///
/// Locals have a reference to the slot and a depth. The depth is used to identify
/// and resolve closures.
#[derive(Copy, Clone, Debug)]
pub enum IdentState {
	None,
	Global,
	Scoped,
	Arguments,
	Slot(SlotRef),
	LiftedSlot(SlotRef, u32)
}

impl IdentState {
	pub fn is_none(&self) -> bool {
		if let IdentState::None = *self {
			true
		} else {
			false
		}
	}
}

pub struct Var {
	pub ident: Ident,
	pub expr: Option<Box<Expr>>
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Expr {
	ArrayLiteral(Vec<Expr>),
	Assign(Op, Box<Expr>, ExprSeq),
	Binary(Op, Box<Expr>, Box<Expr>),
	Call(Box<Expr>, Vec<Expr>),
	Function(FunctionRef),
	Ident(Ident),
	Literal(Rc<Lit>),
	MemberDot(Box<Expr>, Name),
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

#[derive(Debug)]
pub enum Property {
	Assignment(PropertyKey, Box<Expr>),
	Getter(Option<Name>, FunctionRef),
	Setter(Option<Name>, FunctionRef)
}

#[derive(Debug)]
pub enum PropertyKey {
	Ident(Name),
	Literal(Rc<Lit>)
}
