pub mod visitor;

use syntax::token::Lit;
use super::Name;
use std::rc::Rc;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

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
pub struct FunctionRef(pub u32);

impl FunctionRef {
	pub fn usize(&self) -> usize {
		self.0 as usize
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct SlotRef(pub usize);

impl SlotRef {
	pub fn usize(&self) -> usize {
		self.0
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
#[derive(Copy, Clone, Debug, PartialEq)]
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
	New(Box<Expr>),
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

impl Op {
	pub fn precedence(&self) -> u32 {
		// From https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence.
		
		match *self {
			// 19: Grouping
			// 18: Member access
			// 18: Computed member access
			// 18: New with argument list
			// 17: Function call
			// 17: New without argument list
			// 16: Postfix increment
			Op::PostIncr => 16,
			// 16: Postfix decrement
			Op::PostDecr => 16,
			// 15: Logical NOT
			Op::Not => 15,
			// 15: Bitwise NOT
			Op::BitNot => 15,
			// 15: Unary plus
			Op::Positive => 15,
			// 15: Unary negation
			Op::Negative => 15,
			// 15: Prefix increment
			Op::PreIncr => 15,
			// 15: Prefix decrement
			Op::PreDecr => 15,
			// 15: Typeof
			Op::Typeof => 15,
			// 15: Void
			Op::Void => 15,
			// 15: Delete
			Op::Delete => 15,
			// 14: Multiplication
			Op::Multiply => 14,
			// 14: Division
			Op::Divide => 14,
			// 14: Remainder
			Op::Modulus => 14,
			// 13: Addition
			Op::Add => 13,
			// 13: Subtraction
			Op::Subtract => 13,
			// 12: Bitwise left shift
			Op::LeftShiftArithmetic => 12,
			// 12: Bitwise right shift
			Op::RightShiftArithmetic => 12,
			// 12: Bitwise unsigned right shift
			Op::RightShiftLogical => 12,
			// 11: Less than
			Op::LessThan => 11,
			// 11: Less than or equal
			Op::LessThanEquals => 11,
			// 11: Greater than
			Op::GreaterThan => 11,
			// 11: Greater than or equal
			Op::GreaterThanEquals => 11,
			// 11: In
			Op::In => 11,
			// 11: Instanceof
			Op::InstanceOf => 11,
			// 10: Equality
			Op::Equals => 10,
			// 10: Inequality
			Op::NotEquals => 10,
			// 10: Strict equality
			Op::IdentityEquals => 10,
			// 10: Strict inequality
			Op::IdentityNotEquals => 10,
			// 9: Bitwise AND
			Op::BitAnd => 9,
			// 8: Bitwise XOR
			Op::BitXOr => 8,
			// 7: Bitwise OR
			Op::BitOr => 7,
			// 6: Logical AND
			Op::And => 6,
			// 5: Logical OR
			Op::Or => 5,
			// 4: Conditional
			// 3: Assignment
			Op::Assign => 3
			// 2: Yield
			// 1: Spread
			// 0: Comma, sequence
		}
	}
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
