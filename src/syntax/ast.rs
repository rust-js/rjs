#![allow(dead_code)]

use syntax::token::Lit;
use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
	pub items: Vec<Box<Item>>
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Function {
	pub ident: Option<Ident>,
	pub args: Vec<Ident>,
	pub block: Block
}

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Item>
}

#[derive(Debug)]
pub struct Ident {
	pub name: String
}

#[derive(Debug)]
pub struct VarDecl {
	pub vars: Vec<Var>
}

#[derive(Debug)]
pub struct Var {
	pub ident: Ident,
	pub expr: Option<Expr>
}

#[derive(Debug)]
pub struct ExprSeq {
	pub exprs: Vec<Expr>
}

#[derive(Debug)]
pub struct If {
	pub expr: ExprSeq,
	pub then: Item,
	pub else_: Option<Item>
}

#[derive(Debug)]
pub struct Do {
	pub expr: Expr,
	pub stmt: Item
}

#[derive(Debug)]
pub struct While {
	pub expr: Expr,
	pub stmt: Item
}

#[derive(Debug)]
pub struct For {
	pub init: Option<ExprSeq>,
	pub test: Option<ExprSeq>,
	pub incr: Option<ExprSeq>,
	pub stmt: Item
}

#[derive(Debug)]
pub struct ForVar {
	pub init: Option<VarDecl>,
	pub test: Option<ExprSeq>,
	pub incr: Option<ExprSeq>,
	pub stmt: Item
}

#[derive(Debug)]
pub struct ForIn {
	pub in_: Expr,
	pub expr: ExprSeq,
	pub stmt: Item
}

#[derive(Debug)]
pub struct ForVarIn {
	pub in_: Var,
	pub expr: ExprSeq,
	pub stmt: Item
}

#[derive(Debug)]
pub struct Continue {
	pub ident: Option<Ident>
}

#[derive(Debug)]
pub struct Break {
	pub ident: Option<Ident>
}

#[derive(Debug)]
pub struct Return {
	pub expr: Option<ExprSeq>
}

#[derive(Debug)]
pub struct With {
	pub expr: ExprSeq,
	pub stmt: Item
}

#[derive(Debug)]
pub struct Switch {
	pub expr: ExprSeq,
	pub cases: Vec<CaseClause>
}

#[derive(Debug)]
pub struct CaseClause {
	pub expr: Option<ExprSeq>,
	pub stmts: Vec<Item>
}

#[derive(Debug)]
pub struct Throw {
	pub expr: ExprSeq
}

#[derive(Debug)]
pub struct Try {
	pub try: Block,
	pub catch: Option<CatchClause>,
	pub finally: Option<Block>
}

#[derive(Debug)]
pub struct CatchClause {
	pub ident: Ident,
	pub block: Block
}

#[derive(Debug)]
pub struct Labelled {
	pub ident: Ident,
	pub stmt: Item
}

#[derive(Debug)]
pub struct ExprStmt {
	pub expr: ExprSeq
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Unary {
	pub op: Op,
	pub expr: Expr
}

#[derive(Debug)]
pub struct Binary {
	pub op: Op,
	pub left: Expr,
	pub right: Expr
}

#[derive(Debug)]
pub struct Assign {
	pub op: Op,
	pub left: Expr,
	pub right: ExprSeq
}

#[derive(Debug)]
pub struct ArrayLit {
	pub elems: Vec<Expr>
}

#[derive(Debug)]
pub struct ObjectLit {
	pub props: Vec<Property>
}

#[derive(Debug)]
pub enum Property {
	Assignment(Box<PropertyAssignment>),
	Getter(Box<PropertyGetter>),
	Setter(Box<PropertySetter>)
}

#[derive(Debug)]
pub struct PropertyAssignment {
	pub key: PropertyKey,
	pub value: Expr
}

#[derive(Debug)]
pub enum PropertyKey {
	Ident(Ident),
	Literal(Rc<Lit>)
}

#[derive(Debug)]
pub struct PropertyGetter {
	pub name: Option<Ident>,
	pub block: Block
}

#[derive(Debug)]
pub struct PropertySetter {
	pub name: Option<Ident>,
	pub param: Ident,
	pub block: Block
}

#[derive(Debug)]
pub struct MemberIndex {
	pub expr: Expr,
	pub index: ExprSeq
}

#[derive(Debug)]
pub struct MemberDot {
	pub expr: Expr,
	pub ident: Ident
}

#[derive(Debug)]
pub struct Call {
	pub expr: Expr,
	pub args: Vec<Expr>
}

#[derive(Debug)]
pub struct Ternary {
	pub test: Expr,
	pub then: Expr,
	pub else_: Expr
}
