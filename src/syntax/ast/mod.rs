pub mod visitor;

use syntax::{Name, Span};
use syntax::token::{Lit, name};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt;

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

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FunctionRef(pub u32);

impl FunctionRef {
    pub fn usize(&self) -> usize {
        self.0 as usize
    }
}

pub struct Function {
    pub global: bool,
    pub name: Option<Name>,
    pub args: u32,
    pub block: RootBlock,
    pub span: Span
}

/// A local slot is a declaration of a local that is known within the current
/// scope (function). This only applies to functions and when optimizations are
/// enabled. When optimizations are disabled (i.e. when there is an eval
/// somewhere in the stack), local slots are not tracked.
#[derive(Copy, Clone, Debug)]
pub struct Slot {
    pub name: Name,
    pub arguments: bool,
    pub arg: Option<u32>,
    pub state: SlotState
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SlotState {
    Local,
    Scoped,
    Lifted(u32)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct SlotRef(pub usize);

impl SlotRef {
    pub fn usize(&self) -> usize {
        self.0
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FunctionSlotRef(pub FunctionRef, pub SlotRef, pub u32);

impl FunctionSlotRef {
    pub fn function(&self) -> FunctionRef {
        self.0
    }
    
    pub fn slot(&self) -> SlotRef {
        self.1
    }
    
    pub fn depth(&self) -> u32 {
        self.2
    }
}

#[derive(Debug)]
pub struct RootBlock {
    pub args: Vec<Name>,
    pub block: Block,
    pub state: RefCell<RootBlockState>,
    pub strict: bool
}

#[derive(Debug)]
pub struct RootBlockState {
    pub slots: Vec<Slot>,
    pub take_scope: bool,
    pub build_scope: ScopeType,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum ScopeType {
    None,
    Thin(u32),
    Thick
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Item>,
    pub locals: HashMap<Name, SlotRef>
}

#[derive(Debug)]
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
    Leave(Box<Item>),
    Return(Option<ExprSeq>),
    Switch(Option<Label>, ExprSeq, Vec<SwitchClause>),
    Throw(ExprSeq),
    Try(Block, Option<Catch>, Option<Block>),
    VarDecl(Vec<Var>),
    While(Option<Label>, Box<Expr>, Box<Item>),
    With(ExprSeq, Box<Item>)
}

#[derive(Debug)]
pub struct Label {
    pub name: Name
}

pub struct Ident {
    pub name: Name,
    pub arguments: bool,
    pub state: Cell<IdentState>
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ident {{ name: {:?} }}", self.name)
    }
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
    Global(bool),
    Scoped,
    Slot(SlotRef),
    LiftedSlot(FunctionSlotRef),
    ScopedArg(u32, u32),
    Arg(SlotRef, u32),
    LiftedArg(FunctionSlotRef, u32),
    LoadFunction(FunctionRef)
}

impl IdentState {
    pub fn is_none(&self) -> bool {
        if let IdentState::None = *self { true } else { false }
    }
}

#[derive(Debug)]
pub struct Var {
    pub ident: Ident,
    pub expr: Option<Box<Expr>>
}

#[derive(Debug)]
pub struct ExprSeq {
    pub exprs: Vec<Expr>
}

#[derive(Debug)]
pub enum SwitchClause {
    Case(ExprSeq, Vec<Item>),
    Default(Vec<Item>)
}

#[derive(Debug)]
pub struct Catch {
    pub ident: Ident,
    pub block: Block
}

#[derive(Debug)]
pub enum Expr {
    ArrayLiteral(Vec<Expr>),
    Assign(Op, Box<Expr>, Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Function(FunctionRef),
    Ident(Ident),
    Literal(Lit),
    MemberDot(Box<Expr>, Name),
    MemberIndex(Box<Expr>, ExprSeq),
    Missing,
    New(Box<Expr>),
    ObjectLiteral(Vec<Property>),
    Paren(ExprSeq),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    This,
    Unary(Op, Box<Expr>),
    Cast(CastType, Box<Expr>)
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CastType {
    Primitive,
    StringPrimitive,
    NumberPrimitive,
    Boolean,
    Number,
    Integer,
    Int32,
    UInt32,
    UInt16,
    String,
    Object
}

impl CastType {
    pub fn from_name(name: Name) -> Option<CastType> {
        match name {
            name::PRIM => Some(CastType::Primitive),
            name::SPRIM => Some(CastType::StringPrimitive),
            name::NPRIM => Some(CastType::NumberPrimitive),
            name::BOOL => Some(CastType::Boolean),
            name::NUMBER => Some(CastType::Number),
            name::INT => Some(CastType::Integer),
            name::I32 => Some(CastType::Int32),
            name::U32 => Some(CastType::UInt32),
            name::U16 => Some(CastType::UInt16),
            name::STRING => Some(CastType::String),
            name::OBJECT => Some(CastType::Object),
            _ => None
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
    Literal(Lit)
}
