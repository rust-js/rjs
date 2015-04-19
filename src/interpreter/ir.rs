use syntax::ast::Name;
use util::interner::StrInterner;
use std::fmt::Write;

pub struct Block {
	pub ir: Vec<Ir>,
	pub locals: Vec<Option<Name>>,
	pub labels: Vec<IrOffset>
}

impl Block {
	fn print_local(&self, string: &mut String, local: Local, interner: &StrInterner) {
		let Local(index) = local;
		
		write!(string, "{}", index).ok();
		
		let Local(index) = local;
		if let Some(name) = self.locals[index] {
			string.push(' ');
			self.print_name(string, name, interner);
		}
	}
	
	fn print_name(&self, string: &mut String, name: Name, interner: &StrInterner) {
		self.print_string(string, name.as_str(interner))
	}
	
	fn print_string(&self, string: &mut String, value: &str) {
		string.push('\'');
		
		for c in value.chars() {
			match c {
				'\r' => string.push_str("\\r"),
				'\n' => string.push_str("\\n"),
				'\t' => string.push_str("\\t"),
				'\\' => string.push_str("\\\\"),
				'\'' => string.push_str("\\'"),
				c @ _ => string.push(c)
			};
		}
		
		string.push('\'');
	}
	
	fn print_label(&self, string: &mut String, label: Label) {
		let Label(index) = label;
		let IrOffset(offset) = self.labels[index];
		
		write!(string, "L{:04}", offset).ok();
	}
	
	pub fn print(&self, string: &mut String, indent: bool, interner: &StrInterner) {
		for i in 0..self.locals.len() {
			if indent {
				string.push_str("    ");
			}
			
			write!(string, ".local [{}]", i).ok();
			
			if let Some(name) = self.locals[i] {
				string.push_str(name.as_str(interner));
			}
			
			string.push('\n');
		}
		
		for i in 0..self.ir.len() {
			if indent {
				string.push_str("    ");
			}
			
			write!(string, "L{:04}: ", i).ok();
			
			match &self.ir[i] {
				&Ir::Add => string.push_str("add"),
				&Ir::And => string.push_str("and"),
				&Ir::BitAnd => string.push_str("bit.and"),
				&Ir::BitNot => string.push_str("bit.not"),
				&Ir::BitOr => string.push_str("bit.or"),
				&Ir::BitXOr => string.push_str("bit.xor"),
				&Ir::Call(args) => { write!(string, "call {}", args).ok(); },
				&Ir::CurrentIter(local) => {
					string.push_str("iter.cur ");
					self.print_local(string, local, interner);
				},
				&Ir::Debugger => string.push_str("debug"),
				&Ir::Delete => string.push_str("delete"),
				&Ir::Divide => string.push_str("div"),
				&Ir::Dup => string.push_str("dup"),
				&Ir::EndIter(local) => {
					string.push_str("iter.end ");
					self.print_local(string, local, interner);
				},
				&Ir::EnterWith => string.push_str("with.enter"),
				&Ir::Eq => string.push_str("eq"),
				&Ir::Ge => string.push_str("ge"),
				&Ir::Gt => string.push_str("gt"),
				&Ir::In => string.push_str("in"),
				&Ir::InstanceOf => string.push_str("instof"),
				&Ir::IntoIter(local) => {
					string.push_str("iter.into ");
					self.print_local(string, local, interner);
				},
				&Ir::Jump(label) => {
					string.push_str("jmp ");
					self.print_label(string, label);
				},
				&Ir::JumpEq(label) => {
					string.push_str("jmp.eq ");
					self.print_label(string, label);
				},
				&Ir::JumpFalse(label) => {
					string.push_str("jmp.f ");
					self.print_label(string, label);
				},
				&Ir::JumpTrue(label) => {
					string.push_str("jmp.t ");
					self.print_label(string, label);
				},
				&Ir::Le => string.push_str("le"),
				&Ir::Leave(label) => {
					string.push_str("leave ");
					self.print_label(string, label);
				},
				&Ir::LeaveWith => string.push_str("with.leave"),
				&Ir::LoadArguments => string.push_str("ld.arguments"),
				&Ir::LoadException => string.push_str("ld.exception"),
				&Ir::LoadF64(value) => { write!(string, "ld.f64 {}", value).ok(); },
				&Ir::LoadFalse => string.push_str("ld.false"),
				&Ir::LoadFunction(index) => { write!(string, "ld.func {}", index).ok(); },
				&Ir::LoadGlobal(name) => {
					string.push_str("ld.global ");
					self.print_name(string, name, interner);
				},
				&Ir::LoadI32(value) => { write!(string, "ld.i32 {}", value).ok(); },
				&Ir::LoadI64(value) => { write!(string, "ld.i64 {}", value).ok(); },
				&Ir::LoadIndex => string.push_str("ld.idx"),
				&Ir::LoadLifted(name, depth) => {
					string.push_str("ld.lifted ");
					self.print_name(string, name, interner);
					write!(string, ", {}", depth).ok();
				},
				&Ir::LoadLocal(local) => {
					string.push_str("ld.local ");
					self.print_local(string, local, interner);
				},
				&Ir::LoadMissing => string.push_str("ld.missing"),
				&Ir::LoadName(name) => {
					string.push_str("ld.name ");
					self.print_name(string, name, interner);
				},
				&Ir::LoadNameLit => string.push_str("ld.name.lit"),
				&Ir::LoadNull => string.push_str("ld.null"),
				&Ir::LoadParam(index) => { write!(string, "ld.arg {}", index).ok(); },
				&Ir::LoadRegex(ref body, ref flags) => {
					string.push_str("ld.regex ");
					self.print_string(string, &body);
					string.push_str(", ");
					self.print_string(string, &flags);
				},
				&Ir::LoadString(ref value) => {
					string.push_str("ld.str ");
					self.print_string(string, &value);
				},
				&Ir::LoadThis => string.push_str("ld.this"),
				&Ir::LoadTrue => string.push_str("ld.true"),
				&Ir::LoadUndefined => string.push_str("ld.undefined"),
				&Ir::Lsh => string.push_str("lsh"),
				&Ir::Lt => string.push_str("lt"),
				&Ir::Modulus => string.push_str("mod"),
				&Ir::Multiply => string.push_str("mul"),
				&Ir::Ne => string.push_str("ne"),
				&Ir::Negative => string.push_str("neg"),
				&Ir::New(args) => { write!(string, "new {}", args).ok(); },
				&Ir::NewArray => string.push_str("new.array"),
				&Ir::NewObject => string.push_str("new.object"),
				&Ir::NextIter(local, label) => {
					string.push_str("iter.next ");
					self.print_local(string, local, interner);
					string.push_str(", ");
					self.print_label(string, label);
				},
				&Ir::Not => string.push_str("not"),
				&Ir::Or => string.push_str("or"),
				&Ir::Pick(offset) => { write!(string, "pick {}", offset).ok(); },
				&Ir::Pop => string.push_str("pop"),
				&Ir::Positive => string.push_str("pos"),
				&Ir::PushArray => string.push_str("array.push"),
				&Ir::Return => string.push_str("ret"),
				&Ir::Rsh => string.push_str("rsh"),
				&Ir::RshZeroFill => string.push_str("rsh.zf"),
				&Ir::StoreArguments => string.push_str("st.arguments"),
				&Ir::StoreGetter(function) => { write!(string, "st.getter {}", function).ok(); },
				&Ir::StoreNameGetter(name, function) => { write!(string, "st.getter.name {}, {}", name, function).ok(); },
				&Ir::StoreGlobal(name) => {
					string.push_str("st.global ");
					self.print_name(string, name, interner);
				},
				&Ir::StoreIndex => string.push_str("st.index"),
				&Ir::StoreLifted(name, depth) => {
					string.push_str("st.lifted ");
					self.print_name(string, name, interner);
					write!(string, ", {}", depth).ok();
				},
				&Ir::StoreLocal(local) => {
					string.push_str("st.local ");
					self.print_local(string, local, interner);
				},
				&Ir::StoreName(name) => {
					string.push_str("st.name ");
					self.print_name(string, name, interner);
				},
				&Ir::StoreNameLit => string.push_str("st.name.lit"),
				&Ir::StoreParam(index) => { write!(string, "st.arg {}", index).ok(); },
				&Ir::StoreSetter(function) => { write!(string, "st.setter {}", function).ok(); },
				&Ir::StoreNameSetter(name, function) => { write!(string, "st.setter.name {}, {}", name, function).ok(); },
				&Ir::StrictEq => string.push_str("eq.strict"),
				&Ir::StrictNe => string.push_str("ne.strict"),
				&Ir::Subtract => string.push_str("sub"),
				&Ir::Swap => string.push_str("swap"),
				&Ir::Throw => string.push_str("throw"),
				&Ir::Typeof => string.push_str("typeof")
			}
			
			string.push('\n');
		}
	}
}

pub struct Function {
	pub name: Option<Name>,
	pub args: Vec<Name>,
	pub block: Block
}

#[derive(Copy, Clone)]
pub struct IrOffset(usize);

#[derive(Copy, Clone)]
pub struct IrRange(IrOffset, IrOffset);

#[derive(Copy, Clone)]
pub struct Local(usize);

#[derive(Copy, Clone)]
pub struct Label(usize);

pub struct TryCatch {
	try: IrRange,
	catch: Option<IrRange>,
	finally: Option<IrRange>
}

impl TryCatch {
	fn finalize_try(&mut self, offset: usize) {
		let IrRange(start, _) = self.try;
		self.try = IrRange(start, IrOffset(offset));
	}
	
	fn finalize_catch(&mut self, offset: usize) {
		if let Some(IrRange(start, _)) = self.catch {
			self.catch = Some(IrRange(start, IrOffset(offset)));
		} else {
			panic!();
		}
	}
	
	fn finalize_finally(&mut self, offset: usize) {
		if let Some(IrRange(start, _)) = self.finally {
			self.finally = Some(IrRange(start, IrOffset(offset)));
		} else {
			panic!();
		}
	}
}

pub struct IrBuilder {
	ir: Vec<Ir>,
	locals: Vec<Option<Name>>,
	labels: Vec<IrOffset>,
	try_catch_stack: Vec<TryCatch>,
	try_catches: Vec<TryCatch>
}

impl IrBuilder {
	pub fn new() -> IrBuilder {
		IrBuilder {
			ir: Vec::new(),
			locals: Vec::new(),
			labels: Vec::new(),
			try_catch_stack: Vec::new(),
			try_catches: Vec::new()
		}
	}
	
	pub fn local(&mut self, name: Option<Name>) -> Local {
		let local = Local(self.locals.len());
		
		self.locals.push(name);
		
		local
	}
	
	pub fn mark(&mut self, label: Label) {
		let Label(index) = label;
		
		self.labels[index] = IrOffset(self.ir.len());
	}
	
	pub fn label(&mut self) -> Label {
		let label = Label(self.labels.len());
		
		self.labels.push(IrOffset(0));
		
		label
	}
	
	pub fn emit(&mut self, ir: Ir) {
		self.ir.push(ir);
	}
	
	pub fn build(self) -> Block {
		Block {
			ir: self.ir,
			locals: self.locals,
			labels: self.labels
		}
	}
	
	pub fn start_exception_block(&mut self) {
		let try_catch = TryCatch {
			try: IrRange(IrOffset(self.ir.len()), IrOffset(0)),
			catch: None,
			finally: None
		};
		
		self.try_catch_stack.push(try_catch);
	}
	
	pub fn end_exception_block(&mut self) {
		let mut try_catch = self.try_catch_stack.pop().unwrap();
		
		if try_catch.catch.is_some() {
			try_catch.finalize_catch(self.ir.len());
		} else if try_catch.finally.is_some() {
			try_catch.finalize_finally(self.ir.len());
		} else {
			panic!("Try block requires at least a catch or finally");
		}
		
		self.try_catches.push(try_catch);
	}
	
	pub fn start_catch(&mut self) {
		let top = self.try_catch_stack.len() - 1;
		let try_catch = &mut self.try_catch_stack[top];
		
		if try_catch.catch.is_some() {
			panic!("Exception block cannot contain multiple catch blocks");
		}
		
		try_catch.finalize_try(self.ir.len());
		
		try_catch.catch = Some(IrRange(IrOffset(self.ir.len()), IrOffset(0)));
	}
	
	pub fn start_finally(&mut self) {
		let top = self.try_catch_stack.len() - 1;
		let try_catch = &mut self.try_catch_stack[top];
		
		if try_catch.finally.is_some() {
			panic!("Exception block cannot contain multiple finally blocks");
		}
		
		if try_catch.catch.is_some() {
			try_catch.finalize_catch(self.ir.len());
		} else {
			try_catch.finalize_try(self.ir.len());
		}
		
		try_catch.finally = Some(IrRange(IrOffset(self.ir.len()), IrOffset(0)));
	}
}

pub enum Ir {
	Add,
	And,
	BitAnd,
	BitNot,
	BitOr,
	BitXOr,
	Call(u32),
	CurrentIter(Local),
	Debugger,
	Delete,
	Divide,
	Dup,
	EndIter(Local),
	EnterWith,
	Eq,
	Ge,
	Gt,
	In,
	InstanceOf,
	IntoIter(Local),
	Jump(Label),
	JumpEq(Label),
	JumpFalse(Label),
	JumpTrue(Label),
	Le,
	Leave(Label),
	LeaveWith,
	LoadArguments,
	LoadException,
	LoadF64(f64),
	LoadFalse,
	LoadFunction(u32),
	LoadGlobal(Name),
	LoadI32(i32),
	LoadI64(i64),
	LoadIndex,
	LoadLifted(Name, u32),
	LoadLocal(Local),
	LoadMissing,
	LoadName(Name),
	LoadNameLit,
	LoadNull,
	LoadParam(u32),
	LoadRegex(String, String),
	LoadString(String),
	LoadThis,
	LoadTrue,
	LoadUndefined,
	Lsh,
	Lt,
	Modulus,
	Multiply,
	Ne,
	Negative,
	New(u32),
	NewArray,
	NewObject,
	NextIter(Local, Label),
	Not,
	Or,
	Pick(u32),
	Pop,
	Positive,
	PushArray,
	Return,
	Rsh,
	RshZeroFill,
	StoreArguments,
	StoreGlobal(Name),
	StoreIndex,
	StoreLifted(Name, u32),
	StoreLocal(Local),
	StoreName(Name),
	StoreNameLit,
	StoreGetter(u32),
	StoreNameGetter(u32, u32),
	StoreSetter(u32),
	StoreNameSetter(u32, u32),
	StoreParam(u32),
	StrictEq,
	StrictNe,
	Subtract,
	Swap,
	Throw,
	Typeof
}
