use syntax::Name;
use syntax::ast::FunctionRef;
use util::interner::StrInterner;
use std::fmt::Write;
use std::cmp::Ordering;

pub struct Block {
	pub ir: Vec<Ir>,
	pub locals: Vec<Option<Name>>,
	pub try_catches: Vec<TryCatch>
}

impl Block {
	fn print_local(&self, string: &mut String, local: Local, interner: &StrInterner) {
		write!(string, "{}", local.0).ok();
		
		if let Some(name) = self.locals[local.0] {
			string.push(' ');
			self.print_name(string, name, interner);
		}
	}
	
	fn print_name(&self, string: &mut String, name: Name, interner: &StrInterner) {
		self.print_string(string, name.as_str(interner));
		write!(string, " ({})", name.value()).ok();
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
		write!(string, "L{:04}", label.offset()).ok();
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
		
		for i in 0..self.try_catches.len() {
			if indent {
				string.push_str("    ");
			}
			
			let try_catch = &self.try_catches[i];
			
			write!(string, ".try [{}] L{:04}..L{:04}", i, try_catch.try.start().offset(), try_catch.try.end().offset()).ok();
			if let Some(catch) = try_catch.catch {
				write!(string, " .catch L{:04}..L{:04}", catch.start().offset(), catch.end().offset()).ok();
			}
			if let Some(finally) = try_catch.finally {
				write!(string, " .finally L{:04}..L{:04}", finally.start().offset(), finally.end().offset()).ok();
			}
			
			string.push('\n');
		}
		
		for i in 0..self.ir.len() {
			if indent {
				string.push_str("    ");
			}
			
			write!(string, "L{:04}: ", i).ok();
			
			match self.ir[i] {
				Ir::Add => string.push_str("add"),
				Ir::BitAnd => string.push_str("bit.and"),
				Ir::BitNot => string.push_str("bit.not"),
				Ir::BitOr => string.push_str("bit.or"),
				Ir::BitXOr => string.push_str("bit.xor"),
				Ir::Call(args) => { write!(string, "call {}", args).ok(); }
				Ir::CallEval(args) => { write!(string, "call.eval {}", args).ok(); } 
				Ir::CallThis(args) => { write!(string, "call.this {}", args).ok(); }
				Ir::CurrentIter(local) => {
					string.push_str("iter.cur ");
					self.print_local(string, local, interner);
				}
				Ir::Debugger => string.push_str("debug"),
				Ir::Delete => string.push_str("delete"),
				Ir::DeleteName(name) => {
					string.push_str("delete.name ");
					self.print_name(string, name, interner);
				}
				Ir::DeleteEnvName(name) => {
					string.push_str("delete.name.env ");
					self.print_name(string, name, interner);
				}
				Ir::DeleteIndex => string.push_str("delete.index"),
				Ir::Divide => string.push_str("div"),
				Ir::Dup => string.push_str("dup"),
				Ir::EndFinally => string.push_str("finally.end"),
				Ir::EndIter(local) => {
					string.push_str("iter.end ");
					self.print_local(string, local, interner);
				}
				Ir::EnterEnv => string.push_str("env.enter"),
				Ir::EnterWithEnv => string.push_str("env.enter.with"),
				Ir::Eq => string.push_str("eq"),
				Ir::FindEnvObjectFor(name) => {
					string.push_str("find.env.obj.for ");
					self.print_name(string, name, interner);
				}
				Ir::Ge => string.push_str("ge"),
				Ir::Gt => string.push_str("gt"),
				Ir::In => string.push_str("in"),
				Ir::InitEnvName(name) => {
					string.push_str("init.name.env ");
					self.print_name(string, name, interner);
				}
				Ir::InstanceOf => string.push_str("instof"),
				Ir::IntoIter(local) => {
					string.push_str("iter.into ");
					self.print_local(string, local, interner);
				}
				Ir::Jump(label) => {
					string.push_str("jmp ");
					self.print_label(string, label);
				}
				Ir::JumpStrictEq(label) => {
					string.push_str("jmp.eq ");
					self.print_label(string, label);
				}
				Ir::JumpFalse(label) => {
					string.push_str("jmp.f ");
					self.print_label(string, label);
				}
				Ir::JumpTrue(label) => {
					string.push_str("jmp.t ");
					self.print_label(string, label);
				}
				Ir::Le => string.push_str("le"),
				Ir::Leave(label) => {
					string.push_str("leave ");
					self.print_label(string, label);
				}
				Ir::LeaveEnv => string.push_str("env.leave"),
				Ir::LoadArguments => string.push_str("ld.arguments"),
				Ir::LoadEnv(name) => {
					string.push_str("ld.env ");
					self.print_name(string, name, interner);
				}
				Ir::LoadEnvArguments(depth) => { write!(string, "ld.env.args {}", depth).ok(); },
				Ir::LoadEnvObject => string.push_str("ld.env.obj"),
				Ir::LoadEnvObjectFor(name) => {
					string.push_str("ld.env.obj.for ");
					self.print_name(string, name, interner);
				}
				Ir::LoadGlobal(name) => {
					string.push_str("ld.global ");
					self.print_name(string, name, interner);
				}
				Ir::LoadGlobalObject => string.push_str("ld.global.obj"),
				Ir::LoadException => string.push_str("ld.exception"),
				Ir::LoadF64(value) => { write!(string, "ld.f64 {}", value).ok(); }
				Ir::LoadFalse => string.push_str("ld.false"),
				Ir::LoadFunction(index) => { write!(string, "ld.func {}", index.usize()).ok(); }
				Ir::LoadI32(value) => { write!(string, "ld.i32 {}", value).ok(); }
				Ir::LoadI64(value) => { write!(string, "ld.i64 {}", value).ok(); }
				Ir::LoadIndex => string.push_str("ld.idx"),
				Ir::LoadLifted(index, depth) => {
					string.push_str("ld.lifted ");
					write!(string, "{} {}", index, depth).ok();
				}
				Ir::LoadLocal(local) => {
					string.push_str("ld.local ");
					self.print_local(string, local, interner);
				}
				Ir::LoadMissing => string.push_str("ld.missing"),
				Ir::LoadName(name) => {
					string.push_str("ld.name ");
					self.print_name(string, name, interner);
				}
				Ir::LoadNameLit => string.push_str("ld.name.lit"),
				Ir::LoadNull => string.push_str("ld.null"),
				Ir::LoadParam(index) => { write!(string, "ld.arg {}", index).ok(); }
				Ir::LoadRegex(body, flags) => {
					string.push_str("ld.regex ");
					self.print_string(string, &*interner.get(body));
					string.push_str(", ");
					self.print_string(string, &*interner.get(flags));
				}
				Ir::LoadString(value) => {
					string.push_str("ld.str ");
					self.print_string(string, &*interner.get(value));
				}
				Ir::LoadThis => string.push_str("ld.this"),
				Ir::LoadTrue => string.push_str("ld.true"),
				Ir::LoadUndefined => string.push_str("ld.undefined"),
				Ir::Lsh => string.push_str("lsh"),
				Ir::Lt => string.push_str("lt"),
				Ir::Modulus => string.push_str("mod"),
				Ir::Multiply => string.push_str("mul"),
				Ir::Ne => string.push_str("ne"),
				Ir::Negative => string.push_str("neg"),
				Ir::New(args) => { write!(string, "new {}", args).ok(); }
				Ir::NewArguments => string.push_str("new.args"),
				Ir::NewArray => string.push_str("new.array"),
				Ir::NewObject => string.push_str("new.object"),
				Ir::NextIter(local, label) => {
					string.push_str("iter.next ");
					self.print_local(string, local, interner);
					string.push_str(", ");
					self.print_label(string, label);
				}
				Ir::Not => string.push_str("not"),
				Ir::Pick(offset) => { write!(string, "pick {}", offset).ok(); }
				Ir::Pop => string.push_str("pop"),
				Ir::Positive => string.push_str("pos"),
				Ir::Return => string.push_str("ret"),
				Ir::Rsh => string.push_str("rsh"),
				Ir::RshZeroFill => string.push_str("rsh.zf"),
				Ir::StoreGetterUnchecked(function) => { write!(string, "st.this.getter {}", function.usize()).ok(); }
				Ir::StoreGlobal(name) => {
					string.push_str("st.global ");
					self.print_name(string, name, interner);
				}
				Ir::StoreNameGetterUnchecked(name, function) => {
					string.push_str("st.this.getter.name ");
					self.print_name(string, name, interner);
					write!(string, ", {}", function.usize()).ok();
				}
				Ir::StoreIndex => string.push_str("st.idx"),
				Ir::StoreIndexUnchecked => string.push_str("st.this.idx"),
				Ir::StoreLifted(index, depth) => {
					string.push_str("st.lifted ");
					write!(string, "{} {}", index, depth).ok();
				}
				Ir::StoreLocal(local) => {
					string.push_str("st.local ");
					self.print_local(string, local, interner);
				}
				Ir::StoreName(name) => {
					string.push_str("st.name ");
					self.print_name(string, name, interner);
				}
				Ir::StoreNameUnchecked(name) => {
					string.push_str("st.this.name ");
					self.print_name(string, name, interner);
				}
				Ir::StoreParam(index) => { write!(string, "st.arg {}", index).ok(); }
				Ir::StoreEnv(name) => {
					string.push_str("st.env ");
					self.print_name(string, name, interner);
				}
				Ir::StoreEnvArguments => string.push_str("st.env.args"),
				Ir::StoreSetterUnchecked(function) => { write!(string, "st.this.setter {}", function.usize()).ok(); }
				Ir::StoreNameSetterUnchecked(name, function) => {
					string.push_str("st.this.setter.name ");
					self.print_name(string, name, interner);
					write!(string, ", {}", function.usize()).ok();
				}
				Ir::StrictEq => string.push_str("eq.strict"),
				Ir::StrictNe => string.push_str("ne.strict"),
				Ir::Subtract => string.push_str("sub"),
				Ir::Swap => string.push_str("swap"),
				Ir::Throw => string.push_str("throw"),
				Ir::ToBoolean => string.push_str("cast.bool"),
				Ir::ToNumber => string.push_str("cast.number"),
				Ir::ToPropertyKey => string.push_str("cast.key"),
				Ir::Typeof => string.push_str("typeof"),
				Ir::TypeofIndex => string.push_str("typeof.index"),
				Ir::TypeofName(name) => {
					string.push_str("typeof.name ");
					self.print_name(string, name, interner);
				}
				Ir::ValidateMemberTarget => string.push_str("member.target")
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

impl IrOffset {
	pub fn offset(&self) -> usize {
		self.0
	}
}

#[derive(Copy, Clone)]
pub struct IrRange(IrOffset, IrOffset);

impl IrRange {
	pub fn start(&self) -> IrOffset {
		self.0
	}
	
	pub fn end(&self) -> IrOffset {
		self.1
	}
	
	pub fn contains(&self, offset: usize) -> bool {
		offset >= (self.0).0 && offset < (self.1).0
	}
}

#[derive(Copy, Clone)]
pub struct Local(usize);

impl Local {
	pub fn offset(&self) -> usize {
		self.0
	}
}

#[derive(Copy, Clone)]
pub struct Label(usize);

impl Label {
	pub fn offset(&self) -> usize {
		self.0
	}
}

pub struct TryCatch {
	pub try: IrRange,
	pub catch: Option<IrRange>,
	pub finally: Option<IrRange>
}

impl TryCatch {
	fn finalize_try(&mut self, offset: usize) {
		self.try = IrRange(self.try.0, IrOffset(offset));
	}
	
	fn finalize_catch(&mut self, offset: usize) {
		self.catch = Some(IrRange(self.catch.unwrap().0, IrOffset(offset)));
	}
	
	fn finalize_finally(&mut self, offset: usize) {
		self.finally = Some(IrRange(self.finally.unwrap().0, IrOffset(offset)));
	}
	
	pub fn contains(&self, offset: usize) -> bool {
		if offset < (self.try.0).0 {
			false
		} else {
			let end = if let Some(finally) = self.finally {
				(finally.0).0
			} else if let Some(catch) = self.catch {
				(catch.0).0
			} else {
				panic!("Expected at least a catch or finally");
			};
			
			offset < end
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
		self.labels[label.0] = IrOffset(self.ir.len());
	}
	
	pub fn label(&mut self) -> Label {
		let label = Label(self.labels.len());
		
		self.labels.push(IrOffset(0));
		
		label
	}
	
	pub fn emit(&mut self, ir: Ir) {
		self.ir.push(ir);
	}
	
	pub fn build(mut self) -> Block {
		for ir in &mut self.ir {
			ir.fixup_label(&self.labels);
		}
		
		if self.try_catch_stack.len() > 0 {
			panic!("There are unclosed try/catch blocks");
		}
		
		// Ensure the sort order of the try/catch blocks. Try/catches need
		// to be sorted in reverse order for the execution engine to map
		// them correctly to IR instructions.
		
		self.try_catches.sort_by(|x, y| {
			let x_start = x.try.start().offset();
			let y_start = y.try.start().offset();
			
			if x_start > y_start {
				Ordering::Less
			} else if x_start < y_start {
				Ordering::Greater
			} else {
				let x_end = x.try.end().offset();
				let y_end = y.try.end().offset();
				
				if x_end < y_end {
					Ordering::Less
				} else if x_end > y_end {
					Ordering::Greater
				} else {
					panic!("did not expect two try blocks with same start and end");
				}
			}
		});
		
		// Try cach blocks are stored in reverse because that's the order
		// in which they need to be matched.
		
		Block {
			ir: self.ir,
			locals: self.locals,
			try_catches: self.try_catches
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
		
		if try_catch.finally.is_some() {
			try_catch.finalize_finally(self.ir.len());
		} else if try_catch.catch.is_some() {
			try_catch.finalize_catch(self.ir.len());
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
	BitAnd,
	BitNot,
	BitOr,
	BitXOr,
	Call(u32),
	CurrentIter(Local),
	Debugger,
	Delete,
	DeleteIndex,
	DeleteName(Name),
	DeleteEnvName(Name),
	Divide,
	Dup,
	EndFinally,
	EndIter(Local),
	EnterEnv,
	EnterWithEnv,
	Eq,
	FindEnvObjectFor(Name),
	Ge,
	Gt,
	In,
	InitEnvName(Name),
	InstanceOf,
	IntoIter(Local),
	Jump(Label),
	JumpStrictEq(Label),
	JumpFalse(Label),
	JumpTrue(Label),
	Le,
	Leave(Label),
	LeaveEnv,
	LoadArguments,
	LoadException,
	LoadF64(f64),
	LoadFalse,
	LoadFunction(FunctionRef),
	LoadI32(i32),
	LoadI64(i64),
	LoadIndex,
	LoadLifted(u32, u32),
	LoadLocal(Local),
	LoadMissing,
	LoadName(Name),
	LoadNameLit,
	LoadNull,
	LoadParam(u32),
	LoadRegex(Name, Name),
	LoadEnv(Name),
	LoadEnvObject,
	LoadEnvObjectFor(Name),
	LoadGlobal(Name),
	LoadGlobalObject,
	LoadEnvArguments(u32),
	LoadString(Name),
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
	NewArguments,
	NewArray,
	NewObject,
	NextIter(Local, Label),
	Not,
	Pick(u32),
	Pop,
	Positive,
	Return,
	Rsh,
	RshZeroFill,
	CallEval(u32),
	CallThis(u32),
	StoreGetterUnchecked(FunctionRef),
	StoreGlobal(Name),
	StoreIndex,
	StoreIndexUnchecked,
	StoreLifted(u32, u32),
	StoreLocal(Local),
	StoreName(Name),
	StoreNameUnchecked(Name),
	StoreNameGetterUnchecked(Name, FunctionRef),
	StoreNameSetterUnchecked(Name, FunctionRef),
	StoreParam(u32),
	StoreEnv(Name),
	StoreEnvArguments,
	StoreSetterUnchecked(FunctionRef),
	StrictEq,
	StrictNe,
	Subtract,
	Swap,
	Throw,
	ToBoolean,
	ToNumber,
	ToPropertyKey,
	Typeof,
	TypeofIndex,
	TypeofName(Name),
	ValidateMemberTarget
}

impl Ir {
	fn fixup_label(&mut self, labels: &[IrOffset]) {
		match self {
			&mut Ir::Jump(ref mut target) |
			&mut Ir::JumpStrictEq(ref mut target) |
			&mut Ir::JumpFalse(ref mut target) |
			&mut Ir::JumpTrue(ref mut target) |
			&mut Ir::Leave(ref mut target) |
			&mut Ir::NextIter(_, ref mut target)
				=> target.0 = labels[target.0].0,
			_ => {}
		}
	}
}