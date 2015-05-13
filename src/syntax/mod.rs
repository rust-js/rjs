use util::interner::StrInterner;
use util::interner::RcStr;
use std::fmt;
use std::ops::Deref;
use std::i32;

pub mod reader;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod ast;

#[derive(Clone)]
pub struct Span {
	pub start_line: i32,
	pub start_col: i32,
	pub end_line: i32,
	pub end_col: i32
}

impl Span {
	pub fn new(start_line: i32, start_col: i32, end_line: i32, end_col: i32) -> Span {
		Span {
			start_line: start_line,
			start_col: start_col,
			end_line: end_line,
			end_col: end_col
		}
	}
}

#[derive(Eq, Ord, PartialEq, PartialOrd, Hash, Clone, Copy, Debug)]
pub struct Name(i32);

impl Name {
	pub fn from_name(name: usize) -> Name {
		assert!(name < i32::MAX as usize);
		
		Name(-(name as i32 + 1))
	}
	
	pub fn from_index(index: u32) -> Name {
		assert!(index <= i32::MAX as u32);
		
		Name(index as i32)
	}
	
	pub fn as_str<'a>(&'a self, interner: &StrInterner) -> &'a str {
		unsafe {
			// FIXME #12938: can't use copy_lifetime since &str isn't a &T
			::std::mem::transmute::<&str,&str>(&InternedString::new_from_rc_str(interner.get(*self)))
		}
	}
	
	pub fn is_index(&self) -> bool {
		self.0 >= 0
	}
	
	pub fn index(&self) -> Option<u32> {
		if self.is_index() {
			Some(self.0 as u32)
		} else {
			None
		}
	}
	
	pub fn is_name(&self) -> bool {
		!self.is_index()
	}
	
	pub fn name(&self) -> Option<usize> {
		if self.is_name() {
			Some((-self.0 - 1) as usize)
		} else {
			None
		}
	}
	
	pub fn value(&self) -> i32 {
		self.0
	}
}

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
