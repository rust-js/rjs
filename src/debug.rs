use std::cell::RefCell;
use std::sync::Mutex;

lazy_static! {
	static ref DEBUG_OUT : Mutex<RefCell<String>> = Mutex::new(RefCell::new(String::new()));
}

#[macro_export]
macro_rules! debugln {
    ($fmt:expr) => (debug!(concat!($fmt, "\n")));
    ($fmt:expr, $($arg:tt)*) => (debug!(concat!($fmt, "\n"), $($arg)*));
}

#[macro_export]
macro_rules! debug {
    ($fmt:expr) => ( if cfg!(not(ndebug)) { $crate::debug::debug_write($fmt); });
    ($fmt:expr, $($arg:tt)*) => ( if cfg!(not(ndebug)) { $crate::debug::debug_write(&format!($fmt, $($arg)*)); })
}

pub fn debug_write(string: &str) {
	let guard = DEBUG_OUT.lock();
	let lock = guard.unwrap();
	lock.borrow_mut().push_str(string);
}

pub fn reset() -> String {
	let guard = DEBUG_OUT.lock();
	let lock = guard.unwrap();
	
	let mut string = lock.borrow_mut();
	let result = string.clone();
	*string = String::new();
	
	result
}

pub fn get_debug() -> String {
	let guard = DEBUG_OUT.lock();
	let lock = guard.unwrap();
	
	let string = lock.borrow();
	string.clone()
}
