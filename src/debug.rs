use std::cell::RefCell;
use std::sync::Mutex;

static mut DEBUG_ENABLED : bool = false;

lazy_static! {
    static ref DEBUG_OUT : Mutex<RefCell<String>> = Mutex::new(RefCell::new(String::new()));
}

macro_rules! debugln {
    ($fmt:expr) => (debug!(concat!($fmt, "\n")));
    ($fmt:expr, $($arg:tt)*) => (debug!(concat!($fmt, "\n"), $($arg)*));
}

macro_rules! debug {
    ($fmt:expr) => ( if cfg!(not(ndebug)) { $crate::debug::debug_write($fmt); });
    ($fmt:expr, $($arg:tt)*) => ( if cfg!(not(ndebug)) { $crate::debug::debug_write(&format!($fmt, $($arg)*)); })
}

pub fn debug_write(string: &str) {
    if !unsafe { DEBUG_ENABLED } {
        return;
    }
    
    let guard = DEBUG_OUT.lock();
    let lock = guard.unwrap();
    lock.borrow_mut().push_str(string);
}

pub fn debug_enable(enabled: bool) {
    unsafe { DEBUG_ENABLED = enabled };
}

pub fn reset() -> String {
    let guard = DEBUG_OUT.lock();
    let lock = guard.unwrap();
    
    let mut string = lock.borrow_mut();
    let result = string.clone();
    *string = String::new();
    
    result
}

