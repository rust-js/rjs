#[macro_export]
macro_rules! trace {
    ($fmt:expr) => ( if cfg!(feature = "trace") { println!(concat!("[TRACE] ", $fmt)); });
    ($fmt:expr, $($arg:tt)*) => (if cfg!(feature = "trace") { println!(concat!("[TRACE] ", $fmt), $($arg)*); });
}

macro_rules! trace_type {
    ($prefix:expr, $fmt:expr) => (println!(concat!("[TRACE ", $prefix, "] ", $fmt)));
    ($prefix:expr, $fmt:expr, $($arg:tt)*) => (println!(concat!("[TRACE ", $prefix, "] ", $fmt), $($arg)*));
}

#[macro_export]
macro_rules! tracegc {
    ($fmt:expr) => ( if cfg!(feature = "tracegc") { trace_type!("GC", $fmt); });
    ($fmt:expr, $($arg:tt)*) => ( if cfg!(feature = "tracegc") { trace_type!("GC", $fmt, $($arg)*); });
}
