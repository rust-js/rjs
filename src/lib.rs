#[macro_use]
extern crate lazy_static;

pub use rt::result::{JsResult, JsError};

#[macro_use]
pub mod debug;
#[macro_use]
pub mod trace;
pub mod syntax;
pub mod ir;
pub mod util;
pub mod gc;
pub mod rt;
mod errors;
