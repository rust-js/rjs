#![feature(test)]

#[macro_use]
extern crate lazy_static;

pub use rt::{JsResult, JsError};

#[macro_use]
mod debug;
#[macro_use]
mod trace;
mod syntax;
mod ir;
mod util;
pub mod gc;
pub mod rt;
mod errors;
pub mod contrib;
