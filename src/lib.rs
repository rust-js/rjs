pub mod syntax;
pub mod ir;
pub mod util;
#[macro_use]
pub mod gc;
pub mod rt;

pub enum JsError {
	Io(std::io::Error),
	Lex(String),
	Parse(String),
	Message(String),
	Type

}

pub type JsResult<T> = Result<T, JsError>;