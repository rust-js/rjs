#![allow(dead_code)]

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
