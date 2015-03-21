extern crate rjs;

use rjs::interpreter::*;
use rjs::syntax::lexer::Lexer;
use rjs::syntax::parser::Parser;
use rjs::syntax::reader::StringReader;
use rjs::syntax::token::keywords;


#[allow(dead_code)]
fn main() {
    test();
}


fn test() {
	println!("{}", parse("f(1 + 2);"));
	assert!(false);
}

fn parse(js: &str) -> String {
	let mut interner = keywords::new_interner();
	let mut reader = StringReader::new("global.js", js);
	let mut lexer = Lexer::new(&mut reader, &mut interner, false).ok().unwrap();
	let program = {
		let mut parser = Parser::new(&mut lexer, &mut interner);
		parser.parse_program().ok().unwrap()
	};
	let block = build_ir(&program, &mut interner).ok().unwrap();
	
	let mut parsed = String::new();
	block.print(&mut parsed, true, &mut interner);
	parsed
}
