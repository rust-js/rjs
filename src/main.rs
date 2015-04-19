extern crate rjs;

use rjs::interpreter::*;

#[allow(dead_code)]
fn main() {
    test();
}


fn test() {
	parse("f(1 + 2);");
	assert!(false);
}

fn parse(js: &str) {
	let mut ctx = IrContext::new();
	ctx.load_from_str(js).ok().unwrap();
}
