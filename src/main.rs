#![allow(dead_code)]

#[macro_use]
extern crate rjs;
extern crate rustc_serialize;

use rjs::rt::*;
use std::io::prelude::*;
use std::fs::{File, OpenOptions, read_dir, metadata};
use std::collections::{HashMap, HashSet};
use std::thread;
use rjs::debug;
use std::cmp::{Ordering, min};
use std::ascii::AsciiExt;
use rustc_serialize::json::Json;

fn test() {
	let mut env = JsEnv::new().ok().unwrap();
	
	debug::reset();
	
	let result = env.eval(r#"
var list = [];

for (var i = 0; i < 100000; i++) {
	list.push({
		a: 1,
		b: 2,
		c: 3,
		x: 'abc',
		y: 'def',
		z: 'ghi'
	});
}
	"#);
	
	// print!("{}", debug::reset());
	
	if let Err(error) = result {
		let _scope = env.heap().new_local_scope();
		
		let error = error.as_runtime(&mut env);
		let error = error.as_local(env.heap());
		
		let error = if let Ok(error) = error.to_string(&mut env) {
			error.to_string()
		} else {
			"(cannot convert error to string)".to_string()
		};
		
		panic!("Uncaught {}", error)
	}
}

fn main() {
	test();
	return;

	let mut runner = Runner::new();
	
	walk_dir(&mut runner, "");
}

fn walk_dir(runner: &mut Runner, dir: &str) {
	let mut children = Vec::new();
	let full_dir = format!("tests/tc39/test/{}", dir);
	
	for entry in read_dir(&full_dir).ok().unwrap() {
		children.push(
			entry
				.ok().unwrap()
				.path()
				.as_path()
				.file_name().unwrap()
				.to_str().unwrap()
				.to_string()
		);
	}
	
	children.sort_by(|a, b| {
		fn get_primary_order(name: &str) -> usize {
			match name {
				"language" => 0,
				"built-ins" => 1,
				"intl402" => 2,
				"annexb" => 3,
				"harness" => 4,
				_ => panic!("{}", name)
			}
		}
		
		fn build_parts(string: &str) -> Vec<String> {
			let mut in_digits = false;
			let mut buf = String::new();
			let mut parts = Vec::new();
			
			for c in string.chars() {
				if c.is_digit(10) != in_digits {
					if buf.len() > 0 {
						parts.push(buf);
						buf = String::new();
					}
					
					in_digits = c.is_digit(10);
				}
				
				buf.push(c);
			}
			
			if buf.len() > 0 {
				parts.push(buf);
			}
			
			parts
		}
		
		let a = a.to_ascii_lowercase();
		let b = b.to_ascii_lowercase();
		
		if dir.len() == 0 {
			get_primary_order(&a).cmp(&get_primary_order(&b))
		} else {
			let a_parts = build_parts(&a);
			let b_parts = build_parts(&b);
			
			for i in 0..min(a_parts.len(), b_parts.len()) {
				if let Ok(a_value) = a_parts[i].parse::<usize>() {
					if let Ok(b_value) = b_parts[i].parse::<usize>() {
						match a_value.cmp(&b_value) {
							order @ Ordering::Greater | order @ Ordering::Less => return order,
							_ => {}
						}
					}
				}
				
				match a_parts[i].cmp(&b_parts[i]) {
					order @ Ordering::Greater | order @ Ordering::Less => return order,
					_ => {}
				}
			}
			
			a_parts.len().cmp(&b_parts.len())
		}
	});
	
	for child in children {
		let child_dir = if dir.len() == 0 { child.clone() } else { format!("{}/{}", dir, child) };
		let child_full_dir = format!("{}/{}", full_dir, child);
		
		if metadata(&child_full_dir).ok().unwrap().is_dir() {
			walk_dir(runner, &child_dir);
		} else if child.ends_with(".js") {
			runner.run(child_dir);
		}
	}
}

struct Runner {
	skip: HashSet<String>,
	seen: usize
}

impl Runner {
	fn new() -> Runner {
		// Load the list of tests that succeeded last time.
		
		let mut skip = if let Ok(mut file) = File::open("succeeded") {
			let mut content = String::new();
			file.read_to_string(&mut content).ok().unwrap();
			
			content.split('\n').map(|str| str.to_string()).collect::<HashSet<_>>()
		} else {
			HashSet::new()
		};
		
		let mut file = File::open("test262-ignore.json").ok().unwrap();
		
		match Json::from_reader(&mut file) {
			Ok(json) => {
				if let Json::Object(object) = json {
					for key in object.keys() {
						skip.insert(key.clone());
					}
				} else {
					panic!("invalid test262-ignore.json");
				}
			}
			Err(error) => panic!("{:?}", error)
		}
		
		Runner {
			skip: skip,
			seen: 0
		}
	}
	
	fn run(&mut self, file: String) {
		self.seen += 1;
		
		if self.skip.contains(&file.to_string()) {
			return;
		}
		
		debug::reset();
		
		println!("RUNNING {}", file);
		
		let result = {
			let file = file.clone();
			thread::spawn(move || run_safe(file)).join()
		};
		
		match result {
			Ok(..) => {
				let mut out = OpenOptions::new()
					.create(true)
					.append(true)
					.open("succeeded")
					.ok()
					.unwrap();
				
				write!(out, "{}\n", file).ok().unwrap();
			},
			Err(error) => {
				print!("{}", debug::reset());
				
				let progress = (self.seen as f64 / 13360f64) * 100f64;
				let prefix = format!("[{:.1}%] ", progress);
				
				if let Some(string) = error.downcast_ref::<String>() {
					panic!("{}{}", prefix, string);
				} else if let Some(string) = error.downcast_ref::<&str>() {
					panic!("{}{}", prefix, string);
				} else {
					panic!("{}(unknown error)", prefix);
				}
			}
		}
	}
}

fn run_safe(file: String) {
	let file = format!("tests/tc39/test/{}", file);
	
	let mut js = String::new();
	File::open(&file).ok().unwrap().read_to_string(&mut js).ok();
	
	let header = TestHeader::parse(&js);
	
	let negative = header.headers.get("negative").map(|header|
		match *header {
			Header::String(ref negative) => negative.to_string(),
			_ => panic!("expected negative header to be a string")
		}
	);
	
	let mut is_es6 = header.headers.get("es6id").is_some();
	
	if let Some(header) = header.headers.get("features") {
		if let Header::List(ref items) = *header {
			for item in items {
				match &**item {
					"arrow-function" | "generators" => {
						is_es6 = true;
					}
					_ => {}
				}
			}
		}
	}
	
	if is_es6 {
		return;
	}
	
	let mut includes = vec![
		"sta.js".to_string(),
		"cth.js".to_string(),
		"assert.js".to_string()
	];
	
	if let Some(header) = header.headers.get("includes") {
		if let Header::List(ref items) = *header {
			for item in items {
				includes.push(item.clone());
			}
		}
	}
	
	let mut env = JsEnv::new().ok().unwrap();
	
	for include in &includes {
		debugln!("INCLUDING {}", include);
		env.run(&("tests/tc39/harness/".to_string() + include)).ok().unwrap();
	}
	
	debug::reset();
	
	match env.run(&file) {
		Ok(_) => {
			if let Some(negative) = negative {
				panic!("Expected exception {} from negative test", negative);
			}
		},
		Err(error) => {
			let _scope = env.heap().new_local_scope();
			
			let error = error.as_runtime(&mut env);
			let error = error.as_local(env.heap());
			
			let error = if let Ok(error) = error.to_string(&mut env) {
				let mut error = error.to_string();
				
				if let Some(negative) = negative {
					if negative == "." || error == negative || error.starts_with(&(negative.clone() + ":")) {
						return;
					} else {
						use std::fmt::Write;
						
						write!(error, ", expected exception {}", &negative).ok();
					}
				}
				
				error
			} else {
				"(cannot convert error to string)".to_string()
			};
			
			panic!("{}: Uncaught {}", file, error)
		}
	}
}

struct TestHeader {
	headers: HashMap<String, Header>
}

impl TestHeader {
	fn parse(js: &str) -> TestHeader {
		fn add_header(headers: &mut HashMap<String, Header>, index: String, header: Header) {
			if let Header::String(ref string) = header {
				if string[0..1].eq("[") && string[string.len() - 1..string.len()].eq("]") {
					let mut list = Vec::new();
					for item in string[1..string.len() - 1].split(",") {
						list.push(item.trim().to_string());
					}
					headers.insert(index, Header::List(list));
					return;
				}
			}
			
			headers.insert(index, header);
		}
		
		let mut headers = HashMap::new();
		
		if let Some(start) = js.find("/*---") {
			if let Some(end) = js[start..js.len()].find("---*/") {
				let yaml = js[start + 5..start + end].to_string();
				let mut indent = -1;
				let mut index = None;
				let mut header = None;
				
				for line in yaml.lines().map(|line| line.trim_right()).filter(|line| line.len() > 0) {
					if indent == -1 {
						let chars = line.chars().collect::<Vec<_>>();
						
						for i in 0.. {
							if !chars[i].is_whitespace() {
								indent = i;
								break;
							}
						}
					}
					
					let chars = line[indent..].chars().collect::<Vec<_>>();

					if chars[0].is_whitespace() {
						if let Some(ref mut header) = header {
							match *header {
								Header::String(ref mut string) => {
									if string.len() > 0 {
										string.push('\n');
									}
									string.push_str(line.trim());
								}
								Header::List(ref mut list) => {
									let mut offset = 0;
									while chars[offset].is_whitespace() || chars[offset] == '-' {
										offset += 1;
									}
									list.push(string_from_range(&chars, offset, chars.len()));
								}
							}
						} else {
							panic!();
						}
					} else {
						if header.is_some() {
							add_header(&mut headers, index.unwrap(), header.unwrap());
						}
						
						if chars[chars.len() - 1] == '>' {
							let mut offset = chars.len() - 1;
							while chars[offset] != ':' {
								offset -= 1;
							}
							index = Some(string_from_range(&chars, 0, offset));
							header = Some(Header::String(String::new()));
						} else if chars[chars.len() - 1] != ':' {
							let mut offset = chars.len() - 1;
							while chars[offset] != ':' {
								offset -= 1;
							}
							
							index = None;
							header = None;
							add_header(
								&mut headers,
								string_from_range(&chars, 0, offset),
								Header::String(string_from_range(&chars, offset + 1, chars.len()).trim().to_string())
							);
						} else {
							index = Some(string_from_range(&chars, 0, chars.len() - 1));
							header = Some(Header::List(Vec::new()));
						}
					}
				}
				
				if header.is_some() {
					add_header(&mut headers, index.unwrap(), header.unwrap());
				}
			}
		}
		
		TestHeader {
			headers: headers
		}
	}
}

fn string_from_range(chars: &Vec<char>, start: usize, end: usize) -> String {
	let mut string = String::new();
	for i in start..end {
		string.push(chars[i]);
	}
	string
}

enum Header {
	String(String),
	List(Vec<String>)
}
