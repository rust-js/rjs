extern crate strtod;

use ::syntax::lexer::{is_line_terminator, is_whitespace};
use self::strtod::strtod;
use std::f64;

const INFINITY : [char; 8] = ['I', 'n', 'f', 'i', 'n', 'i', 't', 'y'];
const NAN : [char; 3] = ['N', 'a', 'N'];

pub struct DecimalMatcher {
    value: String,
    state: State,
    ty: Type,
    had_any_decimal: bool,
    had_exponent_decimal: bool,
    allow_hex: bool,
    allow_octal: bool
}

#[derive(PartialEq)]
enum State {
    Before,
    HadSign,
    HadDecimal,
    HadExponent,
    HadExponentSign,
    NaN(usize),
    Infinity(usize)
}

#[derive(PartialEq)]
enum Type {
    Unknown,
    Normal,
    SingleZero,
    Hex,
    Octal
}

impl DecimalMatcher {
    pub fn new(allow_hex: bool, allow_octal: bool) -> DecimalMatcher {
        DecimalMatcher {
            value: String::new(),
            state: State::Before,
            ty: Type::Unknown,
            had_any_decimal: false,
            had_exponent_decimal: false,
            allow_hex: allow_hex,
            allow_octal: allow_octal
        }
    }
    
    pub fn from_str(value: &str, allow_hex: bool, allow_octal: bool) -> Decimal {
        let mut matcher = DecimalMatcher::new(allow_hex, allow_octal);
        
        let mut had_one = false;
        
        for c in value.chars() {
            if !had_one && (is_whitespace(c) || is_line_terminator(c)) {
                continue;
            }
            
            had_one = true;
            if !matcher.allowed(c) {
                break;
            }
        }
        
        matcher.complete()
    }
    
    pub fn allowed(&mut self, c: char) -> bool {
        let allowed = match self.state {
            State::Before => {
                match c {
                    c @ '0'...'9' => {
                        self.had_any_decimal = true;
                        self.state = State::HadSign;
                        self.ty = if c == '0' { Type::SingleZero } else { Type::Normal };
                        true
                    }
                    '+' | '-' => {
                        self.state = State::HadSign;
                        true
                    }
                    '.' => {
                        self.state = State::HadDecimal;
                        self.ty = Type::Normal;
                        true
                    }
                    'I' => {
                        self.state = State::Infinity(1);
                        true
                    }
                    'N' => {
                        self.state = State::NaN(1);
                        true
                    }
                    _ => false
                }
            }
            State::HadSign => {
                match c {
                    c @ '0'...'9' => {
                        match self.ty {
                            Type::Unknown => {
                                self.ty = if c == '0' { Type::SingleZero } else { Type::Normal }
                            }
                            Type::SingleZero => {
                                self.ty = if self.allow_octal { Type::Octal } else { Type::Normal };
                            }
                            _ => {}
                        }
                        self.had_any_decimal = true;
                        true
                    }
                    'x' | 'X' if self.allow_hex && self.ty == Type::SingleZero => {
                        self.ty = Type::Hex;
                        self.had_any_decimal = false;
                        true
                    }
                    'a'...'f' | 'A'...'F' if self.ty == Type::Hex => {
                        self.had_any_decimal = true;
                        true
                    }
                    '.' if self.ty == Type::Unknown || self.ty == Type::Normal || self.ty == Type::SingleZero => {
                        self.state = State::HadDecimal;
                        self.ty = Type::Normal;
                        true
                    }
                    'e' | 'E' if self.ty == Type::Normal || self.ty == Type::SingleZero => {
                        self.state = State::HadExponent;
                        self.ty = Type::Normal;
                        true
                    }
                    'I' => {
                        self.state = State::Infinity(1);
                        true
                    }
                    _ => false
                }
            }
            State::Infinity(offset) => {
                if offset < INFINITY.len() && c == INFINITY[offset] {
                    self.state = State::Infinity(offset + 1);
                    true
                } else {
                    false
                }
            }
            State::NaN(offset) => {
                if offset < NAN.len() && c == NAN[offset] {
                    self.state = State::NaN(offset + 1);
                    true
                } else {
                    false
                }
            }
            // We don't move to any of the states below unless type is Normal.
            State::HadDecimal => {
                match c {
                    '0'...'9' => {
                        self.had_any_decimal = true;
                        true
                    }
                    'e' | 'E' if self.had_any_decimal => {
                        self.state = State::HadExponent;
                        true
                    }
                    _ => false
                }
            }
            State::HadExponent => {
                match c {
                    '+' | '-' => {
                        self.state = State::HadExponentSign;
                        true
                    }
                    '0'...'9' => {
                        self.state = State::HadExponentSign;
                        self.had_exponent_decimal = true;
                        true 
                    }
                    _ => false
                }
            }
            State::HadExponentSign => {
                match c {
                    '0'...'9' => {
                        self.had_exponent_decimal = true;
                        true 
                    }
                    _ => false
                }
            }
        };
        
        if allowed {
            self.value.push(c);
        }
        
        allowed
    }
    
    pub fn complete(self) -> Decimal {
        match self.ty {
            Type::SingleZero => Decimal::Integer(self.value),
            Type::Hex => {
                if self.had_any_decimal {
                    Decimal::Hex(self.value)
                } else {
                    Decimal::Error(self.value)
                }
            }
            Type::Octal => Decimal::Octal(self.value),
            _ => {
                match self.state {
                    State::HadSign if self.had_any_decimal
                        => Decimal::Integer(self.value),
                    State::HadDecimal if self.had_any_decimal
                        => Decimal::Decimal(self.value),
                    State::HadExponent | State::HadExponentSign => {
                        if self.had_exponent_decimal {
                            Decimal::Decimal(self.value)
                        } else {
                            let trim = if self.state == State::HadExponentSign { 2 } else { 1 };
                            Decimal::IncompleteDecimal(self.value[0..self.value.len() - trim].to_string())
                        }
                    }
                    _ => {
                        // NaN and Infinity are also resulted as an error.
                        Decimal::Error(self.value)
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum Decimal {
    Error(String),
    IncompleteDecimal(String),
    Integer(String),
    Decimal(String),
    Hex(String),
    Octal(String)
}

impl Decimal {
    pub fn as_f64(&self) -> Option<f64> {
        match *self {
            Decimal::Error(ref value) => match &**value {
                "Infinity" | "+Infinity" => Some(f64::INFINITY),
                "-Infinity" => Some(f64::NEG_INFINITY),
                "NaN" => Some(f64::NAN),
                _ => None
            },
            Decimal::Integer(ref value) => Some(i64::from_str_radix(&value, 10).unwrap() as f64),
            Decimal::Decimal(ref value) | Decimal::IncompleteDecimal(ref value) => strtod(&value),
            Decimal::Hex(ref value) => Some(i64::from_str_radix(&value, 16).unwrap() as f64),
            Decimal::Octal(ref value) => Some(i64::from_str_radix(&value, 8).unwrap() as f64)
        }
    }
}
