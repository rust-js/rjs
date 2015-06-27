pub struct DecimalMatcher {
    value: String,
    state: State,
    ty: Type,
    had_any_decimal: bool,
    had_exponent_decimal: bool,
    allow_octal: bool
}

#[derive(Debug)]
pub enum Decimal {
    Error(String),
    Integer(String),
    Decimal(String),
    Hex(String),
    Octal(String)
}

enum State {
    Before,
    HadSign,
    HadDecimal,
    HadExponent,
    HadExponentSign
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
    pub fn new(allow_octal: bool) -> DecimalMatcher {
        DecimalMatcher {
            value: String::new(),
            state: State::Before,
            ty: Type::Unknown,
            had_any_decimal: false,
            had_exponent_decimal: false,
            allow_octal: allow_octal
        }
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
                    State::HadExponent | State::HadExponentSign if self.had_exponent_decimal
                        => Decimal::Decimal(self.value),
                    _ => Decimal::Error(self.value)
                }
            }
        }
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
                    'x' | 'X' if self.ty == Type::SingleZero => {
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
                    _ => false
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
}
