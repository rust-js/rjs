// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! An "interner" is a data structure that associates values with usize tags and
//! allows bidirectional lookup; i.e. given a value, one can easily find the
//! type, and vice versa.

use syntax::Name;

use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;
use std::str::FromStr;
use std::u32;

#[derive(Clone, PartialEq, Hash, PartialOrd)]
pub struct RcStr {
    string: Rc<String>,
}

impl RcStr {
    pub fn new(string: &str) -> RcStr {
        RcStr {
            string: Rc::new(string.to_string()),
        }
    }
}

impl Eq for RcStr {}

impl Ord for RcStr {
    fn cmp(&self, other: &RcStr) -> Ordering {
        self[..].cmp(&other[..])
    }
}

impl fmt::Debug for RcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Debug;
        self[..].fmt(f)
    }
}

impl fmt::Display for RcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Display;
        self[..].fmt(f)
    }
}

impl Borrow<str> for RcStr {
    fn borrow(&self) -> &str {
        &self.string[..]
    }
}

impl Deref for RcStr {
    type Target = str;

    fn deref(&self) -> &str { &self.string[..] }
}

/// A StrInterner differs from Interner<String> in that it accepts
/// &str rather than RcStr, resulting in less allocation.
pub struct StrInterner {
    map: RefCell<HashMap<RcStr, Name>>,
    vect: RefCell<Vec<RcStr> >,
}

/// When traits can extend traits, we should extend index<Name,T> to get []
impl StrInterner {
    pub fn new() -> StrInterner {
        StrInterner {
            map: RefCell::new(HashMap::new()),
            vect: RefCell::new(Vec::new()),
        }
    }
    
    fn parse_index(&self, val: &str) -> Option<usize> {
        // TODO #77: Improve. We shouldn't have to create a string again. Instead we
        // should parse the val to verify that it will be equal to the index.
        if let Ok(index) = u32::from_str(val) {
            if val == index.to_string() && index < u32::MAX {
                return Some(index as usize);
            }
        }
        
        None
    }

    pub fn intern(&self, val: &str) -> Name {
        if let Some(index) = self.parse_index(val) {
            Name::from_index(index)
        } else {
            let mut map = self.map.borrow_mut();
            match map.get(val) {
                Some(&idx) => return idx,
                None => (),
            }
    
            let new_idx = Name::from_name(self.len());
            let val = RcStr::new(val);
            map.insert(val.clone(), new_idx);
            self.vect.borrow_mut().push(val);
            new_idx
        }
    }

    pub fn get(&self, idx: Name) -> RcStr {
        if let Some(index) = idx.index() {
            RcStr {
                string: Rc::new(index.to_string())
            }
        } else {
            (*self.vect.borrow())[idx.name().unwrap()].clone()
        }
    }

    pub fn len(&self) -> usize {
        self.vect.borrow().len()
    }
}
