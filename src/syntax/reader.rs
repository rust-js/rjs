pub trait Reader {
    fn is_eof(&self) -> bool;
    
    fn peek(&self) -> char;
    
    fn peek_at(&self, index: usize) -> char;
    
    fn next(&mut self) -> char;
    
    fn consume(&mut self, c: char) -> bool;
    
    fn pos(&self) -> (i32, i32);
    
    fn offset(&self) -> usize;
    
    fn seek(&mut self, offset: usize);
    
    fn last_pos(&self) -> (i32, i32);
    
    fn file(&self) -> &str;
    
    fn skip(&mut self, offset: i32);
}

pub struct StringReader {
    file: String,
    text: Vec<char>,
    offset: usize,
    line: i32,
    col: i32,
    last_line: i32,
    last_col: i32
}

impl StringReader {
    pub fn new<'a>(file: &'a str, text: &'a str) -> StringReader {
        StringReader {
            file: file.to_string(),
            text: text.chars().collect(),
            offset: 0,
            line: 1,
            col: 1,
            last_line: 0,
            last_col: 0
        }
    }
}

impl Reader for StringReader {
    fn is_eof(&self) -> bool {
        self.offset >= self.text.len()
    }
    
    fn peek(&self) -> char {
        self.text[self.offset]
    }
    
    fn peek_at(&self, index: usize) -> char {
        self.text[self.offset + index]
    }
    
    fn next(&mut self) -> char {
        let c = self.peek();
        
        self.offset += 1;
        
        self.last_line = self.line;
        self.last_col = self.col;
        
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        
        c
    }
    
    fn consume(&mut self, c: char) -> bool {
        if self.is_eof() {
            false
        } else if self.peek() == c {
            self.next();
            true
        } else {
            false
        }
    }
    
    fn pos(&self) -> (i32, i32) {
        (self.line, self.col)
    }
    
    fn offset(&self) -> usize {
        self.offset
    }
    
    fn seek(&mut self, offset: usize) {
        self.offset = offset
    }

    fn last_pos(&self) -> (i32, i32) {
        (self.last_line, self.last_col)
    }
    
    fn file(&self) -> &str {
        &self.file
    }
    
    fn skip(&mut self, offset: i32) {
        if offset > 0 {
            self.offset += offset as usize;
        } else {
            self.offset -= -offset as usize;
        }
    }
}
