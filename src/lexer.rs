use crate::token::*;

#[derive(Debug)]
pub enum LexError {
    LexError,
    ParseIntError(std::num::ParseIntError),
}
impl From<std::num::ParseIntError> for LexError {
    fn from(e: std::num::ParseIntError) -> Self {
        Self::ParseIntError(e)
    }
}

pub struct Lexer {
    input: String,
    pos: usize,
    read_pos: usize,
    ch: u8,
    row: usize,
    col: usize,
}
impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input: input,
            pos: 0,
            read_pos: 0,
            ch: 0,
            row: 1,
            col: 0,
        };
        lexer.read_char();
        lexer
    }
    pub fn next_token(&mut self) -> Token {
        let r = self.row;
        let c = self.col;
        self.skip_whitespace();
        match self.ch {
            b'a'..=b'z' => self.read_keyword(),
            b'+' | b'-' | b'0'..=b'9' => {
                let pos = self.pos;
                let read_pos = self.read_pos;
                match self.read_number() {
                    Ok(token) => token,
                    _ => self.read_reserved(),
                }
            }
            b'"' => self.read_string(),
            b'$' => self.read_id(),
            b'(' => {
                if self.peek_char() == b';' {
                    self.read_block_comment();
                    self.next_token()
                } else {
                    self.read_char();
                    new_token(TokenType::LPAREN, r, c)
                }
            }
            b')' => {
                self.read_char();
                new_token(TokenType::RPAREN, r, c)
            }
            b';' => {
                if self.peek_char() == b';' {
                    self.read_line_comment();
                    self.next_token()
                } else {
                    self.read_reserved()
                }
            }
            0 => new_token(TokenType::EOF, r, c),
            _ => self.read_reserved(),
        }
    }
    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.bytes().nth(self.read_pos).unwrap();
        }
        self.pos = self.read_pos;
        self.read_pos += 1;
        if self.ch == b'\n' {
            self.row += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }
    fn peek_char(&mut self) -> u8 {
        if self.read_pos >= self.input.len() {
            return 0;
        } else {
            return self.input.bytes().nth(self.read_pos).unwrap();
        }
    }
    fn read_keyword(&mut self) -> Token {
        let r = self.row;
        let c = self.col;
        let pos = self.pos;
        while is_idchar(self.ch) {
            self.read_char();
        }
        let s = &self.input[pos..self.pos];
        new_token(lookup(s), r, c)
    }
    fn read_number(&mut self) -> Result<Token, LexError> {
        let r = self.row;
        let c = self.col;
        let mut buf = String::new();
        let mut state = 0;
        loop {
            match self.ch {
                b'+' | b'-' => {
                    state = match state {
                        0 => 1,
                        14 => 31,
                        17 => 32,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b'0' => {
                    state = match state {
                        0 | 1 => 2,
                        2 | 3 | 4 => 3,
                        5 | 6 | 7 => 6,
                        8 | 9 | 10 => 9,
                        11 | 12 | 13 => 12,
                        14 | 15 | 16 | 31 => 15,
                        17 | 18 | 19 | 32 => 18,
                        26 => 27,
                        28 | 29 | 30 => 29,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' => {
                    state = match state {
                        0 | 1 | 2 | 3 | 4 => 3,
                        5 | 6 | 7 => 6,
                        8 | 9 | 10 => 9,
                        11 | 12 | 13 => 12,
                        14 | 15 | 16 | 31 => 15,
                        17 | 18 | 19 | 32 => 18,
                        28 | 29 | 30 => 29,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b'b' | b'c' | b'd' | b'A' | b'B' | b'C' | b'D' | b'F' => {
                    state = match state {
                        8 | 9 | 10 => 9,
                        11 | 12 | 13 => 12,
                        28 | 29 | 30 => 29,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b'e' | b'E' => {
                    state = match state {
                        8 | 9 | 10 => 9,
                        11 | 12 | 13 => 12,
                        3 | 5 | 6 => 14,
                        28 | 29 | 30 => 29,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b'p' | b'P' => {
                    state = match state {
                        9 | 11 | 12 => 17,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b'x' => {
                    state = match state {
                        2 => 8,
                        27 => 28,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b'.' => {
                    state = match state {
                        2 | 3 => 5,
                        9 => 11,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b'_' => {
                    state = match state {
                        2 | 3 => 4,
                        6 => 7,
                        9 => 10,
                        12 => 13,
                        15 => 16,
                        18 => 19,
                        29 => 30,
                        _ => return Err(LexError::LexError),
                    };
                }
                b'i' => {
                    state = match state {
                        0 | 1 => 20,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b'n' => {
                    state = match state {
                        0 | 1 => 23,
                        20 => 21,
                        24 => 25,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b'f' => {
                    state = match state {
                        8 | 9 | 10 => 9,
                        11 | 12 | 13 => 12,
                        21 => 22, //info
                        28 | 29 | 30 => 29,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b'a' => {
                    state = match state {
                        8 | 9 | 10 => 9,
                        11 | 12 | 13 => 12,
                        23 => 24, //nan
                        28 | 29 | 30 => 29,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                b':' => {
                    state = match state {
                        25 => 26,
                        _ => return Err(LexError::LexError),
                    };
                    buf.push(self.ch.into())
                }
                _ => match state {
                    2 | 3 | 5 | 6 | 9 | 11 | 12 | 15 | 18 | 22 | 25 | 29 => {
                        return Ok(new_token(TokenType::NUM(buf[..].to_string()), r, c))
                    }
                    _ => return Err(LexError::LexError),
                },
            }
            self.read_char();
        }
    }
    fn read_string(&mut self) -> Token {
        let mut buf: Vec<u8> = vec![];
        let position = self.pos + 1;
        let mut escaped = false;
        let r = self.row;
        let c = self.col;
        loop {
            self.read_char();
            if escaped {
                let c: char = self.ch.into();
                if c.is_ascii_hexdigit() {
                    self.read_char();
                    let s = &self.input[self.pos - 1..self.read_pos];
                    buf.push(u8::from_str_radix(s, 16).unwrap());
                    escaped = false;
                } else {
                    match c {
                        't' => {
                            buf.push(0x09_u8);
                        }
                        'n' => {
                            buf.push(0x0A_u8);
                        }
                        'r' => {
                            buf.push(0x0D_u8);
                        }
                        '"' => {
                            buf.push(0x22_u8);
                        }
                        '\'' => {
                            buf.push(0x27_u8);
                        }
                        '\\' => {
                            buf.push(0x5c_u8);
                        }
                        'u' => match self.read_hexnum() {
                            Ok(n) => {
                                if n < 0xD800 || (0xE000 <= n && n < 0x110000) {
                                    let bytes = (n as u32).to_le_bytes();
                                    buf.push(bytes[3]);
                                    buf.push(bytes[2]);
                                    buf.push(bytes[1]);
                                    buf.push(bytes[0]);
                                } else {
                                    buf.push(self.input.as_bytes()[position]);
                                    buf.push(self.input.as_bytes()[position + 1]);
                                }
                            }
                            _ => {
                                buf.push(self.input.as_bytes()[position]);
                                buf.push(self.input.as_bytes()[position + 1]);
                            }
                        },
                        _ => {
                            buf.push(self.input.as_bytes()[position]);
                            buf.push(self.input.as_bytes()[position + 1]);
                        }
                    }
                    escaped = false;
                }
            } else {
                if self.ch == b'\\' {
                    escaped = true;
                } else if self.ch >= 0x20 && self.ch != 0x7F && self.ch != b'"' {
                    buf.push(self.ch);
                } else {
                    self.read_char();
                    break;
                }
            }
        }
        new_token(TokenType::STRING(buf), r, c)
    }
    fn read_id(&mut self) -> Token {
        let r = self.row;
        let c = self.col;
        let pos = self.pos;
        assert_eq!(self.ch, b'$');
        self.read_char();
        while is_idchar(self.ch) {
            self.read_char();
        }
        new_token(TokenType::ID(self.input[pos..self.pos].to_string()), r, c)
    }
    fn read_reserved(&mut self) -> Token {
        let pos = self.pos;
        let r = self.row;
        let c = self.col;
        loop {
            if !is_idchar(self.ch) {
                break;
            }
            self.read_char();
        }
        new_token(
            TokenType::RESERVED(self.input[pos..self.pos].to_string()),
            r,
            c,
        )
    }
    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                b' ' | 0x9 | 0xA | 0xD => self.read_char(),
                _ => break,
            }
        }
    }

    fn read_hexnum(&mut self) -> Result<usize, LexError> {
        let mut ret = self.read_hexdigit()?;
        loop {
            let c: char = self.peek_char().into();
            if c == '_' {
                self.read_char();
            } else if c.is_ascii_hexdigit() {
                self.read_char();
                let mut s = String::new();
                s.push(c);
                let hexdigit = usize::from_str_radix(&s[..], 16)?;
                ret = ret * 16 + hexdigit;
            } else {
                return Ok(ret);
            }
        }
    }
    fn read_hexdigit(&mut self) -> Result<usize, LexError> {
        let c: char = self.ch.into();
        if c.is_ascii_hexdigit() {
            self.read_char();
            let mut s = String::new();
            s.push(c);
            Ok(usize::from_str_radix(&s[..], 16)?)
        } else {
            Err(LexError::LexError)
        }
    }
    fn read_line_comment(&mut self) -> &str {
        self.read_char();
        let position = self.pos;
        loop {
            if self.pos > self.input.len() {
                break;
            }
            self.read_char();
            if self.ch == b'\n' {
                self.read_char();
                break;
            }
        }
        &self.input[position..self.pos]
    }
    fn read_block_comment(&mut self) -> &str {
        let position = self.pos;
        self.read_char();
        let mut sum: usize = 1;
        loop {
            self.read_char();
            if self.ch == b';' && self.peek_char() == b')' {
                self.read_char();
                sum -= 1;
                if sum == 0 {
                    break;
                }
            } else if self.ch == b'(' && self.peek_char() == b';' {
                self.read_char();
                sum += 1
            }
        }
        self.read_char();
        &self.input[position..self.pos]
    }
}

fn is_idchar(ch: u8) -> bool {
    match ch {
        b'0'..=b'9'
        | b'A'..=b'Z'
        | b'a'..=b'z'
        | b'!'
        | b'#'
        | b'$'
        | b'%'
        | b'&'
        | b'\''
        | b'*'
        | b'+'
        | b'-'
        | b'.'
        | b'/'
        | b':'
        | b'<'
        | b'='
        | b'>'
        | b'?'
        | b'@'
        | b'\\'
        | b'^'
        | b'_'
        | b'`'
        | b'|'
        | b'~' => true,
        _ => false,
    }
}
pub fn new_token(token_type: TokenType, row: usize, col: usize) -> Token {
    Token {
        r#type: token_type,
        row: row,
        col: col,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::*;
    use std::io::Read;
    // #[test]
    fn lex_test() {
        if let Ok(entries) = read_dir("testsuite") {
            for entry in entries {
                if let Ok(entry) = entry {
                    let filename = entry.file_name().into_string().unwrap();
                    if filename.ends_with(".wast") {
                        let mut f = std::fs::File::open(format!("testsuite/{}", filename)).unwrap();
                        let mut v = Vec::new();
                        f.read_to_end(&mut v).unwrap();
                        let input = String::from_utf8(v).unwrap();
                        let mut lexer = Lexer::new(input);
                        loop {
                            let token = lexer.next_token();
                            if token.r#type == TokenType::EOF {
                                break;
                            }
                            println!("{:#?}", token);
                        }
                    }
                }
            }
        }
    }
}
