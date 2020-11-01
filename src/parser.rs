use crate::lexer::*;
use crate::token::*;

#[macro_export]
macro_rules! process_instr {
    ($c:ident, $e:expr) => {{
        $c.next_token();
        Ok($e)
    }};
}

#[derive(Debug)]
pub enum ParseError {
    ParseError(Token),
    UnexpectedToken { expected: TokenType, real: Token },
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
    FromUtf8Error(std::string::FromUtf8Error),
}
impl From<std::num::ParseIntError> for ParseError {
    fn from(e: std::num::ParseIntError) -> Self {
        Self::ParseIntError(e)
    }
}
impl From<std::num::ParseFloatError> for ParseError {
    fn from(e: std::num::ParseFloatError) -> Self {
        Self::ParseFloatError(e)
    }
}
impl From<std::string::FromUtf8Error> for ParseError {
    fn from(e: std::string::FromUtf8Error) -> Self {
        Self::FromUtf8Error(e)
    }
}

pub struct ParseContext {
    lexer: Lexer,
    pub cur_token: Token,
    pub peek_token: Token,
    pub cur_new_id: usize,
}
impl ParseContext {
    pub fn new(input: String) -> Self {
        let lexer = Lexer::new(input);
        let mut c = Self {
            lexer: lexer,
            cur_token: new_token(TokenType::ILLEGAL, 0, 0),
            peek_token: new_token(TokenType::ILLEGAL, 0, 0),
            cur_new_id: 0,
        };
        c.next_token();
        c.next_token();
        c
    }
    pub fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
        // println!("self.cur_token={:#?}", self.cur_token);
    }
    pub fn cur_token_is(&self, t: &TokenType) -> bool {
        &self.cur_token.r#type == t
    }
    pub fn expect_cur(&mut self, t: &TokenType) -> Result<(), ParseError> {
        if self.cur_token_is(t) {
            self.next_token();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: t.clone(),
                real: self.cur_token.clone(),
            })
        }
    }
    pub fn peek_token_is(&self, t: &TokenType) -> bool {
        &self.peek_token.r#type == t
    }
}
pub trait Parse<T> {
    fn parse(c: &mut ParseContext) -> Result<T, ParseError>;
}
