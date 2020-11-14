use crate::lexer::*;
use crate::token::*;

#[derive(Debug)]
pub enum Error {
    ParseError(Token),
    UnexpectedToken { expected: TokenType, real: Token },
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
    FromUtf8Error(std::string::FromUtf8Error),
    Malformed(String),
}
impl From<std::num::ParseIntError> for Error {
    fn from(e: std::num::ParseIntError) -> Self {
        Self::ParseIntError(e)
    }
}
impl From<std::num::ParseFloatError> for Error {
    fn from(e: std::num::ParseFloatError) -> Self {
        Self::ParseFloatError(e)
    }
}
impl From<std::string::FromUtf8Error> for Error {
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
    pub fn new(input: String) -> Result<Self, Error> {
        let lexer = Lexer::new(input);
        let mut c = Self {
            lexer: lexer,
            cur_token: new_token(TokenType::ILLEGAL, 0, 0),
            peek_token: new_token(TokenType::ILLEGAL, 0, 0),
            cur_new_id: 0,
        };
        c.next_token()?;
        c.next_token()?;
        Ok(c)
    }
    pub fn next_token(&mut self) -> Result<(), Error> {
        if self.cur_token.r#type == TokenType::EOF {
            return Err(Error::Malformed(String::from("unexpected end")));
        }
        self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
        // println!("self.cur_token={:#?}", self.cur_token);
        Ok(())
    }
    pub fn cur_token_is(&self, t: &TokenType) -> bool {
        &self.cur_token.r#type == t
    }
    pub fn expect_cur(&mut self, t: &TokenType) -> Result<(), Error> {
        if self.cur_token_is(t) {
            self.next_token()?;
            Ok(())
        } else {
            Err(Error::UnexpectedToken {
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
    fn parse(c: &mut ParseContext) -> Result<T, Error>;
}
