#[derive(PartialEq, Clone, Debug)]
pub enum TokenType {
    ILLEGAL, // unknown character
    EOF,     // end of file
    KEYWORD(String),
    NUM(String),
    STRING(Vec<u8>),
    ID(String),
    LPAREN,
    RPAREN,
    RESERVED(String),

    //keywords
    I32,
    I64,
    F32,
    F64,
    FUNC,
    PARAM,
    RESULT,
    FUNCREF,
    MUT,
    BLOCK,
    LOOP,
    IF,
    THEN,
    ELSE,
    END,
    TYPE,
    IMPORT,
    TABLE,
    MEMORY,
    GLOBAL,
    EXPORT,
    START,
    ELEM,
    DATA,
    LOCAL,
    OFFSET,
    MODULE,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub r#type: TokenType,
    pub row: usize,
    pub col: usize,
}

pub fn lookup(s: &str) -> TokenType {
    if s.starts_with("nan:0x") {
        return TokenType::NUM(s.to_string());
    }
    match s {
        "inf" | "nan" => TokenType::NUM(s.to_string()),

        //keywords
        "i32" => TokenType::I32,
        "i64" => TokenType::I64,
        "f32" => TokenType::F32,
        "f64" => TokenType::F64,
        "func" => TokenType::FUNC,
        "param" => TokenType::PARAM,
        "result" => TokenType::RESULT,
        "funcref" => TokenType::FUNCREF,
        "mut" => TokenType::MUT,
        "block" => TokenType::BLOCK,
        "loop" => TokenType::LOOP,
        "if" => TokenType::IF,
        "then" => TokenType::THEN,
        "else" => TokenType::ELSE,
        "end" => TokenType::END,
        "type" => TokenType::TYPE,
        "import" => TokenType::IMPORT,
        "table" => TokenType::TABLE,
        "memory" => TokenType::MEMORY,
        "global" => TokenType::GLOBAL,
        "export" => TokenType::EXPORT,
        "start" => TokenType::START,
        "elem" => TokenType::ELEM,
        "data" => TokenType::DATA,
        "local" => TokenType::LOCAL,
        "offset" => TokenType::OFFSET,
        "module" => TokenType::MODULE,
        _ => TokenType::KEYWORD(s.to_string()),
    }
}
