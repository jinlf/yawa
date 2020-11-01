use crate::module::*;
use crate::parser::*;
use crate::token::*;

#[derive(Debug)]
pub struct Script {
    cmds: Vec<Cmd>,
}
#[derive(Debug)]
pub enum Cmd {
    extmodule(ExtModule),
    register { name: Name, id: Id },
    action(Action),
    assertion(Assertion),
    meta(Meta),
}
#[derive(Debug)]
pub struct ExtModule {
    pub id: Id,
    pub modulefields: Vec<ExtModuleField>,
}
#[derive(Debug)]
pub enum ExtModuleField {
    field(ModuleField),
    binary(Vec<Vec<u8>>),
    quote(Vec<Vec<u8>>),
}
#[derive(Debug)]
pub enum Action {
    invoke {
        id: Id,
        name: Name,
        exprs: Vec<Expr>,
    },
    get {
        id: Id,
        name: Name,
    },
}
#[derive(Debug)]
pub enum Assertion {
    r#return {
        action: Action,
        results: Vec<AssertionResult>,
    },
    action_trap {
        action: Action,
        failure: String,
    },
    exhaustion {
        action: Action,
        failure: String,
    },
    malformed {
        module: ExtModule,
        failure: String,
    },
    invalid {
        module: ExtModule,
        failure: String,
    },
    unlinkable {
        module: ExtModule,
        failure: String,
    },
    module_trap {
        module: ExtModule,
        failure: String,
    },
}
#[derive(Debug)]
pub enum AssertionResult {
    i32_const(NumPat),
    i64_const(NumPat),
    f32_const(NumPat),
    f64_const(NumPat),
}
#[derive(Debug)]
pub enum NumPat {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    nan_canonical,
    nan_arithmetic,
}
#[derive(Debug)]
pub enum Meta {
    script { id: Id, script: Script },
    input { id: Id, filename: Name },
    output { id: Id, filename: Option<Name> },
}

pub struct ScriptParser {}
impl ScriptParser {
    pub fn parse_script(c: &mut ParseContext) -> Result<Script, ParseError> {
        let mut cmds: Vec<Cmd> = vec![];
        loop {
            if !c.cur_token_is(&TokenType::LPAREN) {
                break;
            }
            cmds.push(Self::parse_cmd(c)?);
        }
        Ok(Script { cmds: cmds })
    }
    pub fn parse_cmd(c: &mut ParseContext) -> Result<Cmd, ParseError> {
        match &c.peek_token.r#type {
            TokenType::KEYWORD(keyword) => match &keyword[..] {
                "register" => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::KEYWORD("register".to_string()))?;
                    let name = ModuleParser::parse_name(c)?;
                    let id = ModuleParser::parse_id(c);
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(Cmd::register { name: name, id: id })
                }
                "invoke" | "get" => Ok(Cmd::action(Self::parse_action(c)?)),
                "assert_return" | "assert_trap" | "assert_exhaustion" | "assert_malformed"
                | "assert_invalid" | "assert_unlinkable" => {
                    Ok(Cmd::assertion(Self::parse_assertion(c)?))
                }
                "script" | "input" | "output" => Ok(Cmd::meta(Self::parse_meta(c)?)),
                _ => Err(ParseError::ParseError(c.cur_token.clone())),
            },
            _ => Ok(Cmd::extmodule(Self::parse_extmodule(c)?)),
        }
    }
    pub fn parse_extmodule(c: &mut ParseContext) -> Result<ExtModule, ParseError> {
        let has_module =
            if c.cur_token_is(&TokenType::LPAREN) && c.peek_token_is(&TokenType::MODULE) {
                c.expect_cur(&TokenType::LPAREN)?;
                c.expect_cur(&TokenType::MODULE)?;
                true
            } else {
                false
            };
        let id = ModuleParser::parse_id(c);
        let mut modulefields: Vec<ExtModuleField> = vec![];
        loop {
            if c.cur_token_is(&TokenType::RPAREN) || c.cur_token_is(&TokenType::EOF) {
                break;
            }
            let mut fields = Self::parse_extmodulefield(c)?;
            while fields.len() != 0 {
                modulefields.push(fields.remove(0));
            }
        }
        if has_module {
            c.expect_cur(&TokenType::RPAREN)?;
        }
        Ok(ExtModule {
            id: id,
            modulefields: modulefields,
        })
    }
    pub fn parse_extmodulefield(c: &mut ParseContext) -> Result<Vec<ExtModuleField>, ParseError> {
        if c.cur_token_is(&TokenType::KEYWORD("binary".to_string())) {
            c.expect_cur(&TokenType::KEYWORD("binary".to_string()))?;
            Ok(vec![ExtModuleField::binary(
                ModuleParser::parse_vec_string(c)?,
            )])
        } else if c.cur_token_is(&TokenType::KEYWORD("quote".to_string())) {
            c.expect_cur(&TokenType::KEYWORD("quote".to_string()))?;
            Ok(vec![ExtModuleField::quote(ModuleParser::parse_vec_string(
                c,
            )?)])
        } else {
            let mut modulefields: Vec<ExtModuleField> = vec![];
            let mut fields = ModuleParser::parse_modulefield(c)?;
            while fields.len() != 0 {
                modulefields.push(ExtModuleField::field(fields.remove(0)));
            }
            Ok(modulefields)
        }
    }
    pub fn parse_action(c: &mut ParseContext) -> Result<Action, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => match &keyword[..] {
                "invoke" => {
                    c.expect_cur(&TokenType::KEYWORD("invoke".to_string()))?;
                    let id = ModuleParser::parse_id(c);
                    let name = ModuleParser::parse_name(c)?;
                    let exprs = Self::parse_vec_expr(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(Action::invoke {
                        id: id,
                        name: name,
                        exprs: exprs,
                    })
                }
                "get" => {
                    c.expect_cur(&TokenType::KEYWORD("get".to_string()))?;
                    let id = ModuleParser::parse_id(c);
                    let name = ModuleParser::parse_name(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(Action::get { id: id, name: name })
                }
                _ => Err(ParseError::ParseError(c.cur_token.clone())),
            },
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    fn parse_vec_expr(c: &mut ParseContext) -> Result<Vec<Expr>, ParseError> {
        let mut exprs: Vec<Expr> = vec![];
        loop {
            if c.cur_token_is(&TokenType::RPAREN) {
                break;
            }
            exprs.push(ModuleParser::parse_expr(c)?);
        }
        Ok(exprs)
    }
    pub fn parse_assertion(c: &mut ParseContext) -> Result<Assertion, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => match &keyword[..] {
                "assert_return" => {
                    c.expect_cur(&TokenType::KEYWORD("assert_return".to_string()))?;
                    let action = Self::parse_action(c)?;
                    let results = Self::parse_vec_result(c)?;
                    Ok(Assertion::r#return {
                        action: action,
                        results: results,
                    })
                }
                "assert_trap" => {
                    c.expect_cur(&TokenType::KEYWORD("assert_trap".to_string()))?;
                    if c.peek_token_is(&TokenType::KEYWORD("invoke".to_string()))
                        || c.peek_token_is(&TokenType::KEYWORD("get".to_string()))
                    {
                        let action = Self::parse_action(c)?;
                        let failure = ModuleParser::parse_string(c)?;
                        Ok(Assertion::action_trap {
                            action: action,
                            failure: failure,
                        })
                    } else {
                        let module = Self::parse_extmodule(c)?;
                        let failure = ModuleParser::parse_string(c)?;
                        Ok(Assertion::module_trap {
                            module: module,
                            failure: failure,
                        })
                    }
                }
                "assert_exhaustion" => {
                    c.expect_cur(&TokenType::KEYWORD("assert_exhaustion".to_string()))?;
                    let action = Self::parse_action(c)?;
                    let failure = ModuleParser::parse_string(c)?;
                    Ok(Assertion::exhaustion {
                        action: action,
                        failure: failure,
                    })
                }
                "assert_malformed" => {
                    c.expect_cur(&TokenType::KEYWORD("assert_malformed".to_string()))?;
                    let module = Self::parse_extmodule(c)?;
                    let failure = ModuleParser::parse_string(c)?;
                    Ok(Assertion::malformed {
                        module: module,
                        failure: failure,
                    })
                }
                "assert_invalid" => {
                    c.expect_cur(&TokenType::KEYWORD("assert_invalid".to_string()))?;
                    let module = Self::parse_extmodule(c)?;
                    let failure = ModuleParser::parse_string(c)?;
                    Ok(Assertion::invalid {
                        module: module,
                        failure: failure,
                    })
                }
                "assert_unlinkable" => {
                    c.expect_cur(&TokenType::KEYWORD("assert_unlinkable".to_string()))?;
                    let module = Self::parse_extmodule(c)?;
                    let failure = ModuleParser::parse_string(c)?;
                    Ok(Assertion::unlinkable {
                        module: module,
                        failure: failure,
                    })
                }
                _ => Err(ParseError::ParseError(c.cur_token.clone())),
            },
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    fn parse_vec_result(c: &mut ParseContext) -> Result<Vec<AssertionResult>, ParseError> {
        let mut results: Vec<AssertionResult> = vec![];
        loop {
            match &c.peek_token.r#type {
                TokenType::KEYWORD(keyword) => match &keyword[..] {
                    "i32.const" => {
                        c.expect_cur(&TokenType::LPAREN)?;
                        c.expect_cur(&TokenType::KEYWORD("i32.const".to_string()))?;
                        results.push(AssertionResult::i32_const(Self::parse_numpat(
                            c,
                            ValType::I32,
                        )?));
                        c.expect_cur(&TokenType::RPAREN)?;
                    }
                    "i64.const" => {
                        c.expect_cur(&TokenType::LPAREN)?;
                        c.expect_cur(&TokenType::KEYWORD("i64.const".to_string()))?;
                        results.push(AssertionResult::i64_const(Self::parse_numpat(
                            c,
                            ValType::I64,
                        )?));
                        c.expect_cur(&TokenType::RPAREN)?;
                    }
                    "f32.const" => {
                        c.expect_cur(&TokenType::LPAREN)?;
                        c.expect_cur(&TokenType::KEYWORD("f32.const".to_string()))?;
                        results.push(AssertionResult::f32_const(Self::parse_numpat(
                            c,
                            ValType::F32,
                        )?));
                        c.expect_cur(&TokenType::RPAREN)?;
                    }
                    "f64.const" => {
                        c.expect_cur(&TokenType::LPAREN)?;
                        c.expect_cur(&TokenType::KEYWORD("f64.const".to_string()))?;
                        results.push(AssertionResult::f64_const(Self::parse_numpat(
                            c,
                            ValType::F64,
                        )?));
                        c.expect_cur(&TokenType::RPAREN)?;
                    }
                    _ => return Err(ParseError::ParseError(c.cur_token.clone())),
                },
                _ => return Ok(results),
            }
        }
    }
    pub fn parse_numpat(c: &mut ParseContext, valtype: ValType) -> Result<NumPat, ParseError> {
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => match &keyword[..] {
                "nan:canonical" => Ok(NumPat::nan_canonical),
                "nan:arithmetic" => Ok(NumPat::nan_arithmetic),
                _ => Err(ParseError::ParseError(c.cur_token.clone())),
            },
            _ => match valtype {
                ValType::I32 => Ok(NumPat::I32(ModuleParser::parse_i32(c)?)),
                ValType::I64 => Ok(NumPat::I64(ModuleParser::parse_i64(c)?)),
                ValType::F32 => Ok(NumPat::F32(ModuleParser::parse_f32(c)?)),
                ValType::F64 => Ok(NumPat::F64(ModuleParser::parse_f64(c)?)),
            },
        }
    }
    pub fn parse_meta(c: &mut ParseContext) -> Result<Meta, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => match &keyword[..] {
                "script" => {
                    c.expect_cur(&TokenType::KEYWORD("script".to_string()))?;
                    let id = ModuleParser::parse_id(c);
                    let script = Self::parse_script(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(Meta::script {
                        id: id,
                        script: script,
                    })
                }
                "input" => {
                    c.expect_cur(&TokenType::KEYWORD("input".to_string()))?;
                    let id = ModuleParser::parse_id(c);
                    let filename = ModuleParser::parse_name(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(Meta::input {
                        id: id,
                        filename: filename,
                    })
                }
                "output" => {
                    c.expect_cur(&TokenType::KEYWORD("output".to_string()))?;
                    let id = ModuleParser::parse_id(c);
                    let filename = if c.cur_token_is(&TokenType::RPAREN) {
                        None
                    } else {
                        Some(ModuleParser::parse_name(c)?)
                    };
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(Meta::output {
                        id: id,
                        filename: filename,
                    })
                }
                _ => Err(ParseError::ParseError(c.cur_token.clone())),
            },
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
}
impl Parse<Script> for ScriptParser {
    fn parse(c: &mut ParseContext) -> Result<Script, ParseError> {
        Ok(Self::parse_script(c)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::*;
    use std::io::Read;
    #[test]
    fn scrit_parse_test() {
        if let Ok(entries) = read_dir("testsuite") {
            for entry in entries {
                if let Ok(entry) = entry {
                    let filename = entry.file_name().into_string().unwrap();
                    if filename.ends_with(".wast") {
                        let mut f = std::fs::File::open(format!("testsuite/{}", filename)).unwrap();
                        let mut v = Vec::new();
                        f.read_to_end(&mut v).unwrap();
                        let input = String::from_utf8(v).unwrap();
                        let mut c = ParseContext::new(input);
                        match ScriptParser::parse_script(&mut c) {
                            Ok(s) => println!("{:#?}", s),
                            Err(e) => {
                                println!("{} => {:#?}", filename, e);
                                assert!(false)
                            }
                        }
                    }
                }
            }
        }
    }
}
