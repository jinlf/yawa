use crate::module::*;
use crate::parser::*;
use crate::structure::*;
use crate::token::*;
use std::cell::*;
use std::rc::*;

#[derive(Debug)]
pub struct Script {
    pub cmds: Vec<Cmd>,
}
#[derive(Debug)]
pub enum Cmd {
    extmodule(ExtModule),
    register { name: Name, id: Option<Id> },
    action(Action),
    assertion(Assertion),
    meta(Meta),
}
#[derive(Debug)]
pub enum ExtModule {
    module {
        id: Option<Id>,
        module: Module,
    },
    binary {
        id: Option<Id>,
        contents: Vec<Vec<u8>>,
    },
    quote {
        id: Option<Id>,
        contents: Vec<Vec<u8>>,
    },
}
#[derive(Debug)]
pub enum Action {
    invoke {
        id: Option<Id>,
        name: Name,
        exprs: Vec<Expr>,
    },
    get {
        id: Option<Id>,
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
#[derive(Debug, PartialEq)]
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
impl PartialEq for NumPat {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::I32(i1) => match other {
                Self::I32(i2) => i1 == i2,
                _ => false,
            },
            Self::I64(i1) => match other {
                Self::I64(i2) => i1 == i2,
                _ => false,
            },
            Self::F32(f1) => match other {
                Self::F32(f2) => {
                    if f1.is_nan() && f2.is_nan() {
                        true
                    } else if f1.is_infinite() && f2.is_infinite() {
                        f1.signum() == f2.signum()
                    } else {
                        f1 == f2
                    }
                }
                _ => false,
            },
            Self::F64(f1) => match other {
                Self::F64(f2) => {
                    if f1.is_nan() && f2.is_nan() {
                        true
                    } else if f1.is_infinite() && f2.is_infinite() {
                        f1.signum() == f2.signum()
                    } else {
                        f1 == f2
                    }
                }
                _ => false,
            },
            Self::nan_canonical => match other {
                Self::nan_canonical => true,
                _ => false,
            },
            Self::nan_arithmetic => match other {
                Self::nan_arithmetic => true,
                _ => false,
            },
        }
    }
}
#[derive(Debug)]
pub enum Meta {
    script {
        id: Option<Id>,
        script: Script,
    },
    input {
        id: Option<Id>,
        filename: Name,
    },
    output {
        id: Option<Id>,
        filename: Option<Name>,
    },
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
    fn parse_cmd(c: &mut ParseContext) -> Result<Cmd, ParseError> {
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
            _ => {
                let extmodule = Self::parse_extmodule(c)?;
                Ok(Cmd::extmodule(extmodule))
            }
        }
    }
    fn parse_extmodule(c: &mut ParseContext) -> Result<ExtModule, ParseError> {
        let has_module =
            if c.cur_token_is(&TokenType::LPAREN) && c.peek_token_is(&TokenType::MODULE) {
                c.expect_cur(&TokenType::LPAREN)?;
                c.expect_cur(&TokenType::MODULE)?;
                true
            } else {
                false
            };
        let id = ModuleParser::parse_id(c);
        let extmodule = if c.cur_token_is(&TokenType::KEYWORD("binary".to_string())) {
            c.next_token();
            let contents = ModuleParser::parse_vec_string(c)?;
            ExtModule::binary {
                id: id,
                contents: contents,
            }
        } else if c.cur_token_is(&TokenType::KEYWORD("quote".to_string())) {
            c.next_token();
            let contents = ModuleParser::parse_vec_string(c)?;
            ExtModule::quote {
                id: id,
                contents: contents,
            }
        } else {
            let mut module = Module::new();
            let mut I = IdentifierContext::new();
            loop {
                if !c.cur_token_is(&TokenType::LPAREN) {
                    break;
                }
                ModuleParser::parse_modulefield(c, &mut module, &mut I)?;
            }
            ExtModule::module {
                id: id,
                module: module,
            }
        };
        if has_module {
            c.expect_cur(&TokenType::RPAREN)?;
        }
        Ok(extmodule)
    }
    fn parse_action(c: &mut ParseContext) -> Result<Action, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => match &keyword[..] {
                "invoke" => {
                    c.expect_cur(&TokenType::KEYWORD("invoke".to_string()))?;
                    let id = ModuleParser::parse_id(c);
                    let name = ModuleParser::parse_name(c)?;
                    let mut module = Module::new();
                    let mut I = IdentifierContext::new();
                    let exprs = Self::parse_vec_expr(c, &mut module, &mut I)?;
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
    fn parse_vec_expr(
        c: &mut ParseContext,
        module: &mut Module,
        I: &mut crate::module::IdentifierContext,
    ) -> Result<Vec<Expr>, ParseError> {
        let mut exprs: Vec<Expr> = vec![];
        loop {
            if c.cur_token_is(&TokenType::RPAREN) {
                break;
            }
            exprs.push(ModuleParser::parse_expr(c, module, I)?);
        }
        Ok(exprs)
    }
    fn parse_assertion(c: &mut ParseContext) -> Result<Assertion, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => match &keyword[..] {
                "assert_return" => {
                    c.expect_cur(&TokenType::KEYWORD("assert_return".to_string()))?;
                    let action = Self::parse_action(c)?;
                    let results = Self::parse_vec_result(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
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
                        c.expect_cur(&TokenType::RPAREN)?;
                        Ok(Assertion::action_trap {
                            action: action,
                            failure: failure,
                        })
                    } else {
                        let extmodule = Self::parse_extmodule(c)?;
                        let failure = ModuleParser::parse_string(c)?;
                        c.expect_cur(&TokenType::RPAREN)?;
                        Ok(Assertion::module_trap {
                            module: extmodule,
                            failure: failure,
                        })
                    }
                }
                "assert_exhaustion" => {
                    c.expect_cur(&TokenType::KEYWORD("assert_exhaustion".to_string()))?;
                    let action = Self::parse_action(c)?;
                    let failure = ModuleParser::parse_string(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(Assertion::exhaustion {
                        action: action,
                        failure: failure,
                    })
                }
                "assert_malformed" => {
                    c.expect_cur(&TokenType::KEYWORD("assert_malformed".to_string()))?;
                    let extmodule = Self::parse_extmodule(c)?;
                    let failure = ModuleParser::parse_string(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(Assertion::malformed {
                        module: extmodule,
                        failure: failure,
                    })
                }
                "assert_invalid" => {
                    c.expect_cur(&TokenType::KEYWORD("assert_invalid".to_string()))?;
                    let extmodule = Self::parse_extmodule(c)?;
                    let failure = ModuleParser::parse_string(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(Assertion::invalid {
                        module: extmodule,
                        failure: failure,
                    })
                }
                "assert_unlinkable" => {
                    c.expect_cur(&TokenType::KEYWORD("assert_unlinkable".to_string()))?;
                    let extmodule = Self::parse_extmodule(c)?;
                    let failure = ModuleParser::parse_string(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(Assertion::unlinkable {
                        module: extmodule,
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
                            ValType::r#i32,
                        )?));
                        c.expect_cur(&TokenType::RPAREN)?;
                    }
                    "i64.const" => {
                        c.expect_cur(&TokenType::LPAREN)?;
                        c.expect_cur(&TokenType::KEYWORD("i64.const".to_string()))?;
                        results.push(AssertionResult::i64_const(Self::parse_numpat(
                            c,
                            ValType::r#i64,
                        )?));
                        c.expect_cur(&TokenType::RPAREN)?;
                    }
                    "f32.const" => {
                        c.expect_cur(&TokenType::LPAREN)?;
                        c.expect_cur(&TokenType::KEYWORD("f32.const".to_string()))?;
                        results.push(AssertionResult::f32_const(Self::parse_numpat(
                            c,
                            ValType::r#f32,
                        )?));
                        c.expect_cur(&TokenType::RPAREN)?;
                    }
                    "f64.const" => {
                        c.expect_cur(&TokenType::LPAREN)?;
                        c.expect_cur(&TokenType::KEYWORD("f64.const".to_string()))?;
                        results.push(AssertionResult::f64_const(Self::parse_numpat(
                            c,
                            ValType::r#f64,
                        )?));
                        c.expect_cur(&TokenType::RPAREN)?;
                    }
                    _ => return Err(ParseError::ParseError(c.cur_token.clone())),
                },
                _ => return Ok(results),
            }
        }
    }
    fn parse_numpat(c: &mut ParseContext, valtype: ValType) -> Result<NumPat, ParseError> {
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => match &keyword[..] {
                "nan:canonical" => Ok(NumPat::nan_canonical),
                "nan:arithmetic" => Ok(NumPat::nan_arithmetic),
                _ => Err(ParseError::ParseError(c.cur_token.clone())),
            },
            _ => match valtype {
                ValType::r#i32 => Ok(NumPat::I32(ModuleParser::parse_i32(c)?)),
                ValType::r#i64 => Ok(NumPat::I64(ModuleParser::parse_i64(c)?)),
                ValType::r#f32 => Ok(NumPat::F32(ModuleParser::parse_f32(c)?)),
                ValType::r#f64 => Ok(NumPat::F64(ModuleParser::parse_f64(c)?)),
            },
        }
    }
    fn parse_meta(c: &mut ParseContext) -> Result<Meta, ParseError> {
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
    // #[test]
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
