use crate::binary;
use crate::execution;
use crate::module;
use crate::module::Id;
use crate::parser;
use crate::script::*;
use crate::structure::*;
use crate::token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum Error {
    InterpretError,
    FromUtf8Error(std::string::FromUtf8Error),
    ParseError(parser::Error),
    ExecuteError(execution::Error),
    ValidateError(String),
}
impl From<std::string::FromUtf8Error> for Error {
    fn from(e: std::string::FromUtf8Error) -> Self {
        Self::FromUtf8Error(e)
    }
}
impl From<parser::Error> for Error {
    fn from(e: parser::Error) -> Self {
        Self::ParseError(e)
    }
}
impl From<execution::Error> for Error {
    fn from(e: execution::Error) -> Self {
        Self::ExecuteError(e)
    }
}

pub struct Environment {
    modules: Vec<Module>,
    module_ids: Vec<Option<Id>>,
    module_names: HashMap<String, Option<Id>>,
    module_instances: Vec<Rc<RefCell<execution::ModuleInst>>>,
    stores: Vec<Rc<RefCell<execution::Store>>>,
}
impl Environment {
    fn new() -> Self {
        Self {
            modules: vec![],
            module_ids: vec![],
            module_names: HashMap::new(),
            module_instances: vec![],
            stores: vec![],
        }
    }
}

pub fn intepret(s: Script) -> Result<(), Error> {
    let env = Rc::new(RefCell::new(Environment::new()));
    for cmd in s.cmds.into_iter() {
        match cmd {
            Cmd::extmodule(extmodule) => {
                let (id, module) = match extmodule {
                    ExtModule::module { id, module } => (id, module),
                    ExtModule::binary { id, contents } => {
                        let module = match binary::decode(contents.into_iter().flatten().collect())
                        {
                            Ok(module) => module,
                            Err(e) => {
                                return Err(Error::InterpretError);
                            }
                        };
                        (id, module)
                    }
                    ExtModule::quote { id, contents } => {
                        let module =
                            match String::from_utf8(contents.into_iter().flatten().collect()) {
                                Ok(v) => {
                                    let mut c = parser::ParseContext::new(v)?;
                                    let (id, module) = module::ModuleParser::parse_module(&mut c)?;
                                    module
                                }
                                _ => return Err(Error::InterpretError),
                            };
                        (id, module)
                    }
                };
                env.borrow_mut().modules.push(module);
                env.borrow_mut().module_ids.push(id.clone());
            }
            Cmd::register { name, id } => {
                env.borrow_mut().module_names.insert(name, id);
            }
            Cmd::action(action) => match action {
                Action::invoke { id, name, exprs } => todo!(),
                Action::get { id, name } => todo!(),
            },
            Cmd::assertion(assertion) => match assertion {
                Assertion::r#return { action, results } => {
                    let rets = interpret_action(&action, Rc::clone(&env))?;
                    assert_eq!(rets, results);
                }
                Assertion::action_trap { action, failure } => {
                    match interpret_action(&action, Rc::clone(&env)) {
                        Err(Error::ExecuteError(execution::Error::TrapError(msg))) => assert_eq!(
                            msg,
                            match failure.r#type {
                                token::TokenType::STRING(s) => String::from_utf8(s)?,
                                _ => format!("{:#?}", failure),
                            }
                        ),
                        _ => assert!(false),
                    }
                }
                Assertion::exhaustion { action, failure } => todo!(),
                Assertion::malformed { module, failure } => match module {
                    ExtModule::binary { id, contents } => {
                        match binary::decode(contents.into_iter().flatten().collect()) {
                            Err(binary::Error::Malformed(msg)) => {
                                assert_eq!(
                                    msg,
                                    match failure.r#type {
                                        token::TokenType::STRING(s) => String::from_utf8(s)?,
                                        _ => format!("{:#?}", failure),
                                    }
                                );
                            }
                            _ => assert!(false),
                        };
                    }
                    ExtModule::quote { id, contents } => {
                        match String::from_utf8(contents.into_iter().flatten().collect()) {
                            Ok(v) => {
                                let mut c = parser::ParseContext::new(v)?;
                                match module::ModuleParser::parse_module(&mut c) {
                                    Err(parser::Error::Malformed(msg)) => {
                                        assert_eq!(
                                            msg,
                                            match failure.r#type {
                                                token::TokenType::STRING(s) =>
                                                    String::from_utf8(s)?,
                                                _ => format!("{:#?}", failure),
                                            }
                                        );
                                    }
                                    _ => assert!(false),
                                }
                            }
                            _ => assert!(false),
                        };
                    }
                    _ => assert!(false),
                },
                Assertion::invalid { module, failure } => match module {
                    ExtModule::module { id, module } => {
                        let store = execution::Store::new();
                        match execution::instantiate(Rc::clone(&store), &module, vec![]) {
                            Err(execution::Error::ValidateError(msg)) => assert_eq!(
                                msg,
                                match failure.r#type {
                                    token::TokenType::STRING(s) => String::from_utf8(s)?,
                                    _ => format!("{:#?}", failure),
                                }
                            ),
                            _ => assert!(false),
                        }
                    }
                    _ => assert!(false),
                },
                Assertion::unlinkable { module, failure } => todo!(),
                Assertion::module_trap { module, failure } => todo!(),
            },
            Cmd::meta(meta) => match meta {
                Meta::script { id, script } => todo!(),
                Meta::input { id, filename } => todo!(),
                Meta::output { id, filename } => todo!(),
            },
        }
    }
    Ok(())
}
fn interpret_action(
    action: &Action,
    env: Rc<RefCell<Environment>>,
) -> Result<Vec<AssertionResult>, Error> {
    match action {
        Action::invoke { id, name, exprs } => {
            let mut env = env.borrow_mut();
            let index = if let Some(_) = id {
                env.module_ids
                    .iter()
                    .position(|ident| ident == id)
                    .ok_or(Error::InterpretError)?
            } else {
                env.modules.len() - 1
            };
            let module = env.modules.iter().nth(index).ok_or(Error::InterpretError)?;
            let store = execution::Store::new();
            let (F, vals) = execution::instantiate(Rc::clone(&store), &module, vec![])?;
            let moduleinst = &F.borrow().module;
            let funcaddr = match moduleinst
                .borrow()
                .exports
                .iter()
                .find(|export| export.name == *name)
            {
                Some(execution::ExportInst { name: _, value }) => match value {
                    execution::ExternVal::func(funcaddr) => *funcaddr,
                    _ => return Err(Error::InterpretError),
                },
                _ => return Err(Error::InterpretError),
            };

            let mut config = execution::Config::new(Rc::clone(&store), Rc::clone(&moduleinst));
            let mut vals: Vec<execution::Val> = vec![];
            for expr in exprs.iter() {
                let mut instrs = expr
                    .instrs
                    .iter()
                    .map(|instr| execution::Instr::instr(instr.clone()))
                    .collect();
                vals.push(execution::evalute(&mut config, &mut instrs)?);
            }
            Ok(
                execution::invoke(Rc::clone(&store), Rc::clone(&moduleinst), funcaddr, vals)?
                    .1
                    .iter()
                    .map(|val| match val {
                        execution::Val::i32_const(v) => AssertionResult::i32_const(NumPat::I32(*v)),
                        execution::Val::i64_const(v) => AssertionResult::i64_const(NumPat::I64(*v)),
                        execution::Val::f32_const(v) => AssertionResult::f32_const(NumPat::F32(*v)),
                        execution::Val::f64_const(v) => AssertionResult::f64_const(NumPat::F64(*v)),
                    })
                    .collect(),
            )
        }
        Action::get { id, name } => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::{intepret, ScriptParser};
    use crate::parser::ParseContext;
    use std::fs::read_dir;
    use std::io::Read;

    // #[test]
    fn intepret_test() {
        if let Ok(entries) = read_dir("testsuite") {
            for entry in entries {
                if let Ok(entry) = entry {
                    let filename = entry.file_name().into_string().unwrap();
                    if filename.ends_with(".wast") {
                        let mut f = std::fs::File::open(format!("testsuite/{}", filename)).unwrap();
                        let mut v = Vec::new();
                        f.read_to_end(&mut v).unwrap();
                        let input = String::from_utf8(v).unwrap();
                        let mut c = ParseContext::new(input).unwrap();
                        match ScriptParser::parse_script(&mut c) {
                            Ok(s) => {
                                intepret(s).unwrap();
                            }
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

    #[test]
    fn intepret_test1() {
        let mut f = std::fs::File::open("resources/block.wast").unwrap();
        let mut v = Vec::new();
        f.read_to_end(&mut v).unwrap();
        let input = String::from_utf8(v).unwrap();
        let mut c = ParseContext::new(input).unwrap();
        let s = ScriptParser::parse_script(&mut c).unwrap();
        // println!("{:#?}", s);
        match intepret(s) {
            Ok(_) => {}
            Err(e) => {
                println!("{:#?}", e);
                assert!(false);
            }
        }
    }
}
