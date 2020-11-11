use crate::embedding::*;
use crate::module::Id;
use crate::script::*;
use crate::structure::*;
use std::cell::*;
use std::collections::HashMap;
use std::rc::*;

#[derive(Debug)]
pub enum Error {
    InterpretError,
    EmbeddingError(crate::embedding::Error),
    FromUtf8Error(std::string::FromUtf8Error),
    ParseError(crate::parser::ParseError),
    ExecuteError(crate::execution::Error),
}
impl From<crate::embedding::Error> for Error {
    fn from(e: crate::embedding::Error) -> Self {
        Self::EmbeddingError(e)
    }
}
impl From<std::string::FromUtf8Error> for Error {
    fn from(e: std::string::FromUtf8Error) -> Self {
        Self::FromUtf8Error(e)
    }
}
impl From<crate::parser::ParseError> for Error {
    fn from(e: crate::parser::ParseError) -> Self {
        Self::ParseError(e)
    }
}
impl From<crate::execution::Error> for Error {
    fn from(e: crate::execution::Error) -> Self {
        Self::ExecuteError(e)
    }
}

pub struct Environment {
    modules: Vec<Module>,
    module_ids: Vec<Option<Id>>,
    module_names: HashMap<String, Option<Id>>,
    module_instances: Vec<Rc<RefCell<crate::execution::ModuleInst>>>,
    stores: Vec<Rc<RefCell<crate::execution::Store>>>,
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
                let index = match extmodule {
                    ExtModule::module { id, module } => {
                        let index = env.borrow().modules.len();
                        env.borrow_mut().modules.push(module);
                        env.borrow_mut().module_ids.push(id.clone());
                        env.borrow_mut().stores.push(store_init());
                        index
                    }
                    ExtModule::binary { id, contents } => {
                        let module =
                            match String::from_utf8(contents.into_iter().flatten().collect()) {
                                Ok(v) => {
                                    let mut c = crate::parser::ParseContext::new(v);
                                    let (id, module) =
                                        crate::module::ModuleParser::parse_module(&mut c)?;
                                    module
                                }
                                _ => return Err(Error::InterpretError),
                            };
                        let index = env.borrow().modules.len();
                        env.borrow_mut().modules.push(module);
                        env.borrow_mut().module_ids.push(id.clone());
                        env.borrow_mut().stores.push(store_init());
                        index
                    }
                    ExtModule::quote { id, contents } => {
                        let module =
                            match String::from_utf8(contents.into_iter().flatten().collect()) {
                                Ok(v) => {
                                    let mut c = crate::parser::ParseContext::new(v);
                                    let (id, module) =
                                        crate::module::ModuleParser::parse_module(&mut c)?;
                                    module
                                }
                                _ => return Err(Error::InterpretError),
                            };
                        let index = env.borrow().modules.len();
                        env.borrow_mut().modules.push(module);
                        env.borrow_mut().module_ids.push(id.clone());
                        env.borrow_mut().stores.push(store_init());
                        index
                    }
                };
                let mut env1 = env.borrow_mut();
                let module = env1
                    .modules
                    .iter()
                    .nth(index)
                    .ok_or(Error::InterpretError)?;
                let store = env1.stores.iter().nth(index).ok_or(Error::InterpretError)?;
                let module_inst = crate::module_instantiate(Rc::clone(&store), module, vec![])?;
                env1.module_instances.push(Rc::clone(&module_inst));
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
                        Ok(_) => assert!(false),
                        Err(e) => match e {
                            Error::EmbeddingError(crate::embedding::Error::ExecuteError(
                                crate::execution::Error::TrapError(msg),
                            )) => assert_eq!(msg, failure),
                            _ => assert!(false),
                        },
                    }
                }
                Assertion::exhaustion { action, failure } => todo!(),
                Assertion::malformed { module, failure } => match module {
                    ExtModule::binary { id, contents } => {
                        match String::from_utf8(contents.into_iter().flatten().collect()) {
                            Ok(v) => assert!(false),
                            _ => {}
                        };
                    }
                    ExtModule::quote { id, contents } => {
                        match String::from_utf8(contents.into_iter().flatten().collect()) {
                            Ok(v) => {
                                let mut c = crate::parser::ParseContext::new(v);
                                match crate::module::ModuleParser::parse_module(&mut c) {
                                    Err(e) => {
                                        println!("err");
                                        println!("err");
                                    }
                                    _ => {}
                                }
                            }
                            _ => {}
                        };
                    }
                    _ => {}
                },
                Assertion::invalid { module, failure } => todo!(),
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
            let env = env.borrow();
            let index = if let Some(_) = id {
                env.module_ids
                    .iter()
                    .position(|ident| ident == id)
                    .ok_or(Error::InterpretError)?
            } else {
                env.modules.len() - 1
            };
            let module = env.modules.iter().nth(index).ok_or(Error::InterpretError)?;
            let moduleinst = env
                .module_instances
                .iter()
                .nth(index)
                .ok_or(Error::InterpretError)?;
            let export = instance_export(Rc::clone(moduleinst), name)?;
            let funcaddr = match export {
                crate::execution::ExternVal::func(funcaddr) => funcaddr,
                _ => return Err(Error::InterpretError),
            };

            let store = env.stores.iter().nth(index).ok_or(Error::InterpretError)?;
            let mut config =
                crate::execution::Config::new(Rc::clone(&store), Rc::clone(&moduleinst));
            let mut vals: Vec<crate::execution::Val> = vec![];
            for expr in exprs.iter() {
                let mut instrs = expr
                    .instrs
                    .iter()
                    .map(|instr| crate::execution::Instr::instr(instr.clone()))
                    .collect();
                vals.push(crate::execution::evalute(&mut config, &mut instrs)?);
            }
            Ok(
                func_invoke(Rc::clone(&store), Rc::clone(&moduleinst), funcaddr, vals)?
                    .into_iter()
                    .map(|val| match val {
                        crate::execution::Val::i32_const(v) => {
                            AssertionResult::i32_const(NumPat::I32(v))
                        }
                        crate::execution::Val::i64_const(v) => {
                            AssertionResult::i64_const(NumPat::I64(v))
                        }
                        crate::execution::Val::f32_const(v) => {
                            AssertionResult::f32_const(NumPat::F32(v))
                        }
                        crate::execution::Val::f64_const(v) => {
                            AssertionResult::f64_const(NumPat::F64(v))
                        }
                    })
                    .collect(),
            )
        }
        Action::get { id, name } => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;
    use std::fs::*;
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
                        let mut c = ParseContext::new(input);
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
        let mut f = std::fs::File::open("resources/address.wast").unwrap();
        let mut v = Vec::new();
        f.read_to_end(&mut v).unwrap();
        let input = String::from_utf8(v).unwrap();
        let mut c = ParseContext::new(input);
        let s = ScriptParser::parse_script(&mut c).unwrap();
        println!("Script=>{:#?}", s);
        intepret(s).unwrap();
    }
}
