use crate::binary;
use crate::execution;
use crate::module;
use crate::parser::*;
use crate::structure::*;
use std::cell::*;
use std::rc::*;

//7.1.2 Errors
#[derive(Debug)]
pub enum Error {
    DecodeError(binary::Error),
    ParseError(ParseError),
    ExecuteError(execution::Error),
    NotFound,
}
impl From<binary::Error> for Error {
    fn from(e: binary::Error) -> Self {
        Self::DecodeError(e)
    }
}
impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Self::ParseError(e)
    }
}
impl From<execution::Error> for Error {
    fn from(e: execution::Error) -> Self {
        Self::ExecuteError(e)
    }
}

//7.1.4
pub fn store_init() -> Rc<RefCell<execution::Store>> {
    execution::Store::new()
}
//7.1.5
pub fn module_decode(bytes: Vec<u8>) -> Result<Module, Error> {
    Ok(binary::decode(bytes)?)
}
pub fn module_parse(chars: String) -> Result<Module, Error> {
    let mut c = ParseContext::new(chars);
    let (_, module) = module::ModuleParser::parse_module(&mut c)?;
    Ok(module)
}
pub fn module_validate(module: &Module) -> Result<(), Error> {
    todo!()
}
pub fn module_instantiate(
    store: Rc<RefCell<execution::Store>>,
    module: &Module,
    externvals: Vec<execution::ExternVal>,
) -> Result<Rc<RefCell<execution::ModuleInst>>, Error> {
    let (f, _) = execution::instantiate(Rc::clone(&store), module, externvals)?;
    let frame = f.borrow();
    Ok(Rc::clone(&frame.module))
}
pub fn module_imports(module: &Module) -> Vec<(Name, Name, ExternType)> {
    todo!()
}
pub fn module_exports(module: &Module) -> Vec<(Name, ExternType)> {
    module
        .exports
        .iter()
        .map(|export| match &export.desc {
            ExportDesc::func(funcidx) => {
                let func = &module.funcs[*funcidx as usize];
                let typeidx = func.r#type;
                (
                    export.name.clone(),
                    ExternType::func(module.types[typeidx as usize].clone()),
                )
            }
            ExportDesc::table(tableidx) => {
                let tabletype = &module.tables[*tableidx as usize].r#type;
                (export.name.clone(), ExternType::table(tabletype.clone()))
            }
            ExportDesc::mem(memidx) => {
                let memtype = &module.mems[*memidx as usize].r#type;
                (export.name.clone(), ExternType::mem(memtype.clone()))
            }
            ExportDesc::global(globaidx) => {
                let globaltype = &module.globals[*globaidx as usize].r#type;
                (export.name.clone(), ExternType::global(globaltype.clone()))
            }
        })
        .collect()
}
//7.1.6
pub fn instance_export(
    moduleinst: Rc<RefCell<execution::ModuleInst>>,
    name: &Name,
) -> Result<execution::ExternVal, Error> {
    //1
    match moduleinst
        .borrow()
        .exports
        .iter()
        .find(|export| export.name == *name)
    {
        Some(export) => Ok(export.value.clone()),
        _ => Err(Error::NotFound),
    }
}
//7.1.7
pub fn func_alloc(
    store: &mut execution::Store,
    functype: FuncType,
    hostfunc: execution::HostFunc,
) -> execution::FuncAddr {
    todo!()
}
pub fn func_type(store: &execution::Store, funcaddr: execution::FuncAddr) -> FuncType {
    todo!()
}
pub fn func_invoke(
    store: Rc<RefCell<execution::Store>>,
    moduleinst: Rc<RefCell<execution::ModuleInst>>,
    funcaddr: execution::FuncAddr,
    vals: Vec<execution::Val>,
) -> Result<Vec<execution::Val>, Error> {
    let (F, vals1) = execution::invoke(Rc::clone(&store), Rc::clone(&moduleinst), funcaddr, vals)?;
    Ok(vals1)
}
//7.1.8
pub fn table_alloc(store: &mut execution::Store, tabletype: TableType) -> execution::TableAddr {
    todo!()
}
pub fn table_type(store: &execution::Store, tableaddr: execution::TableAddr) -> TableType {
    todo!()
}
pub fn table_read(
    store: &execution::Store,
    tableaddr: execution::TableAddr,
    i: u32,
) -> Result<Option<execution::FuncAddr>, Error> {
    todo!()
}
pub fn table_write(
    store: &mut execution::Store,
    tableaddr: execution::TableAddr,
    i: u32,
    funcaddr: Option<execution::FuncAddr>,
) -> Result<(), Error> {
    todo!()
}
pub fn table_size(store: &execution::Store, tableaddr: execution::TableAddr) -> u32 {
    todo!()
}
pub fn table_grow(
    store: &mut execution::Store,
    tableaddr: execution::TableAddr,
    n: u32,
) -> Result<(), Error> {
    todo!()
}
//7.1.9
pub fn mem_alloc(store: &mut execution::Store, memtype: MemType) -> execution::MemAddr {
    todo!()
}
pub fn mem_type(store: &execution::Store, memaddr: execution::MemAddr) -> MemType {
    todo!()
}
pub fn mem_read(
    store: &execution::Store,
    memaddr: execution::MemAddr,
    i: u32,
) -> Result<u8, Error> {
    todo!()
}
pub fn mem_write(
    store: &mut execution::Store,
    memaddr: execution::MemAddr,
    i: u32,
    byte: u8,
) -> Result<(), Error> {
    todo!()
}
pub fn mem_size(store: &execution::Store, memaddr: execution::MemAddr) -> u32 {
    todo!()
}
pub fn mem_grow(
    store: &mut execution::Store,
    memaddr: execution::MemAddr,
    n: u32,
) -> Result<(), Error> {
    todo!()
}
//7.1.10
pub fn global_alloc(
    store: &mut execution::Store,
    globaltype: GlobalType,
    val: execution::Val,
) -> execution::GlobalAddr {
    todo!()
}
pub fn global_type(store: &mut execution::Store, globaladdr: execution::GlobalAddr) -> GlobalType {
    todo!()
}
pub fn global_read(
    store: &mut execution::Store,
    globaladdr: execution::GlobalAddr,
) -> execution::Val {
    todo!()
}
pub fn global_write(
    store: &mut execution::Store,
    globaladdr: execution::GlobalAddr,
    val: execution::Val,
) -> Result<(), Error> {
    todo!()
}
