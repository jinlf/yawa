use crate::binary;
use crate::execution;
use crate::module;
use crate::parser::*;
use crate::structure;

//7.1.2 Errors
#[derive(Debug)]
pub enum Error {
    DecodeError(binary::Error),
    ParseError(ParseError),
    ExecuteError(execution::Error),
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
pub fn store_init() -> execution::Store {
    execution::Store::new()
}
//7.1.5
pub fn module_decode(bytes: Vec<u8>) -> Result<structure::Module, Error> {
    Ok(binary::decode(bytes)?)
}
pub fn module_parse(chars: String) -> Result<structure::Module, Error> {
    let mut c = ParseContext::new(chars);
    let module = module::ModuleParser::parse_module(&mut c)?;
    Ok(module.into())
}
pub fn module_validate(module: &structure::Module) -> Result<(), Error> {
    todo!()
}
pub fn module_instantiate(
    store: &mut execution::Store,
    module: &structure::Module,
    externvals: Vec<execution::ExternVal>,
) -> Result<execution::ModuleInst, Error> {
    todo!()
}
pub fn module_imports(
    module: &structure::Module,
) -> Vec<(structure::Name, structure::Name, structure::ExternType)> {
    todo!()
}
pub fn module_exports(module: &structure::Module) -> Vec<(structure::Name, structure::ExternType)> {
    todo!()
}
//7.1.6
pub fn instance_export(
    module_inst: &execution::ModuleInst,
    name: structure::Name,
) -> Result<execution::ExternVal, Error> {
    todo!()
}
//7.1.7
pub fn func_alloc(
    store: &mut execution::Store,
    functype: structure::FuncType,
    hostfunc: execution::HostFunc,
) -> execution::FuncAddr {
    todo!()
}
pub fn func_type(store: &execution::Store, funcaddr: execution::FuncAddr) -> structure::FuncType {
    todo!()
}
pub fn func_invoke(
    store: &mut execution::Store,
    funcaddr: execution::FuncAddr,
    vals: Vec<execution::Val>,
) -> Result<execution::Val, Error> {
    todo!()
}
//7.1.8
pub fn table_alloc(
    store: &mut execution::Store,
    tabletype: structure::TableType,
) -> execution::TableAddr {
    todo!()
}
pub fn table_type(
    store: &execution::Store,
    tableaddr: execution::TableAddr,
) -> structure::TableType {
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
pub fn mem_alloc(store: &mut execution::Store, memtype: structure::MemType) -> execution::MemAddr {
    todo!()
}
pub fn mem_type(store: &execution::Store, memaddr: execution::MemAddr) -> structure::MemType {
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
    globaltype: structure::GlobalType,
    val: execution::Val,
) -> execution::GlobalAddr {
    todo!()
}
pub fn global_type(
    store: &mut execution::Store,
    globaladdr: execution::GlobalAddr,
) -> structure::GlobalType {
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

// convert
impl Into<structure::Module> for module::Module {
    fn into(self) -> structure::Module {
        todo!()
    }
}
