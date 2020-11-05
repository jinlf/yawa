use crate::structure;
use crate::validation;
use libc;
use std::cell::*;
use std::fmt::Debug;
use std::rc::*;

#[derive(Debug)]
pub enum Error {
    ExecuteError,
    ValidateError(validation::Error),
}
impl From<validation::Error> for Error {
    fn from(e: validation::Error) -> Self {
        Self::ValidateError(e)
    }
}

//4.2.1
#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    i32_const(i32),
    i64_const(i64),
    f32_const(f32),
    f64_const(f64),
}
impl Default for Val {
    fn default() -> Self {
        Val::i32_const(0)
    }
}

//4.2.2
pub enum ExecResult {
    vals(Vec<Val>),
    trap,
}

//4.2.3
#[derive(Default, Debug)]
pub struct Store {
    pub funcs: Vec<FuncInst>,
    pub tables: Vec<TableInst>,
    pub mems: Vec<MemInst>,
    pub globals: Vec<GlobalInst>,
}
impl Store {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            funcs: vec![],
            tables: vec![],
            mems: vec![],
            globals: vec![],
        }))
    }
}

//4.2.4
pub type Addr = usize;
pub type FuncAddr = Addr;
pub type TableAddr = Addr;
pub type MemAddr = Addr;
pub type GlobalAddr = Addr;

//4.2.5
#[derive(Debug, Default)]
pub struct ModuleInst {
    pub types: Vec<structure::FuncType>,
    pub funcaddrs: Vec<FuncAddr>,
    pub tableaddrs: Vec<TableAddr>,
    pub memaddrs: Vec<MemAddr>,
    pub globaladdrs: Vec<GlobalAddr>,
    pub exports: Vec<ExportInst>,
}

//4.2.6
#[derive(Debug)]
pub enum FuncInst {
    func {
        r#type: structure::FuncType,
        module: Rc<RefCell<ModuleInst>>,
        code: structure::Func,
    },
    hostfunc {
        r#type: structure::FuncType,
        hostcode: HostFunc,
    },
}
pub type HostFunc = usize;

//4.2.7
#[derive(Debug)]
pub struct TableInst {
    elem: Vec<FuncElem>,
    max: Option<u32>,
}
pub type FuncElem = Option<FuncAddr>;

//4.2.8
pub struct MemInst {
    data: Vec<u8>,
    max: Option<u32>,
}
impl Debug for MemInst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MemInst").field("max", &self.max).finish()
    }
}

//4.2.9
#[derive(Debug)]
pub struct GlobalInst {
    value: Val,
    r#mut: structure::Mut,
}

//4.2.10
#[derive(Debug)]
pub struct ExportInst {
    name: structure::Name,
    value: ExternVal,
}

//4.2.11
#[derive(Debug, Clone)]
pub enum ExternVal {
    func(FuncAddr),
    table(TableAddr),
    mem(MemAddr),
    global(GlobalAddr),
}
pub fn funcs_extrnvals(externvals: Vec<ExternVal>) -> Vec<FuncAddr> {
    externvals
        .into_iter()
        .filter_map(|ev| {
            if let ExternVal::func(funcaddr) = ev {
                Some(funcaddr)
            } else {
                None
            }
        })
        .collect()
}
pub fn tables_extrnvals(externvals: Vec<ExternVal>) -> Vec<TableAddr> {
    externvals
        .into_iter()
        .filter_map(|ev| {
            if let ExternVal::table(tableaddr) = ev {
                Some(tableaddr)
            } else {
                None
            }
        })
        .collect()
}
pub fn mems_extrnvals(externvals: Vec<ExternVal>) -> Vec<MemAddr> {
    externvals
        .into_iter()
        .filter_map(|ev| {
            if let ExternVal::mem(memaddr) = ev {
                Some(memaddr)
            } else {
                None
            }
        })
        .collect()
}
pub fn globals_extrnvals(externvals: Vec<ExternVal>) -> Vec<GlobalAddr> {
    externvals
        .into_iter()
        .filter_map(|ev| {
            if let ExternVal::global(globaladdr) = ev {
                Some(globaladdr)
            } else {
                None
            }
        })
        .collect()
}

//4.2.12
type Stack = Vec<Entry>;
#[derive(Debug)]
enum Entry {
    val(Val),
    label(Label),
    activation(Activation),
}
#[derive(Clone, Debug)]
pub struct Label {
    pub n: usize,
    pub instrs: Vec<Instr>,
}
pub struct Activation {
    pub n: usize,
    pub frame: Rc<RefCell<Frame>>,
}
impl Debug for Activation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("Activation").field("n", &self.n).finish()
    }
}
#[derive(Clone, Debug)]
pub struct Frame {
    pub locals: Vec<Val>,
    pub module: Rc<RefCell<ModuleInst>>,
}
impl PartialEq for Frame {
    fn eq(&self, other: &Self) -> bool {
        self.locals == other.locals //TODO
    }
}
fn expand_typeidx(F: Rc<RefCell<Frame>>, typeidx: structure::TypeIdx) -> structure::FuncType {
    F.borrow().module.borrow().types[typeidx as usize].clone()
}
fn expand_valtypes(
    F: Rc<RefCell<Frame>>,
    valtypes: Vec<structure::ValType>,
) -> structure::FuncType {
    structure::FuncType {
        params: vec![],
        results: valtypes,
    }
}

//4.2.13
#[derive(Clone, Debug)]
pub enum Instr {
    instr(structure::Instr),
    trap,
    invoke(FuncAddr),
    init_elem {
        tableaddr: TableAddr,
        index: u32,
        funcidxes: Vec<structure::FuncIdx>,
    },
    init_data {
        memaddr: MemAddr,
        index: u32,
        bytes: Vec<u8>,
    },
    label {
        n: u32,
        instrs: Vec<Box<Instr>>,
        others: Vec<Box<Instr>>,
    },
    frame {
        n: u32,
        frame: Rc<RefCell<Frame>>,
        instrs: Vec<Box<Instr>>,
    },
}
pub struct Config {
    store: Rc<RefCell<Store>>,
    thread: Thread,
}
pub struct Thread {
    pub frame: Rc<RefCell<Frame>>,
    stack: Stack,
}
fn reduce_to_end(config: &mut Config, instrs: &mut Vec<Instr>) {
    loop {
        if reduce(config, instrs) == false {
            return;
        }
    }
}
fn reduce(config: &mut Config, instrs: &mut Vec<Instr>) -> bool {
    if instrs.len() == 0 {
        return false;
    }
    println!("instrs=>{:#?}", instrs);
    let instr = instrs.remove(0);
    match instr {
        Instr::instr(instr) => match instr {
            structure::Instr::end => {
                let mut vals: Vec<Val> = vec![];
                loop {
                    //2
                    let entry = config.thread.stack.pop();
                    match entry {
                        Some(Entry::val(val)) => {
                            vals.insert(0, val);
                        }
                        Some(Entry::label(_)) => {
                            // Exiting instr* with label L
                            //1
                            let m = vals.len();
                            //3
                            //4
                            for _ in 0..m {
                                config.thread.stack.push(Entry::val(vals.remove(0)));
                            }
                            return true;
                        }
                        Some(Entry::activation(activation)) => {
                            // Returning from a function
                            //1
                            //2
                            let Activation { n, frame } = activation;
                            config.thread.frame = Rc::clone(&frame);
                            //3
                            //4
                            //5
                            //6
                            //7
                            for _ in 0..vals.len() {
                                config.thread.stack.push(Entry::val(vals.remove(0)));
                            }
                            return true;
                        }
                        _ => {
                            instrs.insert(0, Instr::trap);
                            return true;
                        }
                    }
                }
            }
            structure::Instr::drop => {
                let entry = config.thread.stack.pop();
                return true;
            }
            structure::Instr::i32_const(v) => {
                config.thread.stack.push(Entry::val(Val::i32_const(v)));
                return true;
            }
            structure::Instr::local_get(x) => {
                //1
                //2
                //3
                let val = config.thread.frame.borrow().locals[x as usize].clone();
                config.thread.stack.push(Entry::val(val));
                return true;
            }
            structure::Instr::call(x) => {
                let a = config.thread.frame.borrow().module.borrow().funcaddrs[x as usize];
                instrs.insert(0, Instr::invoke(a));
                return true;
            }

            _ => {
                println!("unsupported instr: {:#?}", instrs[0]);
                return false;
            }
        },
        Instr::invoke(a) => {
            //1
            //2
            let f = &config.store.borrow().funcs[a as usize];
            //3
            match f {
                FuncInst::func {
                    r#type,
                    module: _,
                    code,
                } => {
                    let structure::FuncType { params, results } = r#type;
                    let n = params.len();
                    let m = results.len();
                    //4
                    let ts = &code.locals;
                    //5
                    //6
                    //7
                    let mut vals: Vec<Val> = vec![];
                    for _ in 0..n {
                        let entry = config.thread.stack.pop();
                        match entry {
                            Some(Entry::val(val)) => {
                                vals.insert(0, val);
                            }
                            _ => {
                                instrs.insert(0, Instr::trap);
                                return true;
                            }
                        }
                    }
                    //8
                    for t in ts.iter() {
                        match t {
                            structure::ValType::r#i32 => {
                                vals.push(Val::i32_const(0));
                            }
                            structure::ValType::r#i64 => {
                                vals.push(Val::i64_const(0));
                            }
                            structure::ValType::r#f32 => {
                                vals.push(Val::f32_const(0.0));
                            }
                            structure::ValType::r#f64 => {
                                vals.push(Val::f64_const(0.0));
                            }
                        }
                    }
                    //9
                    let newF = Rc::new(RefCell::new(Frame {
                        module: Rc::clone(&config.thread.frame.borrow().module),
                        locals: vals,
                    }));
                    //10
                    config.thread.frame = Rc::clone(&newF);
                    config.thread.stack.push(Entry::activation(Activation {
                        n: m,
                        frame: Rc::clone(&newF),
                    }));
                    //11
                    config.thread.stack.push(Entry::label(Label {
                        n: m,
                        instrs: vec![],
                    }));
                    let body_instrs: Vec<Instr> = code
                        .body
                        .instrs
                        .iter()
                        .map(|instr| Instr::instr(instr.clone()))
                        .collect();
                    instrs.splice(..0, body_instrs);
                    //12
                    instrs.push(Instr::instr(structure::Instr::end));
                    instrs.push(Instr::instr(structure::Instr::end));
                    return true;
                }
                FuncInst::hostfunc { r#type, hostcode } => {
                    let structure::FuncType { params, results } = r#type;
                    let n = params.len();
                    let m = results.len();
                    let mut vals: Vec<Val> = vec![];
                    for _ in 0..n {
                        let entry = config.thread.stack.pop();
                        match entry {
                            Some(Entry::val(val)) => {
                                vals.insert(0, val);
                            }
                            _ => {
                                instrs.insert(0, Instr::trap);
                                return true;
                            }
                        }
                    }
                    match invoke_hostfunc(
                        Rc::clone(&config.store),
                        Rc::clone(&config.thread.frame),
                        *hostcode,
                        &vals,
                    ) {
                        Ok(retvals) => {
                            for retval in retvals.into_iter() {
                                config.thread.stack.push(Entry::val(retval));
                            }
                        }
                        Err(_) => {
                            instrs.insert(0, Instr::trap);
                            return true;
                        }
                    }
                    return true;
                }
            };
        }
        _ => {
            println!("instr: {:#?}", instr);
            todo!();
        }
    }
}

//4.3.2
fn r#bool(c: bool) -> u32 {
    match c {
        true => 1,
        false => 0,
    }
}

//4.3.3

//4.3.4
fn extend_i32_u64(i: i32) -> u64 {
    i as u64
}
fn extend_i64_u32(i: i64) -> u32 {
    i as u32
}
fn extend_u32_s64(i: u32) -> i64 {
    i as i64
}
fn extend_u64_s32(i: u64) -> i32 {
    i as i32
}

//4.4.8
fn evalute(config: &mut Config, instrs: &mut Vec<Instr>) -> Result<Val, Error> {
    reduce_to_end(config, instrs);
    let entry = config.thread.stack.pop();
    match entry {
        Some(Entry::val(val)) => Ok(val),
        _ => Err(Error::ExecuteError),
    }
}

//4.5.2
trait TypeMatch {
    fn type_match(&self, other: &Self) -> bool;
}
impl TypeMatch for structure::Limits {
    fn type_match(&self, other: &Self) -> bool {
        let n1 = self.min;
        let m1 = self.max;
        let n2 = other.min;
        let m2 = other.max;
        n1 >= n2 && (m2.unwrap_or_else(|| u32::MAX) >= m1.unwrap_or_else(|| u32::MIN))
    }
}
impl TypeMatch for structure::FuncType {
    fn type_match(&self, other: &Self) -> bool {
        self.params == other.params && self.results == other.results
    }
}
impl TypeMatch for structure::TableType {
    fn type_match(&self, other: &Self) -> bool {
        self.limits.type_match(&other.limits) && self.elemtype == other.elemtype
    }
}
impl TypeMatch for structure::MemType {
    fn type_match(&self, other: &Self) -> bool {
        self.limits.type_match(&other.limits)
    }
}
impl TypeMatch for structure::GlobalType {
    fn type_match(&self, other: &Self) -> bool {
        self == other
    }
}
impl TypeMatch for structure::ExternType {
    fn type_match(&self, other: &Self) -> bool {
        match self {
            Self::func(functype1) => match other {
                Self::func(functype2) => functype1.type_match(functype2),
                _ => false,
            },
            Self::table(tabletype1) => match other {
                Self::table(tabletype2) => tabletype1.type_match(tabletype2),
                _ => false,
            },
            Self::mem(memtype1) => match other {
                Self::mem(memtype2) => memtype1.type_match(memtype2),
                _ => false,
            },
            Self::global(globaltype1) => match other {
                Self::global(globaltype2) => globaltype1.type_match(globaltype2),
                _ => false,
            },
        }
    }
}

//4.5.3
fn allocfunc(
    S: Rc<RefCell<Store>>,
    func: &structure::Func,
    moduleinst: Rc<RefCell<ModuleInst>>,
) -> FuncAddr {
    //1
    //2
    let funcaddr = S.borrow().funcs.len();
    //3
    let functype = &moduleinst.borrow().types[func.r#type as usize];
    //4
    let funcinst = FuncInst::func {
        r#type: functype.clone(),
        module: Rc::clone(&moduleinst),
        code: func.clone(),
    };
    //5
    S.borrow_mut().funcs.push(funcinst);
    //6
    funcaddr
}
fn allochostfunc(
    S: Rc<RefCell<Store>>,
    functype: &structure::FuncType,
    hostfunc: HostFunc,
) -> FuncAddr {
    //1
    //2
    let funcaddr = S.borrow().funcs.len();
    //3
    let funcinst = FuncInst::hostfunc {
        r#type: functype.clone(),
        hostcode: hostfunc,
    };
    //4
    S.borrow_mut().funcs.push(funcinst);
    //5
    funcaddr
}
fn alloctable(S: Rc<RefCell<Store>>, tabletype: &structure::TableType) -> TableAddr {
    //1
    //2
    let structure::TableType { limits, elemtype } = tabletype;
    let n = limits.min;
    let m = limits.max;
    //3
    let tableaddr = S.borrow().tables.len();
    //4
    let tableinst = TableInst {
        elem: vec![FuncElem::default(); n as usize],
        max: limits.max,
    };
    //5
    S.borrow_mut().tables.push(tableinst);
    //6
    tableaddr
}
fn allocmem(S: Rc<RefCell<Store>>, memtype: &structure::MemType) -> MemAddr {
    //1
    //2
    let structure::MemType { limits } = memtype;
    let n = limits.min;
    let m = limits.max;
    //3
    let memaddr = S.borrow().mems.len();
    //4
    let meminst = MemInst {
        data: vec![0; n as usize * 64 * 1024],
        max: limits.max,
    };
    //5
    S.borrow_mut().mems.push(meminst);
    //6
    memaddr
}
fn allocglobal(S: Rc<RefCell<Store>>, globaltype: &structure::GlobalType, val: &Val) -> GlobalAddr {
    //1
    //2
    let structure::GlobalType { r#mut, valtype: _ } = globaltype;
    //3
    let globaladdr = S.borrow().globals.len();
    //4
    let globalinst = GlobalInst {
        value: val.clone(),
        r#mut: r#mut.clone(),
    };
    //5
    S.borrow_mut().globals.push(globalinst);
    //6
    globaladdr
}
fn growtable(tableinst: &mut TableInst, n: u32) -> Result<(), Error> {
    //1
    //2
    let len: usize = n as usize;
    if len >= u32::MAX as usize {
        //3
        Err(Error::ExecuteError)
    } else if tableinst.max.is_some() && (tableinst.max.unwrap() as usize) < len {
        //4
        Err(Error::ExecuteError)
    } else {
        let mut newitems = vec![FuncElem::default(); len];
        Ok(tableinst.elem.append(&mut newitems)) //5
    }
}
fn growmem(meminst: &mut MemInst, n: u32) -> Result<(), Error> {
    //1
    //2
    assert_eq!(meminst.data.len() % (64 * 1024), 0);
    //3
    let len: usize = n as usize;
    if len > (1_usize << 16) {
        //4
        Err(Error::ExecuteError)
    } else if meminst.max.is_some() && (meminst.max.unwrap() as usize) < len {
        //5
        Err(Error::ExecuteError)
    } else {
        let mut newitems = vec![0; len];
        Ok(meminst.data.append(&mut newitems))
    }
}
fn allocmodule(
    S: Rc<RefCell<Store>>,
    module: &structure::Module,
    externvals_im: Vec<ExternVal>,
    vals: &Vec<Val>,
) -> Rc<RefCell<ModuleInst>> {
    let moduleinst = Rc::new(RefCell::new(ModuleInst::default()));
    moduleinst.borrow_mut().types = module.types.clone();
    //1
    //2
    let mut funcaddrs: Vec<FuncAddr> = module
        .funcs
        .iter()
        .map(|func| allocfunc(Rc::clone(&S), func, Rc::clone(&moduleinst)))
        .collect();
    //3
    let mut tableaddrs: Vec<TableAddr> = module
        .tables
        .iter()
        .map(|table| alloctable(Rc::clone(&S), &table.r#type))
        .collect();
    //4
    let mut memaddrs: Vec<MemAddr> = module
        .mems
        .iter()
        .map(|mem| allocmem(Rc::clone(&S), &mem.r#type))
        .collect();
    //5
    let mut globaladdrs: Vec<GlobalAddr> = module
        .globals
        .iter()
        .enumerate()
        .map(|(i, global)| allocglobal(Rc::clone(&S), &global.r#type, &vals[i]))
        .collect();
    //6
    //7
    //8
    //9
    //10
    let mut funcaddrs_mod: Vec<FuncAddr> = externvals_im
        .iter()
        .filter_map(|externval| match externval {
            ExternVal::func(funcaddr) => Some(funcaddr),
            _ => None,
        })
        .copied()
        .chain(funcaddrs.into_iter())
        .collect();
    //11
    let mut tableaddrs_mod: Vec<TableAddr> = externvals_im
        .iter()
        .filter_map(|externval| match externval {
            ExternVal::table(tableaddr) => Some(tableaddr),
            _ => None,
        })
        .copied()
        .chain(tableaddrs.into_iter())
        .collect();
    //12
    let mut memaddrs_mod: Vec<MemAddr> = externvals_im
        .iter()
        .filter_map(|externval| match externval {
            ExternVal::mem(memaddr) => Some(memaddr),
            _ => None,
        })
        .copied()
        .chain(memaddrs.into_iter())
        .collect();
    //13
    let mut globaladdrs_mod: Vec<GlobalAddr> = externvals_im
        .iter()
        .filter_map(|externval| match externval {
            ExternVal::global(globaladdr) => Some(globaladdr),
            _ => None,
        })
        .copied()
        .chain(globaladdrs.into_iter())
        .collect();
    //14
    let mut exportinsts = vec![];
    for export in module.exports.iter() {
        let value = match export.desc {
            structure::ExportDesc::func(funcidx) => {
                //14.a
                ExternVal::func(funcaddrs_mod[funcidx as usize].clone())
            }
            structure::ExportDesc::table(tableidx) => {
                //14.b
                ExternVal::table(tableaddrs_mod[tableidx as usize].clone())
            }
            structure::ExportDesc::mem(memidx) => {
                //14.c
                ExternVal::mem(memaddrs_mod[memidx as usize].clone())
            }
            structure::ExportDesc::global(globalidx) => {
                //14.d
                ExternVal::global(globaladdrs_mod[globalidx as usize].clone())
            }
        };
        //14.e
        let exportinst = ExportInst {
            name: export.name.clone(),
            value: value,
        };
        //15
        exportinsts.push(exportinst);
    }
    //16
    moduleinst.borrow_mut().funcaddrs.append(&mut funcaddrs_mod);
    moduleinst
        .borrow_mut()
        .tableaddrs
        .append(&mut tableaddrs_mod);
    moduleinst.borrow_mut().memaddrs.append(&mut memaddrs_mod);
    moduleinst
        .borrow_mut()
        .globaladdrs
        .append(&mut globaladdrs_mod);
    moduleinst.borrow_mut().exports.append(&mut exportinsts);
    //17
    Rc::clone(&moduleinst)
}

//4.5.4
fn instantiate(
    S: Rc<RefCell<Store>>,
    module: &structure::Module,
    externvals: Vec<ExternVal>,
) -> Result<(Rc<RefCell<Frame>>, Vec<Val>), Error> {
    //1
    crate::validation::module_validate(module)?;
    //2
    let mut externtypes_im: Vec<structure::ExternType> = vec![];
    for import in module.imports.iter() {
        externtypes_im.push(match &import.desc {
            structure::ImportDesc::func(typeidx) => {
                let functype = module.types[*typeidx as usize].clone();
                structure::ExternType::func(functype)
            }
            structure::ImportDesc::table(tabletype) => {
                structure::ExternType::table(tabletype.clone())
            }
            structure::ImportDesc::mem(memtype) => structure::ExternType::mem(memtype.clone()),
            structure::ImportDesc::global(globaltype) => {
                structure::ExternType::global(globaltype.clone())
            }
        });
    }
    //TODO!
    //3
    let m = module.imports.len();
    let n = externvals.len();
    if m != n {
        return Err(Error::ExecuteError);
    }

    //4
    for i in 0..n {
        let externval = &externvals[i];
        let externtype1 = externtypes_im[i].clone();
        let externtype = match externval {
            ExternVal::func(funcaddr) => match &S.borrow().funcs[*funcaddr as usize] {
                FuncInst::func {
                    r#type,
                    module: _,
                    code: _,
                } => structure::ExternType::func(r#type.clone()),
                FuncInst::hostfunc {
                    r#type,
                    hostcode: _,
                } => structure::ExternType::func(r#type.clone()),
            },
            ExternVal::table(tableaddr) => {
                let tableinst = &S.borrow().tables[*tableaddr as usize];
                let n = tableinst.elem.len() as u32;
                let m = tableinst.max;
                structure::ExternType::table(structure::TableType {
                    elemtype: structure::ElemType::FuncRef,
                    limits: structure::Limits { min: n, max: m },
                })
            }
            ExternVal::mem(memaddr) => {
                let meminst = &S.borrow().mems[*memaddr as usize];
                let n = (meminst.data.len() / (64 * 1024)) as u32;
                let m = meminst.max;
                structure::ExternType::mem(structure::MemType {
                    limits: structure::Limits { min: n, max: m },
                })
            }
            ExternVal::global(globaladdr) => {
                let globalinst = &S.borrow().globals[*globaladdr as usize];
                let valtype = match globalinst.value {
                    Val::i32_const(_) => structure::ValType::r#i32,
                    Val::i64_const(_) => structure::ValType::r#i64,
                    Val::f32_const(_) => structure::ValType::r#f32,
                    Val::f64_const(_) => structure::ValType::r#f64,
                };
                structure::ExternType::global(structure::GlobalType {
                    r#mut: globalinst.r#mut.clone(),
                    valtype: valtype,
                })
            }
        };
        //4.a
        //TODO
        //4.b
        if !externtype.type_match(&externtype1) {
            return Err(Error::ExecuteError);
        }
    }
    //5
    let mut vals: Vec<Val> = vec![];
    //5.a
    let moduleinst_im = Rc::new(RefCell::new(ModuleInst::default()));
    for externval in externvals.iter() {
        match externval {
            ExternVal::global(globaladdr) => {
                moduleinst_im.borrow_mut().globaladdrs.push(*globaladdr);
            }
            _ => {}
        }
    }
    //5.b
    let F_im = Rc::new(RefCell::new(Frame {
        locals: vec![],
        module: Rc::clone(&moduleinst_im),
    }));
    //5.c
    let mut config = Config {
        store: S,
        thread: Thread {
            frame: Rc::clone(&F_im),
            stack: vec![],
        },
    };
    config.thread.stack.push(Entry::activation(Activation {
        n: 0,
        frame: Rc::clone(&F_im),
    }));
    //5.d
    for global in module.globals.iter() {
        //5.d.i
        let mut instrs = global
            .init
            .instrs
            .iter()
            .map(|instr| Instr::instr(instr.clone()))
            .collect();
        vals.push(evalute(&mut config, &mut instrs)?);
    }
    //5.e
    //5.f
    let entry = config.thread.stack.pop();
    match entry {
        Some(Entry::activation(Activation { n: _, frame: _ })) => {}
        _ => return Err(Error::ExecuteError),
    }

    //6
    let moduleinst = allocmodule(Rc::clone(&config.store), module, externvals, &vals);
    //7
    let F = Rc::new(RefCell::new(Frame {
        locals: vec![],
        module: Rc::clone(&moduleinst),
    }));
    //8
    config.thread.frame = Rc::clone(&F);
    config.thread.stack.push(Entry::activation(Activation {
        n: 0,
        frame: Rc::clone(&F),
    }));
    //9
    let mut eovals = vec![];
    for (i, elem) in module.elem.iter().enumerate() {
        //9.a
        let mut instrs = elem
            .offset
            .instrs
            .iter()
            .map(|x| Instr::instr(x.clone()))
            .collect();
        let val = evalute(&mut config, &mut instrs)?;
        match val {
            Val::i32_const(eo) => eovals.push(eo),
            _ => return Err(Error::ExecuteError), //9.b
        }
        //9.c
        let tableidx = elem.table;
        //9.d
        //9.e
        let tableaddr = moduleinst.borrow().tableaddrs[tableidx as usize];
        //9.f
        //9.g
        let tableinst = &config.store.borrow().tables[tableaddr as usize];
        //9.h
        let eend = (eovals[i] as usize) + elem.init.len();
        //9.i
        if eend > tableinst.elem.len() {
            return Err(Error::ExecuteError);
        }
    }
    //10
    let mut dovals = vec![];
    for (i, data) in module.data.iter().enumerate() {
        //10.a
        let mut instrs = data
            .offset
            .instrs
            .iter()
            .map(|x| Instr::instr(x.clone()))
            .collect();
        let val = evalute(&mut config, &mut instrs)?;
        //10.b

        match val {
            Val::i32_const(r#do) => dovals.push(r#do),
            _ => return Err(Error::ExecuteError), //10.b
        }
        //10.c
        let memidx = data.data;
        //10.d
        //10.e
        let memaddr = moduleinst.borrow().memaddrs[memidx as usize];
        //10.f
        //10.g
        let meminst = &config.store.borrow().mems[memaddr as usize];
        //10.h
        let dend = (dovals[i] as usize) + data.init.len();
        //10.i
        if dend > meminst.data.len() {
            return Err(Error::ExecuteError);
        }
    }
    //11
    //12
    let entry = config.thread.stack.pop();
    match entry {
        Some(Entry::activation(Activation { n: _, frame: _ })) => {}
        _ => return Err(Error::ExecuteError),
    }
    //13
    for (i, elem) in module.elem.iter().enumerate() {
        //13.a
        for (j, funcidx) in elem.init.iter().enumerate() {
            //13.a.i
            //13.a.ii
            let funcaddr = moduleinst.borrow().funcaddrs[*funcidx as usize];
            //13.a.iii
            config.store.borrow_mut().tables[i].elem[eovals[i] as usize + j] = Some(funcaddr);
        }
    }
    //14
    for (i, data) in module.data.iter().enumerate() {
        //14.a
        for (j, b) in data.init.iter().enumerate() {
            //14.a.i
            config.store.borrow_mut().mems[i].data[dovals[i] as usize + j] = *b;
        }
    }
    //15
    let mut vals: Vec<Val> = vec![];
    if let Some(structure::Start { func }) = module.start {
        //15.a
        //15.b
        let funcaddr = moduleinst.borrow().funcaddrs[func as usize];
        //15.
        let mut instrs = vec![Instr::invoke(funcaddr)];
        reduce_to_end(&mut config, &mut instrs);

        let funcinst = &config.store.borrow().funcs[funcaddr as usize];
        let functype = match funcinst {
            FuncInst::func {
                r#type,
                module: _,
                code: _,
            } => r#type.clone(),
            FuncInst::hostfunc {
                r#type,
                hostcode: _,
            } => r#type.clone(),
        };
        let m = functype.results.len();
        for _ in 0..m {
            let entry = config.thread.stack.pop();
            match entry {
                Some(Entry::val(val)) => vals.insert(0, val),
                _ => return Err(Error::ExecuteError),
            }
        }
    }

    Ok((Rc::clone(&config.thread.frame), vals))
}

//4.5.5
fn invoke(
    S: Rc<RefCell<Store>>,
    moduleinst: Rc<RefCell<ModuleInst>>,
    funcaddr: FuncAddr,
    vals: Vec<Val>,
) -> Result<(Config, Vec<Val>), Error> {
    //1
    //2
    let funcinst = &S.borrow().funcs[funcaddr as usize];
    //3
    let structure::FuncType { params, results } = match funcinst {
        FuncInst::func {
            r#type,
            module: _,
            code: _,
        } => r#type,
        FuncInst::hostfunc {
            r#type,
            hostcode: _,
        } => r#type,
    };
    let n = params.len();
    let m = results.len();
    //4
    if vals.len() != n {
        return Err(Error::ExecuteError);
    }
    //5
    for i in 0..n {
        let t = &params[i];
        let val = &vals[i];
        match t {
            structure::ValType::r#i32 => match val {
                Val::i32_const(_) => {}
                _ => return Err(Error::ExecuteError), //5.a.i
            },
            structure::ValType::r#i64 => match val {
                Val::i64_const(_) => {}
                _ => return Err(Error::ExecuteError), //5.a.i
            },
            structure::ValType::r#f32 => match val {
                Val::f32_const(_) => {}
                _ => return Err(Error::ExecuteError), //5.a.i
            },
            structure::ValType::r#f64 => match val {
                Val::f64_const(_) => {}
                _ => return Err(Error::ExecuteError), //5.a.i
            },
        }
    }
    //6
    let F = Rc::new(RefCell::new(Frame {
        locals: vec![],
        module: Rc::clone(&moduleinst),
    }));
    //7
    let mut config = Config {
        store: Rc::clone(&S),
        thread: Thread {
            frame: Rc::clone(&F),
            stack: Default::default(),
        },
    };
    config.thread.stack.push(Entry::activation(Activation {
        n: 0,
        frame: Rc::clone(&F),
    }));
    //8
    for val in vals.into_iter() {
        config.thread.stack.push(Entry::val(val));
    }
    //9
    let mut instrs = vec![Instr::invoke(funcaddr)];
    reduce_to_end(&mut config, &mut instrs);

    //1
    //2
    let mut retvals: Vec<Val> = vec![];
    for _ in 0..m {
        let entry = config.thread.stack.pop();
        match entry {
            Some(Entry::val(val)) => {
                retvals.insert(0, val);
            }
            _ => return Err(Error::ExecuteError),
        }
    }
    Ok((config, retvals))
}
fn invoke_hostfunc(
    S: Rc<RefCell<Store>>,
    F: Rc<RefCell<Frame>>,
    hostcode: HostFunc,
    vals: &Vec<Val>,
) -> Result<Vec<Val>, Error> {
    match hostcode {
        0 => {
            // puts
            let meminst = &S.borrow().mems[0];
            match vals[0] {
                Val::i32_const(offset) => {
                    let ret = unsafe {
                        let cstring = std::ffi::CString::from_vec_unchecked(
                            meminst.data[offset as usize..].to_vec(),
                        );
                        libc::puts(cstring.as_ptr())
                    };
                    return Ok(vec![Val::i32_const(ret)]);
                }
                _ => {
                    return Err(Error::ExecuteError);
                }
            }
        }
        _ => Err(Error::ExecuteError),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Read;

    #[test]
    fn test_module_instantiate() {
        let mut f = std::fs::File::open("resources/test.wasm").unwrap();
        let mut v = Vec::new();
        f.read_to_end(&mut v).unwrap();
        let module = crate::module_decode(v).unwrap();
        let mut S = crate::store_init();

        // host func
        let mut funcaddrs: Vec<FuncAddr> = vec![];
        for i in 0..module.imports.len() {
            let import = &module.imports[i];
            match import.desc {
                structure::ImportDesc::func(typeidx) => {
                    let functype = &module.types[typeidx as usize];
                    let funcaddr = allochostfunc(Rc::clone(&S), &functype, i);
                    funcaddrs.push(funcaddr);
                }
                _ => {}
            }
        }

        let (F, vals) = instantiate(Rc::clone(&S), &module, vec![ExternVal::func(0)]).unwrap();
        assert_eq!(vals.len(), 0);
        let module = &F.borrow().module;
        let exports = &module.borrow().exports;
        let func = exports
            .iter()
            .find(|exportinst| exportinst.name == "main")
            .unwrap();
        match func.value {
            ExternVal::func(funcaddr) => {
                let (_, vals) = invoke(
                    Rc::clone(&S),
                    Rc::clone(&F.borrow().module),
                    funcaddr,
                    vec![Val::i32_const(0), Val::i32_const(0)],
                )
                .unwrap();
                println!("vals=>{:#?}", vals);
                assert_eq!(vals.len(), 1);
                assert_eq!(vals[0], Val::i32_const(0));
            }
            _ => assert!(false),
        }
    }
}
