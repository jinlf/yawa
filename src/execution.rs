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

pub fn store_init() -> Store {
    Default::default()
}

// 7.1.5 Modules
pub fn module_instantiate(
    S: &mut Store,
    m: &structure::Module,
    evs: Vec<ExternVal>,
) -> Result<Rc<RefCell<ModuleInst>>, Error> {
    let (F, instrs) = instantiate(S, m, evs)?;

    if instrs.len() == 0 {
        Ok(Rc::clone(&F.borrow().module))
    } else {
        Err(Error::ExecuteError)
    }
}

//4.5.4
fn instantiate(
    S: &mut Store,
    module: &structure::Module,
    externvals: Vec<ExternVal>,
) -> Result<(Rc<RefCell<Frame>>, Vec<Instr>), Error> {
    //1
    crate::validation::module_validate(module)?;
    //2
    for m in 0..module.imports.len() {
        let import = &module.imports[m];
        match import.desc {
            structure::ImportDesc::func(_) => match externvals[m] {
                ExternVal::func(_) => {}
                _ => return Err(Error::ExecuteError),
            },
            structure::ImportDesc::table(_) => match externvals[m] {
                ExternVal::table(_) => {}
                _ => return Err(Error::ExecuteError),
            },
            structure::ImportDesc::mem(_) => match externvals[m] {
                ExternVal::mem(_) => {}
                _ => return Err(Error::ExecuteError),
            },
            structure::ImportDesc::global(_) => match externvals[m] {
                ExternVal::global(_) => {}
                _ => return Err(Error::ExecuteError),
            },
        }
    }
    //3
    let m = module.imports.len();
    let n = externvals.len();
    if m != n {
        return Err(Error::ExecuteError);
    }

    for i in 0..n {
        let externval = &externvals[i];
        match &module.imports[i].desc {
            structure::ImportDesc::func(typeidx) => match externval {
                ExternVal::func(funcaddr) => {
                    match &S.funcs[*funcaddr as usize] {
                        FuncInst::func {
                            r#type,
                            module: _,
                            code: _,
                        } => {
                            if r#type != &module.types[*typeidx as usize] {
                                //4.a
                                return Err(Error::ExecuteError);
                            }
                        }
                        FuncInst::hostfunc {
                            r#type,
                            hostcode: _,
                        } => {
                            if r#type != &module.types[*typeidx as usize] {
                                //4.a
                                return Err(Error::ExecuteError);
                            }
                        }
                        _ => return Err(Error::ExecuteError), //4.b
                    }
                }
                _ => return Err(Error::ExecuteError), //4.b
            },
            structure::ImportDesc::table(tabletype) => match externval {
                ExternVal::table(tableaddr) => {
                    match &S.tables[*tableaddr as usize] {
                        TableInst { elem, max } => {
                            if elem.len() != tabletype.limits.min as usize
                                || max != &tabletype.limits.max
                            {
                                return Err(Error::ExecuteError); //4.a
                            }
                        }
                        _ => return Err(Error::ExecuteError), //4.b
                    }
                }
                _ => return Err(Error::ExecuteError), //4.b
            },
            structure::ImportDesc::mem(memtype) => match externval {
                ExternVal::mem(memaddr) => {
                    match &S.mems[*memaddr as usize] {
                        MemInst { data, max } => {
                            if data.len() != memtype.limits.min as usize * 64 * 1024
                                || max != &memtype.limits.max
                            {
                                return Err(Error::ExecuteError); //4.a
                            }
                        }
                        _ => return Err(Error::ExecuteError), //4.b
                    }
                }
                _ => return Err(Error::ExecuteError), //4.b
            },
            structure::ImportDesc::global(globaltype) => match externval {
                ExternVal::global(globaladdr) => match &S.globals[*globaladdr as usize] {
                    GlobalInst { value, r#mut } => {
                        if &globaltype.r#mut != r#mut {
                            return Err(Error::ExecuteError); //4.a
                        }
                        match globaltype.valtype {
                            structure::ValType::i32 => match value {
                                Val::i32_const(_) => {}
                                _ => return Err(Error::ExecuteError), //4.a
                            },
                            structure::ValType::i64 => match value {
                                Val::i64_const(_) => {}
                                _ => return Err(Error::ExecuteError), //4.a
                            },
                            structure::ValType::f32 => match value {
                                Val::f32_const(_) => {}
                                _ => return Err(Error::ExecuteError), //4.a
                            },
                            structure::ValType::f64 => match value {
                                Val::f64_const(_) => {}
                                _ => return Err(Error::ExecuteError), //4.a
                            },
                        }
                    }
                    _ => return Err(Error::ExecuteError), //4.b
                },
                _ => return Err(Error::ExecuteError), //4.b
            },
        }
    }
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
    let F_im = Rc::new(RefCell::new(Frame::default()));
    F_im.borrow_mut().module = Rc::clone(&moduleinst_im);
    let mut thread: Thread = Default::default();
    thread.frame = Rc::clone(&F_im);
    //5.c
    thread.stack.push(Entry::activation(Activation {
        n: 0,
        frame: Rc::clone(&F_im),
    }));
    //5.d
    let mut vals: Vec<Val> = vec![];
    for i in 0..module.globals.len() {
        //5.d.i
        let global = &module.globals[i];
        thread.instrs = global
            .init
            .instrs
            .iter()
            .map(|x| Instr::instr(x.clone()))
            .collect();
        thread.reduce_to_end(S);
        if let Some(Entry::val(_)) = thread.stack.last() {
            match thread.stack.pop() {
                Some(Entry::val(val)) => {
                    vals.push(val);
                }
                _ => {}
            }
        }
    }
    //5.e
    //5.f
    thread.stack.pop();

    //6
    let moduleinst = allocmodule(S, module, externvals, &vals);
    //7
    let F = Rc::new(RefCell::new(Frame {
        module: Rc::clone(&moduleinst),
        locals: vec![],
    }));
    //8
    thread.stack.push(Entry::activation(Activation {
        n: 0,
        frame: Rc::clone(&F),
    }));
    //9
    let mut eos = vec![];
    for i in 0..module.elem.len() {
        let elem = &module.elem[i];
        //9.a
        thread.instrs = elem
            .offset
            .instrs
            .iter()
            .map(|x| Instr::instr(x.clone()))
            .collect();
        thread.reduce_to_end(S);
        if let Some(Entry::val(_)) = thread.stack.last() {
            match thread.stack.pop() {
                Some(Entry::val(eoval)) => {
                    match eoval {
                        Val::i32_const(eo) => {
                            eos.push(eo);
                        }
                        _ => return Err(Error::ExecuteError), //9.b
                    }
                }
                _ => {}
            }
            //9.c
            let tableidx = elem.table;
            //9.d
            //9.e
            let tableaddr = moduleinst.borrow().tableaddrs[tableidx as usize];
            //9.f
            //9.g
            let tableinst = &S.tables[tableaddr as usize];
            //9.h
            let eend = (eos[i] as usize) + elem.init.len();
            //9.i
            if eend > tableinst.elem.len() {
                return Err(Error::ExecuteError);
            }
        }
    }
    //10
    let mut dos = vec![];
    for i in 0..module.data.len() {
        //10.a
        let data = &module.data[i];
        thread.instrs = data
            .offset
            .instrs
            .iter()
            .map(|x| Instr::instr(x.clone()))
            .collect();
        thread.reduce_to_end(S);
        //10.b
        if let Some(Entry::val(doval)) = thread.stack.last() {
            match *doval {
                Val::i32_const(r#do) => dos.push(r#do),
                _ => return Err(Error::ExecuteError), //10.b
            }
            thread.stack.pop();
        }
        //10.c
        let memidx = data.data;
        //10.d
        //10.e
        let memaddr = moduleinst.borrow().memaddrs[memidx as usize];
        //10.f
        //10.g
        let meminst = &S.mems[memaddr as usize];
        //10.h
        let dend = (dos[i] as usize) + data.init.len();
        //10.i
        if dend > meminst.data.len() {
            return Err(Error::ExecuteError);
        }
    }
    //11
    //12
    thread.stack.pop();
    //13
    for i in 0..module.elem.len() {
        let elem = &module.elem[i];
        //13.a
        for j in 0..elem.init.len() {
            let funcidx = elem.init[j];
            //13.a.i
            //13.a.ii
            let funcaddr = moduleinst.borrow().funcaddrs[funcidx as usize];
            //13.a.iii
            S.tables[i].elem[eos[i] as usize + j] = Some(funcaddr);
        }
    }
    //14
    for i in 0..module.data.len() {
        let data = &module.data[i];
        //14.a
        for j in 0..data.init.len() {
            let b = data.init[j];
            //14.a.i
            S.mems[i].data[dos[i] as usize + j] = b;
        }
    }

    //15
    if let Some(structure::Start { func }) = module.start {
        //15.a
        //15.b
        let funcaddr = moduleinst.borrow().funcaddrs[func as usize];
        //15.
        thread.instrs = vec![Instr::invoke(funcaddr)];
        thread.reduce_to_end(S);
    }

    Ok((Rc::clone(&F), thread.instrs))
}
//4.5.3
fn allocmodule(
    S: &mut Store,
    module: &structure::Module,
    externvals_im: Vec<ExternVal>,
    vals: &Vec<Val>,
) -> Rc<RefCell<ModuleInst>> {
    let moduleinst = Rc::new(RefCell::new(ModuleInst::default()));
    //1
    moduleinst.borrow_mut().types = module.types.clone();
    //2
    let mut funcaddrs: Vec<FuncAddr> = module
        .funcs
        .iter()
        .map(|func| allocfunc(S, func, Rc::clone(&moduleinst)))
        .collect();
    //3
    let mut tableaddrs: Vec<TableAddr> = module
        .tables
        .iter()
        .map(|table| alloctable(S, &table.r#type))
        .collect();
    //4
    let mut memaddrs: Vec<MemAddr> = module
        .mems
        .iter()
        .map(|mem| allocmem(S, &mem.r#type))
        .collect();
    //5
    let mut globaladdrs: Vec<GlobalAddr> = module
        .globals
        .iter()
        .enumerate()
        .map(|(i, global)| allocglobal(S, &global.r#type, &vals[i]))
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

fn allocfunc(
    S: &mut Store,
    func: &structure::Func,
    moduleinst: Rc<RefCell<ModuleInst>>,
) -> FuncAddr {
    //1
    //2
    let funcaddr = S.funcs.len();
    //3
    let functype = &moduleinst.borrow().types[func.r#type as usize];
    //4
    let funcinst = FuncInst::func {
        r#type: functype.clone(),
        module: Rc::clone(&moduleinst),
        code: func.clone(),
    };
    //5
    S.funcs.push(funcinst);
    //6
    funcaddr
}

pub fn func_alloc(S: &mut Store, ft: &structure::FuncType, code: HostFunc) -> FuncAddr {
    allochostfunc(S, ft, code)
}
fn allochostfunc(S: &mut Store, functype: &structure::FuncType, hostfunc: HostFunc) -> FuncAddr {
    //1
    //2
    let funcaddr = S.funcs.len();
    //3
    let funcinst = FuncInst::hostfunc {
        r#type: functype.clone(),
        hostcode: hostfunc,
    };
    //4
    S.funcs.push(funcinst);
    //5
    funcaddr
}
pub fn table_alloc(S: &mut Store, tt: structure::TableType) -> TableAddr {
    alloctable(S, &tt)
}
fn alloctable(S: &mut Store, tabletype: &structure::TableType) -> TableAddr {
    //1
    //2
    let structure::TableType { limits, elemtype } = tabletype;
    //3
    let tableaddr = S.tables.len();
    //4
    let tableinst = TableInst {
        elem: vec![],
        max: limits.max,
    };
    //5
    S.tables.push(tableinst);
    //6
    tableaddr
}
pub fn mem_alloc(S: &mut Store, mt: structure::MemType) -> MemAddr {
    allocmem(S, &mt)
}
fn allocmem(S: &mut Store, memtype: &structure::MemType) -> MemAddr {
    //1
    //2
    let structure::MemType { limits } = memtype;
    //3
    let memaddr = S.mems.len();
    //4
    let meminst = MemInst {
        data: vec![0; limits.min as usize * 64 * 1024],
        max: limits.max,
    };
    //5
    S.mems.push(meminst);
    //6
    memaddr
}
pub fn global_alloc(S: &mut Store, gt: structure::GlobalType, v: Val) -> GlobalAddr {
    allocglobal(S, &gt, &v)
}
fn allocglobal(S: &mut Store, globaltype: &structure::GlobalType, val: &Val) -> GlobalAddr {
    //1
    //2
    let structure::GlobalType { r#mut, valtype: _ } = globaltype;
    //3
    let globaladdr = S.globals.len();
    //4
    let globalinst = GlobalInst {
        value: val.clone(),
        r#mut: r#mut.clone(),
    };
    //5
    S.globals.push(globalinst);
    //6
    globaladdr
}

// 7.1.6 Module Instances
pub fn instance_export(
    moduleinst: Rc<RefCell<ModuleInst>>,
    name: &str,
) -> Result<ExternVal, Error> {
    //1
    //2
    for exportinst in moduleinst.borrow().exports.iter() {
        if exportinst.name == name {
            return Ok(exportinst.value.clone());
        }
    }
    Err(Error::ExecuteError)
}
// 7.1.7 Functions
pub fn func_invoke(S: &mut Store, funcaddr: FuncAddr, vals: Vec<Val>) -> Result<Vec<Val>, Error> {
    let mut vs = vals;

    let (F, vs1) = invoke(S, funcaddr, vs)?;
    vs = vs1;
    Ok(vs)
}
fn invoke(
    S: &mut Store,
    funcaddr: FuncAddr,
    vals: Vec<Val>,
) -> Result<(Rc<RefCell<Frame>>, Vec<Val>), Error> {
    //1
    //2
    let funcinst = &S.funcs[funcaddr as usize];
    //3
    let mut funcmodule = Rc::new(RefCell::new(ModuleInst::default()));
    let structure::FuncType { params, results } = match funcinst {
        FuncInst::func {
            r#type,
            module,
            code,
        } => {
            funcmodule = Rc::clone(&module);
            r#type
        }
        FuncInst::hostfunc { r#type, hostcode } => r#type,
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
            structure::ValType::i32 => match val {
                Val::i32_const(c) => {}
                _ => return Err(Error::ExecuteError), //5.a.i
            },
            structure::ValType::i64 => match val {
                Val::i64_const(c) => {}
                _ => return Err(Error::ExecuteError), //5.a.i
            },
            structure::ValType::f32 => match val {
                Val::f32_const(c) => {}
                _ => return Err(Error::ExecuteError), //5.a.i
            },
            structure::ValType::f64 => match val {
                Val::f64_const(c) => {}
                _ => return Err(Error::ExecuteError), //5.a.i
            },
        }
    }
    //6
    let F = Rc::new(RefCell::new(Frame {
        locals: vec![],
        module: Rc::clone(&funcmodule),
    }));
    //7
    let mut thread: Thread = Default::default();
    thread.frame = Rc::clone(&F);
    thread.stack.push(Entry::activation(Activation {
        n: 0,
        frame: Rc::clone(&F),
    }));
    //8
    for val in vals.into_iter() {
        thread.stack.push(Entry::val(val));
    }
    //9
    thread.instrs = vec![Instr::invoke(funcaddr)];
    thread.reduce_to_end(S);

    //1
    //2
    // let len = thread.stack.len();
    let mut retvals: Vec<Val> = vec![];
    for _ in 0..m {
        match thread.stack.pop() {
            Some(Entry::val(val)) => {
                retvals.insert(0, val);
            }
            _ => return Err(Error::ExecuteError),
        }
    }
    Ok((Rc::clone(&F), retvals))
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
    pub fn new() -> Self {
        Self {
            funcs: vec![],
            tables: vec![],
            mems: vec![],
            globals: vec![],
        }
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
#[derive(Clone, Debug, Default)]
pub struct Frame {
    pub locals: Vec<Val>,
    pub module: Rc<RefCell<ModuleInst>>,
}
impl PartialEq for Frame {
    fn eq(&self, _: &Self) -> bool {
        true
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
        n: u32,
        funcidxes: Vec<structure::FuncIdx>,
    },
    init_data {
        memaddr: MemAddr,
        n: u32,
        bytes: Vec<u8>,
    },
    label {
        n: u32,
        instrs: Vec<Box<Instr>>,
        others: Vec<Box<Instr>>,
    },
    frame {
        n: u32,
        frame: Frame,
        instrs: Vec<Box<Instr>>,
    },
}
pub struct Config {
    store: Store,
    thread: Thread,
}
#[derive(Default)]
pub struct Thread {
    pub frame: Rc<RefCell<Frame>>,
    pub instrs: Vec<Instr>,
    stack: Stack,
}

/// ## Evalaution Contexts

/// ## Reduce
impl Thread {
    pub fn reduce_to_end(&mut self, S: &mut Store) {
        loop {
            if self.reduce(S) == false {
                return;
            }
        }
    }
    pub fn reduce(&mut self, S: &mut Store) -> bool {
        if self.instrs.len() == 0 {
            return false;
        }
        match self.instrs.remove(0) {
            Instr::instr(instr) => match instr {
                structure::Instr::end => {
                    let mut vals: Vec<Val> = vec![];
                    loop {
                        //2
                        match self.stack.pop() {
                            Some(Entry::val(val)) => {
                                vals.insert(0, val);
                            }
                            Some(Entry::label(label)) => {
                                // Exiting instr* with label L
                                //1
                                let m = vals.len();
                                //3
                                //4
                                for _ in 0..m {
                                    self.stack.push(Entry::val(vals.remove(0)));
                                }
                                self.instrs = label
                                    .instrs
                                    .iter()
                                    .cloned()
                                    .chain(self.instrs.drain(..))
                                    .collect();
                                return true;
                            }
                            Some(Entry::activation(activation)) => {
                                // Returning from a function
                                //1
                                //2
                                let Activation { n, frame } = activation;
                                self.frame = Rc::clone(&frame);
                                //3
                                //4
                                //5
                                //6
                                //7
                                for _ in 0..vals.len() {
                                    self.stack.push(Entry::val(vals.remove(0)));
                                }
                                return true;
                            }
                            _ => {
                                self.instrs.insert(0, Instr::trap);
                                return true;
                            }
                        }
                    }
                }
                structure::Instr::drop => {
                    self.stack.pop();
                    return true;
                }
                structure::Instr::i32_const(v) => {
                    self.stack.push(Entry::val(Val::i32_const(v)));
                    return true;
                }
                structure::Instr::local_get(x) => {
                    //1
                    //2
                    //3
                    let val = self.frame.borrow().locals[x as usize].clone();
                    self.stack.push(Entry::val(val));
                    return true;
                }
                structure::Instr::call(x) => {
                    let a = self.frame.borrow().module.borrow().funcaddrs[x as usize];
                    self.instrs.insert(0, Instr::invoke(a));
                    return true;
                }

                _ => {
                    println!("unsupported instr: {:#?}", self.instrs[0]);
                    return false;
                }
            },
            Instr::invoke(a) => {
                //1
                //2
                let f = &S.funcs[a as usize];
                //3
                match f {
                    FuncInst::func {
                        r#type,
                        module,
                        code,
                    } => {
                        let structure::FuncType { params, results } = r#type;
                        let n = params.len();
                        let m = results.len();
                        //4
                        let ts = &code.locals;
                        //5
                        self.instrs = code
                            .body
                            .instrs
                            .iter()
                            .map(|x| Instr::instr(x.clone()))
                            .collect::<Vec<Instr>>()
                            .into_iter()
                            .chain(self.instrs.drain(..))
                            .collect();
                        //6
                        //7
                        let mut vals: Vec<Val> = vec![];
                        for _ in 0..n {
                            match self.stack.pop() {
                                Some(Entry::val(val)) => {
                                    vals.insert(0, val);
                                }
                                _ => {
                                    self.instrs.insert(0, Instr::trap);
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
                            module: Rc::clone(&self.frame.borrow().module),
                            locals: vals,
                        }));
                        //10
                        self.frame = Rc::clone(&newF);
                        self.stack.push(Entry::activation(Activation {
                            n: m,
                            frame: Rc::clone(&newF),
                        }));
                        //11
                        self.stack.push(Entry::label(Label {
                            n: m,
                            instrs: vec![],
                        }));
                        //12
                        self.instrs.push(Instr::instr(structure::Instr::end));
                        self.instrs.push(Instr::instr(structure::Instr::end));
                        return true;
                    }
                    FuncInst::hostfunc { r#type, hostcode } => {
                        let structure::FuncType { params, results } = r#type;
                        let n = params.len();
                        let m = results.len();
                        let mut vals: Vec<Val> = vec![];
                        for _ in 0..n {
                            match self.stack.pop() {
                                Some(Entry::val(val)) => {
                                    vals.insert(0, val);
                                }
                                _ => {
                                    self.instrs.insert(0, Instr::trap);
                                    return true;
                                }
                            }
                        }
                        match invoke_hostfunc(S, Rc::clone(&self.frame), *hostcode, &vals) {
                            Ok(retvals) => {
                                for retval in retvals.into_iter() {
                                    self.stack.push(Entry::val(retval));
                                }
                            }
                            Err(_) => {
                                self.instrs.insert(0, Instr::trap);
                                return true;
                            }
                        }
                        return true;
                    }
                };
            }
            _ => todo!(),
        }
        true
    }
}

fn invoke_hostfunc(
    S: &mut Store,
    F: Rc<RefCell<Frame>>,
    hostcode: HostFunc,
    vals: &Vec<Val>,
) -> Result<Vec<Val>, Error> {
    match hostcode {
        0 => {
            // puts
            let meminst = &S.mems[0];
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
        let mut f = std::fs::File::open("resources/hello_world.wasm").unwrap();
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
                    let funcaddr = func_alloc(&mut S, &functype, i);
                    funcaddrs.push(funcaddr);
                }
                _ => {}
            }
        }

        let moduleinst = module_instantiate(&mut S, &module, vec![ExternVal::func(0)]).unwrap();
        let externval = instance_export(Rc::clone(&moduleinst), "main").unwrap();
        match externval {
            ExternVal::func(funcaddr) => {
                let vals =
                    func_invoke(&mut S, funcaddr, vec![Val::i32_const(0), Val::i32_const(0)])
                        .unwrap();
                println!("vals=>{:#?}", vals);
                assert_eq!(vals.len(), 1);
                assert_eq!(vals[0], Val::i32_const(0));
            }
            _ => {}
        }
    }
}
