use crate::structure;
use crate::validation;
use libc;
use std::cell::RefCell;
use std::convert::TryInto;
use std::fmt::Debug;
use std::rc::Rc;

macro_rules! process_load_instr {
    (i32, $memarg:ident, $config:ident, $instrs:ident) => {{
        match load_N_sx(structure::ValType::r#i32, None, $memarg, $config, $instrs)? {
            Val::i32_const(c) => $config.thread.stack.push(Entry::val(Val::i32_const(c))),
            _ => return Err(Error::ExecuteError(line!())),
        }
        return Ok(true);
    }};
    (i64, $memarg:ident, $config:ident, $instrs:ident) => {{
        match load_N_sx(structure::ValType::r#i64, None, $memarg, $config, $instrs)? {
            Val::i64_const(c) => $config.thread.stack.push(Entry::val(Val::i64_const(c))),
            _ => return Err(Error::ExecuteError(line!())),
        }
        return Ok(true);
    }};
    (f32, $memarg:ident, $config:ident, $instrs:ident) => {{
        match load_N_sx(structure::ValType::r#f32, None, $memarg, $config, $instrs)? {
            Val::f32_const(c) => $config.thread.stack.push(Entry::val(Val::f32_const(c))),
            _ => return Err(Error::ExecuteError(line!())),
        }
        return Ok(true);
    }};
    (f64, $memarg:ident, $config:ident, $instrs:ident) => {{
        match load_N_sx(structure::ValType::r#f64, None, $memarg, $config, $instrs)? {
            Val::f64_const(c) => $config.thread.stack.push(Entry::val(Val::f64_const(c))),
            _ => return Err(Error::ExecuteError(line!())),
        }
        return Ok(true);
    }};
    (i32, $e: expr, $s: expr, $memarg:ident, $config:ident, $instrs:ident) => {{
        match load_N_sx(
            structure::ValType::r#i32,
            Some(($e, $s)),
            $memarg,
            $config,
            $instrs,
        )? {
            Val::i32_const(c) => $config.thread.stack.push(Entry::val(Val::i32_const(c))),
            _ => return Err(Error::ExecuteError(line!())),
        }
        return Ok(true);
    }};
    (i64, $e: expr, $s: expr, $memarg:ident, $config:ident, $instrs:ident) => {{
        match load_N_sx(
            structure::ValType::r#i64,
            Some(($e, $s)),
            $memarg,
            $config,
            $instrs,
        )? {
            Val::i64_const(c) => $config.thread.stack.push(Entry::val(Val::i64_const(c))),
            _ => return Err(Error::ExecuteError(line!())),
        }
        return Ok(true);
    }};
}

#[derive(Debug)]
pub enum Error {
    ExecuteError(u32),
    ValidateError(String),
    NotImplemented(Instr),
    TryFromSliceError(std::array::TryFromSliceError),
    TrapError(String),
}
impl From<validation::Error> for Error {
    fn from(e: validation::Error) -> Self {
        match e {
            validation::Error::ValidateError(msg) => Self::ValidateError(msg),
        }
    }
}
impl From<std::array::TryFromSliceError> for Error {
    fn from(e: std::array::TryFromSliceError) -> Self {
        Self::TryFromSliceError(e)
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
#[derive(Debug)]
pub struct ModuleInst {
    pub types: Vec<structure::FuncType>,
    pub funcaddrs: Vec<FuncAddr>,
    pub tableaddrs: Vec<TableAddr>,
    pub memaddrs: Vec<MemAddr>,
    pub globaladdrs: Vec<GlobalAddr>,
    pub exports: Vec<ExportInst>,
}
impl ModuleInst {
    pub fn new() -> Self {
        Self {
            types: vec![],
            funcaddrs: vec![],
            tableaddrs: vec![],
            memaddrs: vec![],
            globaladdrs: vec![],
            exports: vec![],
        }
    }
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
    pub name: structure::Name,
    pub value: ExternVal,
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
impl Frame {
    fn new(module: Rc<RefCell<ModuleInst>>) -> Self {
        Self {
            locals: vec![],
            module: Rc::clone(&module),
        }
    }
}
impl PartialEq for Frame {
    fn eq(&self, other: &Self) -> bool {
        self.locals == other.locals //TODO
    }
}
fn expand_blocktype(
    F: Rc<RefCell<Frame>>,
    blocktype: &structure::BlockType,
) -> structure::FuncType {
    match blocktype {
        structure::BlockType::typeidx(typeidx) => expand_typeidx(Rc::clone(&F), typeidx),
        structure::BlockType::valtype(valtype) => expand_valtypes(valtype),
    }
}
fn expand_typeidx(F: Rc<RefCell<Frame>>, typeidx: &structure::TypeIdx) -> structure::FuncType {
    F.borrow().module.borrow().types[*typeidx as usize].clone()
}
fn expand_valtypes(valtype: &Option<structure::ValType>) -> structure::FuncType {
    structure::FuncType {
        params: vec![],
        results: valtype.into_iter().cloned().collect(),
    }
}

//4.2.13
#[derive(Clone, Debug)]
pub enum Instr {
    instr(structure::Instr),
    trap(String),
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
impl Config {
    pub fn new(store: Rc<RefCell<Store>>, moduleinst: Rc<RefCell<ModuleInst>>) -> Self {
        Self {
            store: Rc::clone(&store),
            thread: Thread::new(Rc::new(RefCell::new(Frame::new(Rc::clone(&moduleinst))))),
        }
    }
}
pub struct Thread {
    pub frame: Rc<RefCell<Frame>>,
    stack: Stack,
}
impl Thread {
    fn new(frame: Rc<RefCell<Frame>>) -> Self {
        Self {
            frame: Rc::clone(&frame),
            stack: Stack::new(),
        }
    }
}
fn reduce_to_end(config: &mut Config, instrs: &mut Vec<Instr>) -> Result<(), Error> {
    loop {
        if reduce(config, instrs)? == false {
            return Ok(());
        }
    }
}
fn reduce(config: &mut Config, instrs: &mut Vec<Instr>) -> Result<bool, Error> {
    if instrs.len() == 0 {
        return Ok(false);
    }
    let instr = instrs.remove(0);
    match instr {
        Instr::trap(msg) => Err(Error::TrapError(format!("{}", msg))),
        Instr::instr(instr) => match instr {
            structure::Instr::end => {
                let mut vals: Vec<Entry> = vec![];
                loop {
                    //2
                    let entry = config.thread.stack.pop();
                    match entry {
                        Some(Entry::val(val)) => {
                            vals.insert(0, Entry::val(val));
                        }
                        Some(Entry::label(_)) => {
                            //4.4.6 Blocks
                            // Exiting instr* with label L
                            //1
                            let m = vals.len();
                            //3
                            //4
                            config.thread.stack.append(&mut vals);
                            return Ok(true);
                        }
                        Some(Entry::activation(activation)) => {
                            // Returning from a function
                            //1
                            //2
                            let Activation { n, frame: F } = activation;
                            if n != vals.len() {
                                return Err(Error::ExecuteError(line!()));
                            }
                            config.thread.frame = Rc::clone(&F);
                            //3
                            //4
                            //5
                            //6
                            //7
                            config.thread.stack.append(&mut vals);
                            return Ok(true);
                        }
                        _ => {
                            instrs.insert(0, Instr::trap(format!("{}", line!())));
                            return Ok(true);
                        }
                    }
                }
            }
            //4.4.1 Numeric Instructions
            structure::Instr::i32_const(v) => {
                config.thread.stack.push(Entry::val(Val::i32_const(v)));
                return Ok(true);
            }
            structure::Instr::i64_const(v) => {
                config.thread.stack.push(Entry::val(Val::i64_const(v)));
                return Ok(true);
            }
            structure::Instr::f32_const(v) => {
                config.thread.stack.push(Entry::val(Val::f32_const(v)));
                return Ok(true);
            }
            structure::Instr::f64_const(v) => {
                config.thread.stack.push(Entry::val(Val::f64_const(v)));
                return Ok(true);
            }

            //4.4.2 Parametric Instructions
            structure::Instr::drop => {
                config.thread.stack.pop();
                return Ok(true);
            }
            structure::Instr::select => {
                //1
                //2
                let c = match config.thread.stack.pop() {
                    Some(Entry::val(Val::i32_const(c))) => c,
                    _ => return Err(Error::ExecuteError(line!())),
                };
                //3
                //4
                let val2 = match config.thread.stack.pop() {
                    Some(Entry::val(val)) => val,
                    _ => return Err(Error::ExecuteError(line!())),
                };
                //5
                let val1 = match config.thread.stack.pop() {
                    Some(Entry::val(val)) => val,
                    _ => return Err(Error::ExecuteError(line!())),
                };
                if c != 0 {
                    config.thread.stack.push(Entry::val(val1));
                } else {
                    config.thread.stack.push(Entry::val(val2));
                }
                return Ok(true);
            }
            //4.4.3 Variable Instructions
            structure::Instr::local_get(x) => {
                //1
                //2
                //3
                let val = config.thread.frame.borrow().locals[x as usize].clone();
                config.thread.stack.push(Entry::val(val));
                return Ok(true);
            }
            structure::Instr::local_set(x) => {
                //1
                let mut F = config.thread.frame.borrow_mut();
                //2
                //3
                match config.thread.stack.pop() {
                    Some(Entry::val(val)) => F.locals[x as usize] = val,
                    _ => return Err(Error::ExecuteError(line!())),
                }
                return Ok(true);
            }
            //4.4.4 Memory Instructions
            structure::Instr::i32_load(memarg) => process_load_instr!(i32, memarg, config, instrs),
            structure::Instr::i64_load(memarg) => process_load_instr!(i64, memarg, config, instrs),
            structure::Instr::f32_load(memarg) => process_load_instr!(f32, memarg, config, instrs),
            structure::Instr::f64_load(memarg) => process_load_instr!(f64, memarg, config, instrs),
            structure::Instr::i32_load8_s(memarg) => {
                process_load_instr!(i32, 8, true, memarg, config, instrs)
            }
            structure::Instr::i32_load8_u(memarg) => {
                process_load_instr!(i32, 8, false, memarg, config, instrs)
            }
            structure::Instr::i32_load16_s(memarg) => {
                process_load_instr!(i32, 16, true, memarg, config, instrs)
            }
            structure::Instr::i32_load16_u(memarg) => {
                process_load_instr!(i32, 16, false, memarg, config, instrs)
            }

            structure::Instr::i64_load8_s(memarg) => {
                process_load_instr!(i64, 8, true, memarg, config, instrs)
            }
            structure::Instr::i64_load8_u(memarg) => {
                process_load_instr!(i64, 8, false, memarg, config, instrs)
            }
            structure::Instr::i64_load16_s(memarg) => {
                process_load_instr!(i64, 16, true, memarg, config, instrs)
            }
            structure::Instr::i64_load16_u(memarg) => {
                process_load_instr!(i64, 16, false, memarg, config, instrs)
            }
            structure::Instr::i64_load32_s(memarg) => {
                process_load_instr!(i64, 32, true, memarg, config, instrs)
            }
            structure::Instr::i64_load32_u(memarg) => {
                process_load_instr!(i64, 32, false, memarg, config, instrs)
            }

            structure::Instr::i32_store(memarg) => {
                return Ok(store_N(
                    structure::ValType::r#i32,
                    None,
                    memarg,
                    config,
                    instrs,
                )?)
            }
            structure::Instr::i64_store(memarg) => {
                return Ok(store_N(
                    structure::ValType::r#i64,
                    None,
                    memarg,
                    config,
                    instrs,
                )?)
            }
            structure::Instr::f32_store(memarg) => {
                return Ok(store_N(
                    structure::ValType::r#f32,
                    None,
                    memarg,
                    config,
                    instrs,
                )?)
            }
            structure::Instr::f64_store(memarg) => {
                return Ok(store_N(
                    structure::ValType::r#f64,
                    None,
                    memarg,
                    config,
                    instrs,
                )?)
            }
            structure::Instr::i32_store8(memarg) => {
                return Ok(store_N(
                    structure::ValType::r#i32,
                    Some(8),
                    memarg,
                    config,
                    instrs,
                )?)
            }
            structure::Instr::i64_store8(memarg) => {
                return Ok(store_N(
                    structure::ValType::r#i64,
                    Some(8),
                    memarg,
                    config,
                    instrs,
                )?)
            }
            structure::Instr::i32_store16(memarg) => {
                return Ok(store_N(
                    structure::ValType::r#i32,
                    Some(16),
                    memarg,
                    config,
                    instrs,
                )?)
            }
            structure::Instr::i64_store16(memarg) => {
                return Ok(store_N(
                    structure::ValType::r#i64,
                    Some(16),
                    memarg,
                    config,
                    instrs,
                )?)
            }
            structure::Instr::i64_store32(memarg) => {
                return Ok(store_N(
                    structure::ValType::r#i64,
                    Some(32),
                    memarg,
                    config,
                    instrs,
                )?)
            }

            //4.4.5 Control Instructions
            structure::Instr::nop => return Ok(true),
            structure::Instr::block {
                blocktype,
                instrs: block_instrs,
            } => {
                //1
                //2
                let F = &config.thread.frame;
                let functype = expand_blocktype(Rc::clone(F), &blocktype);
                let m = functype.params.len();
                let n = functype.results.len();
                //3
                let L = Label {
                    n: n,
                    instrs: vec![],
                };
                //4
                //5
                let len = config.thread.stack.len();
                let mut valm = config.thread.stack.split_off(len - m);
                //6
                config.thread.stack.push(Entry::label(L));
                config.thread.stack.append(&mut valm);
                instrs.insert(0, Instr::instr(structure::Instr::end));
                instrs.splice(
                    ..0,
                    block_instrs.into_iter().map(|instr| Instr::instr(instr)),
                );
                return Ok(true);
            }
            structure::Instr::r#loop {
                blocktype,
                instrs: loop_instrs,
            } => {
                //1
                //2
                let F = &config.thread.frame;
                let functype = expand_blocktype(Rc::clone(F), &blocktype);
                let m = functype.params.len();
                let n = functype.results.len();
                //3
                let L = Label {
                    n: m,
                    instrs: vec![Instr::instr(structure::Instr::r#loop {
                        blocktype: blocktype,
                        instrs: loop_instrs.clone(),
                    })],
                };
                //4
                //5
                let len = config.thread.stack.len();
                let mut valm = config.thread.stack.split_off(len - m);
                //6
                config.thread.stack.push(Entry::label(L));
                config.thread.stack.append(&mut valm);
                instrs.insert(0, Instr::instr(structure::Instr::end));
                instrs.splice(
                    ..0,
                    loop_instrs.into_iter().map(|instr| Instr::instr(instr)),
                );
                return Ok(true);
            }
            structure::Instr::r#if {
                blocktype,
                instrs1,
                instrs2,
            } => {
                //1
                //2
                let F = &config.thread.frame;
                let functype = expand_blocktype(Rc::clone(F), &blocktype);
                let m = functype.params.len();
                let n = functype.results.len();
                //3
                let L = Label {
                    n: n,
                    instrs: vec![],
                };
                //4
                //5
                let c = match config.thread.stack.pop() {
                    Some(Entry::val(Val::i32_const(c))) => c,
                    _ => return Err(Error::ExecuteError(line!())),
                };
                //6
                //7
                let len = config.thread.stack.len();
                let mut valm = config.thread.stack.split_off(len - m);
                //8
                config.thread.stack.push(Entry::label(L));
                config.thread.stack.append(&mut valm);
                instrs.insert(0, Instr::instr(structure::Instr::end));
                if c != 0 {
                    instrs.splice(..0, instrs1.into_iter().map(|instr| Instr::instr(instr)));
                } else {
                    instrs.splice(..0, instrs2.into_iter().map(|instr| Instr::instr(instr)));
                }
                return Ok(true);
            }
            structure::Instr::br(labelidx) => {
                //1
                //2
                let mut count = 0;
                let mut L: Option<Label> = None;
                for entry in config.thread.stack.iter().rev() {
                    match entry {
                        Entry::label(label) => {
                            if count == labelidx {
                                L = Some(label.clone());
                                break;
                            }
                            count += 1;
                        }
                        _ => {}
                    }
                }
                let L = L.ok_or(Error::ExecuteError(line!()))?;
                let n = L.n;
                let len = config.thread.stack.len();
                let mut valn = config.thread.stack.split_off(len - n);
                for _ in 0..=labelidx as usize {
                    let pos = config
                        .thread
                        .stack
                        .iter()
                        .rev()
                        .position(|entry| match entry {
                            Entry::val(_) => false,
                            _ => true,
                        })
                        .ok_or(Error::ExecuteError(line!()))?;
                    let len = config.thread.stack.len();
                    config.thread.stack.truncate(len - pos);
                    match config.thread.stack.pop() {
                        Some(Entry::label(_)) => {}
                        _ => return Err(Error::ExecuteError(line!())),
                    }
                }
                match instrs.remove(0) {
                    Instr::instr(structure::Instr::end) => {}
                    _ => return Err(Error::ExecuteError(line!())),
                }
                config.thread.stack.append(&mut valn);
                instrs.splice(..0, L.instrs);

                return Ok(true);
            }
            structure::Instr::br_if(labelidx) => {
                //1
                //2
                let c = match config.thread.stack.pop() {
                    Some(Entry::val(Val::i32_const(c))) => c,
                    _ => return Err(Error::ExecuteError(line!())),
                };
                //3
                if c != 0 {
                    instrs.insert(0, Instr::instr(structure::Instr::br(labelidx)));
                }
                return Ok(true);
            }
            structure::Instr::br_table {
                labelidxes,
                labelidx,
            } => {
                //1
                //2
                match config.thread.stack.pop() {
                    Some(Entry::val(Val::i32_const(i))) => {
                        if (i as usize) < labelidxes.len() {
                            instrs.insert(
                                0,
                                Instr::instr(structure::Instr::br(labelidxes[i as usize])),
                            )
                        } else {
                            instrs.insert(0, Instr::instr(structure::Instr::br(labelidx)))
                        }
                    }
                    _ => return Err(Error::ExecuteError(line!())),
                }
                return Ok(true);
            }
            structure::Instr::call(x) => {
                let a = config.thread.frame.borrow().module.borrow().funcaddrs[x as usize];
                instrs.insert(0, Instr::invoke(a));
                return Ok(true);
            }
            structure::Instr::call_indirect(x) => {
                //1
                let F = &config.thread.frame;
                //2
                //3
                let ta = F.borrow().module.borrow().tableaddrs[0];
                //4
                //5
                let tab = &config.store.borrow().tables[ta as usize];
                //6
                //7
                let ft_expect = F.borrow().module.borrow().types[x as usize].clone();
                //8
                //9
                let i = match config.thread.stack.pop() {
                    Some(Entry::val(Val::i32_const(i))) => i,
                    _ => return Err(Error::ExecuteError(line!())),
                };
                //10
                if (i as usize) >= tab.elem.len() {
                    instrs.insert(0, Instr::trap(format!("{}", line!())));
                    return Ok(true);
                }
                //11
                if tab.elem[i as usize].is_none() {
                    instrs.insert(0, Instr::trap(format!("{}", line!())));
                    return Ok(true);
                }
                //12
                let a = tab.elem[i as usize].unwrap();
                //13
                //14
                let f = &config.store.borrow().funcs[a as usize];
                //15
                let ft_actual = match f {
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
                //16
                if ft_actual != ft_expect {
                    instrs.insert(0, Instr::trap(format!("{}", line!())));
                    return Ok(true);
                }
                //17
                instrs.insert(0, Instr::invoke(a));
                return Ok(true);
            }
            _ => return Err(Error::NotImplemented(Instr::instr(instr))),
        },
        Instr::invoke(a) => {
            //4.4.7 Function Calls
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
                                instrs.insert(0, Instr::trap(format!("{}", line!())));
                                return Ok(true);
                            }
                        }
                    }
                    //8
                    for t in ts.iter() {
                        let val0 = match t {
                            structure::ValType::r#i32 => Val::i32_const(0),
                            structure::ValType::r#i64 => Val::i64_const(0),
                            structure::ValType::r#f32 => Val::f32_const(0.0),
                            structure::ValType::r#f64 => Val::f64_const(0.0),
                        };
                        vals.push(val0);
                    }
                    //9
                    let F = Rc::new(RefCell::new(Frame {
                        module: Rc::clone(&config.thread.frame.borrow().module),
                        locals: vals,
                    }));
                    //10
                    config.thread.frame = Rc::clone(&F);
                    config.thread.stack.push(Entry::activation(Activation {
                        n: m,
                        frame: Rc::clone(&F),
                    }));
                    //11
                    config.thread.stack.push(Entry::label(Label {
                        n: m,
                        instrs: vec![],
                    }));

                    instrs.insert(0, Instr::instr(structure::Instr::end));
                    instrs.insert(0, Instr::instr(structure::Instr::end));
                    let body_instrs: Vec<Instr> = code
                        .body
                        .instrs
                        .iter()
                        .map(|instr| Instr::instr(instr.clone()))
                        .collect();
                    instrs.splice(..0, body_instrs);
                    //12
                    return Ok(true);
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
                                instrs.insert(0, Instr::trap(format!("{}", line!())));
                                return Ok(true);
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
                            instrs.insert(0, Instr::trap(format!("{}", line!())));
                            return Ok(true);
                        }
                    }
                    return Ok(true);
                }
            };
        }
        _ => return Err(Error::NotImplemented(instr)),
    }
}
fn load_N_sx(
    t: structure::ValType,
    N_sx: Option<(usize, bool)>,
    memarg: structure::MemArg,
    config: &mut Config,
    instrs: &mut Vec<Instr>,
) -> Result<Val, Error> {
    //1
    let F = Rc::clone(&config.thread.frame);
    //2
    //3
    let a = F.borrow().module.borrow().memaddrs[0];
    //4
    //5
    let mem = &config.store.borrow().mems[a];
    //6
    //7
    let i = match config.thread.stack.pop() {
        Some(Entry::val(Val::i32_const(i))) => i,
        _ => return Err(Error::ExecuteError(line!())),
    };
    //8
    let (ea, overflow) = (i as u32).overflowing_add(memarg.offset);
    if overflow {
        instrs.insert(0, Instr::trap(String::from("out of bounds memory access")));
        return match t {
            structure::ValType::r#i32 => Ok(Val::i32_const(0)),
            structure::ValType::r#i64 => Ok(Val::i64_const(0)),
            structure::ValType::r#f32 => Ok(Val::f32_const(0.0)),
            structure::ValType::r#f64 => Ok(Val::f64_const(0.0)),
        };
    }
    let ea = ea as usize;
    //9
    let N = match N_sx {
        Some((n, _)) => n,
        _ => match t {
            structure::ValType::r#i32 => 32,
            structure::ValType::r#i64 => 64,
            structure::ValType::r#f32 => 32,
            structure::ValType::r#f64 => 64,
        }, //9.a
    };
    //10
    if (ea + N / 8) > mem.data.len() {
        instrs.insert(0, Instr::trap(String::from("out of bounds memory access")));
        return match t {
            structure::ValType::r#i32 => Ok(Val::i32_const(0)),
            structure::ValType::r#i64 => Ok(Val::i64_const(0)),
            structure::ValType::r#f32 => Ok(Val::f32_const(0.0)),
            structure::ValType::r#f64 => Ok(Val::f64_const(0.0)),
        };
    }
    //11
    let b = &mem.data[ea..(ea + N / 8)];
    //12
    match N_sx {
        Some((N, sx)) => {
            let n = ibits_N(N, b)?;
            let c = extend_sx_N_t(N, sx, t, n)?;
            Ok(c)
        }
        _ => {
            let c = match t {
                structure::ValType::r#i32 => Val::i32_const(i32::from_le_bytes(b.try_into()?)),
                structure::ValType::r#i64 => Val::i64_const(i64::from_le_bytes(b.try_into()?)),
                structure::ValType::r#f32 => Val::f32_const(f32::from_le_bytes(b.try_into()?)),
                structure::ValType::r#f64 => Val::f64_const(f64::from_le_bytes(b.try_into()?)),
            };
            Ok(c)
        }
    }
}
fn store_N(
    t: structure::ValType,
    N: Option<usize>,
    memarg: structure::MemArg,
    config: &mut Config,
    instrs: &mut Vec<Instr>,
) -> Result<bool, Error> {
    //1
    let F = &config.thread.frame;
    //2
    //3
    let a = F.borrow().module.borrow().memaddrs[0];
    //4
    //5
    //6
    //7
    let c = match config.thread.stack.pop() {
        Some(Entry::val(val)) => val,
        _ => return Err(Error::ExecuteError(line!())),
    };
    match t {
        structure::ValType::r#i32 => match c {
            Val::i32_const(_) => {}
            _ => return Err(Error::ExecuteError(line!())),
        },
        structure::ValType::r#i64 => match c {
            Val::i64_const(_) => {}
            _ => return Err(Error::ExecuteError(line!())),
        },
        structure::ValType::r#f32 => match c {
            Val::f32_const(_) => {}
            _ => return Err(Error::ExecuteError(line!())),
        },
        structure::ValType::r#f64 => match c {
            Val::f64_const(_) => {}
            _ => return Err(Error::ExecuteError(line!())),
        },
    }
    //8
    //9
    let i = match config.thread.stack.pop() {
        Some(Entry::val(Val::i32_const(i))) => i,
        _ => return Err(Error::ExecuteError(line!())),
    };
    //10
    let ea = i + memarg.offset as i32;
    //11
    let N1 = match N {
        Some(N) => N,
        _ => match t {
            structure::ValType::r#i32 => 32,
            structure::ValType::r#i64 => 64,
            structure::ValType::r#f32 => 32,
            structure::ValType::r#f64 => 64,
        },
    };
    //12
    if (ea + (N1 as i32) / 8) as usize > config.store.borrow_mut().mems[a as usize].data.len() {
        instrs.insert(0, Instr::trap(format!("{}", line!())));
    }
    //13
    let bs = match N {
        Some(N) => {
            let n = wrap_M_N(N, c)?;
            bytes_i_N(N, n)?
        }
        _ => {
            //14
            bytes_t(c)?
        }
    };

    config.store.borrow_mut().mems[a as usize]
        .data
        .splice((ea as usize)..((ea + N1 as i32 / 8) as usize), bs);
    return Ok(true);
}
fn wrap_M_N(N: usize, c: Val) -> Result<Val, Error> {
    match c {
        Val::i32_const(i) => match N {
            8 | 16 => Ok(Val::i32_const(i % (1_i32 << N))),
            _ => Err(Error::ExecuteError(line!())),
        },
        Val::i64_const(i) => match N {
            8 | 16 | 32 => Ok(Val::i64_const(i % (1_i64 << N))),
            _ => Err(Error::ExecuteError(line!())),
        },
        _ => Err(Error::ExecuteError(line!())),
    }
}
fn bytes_i_N(N: usize, n: Val) -> Result<Vec<u8>, Error> {
    match N {
        8 => {
            match n {
                // i32.store8
                Val::i32_const(i) => {
                    let v = i as i8;
                    Ok(v.to_le_bytes().to_vec())
                }
                // i64.store8
                Val::i64_const(i) => {
                    let v = i as i8;
                    Ok(v.to_le_bytes().to_vec())
                }
                _ => Err(Error::ExecuteError(line!())),
            }
        }
        16 => {
            match n {
                // i32.store16
                Val::i32_const(i) => {
                    let v = i as i16;
                    Ok(v.to_le_bytes().to_vec())
                }
                //i64.store16
                Val::i64_const(i) => {
                    let v = i as i16;
                    Ok(v.to_le_bytes().to_vec())
                }
                _ => Err(Error::ExecuteError(line!())),
            }
        }
        32 => {
            match n {
                // i32.store
                Val::i32_const(i) => {
                    let v = i as i32;
                    Ok(v.to_le_bytes().to_vec())
                }
                //i64.store32
                Val::i64_const(i) => {
                    let v = i as i32;
                    Ok(v.to_le_bytes().to_vec())
                }
                _ => Err(Error::ExecuteError(line!())),
            }
        }
        64 => {
            match n {
                //i64.store
                Val::i64_const(i) => Ok(i.to_le_bytes().to_vec()),
                _ => Err(Error::ExecuteError(line!())),
            }
        }
        _ => Err(Error::ExecuteError(line!())),
    }
}
fn bytes_t(c: Val) -> Result<Vec<u8>, Error> {
    match c {
        Val::i32_const(i) => Ok(i.to_le_bytes().to_vec()),
        Val::i64_const(i) => Ok(i.to_le_bytes().to_vec()),
        Val::f32_const(f) => Ok(f.to_le_bytes().to_vec()),
        Val::f64_const(f) => Ok(f.to_le_bytes().to_vec()),
    }
}

fn extend_sx_N_t(N: usize, sx: bool, t: structure::ValType, n: Integer) -> Result<Val, Error> {
    match N {
        8 => {
            if sx {
                match t {
                    structure::ValType::r#i32 => match n {
                        // i32.load8_s
                        Integer::b8(buf) => Ok(Val::i32_const(i8::from_le_bytes(buf) as i32)),
                        _ => Err(Error::ExecuteError(line!())),
                    },
                    structure::ValType::r#i64 => match n {
                        // i64.load8_s
                        Integer::b8(buf) => Ok(Val::i64_const(i8::from_le_bytes(buf) as i64)),
                        _ => Err(Error::ExecuteError(line!())),
                    },
                    _ => Err(Error::ExecuteError(line!())),
                }
            } else {
                match t {
                    structure::ValType::r#i32 => match n {
                        // i32.load8_u
                        Integer::b8(buf) => Ok(Val::i32_const(u8::from_le_bytes(buf) as i32)),
                        _ => Err(Error::ExecuteError(line!())),
                    },
                    structure::ValType::r#i64 => match n {
                        // i64.load8_u
                        Integer::b8(buf) => Ok(Val::i64_const(u8::from_le_bytes(buf) as i64)),
                        _ => Err(Error::ExecuteError(line!())),
                    },
                    _ => Err(Error::ExecuteError(line!())),
                }
            }
        }
        16 => {
            if sx {
                match t {
                    structure::ValType::r#i32 => match n {
                        // i32.load16_s
                        Integer::b16(buf) => Ok(Val::i32_const(i16::from_le_bytes(buf) as i32)),
                        _ => Err(Error::ExecuteError(line!())),
                    },
                    structure::ValType::r#i64 => match n {
                        // i64.load16_s
                        Integer::b16(buf) => Ok(Val::i64_const(i16::from_le_bytes(buf) as i64)),
                        _ => Err(Error::ExecuteError(line!())),
                    },
                    _ => Err(Error::ExecuteError(line!())),
                }
            } else {
                match t {
                    structure::ValType::r#i32 => match n {
                        // i32.load16_u
                        Integer::b16(buf) => Ok(Val::i32_const(u16::from_le_bytes(buf) as i32)),
                        _ => Err(Error::ExecuteError(line!())),
                    },
                    structure::ValType::r#i64 => match n {
                        // i64.load16_u
                        Integer::b16(buf) => Ok(Val::i64_const(u16::from_le_bytes(buf) as i64)),
                        _ => Err(Error::ExecuteError(line!())),
                    },
                    _ => Err(Error::ExecuteError(line!())),
                }
            }
        }
        32 => {
            if sx {
                match t {
                    structure::ValType::r#i64 => match n {
                        // i64.load32_s
                        Integer::b32(buf) => Ok(Val::i64_const(i32::from_le_bytes(buf) as i64)),
                        _ => Err(Error::ExecuteError(line!())),
                    },
                    _ => Err(Error::ExecuteError(line!())),
                }
            } else {
                match t {
                    structure::ValType::r#i64 => match n {
                        // i64.load32_u
                        Integer::b32(buf) => Ok(Val::i64_const(u32::from_le_bytes(buf) as i64)),
                        _ => Err(Error::ExecuteError(line!())),
                    },
                    _ => Err(Error::ExecuteError(line!())),
                }
            }
        }
        _ => Err(Error::ExecuteError(line!())),
    }
}

enum Integer {
    b8([u8; 1]),
    b16([u8; 2]),
    b32([u8; 4]),
    b64([u8; 8]),
}
fn ibits_N(N: usize, b: &[u8]) -> Result<Integer, Error> {
    //todo littleendian
    match N {
        8 => Ok(Integer::b8(b.try_into()?)),
        16 => Ok(Integer::b16(b.try_into()?)),
        32 => Ok(Integer::b32(b.try_into()?)),
        64 => Ok(Integer::b64(b.try_into()?)),
        _ => Err(Error::ExecuteError(line!())),
    }
}
fn fbits_N(N: usize, b: &[u8]) -> Result<Val, Error> {
    match N {
        32 => {
            assert!(b.len() >= 4);
            Ok(Val::f32_const(f32::from_le_bytes(b.try_into()?)))
        }
        64 => {
            assert!(b.len() >= 8);
            Ok(Val::f64_const(f64::from_le_bytes(b.try_into()?)))
        }
        _ => Err(Error::ExecuteError(line!())),
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

//4.4.8 Expressions
pub fn evalute(config: &mut Config, instrs: &mut Vec<Instr>) -> Result<Val, Error> {
    reduce_to_end(config, instrs)?;
    let entry = config.thread.stack.pop();
    match entry {
        Some(Entry::val(val)) => Ok(val),
        _ => Err(Error::ExecuteError(line!())),
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
        Err(Error::ExecuteError(line!()))
    } else if tableinst.max.is_some() && (tableinst.max.unwrap() as usize) < len {
        //4
        Err(Error::ExecuteError(line!()))
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
        Err(Error::ExecuteError(line!()))
    } else if meminst.max.is_some() && (meminst.max.unwrap() as usize) < len {
        //5
        Err(Error::ExecuteError(line!()))
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
    let moduleinst = Rc::new(RefCell::new(ModuleInst::new()));
    moduleinst.borrow_mut().types = module.types.clone();
    //1
    //2
    let funcaddrs: Vec<FuncAddr> = module
        .funcs
        .iter()
        .map(|func| allocfunc(Rc::clone(&S), func, Rc::clone(&moduleinst)))
        .collect();
    //3
    let tableaddrs: Vec<TableAddr> = module
        .tables
        .iter()
        .map(|table| alloctable(Rc::clone(&S), &table.r#type))
        .collect();
    //4
    let memaddrs: Vec<MemAddr> = module
        .mems
        .iter()
        .map(|mem| allocmem(Rc::clone(&S), &mem.r#type))
        .collect();
    //5
    let globaladdrs: Vec<GlobalAddr> = module
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
pub fn instantiate(
    S: Rc<RefCell<Store>>,
    module: &structure::Module,
    externvals: Vec<ExternVal>,
) -> Result<(Rc<RefCell<Frame>>, Vec<Val>), Error> {
    //1
    validation::validate(module)?;
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
        return Err(Error::ExecuteError(line!()));
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
            return Err(Error::ExecuteError(line!()));
        }
    }
    //5
    let mut vals: Vec<Val> = vec![];
    //5.a
    let moduleinst_im = Rc::new(RefCell::new(ModuleInst::new()));
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
        n: n, // TODO
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
        _ => return Err(Error::ExecuteError(line!())),
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
            _ => return Err(Error::ExecuteError(line!())), //9.b
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
            return Err(Error::ExecuteError(line!()));
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
            _ => return Err(Error::ExecuteError(line!())), //10.b
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
            return Err(Error::ExecuteError(line!()));
        }
    }
    //11
    //12
    let entry = config.thread.stack.pop();
    match entry {
        Some(Entry::activation(Activation { n: _, frame: _ })) => {}
        _ => return Err(Error::ExecuteError(line!())),
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
        reduce_to_end(&mut config, &mut instrs)?;

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
                _ => return Err(Error::ExecuteError(line!())),
            }
        }
    }

    Ok((Rc::clone(&config.thread.frame), vals))
}

//4.5.5
pub fn invoke(
    S: Rc<RefCell<Store>>,
    moduleinst: Rc<RefCell<ModuleInst>>,
    funcaddr: FuncAddr,
    vals: Vec<Val>,
) -> Result<(Config, Vec<Val>), Error> {
    //1
    //2
    //3
    let structure::FuncType { params, results } = match S
        .borrow()
        .funcs
        .get(funcaddr as usize)
        .ok_or(Error::ExecuteError(line!()))?
    {
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
    let n = params.len();
    let m = results.len();
    //4
    if vals.len() != n {
        return Err(Error::ExecuteError(line!()));
    }
    //5
    for i in 0..n {
        let t = &params[i];
        let val = &vals[i];
        match t {
            structure::ValType::r#i32 => match val {
                Val::i32_const(_) => {}
                _ => return Err(Error::ExecuteError(line!())), //5.a.i
            },
            structure::ValType::r#i64 => match val {
                Val::i64_const(_) => {}
                _ => return Err(Error::ExecuteError(line!())), //5.a.i
            },
            structure::ValType::r#f32 => match val {
                Val::f32_const(_) => {}
                _ => return Err(Error::ExecuteError(line!())), //5.a.i
            },
            structure::ValType::r#f64 => match val {
                Val::f64_const(_) => {}
                _ => return Err(Error::ExecuteError(line!())), //5.a.i
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
        n: m,
        frame: Rc::clone(&F),
    }));
    //8
    for val in vals.into_iter() {
        config.thread.stack.push(Entry::val(val));
    }
    //9
    let mut instrs = vec![Instr::invoke(funcaddr)];
    reduce_to_end(&mut config, &mut instrs)?;

    //1
    //2
    let mut valm_res: Vec<Val> = vec![];
    for _ in 0..m {
        let entry = config.thread.stack.pop();
        match entry {
            Some(Entry::val(val)) => {
                valm_res.insert(0, val);
            }
            _ => return Err(Error::ExecuteError(line!())),
        }
    }
    Ok((config, valm_res))
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
                    return Err(Error::ExecuteError(line!()));
                }
            }
        }
        _ => Err(Error::ExecuteError(line!())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_return(
        store: Rc<RefCell<Store>>,
        moduleinst: Rc<RefCell<ModuleInst>>,
        name: &str,
        input: Val,
        output: Val,
    ) {
        let m = moduleinst.borrow();
        let exportinst = m
            .exports
            .iter()
            .find(|exportinst| exportinst.name == name)
            .unwrap();
        match exportinst.value {
            ExternVal::func(funcaddr) => {
                let (_, vals) = invoke(
                    Rc::clone(&store),
                    Rc::clone(&moduleinst),
                    funcaddr,
                    vec![input],
                )
                .unwrap();
                assert_eq!(vals.len(), 1);
                assert_eq!(vals[0], output);
            }
            _ => assert!(false),
        }
    }
}
