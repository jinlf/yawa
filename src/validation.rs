use crate::structure::*;
use std::collections::HashSet;

#[derive(Debug, PartialEq)]
pub enum Error {
    ValidateError(String),
}

struct Context {
    types: Vec<FuncType>,
    funcs: Vec<FuncType>,
    tables: Vec<TableType>,
    mems: Vec<MemType>,
    globals: Vec<GlobalType>,
    locals: Vec<ValType>,
    labels: Vec<ResultType>,
    r#return: Option<ResultType>,
}
impl Context {
    fn new() -> Self {
        Self {
            types: vec![],
            funcs: vec![],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            locals: vec![],
            labels: vec![],
            r#return: None,
        }
    }
    fn push(&mut self, other: &Self) {
        self.types.append(&mut other.types.clone());
        self.funcs.append(&mut other.funcs.clone());
        self.tables.append(&mut other.tables.clone());
        self.mems.append(&mut other.mems.clone());
        self.globals.append(&mut other.globals.clone());
        self.locals.append(&mut other.locals.clone());
        self.labels.append(&mut other.labels.clone());
        self.r#return = other.r#return.clone();
    }
}

//3.2.1
fn validate_limits(limits: &Limits, k: usize) -> Result<(), Error> {
    let n = limits.min;
    let m = limits.max;
    if n as usize <= k
        && match m {
            Some(m) => m as usize <= k && n <= m,
            _ => false,
        }
    {
        Ok(())
    } else {
        Err(Error::ValidateError(String::from("TODO")))
    }
}
//3.2.2
fn validate_blocktype(C: &Context, blocktype: &BlockType) -> Result<(), Error> {
    match blocktype {
        BlockType::typeidx(typeidx) => C
            .types
            .get(*typeidx as usize)
            .ok_or(Error::ValidateError(String::from("TODO")))?,
        BlockType::valtype(valtype) => C
            .funcs
            .iter()
            .find(|func| {
                func.params.len() == 0
                    && func.results == valtype.iter().cloned().collect::<Vec<ValType>>()
            })
            .ok_or(Error::ValidateError(String::from("TODO")))?,
    };
    Ok(())
}
//3.2.3
fn validate_functype(functype: &FuncType) -> Result<(), Error> {
    Ok(())
}
//3.2.4
fn validate_tabletype(tabletype: &TableType) -> Result<(), Error> {
    Ok(validate_limits(&tabletype.limits, 1_usize << 32)?)
}
//3.2.5
fn validate_memtype(memtype: &MemType) -> Result<(), Error> {
    Ok(validate_limits(&memtype.limits, 1_usize << 16)?)
}
//3.2.6
fn validate_globaltype(globaltype: &GlobalType) -> Result<(), Error> {
    Ok(())
}
//3.2.7
fn validate_externtype(externtype: &ExternType) -> Result<(), Error> {
    match externtype {
        ExternType::func(functype) => Ok(validate_functype(functype)?),
        ExternType::table(tabletype) => Ok(validate_tabletype(tabletype)?),
        ExternType::mem(memtype) => Ok(validate_memtype(memtype)?),
        ExternType::global(globaltype) => Ok(validate_globaltype(globaltype)?),
    }
}

pub fn validate(module: &Module) -> Result<(), Error> {
    let mut functypes = module.types.clone();
    let mut fts = module
        .funcs
        .iter()
        .map(|func| {
            module
                .types
                .get(func.r#type as usize)
                .cloned()
                .ok_or(Error::ValidateError(String::from("v3")))
        })
        .collect::<Result<Vec<_>, Error>>()?;
    let mut tts = module
        .tables
        .iter()
        .map(|table| table.r#type.clone())
        .collect();
    let mut mts = module.mems.iter().map(|mem| mem.r#type.clone()).collect();
    let mut gts = module
        .globals
        .iter()
        .map(|global| global.r#type.clone())
        .collect();
    let its = module
        .imports
        .iter()
        .map(|import| match &import.desc {
            ImportDesc::func(typeidx) => ExternType::func(module.types[*typeidx as usize].clone()),
            ImportDesc::table(tabletype) => ExternType::table(tabletype.clone()),
            ImportDesc::mem(memtype) => ExternType::mem(memtype.clone()),
            ImportDesc::global(globaltype) => ExternType::global(globaltype.clone()),
        })
        .collect::<Vec<ExternType>>();
    let mut ets = module.exports.clone();
    let mut ifts = crate::structure::funcs(&its);
    let mut itts = crate::structure::tables(&its);
    let mut imts = crate::structure::mems(&its);
    let mut igts = crate::structure::globals(&its);

    let mut C = Context::new();
    C.types.append(&mut functypes);
    C.funcs.append(&mut ifts);
    C.funcs.append(&mut fts);
    C.tables.append(&mut itts);
    C.tables.append(&mut tts);
    C.mems.append(&mut imts);
    C.mems.append(&mut mts);
    C.globals.append(&mut igts.clone());
    C.globals.append(&mut gts);
    let mut C1 = Context::new();
    C1.globals.append(&mut igts);

    for functype in module.types.iter() {
        validate_functype(functype)?
    }
    for func in module.funcs.iter() {
        validate_func(&C, func)?;
    }
    for table in module.tables.iter() {
        validate_table(&C, table)?;
    }
    for mem in module.mems.iter() {
        validate_mem(&C, mem)?;
    }
    for global in module.globals.iter() {
        validate_global(&C1, global)?;
    }
    for elem in module.elem.iter() {
        validate_elem(&C, elem)?;
    }
    for data in module.data.iter() {
        validate_data(&C, data)?;
    }
    if let Some(Start { func }) = module.start {
        let functype = &module.types[func as usize];
        validate_functype(functype)?;
    }
    for import in module.imports.iter() {
        validate_import(&C, import)?;
    }
    for export in module.exports.iter() {
        validate_export(&C, export)?;
    }
    if C.tables.len() > 1 {
        return Err(Error::ValidateError(String::from("TODO")));
    }
    if C.mems.len() > 1 {
        return Err(Error::ValidateError(String::from("TODO")));
    }
    let hashset: HashSet<_> = module
        .exports
        .iter()
        .map(|export| export.name.clone())
        .collect();
    if hashset.len() != module.exports.len() {
        return Err(Error::ValidateError(String::from("TODO")));
    }

    Ok(())
}

fn validate_func(C: &Context, func: &Func) -> Result<(), Error> {
    let mut validator = Validator::new();
    let functype = &C.types[func.r#type as usize];
    validator.push_ctrl(
        Instr::block {
            blocktype: BlockType::valtype(None),
            instrs: vec![],
        },
        functype.params.clone(),
        functype.results.clone(),
    );
    Ok(validate_expr(C, &mut validator, &func.body)?)
}
fn validate_table(C: &Context, table: &Table) -> Result<(), Error> {
    Ok(())
}
fn validate_mem(C: &Context, mem: &Mem) -> Result<(), Error> {
    Ok(())
}
fn validate_global(C: &Context, global: &Global) -> Result<(), Error> {
    Ok(())
}
fn validate_elem(C: &Context, elem: &Elem) -> Result<(), Error> {
    Ok(())
}
fn validate_data(C: &Context, data: &Data) -> Result<(), Error> {
    Ok(())
}
fn validate_import(C: &Context, import: &Import) -> Result<(), Error> {
    Ok(())
}
fn validate_export(C: &Context, export: &Export) -> Result<(), Error> {
    Ok(())
}
fn validate_expr(C: &Context, v: &mut Validator, expr: &Expr) -> Result<(), Error> {
    validate_instrs(C, v, &expr.instrs)?;
    Ok(validate_instrs(C, v, &vec![Instr::end])?)
}
fn validate_load_store(C: &Context, memarg: &MemArg, N: u32) -> Result<(), Error> {
    if C.mems.len() < 1 {
        return Err(Error::ValidateError(String::from("TODO")));
    }
    if memarg.align > (N / 8) {
        return Err(Error::ValidateError(String::from(
            "alignment must not be larger than natural",
        )));
    }
    Ok(())
}
fn validate_instrs(C: &Context, v: &mut Validator, instrs: &Vec<Instr>) -> Result<(), Error> {
    for instr in instrs.iter() {
        v.validate_opcode(C, instr)?;
    }
    Ok(())
}

type val_type = ValType;

#[derive(PartialEq)]
enum stack_item {
    val_type(val_type),
    Unknown,
}
type opcode = Instr;
type opd_stack = Vec<stack_item>;
type ctrl_stack = Vec<ctrl_frame>;
#[derive(Clone)]
struct ctrl_frame {
    opcode: opcode,
    start_types: Vec<val_type>,
    end_types: Vec<val_type>,
    height: usize,
    unreachable: bool,
}

struct Validator {
    opds: opd_stack,
    ctrls: ctrl_stack,
}
impl Validator {
    fn new() -> Self {
        Self {
            opds: vec![],
            ctrls: vec![],
        }
    }
    fn push_opd(&mut self, r#type: stack_item) {
        self.opds.push(r#type)
    }
    fn pop_opd(&mut self) -> Result<stack_item, Error> {
        let len = self.ctrls.len();
        let top = &self.ctrls[len - 1];
        if self.opds.len() == top.height && top.unreachable {
            return Ok(stack_item::Unknown);
        }
        if self.opds.len() == top.height {
            return Err(Error::ValidateError(String::from("v1")));
        }
        Ok(self.opds.pop().unwrap())
    }
    fn pop_opd_expect(&mut self, expect: stack_item) -> Result<stack_item, Error> {
        let actual = self.pop_opd()?;
        match actual {
            stack_item::Unknown => return Ok(expect),
            _ => {}
        }
        match expect {
            stack_item::Unknown => return Ok(actual),
            _ => {}
        }
        if actual != expect {
            return Err(Error::ValidateError(String::from("v2")));
        }
        Ok(actual)
    }
    fn push_opds(&mut self, types: Vec<val_type>) {
        self.opds.append(
            &mut types
                .into_iter()
                .map(|r#type| stack_item::val_type(r#type))
                .collect(),
        );
    }
    fn pop_opds(&mut self, types: &Vec<ValType>) -> Result<(), Error> {
        for t in types.into_iter().rev() {
            self.pop_opd_expect(stack_item::val_type(t.clone()))?;
        }
        Ok(())
    }
    fn push_ctrl(&mut self, opcode: opcode, ins: Vec<val_type>, outs: Vec<val_type>) {
        let frame = ctrl_frame {
            opcode: opcode,
            start_types: ins.clone(),
            end_types: outs,
            height: self.opds.len(),
            unreachable: false,
        };
        self.ctrls.push(frame);
        self.push_opds(ins)
    }
    fn pop_ctrl(&mut self) -> Result<ctrl_frame, Error> {
        if self.ctrls.len() == 0 {
            return Err(Error::ValidateError(String::from("v4")));
        }
        let frame = self.ctrls.pop().unwrap();
        self.pop_opds(&frame.end_types)?;
        if self.opds.len() != frame.height {
            return Err(Error::ValidateError(String::from("type mismatch")));
        }
        Ok(frame)
    }
    fn label_types(frame: ctrl_frame) -> Vec<val_type> {
        match frame.opcode {
            Instr::r#loop { .. } => frame.start_types,
            _ => frame.end_types,
        }
    }
    fn unreachable(&mut self) {
        let len = self.ctrls.len();
        let top = &mut self.ctrls[len - 1];
        self.opds.truncate(top.height);
        top.unreachable = true;
    }
    fn validate_opcode(&mut self, C: &Context, opcode: &opcode) -> Result<(), Error> {
        match opcode {
            Instr::nop => {
                println!();
            }
            Instr::i32_const(_) => {
                self.push_opd(stack_item::val_type(val_type::r#i32));
            }
            Instr::i32_add => {
                self.pop_opd_expect(stack_item::val_type(val_type::r#i32))?;
                self.pop_opd_expect(stack_item::val_type(val_type::r#i32))?;
                self.push_opd(stack_item::val_type(val_type::r#i32));
            }
            Instr::block { blocktype, instrs } => {
                let ft = match blocktype {
                    BlockType::typeidx(typeidx) => C.types[*typeidx as usize].clone(),
                    BlockType::valtype(valtype) => FuncType {
                        params: vec![],
                        results: match valtype {
                            Some(vt) => vec![vt.clone()],
                            _ => vec![],
                        },
                    },
                };
                let ts1 = ft.params;
                let ts2 = ft.results;

                self.pop_opds(&ts1)?;
                self.push_ctrl(
                    Instr::block {
                        blocktype: blocktype.clone(), //unnecessary
                        instrs: instrs.clone(),       // unnecessary
                    },
                    ts1,
                    ts2.clone(),
                );
            }
            Instr::end => {
                let frame = self.pop_ctrl()?;
                self.push_opds(frame.end_types.clone());
            }
            _ => {
                println!("unimplemented: {:#?}", opcode);
            }
        }
        Ok(())
    }
}
