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
fn validate_instr(C: &Context, instr: &Instr) -> Result<(), Error> {
    match instr {
        //3.3.1
        //t.const c
        Instr::i32_const(_) | Instr::i64_const(_) | Instr::f32_const(_) | Instr::f64_const(_) => {
            Ok(())
        }
        //t.unop
        Instr::i32_clz
        | Instr::i32_ctz
        | Instr::i32_popcnt
        | Instr::i64_clz
        | Instr::i64_ctz
        | Instr::i64_popcnt
        | Instr::f32_abs
        | Instr::f32_neg
        | Instr::f32_sqrt
        | Instr::f32_ceil
        | Instr::f32_floor
        | Instr::f32_trunc
        | Instr::f32_nearest
        | Instr::f64_abs
        | Instr::f64_neg
        | Instr::f64_sqrt
        | Instr::f64_ceil
        | Instr::f64_floor
        | Instr::f64_trunc
        | Instr::f64_nearest => Ok(()),
        //t.binop
        Instr::i32_add
        | Instr::i32_sub
        | Instr::i32_mul
        | Instr::i32_div_u
        | Instr::i32_div_s
        | Instr::i32_rem_u
        | Instr::i32_rem_s
        | Instr::i32_and
        | Instr::i32_or
        | Instr::i32_xor
        | Instr::i32_shl
        | Instr::i32_shr_u
        | Instr::i32_shr_s
        | Instr::i32_rotl
        | Instr::i32_rotr
        | Instr::i64_add
        | Instr::i64_sub
        | Instr::i64_mul
        | Instr::i64_div_u
        | Instr::i64_div_s
        | Instr::i64_rem_u
        | Instr::i64_rem_s
        | Instr::i64_and
        | Instr::i64_or
        | Instr::i64_xor
        | Instr::i64_shl
        | Instr::i64_shr_u
        | Instr::i64_shr_s
        | Instr::i64_rotl
        | Instr::i64_rotr
        | Instr::f32_add
        | Instr::f32_sub
        | Instr::f32_mul
        | Instr::f32_div
        | Instr::f32_min
        | Instr::f32_max
        | Instr::f32_copysign
        | Instr::f64_add
        | Instr::f64_sub
        | Instr::f64_mul
        | Instr::f64_div
        | Instr::f64_min
        | Instr::f64_max
        | Instr::f64_copysign => Ok(()),
        // t.testop
        Instr::i32_eqz | Instr::i64_eqz => Ok(()),

        // 3.3.4
        //t.load memarg
        Instr::i32_load8_s(memarg)
        | Instr::i32_load8_u(memarg)
        | Instr::i32_store8(memarg)
        | Instr::i64_load8_s(memarg)
        | Instr::i64_load8_u(memarg)
        | Instr::i64_store8(memarg) => validate_load_store(C, memarg, 8),

        Instr::i32_load16_s(memarg)
        | Instr::i32_load16_u(memarg)
        | Instr::i32_store16(memarg)
        | Instr::i64_load16_s(memarg)
        | Instr::i64_load16_u(memarg)
        | Instr::i64_store16(memarg) => validate_load_store(C, memarg, 16),

        Instr::i32_load(memarg)
        | Instr::f32_load(memarg)
        | Instr::i32_store(memarg)
        | Instr::f32_store(memarg)
        | Instr::i64_load32_s(memarg)
        | Instr::i64_load32_u(memarg)
        | Instr::i64_store32(memarg) => validate_load_store(C, memarg, 32),

        Instr::i64_load(memarg)
        | Instr::f64_load(memarg)
        | Instr::i64_store(memarg)
        | Instr::f64_store(memarg) => validate_load_store(C, memarg, 64),

        _ => Ok(()),
    }
}

pub fn validate(module: &Module) -> Result<(), Error> {
    let mut functypes = module.types.clone();
    let mut fts = module
        .funcs
        .iter()
        .map(|func| module.types[func.r#type as usize].clone())
        .collect();
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
    Ok(validate_expr(C, &func.body)?)
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
fn validate_expr(C: &Context, expr: &Expr) -> Result<(), Error> {
    Ok(validate_instrs(C, &expr.instrs)?)
}
fn validate_instrs(C: &Context, instrs: &Vec<Instr>) -> Result<(), Error> {
    for instr in instrs.iter() {
        validate_instr(C, instr)?;
    }
    Ok(())
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
