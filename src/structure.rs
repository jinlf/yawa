//2.2.4 Names
pub type Name = String;
//2.3.1 Value Types
#[derive(Debug, Clone, PartialEq)]
pub enum ValType {
    r#i32,
    r#i64,
    r#f32,
    r#f64,
}
//2.3.2 Result Types
pub type ResultType = Vec<ValType>;
//2.3.3 Function Types
#[derive(Debug, PartialEq, Clone)]
pub struct FuncType {
    pub params: ResultType,
    pub results: ResultType,
}
//2.3.4 Limits
#[derive(Debug, Clone)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}
//2.3.5 Memory Types
#[derive(Debug, Clone)]
pub struct MemType {
    pub limits: Limits,
}
//2.3.6 Table Types
#[derive(Debug, Clone)]
pub struct TableType {
    pub limits: Limits,
    pub elemtype: ElemType,
}
#[derive(Debug, PartialEq, Clone)]
pub enum ElemType {
    FuncRef,
}
//2.3.7 Global Types
#[derive(Debug, PartialEq, Clone)]
pub struct GlobalType {
    pub r#mut: Mut,
    pub valtype: ValType,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Mut {
    Const,
    Var,
}
//2.3.8 External Types
#[derive(Clone)]
pub enum ExternType {
    func(FuncType),
    table(TableType),
    mem(MemType),
    global(GlobalType),
}
//2.4 Instructions
#[derive(Debug, Clone)]
pub enum Instr {
    //2.4.1 Numeric Instructions
    //inn.const inn
    i32_const(i32),
    i64_const(i64),
    //fnn.const fnn
    f32_const(f32),
    f64_const(f64),
    //inn.iunop
    i32_clz,
    i32_ctz,
    i32_popcnt,
    i64_clz,
    i64_ctz,
    i64_popcnt,
    //fnn.funop
    f32_abs,
    f32_neg,
    f32_sqrt,
    f32_ceil,
    f32_floor,
    f32_trunc,
    f32_nearest,
    f64_abs,
    f64_neg,
    f64_sqrt,
    f64_ceil,
    f64_floor,
    f64_trunc,
    f64_nearest,
    //inn.ibinop
    i32_add,
    i32_sub,
    i32_mul,
    i32_div_u,
    i32_div_s,
    i32_rem_u,
    i32_rem_s,
    i32_and,
    i32_or,
    i32_xor,
    i32_shl,
    i32_shr_u,
    i32_shr_s,
    i32_rotl,
    i32_rotr,
    i64_add,
    i64_sub,
    i64_mul,
    i64_div_u,
    i64_div_s,
    i64_rem_u,
    i64_rem_s,
    i64_and,
    i64_or,
    i64_xor,
    i64_shl,
    i64_shr_u,
    i64_shr_s,
    i64_rotl,
    i64_rotr,
    //fnn.fbinop
    f32_add,
    f32_sub,
    f32_mul,
    f32_div,
    f32_min,
    f32_max,
    f32_copysign,
    f64_add,
    f64_sub,
    f64_mul,
    f64_div,
    f64_min,
    f64_max,
    f64_copysign,
    //inn.itestop
    i32_eqz,
    i64_eqz,
    //inn.irelop
    i32_eq,
    i32_ne,
    i32_lt_u,
    i32_gt_u,
    i32_le_u,
    i32_ge_u,
    i32_lt_s,
    i32_gt_s,
    i32_le_s,
    i32_ge_s,
    i64_eq,
    i64_ne,
    i64_lt_u,
    i64_gt_u,
    i64_le_u,
    i64_ge_u,
    i64_lt_s,
    i64_gt_s,
    i64_le_s,
    i64_ge_s,
    //fnn.frelop
    f32_eq,
    f32_ne,
    f32_lt,
    f32_gt,
    f32_le,
    f32_ge,
    f64_eq,
    f64_ne,
    f64_lt,
    f64_gt,
    f64_le,
    f64_ge,
    //inn.extend8_s
    i32_extend8_s,
    i64_extend8_s,
    //inn.extend16_s
    i32_extend16_s,
    i64_extend16_s,
    //inn.extend32_s
    i32_extend32_s,
    i64_extend32_s,

    i32_wrap_i64,
    i64_extend_i32_u,
    i64_extend_i32_s,
    //inn.trunc_fmm_sx
    i32_trunc_f32_u,
    i32_trunc_f32_s,
    i32_trunc_f64_u,
    i32_trunc_f64_s,
    i64_trunc_f32_u,
    i64_trunc_f32_s,
    i64_trunc_f64_u,
    i64_trunc_f64_s,
    //inn.trunc_sat_fmm_sx
    i32_trunc_sat_f32_u,
    i32_trunc_sat_f32_s,
    i32_trunc_sat_f64_u,
    i32_trunc_sat_f64_s,
    i64_trunc_sat_f32_u,
    i64_trunc_sat_f32_s,
    i64_trunc_sat_f64_u,
    i64_trunc_sat_f64_s,

    f32_demote_f64,
    f64_promote_f32,
    //fnn.convert_imm_sx
    f32_convert_i32_u,
    f32_convert_i32_s,
    f32_convert_i64_u,
    f32_convert_i64_s,
    f64_convert_i32_u,
    f64_convert_i32_s,
    f64_convert_i64_u,
    f64_convert_i64_s,
    //inn.reinterpret_fnn
    i32_reinterpret_f32,
    i64_reinterpret_f64,
    //fnn.reinterpret_inn
    f32_reinterpret_i32,
    f64_reinterpret_i64,

    //2.4.2 Parametric Instructions
    drop,
    select,
    //2.4.3 Variable Instructions
    local_get(LocalIdx),
    local_set(LocalIdx),
    local_tee(LocalIdx),
    global_get(GlobalIdx),
    global_set(GlobalIdx),
    //2.4.4 Memory Instructions
    //inn.load memoarg
    i32_load(MemArg),
    i64_load(MemArg),
    //fnn.load memarg
    f32_load(MemArg),
    f64_load(MemArg),
    //inn.store memarg
    i32_store(MemArg),
    i64_store(MemArg),
    //fnn.store memarg
    f32_store(MemArg),
    f64_store(MemArg),
    //inn.load8_sx memarg
    i32_load8_u(MemArg),
    i32_load8_s(MemArg),
    i64_load8_u(MemArg),
    i64_load8_s(MemArg),
    //inn.load16_sx memarg
    i32_load16_u(MemArg),
    i32_load16_s(MemArg),
    i64_load16_u(MemArg),
    i64_load16_s(MemArg),
    //i64.load32_sx memarg
    i64_load32_u(MemArg),
    i64_load32_s(MemArg),
    //inn.store8 memarg
    i32_store8(MemArg),
    i64_store8(MemArg),
    //inn.store16 memarg
    i32_store16(MemArg),
    i64_store16(MemArg),
    i64_store32(MemArg),
    memory_size,
    memory_grow,

    //2.4.5 Control Instructions
    nop,
    unreachable,
    block {
        blocktype: BlockType,
        instrs: Vec<Instr>,
    },
    r#loop {
        blocktype: BlockType,
        instrs: Vec<Instr>,
    },
    r#if {
        blocktype: BlockType,
        instrs1: Vec<Instr>,
        instrs2: Vec<Instr>,
    },
    br(LabelIdx),
    bf_if(LabelIdx),
    br_table {
        labelidxes: Vec<LabelIdx>,
        labelidx: LabelIdx,
    },
    r#return,
    call(FuncIdx),
    call_indirect(TypeIdx),

    end,
}
#[derive(Debug, Clone)]
pub struct MemArg {
    pub offset: u32,
    pub align: u32,
}
pub type BlockType = u32;

//2.4.6 Expressions
#[derive(Debug, Clone)]
pub struct Expr {
    pub instrs: Vec<Instr>,
}
impl Expr {
    pub fn new() -> Self {
        Self { instrs: vec![] }
    }
}
//2.5 Modules
#[derive(Debug)]
pub struct Module {
    pub types: Vec<FuncType>,
    pub funcs: Vec<Func>,
    pub tables: Vec<Table>,
    pub mems: Vec<Mem>,
    pub globals: Vec<Global>,
    pub elem: Vec<Elem>,
    pub data: Vec<Data>,
    pub start: Option<Start>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}
impl Module {
    pub fn new() -> Self {
        Self {
            types: vec![],
            funcs: vec![],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            elem: vec![],
            data: vec![],
            start: None,
            imports: vec![],
            exports: vec![],
        }
    }
}
//2.5.1 Indices
pub type TypeIdx = u32;
pub type FuncIdx = u32;
pub type TableIdx = u32;
pub type MemIdx = u32;
pub type GlobalIdx = u32;
pub type LocalIdx = u32;
pub type LabelIdx = u32;
//2.5.3 Functions
#[derive(Debug, Clone)]
pub struct Func {
    pub r#type: TypeIdx,
    pub locals: Vec<ValType>,
    pub body: Expr,
}
//2.5.4 Tables
#[derive(Debug)]
pub struct Table {
    pub r#type: TableType,
}
//2.5.5 Memories
#[derive(Debug)]
pub struct Mem {
    pub r#type: MemType,
}
//2.5.6 Globals
#[derive(Debug)]
pub struct Global {
    pub r#type: GlobalType,
    pub init: Expr,
}
//2.5.7 Element Segments
#[derive(Debug)]
pub struct Elem {
    pub table: TableIdx,
    pub offset: Expr,
    pub init: Vec<FuncIdx>,
}
//2.5.8 Data Segments
#[derive(Debug)]
pub struct Data {
    pub data: MemIdx,
    pub offset: Expr,
    pub init: Vec<u8>,
}
//2.5.9 Start Function
#[derive(Debug)]
pub struct Start {
    pub func: FuncIdx,
}
//2.5.10 Exports
#[derive(Debug)]
pub struct Export {
    pub name: Name,
    pub desc: ExportDesc,
}
#[derive(Debug)]
pub enum ExportDesc {
    func(FuncIdx),
    table(TableIdx),
    mem(MemIdx),
    global(GlobalIdx),
}
//2.5.11 Imports
#[derive(Debug)]
pub struct Import {
    pub module: Name,
    pub name: Name,
    pub desc: ImportDesc,
}
#[derive(Debug)]
pub enum ImportDesc {
    func(TypeIdx),
    table(TableType),
    mem(MemType),
    global(GlobalType),
}
