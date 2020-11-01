use crate::parser::*;
use crate::process_instr;
use crate::token::*;
use std::str::FromStr;

pub type Name = String;
#[derive(Clone, Debug)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}
#[derive(Debug)]
pub struct FuncType {
    pub params: Vec<Param>,
    pub results: Vec<TFResult>,
}
pub type Id = String;
#[derive(Clone, Debug)]
pub struct Param {
    pub id: Id,
    pub valtype: ValType,
}
pub type TFResult = ValType;
#[derive(Clone, Debug)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}
pub type MemType = Limits;
#[derive(Clone, Debug)]
pub struct TableType {
    pub limits: Limits,
    pub elemtype: ElemType,
}
#[derive(Clone, Debug)]
pub enum ElemType {
    FuncRef,
}
#[derive(Clone, Debug)]
pub struct GlobalType {
    pub r#mut: bool,
    pub valtype: ValType,
}
#[derive(Debug)]
pub enum Instr {
    plaininstr(PlainInstr),
    blockinstr(BlockInstr),
}
pub type Label = Id;
#[derive(Debug)]
pub enum BlockType {
    result(Option<TFResult>),
    typeuse(TypeUse),
}
#[derive(Debug)]
pub enum BlockInstr {
    block {
        label: Label,
        blocktype: BlockType,
        instrs: Vec<Instr>,
        id: Id,
    },
    r#loop {
        label: Label,
        blocktype: BlockType,
        instrs: Vec<Instr>,
        id: Id,
    },
    r#if {
        label: Label,
        blocktype: BlockType,
        instrs1: Vec<Instr>,
        id1: Id,
        instrs2: Vec<Instr>,
        id2: Id,
    },
}
#[derive(Debug)]
pub enum PlainInstr {
    //6.5.2
    unreachable,
    nop,
    br(LabelIdx),
    br_if(LabelIdx),
    br_table {
        labelidxes: Vec<LabelIdx>,
        labelidx: LabelIdx,
    },
    r#return,
    call(FuncIdx),
    call_indirect(TypeUse),
    //6.5.3
    drop,
    select,
    //6.5.4
    local_get(LocalIdx),
    local_set(LocalIdx),
    local_tee(LocalIdx),
    global_get(GlobalIdx),
    global_set(GlobalIdx),
    //6.5.5
    i32_load(MemArg<4>),
    i64_load(MemArg<8>),
    f32_load(MemArg<4>),
    f64_load(MemArg<8>),
    i32_load8_s(MemArg<1>),
    i32_load8_u(MemArg<1>),
    i32_load16_s(MemArg<2>),
    i32_load16_u(MemArg<2>),
    i64_load8_s(MemArg<1>),
    i64_load8_u(MemArg<1>),
    i64_load16_s(MemArg<2>),
    i64_load16_u(MemArg<2>),
    i64_load32_s(MemArg<4>),
    i64_load32_u(MemArg<4>),
    i32_store(MemArg<4>),
    i64_store(MemArg<8>),
    f32_store(MemArg<4>),
    f64_store(MemArg<8>),
    i32_store8(MemArg<1>),
    i32_store16(MemArg<2>),
    i64_store8(MemArg<1>),
    i64_store16(MemArg<2>),
    i64_store32(MemArg<4>),
    memory_size,
    memory_grow,
    //6.5.6
    i32_const(i32),
    i64_const(i64),
    f32_const(f32),
    f64_const(f64),
    i32_clz,
    i32_ctz,
    i32_popcnt,
    i32_add,
    i32_sub,
    i32_mul,
    i32_div_s,
    i32_div_u,
    i32_rem_s,
    i32_rem_u,
    i32_and,
    i32_or,
    i32_xor,
    i32_shl,
    i32_shr_s,
    i32_shr_u,
    i32_rotl,
    i32_rotr,
    i64_clz,
    i64_ctz,
    i64_popcnt,
    i64_add,
    i64_sub,
    i64_mul,
    i64_div_s,
    i64_div_u,
    i64_rem_s,
    i64_rem_u,
    i64_and,
    i64_or,
    i64_xor,
    i64_shl,
    i64_shr_s,
    i64_shr_u,
    i64_rotl,
    i64_rotr,
    f32_abs,
    f32_neg,
    f32_ceil,
    f32_floor,
    f32_trunc,
    f32_nearest,
    f32_sqrt,
    f32_add,
    f32_sub,
    f32_mul,
    f32_div,
    f32_min,
    f32_max,
    f32_copysign,
    f64_abs,
    f64_neg,
    f64_ceil,
    f64_floor,
    f64_trunc,
    f64_nearest,
    f64_sqrt,
    f64_add,
    f64_sub,
    f64_mul,
    f64_div,
    f64_min,
    f64_max,
    f64_copysign,
    i32_eqz,
    i32_eq,
    i32_ne,
    i32_lt_s,
    i32_lt_u,
    i32_gt_s,
    i32_gt_u,
    i32_le_s,
    i32_le_u,
    i32_ge_s,
    i32_ge_u,
    i64_eqz,
    i64_eq,
    i64_ne,
    i64_lt_s,
    i64_lt_u,
    i64_gt_s,
    i64_gt_u,
    i64_le_s,
    i64_le_u,
    i64_ge_s,
    i64_ge_u,
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
    i32_wrap_i64,
    i32_trunc_f32_s,
    i32_trunc_f32_u,
    i32_trunc_f64_s,
    i32_trunc_f64_u,
    i32_trunc_sat_f32_s,
    i32_trunc_sat_f32_u,
    i32_trunc_sat_f64_s,
    i32_trunc_sat_f64_u,
    i64_extend_i32_s,
    i64_extend_i32_u,
    i64_trunc_f32_s,
    i64_trunc_f32_u,
    i64_trunc_f64_s,
    i64_trunc_f64_u,
    i64_trunc_sat_f32_s,
    i64_trunc_sat_f32_u,
    i64_trunc_sat_f64_s,
    i64_trunc_sat_f64_u,
    f32_convert_i32_s,
    f32_convert_i32_u,
    f32_convert_i64_s,
    f32_convert_i64_u,
    f32_demote_f64,
    f64_convert_i32_s,
    f64_convert_i32_u,
    f64_convert_i64_s,
    f64_convert_i64_u,
    f64_promote_f32,
    i32_reinterpret_f32,
    i64_reinterpret_f64,
    f32_reinterpret_i32,
    f64_reinterpret_i64,
    i32_extend8_s,
    i32_extend16_s,
    i64_extend8_s,
    i64_extend16_s,
    i64_extend32_s,
    end,
}
#[derive(Debug)]
pub struct MemArg<const N: u32> {
    pub offset: Offset,
    pub align: Align<N>,
}
pub type Offset = u32;
#[derive(Debug)]
pub struct Align<const N: u32> {
    pub align: u32,
    pub n: u32,
}
impl<const N: u32> Align<N> {
    fn new(align: u32) -> Self {
        Self { n: N, align: align }
    }
}
#[derive(Debug)]
pub struct Expr {
    pub instrs: Vec<Instr>,
}
#[derive(Clone, Debug)]
pub enum Idx {
    x(u32),
    v(Id),
}
pub type TypeIdx = Idx;
pub type FuncIdx = Idx;
pub type TableIdx = Idx;
pub type MemIdx = Idx;
pub type GlobalIdx = Idx;
pub type LocalIdx = Idx;
pub type LabelIdx = Idx;
#[derive(Debug)]
pub struct Type {
    pub id: Id,
    pub functype: FuncType,
}
#[derive(Clone, Debug)]
pub struct TypeUse {
    pub typeidx: TypeIdx,
    pub params: Vec<Param>,
    pub results: Vec<TFResult>,
}
#[derive(Debug)]
pub struct Import {
    pub module: Name,
    pub name: Name,
    pub desc: ImportDesc,
}
#[derive(Debug)]
pub enum ImportDesc {
    func { id: Id, func: TypeUse },
    table { id: Id, table: TableType },
    mem { id: Id, mem: MemType },
    global { id: Id, global: GlobalType },
}
#[derive(Debug)]
pub struct Func {
    pub id: Id,
    pub r#type: TypeUse,
    pub locals: Vec<Local>,
    pub body: Vec<Instr>,
}
#[derive(Debug)]
pub struct Local {
    pub id: Id,
    pub valtype: ValType,
}
#[derive(Debug)]
pub struct Table {
    pub id: Id,
    pub r#type: TableType,
}
#[derive(Debug)]
pub struct Mem {
    pub id: Id,
    pub r#type: MemType,
}
#[derive(Debug)]
pub struct Global {
    pub id: Id,
    pub r#type: GlobalType,
    pub init: Expr,
}
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
pub type Start = FuncIdx;
#[derive(Debug)]
pub struct Elem {
    pub table: TableIdx,
    pub offset: Expr,
    pub init: Vec<FuncIdx>,
}
#[derive(Debug)]
pub struct Data {
    pub data: MemIdx,
    pub offset: Expr,
    pub init: Vec<Vec<u8>>,
}
#[derive(Debug)]
pub struct Module {
    pub id: Id,
    pub modulefields: Vec<ModuleField>,
}
#[derive(Debug)]
pub enum ModuleField {
    types(Type),
    imports(Import),
    funcs(Func),
    tables(Table),
    mems(Mem),
    globals(Global),
    exports(Export),
    start(Start),
    elem(Elem),
    data(Data),
}

pub struct ModuleParser {}
impl ModuleParser {
    fn generate_id(c: &mut ParseContext) -> Id {
        c.cur_new_id += 1;
        format!("$${}", c.cur_new_id)
    }

    //6.3.1
    pub fn parse_u32(c: &mut ParseContext) -> Result<u32, ParseError> {
        match &c.cur_token.r#type {
            TokenType::NUM(s) => {
                let v = if s.starts_with("0x") {
                    u64::from_str_radix(&s[2..], 16)?
                } else {
                    u64::from_str_radix(&s[..], 10)?
                };
                c.next_token();
                Ok(v as u32)
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    pub fn read_u32(s: String) -> Result<u32, ParseError> {
        let v = if s.starts_with("0x") {
            u64::from_str_radix(&s[2..], 16)?
        } else {
            u64::from_str_radix(&s[..], 10)?
        };
        Ok(v as u32)
    }
    pub fn parse_i32(c: &mut ParseContext) -> Result<i32, ParseError> {
        match &c.cur_token.r#type {
            TokenType::NUM(num) => {
                let mut s = num.clone();
                c.next_token();
                let mut factor = 1_i64;
                if s.starts_with("+") {
                    s.remove(0);
                } else if s.starts_with("-") {
                    factor = -1_i64;
                    s.remove(0);
                }
                let v = if s.starts_with("0x") {
                    u32::from_str_radix(&s[2..], 16)?
                } else {
                    u32::from_str_radix(&s[..], 10)?
                };
                if factor > 0 {
                    Ok(v as i32)
                } else {
                    if v == 0 {
                        Ok(0_i32)
                    } else {
                        Ok((!v + 1) as i32)
                    }
                }
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    pub fn parse_i64(c: &mut ParseContext) -> Result<i64, ParseError> {
        match &c.cur_token.r#type {
            TokenType::NUM(num) => {
                let mut s = num.clone();
                c.next_token();
                let mut factor = 1_i64;
                if s.starts_with("+") {
                    s.remove(0);
                } else if s.starts_with("-") {
                    factor = -1_i64;
                    s.remove(0);
                }
                let v = if s.starts_with("0x") {
                    u64::from_str_radix(&s[2..], 16)?
                } else {
                    u64::from_str_radix(&s[..], 10)?
                };
                if factor > 0 {
                    Ok(v as i64)
                } else {
                    if v == 0 {
                        Ok(0 as i64)
                    } else {
                        Ok((!v + 1) as i64)
                    }
                }
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    //6.3.2
    pub fn parse_f32(c: &mut ParseContext) -> Result<f32, ParseError> {
        Ok(Self::parse_f64(c)? as f32)
    }
    pub fn parse_f64(c: &mut ParseContext) -> Result<f64, ParseError> {
        match &c.cur_token.r#type {
            TokenType::NUM(num) => {
                let mut s = num.clone();
                c.next_token();
                let mut factor = 1_f64;
                if s.starts_with("+") {
                    s.remove(0);
                } else if s.starts_with("-") {
                    factor = -1_f64;
                    s.remove(0);
                }
                if s.starts_with("0x") {
                    // hexfloat
                    let p1 = s.find(".");
                    let p2 = s.find(|c: char| c == 'p' || c == 'P');
                    if p1.is_some() {
                        let p1 = p1.unwrap();
                        if p2.is_some() {
                            let p2 = p2.unwrap();
                            if p1 + 1 != p2 {
                                let p = Self::f64_from_hexstring(&s[2..p1])?;
                                let q = Self::parse_hexfrac(&s[p1 + 1..p2])?;
                                let e = i32::from_str_radix(&s[p2 + 1..], 10)?;
                                return Ok((p + q) * 2_f64.powi(e) * factor);
                            } else {
                                let p = Self::f64_from_hexstring(&s[2..p1])?;
                                let e = i32::from_str_radix(&s[p2 + 1..], 10)?;
                                return Ok(p * 2_f64.powi(e) * factor);
                            }
                        } else {
                            if p1 + 1 != s.len() {
                                let p = Self::f64_from_hexstring(&s[2..p1])?;
                                let q = Self::parse_hexfrac(&s[p1 + 1..])?;
                                return Ok((p + q) * factor);
                            } else {
                                let p = Self::f64_from_hexstring(&s[2..p1])?;
                                return Ok(p * factor);
                            }
                        }
                    } else {
                        if p2.is_some() {
                            let p2 = p2.unwrap();
                            let p = Self::f64_from_hexstring(&s[2..p2])?;
                            let e = i32::from_str_radix(&s[p2 + 1..], 10)?;
                            return Ok(p * 2_f64.powi(e) * factor);
                        } else {
                            let p = Self::f64_from_hexstring(&s[2..])?;
                            return Ok(p * factor);
                        }
                    }
                } else if s == "inf" {
                    if factor == 1_f64 {
                        Ok(f64::from_str("inf")?)
                    } else {
                        Ok(f64::from_str("-inf")?)
                    }
                } else if s == "nan" {
                    if factor == 1_f64 {
                        Ok(f64::from_str("NaN")?)
                    } else {
                        Ok(f64::from_str("-NaN")?)
                    }
                } else if s.starts_with("nan:0x") {
                    Ok(f64::from_str("NaN")?)
                } else {
                    let p1 = s.find(".");
                    let p2 = s.find(|c: char| c == 'e' || c == 'E');
                    if p1.is_some() {
                        let p1 = p1.unwrap();
                        if p2.is_some() {
                            let p2 = p2.unwrap();
                            if p1 + 1 != p2 {
                                let p = f64::from_str(&s[..p1])?;
                                let q = Self::parse_frac(&s[p1 + 1..p2])?;
                                let e = i32::from_str_radix(&s[p2 + 1..], 10)?;
                                return Ok((p + q) * 10_f64.powi(e) * factor);
                            } else {
                                let p = f64::from_str(&s[..p1])?;
                                let e = i32::from_str_radix(&s[p2 + 1..], 10)?;
                                return Ok(p * 10_f64.powi(e) * factor);
                            }
                        } else {
                            if p1 + 1 != s.len() {
                                let p = f64::from_str(&s[..p1])?;
                                let q = Self::parse_frac(&s[p1 + 1..])?;
                                return Ok((p + q) * factor);
                            } else {
                                let p = f64::from_str(&s[..p1])?;
                                return Ok(p * factor);
                            }
                        }
                    } else {
                        if p2.is_some() {
                            let p2 = p2.unwrap();
                            let p = f64::from_str(&s[..p2])?;
                            let e = i32::from_str_radix(&s[p2 + 1..], 10)?;
                            return Ok(p * 10_f64.powi(e) * factor);
                        } else {
                            let p = f64::from_str(&s[..])?;
                            return Ok(p * factor);
                        }
                    }
                }
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    fn f64_from_hexstring(s: &str) -> Result<f64, ParseError> {
        match u64::from_str_radix(s, 16) {
            Ok(u) => Ok(u as f64),
            _ => {
                let mut v: f64 = 0.0;
                for c in s.chars() {
                    let mut s = String::new();
                    s.push(c);
                    v = v * 16_f64 + u8::from_str_radix(&s[..], 16)? as f64;
                }
                Ok(v)
            }
        }
    }
    fn parse_frac(s: &str) -> Result<f64, ParseError> {
        let mut d: f64 = 0.0;
        let mut first = true;
        for c in s.chars() {
            let mut t = String::new();
            t.push(c);
            if first {
                first = false;
                d = (u8::from_str_radix(&t[..], 10)? as f64) / 10_f64;
            } else {
                let p = u8::from_str_radix(&t[..], 10)? as f64;
                d = (d + p / 10_f64) / 10_f64;
            }
        }
        Ok(d)
    }
    fn parse_hexfrac(s: &str) -> Result<f64, ParseError> {
        let mut h: f64 = 0.0;
        let mut first = true;
        for c in s.chars() {
            let mut t = String::new();
            t.push(c);
            if first {
                first = false;
                h = (u8::from_str_radix(&t[..], 16)? as f64) / 16_f64;
            } else {
                let p = u8::from_str_radix(&t[..], 16)? as f64;
                h = (h + p / 16_f64) / 16_f64;
            }
        }
        Ok(h)
    }
    //6.3.3
    pub fn parse_string(c: &mut ParseContext) -> Result<String, ParseError> {
        match &c.cur_token.r#type {
            TokenType::STRING(buf) => {
                let s = String::from_utf8(buf.to_vec())?;
                c.next_token();
                Ok(s)
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    //6.3.4
    pub fn parse_name(c: &mut ParseContext) -> Result<Name, ParseError> {
        Ok(Self::parse_string(c)?)
    }
    //6.3.5
    pub fn parse_id(c: &mut ParseContext) -> Id {
        let (v, next) = match &c.cur_token.r#type {
            TokenType::ID(v) => (v.to_string(), true),
            _ => (String::new(), false),
        };
        if next {
            c.next_token();
            v
        } else {
            Self::generate_id(c)
        }
    }
    //6.4.1
    pub fn parse_valtype(c: &mut ParseContext) -> Result<ValType, ParseError> {
        match &c.cur_token.r#type {
            TokenType::I32 => {
                c.expect_cur(&TokenType::I32)?;
                Ok(ValType::I32)
            }
            TokenType::I64 => {
                c.expect_cur(&TokenType::I64)?;
                Ok(ValType::I64)
            }
            TokenType::F32 => {
                c.expect_cur(&TokenType::F32)?;
                Ok(ValType::F32)
            }
            TokenType::F64 => {
                c.expect_cur(&TokenType::F64)?;
                Ok(ValType::F64)
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    //6.4.2
    pub fn parse_functype(c: &mut ParseContext) -> Result<FuncType, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::FUNC)?;
        let mut params: Vec<Param> = vec![];
        while c.peek_token_is(&TokenType::PARAM) {
            let mut ps = Self::parse_param(c)?;
            while ps.len() != 0 {
                params.push(ps.remove(0));
            }
        }
        let mut results: Vec<TFResult> = vec![];
        while c.peek_token_is(&TokenType::RESULT) {
            let mut rs = Self::parse_result(c)?;
            while rs.len() != 0 {
                results.push(rs.remove(0));
            }
        }
        c.expect_cur(&TokenType::RPAREN)?;
        Ok(FuncType {
            params: params,
            results: results,
        })
    }
    fn parse_param(c: &mut ParseContext) -> Result<Vec<Param>, ParseError> {
        let mut params: Vec<Param> = vec![];
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::PARAM)?;
        match &c.cur_token.r#type {
            TokenType::ID(_) => {
                let id = Self::parse_id(c);
                let valtype = Self::parse_valtype(c)?;
                c.expect_cur(&TokenType::RPAREN)?;
                Ok(vec![Param {
                    id: id,
                    valtype: valtype,
                }])
            }
            _ => {
                loop {
                    if c.cur_token_is(&TokenType::RPAREN) {
                        break;
                    }
                    params.push(Param {
                        id: Self::generate_id(c),
                        valtype: Self::parse_valtype(c)?,
                    })
                }
                c.expect_cur(&TokenType::RPAREN)?;
                Ok(params)
            }
        }
    }
    fn parse_result(c: &mut ParseContext) -> Result<Vec<TFResult>, ParseError> {
        let mut results: Vec<TFResult> = vec![];
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::RESULT)?;
        loop {
            if c.cur_token_is(&TokenType::RPAREN) {
                break;
            }
            results.push(Self::parse_valtype(c)?)
        }
        c.expect_cur(&TokenType::RPAREN)?;
        Ok(results)
    }
    //6.4.3
    fn parse_limits(c: &mut ParseContext) -> Result<Limits, ParseError> {
        match &c.cur_token.r#type {
            TokenType::NUM(_) => {
                let min = Self::parse_u32(c)?;
                let max = match &c.cur_token.r#type {
                    TokenType::NUM(_) => Some(Self::parse_u32(c)?),
                    _ => None,
                };
                Ok(Limits { min: min, max: max })
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    //6.4.4
    fn parse_memtype(c: &mut ParseContext) -> Result<MemType, ParseError> {
        Ok(Self::parse_limits(c)?)
    }
    //6.4.5
    pub fn parse_tabletype(c: &mut ParseContext) -> Result<TableType, ParseError> {
        let limits = Self::parse_limits(c)?;
        let elemtype = Self::parse_elemtype(c)?;
        Ok(TableType {
            limits: limits,
            elemtype: elemtype,
        })
    }
    pub fn parse_elemtype(c: &mut ParseContext) -> Result<ElemType, ParseError> {
        match &c.cur_token.r#type {
            TokenType::FUNCREF => {
                c.expect_cur(&TokenType::FUNCREF)?;
                Ok(ElemType::FuncRef)
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    //6.4.6
    fn parse_globaltype(c: &mut ParseContext) -> Result<GlobalType, ParseError> {
        if c.cur_token_is(&TokenType::LPAREN) {
            c.expect_cur(&TokenType::LPAREN)?;
            c.expect_cur(&TokenType::MUT)?;
            let valtype = Self::parse_valtype(c)?;
            c.expect_cur(&TokenType::RPAREN)?;
            Ok(GlobalType {
                r#mut: true,
                valtype: valtype,
            })
        } else {
            let valtype = Self::parse_valtype(c)?;
            Ok(GlobalType {
                r#mut: false,
                valtype: valtype,
            })
        }
    }
    //6.5.1
    fn parse_label(c: &mut ParseContext) -> Result<Label, ParseError> {
        Ok(Self::parse_id(c))
    }
    //6.5
    pub fn parse_instr(c: &mut ParseContext) -> Result<Vec<Instr>, ParseError> {
        match &c.cur_token.r#type {
            TokenType::LPAREN => {
                c.expect_cur(&TokenType::LPAREN)?;
                match &c.cur_token.r#type {
                    TokenType::BLOCK => {
                        c.expect_cur(&TokenType::BLOCK)?;
                        let label = Self::parse_label(c)?;
                        let blocktype = Self::parse_blocktype(c)?;
                        let mut instrs: Vec<Instr> = vec![];
                        loop {
                            if c.cur_token_is(&TokenType::RPAREN) {
                                break;
                            }
                            let mut instr = Self::parse_instr(c)?;
                            while instr.len() != 0 {
                                instrs.push(instr.remove(0));
                            }
                        }
                        c.expect_cur(&TokenType::RPAREN)?;
                        Ok(vec![Instr::blockinstr(BlockInstr::block {
                            label: label,
                            blocktype: blocktype,
                            instrs: instrs,
                            id: Self::generate_id(c),
                        })])
                    }
                    TokenType::LOOP => {
                        c.expect_cur(&TokenType::LOOP)?;
                        let label = Self::parse_label(c)?;
                        let blocktype = Self::parse_blocktype(c)?;
                        let mut instrs: Vec<Instr> = vec![];
                        loop {
                            if c.cur_token_is(&TokenType::RPAREN) {
                                break;
                            }
                            let mut instr = Self::parse_instr(c)?;
                            while instr.len() != 0 {
                                instrs.push(instr.remove(0));
                            }
                        }
                        c.expect_cur(&TokenType::RPAREN)?;
                        Ok(vec![Instr::blockinstr(BlockInstr::r#loop {
                            label: label,
                            blocktype: blocktype,
                            instrs: instrs,
                            id: Self::generate_id(c),
                        })])
                    }
                    TokenType::IF => {
                        let mut instrs: Vec<Instr> = vec![];
                        c.expect_cur(&TokenType::IF)?;
                        let label = Self::parse_label(c)?;
                        let blocktype = Self::parse_blocktype(c)?;
                        loop {
                            if c.cur_token_is(&TokenType::LPAREN)
                                && c.peek_token_is(&TokenType::THEN)
                            {
                                break;
                            }
                            let mut foldedinstr = Self::parse_instr(c)?;
                            while foldedinstr.len() != 0 {
                                instrs.push(foldedinstr.remove(0));
                            }
                        }
                        c.expect_cur(&TokenType::LPAREN)?;
                        c.expect_cur(&TokenType::THEN)?;
                        let mut instrs1: Vec<Instr> = vec![];
                        loop {
                            if c.cur_token_is(&TokenType::RPAREN) {
                                break;
                            }
                            let mut instr = Self::parse_instr(c)?;
                            while instr.len() != 0 {
                                instrs1.push(instr.remove(0));
                            }
                        }
                        c.expect_cur(&TokenType::RPAREN)?;
                        let mut instrs2: Vec<Instr> = vec![];
                        if c.cur_token_is(&TokenType::LPAREN) {
                            c.expect_cur(&TokenType::LPAREN)?;
                            c.expect_cur(&TokenType::ELSE)?;
                            loop {
                                if c.cur_token_is(&TokenType::RPAREN) {
                                    break;
                                }
                                let mut instr = Self::parse_instr(c)?;
                                while instr.len() != 0 {
                                    instrs2.push(instr.remove(0));
                                }
                            }
                            c.expect_cur(&TokenType::RPAREN)?;
                        }
                        c.expect_cur(&TokenType::RPAREN)?;
                        instrs.push(Instr::blockinstr(BlockInstr::r#if {
                            label: label,
                            blocktype: blocktype,
                            instrs1: instrs1,
                            id1: Self::generate_id(c),
                            instrs2: instrs2,
                            id2: Self::generate_id(c),
                        }));
                        Ok(instrs)
                    }
                    _ => {
                        let mut instrs: Vec<Instr> = vec![];
                        let plaininstr = Self::parse_plaininstr(c)?;
                        loop {
                            if c.cur_token_is(&TokenType::RPAREN) {
                                break;
                            }
                            let mut foldedinstrs = Self::parse_instr(c)?;
                            while foldedinstrs.len() != 0 {
                                instrs.push(foldedinstrs.remove(0));
                            }
                        }
                        c.expect_cur(&TokenType::RPAREN)?;
                        instrs.push(Instr::plaininstr(plaininstr));
                        Ok(instrs)
                    }
                }
            }
            TokenType::BLOCK | TokenType::LOOP | TokenType::IF => {
                Ok(vec![Instr::blockinstr(Self::parse_blockinstr(c)?)])
            }
            _ => Ok(vec![Instr::plaininstr(Self::parse_plaininstr(c)?)]),
        }
    }
    fn parse_blocktype(c: &mut ParseContext) -> Result<BlockType, ParseError> {
        let mut typeuse = Self::parse_typeuse(c)?;
        if typeuse.params.len() == 0 {
            if typeuse.results.len() == 1 {
                Ok(BlockType::result(Some(typeuse.results.remove(0))))
            } else if typeuse.results.len() == 0 {
                Ok(BlockType::result(None))
            } else {
                Ok(BlockType::typeuse(typeuse))
            }
        } else {
            Ok(BlockType::typeuse(typeuse))
        }
    }
    fn parse_blockinstr(c: &mut ParseContext) -> Result<BlockInstr, ParseError> {
        match &c.cur_token.r#type {
            TokenType::BLOCK => {
                c.expect_cur(&TokenType::BLOCK)?;
                let label = Self::parse_label(c)?;
                let blocktype = Self::parse_blocktype(c)?;
                let mut instrs: Vec<Instr> = vec![];
                loop {
                    if c.cur_token_is(&TokenType::END) {
                        break;
                    }
                    let mut instr = Self::parse_instr(c)?;
                    while instr.len() != 0 {
                        instrs.push(instr.remove(0));
                    }
                }
                c.expect_cur(&TokenType::END)?;
                let id = Self::parse_id(c);
                Ok(BlockInstr::block {
                    label: label,
                    blocktype: blocktype,
                    instrs: instrs,
                    id: id,
                })
            }
            TokenType::LOOP => {
                c.expect_cur(&TokenType::LOOP)?;
                let label = Self::parse_label(c)?;
                let blocktype = Self::parse_blocktype(c)?;
                let mut instrs: Vec<Instr> = vec![];
                loop {
                    if c.cur_token_is(&TokenType::END) {
                        break;
                    }
                    let mut instr = Self::parse_instr(c)?;
                    while instr.len() != 0 {
                        instrs.push(instr.remove(0));
                    }
                }
                c.expect_cur(&TokenType::END)?;
                let id = Self::parse_id(c);
                Ok(BlockInstr::r#loop {
                    label: label,
                    blocktype: blocktype,
                    instrs: instrs,
                    id: id,
                })
            }
            TokenType::IF => {
                c.expect_cur(&TokenType::IF)?;
                let label = Self::parse_label(c)?;
                let blocktype = Self::parse_blocktype(c)?;
                let mut instrs1: Vec<Instr> = vec![];
                loop {
                    if c.cur_token_is(&TokenType::ELSE) {
                        break;
                    }
                    let mut instr = Self::parse_instr(c)?;
                    while instr.len() != 0 {
                        instrs1.push(instr.remove(0));
                    }
                }
                c.expect_cur(&TokenType::ELSE)?;
                let id1 = Self::parse_id(c);
                let mut instrs2: Vec<Instr> = vec![];
                loop {
                    if c.cur_token_is(&TokenType::END) {
                        break;
                    }
                    let mut instr = Self::parse_instr(c)?;
                    while instr.len() != 0 {
                        instrs2.push(instr.remove(0));
                    }
                }
                c.expect_cur(&TokenType::END)?;
                let id2 = Self::parse_id(c);
                Ok(BlockInstr::r#if {
                    label: label,
                    blocktype: blocktype,
                    instrs1: instrs1,
                    id1: id1,
                    instrs2: instrs2,
                    id2: id2,
                })
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    fn parse_plaininstr(c: &mut ParseContext) -> Result<PlainInstr, ParseError> {
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => match &keyword[..] {
                // 6.5.2
                "unreachable" => process_instr!(c, PlainInstr::unreachable),
                "nop" => process_instr!(c, PlainInstr::nop),
                "br" => process_instr!(c, PlainInstr::br(Self::parse_labelidx(c)?)),
                "br_if" => process_instr!(c, PlainInstr::br_if(Self::parse_labelidx(c)?)),
                "br_table" => {
                    c.expect_cur(&TokenType::KEYWORD("br_table".to_string()))?;
                    let mut labelidxes = Self::parse_vec_labelidx(c)?;
                    if labelidxes.len() < 1 {
                        return Err(ParseError::ParseError(c.cur_token.clone()));
                    }
                    let labelidx = labelidxes.remove(labelidxes.len() - 1);
                    Ok(PlainInstr::br_table {
                        labelidxes: labelidxes,
                        labelidx: labelidx,
                    })
                }
                "return" => process_instr!(c, PlainInstr::r#return),
                "call" => process_instr!(c, PlainInstr::call(Self::parse_funcidx(c)?)),
                "call_indirect" => {
                    process_instr!(c, PlainInstr::call_indirect(Self::parse_typeuse(c)?))
                }
                //6.5.3
                "drop" => process_instr!(c, PlainInstr::drop),
                "select" => process_instr!(c, PlainInstr::select),
                //6.5.4
                "local.get" => process_instr!(c, PlainInstr::local_get(Self::parse_localidx(c)?)),
                "local.set" => process_instr!(c, PlainInstr::local_set(Self::parse_localidx(c)?)),
                "local.tee" => process_instr!(c, PlainInstr::local_tee(Self::parse_localidx(c)?)),
                "global.get" => process_instr!(c, PlainInstr::global_get(Self::parse_localidx(c)?)),
                "global.set" => process_instr!(c, PlainInstr::global_set(Self::parse_localidx(c)?)),
                //6.5.5
                "i32.load" => process_instr!(c, PlainInstr::i32_load(Self::parse_memarg::<4>(c)?)),
                "i64.load" => process_instr!(c, PlainInstr::i64_load(Self::parse_memarg::<8>(c)?)),
                "f32.load" => process_instr!(c, PlainInstr::f32_load(Self::parse_memarg::<4>(c)?)),
                "f64.load" => process_instr!(c, PlainInstr::f64_load(Self::parse_memarg::<8>(c)?)),
                "i32.load8_s" => {
                    process_instr!(c, PlainInstr::i32_load8_s(Self::parse_memarg::<1>(c)?))
                }
                "i32.load8_u" => {
                    process_instr!(c, PlainInstr::i32_load8_u(Self::parse_memarg::<1>(c)?))
                }
                "i32.load16_s" => {
                    process_instr!(c, PlainInstr::i32_load16_s(Self::parse_memarg::<2>(c)?))
                }
                "i32.load16_u" => {
                    process_instr!(c, PlainInstr::i32_load16_u(Self::parse_memarg::<2>(c)?))
                }
                "i64.load8_s" => {
                    process_instr!(c, PlainInstr::i64_load8_s(Self::parse_memarg::<1>(c)?))
                }
                "i64.load8_u" => {
                    process_instr!(c, PlainInstr::i64_load8_u(Self::parse_memarg::<1>(c)?))
                }
                "i64.load16_s" => {
                    process_instr!(c, PlainInstr::i64_load16_s(Self::parse_memarg::<2>(c)?))
                }
                "i64.load16_u" => {
                    process_instr!(c, PlainInstr::i64_load16_u(Self::parse_memarg::<2>(c)?))
                }
                "i64.load32_s" => {
                    process_instr!(c, PlainInstr::i64_load32_s(Self::parse_memarg::<4>(c)?))
                }
                "i64.load32_u" => {
                    process_instr!(c, PlainInstr::i64_load32_u(Self::parse_memarg::<4>(c)?))
                }
                "i32.store" => {
                    process_instr!(c, PlainInstr::i32_store(Self::parse_memarg::<4>(c)?))
                }
                "i64.store" => {
                    process_instr!(c, PlainInstr::i64_store(Self::parse_memarg::<8>(c)?))
                }
                "f32.store" => {
                    process_instr!(c, PlainInstr::f32_store(Self::parse_memarg::<4>(c)?))
                }
                "f64.store" => {
                    process_instr!(c, PlainInstr::f64_store(Self::parse_memarg::<8>(c)?))
                }
                "i32.store8" => {
                    process_instr!(c, PlainInstr::i32_store8(Self::parse_memarg::<1>(c)?))
                }
                "i32.store16" => {
                    process_instr!(c, PlainInstr::i32_store16(Self::parse_memarg::<2>(c)?))
                }
                "i64.store8" => {
                    process_instr!(c, PlainInstr::i64_store8(Self::parse_memarg::<1>(c)?))
                }
                "i64.store16" => {
                    process_instr!(c, PlainInstr::i64_store16(Self::parse_memarg::<2>(c)?))
                }
                "i64.store32" => {
                    process_instr!(c, PlainInstr::i64_store32(Self::parse_memarg::<4>(c)?))
                }
                "memory.size" => process_instr!(c, PlainInstr::memory_size),
                "memory.grow" => process_instr!(c, PlainInstr::memory_grow),
                //6.5.6
                "i32.const" => process_instr!(c, PlainInstr::i32_const(Self::parse_i32(c)?)),
                "i64.const" => process_instr!(c, PlainInstr::i64_const(Self::parse_i64(c)?)),
                "f32.const" => process_instr!(c, PlainInstr::f32_const(Self::parse_f32(c)?)),
                "f64.const" => process_instr!(c, PlainInstr::f64_const(Self::parse_f64(c)?)),
                "i32.clz" => process_instr!(c, PlainInstr::i32_clz),
                "i32.ctz" => process_instr!(c, PlainInstr::i32_ctz),
                "i32.popcnt" => process_instr!(c, PlainInstr::i32_popcnt),
                "i32.add" => process_instr!(c, PlainInstr::i32_add),
                "i32.sub" => process_instr!(c, PlainInstr::i32_sub),
                "i32.mul" => process_instr!(c, PlainInstr::i32_mul),
                "i32.div_s" => process_instr!(c, PlainInstr::i32_div_s),
                "i32.div_u" => process_instr!(c, PlainInstr::i32_div_u),
                "i32.rem_s" => process_instr!(c, PlainInstr::i32_rem_s),
                "i32.rem_u" => process_instr!(c, PlainInstr::i32_rem_u),
                "i32.and" => process_instr!(c, PlainInstr::i32_and),
                "i32.or" => process_instr!(c, PlainInstr::i32_or),
                "i32.xor" => process_instr!(c, PlainInstr::i32_xor),
                "i32.shl" => process_instr!(c, PlainInstr::i32_shl),
                "i32.shr_s" => process_instr!(c, PlainInstr::i32_shr_s),
                "i32.shr_u" => process_instr!(c, PlainInstr::i32_shr_u),
                "i32.rotl" => process_instr!(c, PlainInstr::i32_rotl),
                "i32.rotr" => process_instr!(c, PlainInstr::i32_rotr),
                "i64.clz" => process_instr!(c, PlainInstr::i64_clz),
                "i64.ctz" => process_instr!(c, PlainInstr::i64_ctz),
                "i64.popcnt" => process_instr!(c, PlainInstr::i64_popcnt),
                "i64.add" => process_instr!(c, PlainInstr::i64_add),
                "i64.sub" => process_instr!(c, PlainInstr::i64_sub),
                "i64.mul" => process_instr!(c, PlainInstr::i64_mul),
                "i64.div_s" => process_instr!(c, PlainInstr::i64_div_s),
                "i64.div_u" => process_instr!(c, PlainInstr::i64_div_u),
                "i64.rem_s" => process_instr!(c, PlainInstr::i64_rem_s),
                "i64.rem_u" => process_instr!(c, PlainInstr::i64_rem_u),
                "i64.and" => process_instr!(c, PlainInstr::i64_and),
                "i64.or" => process_instr!(c, PlainInstr::i64_or),
                "i64.xor" => process_instr!(c, PlainInstr::i64_xor),
                "i64.shl" => process_instr!(c, PlainInstr::i64_shl),
                "i64.shr_s" => process_instr!(c, PlainInstr::i64_shr_s),
                "i64.shr_u" => process_instr!(c, PlainInstr::i64_shr_u),
                "i64.rotl" => process_instr!(c, PlainInstr::i64_rotl),
                "i64.rotr" => process_instr!(c, PlainInstr::i64_rotr),
                "f32.abs" => process_instr!(c, PlainInstr::f32_abs),
                "f32.neg" => process_instr!(c, PlainInstr::f32_neg),
                "f32.ceil" => process_instr!(c, PlainInstr::f32_ceil),
                "f32.floor" => process_instr!(c, PlainInstr::f32_floor),
                "f32.trunc" => process_instr!(c, PlainInstr::f32_trunc),
                "f32.nearest" => process_instr!(c, PlainInstr::f32_nearest),
                "f32.sqrt" => process_instr!(c, PlainInstr::f32_sqrt),
                "f32.add" => process_instr!(c, PlainInstr::f32_add),
                "f32.sub" => process_instr!(c, PlainInstr::f32_sub),
                "f32.mul" => process_instr!(c, PlainInstr::f32_mul),
                "f32.div" => process_instr!(c, PlainInstr::f32_div),
                "f32.min" => process_instr!(c, PlainInstr::f32_min),
                "f32.max" => process_instr!(c, PlainInstr::f32_max),
                "f32.copysign" => process_instr!(c, PlainInstr::f32_copysign),
                "f64.abs" => process_instr!(c, PlainInstr::f64_abs),
                "f64.neg" => process_instr!(c, PlainInstr::f64_neg),
                "f64.ceil" => process_instr!(c, PlainInstr::f64_ceil),
                "f64.floor" => process_instr!(c, PlainInstr::f64_floor),
                "f64.trunc" => process_instr!(c, PlainInstr::f64_trunc),
                "f64.nearest" => process_instr!(c, PlainInstr::f64_nearest),
                "f64.sqrt" => process_instr!(c, PlainInstr::f64_sqrt),
                "f64.add" => process_instr!(c, PlainInstr::f64_add),
                "f64.sub" => process_instr!(c, PlainInstr::f64_sub),
                "f64.mul" => process_instr!(c, PlainInstr::f64_mul),
                "f64.div" => process_instr!(c, PlainInstr::f64_div),
                "f64.min" => process_instr!(c, PlainInstr::f64_min),
                "f64.max" => process_instr!(c, PlainInstr::f64_max),
                "f64.copysign" => process_instr!(c, PlainInstr::f64_copysign),
                "i32.eqz" => process_instr!(c, PlainInstr::i32_eqz),
                "i32.eq" => process_instr!(c, PlainInstr::i32_eq),
                "i32.ne" => process_instr!(c, PlainInstr::i32_ne),
                "i32.lt_s" => process_instr!(c, PlainInstr::i32_lt_s),
                "i32.lt_u" => process_instr!(c, PlainInstr::i32_lt_u),
                "i32.gt_s" => process_instr!(c, PlainInstr::i32_gt_s),
                "i32.gt_u" => process_instr!(c, PlainInstr::i32_gt_u),
                "i32.le_s" => process_instr!(c, PlainInstr::i32_le_s),
                "i32.le_u" => process_instr!(c, PlainInstr::i32_le_u),
                "i32.ge_s" => process_instr!(c, PlainInstr::i32_ge_s),
                "i32.ge_u" => process_instr!(c, PlainInstr::i32_ge_u),
                "i64.eqz" => process_instr!(c, PlainInstr::i64_eqz),
                "i64.eq" => process_instr!(c, PlainInstr::i64_eq),
                "i64.ne" => process_instr!(c, PlainInstr::i64_ne),
                "i64.lt_s" => process_instr!(c, PlainInstr::i64_lt_s),
                "i64.lt_u" => process_instr!(c, PlainInstr::i64_lt_u),
                "i64.gt_s" => process_instr!(c, PlainInstr::i64_gt_s),
                "i64.gt_u" => process_instr!(c, PlainInstr::i64_gt_u),
                "i64.le_s" => process_instr!(c, PlainInstr::i64_le_s),
                "i64.le_u" => process_instr!(c, PlainInstr::i64_le_u),
                "i64.ge_s" => process_instr!(c, PlainInstr::i64_ge_s),
                "i64.ge_u" => process_instr!(c, PlainInstr::i64_ge_u),
                "f32.eq" => process_instr!(c, PlainInstr::f32_eq),
                "f32.ne" => process_instr!(c, PlainInstr::f32_ne),
                "f32.lt" => process_instr!(c, PlainInstr::f32_lt),
                "f32.gt" => process_instr!(c, PlainInstr::f32_gt),
                "f32.le" => process_instr!(c, PlainInstr::f32_le),
                "f32.ge" => process_instr!(c, PlainInstr::f32_ge),
                "f64.eq" => process_instr!(c, PlainInstr::f64_eq),
                "f64.ne" => process_instr!(c, PlainInstr::f64_ne),
                "f64.lt" => process_instr!(c, PlainInstr::f64_lt),
                "f64.gt" => process_instr!(c, PlainInstr::f64_gt),
                "f64.le" => process_instr!(c, PlainInstr::f64_le),
                "f64.ge" => process_instr!(c, PlainInstr::f64_ge),
                "i32.wrap_i64" => process_instr!(c, PlainInstr::i32_wrap_i64),
                "i32.trunc_f32_s" => process_instr!(c, PlainInstr::i32_trunc_f32_s),
                "i32.trunc_f32_u" => process_instr!(c, PlainInstr::i32_trunc_f32_u),
                "i32.trunc_f64_s" => process_instr!(c, PlainInstr::i32_trunc_f64_s),
                "i32.trunc_f64_u" => process_instr!(c, PlainInstr::i32_trunc_f64_u),
                "i32.trunc_sat_f32_s" => process_instr!(c, PlainInstr::i32_trunc_sat_f32_s),
                "i32.trunc_sat_f32_u" => process_instr!(c, PlainInstr::i32_trunc_sat_f32_u),
                "i32.trunc_sat_f64_s" => process_instr!(c, PlainInstr::i32_trunc_sat_f64_s),
                "i32.trunc_sat_f64_u" => process_instr!(c, PlainInstr::i32_trunc_sat_f64_u),
                "i64.extend_i32_s" => process_instr!(c, PlainInstr::i64_extend_i32_s),
                "i64.extend_i32_u" => process_instr!(c, PlainInstr::i64_extend_i32_u),
                "i64.trunc_f32_s" => process_instr!(c, PlainInstr::i64_trunc_f32_s),
                "i64.trunc_f32_u" => process_instr!(c, PlainInstr::i64_trunc_f32_u),
                "i64.trunc_f64_s" => process_instr!(c, PlainInstr::i64_trunc_f64_s),
                "i64.trunc_f64_u" => process_instr!(c, PlainInstr::i64_trunc_f64_u),
                "i64.trunc_sat_f32_s" => process_instr!(c, PlainInstr::i64_trunc_sat_f32_s),
                "i64.trunc_sat_f32_u" => process_instr!(c, PlainInstr::i64_trunc_sat_f32_u),
                "i64.trunc_sat_f64_s" => process_instr!(c, PlainInstr::i64_trunc_sat_f64_s),
                "i64.trunc_sat_f64_u" => process_instr!(c, PlainInstr::i64_trunc_sat_f64_u),
                "f32.convert_i32_s" => process_instr!(c, PlainInstr::f32_convert_i32_s),
                "f32.convert_i32_u" => process_instr!(c, PlainInstr::f32_convert_i32_u),
                "f32.convert_i64_s" => process_instr!(c, PlainInstr::f32_convert_i64_s),
                "f32.convert_i64_u" => process_instr!(c, PlainInstr::f32_convert_i64_u),
                "f32.demote_f64" => process_instr!(c, PlainInstr::f32_demote_f64),
                "f64.convert_i32_s" => process_instr!(c, PlainInstr::f64_convert_i32_s),
                "f64.convert_i32_u" => process_instr!(c, PlainInstr::f64_convert_i32_u),
                "f64.convert_i64_s" => process_instr!(c, PlainInstr::f64_convert_i64_s),
                "f64.convert_i64_u" => process_instr!(c, PlainInstr::f64_convert_i64_u),
                "f64.promote_f32" => process_instr!(c, PlainInstr::f64_promote_f32),
                "i32.reinterpret_f32" => process_instr!(c, PlainInstr::i32_reinterpret_f32),
                "i64.reinterpret_f64" => process_instr!(c, PlainInstr::i64_reinterpret_f64),
                "f32.reinterpret_i32" => process_instr!(c, PlainInstr::f32_reinterpret_i32),
                "f64.reinterpret_i64" => process_instr!(c, PlainInstr::f64_reinterpret_i64),
                "i32.extend8_s" => process_instr!(c, PlainInstr::i32_extend8_s),
                "i32.extend16_s" => process_instr!(c, PlainInstr::i32_extend16_s),
                "i64.extend8_s" => process_instr!(c, PlainInstr::i64_extend8_s),
                "i64.extend16_s" => process_instr!(c, PlainInstr::i64_extend16_s),
                "i64.extend32_s" => process_instr!(c, PlainInstr::i64_extend32_s),
                _ => Err(ParseError::ParseError(c.cur_token.clone())),
            },
            TokenType::END => process_instr!(c, PlainInstr::end),
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    fn parse_vec_labelidx(c: &mut ParseContext) -> Result<Vec<LabelIdx>, ParseError> {
        let mut labelidxes: Vec<LabelIdx> = vec![];
        loop {
            match &c.cur_token.r#type {
                TokenType::NUM(_) | TokenType::ID(_) => labelidxes.push(Self::parse_labelidx(c)?),
                _ => break,
            }
        }
        Ok(labelidxes)
    }
    fn parse_memarg<const N: u32>(c: &mut ParseContext) -> Result<MemArg<N>, ParseError> {
        let mut offset = 0;
        let mut align = N;
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => {
                if keyword.starts_with("offset=") {
                    offset = Self::parse_offset(c)?;
                    match &c.cur_token.r#type {
                        TokenType::KEYWORD(keyword) => {
                            if keyword.starts_with("align=") {
                                align = Self::parse_align::<N>(c)?;
                                Ok(MemArg::<N> {
                                    offset: offset,
                                    align: Align::<N>::new(align),
                                })
                            } else {
                                Ok(MemArg::<N> {
                                    offset: offset,
                                    align: Align::<N>::new(align),
                                })
                            }
                        }
                        _ => Ok(MemArg::<N> {
                            offset: offset,
                            align: Align::<N>::new(align),
                        }),
                    }
                } else if keyword.starts_with("align=") {
                    align = Self::parse_align::<N>(c)?;
                    Ok(MemArg::<N> {
                        offset: offset,
                        align: Align::<N>::new(align),
                    })
                } else {
                    Ok(MemArg::<N> {
                        offset: offset,
                        align: Align::<N>::new(align),
                    })
                }
            }
            _ => Ok(MemArg::<N> {
                offset: offset,
                align: Align::<N>::new(align),
            }),
        }
    }
    fn parse_offset(c: &mut ParseContext) -> Result<u32, ParseError> {
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => {
                let s = keyword[7..].to_string();
                c.next_token();
                Ok(Self::read_u32(s)?)
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    fn parse_align<const N: u32>(c: &mut ParseContext) -> Result<u32, ParseError> {
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => {
                let s = keyword[6..].to_string();
                c.next_token();
                Ok(Self::read_u32(s)?)
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    //6.5.8
    pub fn parse_expr(c: &mut ParseContext) -> Result<Expr, ParseError> {
        let mut instrs: Vec<Instr> = vec![];
        loop {
            if c.cur_token_is(&TokenType::END) || c.cur_token_is(&TokenType::RPAREN) {
                break;
            }
            let mut instr = Self::parse_instr(c)?;
            while instr.len() != 0 {
                instrs.push(instr.remove(0));
            }
        }
        Ok(Expr { instrs: instrs })
    }
    //6.6.1
    fn parse_idx(c: &mut ParseContext) -> Result<Idx, ParseError> {
        match &c.cur_token.r#type {
            TokenType::NUM(_) => Ok(Idx::x(Self::parse_u32(c)?)),
            TokenType::ID(_) => Ok(Idx::v(Self::parse_id(c))),
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    fn parse_typeidx(c: &mut ParseContext) -> Result<TypeIdx, ParseError> {
        Ok(Self::parse_idx(c)?)
    }
    fn parse_funcidx(c: &mut ParseContext) -> Result<FuncIdx, ParseError> {
        Ok(Self::parse_idx(c)?)
    }
    fn parse_tableidx(c: &mut ParseContext) -> Result<TableIdx, ParseError> {
        Ok(Self::parse_idx(c)?)
    }
    fn parse_memidx(c: &mut ParseContext) -> Result<MemIdx, ParseError> {
        Ok(Self::parse_idx(c)?)
    }
    fn parse_globalidx(c: &mut ParseContext) -> Result<GlobalIdx, ParseError> {
        Ok(Self::parse_idx(c)?)
    }
    fn parse_localidx(c: &mut ParseContext) -> Result<LocalIdx, ParseError> {
        Ok(Self::parse_idx(c)?)
    }
    fn parse_labelidx(c: &mut ParseContext) -> Result<LabelIdx, ParseError> {
        Ok(Self::parse_idx(c)?)
    }
    //6.6.2
    fn parse_types(c: &mut ParseContext) -> Result<Vec<ModuleField>, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::TYPE)?;
        let id = Self::parse_id(c);
        let functype = Self::parse_functype(c)?;
        c.expect_cur(&TokenType::RPAREN)?;
        Ok(vec![ModuleField::types(Type {
            id: id,
            functype: functype,
        })])
    }
    //6.6.3
    pub fn parse_typeuse(c: &mut ParseContext) -> Result<TypeUse, ParseError> {
        if c.cur_token_is(&TokenType::LPAREN)
            && (c.peek_token_is(&TokenType::TYPE)
                || c.peek_token_is(&TokenType::PARAM)
                || c.peek_token_is(&TokenType::RESULT))
        {
            match &c.peek_token.r#type {
                TokenType::PARAM => {
                    let params = Self::parse_vec_param(c)?;
                    let results = Self::parse_vec_result(c)?;
                    Ok(TypeUse {
                        typeidx: TypeIdx::x(u32::MAX), //placeholder
                        params: params,
                        results: results,
                    })
                }
                TokenType::RESULT => {
                    let results = Self::parse_vec_result(c)?;
                    Ok(TypeUse {
                        typeidx: TypeIdx::x(u32::MAX), //placeholder
                        params: vec![],
                        results: results,
                    })
                }
                TokenType::TYPE => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::TYPE)?;
                    if c.cur_token_is(&TokenType::LPAREN) {
                        c.expect_cur(&TokenType::LPAREN)?;
                        c.expect_cur(&TokenType::FUNC)?;
                        let params = Self::parse_vec_param(c)?;
                        let results = Self::parse_vec_result(c)?;
                        c.expect_cur(&TokenType::RPAREN)?;
                        c.expect_cur(&TokenType::RPAREN)?;
                        Ok(TypeUse {
                            typeidx: TypeIdx::x(u32::MAX), //placeholder
                            params: params,
                            results: results,
                        })
                    } else {
                        let typeidx = Self::parse_typeidx(c)?;
                        c.expect_cur(&TokenType::RPAREN)?;
                        let params = Self::parse_vec_param(c)?;
                        let results = Self::parse_vec_result(c)?;
                        Ok(TypeUse {
                            typeidx: typeidx,
                            params: params,
                            results: results,
                        })
                    }
                }
                _ => Err(ParseError::ParseError(c.cur_token.clone())),
            }
        } else {
            Ok(TypeUse {
                typeidx: TypeIdx::x(u32::MAX), //placeholder,
                params: vec![],
                results: vec![],
            })
        }
    }
    fn parse_vec_param(c: &mut ParseContext) -> Result<Vec<Param>, ParseError> {
        let mut params: Vec<Param> = vec![];
        loop {
            if (!c.cur_token_is(&TokenType::LPAREN)) || (!c.peek_token_is(&TokenType::PARAM)) {
                break;
            }
            let mut param = Self::parse_param(c)?;
            while param.len() != 0 {
                params.push(param.remove(0));
            }
        }
        Ok(params)
    }
    fn parse_vec_result(c: &mut ParseContext) -> Result<Vec<TFResult>, ParseError> {
        let mut results: Vec<TFResult> = vec![];
        loop {
            if (!c.cur_token_is(&TokenType::LPAREN)) || (!c.peek_token_is(&TokenType::RESULT)) {
                break;
            }
            let mut result = Self::parse_result(c)?;
            while result.len() != 0 {
                results.push(result.remove(0));
            }
        }
        Ok(results)
    }
    //6.6.4
    fn parse_imports(c: &mut ParseContext) -> Result<Vec<ModuleField>, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::IMPORT)?;
        let module = Self::parse_name(c)?;
        let name = Self::parse_name(c)?;
        let desc = Self::parse_importdesc(c)?;
        c.expect_cur(&TokenType::RPAREN)?;
        Ok(vec![ModuleField::imports(Import {
            module: module,
            name: name,
            desc: desc,
        })])
    }
    pub fn parse_importdesc(c: &mut ParseContext) -> Result<ImportDesc, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        match &c.cur_token.r#type {
            TokenType::FUNC => {
                c.expect_cur(&TokenType::FUNC)?;
                let id = Self::parse_id(c);
                let func = Self::parse_typeuse(c)?;
                c.expect_cur(&TokenType::RPAREN)?;
                Ok(ImportDesc::func { id: id, func: func })
            }
            TokenType::TABLE => {
                c.expect_cur(&TokenType::TABLE)?;
                let id = Self::parse_id(c);
                let table = Self::parse_tabletype(c)?;
                c.expect_cur(&TokenType::RPAREN)?;
                Ok(ImportDesc::table {
                    id: id,
                    table: table,
                })
            }
            TokenType::MEMORY => {
                c.expect_cur(&TokenType::MEMORY)?;
                let id = Self::parse_id(c);
                let mem = Self::parse_memtype(c)?;
                c.expect_cur(&TokenType::RPAREN)?;
                Ok(ImportDesc::mem { id: id, mem: mem })
            }
            TokenType::GLOBAL => {
                c.expect_cur(&TokenType::GLOBAL)?;
                let id = Self::parse_id(c);
                let global = Self::parse_globaltype(c)?;
                Ok(ImportDesc::global {
                    id: id,
                    global: global,
                })
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    //6.6.5
    fn parse_funcs(c: &mut ParseContext) -> Result<Vec<ModuleField>, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::FUNC)?;
        let id = Self::parse_id(c);
        if c.cur_token_is(&TokenType::LPAREN) {
            match &c.peek_token.r#type {
                TokenType::IMPORT => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::IMPORT)?;
                    let module = Self::parse_name(c)?;
                    let name = Self::parse_name(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    let typeuse = Self::parse_typeuse(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(vec![
                        ModuleField::funcs(Func {
                            id: id.clone(),
                            r#type: typeuse.clone(),
                            locals: vec![],
                            body: vec![],
                        }),
                        ModuleField::imports(Import {
                            module: module,
                            name: name,
                            desc: ImportDesc::func {
                                id: id,
                                func: typeuse,
                            },
                        }),
                    ])
                }
                TokenType::EXPORT => {
                    let mut temp: Vec<ModuleField> = vec![];
                    let mut modulefields: Vec<ModuleField> = vec![];
                    loop {
                        if c.cur_token_is(&TokenType::LPAREN)
                            && (c.peek_token_is(&TokenType::IMPORT)
                                || c.peek_token_is(&TokenType::EXPORT))
                        {
                            match &c.peek_token.r#type {
                                TokenType::IMPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::IMPORT)?;
                                    let module = Self::parse_name(c)?;
                                    let name = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::imports(Import {
                                        module: module,
                                        name: name,
                                        desc: ImportDesc::func {
                                            id: id.clone(),
                                            func: TypeUse {
                                                typeidx: TypeIdx::x(0),
                                                params: vec![],
                                                results: vec![],
                                            },
                                        }, // placeholder
                                    }))
                                }
                                TokenType::EXPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::EXPORT)?;
                                    let name = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::exports(Export {
                                        name: name,
                                        desc: ExportDesc::func(FuncIdx::x(0)), // placeholder
                                    }));
                                }
                                _ => return Err(ParseError::ParseError(c.cur_token.clone())),
                            }
                        } else {
                            let typeuse = Self::parse_typeuse(c)?;
                            let locals = Self::parse_vec_local(c)?;
                            let instrs = Self::parse_vec_instr(c)?;
                            c.expect_cur(&TokenType::RPAREN)?;
                            modulefields.push(ModuleField::funcs(Func {
                                id: id.clone(),
                                r#type: typeuse.clone(),
                                locals: locals,
                                body: instrs,
                            }));
                            while temp.len() != 0 {
                                match temp.remove(0) {
                                    ModuleField::imports(Import {
                                        module,
                                        name,
                                        desc: _,
                                    }) => {
                                        modulefields.push(ModuleField::imports(Import {
                                            module: module,
                                            name: name,
                                            desc: ImportDesc::func {
                                                id: id.clone(),
                                                func: typeuse.clone(),
                                            },
                                        }));
                                    }
                                    ModuleField::exports(Export { name, desc: _ }) => modulefields
                                        .push(ModuleField::exports(Export {
                                            name: name,
                                            desc: ExportDesc::func(FuncIdx::v(id.clone())),
                                        })),
                                    _ => return Err(ParseError::ParseError(c.cur_token.clone())),
                                }
                            }
                            return Ok(modulefields);
                        }
                    }
                }
                _ => {
                    let r#type = Self::parse_typeuse(c)?;
                    let locals = Self::parse_vec_local(c)?;
                    let body = Self::parse_vec_instr(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(vec![ModuleField::funcs(Func {
                        id: id,
                        r#type: r#type,
                        locals: locals,
                        body: body,
                    })])
                }
            }
        } else if c.cur_token_is(&TokenType::RPAREN) {
            c.expect_cur(&TokenType::RPAREN)?;
            Ok(vec![ModuleField::funcs(Func {
                id: id,
                r#type: TypeUse {
                    typeidx: TypeIdx::x(u32::MAX), //placeholder
                    params: vec![],
                    results: vec![],
                },
                locals: vec![],
                body: vec![],
            })])
        } else {
            Err(ParseError::ParseError(c.cur_token.clone()))
        }
    }
    fn parse_vec_local(c: &mut ParseContext) -> Result<Vec<Local>, ParseError> {
        let mut locals: Vec<Local> = vec![];
        loop {
            if (!c.cur_token_is(&TokenType::LPAREN)) || (!c.peek_token_is(&TokenType::LOCAL)) {
                break;
            }
            let mut local = Self::parse_local(c)?;
            while local.len() != 0 {
                locals.push(local.remove(0));
            }
        }
        Ok(locals)
    }
    fn parse_local(c: &mut ParseContext) -> Result<Vec<Local>, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::LOCAL)?;
        let id = Self::parse_id(c);
        let mut locals: Vec<Local> = vec![];
        let mut valtypes = Self::parse_vec_valtype(c)?;
        c.expect_cur(&TokenType::RPAREN)?;
        while valtypes.len() != 0 {
            locals.push(Local {
                id: Self::generate_id(c),
                valtype: valtypes.remove(0),
            })
        }
        Ok(locals)
    }
    fn parse_vec_valtype(c: &mut ParseContext) -> Result<Vec<ValType>, ParseError> {
        let mut valtypes: Vec<ValType> = vec![];
        loop {
            if c.cur_token_is(&TokenType::RPAREN) {
                break;
            }
            valtypes.push(Self::parse_valtype(c)?);
        }
        Ok(valtypes)
    }
    fn parse_vec_instr(c: &mut ParseContext) -> Result<Vec<Instr>, ParseError> {
        let mut instrs: Vec<Instr> = vec![];
        loop {
            if c.cur_token_is(&TokenType::RPAREN) {
                break;
            }
            let mut instr = Self::parse_instr(c)?;
            while instr.len() != 0 {
                instrs.push(instr.remove(0));
            }
        }
        Ok(instrs)
    }
    //6.6.6
    fn parse_tables(c: &mut ParseContext) -> Result<Vec<ModuleField>, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::TABLE)?;
        let id = Self::parse_id(c);
        if c.cur_token_is(&TokenType::LPAREN) {
            match &c.peek_token.r#type {
                TokenType::IMPORT => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::IMPORT)?;
                    let module = Self::parse_name(c)?;
                    let name = Self::parse_name(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    let tabletype = Self::parse_tabletype(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(vec![
                        ModuleField::tables(Table {
                            id: id.clone(),
                            r#type: tabletype.clone(),
                        }),
                        ModuleField::imports(Import {
                            module: module,
                            name: name,
                            desc: ImportDesc::table {
                                id: id.clone(),
                                table: tabletype,
                            },
                        }),
                    ])
                }
                TokenType::EXPORT => {
                    let mut temp: Vec<ModuleField> = vec![];
                    let mut modulefields: Vec<ModuleField> = vec![];
                    loop {
                        if c.cur_token_is(&TokenType::LPAREN)
                            && (c.peek_token_is(&TokenType::IMPORT)
                                || c.peek_token_is(&TokenType::EXPORT))
                        {
                            match &c.peek_token.r#type {
                                TokenType::IMPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::IMPORT)?;
                                    let module = Self::parse_name(c)?;
                                    let name = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::imports(Import {
                                        module: module,
                                        name: name,
                                        desc: ImportDesc::table {
                                            id: id.clone(),
                                            table: TableType {
                                                limits: Limits { min: 0, max: None },
                                                elemtype: ElemType::FuncRef,
                                            },
                                        },
                                    }));
                                }
                                TokenType::EXPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::EXPORT)?;
                                    let name = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::exports(Export {
                                        name: name,
                                        desc: ExportDesc::table(TableIdx::x(0)), //placeholder
                                    }))
                                }
                                _ => return Err(ParseError::ParseError(c.cur_token.clone())),
                            }
                        } else {
                            let tabletype = Self::parse_tabletype(c)?;
                            c.expect_cur(&TokenType::RPAREN)?;
                            modulefields.push(ModuleField::tables(Table {
                                id: id.clone(),
                                r#type: tabletype.clone(),
                            }));
                            while temp.len() != 0 {
                                match temp.remove(0) {
                                    ModuleField::imports(Import {
                                        module,
                                        name,
                                        desc: _,
                                    }) => {
                                        modulefields.push(ModuleField::imports(Import {
                                            module: module,
                                            name,
                                            desc: ImportDesc::table {
                                                id: id.clone(),
                                                table: tabletype.clone(),
                                            },
                                        }));
                                    }
                                    ModuleField::exports(Export { name, desc: _ }) => modulefields
                                        .push(ModuleField::exports(Export {
                                            name: name,
                                            desc: ExportDesc::table(TableIdx::v(id.clone())),
                                        })),
                                    _ => return Err(ParseError::ParseError(c.cur_token.clone())),
                                }
                            }
                            return Ok(modulefields);
                        }
                    }
                }
                _ => return Err(ParseError::ParseError(c.cur_token.clone())),
            }
        } else if c.cur_token_is(&TokenType::FUNCREF) {
            let elemtype = Self::parse_elemtype(c)?;
            c.expect_cur(&TokenType::LPAREN)?;
            c.expect_cur(&TokenType::ELEM)?;
            let funcidxes = Self::parse_vec_funcidx(c)?;
            let n = funcidxes.len() as u32;
            c.expect_cur(&TokenType::RPAREN)?;
            c.expect_cur(&TokenType::RPAREN)?;
            Ok(vec![
                ModuleField::tables(Table {
                    id: id.clone(),
                    r#type: TableType {
                        limits: Limits {
                            min: n,
                            max: Some(n),
                        },
                        elemtype: elemtype,
                    },
                }),
                ModuleField::elem(Elem {
                    table: TableIdx::v(id.clone()),
                    offset: Expr {
                        instrs: vec![Instr::plaininstr(PlainInstr::i32_const(0))],
                    },
                    init: funcidxes,
                }),
            ])
        } else {
            let tabletype = Self::parse_tabletype(c)?;
            c.expect_cur(&TokenType::RPAREN)?;
            Ok(vec![ModuleField::tables(Table {
                id: id,
                r#type: tabletype,
            })])
        }
    }
    pub fn parse_vec_funcidx(c: &mut ParseContext) -> Result<Vec<FuncIdx>, ParseError> {
        let mut funcidxes: Vec<FuncIdx> = vec![];
        loop {
            if c.cur_token_is(&TokenType::RPAREN) {
                break;
            }
            funcidxes.push(Self::parse_funcidx(c)?);
        }
        Ok(funcidxes)
    }
    //6.6.7
    fn parse_mems(c: &mut ParseContext) -> Result<Vec<ModuleField>, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::MEMORY)?;
        let id = Self::parse_id(c);
        if c.cur_token_is(&TokenType::LPAREN) {
            match &c.peek_token.r#type {
                TokenType::IMPORT => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::IMPORT)?;
                    let module = Self::parse_name(c)?;
                    let name = Self::parse_name(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    let memtype = Self::parse_memtype(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(vec![
                        ModuleField::mems(Mem {
                            id: id.clone(),
                            r#type: memtype.clone(),
                        }),
                        ModuleField::imports(Import {
                            module: module,
                            name: name,
                            desc: ImportDesc::mem {
                                id: id.clone(),
                                mem: memtype,
                            },
                        }),
                    ])
                }
                TokenType::EXPORT => {
                    let mut temp: Vec<ModuleField> = vec![];
                    let mut modulefields: Vec<ModuleField> = vec![];
                    loop {
                        if c.cur_token_is(&TokenType::LPAREN)
                            && (c.peek_token_is(&TokenType::IMPORT)
                                || c.peek_token_is(&TokenType::EXPORT))
                        {
                            match &c.peek_token.r#type {
                                TokenType::IMPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::IMPORT)?;
                                    let module = Self::parse_name(c)?;
                                    let name = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::imports(Import {
                                        module: module,
                                        name: name,
                                        desc: ImportDesc::mem {
                                            id: id.clone(),
                                            mem: Limits { min: 0, max: None },
                                        },
                                    }));
                                }
                                TokenType::EXPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::EXPORT)?;
                                    let name = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::exports(Export {
                                        name: name,
                                        desc: ExportDesc::mem(MemIdx::x(0)), //placeholder
                                    }))
                                }
                                _ => return Err(ParseError::ParseError(c.cur_token.clone())),
                            }
                        } else {
                            let memtype = Self::parse_memtype(c)?;
                            c.expect_cur(&TokenType::RPAREN)?;
                            modulefields.push(ModuleField::mems(Mem {
                                id: id.clone(),
                                r#type: memtype.clone(),
                            }));
                            while temp.len() != 0 {
                                match temp.remove(0) {
                                    ModuleField::imports(Import {
                                        module,
                                        name,
                                        desc: _,
                                    }) => {
                                        modulefields.push(ModuleField::imports(Import {
                                            module: module,
                                            name,
                                            desc: ImportDesc::mem {
                                                id: id.clone(),
                                                mem: memtype.clone(),
                                            },
                                        }));
                                    }
                                    ModuleField::exports(Export { name, desc: _ }) => modulefields
                                        .push(ModuleField::exports(Export {
                                            name: name,
                                            desc: ExportDesc::mem(MemIdx::v(id.clone())),
                                        })),
                                    _ => return Err(ParseError::ParseError(c.cur_token.clone())),
                                }
                            }
                            return Ok(modulefields);
                        }
                    }
                }
                TokenType::DATA => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::DATA)?;
                    let datastring = Self::parse_vec_string(c)?;
                    let n = datastring.len() as f64;
                    let m = (n / (64_f64 * 1024_f64)).ceil() as u32;
                    c.expect_cur(&TokenType::RPAREN)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(vec![
                        ModuleField::mems(Mem {
                            id: id.clone(),
                            r#type: MemType {
                                min: m,
                                max: Some(m),
                            },
                        }),
                        ModuleField::data(Data {
                            data: MemIdx::v(id.clone()),
                            offset: Expr {
                                instrs: vec![Instr::plaininstr(PlainInstr::i32_const(0))],
                            },
                            init: datastring,
                        }),
                    ])
                }
                _ => Err(ParseError::ParseError(c.cur_token.clone())),
            }
        } else {
            let memtype = Self::parse_memtype(c)?;
            c.expect_cur(&TokenType::RPAREN)?;
            Ok(vec![ModuleField::mems(Mem {
                id: id,
                r#type: memtype,
            })])
        }
    }
    pub fn parse_vec_string(c: &mut ParseContext) -> Result<Vec<Vec<u8>>, ParseError> {
        let mut strs: Vec<Vec<u8>> = vec![];
        loop {
            match &c.cur_token.r#type {
                TokenType::STRING(buf) => {
                    strs.push(buf.to_vec());
                    c.next_token();
                }
                _ => return Ok(strs),
            }
        }
    }
    //6.6.8
    fn parse_globals(c: &mut ParseContext) -> Result<Vec<ModuleField>, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::GLOBAL)?;
        let id = Self::parse_id(c);
        if c.cur_token_is(&TokenType::LPAREN) {
            match &c.peek_token.r#type {
                TokenType::IMPORT => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::IMPORT)?;
                    let module = Self::parse_name(c)?;
                    let name = Self::parse_name(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    let globaltype = Self::parse_globaltype(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(vec![
                        ModuleField::globals(Global {
                            id: id.clone(),
                            r#type: globaltype.clone(),
                            init: Expr { instrs: vec![] },
                        }),
                        ModuleField::imports(Import {
                            module: module,
                            name: name,
                            desc: ImportDesc::global {
                                id: id.clone(),
                                global: globaltype,
                            },
                        }),
                    ])
                }
                TokenType::EXPORT => {
                    let mut temp: Vec<ModuleField> = vec![];
                    let mut modulefields: Vec<ModuleField> = vec![];
                    loop {
                        if c.cur_token_is(&TokenType::LPAREN)
                            && (c.peek_token_is(&TokenType::IMPORT)
                                || c.peek_token_is(&TokenType::EXPORT))
                        {
                            match &c.peek_token.r#type {
                                TokenType::IMPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::IMPORT)?;
                                    let module = Self::parse_name(c)?;
                                    let name = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::imports(Import {
                                        module: module,
                                        name: name,
                                        desc: ImportDesc::global {
                                            id: id.clone(),
                                            global: GlobalType {
                                                r#mut: false,
                                                valtype: ValType::I32,
                                            },
                                        }, //placeholder
                                    }))
                                }
                                TokenType::EXPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::EXPORT)?;
                                    let name = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::exports(Export {
                                        name: name,
                                        desc: ExportDesc::global(GlobalIdx::x(0)), //placeholder
                                    }))
                                }
                                _ => return Err(ParseError::ParseError(c.cur_token.clone())),
                            }
                        } else {
                            let globaltype = Self::parse_globaltype(c)?;
                            let init = Self::parse_expr(c)?;
                            c.expect_cur(&TokenType::RPAREN)?;
                            modulefields.push(ModuleField::globals(Global {
                                id: id.clone(),
                                r#type: globaltype.clone(),
                                init: init,
                            }));
                            while temp.len() != 0 {
                                match temp.remove(0) {
                                    ModuleField::imports(Import {
                                        module,
                                        name,
                                        desc: _,
                                    }) => modulefields.push(ModuleField::imports(Import {
                                        module: module,
                                        name: name,
                                        desc: ImportDesc::global {
                                            id: id.clone(),
                                            global: globaltype.clone(),
                                        },
                                    })),
                                    ModuleField::exports(Export { name, desc: _ }) => modulefields
                                        .push(ModuleField::exports(Export {
                                            name: name,
                                            desc: ExportDesc::global(GlobalIdx::v(id.clone())),
                                        })),
                                    _ => return Err(ParseError::ParseError(c.cur_token.clone())),
                                }
                            }
                            return Ok(modulefields);
                        }
                    }
                }
                _ => {
                    let r#type = Self::parse_globaltype(c)?;
                    let init = Self::parse_expr(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    Ok(vec![ModuleField::globals(Global {
                        id: id.clone(),
                        r#type: r#type,
                        init: init,
                    })])
                }
            }
        } else {
            let r#type = Self::parse_globaltype(c)?;
            let init = Self::parse_expr(c)?;
            c.expect_cur(&TokenType::RPAREN)?;
            Ok(vec![ModuleField::globals(Global {
                id: id.clone(),
                r#type: r#type,
                init: init,
            })])
        }
    }
    //6.6.9
    fn parse_exports(c: &mut ParseContext) -> Result<Vec<ModuleField>, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::EXPORT)?;
        let name = Self::parse_name(c)?;
        let desc = Self::parse_exportdesc(c)?;
        Ok(vec![ModuleField::exports(Export {
            name: name,
            desc: desc,
        })])
    }
    fn parse_exportdesc(c: &mut ParseContext) -> Result<ExportDesc, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        match &c.cur_token.r#type {
            TokenType::FUNC => {
                c.expect_cur(&TokenType::FUNC)?;
                let func = Self::parse_funcidx(c)?;
                Ok(ExportDesc::func(func))
            }
            TokenType::TABLE => {
                c.expect_cur(&TokenType::TABLE)?;
                let table = Self::parse_tableidx(c)?;
                Ok(ExportDesc::table(table))
            }
            TokenType::MEMORY => {
                c.expect_cur(&TokenType::MEMORY)?;
                let mem = Self::parse_memidx(c)?;
                Ok(ExportDesc::mem(mem))
            }
            TokenType::GLOBAL => {
                c.expect_cur(&TokenType::GLOBAL)?;
                let global = Self::parse_globalidx(c)?;
                Ok(ExportDesc::global(global))
            }
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
    //6.6.10
    fn parse_starts(c: &mut ParseContext) -> Result<Vec<ModuleField>, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::START)?;
        let funcidx = Self::parse_funcidx(c)?;
        c.expect_cur(&TokenType::RPAREN)?;
        Ok(vec![ModuleField::start(funcidx)])
    }
    //6.6.11
    fn parse_elems(c: &mut ParseContext) -> Result<Vec<ModuleField>, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::ELEM)?;
        let tableidx = match &c.cur_token.r#type {
            TokenType::NUM(_) | TokenType::ID(_) => Self::parse_tableidx(c)?,
            _ => TableIdx::x(0),
        };
        let offset = if c.cur_token_is(&TokenType::LPAREN) && c.peek_token_is(&TokenType::OFFSET) {
            c.expect_cur(&TokenType::LPAREN)?;
            c.expect_cur(&TokenType::OFFSET)?;
            let offset = Self::parse_expr(c)?;
            c.expect_cur(&TokenType::RPAREN)?;
            offset
        } else {
            Expr {
                instrs: Self::parse_instr(c)?,
            }
        };
        let funcidxes = Self::parse_vec_funcidx(c)?;
        c.expect_cur(&TokenType::RPAREN)?;
        Ok(vec![ModuleField::elem(Elem {
            table: tableidx,
            offset: offset,
            init: funcidxes,
        })])
    }
    //6.6.12
    fn parse_datas(c: &mut ParseContext) -> Result<Vec<ModuleField>, ParseError> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::DATA)?;
        let memidx = match &c.cur_token.r#type {
            TokenType::NUM(_) | TokenType::ID(_) => Self::parse_memidx(c)?,
            _ => MemIdx::x(0),
        };
        let offset = if c.cur_token_is(&TokenType::LPAREN) && c.peek_token_is(&TokenType::OFFSET) {
            c.expect_cur(&TokenType::LPAREN)?;
            c.expect_cur(&TokenType::OFFSET)?;
            let offset = Self::parse_expr(c)?;
            c.expect_cur(&TokenType::RPAREN)?;
            offset
        } else {
            Expr {
                instrs: Self::parse_instr(c)?,
            }
        };
        let datastring = Self::parse_vec_string(c)?;
        c.expect_cur(&TokenType::RPAREN)?;
        Ok(vec![ModuleField::data(Data {
            data: memidx,
            offset: offset,
            init: datastring,
        })])
    }
    //6.6.13
    pub fn parse_module(c: &mut ParseContext) -> Result<Module, ParseError> {
        let has_module =
            if c.cur_token_is(&TokenType::LPAREN) && c.peek_token_is(&TokenType::MODULE) {
                c.expect_cur(&TokenType::LPAREN)?;
                c.expect_cur(&TokenType::MODULE)?;
                true
            } else {
                false
            };
        let id = Self::parse_id(c);
        let mut modulefields: Vec<ModuleField> = vec![];
        loop {
            if !c.cur_token_is(&TokenType::LPAREN) {
                break;
            }
            let mut fields = Self::parse_modulefield(c)?;
            while fields.len() != 0 {
                modulefields.push(fields.remove(0));
            }
        }
        if has_module {
            c.expect_cur(&TokenType::RPAREN)?;
        }
        Ok(Module {
            id: id,
            modulefields: modulefields,
        })
    }
    pub fn parse_modulefield(c: &mut ParseContext) -> Result<Vec<ModuleField>, ParseError> {
        match &c.peek_token.r#type {
            TokenType::FUNC => Ok(Self::parse_funcs(c)?),
            TokenType::TYPE => Ok(Self::parse_types(c)?),
            TokenType::IMPORT => Ok(Self::parse_imports(c)?),
            TokenType::TABLE => Ok(Self::parse_tables(c)?),
            TokenType::MEMORY => Ok(Self::parse_mems(c)?),
            TokenType::GLOBAL => Ok(Self::parse_globals(c)?),
            TokenType::EXPORT => Ok(Self::parse_exports(c)?),
            TokenType::START => Ok(Self::parse_starts(c)?),
            TokenType::ELEM => Ok(Self::parse_elems(c)?),
            TokenType::DATA => Ok(Self::parse_datas(c)?),
            _ => Err(ParseError::ParseError(c.cur_token.clone())),
        }
    }
}
impl Parse<Module> for ModuleParser {
    fn parse(c: &mut ParseContext) -> std::result::Result<Module, ParseError> {
        Ok(Self::parse_module(c)?)
    }
}
