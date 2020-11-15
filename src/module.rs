use crate::parser;
use crate::structure::*;
use crate::token::*;
use std::str::FromStr;

//6.1.3
#[derive(Clone, Debug)]
pub struct IdentifierContext {
    pub types: Vec<Option<Id>>,
    pub funcs: Vec<Option<Id>>,
    pub tables: Vec<Option<Id>>,
    pub mems: Vec<Option<Id>>,
    pub globals: Vec<Option<Id>>,
    pub locals: Vec<Option<Id>>,
    pub labels: Vec<Option<Id>>,
    pub typedefs: Vec<FuncType>,
}
impl IdentifierContext {
    pub fn new() -> Self {
        Self {
            types: vec![],
            funcs: vec![],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            locals: vec![],
            labels: vec![],
            typedefs: vec![],
        }
    }
    pub fn append(&mut self, other: &Self) {
        self.types.append(&mut other.types.clone());
        self.funcs.append(&mut other.funcs.clone());
        self.tables.append(&mut other.tables.clone());
        self.mems.append(&mut other.mems.clone());
        self.globals.append(&mut other.globals.clone());
        self.locals.append(&mut other.locals.clone());
        self.labels.append(&mut other.labels.clone());
        self.typedefs.append(&mut other.typedefs.clone());
    }
}

fn find_or_push_type(
    module: &mut Module,
    ft: &FuncType,
    id: Option<Id>,
    I: &mut IdentifierContext,
) -> u32 {
    match I.typedefs.iter().position(|fuctype| fuctype == ft) {
        Some(p) => {
            match id {
                Some(_) => {
                    let index = I.typedefs.len() as u32;
                    I.types.push(id);
                    I.typedefs.push(ft.clone());
                    module.types.push(ft.clone());
                    return index;
                }
                _ => {}
            }
            p as u32
        }
        _ => {
            let index = I.typedefs.len() as u32;
            I.types.push(id);
            I.typedefs.push(ft.clone());
            module.types.push(ft.clone());
            index
        }
    }
}

pub type Id = String;

#[derive(Debug)]
enum ModuleField {
    types(FuncType),
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
struct Param {
    id: Option<Id>,
    valtype: ValType,
}
enum Idx {
    v(Option<Id>),
    x(u32),
}
struct Local {
    id: Option<Id>,
    valtype: ValType,
}

pub struct ModuleParser {}
impl ModuleParser {
    //6.3.1
    pub fn parse_u32(c: &mut parser::ParseContext) -> Result<u32, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::NUM(s) => {
                let v = if s.starts_with("0x") {
                    u64::from_str_radix(&s[2..], 16)?
                } else {
                    u64::from_str_radix(&s[..], 10)?
                };
                c.next_token()?;
                Ok(v as u32)
            }
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    pub fn read_u32(s: String) -> Result<u32, parser::Error> {
        let v = if s.starts_with("0x") {
            u64::from_str_radix(&s[2..], 16)?
        } else {
            u64::from_str_radix(&s[..], 10)?
        };
        if v > u32::MAX as u64 {
            Err(parser::Error::Malformed(String::from("i32 constant")))
        } else {
            Ok(v as u32)
        }
    }
    pub fn parse_i32(c: &mut parser::ParseContext) -> Result<i32, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::NUM(num) => {
                let mut s = num.clone();
                c.next_token()?;
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
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    pub fn parse_i64(c: &mut parser::ParseContext) -> Result<i64, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::NUM(num) => {
                let mut s = num.clone();
                c.next_token()?;
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
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    //6.3.2
    pub fn parse_f32(c: &mut parser::ParseContext) -> Result<f32, parser::Error> {
        Ok(Self::parse_f64(c)? as f32)
    }
    pub fn parse_f64(c: &mut parser::ParseContext) -> Result<f64, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::NUM(num) => {
                let mut s = num.clone();
                c.next_token()?;
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
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    fn f64_from_hexstring(s: &str) -> Result<f64, parser::Error> {
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
    fn parse_frac(s: &str) -> Result<f64, parser::Error> {
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
    fn parse_hexfrac(s: &str) -> Result<f64, parser::Error> {
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
    pub fn parse_string(c: &mut parser::ParseContext) -> Result<Vec<u8>, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::STRING(buf) => {
                let bs = buf.clone();
                c.next_token()?;
                Ok(bs)
            }
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    //6.3.4
    pub fn parse_name(c: &mut parser::ParseContext) -> Result<Name, parser::Error> {
        Ok(String::from_utf8(Self::parse_string(c)?)?)
    }
    //6.3.5
    pub fn parse_id(c: &mut parser::ParseContext) -> Result<Option<Id>, parser::Error> {
        let id = match &c.cur_token.r#type {
            TokenType::ID(v) => Some(v.clone()),
            _ => None,
        };
        match id {
            Some(_) => c.next_token()?,
            _ => {}
        }
        Ok(id)
    }
    //6.4.1
    pub fn parse_valtype(c: &mut parser::ParseContext) -> Result<ValType, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::I32 => {
                c.expect_cur(&TokenType::I32)?;
                Ok(ValType::r#i32)
            }
            TokenType::I64 => {
                c.expect_cur(&TokenType::I64)?;
                Ok(ValType::r#i64)
            }
            TokenType::F32 => {
                c.expect_cur(&TokenType::F32)?;
                Ok(ValType::r#f32)
            }
            TokenType::F64 => {
                c.expect_cur(&TokenType::F64)?;
                Ok(ValType::r#f64)
            }
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    //6.4.2
    pub fn parse_functype(c: &mut parser::ParseContext) -> Result<FuncType, parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::FUNC)?;
        let mut ts1: Vec<Param> = vec![];
        while c.peek_token_is(&TokenType::PARAM) {
            let mut ps = Self::parse_param(c)?;
            ts1.append(&mut ps);
        }
        let mut ts2: Vec<ValType> = vec![];
        while c.peek_token_is(&TokenType::RESULT) {
            let mut rs = Self::parse_result(c)?;
            ts2.append(&mut rs);
        }
        c.expect_cur(&TokenType::RPAREN)?;
        Ok(FuncType {
            params: ts1.iter().map(|param| param.valtype.clone()).collect(),
            results: ts2,
        })
    }
    fn parse_param(c: &mut parser::ParseContext) -> Result<Vec<Param>, parser::Error> {
        let mut params: Vec<Param> = vec![];
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::PARAM)?;
        match &c.cur_token.r#type {
            TokenType::ID(_) => {
                let id = Self::parse_id(c)?;
                let t = Self::parse_valtype(c)?;
                c.expect_cur(&TokenType::RPAREN)?;
                Ok(vec![Param { id: id, valtype: t }])
            }
            _ => {
                loop {
                    if c.cur_token_is(&TokenType::RPAREN) {
                        break;
                    }
                    params.push(Param {
                        id: Self::parse_id(c)?,
                        valtype: Self::parse_valtype(c)?,
                    })
                }
                c.expect_cur(&TokenType::RPAREN)?;
                Ok(params)
            }
        }
    }
    fn parse_result(c: &mut parser::ParseContext) -> Result<Vec<ValType>, parser::Error> {
        let mut results: Vec<ValType> = vec![];
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
    fn parse_limits(c: &mut parser::ParseContext) -> Result<Limits, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::NUM(_) => {
                let min = Self::parse_u32(c)?;
                let max = match &c.cur_token.r#type {
                    TokenType::NUM(_) => Some(Self::parse_u32(c)?),
                    _ => None,
                };
                Ok(Limits { min: min, max: max })
            }
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    //6.4.4
    fn parse_memtype(c: &mut parser::ParseContext) -> Result<MemType, parser::Error> {
        Ok(MemType {
            limits: Self::parse_limits(c)?,
        })
    }
    //6.4.5
    pub fn parse_tabletype(c: &mut parser::ParseContext) -> Result<TableType, parser::Error> {
        let limits = Self::parse_limits(c)?;
        let elemtype = Self::parse_elemtype(c)?;
        Ok(TableType {
            limits: limits,
            elemtype: elemtype,
        })
    }
    pub fn parse_elemtype(c: &mut parser::ParseContext) -> Result<ElemType, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::FUNCREF => {
                c.expect_cur(&TokenType::FUNCREF)?;
                Ok(ElemType::FuncRef)
            }
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    //6.4.6
    fn parse_globaltype(c: &mut parser::ParseContext) -> Result<GlobalType, parser::Error> {
        if c.cur_token_is(&TokenType::LPAREN) {
            c.expect_cur(&TokenType::LPAREN)?;
            c.expect_cur(&TokenType::MUT)?;
            let valtype = Self::parse_valtype(c)?;
            c.expect_cur(&TokenType::RPAREN)?;
            Ok(GlobalType {
                r#mut: Mut::Var,
                valtype: valtype,
            })
        } else {
            let valtype = Self::parse_valtype(c)?;
            Ok(GlobalType {
                r#mut: Mut::Const,
                valtype: valtype,
            })
        }
    }
    //6.5.1
    fn parse_label(
        c: &mut parser::ParseContext,
        I: &mut IdentifierContext,
    ) -> Result<IdentifierContext, parser::Error> {
        let id = Self::parse_id(c)?;
        let mut I1 = I.clone();
        if let Some(v) = id {
            I1.labels.insert(0, Some(v));
        } else {
            I1.labels.insert(0, None);
        }
        Ok(I1)
    }
    //6.5
    pub fn parse_instr(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<Vec<Instr>, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::LPAREN => {
                c.expect_cur(&TokenType::LPAREN)?;
                match &c.cur_token.r#type {
                    TokenType::BLOCK => {
                        c.expect_cur(&TokenType::BLOCK)?;
                        let mut I1 = Self::parse_label(c, I)?;
                        let bt = Self::parse_blocktype(c, module, I)?;
                        let mut ins: Vec<Instr> = vec![];
                        loop {
                            if c.cur_token_is(&TokenType::RPAREN) {
                                break;
                            }
                            let mut instr = Self::parse_instr(c, module, &mut I1)?;
                            ins.append(&mut instr);
                        }
                        c.expect_cur(&TokenType::RPAREN)?;

                        Ok(vec![Instr::block {
                            blocktype: bt,
                            instrs: ins,
                        }])
                    }
                    TokenType::LOOP => {
                        c.expect_cur(&TokenType::LOOP)?;
                        let mut I1 = Self::parse_label(c, I)?;
                        let bt = Self::parse_blocktype(c, module, I)?;
                        let mut ins: Vec<Instr> = vec![];
                        loop {
                            if c.cur_token_is(&TokenType::RPAREN) {
                                break;
                            }
                            let mut instr = Self::parse_instr(c, module, &mut I1)?;
                            ins.append(&mut instr);
                        }
                        c.expect_cur(&TokenType::RPAREN)?;
                        Ok(vec![Instr::r#loop {
                            blocktype: bt,
                            instrs: ins,
                        }])
                    }
                    TokenType::IF => {
                        let mut ins: Vec<Instr> = vec![];
                        c.expect_cur(&TokenType::IF)?;
                        let mut I1 = Self::parse_label(c, I)?;
                        let bt = Self::parse_blocktype(c, module, I)?;
                        loop {
                            if c.cur_token_is(&TokenType::LPAREN)
                                && c.peek_token_is(&TokenType::THEN)
                            {
                                break;
                            }
                            let mut foldedinstrs = Self::parse_instr(c, module, I)?;
                            ins.append(&mut foldedinstrs);
                        }
                        c.expect_cur(&TokenType::LPAREN)?;
                        c.expect_cur(&TokenType::THEN)?;
                        let mut ins1: Vec<Instr> = vec![];
                        loop {
                            if c.cur_token_is(&TokenType::RPAREN) {
                                break;
                            }
                            let mut instr = Self::parse_instr(c, module, &mut I1)?;
                            ins1.append(&mut instr);
                        }
                        c.expect_cur(&TokenType::RPAREN)?;
                        let mut ins2: Vec<Instr> = vec![];
                        if c.cur_token_is(&TokenType::LPAREN) {
                            c.expect_cur(&TokenType::LPAREN)?;
                            c.expect_cur(&TokenType::ELSE)?;
                            loop {
                                if c.cur_token_is(&TokenType::RPAREN) {
                                    break;
                                }
                                let mut instr = Self::parse_instr(c, module, &mut I1)?;
                                ins2.append(&mut instr);
                            }
                            c.expect_cur(&TokenType::RPAREN)?;
                        }
                        c.expect_cur(&TokenType::RPAREN)?;
                        ins.push(Instr::r#if {
                            blocktype: bt,
                            instrs1: ins1,
                            instrs2: ins2,
                        });
                        Ok(ins)
                    }
                    _ => {
                        let mut ins: Vec<Instr> = vec![];
                        let plaininstr = Self::parse_plaininstr(c, module, I)?;
                        loop {
                            if c.cur_token_is(&TokenType::RPAREN) {
                                break;
                            }
                            let mut foldedinstrs = Self::parse_instr(c, module, I)?;
                            ins.append(&mut foldedinstrs);
                        }
                        c.expect_cur(&TokenType::RPAREN)?;
                        ins.push(plaininstr);
                        Ok(ins)
                    }
                }
            }
            TokenType::BLOCK | TokenType::LOOP | TokenType::IF => {
                Ok(vec![Self::parse_blockinstr(c, module, I)?])
            }
            _ => Ok(vec![Self::parse_plaininstr(c, module, I)?]),
        }
    }
    fn parse_blocktype(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<BlockType, parser::Error> {
        let (x, I1) = Self::parse_typeuse(c, module, I)?;
        if I1.typedefs.len() == 0 {
            Ok(BlockType::typeidx(x))
        } else if I.typedefs[x as usize].results.len() == 1 {
            Ok(BlockType::valtype(Some(
                I.typedefs[x as usize].results[0].clone(),
            )))
        } else if I.typedefs[x as usize].results.len() == 0 {
            Ok(BlockType::valtype(None))
        } else {
            Err(parser::Error::ParseError(c.cur_token.clone()))
        }
    }
    fn parse_blockinstr(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<Instr, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::BLOCK => {
                c.expect_cur(&TokenType::BLOCK)?;
                let mut I1 = Self::parse_label(c, I)?;
                let bt = Self::parse_blocktype(c, module, I)?;
                let mut ins: Vec<Instr> = vec![];
                loop {
                    if c.cur_token_is(&TokenType::END) {
                        break;
                    }
                    let mut instr = Self::parse_instr(c, module, &mut I1)?;
                    ins.append(&mut instr);
                }
                c.expect_cur(&TokenType::END)?;
                let id = Self::parse_id(c)?;
                match id {
                    None => {}
                    _ => {
                        if id != *I1.labels.last().clone().unwrap() {
                            return Err(parser::Error::ParseError(c.cur_token.clone()));
                        }
                    }
                }

                Ok(Instr::block {
                    blocktype: bt,
                    instrs: ins,
                })
            }
            TokenType::LOOP => {
                c.expect_cur(&TokenType::LOOP)?;
                let mut I1 = Self::parse_label(c, I)?;
                let bt = Self::parse_blocktype(c, module, I)?;
                let mut ins: Vec<Instr> = vec![];
                loop {
                    if c.cur_token_is(&TokenType::END) {
                        break;
                    }
                    let mut instr = Self::parse_instr(c, module, &mut I1)?;
                    ins.append(&mut instr);
                }
                c.expect_cur(&TokenType::END)?;
                let id = Self::parse_id(c)?;
                match id {
                    None => {}
                    _ => {
                        if id != *I1.labels.last().clone().unwrap() {
                            return Err(parser::Error::ParseError(c.cur_token.clone()));
                        }
                    }
                }

                Ok(Instr::r#loop {
                    blocktype: bt,
                    instrs: ins,
                })
            }
            TokenType::IF => {
                c.expect_cur(&TokenType::IF)?;
                let mut I1 = Self::parse_label(c, I)?;
                let bt = Self::parse_blocktype(c, module, I)?;
                let mut ins1: Vec<Instr> = vec![];
                loop {
                    if c.cur_token_is(&TokenType::ELSE) {
                        break;
                    }
                    let mut instr = Self::parse_instr(c, module, &mut I1)?;
                    ins1.append(&mut instr);
                }
                c.expect_cur(&TokenType::ELSE)?;
                let id1 = Self::parse_id(c)?;
                let mut ins2: Vec<Instr> = vec![];
                loop {
                    if c.cur_token_is(&TokenType::END) {
                        break;
                    }
                    let mut instr = Self::parse_instr(c, module, &mut I1)?;
                    ins2.append(&mut instr);
                }
                c.expect_cur(&TokenType::END)?;
                let id2 = Self::parse_id(c)?;
                match id1 {
                    None => {}
                    _ => {
                        if id1 != *I1.labels.last().clone().unwrap() {
                            return Err(parser::Error::ParseError(c.cur_token.clone()));
                        }
                    }
                }
                match id2 {
                    None => {}
                    _ => {
                        if id2 != *I1.labels.last().clone().unwrap() {
                            return Err(parser::Error::ParseError(c.cur_token.clone()));
                        }
                    }
                }

                Ok(Instr::r#if {
                    blocktype: bt,
                    instrs1: ins1,
                    instrs2: ins2,
                })
            }
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    fn parse_plaininstr(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<Instr, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => match &keyword[..] {
                // 6.5.2
                "unreachable" => Self::process_instr(c, Instr::unreachable),
                "nop" => Self::process_instr(c, Instr::nop),
                "br" => {
                    c.next_token()?;
                    Ok(Instr::br(Self::parse_labelidx(c, I)?))
                }
                "br_if" => {
                    c.next_token()?;
                    let instr = Instr::br_if(Self::parse_labelidx(c, I)?);
                    Ok(instr)
                }
                "br_table" => {
                    c.expect_cur(&TokenType::KEYWORD("br_table".to_string()))?;
                    let mut labelidxes = Self::parse_vec_labelidx(c, I)?;
                    if labelidxes.len() < 1 {
                        return Err(parser::Error::ParseError(c.cur_token.clone()));
                    }
                    let labelidx = labelidxes.remove(labelidxes.len() - 1);
                    Ok(Instr::br_table {
                        labelidxes: labelidxes,
                        labelidx: labelidx,
                    })
                }
                "return" => Self::process_instr(c, Instr::r#return),
                "call" => {
                    c.next_token()?;
                    let instr = Instr::call(Self::parse_funcidx(c, I)?);
                    Ok(instr)
                }
                "call_indirect" => {
                    c.next_token()?;
                    let instr = Instr::call_indirect(Self::parse_typeuse(c, module, I)?.0);
                    Ok(instr)
                }
                //6.5.3
                "drop" => Self::process_instr(c, Instr::drop),
                "select" => Self::process_instr(c, Instr::select),
                //6.5.4
                "local.get" => {
                    c.next_token()?;
                    let instr = Instr::local_get(Self::parse_localidx(c, I)?);
                    Ok(instr)
                }
                "local.set" => {
                    c.next_token()?;
                    let instr = Instr::local_set(Self::parse_localidx(c, I)?);
                    Ok(instr)
                }
                "local.tee" => {
                    c.next_token()?;
                    let instr = Instr::local_tee(Self::parse_localidx(c, I)?);
                    Ok(instr)
                }
                "global.get" => {
                    c.next_token()?;
                    let instr = Instr::global_get(Self::parse_globalidx(c, I)?);
                    Ok(instr)
                }
                "global.set" => {
                    c.next_token()?;
                    let instr = Instr::global_set(Self::parse_globalidx(c, I)?);
                    Ok(instr)
                }
                //6.5.5
                "i32.load" => {
                    c.next_token()?;
                    let instr = Instr::i32_load(Self::parse_memarg::<4>(c)?);
                    Ok(instr)
                }
                "i64.load" => {
                    c.next_token()?;
                    let instr = Instr::i64_load(Self::parse_memarg::<8>(c)?);
                    Ok(instr)
                }
                "f32.load" => {
                    c.next_token()?;
                    let instr = Instr::f32_load(Self::parse_memarg::<4>(c)?);
                    Ok(instr)
                }
                "f64.load" => {
                    c.next_token()?;
                    let instr = Instr::f64_load(Self::parse_memarg::<8>(c)?);
                    Ok(instr)
                }
                "i32.load8_s" => {
                    c.next_token()?;
                    let instr = Instr::i32_load8_s(Self::parse_memarg::<1>(c)?);
                    Ok(instr)
                }
                "i32.load8_u" => {
                    c.next_token()?;
                    let instr = Instr::i32_load8_u(Self::parse_memarg::<1>(c)?);
                    Ok(instr)
                }
                "i32.load16_s" => {
                    c.next_token()?;
                    let instr = Instr::i32_load16_s(Self::parse_memarg::<2>(c)?);
                    Ok(instr)
                }
                "i32.load16_u" => {
                    c.next_token()?;
                    let instr = Instr::i32_load16_u(Self::parse_memarg::<2>(c)?);
                    Ok(instr)
                }
                "i64.load8_s" => {
                    c.next_token()?;
                    let instr = Instr::i64_load8_s(Self::parse_memarg::<1>(c)?);
                    Ok(instr)
                }
                "i64.load8_u" => {
                    c.next_token()?;
                    let instr = Instr::i64_load8_u(Self::parse_memarg::<1>(c)?);
                    Ok(instr)
                }
                "i64.load16_s" => {
                    c.next_token()?;
                    let instr = Instr::i64_load16_s(Self::parse_memarg::<2>(c)?);
                    Ok(instr)
                }
                "i64.load16_u" => {
                    c.next_token()?;
                    let instr = Instr::i64_load16_u(Self::parse_memarg::<2>(c)?);
                    Ok(instr)
                }
                "i64.load32_s" => {
                    c.next_token()?;
                    let instr = Instr::i64_load32_s(Self::parse_memarg::<4>(c)?);
                    Ok(instr)
                }
                "i64.load32_u" => {
                    c.next_token()?;
                    let instr = Instr::i64_load32_u(Self::parse_memarg::<4>(c)?);
                    Ok(instr)
                }
                "i32.store" => {
                    c.next_token()?;
                    let instr = Instr::i32_store(Self::parse_memarg::<4>(c)?);
                    Ok(instr)
                }
                "i64.store" => {
                    c.next_token()?;
                    let instr = Instr::i64_store(Self::parse_memarg::<8>(c)?);
                    Ok(instr)
                }
                "f32.store" => {
                    c.next_token()?;
                    let instr = Instr::f32_store(Self::parse_memarg::<4>(c)?);
                    Ok(instr)
                }
                "f64.store" => {
                    c.next_token()?;
                    let instr = Instr::f64_store(Self::parse_memarg::<8>(c)?);
                    Ok(instr)
                }
                "i32.store8" => {
                    c.next_token()?;
                    let instr = Instr::i32_store8(Self::parse_memarg::<1>(c)?);
                    Ok(instr)
                }
                "i32.store16" => {
                    c.next_token()?;
                    let instr = Instr::i32_store16(Self::parse_memarg::<2>(c)?);
                    Ok(instr)
                }
                "i64.store8" => {
                    c.next_token()?;
                    let instr = Instr::i64_store8(Self::parse_memarg::<1>(c)?);
                    Ok(instr)
                }
                "i64.store16" => {
                    c.next_token()?;
                    let instr = Instr::i64_store16(Self::parse_memarg::<2>(c)?);
                    Ok(instr)
                }
                "i64.store32" => {
                    c.next_token()?;
                    let instr = Instr::i64_store32(Self::parse_memarg::<4>(c)?);
                    Ok(instr)
                }
                "memory.size" => Self::process_instr(c, Instr::memory_size),
                "memory.grow" => Self::process_instr(c, Instr::memory_grow),
                //6.5.6
                "i32.const" => {
                    c.next_token()?;
                    let instr = Instr::i32_const(Self::parse_i32(c)?);
                    Ok(instr)
                }
                "i64.const" => {
                    c.next_token()?;
                    let instr = Instr::i64_const(Self::parse_i64(c)?);
                    Ok(instr)
                }
                "f32.const" => {
                    c.next_token()?;
                    let instr = Instr::f32_const(Self::parse_f32(c)?);
                    Ok(instr)
                }
                "f64.const" => {
                    c.next_token()?;
                    let instr = Instr::f64_const(Self::parse_f64(c)?);
                    Ok(instr)
                }
                "i32.clz" => Self::process_instr(c, Instr::i32_clz),
                "i32.ctz" => Self::process_instr(c, Instr::i32_ctz),
                "i32.popcnt" => Self::process_instr(c, Instr::i32_popcnt),
                "i32.add" => Self::process_instr(c, Instr::i32_add),
                "i32.sub" => Self::process_instr(c, Instr::i32_sub),
                "i32.mul" => Self::process_instr(c, Instr::i32_mul),
                "i32.div_s" => Self::process_instr(c, Instr::i32_div_s),
                "i32.div_u" => Self::process_instr(c, Instr::i32_div_u),
                "i32.rem_s" => Self::process_instr(c, Instr::i32_rem_s),
                "i32.rem_u" => Self::process_instr(c, Instr::i32_rem_u),
                "i32.and" => Self::process_instr(c, Instr::i32_and),
                "i32.or" => Self::process_instr(c, Instr::i32_or),
                "i32.xor" => Self::process_instr(c, Instr::i32_xor),
                "i32.shl" => Self::process_instr(c, Instr::i32_shl),
                "i32.shr_s" => Self::process_instr(c, Instr::i32_shr_s),
                "i32.shr_u" => Self::process_instr(c, Instr::i32_shr_u),
                "i32.rotl" => Self::process_instr(c, Instr::i32_rotl),
                "i32.rotr" => Self::process_instr(c, Instr::i32_rotr),
                "i64.clz" => Self::process_instr(c, Instr::i64_clz),
                "i64.ctz" => Self::process_instr(c, Instr::i64_ctz),
                "i64.popcnt" => Self::process_instr(c, Instr::i64_popcnt),
                "i64.add" => Self::process_instr(c, Instr::i64_add),
                "i64.sub" => Self::process_instr(c, Instr::i64_sub),
                "i64.mul" => Self::process_instr(c, Instr::i64_mul),
                "i64.div_s" => Self::process_instr(c, Instr::i64_div_s),
                "i64.div_u" => Self::process_instr(c, Instr::i64_div_u),
                "i64.rem_s" => Self::process_instr(c, Instr::i64_rem_s),
                "i64.rem_u" => Self::process_instr(c, Instr::i64_rem_u),
                "i64.and" => Self::process_instr(c, Instr::i64_and),
                "i64.or" => Self::process_instr(c, Instr::i64_or),
                "i64.xor" => Self::process_instr(c, Instr::i64_xor),
                "i64.shl" => Self::process_instr(c, Instr::i64_shl),
                "i64.shr_s" => Self::process_instr(c, Instr::i64_shr_s),
                "i64.shr_u" => Self::process_instr(c, Instr::i64_shr_u),
                "i64.rotl" => Self::process_instr(c, Instr::i64_rotl),
                "i64.rotr" => Self::process_instr(c, Instr::i64_rotr),
                "f32.abs" => Self::process_instr(c, Instr::f32_abs),
                "f32.neg" => Self::process_instr(c, Instr::f32_neg),
                "f32.ceil" => Self::process_instr(c, Instr::f32_ceil),
                "f32.floor" => Self::process_instr(c, Instr::f32_floor),
                "f32.trunc" => Self::process_instr(c, Instr::f32_trunc),
                "f32.nearest" => Self::process_instr(c, Instr::f32_nearest),
                "f32.sqrt" => Self::process_instr(c, Instr::f32_sqrt),
                "f32.add" => Self::process_instr(c, Instr::f32_add),
                "f32.sub" => Self::process_instr(c, Instr::f32_sub),
                "f32.mul" => Self::process_instr(c, Instr::f32_mul),
                "f32.div" => Self::process_instr(c, Instr::f32_div),
                "f32.min" => Self::process_instr(c, Instr::f32_min),
                "f32.max" => Self::process_instr(c, Instr::f32_max),
                "f32.copysign" => Self::process_instr(c, Instr::f32_copysign),
                "f64.abs" => Self::process_instr(c, Instr::f64_abs),
                "f64.neg" => Self::process_instr(c, Instr::f64_neg),
                "f64.ceil" => Self::process_instr(c, Instr::f64_ceil),
                "f64.floor" => Self::process_instr(c, Instr::f64_floor),
                "f64.trunc" => Self::process_instr(c, Instr::f64_trunc),
                "f64.nearest" => Self::process_instr(c, Instr::f64_nearest),
                "f64.sqrt" => Self::process_instr(c, Instr::f64_sqrt),
                "f64.add" => Self::process_instr(c, Instr::f64_add),
                "f64.sub" => Self::process_instr(c, Instr::f64_sub),
                "f64.mul" => Self::process_instr(c, Instr::f64_mul),
                "f64.div" => Self::process_instr(c, Instr::f64_div),
                "f64.min" => Self::process_instr(c, Instr::f64_min),
                "f64.max" => Self::process_instr(c, Instr::f64_max),
                "f64.copysign" => Self::process_instr(c, Instr::f64_copysign),
                "i32.eqz" => Self::process_instr(c, Instr::i32_eqz),
                "i32.eq" => Self::process_instr(c, Instr::i32_eq),
                "i32.ne" => Self::process_instr(c, Instr::i32_ne),
                "i32.lt_s" => Self::process_instr(c, Instr::i32_lt_s),
                "i32.lt_u" => Self::process_instr(c, Instr::i32_lt_u),
                "i32.gt_s" => Self::process_instr(c, Instr::i32_gt_s),
                "i32.gt_u" => Self::process_instr(c, Instr::i32_gt_u),
                "i32.le_s" => Self::process_instr(c, Instr::i32_le_s),
                "i32.le_u" => Self::process_instr(c, Instr::i32_le_u),
                "i32.ge_s" => Self::process_instr(c, Instr::i32_ge_s),
                "i32.ge_u" => Self::process_instr(c, Instr::i32_ge_u),
                "i64.eqz" => Self::process_instr(c, Instr::i64_eqz),
                "i64.eq" => Self::process_instr(c, Instr::i64_eq),
                "i64.ne" => Self::process_instr(c, Instr::i64_ne),
                "i64.lt_s" => Self::process_instr(c, Instr::i64_lt_s),
                "i64.lt_u" => Self::process_instr(c, Instr::i64_lt_u),
                "i64.gt_s" => Self::process_instr(c, Instr::i64_gt_s),
                "i64.gt_u" => Self::process_instr(c, Instr::i64_gt_u),
                "i64.le_s" => Self::process_instr(c, Instr::i64_le_s),
                "i64.le_u" => Self::process_instr(c, Instr::i64_le_u),
                "i64.ge_s" => Self::process_instr(c, Instr::i64_ge_s),
                "i64.ge_u" => Self::process_instr(c, Instr::i64_ge_u),
                "f32.eq" => Self::process_instr(c, Instr::f32_eq),
                "f32.ne" => Self::process_instr(c, Instr::f32_ne),
                "f32.lt" => Self::process_instr(c, Instr::f32_lt),
                "f32.gt" => Self::process_instr(c, Instr::f32_gt),
                "f32.le" => Self::process_instr(c, Instr::f32_le),
                "f32.ge" => Self::process_instr(c, Instr::f32_ge),
                "f64.eq" => Self::process_instr(c, Instr::f64_eq),
                "f64.ne" => Self::process_instr(c, Instr::f64_ne),
                "f64.lt" => Self::process_instr(c, Instr::f64_lt),
                "f64.gt" => Self::process_instr(c, Instr::f64_gt),
                "f64.le" => Self::process_instr(c, Instr::f64_le),
                "f64.ge" => Self::process_instr(c, Instr::f64_ge),
                "i32.wrap_i64" => Self::process_instr(c, Instr::i32_wrap_i64),
                "i32.trunc_f32_s" => Self::process_instr(c, Instr::i32_trunc_f32_s),
                "i32.trunc_f32_u" => Self::process_instr(c, Instr::i32_trunc_f32_u),
                "i32.trunc_f64_s" => Self::process_instr(c, Instr::i32_trunc_f64_s),
                "i32.trunc_f64_u" => Self::process_instr(c, Instr::i32_trunc_f64_u),
                "i32.trunc_sat_f32_s" => Self::process_instr(c, Instr::i32_trunc_sat_f32_s),
                "i32.trunc_sat_f32_u" => Self::process_instr(c, Instr::i32_trunc_sat_f32_u),
                "i32.trunc_sat_f64_s" => Self::process_instr(c, Instr::i32_trunc_sat_f64_s),
                "i32.trunc_sat_f64_u" => Self::process_instr(c, Instr::i32_trunc_sat_f64_u),
                "i64.extend_i32_s" => Self::process_instr(c, Instr::i64_extend_i32_s),
                "i64.extend_i32_u" => Self::process_instr(c, Instr::i64_extend_i32_u),
                "i64.trunc_f32_s" => Self::process_instr(c, Instr::i64_trunc_f32_s),
                "i64.trunc_f32_u" => Self::process_instr(c, Instr::i64_trunc_f32_u),
                "i64.trunc_f64_s" => Self::process_instr(c, Instr::i64_trunc_f64_s),
                "i64.trunc_f64_u" => Self::process_instr(c, Instr::i64_trunc_f64_u),
                "i64.trunc_sat_f32_s" => Self::process_instr(c, Instr::i64_trunc_sat_f32_s),
                "i64.trunc_sat_f32_u" => Self::process_instr(c, Instr::i64_trunc_sat_f32_u),
                "i64.trunc_sat_f64_s" => Self::process_instr(c, Instr::i64_trunc_sat_f64_s),
                "i64.trunc_sat_f64_u" => Self::process_instr(c, Instr::i64_trunc_sat_f64_u),
                "f32.convert_i32_s" => Self::process_instr(c, Instr::f32_convert_i32_s),
                "f32.convert_i32_u" => Self::process_instr(c, Instr::f32_convert_i32_u),
                "f32.convert_i64_s" => Self::process_instr(c, Instr::f32_convert_i64_s),
                "f32.convert_i64_u" => Self::process_instr(c, Instr::f32_convert_i64_u),
                "f32.demote_f64" => Self::process_instr(c, Instr::f32_demote_f64),
                "f64.convert_i32_s" => Self::process_instr(c, Instr::f64_convert_i32_s),
                "f64.convert_i32_u" => Self::process_instr(c, Instr::f64_convert_i32_u),
                "f64.convert_i64_s" => Self::process_instr(c, Instr::f64_convert_i64_s),
                "f64.convert_i64_u" => Self::process_instr(c, Instr::f64_convert_i64_u),
                "f64.promote_f32" => Self::process_instr(c, Instr::f64_promote_f32),
                "i32.reinterpret_f32" => Self::process_instr(c, Instr::i32_reinterpret_f32),
                "i64.reinterpret_f64" => Self::process_instr(c, Instr::i64_reinterpret_f64),
                "f32.reinterpret_i32" => Self::process_instr(c, Instr::f32_reinterpret_i32),
                "f64.reinterpret_i64" => Self::process_instr(c, Instr::f64_reinterpret_i64),
                "i32.extend8_s" => Self::process_instr(c, Instr::i32_extend8_s),
                "i32.extend16_s" => Self::process_instr(c, Instr::i32_extend16_s),
                "i64.extend8_s" => Self::process_instr(c, Instr::i64_extend8_s),
                "i64.extend16_s" => Self::process_instr(c, Instr::i64_extend16_s),
                "i64.extend32_s" => Self::process_instr(c, Instr::i64_extend32_s),
                _ => Err(parser::Error::ParseError(c.cur_token.clone())),
            },
            TokenType::END => Self::process_instr(c, Instr::end),
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    fn parse_vec_labelidx(
        c: &mut parser::ParseContext,
        I: &mut IdentifierContext,
    ) -> Result<Vec<u32>, parser::Error> {
        let mut labelidxes: Vec<u32> = vec![];
        loop {
            match &c.cur_token.r#type {
                TokenType::NUM(_) | TokenType::ID(_) => {
                    labelidxes.push(Self::parse_labelidx(c, I)?)
                }
                _ => break,
            }
        }
        Ok(labelidxes)
    }
    fn parse_memarg<const N: u32>(c: &mut parser::ParseContext) -> Result<MemArg, parser::Error> {
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
                                Ok(MemArg {
                                    offset: offset,
                                    align: align,
                                })
                            } else {
                                Ok(MemArg {
                                    offset: offset,
                                    align: align,
                                })
                            }
                        }
                        _ => Ok(MemArg {
                            offset: offset,
                            align: align,
                        }),
                    }
                } else if keyword.starts_with("align=") {
                    align = Self::parse_align::<N>(c)?;
                    Ok(MemArg {
                        offset: offset,
                        align: align,
                    })
                } else {
                    Ok(MemArg {
                        offset: offset,
                        align: align,
                    })
                }
            }
            _ => Ok(MemArg {
                offset: offset,
                align: align,
            }),
        }
    }
    fn parse_offset(c: &mut parser::ParseContext) -> Result<u32, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => {
                let s = keyword[7..].to_string();
                c.next_token()?;
                Ok(Self::read_u32(s)?)
            }
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    fn parse_align<const N: u32>(c: &mut parser::ParseContext) -> Result<u32, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::KEYWORD(keyword) => {
                let s = keyword[6..].to_string();
                c.next_token()?;
                let align = Self::read_u32(s)?;
                if align == 0 || (align & (align - 1)) != 0 {
                    Err(parser::Error::Malformed(String::from("alignment")))
                } else {
                    Ok(align)
                }
            }
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    //6.5.8
    pub fn parse_expr(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<Expr, parser::Error> {
        let mut instrs: Vec<Instr> = vec![];
        loop {
            if c.cur_token_is(&TokenType::END) || c.cur_token_is(&TokenType::RPAREN) {
                break;
            }
            let mut instr = Self::parse_instr(c, module, I)?;
            instrs.append(&mut instr);
        }
        Ok(Expr { instrs: instrs })
    }
    //6.6.1
    fn parse_idx(c: &mut parser::ParseContext) -> Result<Idx, parser::Error> {
        match &c.cur_token.r#type {
            TokenType::NUM(_) => Ok(Idx::x(Self::parse_u32(c)?)),
            TokenType::ID(_) => Ok(Idx::v(Self::parse_id(c)?)),
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    fn parse_typeidx(
        c: &mut parser::ParseContext,
        I: &mut IdentifierContext,
    ) -> Result<u32, parser::Error> {
        let idx = Self::parse_idx(c)?;
        match idx {
            Idx::v(id) => match I.types.iter().position(|tid| *tid == id) {
                Some(x) => Ok(x as u32),
                _ => Err(parser::Error::ParseError(c.cur_token.clone())),
            },
            Idx::x(x) => Ok(x),
        }
    }
    fn parse_funcidx(
        c: &mut parser::ParseContext,
        I: &mut IdentifierContext,
    ) -> Result<u32, parser::Error> {
        let idx = Self::parse_idx(c)?;
        match idx {
            Idx::v(id) => match I.funcs.iter().position(|fid| *fid == id) {
                Some(x) => Ok(x as u32),
                _ => Err(parser::Error::ParseError(c.cur_token.clone())),
            },
            Idx::x(x) => Ok(x),
        }
    }
    fn parse_tableidx(
        c: &mut parser::ParseContext,
        I: &mut IdentifierContext,
    ) -> Result<u32, parser::Error> {
        let idx = Self::parse_idx(c)?;
        match idx {
            Idx::v(id) => match I.tables.iter().position(|tid| *tid == id) {
                Some(x) => Ok(x as u32),
                _ => Err(parser::Error::ParseError(c.cur_token.clone())),
            },
            Idx::x(x) => Ok(x),
        }
    }
    fn parse_memidx(
        c: &mut parser::ParseContext,
        I: &mut IdentifierContext,
    ) -> Result<u32, parser::Error> {
        let idx = Self::parse_idx(c)?;
        match idx {
            Idx::v(id) => match I.mems.iter().position(|mid| *mid == id) {
                Some(x) => Ok(x as u32),
                _ => Err(parser::Error::ParseError(c.cur_token.clone())),
            },
            Idx::x(x) => Ok(x),
        }
    }
    fn parse_globalidx(
        c: &mut parser::ParseContext,
        I: &mut IdentifierContext,
    ) -> Result<u32, parser::Error> {
        let idx = Self::parse_idx(c)?;
        match idx {
            Idx::v(id) => match I.globals.iter().position(|gid| *gid == id) {
                Some(x) => Ok(x as u32),
                _ => Err(parser::Error::ParseError(c.cur_token.clone())),
            },
            Idx::x(x) => Ok(x),
        }
    }
    fn parse_localidx(
        c: &mut parser::ParseContext,
        I: &mut IdentifierContext,
    ) -> Result<u32, parser::Error> {
        let idx = Self::parse_idx(c)?;
        match idx {
            Idx::v(id) => match I.locals.iter().position(|lid| *lid == id) {
                Some(x) => Ok(x as u32),
                _ => Err(parser::Error::ParseError(c.cur_token.clone())),
            },
            Idx::x(x) => Ok(x),
        }
    }
    fn parse_labelidx(
        c: &mut parser::ParseContext,
        I: &mut IdentifierContext,
    ) -> Result<u32, parser::Error> {
        let idx = Self::parse_idx(c)?;
        match idx {
            Idx::v(id) => match I.labels.iter().position(|lid| *lid == id) {
                Some(x) => Ok(x as u32),
                _ => Err(parser::Error::ParseError(c.cur_token.clone())),
            },
            Idx::x(x) => Ok(x),
        }
    }
    //6.6.2
    fn parse_types(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(), parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::TYPE)?;
        let id = Self::parse_id(c)?;
        let ft = Self::parse_functype(c)?;
        c.expect_cur(&TokenType::RPAREN)?;

        let typeidx = find_or_push_type(module, &ft, id, I);
        Ok(())
    }
    //6.6.3
    pub fn parse_typeuse(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(u32, IdentifierContext), parser::Error> {
        if c.cur_token_is(&TokenType::LPAREN)
            && (c.peek_token_is(&TokenType::TYPE)
                || c.peek_token_is(&TokenType::PARAM)
                || c.peek_token_is(&TokenType::RESULT))
        {
            match &c.peek_token.r#type {
                TokenType::PARAM => {
                    let ts1 = Self::parse_vec_param(c)?;
                    let ts2 = Self::parse_vec_result(c)?;
                    let x = find_or_push_type(
                        module,
                        &FuncType {
                            params: ts1.iter().map(|param| param.valtype.clone()).collect(),
                            results: ts2,
                        },
                        None,
                        I,
                    );
                    Ok((x, {
                        let mut I1 = IdentifierContext::new();
                        I1.locals.append(
                            &mut ts1
                                .iter()
                                .map(|param| param.id.clone())
                                .collect::<Vec<Option<Id>>>(),
                        );
                        I1
                    }))
                }
                TokenType::RESULT => {
                    let ts1: Vec<Param> = vec![];
                    let ts2 = Self::parse_vec_result(c)?;
                    let x = find_or_push_type(
                        module,
                        &FuncType {
                            params: ts1.iter().map(|param| param.valtype.clone()).collect(),
                            results: ts2,
                        },
                        None,
                        I,
                    );
                    Ok((x, {
                        let mut I1 = IdentifierContext::new();
                        I1.locals.append(
                            &mut ts1
                                .iter()
                                .map(|param| param.id.clone())
                                .collect::<Vec<Option<Id>>>(),
                        );
                        I1
                    }))
                }
                TokenType::TYPE => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::TYPE)?;
                    if c.cur_token_is(&TokenType::LPAREN) {
                        c.expect_cur(&TokenType::LPAREN)?;
                        c.expect_cur(&TokenType::FUNC)?;
                        let ts1 = Self::parse_vec_param(c)?;
                        let ts2 = Self::parse_vec_result(c)?;
                        c.expect_cur(&TokenType::RPAREN)?;
                        c.expect_cur(&TokenType::RPAREN)?;

                        let x = find_or_push_type(
                            module,
                            &FuncType {
                                params: ts1.iter().map(|param| param.valtype.clone()).collect(),
                                results: ts2,
                            },
                            None,
                            I,
                        );
                        Ok((x, {
                            let mut I1 = IdentifierContext::new();
                            I1.locals.append(
                                &mut ts1
                                    .iter()
                                    .map(|param| param.id.clone())
                                    .collect::<Vec<Option<Id>>>(),
                            );
                            I1
                        }))
                    } else {
                        let x = Self::parse_typeidx(c, I)?;
                        c.expect_cur(&TokenType::RPAREN)?;
                        let ts1 = Self::parse_vec_param(c)?;
                        let ts2 = Self::parse_vec_result(c)?;

                        let functype = I.typedefs[x as usize].clone();
                        // TODO check
                        // if functype.params
                        //     != ts1
                        //         .iter()
                        //         .map(|param| param.valtype.clone())
                        //         .collect::<Vec<ValType>>()
                        //     || functype.results != ts2
                        // {
                        //     return Err(parser::Error::ParseError(c.cur_token.clone()));
                        // }

                        Ok((x, {
                            let mut I1 = IdentifierContext::new();
                            I1.locals.append(
                                &mut ts1
                                    .iter()
                                    .map(|param| param.id.clone())
                                    .collect::<Vec<Option<Id>>>(),
                            );
                            I1
                        }))
                    }
                }
                _ => Err(parser::Error::ParseError(c.cur_token.clone())),
            }
        } else {
            let ts1: Vec<Param> = vec![];
            let ts2: Vec<ValType> = vec![];

            let x = find_or_push_type(
                module,
                &FuncType {
                    params: ts1.iter().map(|param| param.valtype.clone()).collect(),
                    results: ts2,
                },
                None,
                I,
            );
            Ok((x, {
                let mut I1 = IdentifierContext::new();
                I1.locals.append(
                    &mut ts1
                        .iter()
                        .map(|param| param.id.clone())
                        .collect::<Vec<Option<Id>>>(),
                );
                I1
            }))
        }
    }
    fn parse_vec_param(c: &mut parser::ParseContext) -> Result<Vec<Param>, parser::Error> {
        let mut params: Vec<Param> = vec![];
        loop {
            if (!c.cur_token_is(&TokenType::LPAREN)) || (!c.peek_token_is(&TokenType::PARAM)) {
                break;
            }
            let mut param = Self::parse_param(c)?;
            params.append(&mut param);
        }
        Ok(params)
    }
    fn parse_vec_result(c: &mut parser::ParseContext) -> Result<Vec<ValType>, parser::Error> {
        let mut results: Vec<ValType> = vec![];
        loop {
            if (!c.cur_token_is(&TokenType::LPAREN)) || (!c.peek_token_is(&TokenType::RESULT)) {
                break;
            }
            let mut result = Self::parse_result(c)?;
            results.append(&mut result);
        }
        Ok(results)
    }
    //6.6.4
    fn parse_imports(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(), parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::IMPORT)?;
        let r#mod = Self::parse_name(c)?;
        let nm = Self::parse_name(c)?;
        let d = Self::parse_importdesc(c, module, I)?;
        c.expect_cur(&TokenType::RPAREN)?;

        module.imports.push(Import {
            module: r#mod,
            name: nm,
            desc: d,
        });
        Ok(())
    }
    pub fn parse_importdesc(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<ImportDesc, parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        match &c.cur_token.r#type {
            TokenType::FUNC => {
                c.expect_cur(&TokenType::FUNC)?;
                let id = Self::parse_id(c)?;
                let (x, I1) = Self::parse_typeuse(c, module, I)?;
                c.expect_cur(&TokenType::RPAREN)?;

                I.funcs.push(id);
                Ok(ImportDesc::func(x))
            }
            TokenType::TABLE => {
                c.expect_cur(&TokenType::TABLE)?;
                let id = Self::parse_id(c)?;
                let tt = Self::parse_tabletype(c)?;
                c.expect_cur(&TokenType::RPAREN)?;

                I.tables.push(id);
                Ok(ImportDesc::table(tt))
            }
            TokenType::MEMORY => {
                c.expect_cur(&TokenType::MEMORY)?;
                let id = Self::parse_id(c)?;
                let mt = Self::parse_memtype(c)?;
                c.expect_cur(&TokenType::RPAREN)?;

                I.mems.push(id);
                Ok(ImportDesc::mem(mt))
            }
            TokenType::GLOBAL => {
                c.expect_cur(&TokenType::GLOBAL)?;
                let id = Self::parse_id(c)?;
                let gt = Self::parse_globaltype(c)?;
                c.expect_cur(&TokenType::RPAREN)?;

                I.globals.push(id);
                Ok(ImportDesc::global(gt))
            }
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    //6.6.5
    fn parse_funcs(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(), parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::FUNC)?;
        let id = Self::parse_id(c)?;
        if c.cur_token_is(&TokenType::LPAREN) {
            match &c.peek_token.r#type {
                TokenType::IMPORT => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::IMPORT)?;
                    let r#mod = Self::parse_name(c)?;
                    let nm = Self::parse_name(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    let (x, I1) = Self::parse_typeuse(c, module, I)?;
                    c.expect_cur(&TokenType::RPAREN)?;

                    module.imports.push(Import {
                        module: r#mod,
                        name: nm,
                        desc: ImportDesc::func(x),
                    });
                    Ok(())
                }
                TokenType::EXPORT => {
                    let mut temp: Vec<ModuleField> = vec![];
                    loop {
                        if c.cur_token_is(&TokenType::LPAREN)
                            && (c.peek_token_is(&TokenType::IMPORT)
                                || c.peek_token_is(&TokenType::EXPORT))
                        {
                            match &c.peek_token.r#type {
                                TokenType::IMPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::IMPORT)?;
                                    let r#mod = Self::parse_name(c)?;
                                    let nm = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::imports(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: ImportDesc::func(u32::MAX), // placeholder
                                    }))
                                }
                                TokenType::EXPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::EXPORT)?;
                                    let nm = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::exports(Export {
                                        name: nm,
                                        desc: ExportDesc::func(u32::MAX), // placeholder
                                    }))
                                }
                                _ => return Err(parser::Error::ParseError(c.cur_token.clone())),
                            }
                        } else {
                            let id = Self::get_fresh_id(c, id);
                            let (x, I1) = Self::parse_typeuse(c, module, I)?;
                            let ts = Self::parse_vec_local(c)?;
                            let mut I2 = I.clone();
                            I2.append(&I1);
                            for local in ts.iter() {
                                I2.locals.push(local.id.clone());
                            }
                            let ins = Self::parse_vec_instr(c, module, &mut I2)?;
                            c.expect_cur(&TokenType::RPAREN)?;

                            I.funcs.push(id);
                            let funcidx = module.funcs.len() as u32;
                            module.funcs.push(Func {
                                r#type: x,
                                locals: ts.into_iter().map(|local| local.valtype).collect(),
                                body: Expr { instrs: ins },
                            });
                            while temp.len() != 0 {
                                match temp.remove(0) {
                                    ModuleField::imports(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: _,
                                    }) => module.imports.push(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: ImportDesc::func(x),
                                    }),
                                    ModuleField::exports(Export { name: nm, desc: _ }) => {
                                        module.exports.push(Export {
                                            name: nm,
                                            desc: ExportDesc::func(funcidx),
                                        })
                                    }
                                    _ => {
                                        return Err(parser::Error::ParseError(c.cur_token.clone()))
                                    }
                                }
                            }
                            return Ok(());
                        }
                    }
                }
                _ => {
                    let (x, I1) = Self::parse_typeuse(c, module, I)?;
                    let locals = Self::parse_vec_local(c)?;
                    let mut I2 = I.clone();
                    I2.append(&I1);
                    for local in locals.iter() {
                        I2.locals.push(local.id.clone());
                    }
                    let body = Self::parse_vec_instr(c, module, &mut I2)?;
                    c.expect_cur(&TokenType::RPAREN)?;

                    I.funcs.push(id);
                    module.funcs.push(Func {
                        r#type: x,
                        locals: locals.into_iter().map(|local| local.valtype).collect(),
                        body: Expr { instrs: body },
                    });
                    Ok(())
                }
            }
        } else if c.cur_token_is(&TokenType::RPAREN) {
            c.expect_cur(&TokenType::RPAREN)?;
            let x = find_or_push_type(
                module,
                &FuncType {
                    params: vec![],
                    results: vec![],
                },
                None,
                I,
            );

            // empty func
            I.funcs.push(id);
            module.funcs.push(Func {
                r#type: x,
                locals: vec![],
                body: Expr { instrs: vec![] },
            });
            Ok(())
        } else {
            Err(parser::Error::ParseError(c.cur_token.clone()))
        }
    }
    fn parse_vec_local(c: &mut parser::ParseContext) -> Result<Vec<Local>, parser::Error> {
        let mut locals: Vec<Local> = vec![];
        loop {
            if (!c.cur_token_is(&TokenType::LPAREN)) || (!c.peek_token_is(&TokenType::LOCAL)) {
                break;
            }
            let mut local = Self::parse_local(c)?;
            locals.append(&mut local);
        }
        Ok(locals)
    }
    fn parse_local(c: &mut parser::ParseContext) -> Result<Vec<Local>, parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::LOCAL)?;
        let id = Self::parse_id(c)?;
        let mut locals: Vec<Local> = vec![];
        let mut valtypes = Self::parse_vec_valtype(c)?;
        c.expect_cur(&TokenType::RPAREN)?;
        while valtypes.len() != 0 {
            locals.push(Local {
                id: id.clone(),
                valtype: valtypes.remove(0),
            })
        }
        Ok(locals)
    }
    fn parse_vec_valtype(c: &mut parser::ParseContext) -> Result<Vec<ValType>, parser::Error> {
        let mut valtypes: Vec<ValType> = vec![];
        loop {
            if c.cur_token_is(&TokenType::RPAREN) {
                break;
            }
            valtypes.push(Self::parse_valtype(c)?);
        }
        Ok(valtypes)
    }
    fn parse_vec_instr(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<Vec<Instr>, parser::Error> {
        let mut instrs: Vec<Instr> = vec![];
        loop {
            if c.cur_token_is(&TokenType::RPAREN) {
                break;
            }
            let mut instr = Self::parse_instr(c, module, I)?;
            instrs.append(&mut instr);
        }
        Ok(instrs)
    }
    //6.6.6
    fn parse_tables(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(), parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::TABLE)?;
        let id = Self::parse_id(c)?;
        if c.cur_token_is(&TokenType::LPAREN) {
            match &c.peek_token.r#type {
                TokenType::IMPORT => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::IMPORT)?;
                    let r#mod = Self::parse_name(c)?;
                    let nm = Self::parse_name(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    let tt = Self::parse_tabletype(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;

                    I.tables.push(id);
                    module.tables.push(Table { r#type: tt.clone() });
                    module.imports.push(Import {
                        module: r#mod,
                        name: nm,
                        desc: ImportDesc::table(tt),
                    });
                    Ok(())
                }
                TokenType::EXPORT => {
                    let mut temp: Vec<ModuleField> = vec![];
                    loop {
                        if c.cur_token_is(&TokenType::LPAREN)
                            && (c.peek_token_is(&TokenType::IMPORT)
                                || c.peek_token_is(&TokenType::EXPORT))
                        {
                            match &c.peek_token.r#type {
                                TokenType::IMPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::IMPORT)?;
                                    let r#mod = Self::parse_name(c)?;
                                    let nm = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::imports(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: ImportDesc::table(TableType {
                                            limits: Limits { min: 0, max: None },
                                            elemtype: ElemType::FuncRef,
                                        }), //placeholder
                                    }))
                                }
                                TokenType::EXPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::EXPORT)?;
                                    let nm = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::exports(Export {
                                        name: nm,
                                        desc: ExportDesc::table(u32::MAX), //placeholder
                                    }));
                                }
                                _ => return Err(parser::Error::ParseError(c.cur_token.clone())),
                            }
                        } else {
                            let tt = Self::parse_tabletype(c)?;
                            c.expect_cur(&TokenType::RPAREN)?;

                            I.tables.push(id);
                            let tableidx = module.tables.len() as u32;
                            module.tables.push(Table { r#type: tt.clone() });
                            while temp.len() != 0 {
                                match temp.remove(0) {
                                    ModuleField::imports(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: _,
                                    }) => module.imports.push(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: ImportDesc::table(tt.clone()),
                                    }),
                                    ModuleField::exports(Export { name: nm, desc: _ }) => {
                                        module.exports.push(Export {
                                            name: nm,
                                            desc: ExportDesc::table(tableidx),
                                        })
                                    }
                                    _ => {
                                        return Err(parser::Error::ParseError(c.cur_token.clone()))
                                    }
                                }
                            }
                            return Ok(());
                        }
                    }
                }
                _ => return Err(parser::Error::ParseError(c.cur_token.clone())),
            }
        } else if c.cur_token_is(&TokenType::FUNCREF) {
            let elemtype = Self::parse_elemtype(c)?;
            c.expect_cur(&TokenType::LPAREN)?;
            c.expect_cur(&TokenType::ELEM)?;
            let xn = Self::parse_vec_funcidx(c, I)?;
            let n = xn.len() as u32;
            c.expect_cur(&TokenType::RPAREN)?;
            c.expect_cur(&TokenType::RPAREN)?;

            I.tables.push(id);
            let tableidx = module.tables.len() as u32;
            module.tables.push(Table {
                r#type: TableType {
                    limits: Limits {
                        min: n,
                        max: Some(n),
                    },
                    elemtype: elemtype,
                },
            });
            module.elem.push(Elem {
                table: tableidx,
                offset: Expr {
                    instrs: vec![Instr::i32_const(0)],
                },
                init: xn,
            });
            Ok(())
        } else {
            let tt = Self::parse_tabletype(c)?;
            c.expect_cur(&TokenType::RPAREN)?;
            I.tables.push(id);
            module.tables.push(Table { r#type: tt });
            Ok(())
        }
    }
    pub fn parse_vec_funcidx(
        c: &mut parser::ParseContext,
        I: &mut IdentifierContext,
    ) -> Result<Vec<u32>, parser::Error> {
        let mut funcidxes: Vec<u32> = vec![];
        loop {
            if c.cur_token_is(&TokenType::RPAREN) {
                break;
            }
            funcidxes.push(Self::parse_funcidx(c, I)?);
        }
        Ok(funcidxes)
    }
    //6.6.7
    fn parse_mems(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(), parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::MEMORY)?;
        let id = Self::parse_id(c)?;
        if c.cur_token_is(&TokenType::LPAREN) {
            match &c.peek_token.r#type {
                TokenType::IMPORT => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::IMPORT)?;
                    let r#mod = Self::parse_name(c)?;
                    let nm = Self::parse_name(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    let mt = Self::parse_memtype(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;

                    I.mems.push(id);
                    module.mems.push(Mem { r#type: mt.clone() });
                    module.imports.push(Import {
                        module: r#mod,
                        name: nm,
                        desc: ImportDesc::mem(mt),
                    });
                    Ok(())
                }
                TokenType::EXPORT => {
                    let mut temp: Vec<ModuleField> = vec![];
                    loop {
                        if c.cur_token_is(&TokenType::LPAREN)
                            && (c.peek_token_is(&TokenType::IMPORT)
                                || c.peek_token_is(&TokenType::EXPORT))
                        {
                            match &c.peek_token.r#type {
                                TokenType::IMPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::IMPORT)?;
                                    let r#mod = Self::parse_name(c)?;
                                    let nm = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::imports(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: ImportDesc::mem(MemType {
                                            limits: Limits { min: 0, max: None },
                                        }), // placeholder
                                    }));
                                }
                                TokenType::EXPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::EXPORT)?;
                                    let nm = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::exports(Export {
                                        name: nm,
                                        desc: ExportDesc::mem(0), //placeholder
                                    }))
                                }
                                _ => return Err(parser::Error::ParseError(c.cur_token.clone())),
                            }
                        } else {
                            let mt = Self::parse_memtype(c)?;
                            c.expect_cur(&TokenType::RPAREN)?;

                            I.mems.push(id);
                            let memidx = I.mems.len() as u32;
                            module.mems.push(Mem { r#type: mt.clone() });
                            while temp.len() != 0 {
                                match temp.remove(0) {
                                    ModuleField::imports(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: _,
                                    }) => module.imports.push(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: ImportDesc::mem(mt.clone()),
                                    }),

                                    ModuleField::exports(Export { name: nm, desc: _ }) => {
                                        module.exports.push(Export {
                                            name: nm,
                                            desc: ExportDesc::mem(memidx),
                                        })
                                    }
                                    _ => {
                                        return Err(parser::Error::ParseError(c.cur_token.clone()))
                                    }
                                }
                            }
                            return Ok(());
                        }
                    }
                }
                TokenType::DATA => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::DATA)?;
                    let datastring = Self::parse_vec_string(c)?
                        .into_iter()
                        .flatten()
                        .collect::<Vec<u8>>();
                    let n = datastring.len() as f64;
                    let m = (n / (64_f64 * 1024_f64)).ceil() as u32;
                    c.expect_cur(&TokenType::RPAREN)?;
                    c.expect_cur(&TokenType::RPAREN)?;

                    I.mems.push(id);
                    let memidx = I.mems.len() as u32;
                    module.mems.push(Mem {
                        r#type: MemType {
                            limits: Limits {
                                min: m,
                                max: Some(m),
                            },
                        },
                    });
                    module.data.push(Data {
                        data: memidx,
                        offset: Expr {
                            instrs: vec![Instr::i32_const(0)],
                        },
                        init: datastring,
                    });
                    Ok(())
                }
                _ => Err(parser::Error::ParseError(c.cur_token.clone())),
            }
        } else {
            let mt = Self::parse_memtype(c)?;
            c.expect_cur(&TokenType::RPAREN)?;

            I.mems.push(id);
            module.mems.push(Mem { r#type: mt });
            Ok(())
        }
    }
    pub fn parse_vec_string(c: &mut parser::ParseContext) -> Result<Vec<Vec<u8>>, parser::Error> {
        let mut strs: Vec<Vec<u8>> = vec![];
        loop {
            match &c.cur_token.r#type {
                TokenType::STRING(buf) => {
                    strs.push(buf.to_vec());
                    c.next_token()?;
                }
                _ => return Ok(strs),
            }
        }
    }
    //6.6.8
    fn parse_globals(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(), parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::GLOBAL)?;
        let id = Self::parse_id(c)?;
        if c.cur_token_is(&TokenType::LPAREN) {
            match &c.peek_token.r#type {
                TokenType::IMPORT => {
                    c.expect_cur(&TokenType::LPAREN)?;
                    c.expect_cur(&TokenType::IMPORT)?;
                    let r#mod = Self::parse_name(c)?;
                    let nm = Self::parse_name(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;
                    let gt = Self::parse_globaltype(c)?;
                    c.expect_cur(&TokenType::RPAREN)?;

                    I.globals.push(id);
                    module.globals.push(Global {
                        r#type: gt.clone(),
                        init: Expr { instrs: vec![] },
                    });
                    module.imports.push(Import {
                        module: r#mod,
                        name: nm,
                        desc: ImportDesc::global(gt),
                    });
                    Ok(())
                }
                TokenType::EXPORT => {
                    let mut temp: Vec<ModuleField> = vec![];
                    loop {
                        if c.cur_token_is(&TokenType::LPAREN)
                            && (c.peek_token_is(&TokenType::IMPORT)
                                || c.peek_token_is(&TokenType::EXPORT))
                        {
                            match &c.peek_token.r#type {
                                TokenType::IMPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::IMPORT)?;
                                    let r#mod = Self::parse_name(c)?;
                                    let nm = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::imports(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: ImportDesc::global(GlobalType {
                                            r#mut: Mut::Const,
                                            valtype: ValType::r#i32,
                                        }), //placeholder
                                    }));
                                }
                                TokenType::EXPORT => {
                                    c.expect_cur(&TokenType::LPAREN)?;
                                    c.expect_cur(&TokenType::EXPORT)?;
                                    let nm = Self::parse_name(c)?;
                                    c.expect_cur(&TokenType::RPAREN)?;
                                    temp.push(ModuleField::exports(Export {
                                        name: nm,
                                        desc: ExportDesc::global(0), //placeholder
                                    }))
                                }
                                _ => return Err(parser::Error::ParseError(c.cur_token.clone())),
                            }
                        } else {
                            let gt = Self::parse_globaltype(c)?;
                            let e = Self::parse_expr(c, module, I)?;
                            c.expect_cur(&TokenType::RPAREN)?;

                            I.globals.push(id);
                            let globalidx = module.globals.len() as u32;
                            module.globals.push(Global {
                                r#type: gt.clone(),
                                init: e,
                            });
                            while temp.len() != 0 {
                                match temp.remove(0) {
                                    ModuleField::imports(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: _,
                                    }) => module.imports.push(Import {
                                        module: r#mod,
                                        name: nm,
                                        desc: ImportDesc::global(gt.clone()),
                                    }),
                                    ModuleField::exports(Export { name: nm, desc: _ }) => {
                                        module.exports.push(Export {
                                            name: nm,
                                            desc: ExportDesc::global(globalidx),
                                        })
                                    }
                                    _ => {
                                        return Err(parser::Error::ParseError(c.cur_token.clone()))
                                    }
                                }
                            }
                            return Ok(());
                        }
                    }
                }
                _ => {
                    let gt = Self::parse_globaltype(c)?;
                    let e = Self::parse_expr(c, module, I)?;
                    c.expect_cur(&TokenType::RPAREN)?;

                    I.globals.push(id);
                    module.globals.push(Global {
                        r#type: gt,
                        init: e,
                    });
                    Ok(())
                }
            }
        } else {
            let gt = Self::parse_globaltype(c)?;
            let e = Self::parse_expr(c, module, I)?;
            c.expect_cur(&TokenType::RPAREN)?;

            I.globals.push(id);
            module.globals.push(Global {
                r#type: gt,
                init: e,
            });
            Ok(())
        }
    }
    //6.6.9
    fn parse_exports(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(), parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::EXPORT)?;
        let nm = Self::parse_name(c)?;
        let d = Self::parse_exportdesc(c, I)?;

        module.exports.push(Export { name: nm, desc: d });
        Ok(())
    }
    fn parse_exportdesc(
        c: &mut parser::ParseContext,
        I: &mut IdentifierContext,
    ) -> Result<ExportDesc, parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        match &c.cur_token.r#type {
            TokenType::FUNC => {
                c.expect_cur(&TokenType::FUNC)?;
                let x = Self::parse_funcidx(c, I)?;
                Ok(ExportDesc::func(x))
            }
            TokenType::TABLE => {
                c.expect_cur(&TokenType::TABLE)?;
                let x = Self::parse_tableidx(c, I)?;
                Ok(ExportDesc::table(x))
            }
            TokenType::MEMORY => {
                c.expect_cur(&TokenType::MEMORY)?;
                let x = Self::parse_memidx(c, I)?;
                Ok(ExportDesc::mem(x))
            }
            TokenType::GLOBAL => {
                c.expect_cur(&TokenType::GLOBAL)?;
                let x = Self::parse_globalidx(c, I)?;
                Ok(ExportDesc::global(x))
            }
            _ => Err(parser::Error::ParseError(c.cur_token.clone())),
        }
    }
    //6.6.10
    fn parse_starts(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(), parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::START)?;
        let x = Self::parse_funcidx(c, I)?;
        c.expect_cur(&TokenType::RPAREN)?;

        module.start = Some(Start { func: x });
        Ok(())
    }
    //6.6.11
    fn parse_elems(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(), parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::ELEM)?;
        let x = match &c.cur_token.r#type {
            TokenType::NUM(_) | TokenType::ID(_) => Self::parse_tableidx(c, I)?,
            _ => 0,
        };
        let e = if c.cur_token_is(&TokenType::LPAREN) && c.peek_token_is(&TokenType::OFFSET) {
            c.expect_cur(&TokenType::LPAREN)?;
            c.expect_cur(&TokenType::OFFSET)?;
            let e = Self::parse_expr(c, module, I)?;
            c.expect_cur(&TokenType::RPAREN)?;
            e
        } else {
            Expr {
                instrs: Self::parse_instr(c, module, I)?,
            }
        };
        let ys = Self::parse_vec_funcidx(c, I)?;
        c.expect_cur(&TokenType::RPAREN)?;

        module.elem.push(Elem {
            table: x,
            offset: e,
            init: ys,
        });
        Ok(())
    }
    //6.6.12
    fn parse_datas(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(), parser::Error> {
        c.expect_cur(&TokenType::LPAREN)?;
        c.expect_cur(&TokenType::DATA)?;
        let x = match &c.cur_token.r#type {
            TokenType::NUM(_) | TokenType::ID(_) => Self::parse_memidx(c, I)?,
            _ => 0,
        };
        let e = if c.cur_token_is(&TokenType::LPAREN) && c.peek_token_is(&TokenType::OFFSET) {
            c.expect_cur(&TokenType::LPAREN)?;
            c.expect_cur(&TokenType::OFFSET)?;
            let e = Self::parse_expr(c, module, I)?;
            c.expect_cur(&TokenType::RPAREN)?;
            e
        } else {
            Expr {
                instrs: Self::parse_instr(c, module, I)?,
            }
        };
        let bs = Self::parse_vec_string(c)?;
        c.expect_cur(&TokenType::RPAREN)?;

        module.data.push(Data {
            data: x,
            offset: e,
            init: bs.into_iter().flatten().collect(),
        });
        Ok(())
    }
    //6.6.13
    pub fn parse_module(
        c: &mut parser::ParseContext,
    ) -> Result<(Option<Id>, Module), parser::Error> {
        if c.cur_token_is(&TokenType::EOF) {
            return Err(parser::Error::Malformed(String::from("unexpected end")));
        }
        let mut I = IdentifierContext::new();
        let mut module = Module::new();
        let has_module =
            if c.cur_token_is(&TokenType::LPAREN) && c.peek_token_is(&TokenType::MODULE) {
                c.expect_cur(&TokenType::LPAREN)?;
                c.expect_cur(&TokenType::MODULE)?;
                true
            } else {
                false
            };
        let id = Self::parse_id(c)?;
        loop {
            if !c.cur_token_is(&TokenType::LPAREN) {
                break;
            }
            Self::parse_modulefield(c, &mut module, &mut I)?;
        }
        if has_module {
            c.expect_cur(&TokenType::RPAREN)?;
        }
        Ok((id, module))
    }
    pub fn parse_modulefield(
        c: &mut parser::ParseContext,
        module: &mut Module,
        I: &mut IdentifierContext,
    ) -> Result<(), parser::Error> {
        match &c.peek_token.r#type {
            TokenType::TYPE => Self::parse_types(c, module, I)?,
            TokenType::IMPORT => Self::parse_imports(c, module, I)?,
            TokenType::FUNC => Self::parse_funcs(c, module, I)?,
            TokenType::TABLE => Self::parse_tables(c, module, I)?,
            TokenType::MEMORY => Self::parse_mems(c, module, I)?,
            TokenType::GLOBAL => Self::parse_globals(c, module, I)?,
            TokenType::EXPORT => Self::parse_exports(c, module, I)?,
            TokenType::START => Self::parse_starts(c, module, I)?,
            TokenType::ELEM => Self::parse_elems(c, module, I)?,
            TokenType::DATA => Self::parse_datas(c, module, I)?,
            _ => return Err(parser::Error::ParseError(c.cur_token.clone())),
        }
        Ok(())
    }

    fn process_instr(c: &mut parser::ParseContext, instr: Instr) -> Result<Instr, parser::Error> {
        c.next_token()?;
        Ok(instr)
    }
    fn get_fresh_id(c: &mut parser::ParseContext, id: Option<Id>) -> Option<Id> {
        match id {
            Some(v) => Some(v),
            _ => {
                c.cur_new_id += 1;
                Some(format!("$${}", c.cur_new_id))
            }
        }
    }
}
impl parser::Parse<(Option<Id>, Module)> for ModuleParser {
    fn parse(
        c: &mut parser::ParseContext,
    ) -> std::result::Result<(Option<Id>, Module), parser::Error> {
        Ok(Self::parse_module(c)?)
    }
}
