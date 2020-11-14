use crate::leb128;
use crate::structure::*;
use std::io::Cursor;
use std::io::Read;

#[derive(Debug)]
pub enum Error {
    DecodeError,
    Leb128Error(leb128::Error),
    Utf8Error(std::string::FromUtf8Error),
    NotImplemented(u8),
    Malformed(String),
}
impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::Malformed(String::from("unexpected end"))
    }
}
impl From<leb128::Error> for Error {
    fn from(e: leb128::Error) -> Self {
        Self::Leb128Error(e)
    }
}
impl From<std::string::FromUtf8Error> for Error {
    fn from(e: std::string::FromUtf8Error) -> Self {
        Self::Utf8Error(e)
    }
}

pub fn decode(bytes: Vec<u8>) -> Result<Module, Error> {
    decode_module(&mut Cursor::new(&bytes[..]))
}

struct Custom {
    name: Name,
    bytes: Vec<u8>,
}
fn decode_customesec(input: &mut Cursor<&[u8]>) -> Result<Custom, Error> {
    let size = decode_u32(input)?;
    let mut buf = vec![0_u8; size as usize];
    input.read_exact(&mut buf)?;
    let mut cont = Cursor::new(&buf[..]);
    let byte_count = decode_u32(&mut cont)?;
    let mut name_buf = vec![0_u8; byte_count as usize];
    cont.read_exact(&mut name_buf)?;
    let name = String::from_utf8(name_buf)?;
    let pos = cont.position() as usize;
    Ok(Custom {
        name: name,
        bytes: cont.get_ref()[pos..].to_vec(),
    })
}
fn decode_module(input: &mut Cursor<&[u8]>) -> Result<Module, Error> {
    decode_magic(input)?;
    decode_version(input)?;
    let mut module = Module::new();
    let mut typeidxes = vec![];
    let mut codes = vec![];
    loop {
        match decode_byte(input) {
            Ok(section_id) => match section_id {
                0x00 => {
                    decode_customesec(input)?;
                }
                0x01 => module.types = decode_typesec(input)?,
                0x02 => module.imports = decode_importsec(input)?,
                0x03 => typeidxes = decode_funcsec(input)?,
                0x04 => module.tables = decode_tablesec(input)?,
                0x05 => module.mems = decode_memsec(input)?,
                0x06 => module.globals = decode_globalsec(input)?,
                0x07 => module.exports = decode_exportsec(input)?,
                0x08 => {
                    let start = decode_startsec(input)?;
                    if start.is_some() && module.start.is_some() {
                        return Err(Error::Malformed(String::from("junk after last section")));
                    }
                    module.start = start;
                }
                0x09 => module.elem = decode_elemsec(input)?,
                0x0A => {
                    codes = decode_codesec(input)?;
                    let mut funcs: Vec<Func> = vec![];
                    for (i, c) in codes.iter().enumerate() {
                        funcs.push(Func {
                            r#type: {
                                if typeidxes.len() <= i {
                                    return Err(Error::Malformed(String::from(
                                        "function and code section have inconsistent lengths",
                                    )));
                                }
                                typeidxes[i]
                            },
                            locals: (&c.code).locals.iter().map(|x| x.t.clone()).collect(),
                            body: c.code.e.clone(),
                        })
                    }
                }
                0x0B => module.data = decode_datasec(input)?,
                _ => return Err(Error::Malformed(String::from("malformed section id"))),
            },
            _ => break,
        }
        if input.get_ref().len() == input.position() as usize {
            break;
        }
    }
    if typeidxes.len() != codes.len() {
        return Err(Error::Malformed(String::from(
            "function and code section have inconsistent lengths",
        )));
    }

    Ok(module)
}
fn decode_magic(input: &mut Cursor<&[u8]>) -> Result<[u8; 4], Error> {
    let mut magic: [u8; 4] = Default::default();
    match input.read_exact(&mut magic) {
        Ok(_) => match magic {
            [0x00, 0x61, 0x73, 0x6D] => Ok(magic),
            _ => Err(Error::Malformed(String::from("magic header not detected"))),
        },
        Err(e) => Err(e.into()),
    }
}
fn decode_version(input: &mut Cursor<&[u8]>) -> Result<[u8; 4], Error> {
    let mut version: [u8; 4] = Default::default();
    match input.read_exact(&mut version) {
        Ok(_) => match version {
            [0x01, 0x00, 0x00, 0x00] => Ok(version),
            _ => Err(Error::Malformed(String::from("unknown binary version"))),
        },
        Err(e) => Err(e.into()),
    }
}
fn decode_sec<T>(input: &mut Cursor<&[u8]>, id: u32) -> Result<Vec<T>, Error>
where
    T: Vectorable,
{
    let size = decode_u32(input)? as u64;
    let max = input.position() + size;
    let ret = decode_vec::<T>(input, max)?;
    let newpos = input.position();
    if newpos < max {
        return Err(Error::Malformed(String::from("section size mismatch")));
    } else if newpos > max {
        return Err(Error::Malformed(String::from("asdfasf")));
    }
    Ok(ret)
}
fn decode_vec<T>(input: &mut Cursor<&[u8]>, max: u64) -> Result<Vec<T>, Error>
where
    T: Vectorable,
{
    let n = decode_u32(input)?;
    let mut xn: Vec<T> = vec![];
    for _ in 0..n {
        if input.position() >= max {
            return Err(Error::Malformed(String::from(
                "unexpected end of section or function",
            )));
        }
        xn.push(T::decode(input, max)?)
    }
    Ok(xn)
}
fn decode_typesec(input: &mut Cursor<&[u8]>) -> Result<Vec<FuncType>, Error> {
    decode_sec::<FuncType>(input, 0x01)
}
fn decode_importsec(input: &mut Cursor<&[u8]>) -> Result<Vec<Import>, Error> {
    decode_sec::<Import>(input, 0x02)
}
fn decode_funcsec(input: &mut Cursor<&[u8]>) -> Result<Vec<TypeIdx>, Error> {
    decode_sec::<TypeIdx>(input, 0x03)
}
fn decode_tablesec(input: &mut Cursor<&[u8]>) -> Result<Vec<Table>, Error> {
    decode_sec::<Table>(input, 0x04)
}
fn decode_memsec(input: &mut Cursor<&[u8]>) -> Result<Vec<Mem>, Error> {
    decode_sec::<Mem>(input, 0x05)
}
fn decode_globalsec(input: &mut Cursor<&[u8]>) -> Result<Vec<Global>, Error> {
    decode_sec::<Global>(input, 0x06)
}
fn decode_exportsec(input: &mut Cursor<&[u8]>) -> Result<Vec<Export>, Error> {
    decode_sec::<Export>(input, 0x07)
}
fn decode_startsec(input: &mut Cursor<&[u8]>) -> Result<Option<Start>, Error> {
    let size = decode_u32(input)?;
    let mut buf = vec![0_u8; size as usize];
    input.read_exact(&mut buf)?;
    let mut cont = Cursor::new(&buf[..]);
    let start = decode_start(&mut cont)?;
    Ok(Some(start))
}
fn decode_elemsec(input: &mut Cursor<&[u8]>) -> Result<Vec<Elem>, Error> {
    match decode_sec::<Elem>(input, 0x09) {
        Ok(v) => Ok(v),
        Err(Error::Malformed(msg)) => {
            if msg == String::from("unexpected end of section or function") {
                Err(Error::Malformed(String::from("unexpected end")))
            } else {
                Err(Error::Malformed(msg))
            }
        }
        Err(e) => Err(e.into()),
    }
}
fn decode_codesec(input: &mut Cursor<&[u8]>) -> Result<Vec<Code>, Error> {
    decode_sec::<Code>(input, 0x0A)
}
fn decode_datasec(input: &mut Cursor<&[u8]>) -> Result<Vec<Data>, Error> {
    decode_sec::<Data>(input, 0x0B)
}
fn decode_functype(input: &mut Cursor<&[u8]>, max: u64) -> Result<FuncType, Error> {
    let len = input.get_ref().len();
    if input.position() as usize == len {
        return Err(Error::Malformed(String::from(
            "unexpected end of section or function",
        )));
    }

    expect_byte(input, 0x60)?;
    Ok(FuncType {
        params: decode_resulttype(input, max)?,
        results: decode_resulttype(input, max)?,
    })
}
fn decode_byte(input: &mut Cursor<&[u8]>) -> Result<u8, Error> {
    let mut bytes: [u8; 1] = Default::default();
    input.read_exact(&mut bytes)?;
    Ok(bytes[0])
}
fn expect_byte(input: &mut Cursor<&[u8]>, expected: u8) -> Result<(), Error> {
    let byte = decode_byte(input)?;
    if byte == expected {
        Ok(())
    } else {
        Err(Error::DecodeError)
    }
}
fn decode_resulttype(input: &mut Cursor<&[u8]>, max: u64) -> Result<ResultType, Error> {
    decode_vec::<ValType>(input, max)
}
fn decode_import(input: &mut Cursor<&[u8]>) -> Result<Import, Error> {
    let len = input.get_ref().len();
    if input.position() as usize == len {
        return Err(Error::Malformed(String::from(
            "unexpected end of section or function",
        )));
    }
    Ok(Import {
        module: decode_name(input)?,
        name: decode_name(input)?,
        desc: decode_importdesc(input)?,
    })
}
fn decode_name(input: &mut Cursor<&[u8]>) -> Result<Name, Error> {
    let n = decode_u32(input)?;
    let mut buf = vec![0_u8; n as usize];
    input.read_exact(&mut buf)?;
    Ok(String::from_utf8(buf)?)
}
fn decode_importdesc(input: &mut Cursor<&[u8]>) -> Result<ImportDesc, Error> {
    let byte = decode_byte(input)?;
    match byte {
        0x00 => Ok(ImportDesc::func(decode_typeidx(input)?)),
        0x01 => Ok(ImportDesc::table(decode_tabletype(input)?)),
        0x02 => Ok(ImportDesc::mem(decode_memtype(input)?)),
        0x03 => Ok(ImportDesc::global(decode_globaltype(input)?)),
        _ => Err(Error::Malformed(String::from("malformed import kind"))),
    }
}
fn decode_typeidx(input: &mut Cursor<&[u8]>) -> Result<TypeIdx, Error> {
    decode_u32(input)
}
fn decode_funcidx(input: &mut Cursor<&[u8]>) -> Result<FuncIdx, Error> {
    decode_u32(input)
}
fn decode_tableidx(input: &mut Cursor<&[u8]>) -> Result<TableIdx, Error> {
    decode_u32(input)
}
fn decode_memidx(input: &mut Cursor<&[u8]>) -> Result<MemIdx, Error> {
    decode_u32(input)
}
fn decode_globalidx(input: &mut Cursor<&[u8]>) -> Result<GlobalIdx, Error> {
    decode_u32(input)
}
fn decode_localidx(input: &mut Cursor<&[u8]>) -> Result<LocalIdx, Error> {
    decode_u32(input)
}
fn decode_labelidx(input: &mut Cursor<&[u8]>) -> Result<LabelIdx, Error> {
    decode_u32(input)
}
fn decode_u32(input: &mut Cursor<&[u8]>) -> Result<u32, Error> {
    match leb128::read_unsigned(input, 35) {
        Err(leb128::Error::Overflow) => {
            return Err(Error::Malformed(String::from(
                "integer representation too long",
            )))
        }
        Ok(v) => {
            if v > (u32::MAX as u64) {
                return Err(Error::Malformed(String::from("integer too large")));
            }
            Ok(v as u32)
        }
        Err(e) => Err(e.into()),
    }
}
fn decode_tabletype(input: &mut Cursor<&[u8]>) -> Result<TableType, Error> {
    Ok(TableType {
        elemtype: decode_elemtype(input)?,
        limits: decode_limits(input)?,
    })
}
fn decode_elemtype(input: &mut Cursor<&[u8]>) -> Result<ElemType, Error> {
    let byte = decode_byte(input)?;
    match byte {
        0x70 => Ok(ElemType::FuncRef),
        _ => Err(Error::DecodeError),
    }
}
fn decode_limits(input: &mut Cursor<&[u8]>) -> Result<Limits, Error> {
    let pos = input.position();
    let byte = decode_byte(input)?;
    match byte {
        0x00 => Ok(Limits {
            min: decode_u32(input)?,
            max: None,
        }),
        0x01 => Ok(Limits {
            min: decode_u32(input)?,
            max: Some(decode_u32(input)?),
        }),
        _ => {
            input.set_position(pos);
            let byte = decode_byte(input)?;
            if byte == 0x00 || byte == 0x01 || byte >= 128 {
                return Err(Error::Malformed(String::from(
                    "integer representation too long",
                )));
            }
            Err(Error::Malformed(String::from("integer too large")))
        }
    }
}
fn decode_memtype(input: &mut Cursor<&[u8]>) -> Result<MemType, Error> {
    Ok(MemType {
        limits: decode_limits(input)?,
    })
}
fn decode_globaltype(input: &mut Cursor<&[u8]>) -> Result<GlobalType, Error> {
    Ok(GlobalType {
        valtype: decode_valtype(input)?,
        r#mut: decode_mut(input)?,
    })
}
fn decode_valtype(input: &mut Cursor<&[u8]>) -> Result<ValType, Error> {
    let byte = decode_byte(input)?;
    match byte {
        0x7F => Ok(ValType::i32),
        0x7E => Ok(ValType::i64),
        0x7D => Ok(ValType::f32),
        0x7C => Ok(ValType::f64),
        _ => Err(Error::DecodeError),
    }
}
fn decode_mut(input: &mut Cursor<&[u8]>) -> Result<Mut, Error> {
    let byte = decode_byte(input)?;
    match byte {
        0x00 => Ok(Mut::Const),
        0x01 => Ok(Mut::Var),
        _ => Err(Error::DecodeError),
    }
}
fn decode_table(input: &mut Cursor<&[u8]>) -> Result<Table, Error> {
    let len = input.get_ref().len();
    if input.position() as usize == len {
        return Err(Error::Malformed(String::from(
            "unexpected end of section or function",
        )));
    }
    Ok(Table {
        r#type: decode_tabletype(input)?,
    })
}
fn decode_mem(input: &mut Cursor<&[u8]>) -> Result<Mem, Error> {
    let len = input.get_ref().len();
    if input.position() as usize == len {
        return Err(Error::Malformed(String::from(
            "unexpected end of section or function",
        )));
    }
    Ok(Mem {
        r#type: decode_memtype(input)?,
    })
}
fn decode_global(input: &mut Cursor<&[u8]>, max: u64) -> Result<Global, Error> {
    let len = input.get_ref().len();
    if input.position() as usize == len {
        return Err(Error::Malformed(String::from(
            "unexpected end of section or function",
        )));
    }
    Ok(Global {
        r#type: decode_globaltype(input)?,
        init: decode_expr(input, max)?,
    })
}
fn decode_expr(input: &mut Cursor<&[u8]>, max: u64) -> Result<Expr, Error> {
    let mut expr = Expr::new();
    loop {
        let pos = input.position();
        let byte = decode_byte(input)?;
        if byte == 0x0B {
            return Ok(expr);
        }
        input.set_position(pos);
        expr.instrs.push(decode_instr(input, max)?)
    }
}
fn decode_blocktype(input: &mut Cursor<&[u8]>) -> Result<BlockType, Error> {
    let pos = input.position();
    let byte = decode_byte(input)?;
    match byte {
        0x40 => Ok(BlockType::valtype(None)),
        0x7F => Ok(BlockType::valtype(Some(ValType::r#i32))),
        0x7E => Ok(BlockType::valtype(Some(ValType::r#i64))),
        0x7D => Ok(BlockType::valtype(Some(ValType::r#f32))),
        0x7C => Ok(BlockType::valtype(Some(ValType::r#f64))),
        _ => {
            input.set_position(pos);
            Ok(BlockType::typeidx(leb128::read_signed(input, 33)? as u32))
        }
    }
}
fn decode_end(input: &mut Cursor<&[u8]>) -> Result<(), Error> {
    let byte = decode_byte(input)?;
    if byte != 0x0B {
        Err(Error::Malformed(String::from("aaa")))
    } else {
        Ok(())
    }
}
fn decode_instr(input: &mut Cursor<&[u8]>, max: u64) -> Result<Instr, Error> {
    let byte = decode_byte(input)?;
    match byte {
        //5.4.1 Control Instructions
        0x00 => Ok(Instr::unreachable),
        0x01 => Ok(Instr::nop),
        0x02 => {
            let blocktype = decode_blocktype(input)?;
            let mut instrs: Vec<Instr> = vec![];
            loop {
                let pos = input.position();
                let byte = decode_byte(input)?;
                if byte == 0x0B {
                    return Ok(Instr::block {
                        blocktype: blocktype,
                        instrs: instrs,
                    });
                } else {
                    input.set_position(pos);
                    let instr = decode_instr(input, max)?;
                    instrs.push(instr);
                }
            }
        }
        0x03 => {
            let blocktype = decode_blocktype(input)?;
            let mut instrs: Vec<Instr> = vec![];
            loop {
                let pos = input.position();
                let byte = decode_byte(input)?;
                if byte == 0x0B {
                    return Ok(Instr::r#loop {
                        blocktype: blocktype,
                        instrs: instrs,
                    });
                } else {
                    input.set_position(pos);
                    let instr = decode_instr(input, max)?;
                    instrs.push(instr);
                }
            }
        }
        0x04 => {
            let blocktype = decode_blocktype(input)?;
            let mut instrs1: Vec<Instr> = vec![];
            let mut instrs2: Vec<Instr> = vec![];
            let mut instrs_ref = &mut instrs1;
            loop {
                let pos = input.position();
                let byte = decode_byte(input)?;
                if byte == 0x05 {
                    instrs_ref = &mut instrs2;
                } else if byte == 0x0B {
                    return Ok(Instr::r#if {
                        blocktype: blocktype,
                        instrs1: instrs1,
                        instrs2: instrs2,
                    });
                } else {
                    input.set_position(pos);
                    let instr = decode_instr(input, max)?;
                    instrs_ref.push(instr);
                }
            }
        }
        0x0C => Ok(Instr::br(decode_labelidx(input)?)),
        0x0D => Ok(Instr::br_if(decode_labelidx(input)?)),
        0x0E => Ok(Instr::br_table {
            labelidxes: decode_vec::<LabelIdx>(input, max)?,
            labelidx: decode_labelidx(input)?,
        }),
        0x0F => Ok(Instr::r#return),
        0x10 => Ok(Instr::call(decode_funcidx(input)?)),
        0x11 => {
            let call_indirect = Instr::call_indirect(decode_typeidx(input)?);
            let zero = decode_byte(input)?;
            if zero != 0x00 {
                return Err(Error::Malformed(String::from("zero flag expected")));
            }
            Ok(call_indirect)
        }
        //5.4.2 Parametric Instructions
        0x1A => Ok(Instr::drop),
        0x1B => Ok(Instr::select),
        //5.4.3 Variable Instructions
        0x20 => Ok(Instr::local_get(decode_localidx(input)?)),
        0x21 => Ok(Instr::local_set(decode_localidx(input)?)),
        0x22 => Ok(Instr::local_tee(decode_localidx(input)?)),
        0x23 => Ok(Instr::global_get(decode_globalidx(input)?)),
        0x24 => Ok(Instr::global_set(decode_globalidx(input)?)),
        //5.4.4 Memory Instructions
        0x28 => Ok(Instr::i32_load(decode_memarg(input)?)),
        0x29 => Ok(Instr::i64_load(decode_memarg(input)?)),
        0x2A => Ok(Instr::f32_load(decode_memarg(input)?)),
        0x2B => Ok(Instr::f64_load(decode_memarg(input)?)),
        0x2C => Ok(Instr::i32_load8_s(decode_memarg(input)?)),
        0x2D => Ok(Instr::i32_load8_u(decode_memarg(input)?)),
        0x2E => Ok(Instr::i32_load16_s(decode_memarg(input)?)),
        0x2F => Ok(Instr::i32_load16_u(decode_memarg(input)?)),
        0x30 => Ok(Instr::i64_load8_s(decode_memarg(input)?)),
        0x31 => Ok(Instr::i64_load8_u(decode_memarg(input)?)),
        0x32 => Ok(Instr::i64_load16_s(decode_memarg(input)?)),
        0x33 => Ok(Instr::i64_load16_u(decode_memarg(input)?)),
        0x34 => Ok(Instr::i64_load32_s(decode_memarg(input)?)),
        0x35 => Ok(Instr::i64_load32_u(decode_memarg(input)?)),
        0x36 => Ok(Instr::i32_store(decode_memarg(input)?)),
        0x37 => Ok(Instr::i64_store(decode_memarg(input)?)),
        0x38 => Ok(Instr::f32_store(decode_memarg(input)?)),
        0x39 => Ok(Instr::f64_store(decode_memarg(input)?)),
        0x3A => Ok(Instr::i32_store8(decode_memarg(input)?)),
        0x3B => Ok(Instr::i32_store16(decode_memarg(input)?)),
        0x3C => Ok(Instr::i64_store8(decode_memarg(input)?)),
        0x3D => Ok(Instr::i64_store16(decode_memarg(input)?)),
        0x3E => Ok(Instr::i64_store32(decode_memarg(input)?)),
        0x3F => {
            let zero = decode_byte(input)?;
            if zero != 0 {
                return Err(Error::Malformed(String::from("zero flag expected")));
            }
            Ok(Instr::memory_size)
        }
        0x40 => {
            let zero = decode_byte(input)?;
            if zero != 0 {
                return Err(Error::Malformed(String::from("zero flag expected")));
            }
            Ok(Instr::memory_grow)
        }
        //5.4.5 Numeric Instructions
        0x41 => Ok(Instr::i32_const(decode_i32(input)?)),
        0x42 => Ok(Instr::i64_const(decode_i64(input)?)),
        0x43 => Ok(Instr::f32_const(decode_f32(input)?)),
        0x44 => Ok(Instr::f64_const(decode_f64(input)?)),

        0x45 => Ok(Instr::i32_eqz),
        0x46 => Ok(Instr::i32_eq),
        0x47 => Ok(Instr::i32_ne),
        0x48 => Ok(Instr::i32_lt_s),
        0x49 => Ok(Instr::i32_lt_u),
        0x4A => Ok(Instr::i32_gt_s),
        0x4B => Ok(Instr::i32_gt_u),
        0x4C => Ok(Instr::i32_le_s),
        0x4D => Ok(Instr::i32_le_u),
        0x4E => Ok(Instr::i32_ge_s),
        0x4F => Ok(Instr::i32_ge_u),

        0x50 => Ok(Instr::i64_eqz),
        0x51 => Ok(Instr::i64_eq),
        0x52 => Ok(Instr::i64_ne),
        0x53 => Ok(Instr::i64_lt_s),
        0x54 => Ok(Instr::i64_lt_u),
        0x55 => Ok(Instr::i64_gt_s),
        0x56 => Ok(Instr::i64_gt_u),
        0x57 => Ok(Instr::i64_le_s),
        0x58 => Ok(Instr::i64_le_u),
        0x59 => Ok(Instr::i64_ge_s),
        0x5A => Ok(Instr::i64_ge_u),

        0x5B => Ok(Instr::f32_eq),
        0x5C => Ok(Instr::f32_ne),
        0x5D => Ok(Instr::f32_lt),
        0x5E => Ok(Instr::f32_gt),
        0x5F => Ok(Instr::f32_le),
        0x60 => Ok(Instr::f32_ge),

        0x61 => Ok(Instr::f64_eq),
        0x62 => Ok(Instr::f64_ne),
        0x63 => Ok(Instr::f64_lt),
        0x64 => Ok(Instr::f64_gt),
        0x65 => Ok(Instr::f64_le),
        0x66 => Ok(Instr::f64_ge),

        0x67 => Ok(Instr::i32_clz),
        0x68 => Ok(Instr::i32_ctz),
        0x69 => Ok(Instr::i32_popcnt),
        0x6A => Ok(Instr::i32_add),
        0x6B => Ok(Instr::i32_sub),
        0x6C => Ok(Instr::i32_mul),
        0x6D => Ok(Instr::i32_div_u),
        0x6E => Ok(Instr::i32_div_s),
        0x6F => Ok(Instr::i32_rem_u),
        0x70 => Ok(Instr::i32_rem_s),
        0x71 => Ok(Instr::i32_and),
        0x72 => Ok(Instr::i32_or),
        0x73 => Ok(Instr::i32_xor),
        0x74 => Ok(Instr::i32_shl),
        0x75 => Ok(Instr::i32_shr_u),
        0x76 => Ok(Instr::i32_shr_s),
        0x77 => Ok(Instr::i32_rotl),
        0x78 => Ok(Instr::i32_rotr),

        0x79 => Ok(Instr::i64_clz),
        0x7A => Ok(Instr::i64_ctz),
        0x7B => Ok(Instr::i64_popcnt),
        0x7C => Ok(Instr::i64_add),
        0x7D => Ok(Instr::i64_sub),
        0x7E => Ok(Instr::i64_mul),
        0x7F => Ok(Instr::i64_div_u),
        0x80 => Ok(Instr::i64_div_s),
        0x81 => Ok(Instr::i64_rem_u),
        0x82 => Ok(Instr::i64_rem_s),
        0x83 => Ok(Instr::i64_and),
        0x84 => Ok(Instr::i64_or),
        0x85 => Ok(Instr::i64_xor),
        0x86 => Ok(Instr::i64_shl),
        0x87 => Ok(Instr::i64_shr_u),
        0x88 => Ok(Instr::i64_shr_s),
        0x89 => Ok(Instr::i64_rotl),
        0x8A => Ok(Instr::i64_rotr),

        0x8B => Ok(Instr::f32_abs),
        0x8C => Ok(Instr::f32_neg),
        0x8D => Ok(Instr::f32_ceil),
        0x8E => Ok(Instr::f32_floor),
        0x8F => Ok(Instr::f32_trunc),
        0x90 => Ok(Instr::f32_nearest),
        0x91 => Ok(Instr::f32_sqrt),
        0x92 => Ok(Instr::f32_add),
        0x93 => Ok(Instr::f32_sub),
        0x94 => Ok(Instr::f32_mul),
        0x95 => Ok(Instr::f32_div),
        0x96 => Ok(Instr::f32_min),
        0x97 => Ok(Instr::f32_max),
        0x98 => Ok(Instr::f32_copysign),

        0x99 => Ok(Instr::f64_abs),
        0x9A => Ok(Instr::f64_neg),
        0x9B => Ok(Instr::f64_ceil),
        0x9C => Ok(Instr::f64_floor),
        0x9D => Ok(Instr::f64_trunc),
        0x9E => Ok(Instr::f64_nearest),
        0x9F => Ok(Instr::f64_sqrt),
        0xA0 => Ok(Instr::f64_add),
        0xA1 => Ok(Instr::f64_sub),
        0xA2 => Ok(Instr::f64_mul),
        0xA3 => Ok(Instr::f64_div),
        0xA4 => Ok(Instr::f64_min),
        0xA5 => Ok(Instr::f64_max),
        0xA6 => Ok(Instr::f64_copysign),

        0xA7 => Ok(Instr::i32_wrap_i64),
        0xA8 => Ok(Instr::i32_trunc_f32_s),
        0xA9 => Ok(Instr::i32_trunc_f32_u),
        0xAA => Ok(Instr::i32_trunc_f64_s),
        0xAB => Ok(Instr::i32_trunc_f64_u),
        0xAC => Ok(Instr::i64_extend_i32_s),
        0xAD => Ok(Instr::i64_extend_i32_u),
        0xAE => Ok(Instr::i64_trunc_f32_s),
        0xAF => Ok(Instr::i64_trunc_f32_u),
        0xB0 => Ok(Instr::i64_trunc_f64_s),
        0xB1 => Ok(Instr::i64_trunc_f64_u),
        0xB2 => Ok(Instr::f32_convert_i32_s),
        0xB3 => Ok(Instr::f32_convert_i32_u),
        0xB4 => Ok(Instr::f32_convert_i64_s),
        0xB5 => Ok(Instr::f32_convert_i64_u),
        0xB6 => Ok(Instr::f32_demote_f64),
        0xB7 => Ok(Instr::f64_convert_i32_s),
        0xB8 => Ok(Instr::f64_convert_i32_u),
        0xB9 => Ok(Instr::f64_convert_i64_s),
        0xBA => Ok(Instr::f64_convert_i64_u),
        0xBB => Ok(Instr::f64_promote_f32),
        0xBC => Ok(Instr::i32_reinterpret_f32),
        0xBD => Ok(Instr::i64_reinterpret_f64),
        0xBE => Ok(Instr::f32_reinterpret_i32),
        0xBF => Ok(Instr::f64_reinterpret_i64),

        0xC0 => Ok(Instr::i32_extend8_s),
        0xC1 => Ok(Instr::i32_extend16_s),
        0xC2 => Ok(Instr::i64_extend8_s),
        0xC3 => Ok(Instr::i64_extend16_s),
        0xC4 => Ok(Instr::i64_extend32_s),

        0xFC => {
            let value = decode_u32(input)?;
            let bytes = value.to_le_bytes();
            match bytes[0] {
                0 => {
                    let v = decode_u32(input)?;
                    Ok(Instr::i32_trunc_sat_f32_s)
                }
                1 => {
                    let v = decode_u32(input)?;
                    Ok(Instr::i32_trunc_sat_f32_u)
                }
                2 => {
                    let v = decode_u32(input)?;
                    Ok(Instr::i32_trunc_sat_f64_s)
                }
                3 => {
                    let v = decode_u32(input)?;
                    Ok(Instr::i32_trunc_sat_f64_u)
                }
                4 => {
                    let v = decode_u32(input)?;
                    Ok(Instr::i64_trunc_sat_f32_s)
                }
                5 => {
                    let v = decode_u32(input)?;
                    Ok(Instr::i64_trunc_sat_f32_u)
                }
                6 => {
                    let v = decode_u32(input)?;
                    Ok(Instr::i64_trunc_sat_f64_s)
                }
                7 => {
                    let v = decode_u32(input)?;
                    Ok(Instr::i64_trunc_sat_f64_u)
                }
                _ => return Err(Error::Malformed(String::from("ccc"))),
            }
        }
        _ => Err(Error::NotImplemented(byte)),
    }
}
fn decode_memarg(input: &mut Cursor<&[u8]>) -> Result<MemArg, Error> {
    let align = decode_u32(input)?;
    let offset = decode_u32(input)?;
    Ok(MemArg {
        offset: offset,
        align: align,
    })
}
fn decode_i32(input: &mut Cursor<&[u8]>) -> Result<i32, Error> {
    match leb128::read_signed(input, 35) {
        Err(leb128::Error::Overflow) => {
            return Err(Error::Malformed(String::from(
                "integer representation too long",
            )))
        }
        Ok(v) => {
            if v > (i32::MAX as i128) || v < (i32::MIN as i128) {
                return Err(Error::Malformed(String::from("integer too large")));
            }
            Ok(v as i32)
        }
        Err(e) => Err(e.into()),
    }
}
fn decode_i64(input: &mut Cursor<&[u8]>) -> Result<i64, Error> {
    match leb128::read_signed(input, 70) {
        Err(leb128::Error::Overflow) => {
            return Err(Error::Malformed(String::from(
                "integer representation too long",
            )))
        }
        Ok(v) => {
            if v > (i64::MAX as i128) || v < (i64::MIN as i128) {
                return Err(Error::Malformed(String::from("integer too large")));
            }
            Ok(v as i64)
        }
        Err(e) => Err(e.into()),
    }
}
fn decode_f32(input: &mut Cursor<&[u8]>) -> Result<f32, Error> {
    let mut bytes = [0_u8; 4];
    input.read_exact(&mut bytes)?;
    Ok(f32::from_le_bytes(bytes))
}
fn decode_f64(input: &mut Cursor<&[u8]>) -> Result<f64, Error> {
    let mut bytes = [0_u8; 4];
    input.read_exact(&mut bytes)?;
    Ok(f32::from_le_bytes(bytes) as f64)
}
fn decode_export(input: &mut Cursor<&[u8]>) -> Result<Export, Error> {
    let len = input.get_ref().len();
    if input.position() as usize == len {
        return Err(Error::Malformed(String::from(
            "unexpected end of section or function",
        )));
    }
    Ok(Export {
        name: decode_name(input)?,
        desc: decode_exportdesc(input)?,
    })
}
fn decode_exportdesc(input: &mut Cursor<&[u8]>) -> Result<ExportDesc, Error> {
    let byte = decode_byte(input)?;
    match byte {
        0x00 => Ok(ExportDesc::func(decode_funcidx(input)?)),
        0x01 => Ok(ExportDesc::table(decode_tableidx(input)?)),
        0x02 => Ok(ExportDesc::mem(decode_memidx(input)?)),
        0x03 => Ok(ExportDesc::global(decode_globalidx(input)?)),
        _ => Err(Error::DecodeError),
    }
}
fn decode_start(input: &mut Cursor<&[u8]>) -> Result<Start, Error> {
    let len = input.get_ref().len();
    if input.position() as usize == len {
        return Err(Error::Malformed(String::from(
            "unexpected end of section or function",
        )));
    }
    Ok(Start {
        func: decode_funcidx(input)?,
    })
}
fn decode_elem(input: &mut Cursor<&[u8]>, max: u64) -> Result<Elem, Error> {
    let len = input.get_ref().len();
    if input.position() as usize == len {
        return Err(Error::Malformed(String::from("unexpected end")));
    }
    Ok(Elem {
        table: decode_tableidx(input)?,
        offset: decode_expr(input, max)?,
        init: decode_vec::<FuncIdx>(input, max)?,
    })
}
fn decode_code(input: &mut Cursor<&[u8]>, max: u64) -> Result<Code, Error> {
    let len = input.get_ref().len();
    if input.position() as usize == len {
        return Err(Error::Malformed(String::from(
            "unexpected end of section or function",
        )));
    }

    let size = leb128::read_signed(input, 32)? as u32;
    let mut buf = vec![0; size as usize];
    input.read_exact(&mut buf)?;
    let mut cont = Cursor::new(&buf[..]);
    Ok(Code {
        size: size,
        code: decode_func(&mut cont, max)?,
    })
}
fn decode_func(input: &mut Cursor<&[u8]>, max: u64) -> Result<BfFunc, Error> {
    let len = input.get_ref().len();
    if input.position() as usize == len {
        return Err(Error::Malformed(String::from(
            "unexpected end of section or function",
        )));
    }
    Ok(BfFunc {
        locals: decode_vec::<Locals>(input, max)?,
        e: decode_expr(input, max)?,
    })
}
fn decode_locals(input: &mut Cursor<&[u8]>) -> Result<Locals, Error> {
    let n = leb128::read_unsigned(input, 32)?;
    if n >= u32::MAX as u64 {
        return Err(Error::Malformed(String::from("too many locals")));
    }
    Ok(Locals {
        n: n as u32,
        t: decode_valtype(input)?,
    })
}
fn decode_data(input: &mut Cursor<&[u8]>, max: u64) -> Result<Data, Error> {
    let len = input.get_ref().len();
    if input.position() as usize == len {
        return Err(Error::Malformed(String::from(
            "unexpected end of section or function",
        )));
    }
    Ok(Data {
        data: decode_memidx(input)?,
        offset: decode_expr(input, max)?,
        init: {
            let n = leb128::read_unsigned(input, 32)? as u32;
            let mut bytes = vec![0; n as usize];
            match input.read_exact(&mut bytes) {
                Ok(_) => {}
                _ => {
                    return Err(Error::Malformed(String::from(
                        "unexpected end of section or function",
                    )))
                }
            }
            bytes
        },
    })
}
#[derive(Debug)]
struct Code {
    size: u32,
    code: BfFunc,
}
#[derive(Debug)]
struct BfFunc {
    locals: Vec<Locals>,
    e: Expr,
}
#[derive(Debug)]
struct Locals {
    n: u32,
    t: ValType,
}

trait Vectorable {
    fn decode(input: &mut Cursor<&[u8]>, max: u64) -> Result<Self, Error>
    where
        Self: std::marker::Sized;
}
impl Vectorable for FuncType {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_functype(input, max)
    }
}
impl Vectorable for ValType {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        let byte = decode_byte(input)?;
        match byte {
            0x7F => Ok(Self::i32),
            0x7E => Ok(Self::i64),
            0x7D => Ok(Self::f32),
            0x7C => Ok(Self::f64),
            _ => Err(Error::DecodeError),
        }
    }
}
impl Vectorable for Import {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_import(input)
    }
}
impl Vectorable for u32 {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_u32(input)
    }
}
impl Vectorable for Table {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_table(input)
    }
}
impl Vectorable for Mem {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_mem(input)
    }
}
impl Vectorable for Global {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_global(input, max)
    }
}
impl Vectorable for Export {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_export(input)
    }
}
impl Vectorable for Start {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_start(input)
    }
}
impl Vectorable for Elem {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_elem(input, max)
    }
}
impl Vectorable for Code {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_code(input, max)
    }
}
impl Vectorable for Locals {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_locals(input)
    }
}
impl Vectorable for Data {
    fn decode(input: &mut std::io::Cursor<&[u8]>, max: u64) -> std::result::Result<Self, Error> {
        decode_data(input, max)
    }
}
