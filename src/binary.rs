use crate::leb128;
use crate::structure::*;
use std::io::Cursor;
use std::io::Read;

#[derive(Debug)]
pub enum Error {
    DecodeError,
    IoError(std::io::Error),
    Leb128Error(leb128::Error),
    Utf8Error(std::string::FromUtf8Error),
}
impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::IoError(e)
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
fn decode_module(input: &mut Cursor<&[u8]>) -> Result<Module, Error> {
    decode_magic(input)?;
    decode_version(input)?;
    let functypes = decode_typesec(input)?;
    let imports = decode_importsec(input)?;
    let typeidxes = decode_funcsec(input)?;
    let tables = decode_tablesec(input)?;
    let mems = decode_memsec(input)?;
    let globals = decode_globalsec(input)?;
    let exports = decode_exportsec(input)?;
    let start = decode_startsec(input)?;
    let elem = decode_elemsec(input)?;
    let code = decode_codesec(input)?;
    let data = decode_datasec(input)?;
    let mut funcs: Vec<Func> = vec![];
    for (i, c) in code.iter().enumerate() {
        funcs.push(Func {
            r#type: typeidxes[i],
            locals: (&c.code).locals.iter().map(|x| x.t.clone()).collect(),
            body: c.code.e.clone(),
        })
    }

    Ok(Module {
        types: functypes,
        funcs: funcs,
        tables: tables,
        mems: mems,
        globals: globals,
        elem: elem,
        data: data,
        start: start,
        imports: imports,
        exports: exports,
    })
}
fn decode_magic(input: &mut Cursor<&[u8]>) -> Result<[u8; 4], Error> {
    let mut magic: [u8; 4] = Default::default();
    input.read_exact(&mut magic)?;
    match magic {
        [0x00, 0x61, 0x73, 0x6D] => Ok(magic),
        _ => Err(Error::DecodeError),
    }
}
fn decode_version(input: &mut Cursor<&[u8]>) -> Result<[u8; 4], Error> {
    let mut version: [u8; 4] = Default::default();
    input.read_exact(&mut version)?;
    match version {
        [0x01, 0x00, 0x00, 0x00] => Ok(version),
        _ => Err(Error::DecodeError),
    }
}
fn decode_sec<T>(input: &mut Cursor<&[u8]>, id: u32) -> Result<Vec<T>, Error>
where
    T: Vectorable,
{
    let pos = input.position();
    let section_id = leb128::read_unsigned(input, 32)? as u32;
    if section_id != id {
        input.set_position(pos);
        return Ok(vec![]);
    }
    let size = leb128::read_unsigned(input, 32)? as u32;
    let mut buf = vec![0_u8; size as usize];
    input.read_exact(&mut buf)?;
    let mut cont = Cursor::new(&buf[..]);
    decode_vec::<T>(&mut cont)
}
fn decode_vec<T>(input: &mut Cursor<&[u8]>) -> Result<Vec<T>, Error>
where
    T: Vectorable,
{
    let n = leb128::read_unsigned(input, 32)? as u32;
    let mut xn: Vec<T> = vec![];
    for _ in 0..n {
        xn.push(T::decode(input)?)
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
    let pos = input.position();
    let section_id = leb128::read_unsigned(input, 32)? as u32;
    if section_id != 0x08 {
        input.set_position(pos);
        return Ok(None);
    }
    let size = leb128::read_unsigned(input, 32)? as u32;
    let mut buf = vec![0_u8; size as usize];
    input.read_exact(&mut buf)?;
    let mut cont = Cursor::new(&buf[..]);
    let start = decode_start(&mut cont)?;
    Ok(Some(start))
}
fn decode_elemsec(input: &mut Cursor<&[u8]>) -> Result<Vec<Elem>, Error> {
    decode_sec::<Elem>(input, 0x09)
}
fn decode_codesec(input: &mut Cursor<&[u8]>) -> Result<Vec<Code>, Error> {
    decode_sec::<Code>(input, 0x0A)
}
fn decode_datasec(input: &mut Cursor<&[u8]>) -> Result<Vec<Data>, Error> {
    decode_sec::<Data>(input, 0x0B)
}
fn decode_functype(input: &mut Cursor<&[u8]>) -> Result<FuncType, Error> {
    expect_byte(input, 0x60)?;
    Ok(FuncType {
        params: decode_resulttype(input)?,
        results: decode_resulttype(input)?,
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
fn decode_resulttype(input: &mut Cursor<&[u8]>) -> Result<ResultType, Error> {
    decode_vec::<ValType>(input)
}
fn decode_import(input: &mut Cursor<&[u8]>) -> Result<Import, Error> {
    Ok(Import {
        module: decode_name(input)?,
        name: decode_name(input)?,
        desc: decode_importdesc(input)?,
    })
}
fn decode_name(input: &mut Cursor<&[u8]>) -> Result<Name, Error> {
    let n = leb128::read_unsigned(input, 32)? as u32;
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
        _ => Err(Error::DecodeError),
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
    Ok(leb128::read_unsigned(input, 32)? as u32)
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
        _ => Err(Error::DecodeError),
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
    Ok(Table {
        r#type: decode_tabletype(input)?,
    })
}
fn decode_mem(input: &mut Cursor<&[u8]>) -> Result<Mem, Error> {
    Ok(Mem {
        r#type: decode_memtype(input)?,
    })
}
fn decode_global(input: &mut Cursor<&[u8]>) -> Result<Global, Error> {
    Ok(Global {
        r#type: decode_globaltype(input)?,
        init: decode_expr(input)?,
    })
}
fn decode_expr(input: &mut Cursor<&[u8]>) -> Result<Expr, Error> {
    let mut expr = Expr::new();
    loop {
        let pos = input.position();
        let byte = decode_byte(input)?;
        if byte == 0x0B {
            return Ok(expr);
        }
        input.set_position(pos);
        expr.instrs.push(decode_instr(input)?)
    }
}
fn decode_instr(input: &mut Cursor<&[u8]>) -> Result<Instr, Error> {
    let byte = decode_byte(input)?;
    match byte {
        //5.4.1 Control Instructions
        0x10 => Ok(Instr::call(decode_funcidx(input)?)),
        //5.4.2 Parametric Instructions
        0x1A => Ok(Instr::drop),
        //5.4.3 Variable Instructions
        0x20 => Ok(Instr::local_get(decode_localidx(input)?)),
        //5.4.5 Numeric Instructions
        0x41 => Ok(Instr::i32_const(decode_i32(input)?)),
        _ => todo!(),
    }
}
fn decode_i32(input: &mut Cursor<&[u8]>) -> Result<i32, Error> {
    Ok(leb128::read_signed(input, 32)? as i32)
}
fn decode_export(input: &mut Cursor<&[u8]>) -> Result<Export, Error> {
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
    Ok(Start {
        func: decode_funcidx(input)?,
    })
}
fn decode_elem(input: &mut Cursor<&[u8]>) -> Result<Elem, Error> {
    Ok(Elem {
        table: decode_tableidx(input)?,
        offset: decode_expr(input)?,
        init: decode_vec::<FuncIdx>(input)?,
    })
}
fn decode_code(input: &mut Cursor<&[u8]>) -> Result<Code, Error> {
    let size = leb128::read_signed(input, 32)? as u32;
    let mut buf = vec![0; size as usize];
    input.read_exact(&mut buf)?;
    let mut cont = Cursor::new(&buf[..]);
    Ok(Code {
        size: size,
        code: decode_func(&mut cont)?,
    })
}
fn decode_func(input: &mut Cursor<&[u8]>) -> Result<BfFunc, Error> {
    Ok(BfFunc {
        locals: decode_vec::<Locals>(input)?,
        e: decode_expr(input)?,
    })
}
fn decode_locals(input: &mut Cursor<&[u8]>) -> Result<Locals, Error> {
    Ok(Locals {
        n: leb128::read_unsigned(input, 32)? as u32,
        t: decode_valtype(input)?,
    })
}
fn decode_data(input: &mut Cursor<&[u8]>) -> Result<Data, Error> {
    Ok(Data {
        data: decode_memidx(input)?,
        offset: decode_expr(input)?,
        init: {
            let n = leb128::read_unsigned(input, 32)? as u32;
            let mut bytes = vec![0; n as usize];
            input.read_exact(&mut bytes)?;
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
    fn decode(input: &mut Cursor<&[u8]>) -> Result<Self, Error>
    where
        Self: std::marker::Sized;
}
impl Vectorable for FuncType {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_functype(input)
    }
}
impl Vectorable for ValType {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
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
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_import(input)
    }
}
impl Vectorable for u32 {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_u32(input)
    }
}
impl Vectorable for Table {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_table(input)
    }
}
impl Vectorable for Mem {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_mem(input)
    }
}
impl Vectorable for Global {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_global(input)
    }
}
impl Vectorable for Export {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_export(input)
    }
}
impl Vectorable for Start {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_start(input)
    }
}
impl Vectorable for Elem {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_elem(input)
    }
}
impl Vectorable for Code {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_code(input)
    }
}
impl Vectorable for Locals {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_locals(input)
    }
}
impl Vectorable for Data {
    fn decode(input: &mut std::io::Cursor<&[u8]>) -> std::result::Result<Self, Error> {
        decode_data(input)
    }
}
