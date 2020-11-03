#[derive(Debug)]
pub enum Error {
    Overflow,
    IoError(std::io::Error),
}
impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::IoError(e)
    }
}

pub fn read_unsigned<R>(r: &mut R, len: i8) -> Result<u64, Error>
where
    R: std::io::Read,
{
    if len <= 0 {
        return Err(Error::Overflow);
    }
    let N = len;
    let mut bytes: [u8; 1] = Default::default();
    r.read(&mut bytes)?;
    let n = bytes[0];
    if n < 128 && (n as u64) < (1 << N) {
        return Ok(n as u64);
    }
    if n >= 128 && N > 7 {
        let m = read_unsigned(r, N - 7)?;
        Ok(128 * m + (n - 128) as u64)
    } else {
        Err(Error::Overflow)
    }
}

pub fn read_signed<R>(r: &mut R, len: i8) -> Result<i64, Error>
where
    R: std::io::Read,
{
    if len <= 0 {
        return Err(Error::Overflow);
    }
    let N = len;
    let mut bytes: [u8; 1] = Default::default();
    r.read(&mut bytes)?;
    let n = bytes[0];
    if n < 64 && (n as i64) < (1 << (N - 1)) {
        return Ok(n as i64);
    }
    if n >= 64 && n < 128 && (n as i64) >= 128 - (1 << (N - 1)) {
        return Ok(n as i64 - 128);
    }
    if n >= 128 && N > 7 {
        let m = read_signed(r, N - 7)?;
        Ok(128 * m + n as i64 - 128)
    } else {
        Err(Error::Overflow)
    }
}
