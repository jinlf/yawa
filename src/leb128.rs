#[derive(Debug)]
pub enum Error {
    Overflow,
    IoError(std::io::Error),
    EOFError,
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
    if r.read_exact(&mut bytes).is_err() {
        return Err(Error::EOFError);
    }
    let mut n = bytes[0] as u64;
    if n < 128 && n < (1_u64 << N) {
        return Ok(n);
    }
    if n >= 128 && N > 7 {
        let m = read_unsigned(r, N - 7)?;
        n = 128 * m + (n - 128);
        Ok(n)
    } else {
        Err(Error::Overflow)
    }
}

pub fn read_signed<R>(r: &mut R, len: i8) -> Result<i128, Error>
where
    R: std::io::Read,
{
    if len <= 0 {
        return Err(Error::Overflow);
    }
    let N = len;
    let mut bytes: [u8; 1] = Default::default();
    r.read_exact(&mut bytes)?;
    let n = bytes[0] as i128;
    if n < 64 && n < (1_i128 << (N - 1)) {
        return Ok(n);
    }
    if 64 <= n && n < 128 && n >= (128 - (1 << (N - 1))) {
        return Ok(n - 128);
    }
    if n >= 128 && N > 7 {
        let m = read_signed(r, N - 7)?;
        Ok(128 * m + (n - 128))
    } else {
        Err(Error::Overflow)
    }
}

#[cfg(test)]
mod tests {
    use leb128;
    // #[test]
    fn my_test1() {
        let mut buf = [0; 1024];
        buf[0] = 0x80;
        buf[1] = 0x80;
        buf[2] = 0x80;
        buf[3] = 0x80;
        buf[4] = 0x80;
        buf[5] = 0x80;
        buf[6] = 0x80;
        buf[7] = 0x80;
        buf[8] = 0x80;
        buf[9] = 0x7e;
        // Write to anything that implements `std::io::Write`.
        // {
        //     let mut writable = &mut buf[..];
        //     leb128::write::signed(&mut writable, -12345).expect("Should write number");
        // }
        // Read from anything that implements `std::io::Read`.
        let mut readable = &buf[..];
        let val = leb128::read::signed(&mut readable).expect("Should read number");
        assert_eq!(val, 30064771074);
    }
}
