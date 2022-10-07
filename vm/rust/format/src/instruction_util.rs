use std::io::{Write, Result, Read};


pub fn write_index<F: Write>(f: &mut F, mut value: usize) -> Result<()> {
    loop {
        let next_part = (value & 0x7F) as u8;
        value = value >> 7;
        if value > 0 {
            f.write(std::slice::from_ref(&(next_part | 0x80)))?;
        }
        else {
            f.write(std::slice::from_ref(&next_part))?;
            return Ok(());
        }

    }
}



pub fn read_index<F: Read>(f: &mut F) -> Result<usize> {
    let mut result: usize = 0;

    loop {
        let mut b: u8 = 0;
        f.read_exact(std::slice::from_mut(&mut b))?;

        result = (result << 7) | (b & 0x7F) as usize;
        if (b & 0x80) == 0 {
            break;
        }
    }

    Ok(result)
}

pub fn write_int8<F: Write>(f: &mut F, value: i8) -> Result<()> {
    let b = value as u8;
    f.write(std::slice::from_ref(&b))?;
    Ok(())
}

pub fn read_int8<F: Read>(f: &mut F) -> Result<i8> {
    let mut b: u8 = 0;
    f.read_exact(std::slice::from_mut(&mut b))?;
    Ok(b as i8)
}
