use std::{str::FromStr, io::Write};

use log::trace;
use anyhow::{Result, bail};

// return empty string on EOF
pub fn rpc_read(reader: &mut impl std::io::BufRead) -> Result<String> {
    let mut content_len: Option<usize> = None;

    loop {
        let mut line = String::new();
        reader.read_line(&mut line)?;
        if line.is_empty() {
            return Ok(String::new());
        }
        if line == "\r\n" {
            if let Some(content_len) = content_len {
                let mut result: Vec<u8> = vec![0; content_len];
                reader.read_exact(&mut result)?;
                return Ok(String::from_utf8(result)?);
            }
        }
        let splitted: Vec<&str> = line.trim().splitn(2, ": ").collect();
        if splitted.len() != 2 {
            bail!("Invalid header format");
        }
        trace!("Header: {:?}", splitted);
        if splitted[0] == "Content-Length" {
            content_len = Some(usize::from_str(splitted[1])?);
        }
    }
}

pub fn rpc_write<T>(writer: &mut std::io::BufWriter<T>, content: &str) -> Result<()>
where T: std::io::Write {
    write!(writer, "Content-Length: {}\r\n", content.len())?;
    write!(writer, "\r\n")?;
    writer.write_all(content.as_bytes())?;
    writer.flush()?;
    Ok(())
}
