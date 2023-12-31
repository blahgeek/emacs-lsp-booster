use anyhow::Result;

use serde_json as json;
use crate::{rpcio, bytecode};

fn run_channel_to_writer(channel_sub: std::sync::mpsc::Receiver<String>,
                         writer: impl std::io::Write) -> Result<()> {
    let mut bufwriter = std::io::BufWriter::new(writer);
    for msg in channel_sub.iter() {
        rpcio::rpc_write(&mut bufwriter, &msg)?;
    }
    Ok(())
}

fn run_reader_to_channel(reader: impl std::io::Read,
                         channel_pub: std::sync::mpsc::Sender<String>) -> Result<()> {
    let mut bufreader = std::io::BufReader::new(reader);
    loop {
        let msg = rpcio::rpc_read(&mut bufreader)?;
        if msg.is_empty() {
            break
        }
        channel_pub.send(msg)?;
    }
    Ok(())
}

fn run_read_to_bytecode_to_channel(reader: impl std::io::Read,
                                   channel_pub: std::sync::mpsc::Sender<String>) -> Result<()> {
    let mut bufreader = std::io::BufReader::new(reader);
    loop {
        let msg = rpcio::rpc_read(&mut bufreader)?;
        if msg.is_empty() {
            break
        }
        let json_val = json::from_str(&msg)?;
        let bytecode_str = bytecode::generate_bytecode_repl(&json_val)?;
        channel_pub.send(bytecode_str)?;
    }
    Ok(())
}

pub fn run_app_forever(client_reader: impl std::io::Read + Send + 'static,
                       client_writer: impl std::io::Write + Send + 'static,
                       mut server_cmd: std::process::Command) -> Result<()> {
    let proc = server_cmd
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::inherit())
        .spawn()?;

    let (c2s_channel_pub, c2s_channel_sub) = std::sync::mpsc::channel::<String>();
    let (s2c_channel_pub, s2c_channel_sub) = std::sync::mpsc::channel::<String>();

    let threads = vec![
        std::thread::spawn(move || {
            run_channel_to_writer(c2s_channel_sub, proc.stdin.unwrap()).unwrap()
        }),
        std::thread::spawn(move || {
            run_channel_to_writer(s2c_channel_sub, client_writer).unwrap()
        }),
        std::thread::spawn(move || {
            run_read_to_bytecode_to_channel(proc.stdout.unwrap(), s2c_channel_pub).unwrap()
        }),
        std::thread::spawn(move || {
            run_reader_to_channel(client_reader, c2s_channel_pub).unwrap()
        }),
    ];

    threads.into_iter().for_each(|x| x.join().unwrap());

    Ok(())
}
