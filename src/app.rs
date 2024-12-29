use std::sync::{
    atomic::{self, AtomicI32},
    mpsc, Arc,
};

use anyhow::{Context, Result};
use log::{debug, info, warn};
use sonic_rs as json;

use crate::lsp_message::{LspRequest, LspResponse, LspResponseError};
use crate::{
    bytecode::{self, BytecodeOptions},
    rpcio,
};

fn process_channel_to_writer(
    channel_sub: mpsc::Receiver<String>,
    channel_counter: Option<Arc<AtomicI32>>,
    writer: impl std::io::Write,
) -> Result<()> {
    let mut bufwriter = std::io::BufWriter::new(writer);
    for msg in channel_sub.iter() {
        if let Some(ref channel_counter) = channel_counter {
            channel_counter.fetch_add(-1, atomic::Ordering::AcqRel);
        }
        rpcio::rpc_write(&mut bufwriter, &msg)?;
    }
    Ok(())
}

const MAX_PENDING_MSG_COUNT: i32 = 128;

// Read from client, write to server.
// Or, if server is blocked, reject the request and fake reply when possible
fn process_client_reader(
    reader: impl std::io::Read,
    server_channel_pub: mpsc::Sender<String>,
    server_channel_counter: Arc<AtomicI32>,
    client_channel_pub: mpsc::Sender<String>,
) -> Result<()> {
    let mut bufreader = std::io::BufReader::new(reader);
    loop {
        let msg = rpcio::rpc_read(&mut bufreader)?;
        if msg.is_empty() {
            break;
        }

        if server_channel_counter.load(atomic::Ordering::Acquire) >= MAX_PENDING_MSG_COUNT {
            let lsp_request: LspRequest = json::from_str(&msg)?;
            // only cancel when it's not notification
            if !lsp_request.is_notification() {
                warn!(
                    "Buffer full, rejecting request: {} (id={:?})",
                    lsp_request.method, lsp_request.id
                );
                let resp = LspResponse {
                    jsonrpc: lsp_request.jsonrpc,
                    id: lsp_request.id.unwrap(),
                    result: json::Value::new(),
                    error: Some(LspResponseError {
                        code: -32803,
                        message: "[emacs-lsp-booster] Server is busy".to_string(),
                    }),
                };
                client_channel_pub.send(json::to_string(&resp)?)?;
                continue;
            }
        }

        server_channel_pub.send(msg)?;
        server_channel_counter.fetch_add(1, atomic::Ordering::AcqRel);
    }

    Ok(())
}

fn process_server_reader(
    reader: impl std::io::Read,
    channel_pub: mpsc::Sender<String>,
    bytecode_options: Option<BytecodeOptions>,
) -> Result<()> {
    let mut bufreader = std::io::BufReader::new(reader);
    loop {
        let msg = rpcio::rpc_read(&mut bufreader)?;
        if msg.is_empty() {
            break;
        }
        if let Some(ref bytecode_options) = bytecode_options {
            let json_val = json::from_str(&msg)?;
            match bytecode::generate_bytecode_repl(&json_val, bytecode_options.clone()) {
                Ok(bytecode_str) => {
                    debug!(
                        "server->client: json {} bytes; converted to bytecode, {} bytes",
                        msg.len(),
                        bytecode_str.len()
                    );
                    channel_pub.send(bytecode_str)?;
                    continue;
                }
                Err(err) => {
                    warn!("Failed to convert json to bytecode: {}", err);
                }
            }
        }
        debug!("server->client: json {} bytes; forward as-is", msg.len());
        channel_pub.send(msg)?;
    }
    Ok(())
}

pub struct AppOptions {
    // if bytecode_options is None, then don't generate bytecode!
    pub bytecode_options: Option<bytecode::BytecodeOptions>,
}

pub fn run_app_forever(client_reader: impl std::io::Read + Send + 'static,
                       client_writer: impl std::io::Write + Send + 'static,
                       mut server_cmd: std::process::Command,
                       options: AppOptions) -> Result<std::process::ExitStatus> {
    info!("About to run the lsp server with command {:?}", server_cmd);
    if let Some(ref bytecode_options) = options.bytecode_options {
        info!(
            "Will convert server json to bytecode! bytecode options: {:?}",
            bytecode_options
        );
    } else {
        info!("Bytecode disabled! Will forward server json as-is.")
    }

    let mut proc = server_cmd
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::inherit())
        .spawn()
        .with_context(|| {
            format!(
                "Failed to run the lsp server with command: {:?}",
                server_cmd
            )
        })?;

    let (c2s_channel_pub, c2s_channel_sub) = mpsc::channel::<String>();
    let c2s_channel_counter = Arc::new(AtomicI32::new(0));
    let (s2c_channel_pub, s2c_channel_sub) = mpsc::channel::<String>();

    {
        let c2s_channel_counter = c2s_channel_counter.clone();
        let proc_stdin = proc.stdin.take().unwrap();
        std::thread::spawn(move || {
            debug!("Started client->server write thread");
            process_channel_to_writer(c2s_channel_sub, Some(c2s_channel_counter), proc_stdin)
                .with_context(|| "Client->server write thread failed")
                .unwrap();
            debug!("Finished client->server write thread");
        });
    }
    std::thread::spawn(move || {
        debug!("Started server->client write thread");
        process_channel_to_writer(s2c_channel_sub, None, client_writer)
            .with_context(|| "Server->client write thread failed")
            .unwrap();
        debug!("Finished server->client write thread");
    });
    {
        let s2c_channel_pub = s2c_channel_pub.clone();
        let proc_stdout = proc.stdout.take().unwrap();
        std::thread::spawn(move || {
            debug!("Started server->client read thread");
            process_server_reader(proc_stdout, s2c_channel_pub, options.bytecode_options)
                .with_context(|| "Server->client read thread failed")
                .unwrap();
            debug!("Finished server->client read thread");
        });
    }
    std::thread::spawn(move || {
        debug!("Started client->server read thread");
        process_client_reader(
            client_reader,
            c2s_channel_pub,
            c2s_channel_counter,
            s2c_channel_pub,
        )
        .with_context(|| "Client->server read thread failed")
        .unwrap();
        debug!("Finished client->server read thread");
    });

    Ok(proc.wait()?)
}
