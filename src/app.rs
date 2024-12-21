use std::{fmt::Debug, net::{TcpListener, TcpStream, ToSocketAddrs}, sync::{atomic::{self, AtomicI32}, mpsc, Arc}};

use log::{warn, info, debug};
use anyhow::{Result, Context};
use serde_json as json;

use crate::{bytecode::{self, BytecodeOptions}, rpcio};
use crate::lsp_message::{LspRequest, LspResponse, LspResponseError};

fn process_channel_to_writer(channel_sub: mpsc::Receiver<String>,
                             channel_counter: Option<Arc<AtomicI32>>,
                             writer: impl std::io::Write) -> Result<()> {
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
fn process_client_reader(reader: impl std::io::Read,
                         server_channel_pub: mpsc::Sender<String>,
                         server_channel_counter: Arc<AtomicI32>,
                         client_channel_pub: mpsc::Sender<String>) -> Result<()> {
    let mut bufreader = std::io::BufReader::new(reader);
    loop {
        let msg = rpcio::rpc_read(&mut bufreader)?;
        if msg.is_empty() {
            break
        }

        if server_channel_counter.load(atomic::Ordering::Acquire) >= MAX_PENDING_MSG_COUNT {
            let lsp_request: LspRequest = json::from_str(&msg)?;
            // only cancel when it's not notification
            if !lsp_request.is_notification() {
                warn!("Buffer full, rejecting request: {} (id={:?})",
                      lsp_request.method, lsp_request.id);
                let resp = LspResponse {
                    jsonrpc: lsp_request.jsonrpc,
                    id: lsp_request.id.unwrap(),
                    result: json::Value::Null,
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

fn process_server_reader(reader: impl std::io::Read,
                         channel_pub: mpsc::Sender<String>,
                         bytecode_options: Option<BytecodeOptions>) -> Result<()> {
    let mut bufreader = std::io::BufReader::new(reader);
    loop {
        let msg = rpcio::rpc_read(&mut bufreader)?;
        if msg.is_empty() {
            break
        }
        if let Some(ref bytecode_options) = bytecode_options {
            let json_val = json::from_str(&msg)?;
            match bytecode::generate_bytecode_repl(&json_val, bytecode_options.clone()) {
                Ok(bytecode_str) => {
                    debug!("server->client: json {} bytes; converted to bytecode, {} bytes",
                           msg.len(), bytecode_str.len());
                    channel_pub.send(bytecode_str)?;
                    continue
                },
                Err(err) => {
                    warn!("Failed to convert json to bytecode: {}", err);
                },
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

// Return the receiver which can be used get notifications about thread termination
fn run_app(client_reader: impl std::io::Read + Send + 'static,
           client_writer: impl std::io::Write + Send + 'static,
           server_reader: impl std::io::Read + Send + 'static,
           server_writer: impl std::io::Write + Send + 'static,
           options: AppOptions) -> Result<mpsc::Receiver<()>> {
    if let Some(ref bytecode_options) = options.bytecode_options {
        info!("Will convert server json to bytecode! bytecode options: {:?}", bytecode_options);
    } else {
        info!("Bytecode disabled! Will forward server json as-is.")
    }

    let (finish_sender, finish_receiver) = mpsc::channel::<()>();

    let (c2s_channel_pub, c2s_channel_sub) = mpsc::channel::<String>();
    let c2s_channel_counter = Arc::new(AtomicI32::new(0));
    let (s2c_channel_pub, s2c_channel_sub) = mpsc::channel::<String>();

    {
        let finish_sender = finish_sender.clone();
        let c2s_channel_counter = c2s_channel_counter.clone();
        std::thread::spawn(move || {
            debug!("Started client->server write thread");
            process_channel_to_writer(c2s_channel_sub, Some(c2s_channel_counter), server_writer)
                .with_context(|| "Client->server write thread failed")
                .unwrap();
            debug!("Finished client->server write thread");
            let _ = finish_sender.send(());  // ignore error
        });
    }
    {
        let finish_sender = finish_sender.clone();
        std::thread::spawn(move || {
            debug!("Started server->client write thread");
            process_channel_to_writer(s2c_channel_sub, None, client_writer)
                .with_context(|| "Server->client write thread failed")
                .unwrap();
            debug!("Finished server->client write thread");
            let _ = finish_sender.send(());  // ignore error
        });
    }
    {
        let finish_sender = finish_sender.clone();
        let s2c_channel_pub = s2c_channel_pub.clone();
        std::thread::spawn(move || {
            debug!("Started server->client read thread");
            process_server_reader(server_reader, s2c_channel_pub, options.bytecode_options)
                .with_context(|| "Server->client read thread failed")
                .unwrap();
            debug!("Finished server->client read thread");
            let _ = finish_sender.send(());  // ignore error
        });
    }
    std::thread::spawn(move || {
        debug!("Started client->server read thread");
        process_client_reader(
            client_reader, c2s_channel_pub, c2s_channel_counter, s2c_channel_pub)
            .with_context(|| "Client->server read thread failed")
            .unwrap();
        debug!("Finished client->server read thread");
        let _ = finish_sender.send(());  // ignore error
    });

    Ok(finish_receiver)
}

pub fn run_app_stdio(mut server_cmd: std::process::Command,
                     options: AppOptions) -> Result<std::process::ExitStatus> {
    info!("Running server {:?}", server_cmd);
    let mut proc = server_cmd
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::inherit())
        .spawn()?;

    run_app(std::io::stdin(), std::io::stdout(),
            proc.stdout.take().unwrap(), proc.stdin.take().unwrap(),
            options)?;
    Ok(proc.wait()?)
}

pub fn run_app_tcp(server_addr: impl ToSocketAddrs + Debug,
                   listen_addr: impl ToSocketAddrs + Debug,
                   options: AppOptions) -> Result<()> {
    info!("Connecting to server at {:?}", server_addr);
    let server_conn = TcpStream::connect(server_addr)?;

    info!("Listenting at {:?}", listen_addr);
    let client_listener = TcpListener::bind(listen_addr)?;
    // NOTE: only accept single client for now. Is it enough?
    let (client_conn, _) = client_listener.accept()?;
    info!("Client connected, start running");

    let finish_receiver = run_app(client_conn.try_clone()?,
                                  client_conn.try_clone()?,
                                  server_conn.try_clone()?,
                                  server_conn.try_clone()?,
                                  options)?;
    let _ = finish_receiver.recv();  // wait for finish, ignore error
    Ok(())
}
