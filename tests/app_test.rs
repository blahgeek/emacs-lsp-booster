use anyhow::Result;
use tempfile;
use env_logger;

use emacs_lsp_booster::{app, rpcio};

#[test]
fn test_app_with_echo_server() -> Result<()> {
    env_logger::init();

    let (input_pair_in, input_pair_out) = std::os::unix::net::UnixStream::pair()?;
    let tmpdir = tempfile::tempdir()?;
    let output_file = std::fs::File::create(tmpdir.path().join("output.txt"))?;

    std::thread::spawn(move || {
        let mut input_bufwriter = std::io::BufWriter::new(input_pair_in);
        for _ in 0..10 {
            rpcio::rpc_write(&mut input_bufwriter, "{}").unwrap();
        }
        loop {
            std::thread::park();
        }
    });

    let mut cmd = std::process::Command::new("timeout");
    cmd.args(&["1", "cat"]);

    let exit_status = app::run_app_forever(input_pair_out, output_file, cmd, app::AppOptions {
        bytecode_options: Default::default(),
    })?;
    assert!(!exit_status.success());  // timeout kill

    let output = std::fs::read_to_string(tmpdir.path().join("output.txt"))?;
    assert_eq!(output.chars().filter(|x| *x == '#').count(), 10);

    Ok(())
}
