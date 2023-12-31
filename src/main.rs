use anyhow::{Result, bail};

use emacs_lsp_booster::app;


fn main() -> Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        bail!("Usage: {} LSP_SERVER_CMD ARGS...", args[0]);
    }

    let mut cmd = std::process::Command::new(&args[1]);
    cmd.args(&args[1..]);

    app::run_app_forever(std::io::stdin(), std::io::stdout(), cmd)
}
