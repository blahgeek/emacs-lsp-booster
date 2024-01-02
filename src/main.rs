use anyhow::{Result, bail};

use emacs_lsp_booster::app;


fn main() -> Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        bail!("Usage: {} LSP_SERVER_CMD ARGS...", args[0]);
    }

    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "info");
    }
    env_logger::init();

    // exit the process if any thread panic
    let original_panic_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        original_panic_hook(info);
        std::process::exit(1);
    }));

    let mut cmd = std::process::Command::new(&args[1]);
    cmd.args(&args[2..]);

    app::run_app_forever(std::io::stdin(), std::io::stdout(), cmd)
}
