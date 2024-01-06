use anyhow::{Result, bail};
use clap::Parser;

use emacs_lsp_booster::app;


#[derive(Parser, Default)]
#[command(long_about = None, about = None,
          arg_required_else_help = true, after_help = "For backward compatibility, `emacs-lsp-booster <SERVER_CMD>...` (without any options) is also supported\n" )]
struct Cli {
    #[command(flatten)]
    verbose: clap_verbosity_flag::Verbosity,

    #[arg(last = true)]
    server_cmd: Vec<String>,
}

fn parse_args<T, S>(args: T) -> Cli
where T: IntoIterator<Item=S>,
      S: Into<String> {
    let args = args.into_iter().map(|x| x.into()).collect::<Vec<String>>();
    // backward compatible. support `emacs-lsp-booster server_cmd args...` directly
    if args.len() > 1 && !args[1].starts_with('-') && !args.contains(&"--".into()) {
        Cli {
            server_cmd: args[1..].to_vec(),
            ..Default::default()
        }
    } else {
        Cli::parse_from(args)
    }
}

fn main() -> Result<()> {
    let cli = parse_args(std::env::args());
    env_logger::Builder::new().filter_level(cli.verbose.log_level_filter()).init();

    if cli.server_cmd.is_empty() {
        bail!("Please specify the server command");
    }

    // exit the process if any thread panic
    let original_panic_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        original_panic_hook(info);
        std::process::exit(1);
    }));

    let mut cmd = std::process::Command::new(&cli.server_cmd[0]);
    cmd.args(&cli.server_cmd[1..]);

    let exit_status = app::run_app_forever(std::io::stdin(), std::io::stdout(), cmd)?;
    std::process::exit(exit_status.code().unwrap_or(1))
}

#[test]
fn test_parse_args() {
    let cli = parse_args(vec!["emacs-lsp-booster", "server_cmd", "arg1"]);
    assert_eq!(cli.server_cmd, vec!["server_cmd", "arg1"]);

    let cli = parse_args(vec!["emacs-lsp-booster", "--", "server_cmd", "arg1"]);
    assert_eq!(cli.server_cmd, vec!["server_cmd", "arg1"]);

    let cli = parse_args(vec!["emacs-lsp-booster", "-v", "--", "server_cmd", "arg1"]);
    assert_eq!(cli.verbose.log_level_filter(), log::LevelFilter::Warn);
    assert_eq!(cli.server_cmd, vec!["server_cmd", "arg1"]);
}
