use anyhow::{Result, bail};
use log::trace;
use clap::Parser;

use emacs_lsp_booster::app;
use emacs_lsp_booster::bytecode;

#[derive(Parser)]
#[command(long_about = None, about = None,
          arg_required_else_help = true, after_help = "For backward compatibility, `emacs-lsp-booster <SERVER_CMD>...` (without any options) is also supported" )]
struct Cli {
    #[command(flatten)]
    verbose: clap_verbosity_flag::Verbosity<clap_verbosity_flag::InfoLevel>,

    #[arg(last = true)]
    server_cmd: Vec<String>,

    #[arg(
        short = 'n',
        long,
        help = "Disable bytecode generation. Simply forward server json as-is. Useful for debugging or benchmarking."
    )]
    disable_bytecode: bool,

    #[arg(
        long,
        default_value = "plist",
        help = "Lisp type used to represent a JSON object. Plist is the most performant one.\nMust match what lsp client expects.\n"
    )]
    json_object_type: bytecode::ObjectType,

    #[arg(
        long,
        default_value = "nil",
        help = "Which lisp value is used to represent a JSON null value. Support :keyword or nil.\nMust match what lsp client expects.\n"
    )]
    json_null_value: bytecode::LispObject,

    #[arg(
        long,
        default_value = "nil",
        help = "Which lisp value is used to represent a JSON false value. Support :keyword or nil.\nMust match what lsp client expects.\n"
    )]
    json_false_value: bytecode::LispObject,
}

fn parse_args<T, S>(args: T) -> Cli
where
    T: IntoIterator<Item = S>,
    S: Into<String>,
{
    let args = args.into_iter().map(|x| x.into()).collect::<Vec<String>>();
    // backward compatible. support `emacs-lsp-booster server_cmd args...` directly
    if args.len() > 1 && !args[1].starts_with('-') && !args.contains(&"--".into()) {
        let mut fake_args = vec![args[0].clone(), "--".into()];
        fake_args.extend_from_slice(&args[1..]);
        Cli::parse_from(fake_args)
    } else {
        Cli::parse_from(args)
    }
}

fn main() -> Result<()> {
    let cli = parse_args(std::env::args());
    env_logger::Builder::new()
        .filter_level(cli.verbose.log_level_filter())
        .init();

    if cli.server_cmd.is_empty() {
        bail!("Please specify the server command");
    }

    // exit the process if any thread panic
    let original_panic_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        original_panic_hook(info);
        std::process::exit(1);
    }));

    // In windows, Command::new cannot find .cmd files, so use `which` to do that
    // https://github.com/rust-lang/rust/issues/37519
    let server_cmd_prog = if cfg!(windows) {
        which::which(&cli.server_cmd[0])?
    } else {
        std::path::PathBuf::from(&cli.server_cmd[0])
    };
    trace!("Using server prog: {:?}", server_cmd_prog);
    let mut cmd = std::process::Command::new(&server_cmd_prog);
    cmd.args(&cli.server_cmd[1..]);

    let exit_status = app::run_app_forever(
        std::io::stdin(),
        std::io::stdout(),
        cmd,
        app::AppOptions {
            bytecode_options: if !cli.disable_bytecode {
                Some(bytecode::BytecodeOptions {
                    object_type: cli.json_object_type,
                    null_value: cli.json_null_value,
                    false_value: cli.json_false_value,
                })
            } else {
                None
            },
        },
    )?;
    std::process::exit(exit_status.code().unwrap_or(1))
}

#[test]
fn test_parse_args() {
    let cli = parse_args(vec!["emacs-lsp-booster", "server_cmd", "arg1"]);
    assert_eq!(cli.server_cmd, vec!["server_cmd", "arg1"]);
    assert_eq!(cli.verbose.log_level_filter(), log::LevelFilter::Info);

    let cli = parse_args(vec!["emacs-lsp-booster", "--", "server_cmd", "arg1"]);
    assert_eq!(cli.server_cmd, vec!["server_cmd", "arg1"]);

    let cli = parse_args(vec![
        "emacs-lsp-booster",
        "-v",
        "--json-object-type",
        "hashtable",
        "--json-null-value",
        ":null",
        "--json-false-value",
        ":json-false",
        "--",
        "server_cmd",
        "arg1",
    ]);
    assert_eq!(cli.verbose.log_level_filter(), log::LevelFilter::Debug);
    assert_eq!(cli.server_cmd, vec!["server_cmd", "arg1"]);
    assert_eq!(cli.json_object_type, bytecode::ObjectType::Hashtable);
    assert_eq!(
        cli.json_null_value,
        bytecode::LispObject::Keyword("null".into())
    );
    assert_eq!(
        cli.json_false_value,
        bytecode::LispObject::Keyword("json-false".into())
    );
}
