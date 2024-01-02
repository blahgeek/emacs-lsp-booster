use serde_json as json;
use anyhow::Result;
use tempfile;

use emacs_lsp_booster::bytecode;


fn run_one_test(json_str: &str, object_type: bytecode::ObjectType) -> Result<()> {
    let json_value: json::Value = json::from_str(json_str)?;
    let json_str_nowhitespaces = json_value.to_string();
    let bytecode = bytecode::generate_bytecode_repl(&json_value, bytecode::BytecodeOptions {
        object_type: object_type.clone(),
        ..Default::default()
    })?;

    eprintln!("Json: {} bytes, Bytecode: {} bytes, ratio={}",
              json_str_nowhitespaces.len(), bytecode.len(),
              bytecode.len() as f64 / json_str_nowhitespaces.len() as f64);

    let tmpdir = tempfile::tempdir()?;
    let json_file = tmpdir.path().join("data.json");
    std::fs::write(&json_file, json_str_nowhitespaces.as_bytes())?;

    let bytecode_file = tmpdir.path().join("bytecode.txt");
    std::fs::write(&bytecode_file, bytecode.as_bytes())?;

    let elisp_file = tmpdir.path().join("script.el");
    let elisp_code = format!(include_str!("./benchmark_and_compare.template.el"),
                             json_file.display(),
                             bytecode_file.display(),
                             match object_type {
                                 bytecode::ObjectType::Plist => "plist",
                                 bytecode::ObjectType::Hashtable => "hashtable",
                             });
    std::fs::write(&elisp_file, elisp_code.as_bytes())?;

    let mut child = std::process::Command::new("emacs")
        .arg("--batch")
        .arg("-l").arg(elisp_file.as_os_str())
        .stdout(std::process::Stdio::inherit())
        .stderr(std::process::Stdio::inherit())
        .spawn()?;
    let emacs_return_code = child.wait()?;
    assert!(emacs_return_code.success());

    Ok(())
}

#[test]
fn test_huge_array() {
    let value = json::Value::Array(
        (0..100000).map(|x| json::Value::String(format!("{}", x)))
            .collect()
    );
    run_one_test(&value.to_string(), bytecode::ObjectType::Plist).unwrap();
}

#[test]
fn test_huge_object() {
    let value = json::Value::Object(
        (0..100000).map(|x| (format!("x{}", x),
                             json::Value::Number(x.into())))
            .collect()
    );
    run_one_test(&value.to_string(), bytecode::ObjectType::Plist).unwrap();
}

#[test]
fn test_completion_100k() {
    run_one_test(include_str!("./data/completion.json"), bytecode::ObjectType::Plist).unwrap();
    run_one_test(include_str!("./data/completion.json"), bytecode::ObjectType::Hashtable).unwrap();
}

#[test]
fn test_completion_100k_2() {
    run_one_test(include_str!("./data/completion2.json"), bytecode::ObjectType::Plist).unwrap();
}

#[test]
fn test_completion_4k() {
    run_one_test(include_str!("./data/completion3.json"), bytecode::ObjectType::Plist).unwrap();
}

#[test]
fn test_diagnostics_12k() {
    run_one_test(include_str!("./data/publishDiagnostics.json"), bytecode::ObjectType::Plist).unwrap();
}

#[test]
fn test_diagnostics_12k_2() {
    run_one_test(include_str!("./data/publishDiagnostics2.json"), bytecode::ObjectType::Plist).unwrap();
}
