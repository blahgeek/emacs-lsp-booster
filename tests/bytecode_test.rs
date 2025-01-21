use simd_json as json;
use anyhow::Result;
use tempfile;

use emacs_lsp_booster::bytecode;


fn run_one_test(json_str: &mut str, object_type: bytecode::ObjectType) -> Result<()> {
    let json_value: serde_json::Value = unsafe{json::from_str(json_str)?};
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
                                 bytecode::ObjectType::Hashtable => "hash-table",
                                 bytecode::ObjectType::Alist => "alist",
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
fn test_bytecode() {
    // unicode test
    let mut s = String::from(r#"{"a":"ÀÁÂÃÄÅÆÇÈÉÊËÌ abcd \n 你好世界"}"#);
    run_one_test(&mut s, bytecode::ObjectType::Plist).unwrap();

    for object_type in vec![bytecode::ObjectType::Plist,
                            bytecode::ObjectType::Alist,
                            bytecode::ObjectType::Hashtable] {
        eprintln!("Testing completion.json (~100KB), object type = {:?}", object_type);
        let mut s = String::from(include_str!("./data/completion.json"));
        run_one_test(&mut s, object_type).unwrap();

        eprintln!("Testing completion2.json (~100KB), object type = {:?}", object_type);
        let mut s = String::from(include_str!("./data/completion2.json"));
        run_one_test(&mut s, object_type).unwrap();

        eprintln!("Testing completion3.json (~4KB), object type = {:?}", object_type);
        let mut s = String::from(include_str!("./data/completion3.json"));
        run_one_test(&mut s, object_type).unwrap();

        eprintln!("Testing publishDiagnostics.json (~12KB), object type = {:?}", object_type);
        let mut s = String::from(include_str!("./data/publishDiagnostics.json"));
        run_one_test(&mut s, object_type).unwrap();

        eprintln!("Testing publishDiagnostics2.json (~12KB), object type = {:?}", object_type);
        let mut s = String::from(include_str!("./data/publishDiagnostics2.json"));
        run_one_test(&mut s, object_type).unwrap();
    }

    {
        eprintln!("Testing huge array (100000 elements)");
        let value = serde_json::Value::Array(
            (0..100000).map(|x| serde_json::Value::String(format!("{}", x)))
                .collect()
        );
        run_one_test(&mut value.to_string(), bytecode::ObjectType::Plist).unwrap();
    }

    {
        eprintln!("Testing huge map (100000 elements)");
        let value = serde_json::Value::Object(
            (0..100000).map(|x| (format!("x{}", x),
                                 serde_json::Value::Number(x.into())))
                .collect()
        );
        run_one_test(&mut value.to_string(), bytecode::ObjectType::Plist).unwrap();
    }
}
