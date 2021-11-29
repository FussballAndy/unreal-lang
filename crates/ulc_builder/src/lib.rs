mod types;

use std::{io::Read, path::PathBuf, process::Command};

use anyhow::Context;
pub use types::*;

use ulc_codegen_cranelift::CraneliftCodegonBackend;
use ulc_middle_ast::MiddleAstRoot;
use ulc_parser::Parser;
use ulc_types::errors::SyntaxError;

struct BuildData<'input> {
    pub root: PathBuf,
    pub main_file: &'input str,
}

pub fn build(root_dir: PathBuf, config: Config, build_config: BuildConfig) -> anyhow::Result<()> {
    log::info!(
        "Building {} @ {}",
        config.project.name,
        config.project.version
    );
    let path = root_dir.join("src").join(
        config
            .project
            .entry_file
            .clone()
            .unwrap_or_else(|| "main.ul".to_owned()),
    );
    let mut contents = String::new();
    let mut file = std::fs::File::open(&path).context(format!(
        "The entry file '{}' was not found!",
        std::fs::canonicalize(&path)?.to_str().unwrap()
    ))?;
    file.read_to_string(&mut contents)?;

    build_input(
        BuildData {
            root: root_dir,
            main_file: path.to_str().unwrap(),
        },
        contents,
        config,
        build_config,
    )
}

fn build_input(
    data: BuildData,
    input: String,
    config: Config,
    _build_config: BuildConfig,
) -> anyhow::Result<()> {
    if !data.root.join("target").exists() {
        std::fs::create_dir(data.root.join("target"))?;
    }
    let mut parser = Parser::new(&input);
    let mut stmts = Vec::new();

    loop {
        match parser.parse_global_statement() {
            Ok(stmt) => stmts.push(stmt),
            Err(err) => {
                if let SyntaxError::End = err {
                    break;
                }
                err.display(&input, data.main_file);
                anyhow::bail!("Aborted due to error. Read error report above.")
            }
        }
    }

    let mut root = MiddleAstRoot::new();

    root.append_all_funcs(stmts).map_err(|err| {
        err.0.display(&input, data.main_file);
        anyhow::anyhow!("Aborted due to error. Read error report above.")
    })?;

    let mut codegen = CraneliftCodegonBackend::new(data.main_file);
    codegen.compile(root)?;

    let obj_path = data
        .root
        .join("target")
        .join("objs")
        .join(format!("{}.o", data.main_file));

    codegen.finish(
        &data
            .root
            .join("target")
            .join("clif")
            .join(format!("{}.clif", data.main_file)),
        &obj_path,
    )?;

    let binary_name = if let Some(bin) = config.binary {
        bin.file_name
    } else {
        config.project.name + if cfg!(windows) { ".exe" } else { "" }
    };

    let mut gcc_cmd = if let Some(gcc) = option_env!("GCC_PATH") {
        Command::new(gcc)
    } else {
        Command::new("gcc")
    };
    gcc_cmd.args([
        "-o",
        &binary_name,
        std::fs::canonicalize(&obj_path)?.to_str().unwrap(),
    ]);
    gcc_cmd.status()?;

    Ok(())
}
