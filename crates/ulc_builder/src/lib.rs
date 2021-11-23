mod types;

use std::{io::Read, path::PathBuf};

use anyhow::Context;
pub use types::*;

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
        build_config,
    )
}

fn build_input(data: BuildData, input: String, _build_config: BuildConfig) -> anyhow::Result<()> {
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

    Ok(())
}
