mod types;

use std::{
    io::Read,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

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

    let main_file = config
        .project
        .entry_file
        .clone()
        .unwrap_or_else(|| "main.ul".to_owned());

    let mut entry_file_path = root_dir.clone();
    entry_file_path.push("src");
    entry_file_path.push(&main_file);

    let mut contents = String::new();
    let mut file = std::fs::File::open(&entry_file_path).context(format!(
        "The entry file '{}' was not found!",
        &canonicalize(&entry_file_path)?
    ))?;
    file.read_to_string(&mut contents)?;

    build_input(
        BuildData {
            root: root_dir,
            main_file: &main_file,
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
    build_config: BuildConfig,
) -> anyhow::Result<()> {
    if !data.root.join("target").exists() {
        std::fs::create_dir(data.root.join("target"))?;
        if build_config.verbose {
            log::info!("Created target directory.");
        }
    }
    if !data.root.join("target").join("clif").exists() {
        std::fs::create_dir(data.root.join("target").join("clif"))?;
    }
    if !data.root.join("target").join("objs").exists() {
        std::fs::create_dir(data.root.join("target").join("objs"))?;
    }

    let mut parser = Parser::new(&input);
    let mut stmts = Vec::new();

    if build_config.verbose {
        log::info!("Starting parser.");
    }

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

    if build_config.verbose {
        log::info!("Finished parser.");
    }

    let mut root = MiddleAstRoot::new();

    root.append_all_funcs(stmts).map_err(|err| {
        err.0.display(&input, data.main_file);
        anyhow::anyhow!("Aborted due to error. Read error report above.")
    })?;

    if build_config.verbose {
        log::info!("Checked code! Everythings fine.");
    }

    let mut codegen = CraneliftCodegonBackend::new(data.main_file);
    log::info!("Starting compilation.");
    codegen.compile(root)?;
    log::info!("Done.");

    let main_file = &data.main_file[..(data.main_file.len() - 3)];

    let ir_file = data
        .root
        .join("target")
        .join("clif")
        .join(format!("{}.clif", main_file));

    let obj_path = data
        .root
        .join("target")
        .join("objs")
        .join(format!("{}.o", main_file));

    codegen.finish(&ir_file, &obj_path)?;

    let binary_name = if let Some(bin) = config.binary {
        bin.file_name
    } else {
        config.project.name + if cfg!(windows) { ".exe" } else { "" }
    };

    let output_path = data.root.join("target").join(binary_name);

    let mut clang_cmd = if let Some(clang) = option_env!("CLANG_PATH") {
        Command::new(clang)
    } else {
        Command::new("clang")
    };

    clang_cmd.args([
        &canonicalize(&obj_path)?,
        "-o",
        output_path.to_str().unwrap(),
    ]);
    clang_cmd.stdout(Stdio::null());
    clang_cmd.stderr(Stdio::piped());
    let mut clang_child = clang_cmd.spawn()?;
    let clang_res = clang_child.wait()?;
    if clang_res.success() {
        log::info!(
            "Finished linking! Executable was put in {}!",
            &canonicalize(output_path)?
        );
        Ok(())
    } else {
        let mut err = String::new();
        clang_child
            .stderr
            .take()
            .unwrap()
            .read_to_string(&mut err)?;
        log::error!("Encountered the following Clang error: \n{}", err);
        Err(anyhow::anyhow!("Read the Clang error above!"))
    }
}

fn canonicalize<P: AsRef<Path>>(path: P) -> anyhow::Result<String> {
    let pat = path.as_ref();
    let can_path = std::fs::canonicalize(pat)?;
    let can = can_path.to_str().unwrap();
    let ret = if cfg!(windows) { &can[4..] } else { can };
    Ok(ret.to_owned())
}
