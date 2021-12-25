mod types;

use std::{
    collections::HashMap,
    io::Read,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    time::Instant,
};

use anyhow::Context;
pub use types::*;

use ulc_codegen_cranelift::CraneliftCodegonBackend;
use ulc_middle_ast::{FuncData, MiddleAstRoot};
use ulc_parser::chumsky_parser;

struct BuildData {
    pub root: PathBuf,
    pub main_file: String,
}

pub fn build(root_dir: PathBuf, config: Config, build_config: BuildConfig) -> anyhow::Result<()> {
    let start = Instant::now();
    log::info!(
        "Building {} @ {}",
        config.project.name,
        config.project.version
    );

    let main_file = config
        .project
        .entry_file
        .clone()
        .unwrap_or_else(|| "main".to_owned());

    if !root_dir.join("target").exists() {
        std::fs::create_dir(root_dir.join("target"))?;
        if build_config.verbose {
            log::info!("Created target directory.");
        }
    }
    if !root_dir.join("target").join("clif").exists() {
        std::fs::create_dir(root_dir.join("target").join("clif"))?;
    }
    if !root_dir.join("target").join("objs").exists() {
        std::fs::create_dir(root_dir.join("target").join("objs"))?;
    }

    build_input(
        BuildData {
            root: root_dir,
            main_file,
        },
        config,
        build_config,
    )?;

    log::info!(
        "Build finished after {:.2} seconds!",
        start.elapsed().as_secs_f32()
    );

    Ok(())
}

fn build_input(data: BuildData, config: Config, build_config: BuildConfig) -> anyhow::Result<()> {
    let mut finished_files = Vec::new();
    let mut queued_files = vec![data.main_file.clone()];

    let mut outlines = HashMap::new();
    let mut mast_roots = HashMap::new();

    while let Some(fil) = queued_files.pop() {
        if finished_files.contains(&fil) {
            continue;
        }

        let mut cur_file_path = data.root.join("src").join(&fil);
        cur_file_path.set_extension("ul");

        // If someone likes doing weird stuff with filenames then so be it
        let fil = cur_file_path
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned();

        log::info!("Parsing: {}", &fil);

        let input = std::fs::read_to_string(&cur_file_path).context(format!(
            "The file '{}' was not found!",
            &canonicalize(&cur_file_path)?
        ))?;

        let parser = chumsky_parser(&input);

        let top_level_stmts = match parser.parsed_funcs {
            Some(funs) => funs,
            None => {
                parser
                    .lexer_errors
                    .into_iter()
                    .for_each(|e| println!("{}", e));
                parser
                    .parser_errors
                    .into_iter()
                    .for_each(|e| e.display(&input, &fil));
                anyhow::bail!("Aborted due to error. Read error report above.")
            }
        };

        let is_main = data.main_file.starts_with(&fil);
        let mut root = MiddleAstRoot::new(fil.clone(), is_main);

        let (funcs, imports) = root.append_all_tls(top_level_stmts).map_err(|err| {
            err.0.display(&input, &fil);
            anyhow::anyhow!("Aborted due to error. Read error report above.")
        })?;

        for import in imports {
            queued_files.push(import);
        }

        outlines.insert(
            fil.clone(),
            funcs
                .iter()
                .filter_map(|x| {
                    if is_main && &x.node.ident.node == "main" {
                        None
                    } else {
                        Some(FuncData {
                            ident: (x.node.ident.node.clone(), None),
                            param_tys: x.node.params.iter().map(|a| (a.node.1, None)).collect(),
                            ret_ty: x.node.return_type.node,
                        })
                    }
                })
                .collect::<Vec<_>>(),
        );
        mast_roots.insert((fil.clone(), input), (root, funcs));

        finished_files.push(fil);
    }

    drop(queued_files);
    drop(finished_files);

    let mut obj_file_paths = Vec::new();

    for ((cur_ns, input), (mut root, mast_root)) in mast_roots {
        root.translate(&outlines, mast_root).map_err(|err| {
            err.0.display(&input, &cur_ns);
            anyhow::anyhow!("Aborted due to error. Read error report above.")
        })?;

        log::info!("Compiling: {}", cur_ns);

        let obj_path = native_codegen(&cur_ns, &data.root, root)?;

        let canned_obj_path = canonicalize(obj_path)?;

        obj_file_paths.push(canned_obj_path);
    }

    let binary_name = if let Some(bin) = config.binary {
        bin.file_name
    } else {
        config.project.name
    } + if cfg!(windows) { ".exe" } else { "" };

    let output_path = data.root.join("target").join(binary_name);

    let mut clang_cmd = if let Some(clang) = option_env!("CLANG_PATH") {
        Command::new(clang)
    } else {
        Command::new("clang")
    };

    let mut args = obj_file_paths;
    args.push("-o".to_owned());

    let canned_output_path =
        canonicalize(&output_path).unwrap_or_else(|_| output_path.to_str().unwrap().to_owned());

    args.push(canned_output_path.clone());

    log::info!("Starting linking");

    if build_config.verbose {
        log::info!("Calling linker with: {}", &args.join(", "));
    }

    clang_cmd.args(args);
    clang_cmd.stdout(Stdio::null());
    clang_cmd.stderr(Stdio::piped());
    let mut clang_child = clang_cmd.spawn()?;
    let clang_res = clang_child.wait()?;
    if clang_res.success() {
        log::info!(
            "Finished linking! Executable was put in {}!",
            canned_output_path
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

fn native_codegen(
    file: &str,
    file_root: &Path,
    ast_root: MiddleAstRoot,
) -> anyhow::Result<PathBuf> {
    let mut codegen = CraneliftCodegonBackend::new(file);
    codegen.compile(ast_root)?;

    let ir_file = file_root
        .join("target")
        .join("clif")
        .join(format!("{}.clif", file));

    let obj_path = file_root
        .join("target")
        .join("objs")
        .join(format!("{}.o", file));

    codegen.finish(&ir_file, &obj_path)?;
    Ok(obj_path)
}

fn canonicalize<P: AsRef<Path>>(path: P) -> anyhow::Result<String> {
    let pat = path.as_ref();
    let can_path = std::fs::canonicalize(pat)?;
    let can = can_path.to_str().unwrap();
    let ret = if cfg!(windows) { &can[4..] } else { can };
    Ok(ret.to_owned())
}
