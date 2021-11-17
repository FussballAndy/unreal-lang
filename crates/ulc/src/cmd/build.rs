use anyhow::Context;
use clap::ArgMatches;
use std::env::current_dir;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use ulc_builder::{BuildConfig, Config};

pub fn execute(args: &ArgMatches) -> anyhow::Result<()> {
    let path = if let Some(path_str) = args.value_of("DIRECTORY") {
        PathBuf::from(path_str)
    } else {
        current_dir()?
    };
    let build_config = BuildConfig {
        abort: args.value_of("abort").is_some(),
        emit_llvm: args.value_of("llvm").is_some(),
    };
    let mut config_file =
        File::open(path.join("config.toml")).context("File 'config.toml' not found!")?;
    let mut contents = String::new();
    config_file.read_to_string(&mut contents)?;
    let config: Config = toml::from_str(&contents)?;
    ulc_builder::build(path, config, build_config)
}
