use std::process::Command;

use anyhow::Context;

pub fn execute() -> anyhow::Result<()> {
    let gcc_path = option_env!("GCC_PATH");
    if let Some(gcc) = gcc_path {
        Command::new(gcc)
            .args(["--version"])
            .status()
            .context("GCC_PATH env points to not found GCC!")
            .map(|_| ())
    } else {
        Command::new("gcc")
            .args(["--version"])
            .status()
            .context("GCC needs to be installed and added to path!")
            .map(|_| ())
    }
}
