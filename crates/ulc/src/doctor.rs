use std::process::{Command, Stdio};

use anyhow::Context;

pub(crate) fn run_doctor() -> anyhow::Result<()> {
    let clang_path = option_env!("CLANG_PATH");
    let res_status = if let Some(clang) = clang_path {
        Command::new(clang)
            .args(["--version"])
            .stdout(Stdio::null())
            .status()
            .context("CLANG_PATH env points to not found Clang!")?
    } else {
        Command::new("clang")
            .args(["--version"])
            .stdout(Stdio::null())
            .status()
            .context("Clang needs to be installed and either added to path or CLANG_PATH needs to be set to the clang exectutable path!")?
    };
    if res_status.success() {
        Ok(())
    } else if let Some(code) = res_status.code() {
        Err(anyhow::anyhow!(
            "Clang exited with {}! Please run 'clang --version' yourself to see output!",
            code
        ))
    } else {
        Err(anyhow::anyhow!("Clang exited unexpetedly!"))
    }
}
