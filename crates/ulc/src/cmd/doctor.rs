pub fn execute() -> anyhow::Result<()> {
    crate::doctor::run_doctor()?;
    log::info!("Doctor was successful!");
    Ok(())
}
