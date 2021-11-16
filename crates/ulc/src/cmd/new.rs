use clap::ArgMatches;
use std::fs;
use std::io::Write;
use std::path::Path;

const CONFIG_TEMPLATE: &str = include_str!("../../../../template/config.toml");
const MAIN_TEMPLATE: &str = include_str!("../../../../template/main.ul");

pub fn execute(args: &ArgMatches) -> anyhow::Result<()> {
    if let Some(dir) = args.value_of("DIRECTORY") {
        let path = Path::new(dir);
        if path.exists() {
            Err(anyhow::anyhow!(
                "Path to project already exists! Please delete."
            ))
        } else if let Some(file_name) = path.file_name() {
            fs::create_dir(path)?;
            let src_path = &path.join("src");
            fs::create_dir(src_path)?;
            let mut config_file = fs::File::create(path.join("config.toml"))?;
            config_file.write_all(
                CONFIG_TEMPLATE
                    .replace("%NAME%", file_name.to_str().unwrap())
                    .as_bytes(),
            )?;
            let mut main_file = fs::File::create(src_path.join("main.ul"))?;
            main_file.write_all(MAIN_TEMPLATE.as_bytes())?;
            Ok(())
        } else {
            Err(anyhow::anyhow!("Path to project is not valid!"))
        }
    } else {
        Err(anyhow::anyhow!(
            "Directory not present in command-line arguments!"
        ))
    }
}
