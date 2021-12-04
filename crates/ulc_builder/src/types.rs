use serde::Deserialize;

#[derive(Deserialize)]
pub struct Config {
    pub project: Project,
    pub binary: Option<Binary>,
}

#[derive(Deserialize)]
pub struct Project {
    pub name: String,
    pub version: String,
    pub description: String,
    pub authors: Vec<String>,
    pub entry_file: Option<String>,
}

#[derive(Deserialize)]
pub struct Binary {
    pub file_name: String,
}

pub struct BuildConfig {
    pub abort: bool,
    pub emit_clir: bool,
    pub verbose: bool,
}
