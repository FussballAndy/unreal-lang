use chrono::Local;
use clap::{App, Arg, SubCommand};
use env_logger::Builder;
use log::LevelFilter;
use std::io::Write;

mod cmd;

fn main() {
    init_logger();

    let app = create_clap_app();

    let res = match app.get_matches().subcommand() {
        ("new", Some(sub_matches)) => cmd::new::execute(sub_matches),
        ("build", Some(sub_matches)) => cmd::build::execute(sub_matches),
        _ => unreachable!(),
    };

    if let Err(e) = res {
        log::error!("Error: {}", e);

        for c in e.chain().skip(1) {
            log::error!("\t Caused by: {}", c)
        }
    }
}

fn create_clap_app<'a, 'b>() -> App<'a, 'b> {
    App::new("unreal-lang")
        .version("0.1")
        .author("UnrealCode")
        .about("CLI for unreal-lang")
        .subcommand(
            SubCommand::with_name("new")
                .about("Creates a new unreal-lang project")
                .arg(
                    Arg::with_name("DIRECTORY")
                        .help("The name/path of the project")
                        .required(true)
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("build")
                .about("Builds a unreal-lang project")
                .arg(
                    Arg::with_name("DIRECTORY")
                        .help("The path to the project to build")
                        .required(false)
                        .index(1),
                )
                .arg(
                    Arg::with_name("abort")
                        .long("abort")
                        .short("A")
                        .help("Aborts on warnings!"),
                )
                .arg(
                    Arg::with_name("llvm")
                        .long("emit-llvm")
                        .short("E")
                        .help("Emit LLVM source."),
                ),
        )
}

fn init_logger() {
    let mut builder = Builder::new();

    builder.format(|formatter, record| {
        writeln!(
            formatter,
            "[{}] {} {}: {}",
            Local::now().format("%H:%M:%S"),
            record.level(),
            record.target(),
            record.args()
        )
    });

    builder.filter(None, LevelFilter::Info);

    builder.init();
}
