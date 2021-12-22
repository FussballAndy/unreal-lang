use chrono::Local;
use clap::{App, AppSettings, Arg, SubCommand};
use log::LevelFilter;

mod cmd;
mod doctor;

fn main() {
    init_logger();

    let app = create_clap_app();

    let res = match app.get_matches().subcommand() {
        ("new", Some(sub_matches)) => cmd::new::execute(sub_matches),
        ("build", Some(sub_matches)) => cmd::build::execute(sub_matches),
        ("doctor", Some(_)) => cmd::doctor::execute(),
        _ => unreachable!(),
    };

    if let Err(e) = res {
        log::error!("{}", e);

        for c in e.chain().skip(1) {
            log::error!("\t Caused by: {}", c)
        }
    }
}

fn create_clap_app<'a, 'b>() -> App<'a, 'b> {
    App::new("unreal-lang")
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
                    Arg::with_name("clir")
                        .long("emit-clir")
                        .short("E")
                        .help("Emit LLVM source."),
                )
                .arg(
                    Arg::with_name("verbose")
                        .long("verbose")
                        .short("v")
                        .help("Toggle Verbose logging."),
                ),
        )
        .subcommand(
            SubCommand::with_name("doctor").about("Check if everything is set up correctly!"),
        )
        .setting(AppSettings::ArgRequiredElseHelp)
}

fn init_logger() {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "[{}] {}: {}",
                Local::now().format("%H:%M:%S"),
                record.level(),
                message
            ))
        })
        .level(LevelFilter::Info)
        .level_for("cranelift_object", LevelFilter::Off)
        .chain(std::io::stdout())
        .apply()
        .unwrap();
}
