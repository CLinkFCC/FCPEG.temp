use std::collections::*;
use std::option::*;
use std::thread::*;
use std::time::*;

use argh::FromArgs;

use fcpeg::*;

use rustnutlib::*;
use rustnutlib::console::*;
use rustnutlib::file::*;

fn main() {
    let cmd: MainCommand = argh::from_env();

    match cmd.subcmd {
        Subcommand::Parse(subcmd) => spawn(move || proc_parse_subcmd(&subcmd)).join().unwrap(),
    }
}

/// cmd command
#[derive(FromArgs, PartialEq)]
struct MainCommand {
    #[argh(subcommand)]
    subcmd: Subcommand,
}

/// subcommands of cmd command
#[derive(FromArgs, PartialEq)]
#[argh(subcommand)]
enum Subcommand {
    Parse(ParseSubcommand),
}

/// parse subcommand
#[derive(Clone, FromArgs, PartialEq)]
#[argh(subcommand, name = "parse")]
struct ParseSubcommand {
    /// file path of fcpeg source
    #[argh(option, short = 'f')]
    fcpeg: String,

    /// file path of input source
    #[argh(option, short = 'i')]
    input: String,

    /// whether to show help or not
    #[argh(switch)]
    man: bool,

    /// whether to enable monitoring mode
    #[argh(switch)]
    mon: bool,

    /// whether to output syntax tree
    #[argh(switch, short = 'o')]
    output: bool,

    /// whether to output processing time
    #[argh(switch, short = 't')]
    time: bool,
}

fn proc_parse_subcmd(subcmd: &ParseSubcommand) {
    let mut cons = match Console::load(None) {
        Ok(v) => v,
        Err(_) => {
            println!("[err] failed to launch parser: failure on loading console data");
            return;
        },
    };

    // note: マニュアルメッセージ
    if subcmd.man {
        show_parse_cmd_help(&mut cons);
        return;
    }

    let fcpeg_file_path = subcmd.fcpeg.clone();
    let input_file_path = subcmd.input.clone();
    let output_tree = subcmd.output;
    let is_monitored = subcmd.mon;
    let count_duration = subcmd.time;

    if is_monitored {
        cons.log(log!(Notice, "command help", "You can quit parsing with '^C'."), false);

        match parse_with_monitoring(&mut cons, fcpeg_file_path, input_file_path, 1, Some(600), output_tree, count_duration) {
            Ok(()) => (),
            Err(e) => cons.log(e.get_log(), false),
        }
    } else {
        match parse(fcpeg_file_path, input_file_path, output_tree, count_duration) {
            Ok(()) => (),
            Err(e) => cons.log(e.get_log(), false),
        }
    }
}

fn show_parse_cmd_help(cons: &mut Console) {
    let log = log!(Notice, "command help",
        "parse:\tparse specified files",
        "\t-f:\tspecify .fcpeg file",
        "\t-h:\tshow help",
        "\t-i:\tspecify input files",
        "\t-mon:\tmonitor source files"
    );

    cons.log(log, false);
}

fn parse(fcpeg_file_path: String, input_file_path: String, output_tree: bool, count_duration: bool) -> FCPEGResult<()> {
    let start_count = Instant::now();
    let mut file_alias_map = HashMap::<String, String>::new();
    file_alias_map.insert("A".to_string(), "src/a.fcpeg".to_string());
    let mut parser = FCPEGParser::load(fcpeg_file_path, file_alias_map)?;
    let tree = parser.parse(input_file_path.clone())?;

    let duration = start_count.elapsed();

    if output_tree {
        println!("--- Syntax Tree ---");
        println!();
        println!("{}", input_file_path);
        tree.print(false);
        println!();
    }

    if count_duration {
        println!("{} msec | {} μsec", duration.as_millis(), duration.as_micros());
        println!();
    }

    println!("--- End ---");
    println!();

    return Ok(());
}

fn parse_with_monitoring(cons: &mut Console, fcpeg_file_path: String, input_file_path: String, interval_sec: usize, quit_limit_sec: Option<usize>, output_tree: bool, count_duration: bool) -> FCPEGResult<()> {
    let detector_target_file_paths = vec![fcpeg_file_path.clone(), input_file_path.clone()];
    let mut detector = FileChangeDetector::new(detector_target_file_paths);
    let mut loop_count = 0;

    match parse(fcpeg_file_path.clone(), input_file_path.clone(), output_tree, count_duration) {
        Ok(()) => (),
        Err(e) => cons.log(e.get_log(), true),
    }

    loop {
        match quit_limit_sec {
            Some(v) => {
                if loop_count + 1 * interval_sec >= v {
                    break;
                }
            },
            None => (),
        }

        if detector.detect_multiple_file_changes() {
            match parse(fcpeg_file_path.clone(), input_file_path.clone(), output_tree, count_duration) {
                Ok(()) => (),
                Err(e) => cons.log(e.get_log(), true),
            }
        }

        loop_count += 1;
        sleep(Duration::from_millis(1000));
    }

    return Ok(());
}

struct FileChangeDetector {
    log_map: HashMap<String, String>,
    target_file_paths: Vec<String>,
}

impl FileChangeDetector {
    pub fn new(target_file_paths: Vec<String>) -> FileChangeDetector {
        return FileChangeDetector {
            log_map: HashMap::new(),
            target_file_paths: target_file_paths,
        };
    }

    fn detect_file_change(&mut self, file_path: String) -> bool {
        match FileMan::read_all(&file_path) {
            Err(_e) => return false,
            Ok(v) => {
                let is_same_cont = match self.log_map.get(&file_path) {
                    Some(latest_cont) => *latest_cont == v,
                    None => {
                        self.log_map.insert(file_path, v);
                        return false;
                    },
                };

                if !is_same_cont {
                    self.log_map.insert(file_path, v);
                }

                return !is_same_cont;
            },
        };
    }

    pub fn detect_multiple_file_changes(&mut self) -> bool {
        for each_path in self.target_file_paths.clone() {
            if self.detect_file_change(each_path.clone()) {
                return true;
            }
        }

        return false;
    }
}
