use std::collections::*;
use std::option::*;
use std::thread::*;
use std::time::*;

use argh::FromArgs;

use fcpeg::*;

use rustnutlib::console::*;
use rustnutlib::fileman::*;

fn main() {
    let cmd: MainCommand = argh::from_env();

    match cmd.subcmd {
        Subcommand::Parse(subcmd) => proc_parse_subcmd(&subcmd),
    }
}

/// cmd command
#[derive(FromArgs, PartialEq, Debug)]
struct MainCommand {
    #[argh(subcommand)]
    subcmd: Subcommand,
}

/// subcommand of cmd command
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
enum Subcommand {
    Parse(ParseSubcommand),
}

/// parse subcommand
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "parse")]
struct ParseSubcommand {
    /// file path of fcpeg source
    #[argh(option, short = 'f')]
    fcpeg: String,

    /// file path of input source
    #[argh(option, short = 'i')]
    input: String,

    /// whether to show help or not
    #[argh(option, default = "false")]
    man: bool,

    /// whether to output syntax tree or not
    #[argh(option, default = "false")]
    mon: bool,

    /// whether to output syntax tree or not
    #[argh(option, short = 'o', default = "false")]
    output: bool,

    /// whether to output syntax tree or not
    #[argh(option, short = 't', default = "false")]
    time: bool,
}

fn proc_parse_subcmd(subcmd: &ParseSubcommand) {
    let mut cons = Console::new();

    // note: マニュアルメッセージ
    if subcmd.man {
        show_parse_cmd_help(&mut cons);
        return;
    }

    let fcpeg_file_path = &subcmd.fcpeg;
    let input_src_paths = subcmd.input.split(";").collect::<Vec<&str>>().iter().map(|s| s.to_string()).collect::<Vec<String>>();
    let output_tree = subcmd.output;
    let is_monitored = subcmd.mon;
    let count_duration = subcmd.time;

    if is_monitored {
        parse_with_monitoring(fcpeg_file_path, &input_src_paths, &mut cons, 1, None, output_tree, count_duration);
    } else {
        parse(fcpeg_file_path, &input_src_paths, &mut cons, output_tree, count_duration);
    }
}

fn show_parse_cmd_help(cons: &mut Console) {
    cons.log(ConsoleLogData::new(ConsoleLogKind::Notice, "command help", vec![
        "parse:\tparse specified files".to_string(),
        "\t-f:\tspecify .fcpeg file".to_string(),
        "\t-h:\tshow help".to_string(),
        "\t-i:\tspecify input files".to_string(),
        "\t-mon:\tmonitor source files".to_string()
    ], vec![]), false);
}

fn show_syntax_trees(input_file_paths: &Vec<String>, trees: &Vec<data::SyntaxTree>) {
    println!("--- Syntax Tree ---");
    println!();

    if input_file_paths.len() != trees.len() {
        println!("* internal error *");
        return;
    }

    for i in 0..input_file_paths.len() {
        println!("{}", input_file_paths[i]);
        trees[i].print(false);
        println!();
    }
}

fn parse(fcpeg_file_path: &String, fcpeg_src_paths: &Vec<String>, cons: &mut Console, output_tree: bool, count_duration: bool) {
    let start_count = Instant::now();

    let trees = match FCPEG::parse_from_paths(fcpeg_file_path, fcpeg_src_paths) {
        Err(e) => {
            cons.log(e.get_console_data(), false);
            println!("--- End ---");
            println!();
            return;
        },
        Ok(v) => v,
    };

    let duration = start_count.elapsed();

    if output_tree {
        show_syntax_trees(fcpeg_src_paths, &trees);
    }

    if count_duration {
        println!("{} msec | {} μsec", duration.as_millis(), duration.as_micros());
        println!();
    }

    println!("--- End ---");
    println!();
}

fn parse_with_monitoring(fcpeg_file_path: &String, fcpeg_src_paths: &Vec<String>, cons: &mut Console, interval_sec: usize, quit_limit_sec: Option<usize>, output_tree: bool, count_duration: bool) {
    cons.log(ConsoleLogData::new(ConsoleLogKind::Notice, "command help", vec!["You can quit parsing with '^C'.".to_string()], vec![]), false);

    match quit_limit_sec {
        Some(v) => cons.log(ConsoleLogData::new(ConsoleLogKind::Notice, "command help", vec![format!("This program will automatically quit {} sec(s) later.", interval_sec * v)], vec![]), false),
        None => (),
    }

    let mut detector_target_file_paths = vec![fcpeg_file_path.clone()];
    detector_target_file_paths.append(&mut fcpeg_src_paths.clone());
    let mut detector = FileChangeDetector::new(detector_target_file_paths);
    let mut loop_count = 0;

    parse(fcpeg_file_path, fcpeg_src_paths, cons, output_tree, count_duration);

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
            parse(fcpeg_file_path, fcpeg_src_paths, cons, output_tree, count_duration);
        }

        loop_count += 1;
        sleep(Duration::from_millis(1000));
    }
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

    fn detect_file_change(&mut self, file_path: &str) -> bool {
        match FileMan::read_all(file_path) {
            Err(_e) => return false,
            Ok(v) => {
                let is_same_cont = match self.log_map.get(file_path) {
                    Some(latest_cont) => *latest_cont == v,
                    None => {
                        self.log_map.insert(file_path.to_string(), v);
                        return false;
                    },
                };

                if !is_same_cont {
                    self.log_map.insert(file_path.to_string(), v);
                }

                return !is_same_cont;
            },
        };
    }

    pub fn detect_multiple_file_changes(&mut self) -> bool {
        for each_path in self.target_file_paths.clone() {
            if self.detect_file_change(&each_path.clone()) {
                return true;
            }
        }

        return false;
    }
}
