use std::collections::*;
use std::option::*;
use std::thread::*;
use std::time::*;

use argh::FromArgs;

use fcpeg::*;

use rustnutlib::*;
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

    /// whether to enable monitoring mode
    #[argh(option, default = "false")]
    mon: bool,

    /// whether to output syntax tree
    #[argh(option, short = 'o', default = "false")]
    output: bool,

    /// whether to output processing time
    #[argh(option, default = "false")]
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
    let log = log!(Notice, "command help",
        "parse:\tparse specified files",
        "\t-f:\tspecify .fcpeg file",
        "\t-h:\tshow help",
        "\t-i:\tspecify input files",
        "\t-mon:\tmonitor source files"
    );

    cons.log(log, false);
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
            cons.log(e.get_log(), false);
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
    let log = log!(Notice, "command help", "You can quit parsing with '^C'.");
    cons.log(log, false);

    match quit_limit_sec {
        Some(v) => {
            let log = log!(Notice, "command help", format!("This program will automatically quit {} sec(s) later.", interval_sec * v));
            cons.log(log, false);
        },
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
