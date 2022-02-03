use std::cell::RefCell;
use std::collections::*;
use std::option::*;
use std::rc::Rc;
use std::thread::*;
use std::time::*;

use argh::FromArgs;

use fcpeg::*;

use rustnutlib::*;
use rustnutlib::console::*;
use rustnutlib::file::*;

fn main() {
    let cmd: MainCommand = argh::from_env();
    let cons = match Console::load(None, ConsoleLogLimit::NoLimit) {
        Ok(v) => v,
        Err(_) => {
            println!("[err] failed to launch parser: failure on loading console data");
            return;
        },
    };

    match cmd.subcmd {
        Subcommand::Manual(subcmd) => spawn(move || proc_manual_subcommand(&subcmd, cons)).join().unwrap(),
        Subcommand::Parse(subcmd) => spawn(move || proc_parse_subcmd(&subcmd, cons)).join().unwrap(),
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
    Manual(ManualSubcommand),
    Parse(ParseSubcommand),
}

/// man subcommand
#[derive(Clone, FromArgs, PartialEq)]
#[argh(subcommand, name = "man")]
struct ManualSubcommand {}

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

    /// whether to enable monitoring mode
    #[argh(switch)]
    mon: bool,

    /// whether to disable optimization
    #[argh(switch)]
    noopt: bool,

    /// whether to output syntax tree
    #[argh(switch, short = 'o')]
    output: bool,

    /// whether to output processing time
    #[argh(switch, short = 't')]
    time: bool,
}

fn proc_parse_subcmd(subcmd: &ParseSubcommand, cons: Console) {
    let fcpeg_file_path = subcmd.fcpeg.clone();
    let input_file_path = subcmd.input.clone();
    let output_tree = subcmd.output;
    let disable_opt = subcmd.noopt;
    let is_monitored = subcmd.mon;
    let count_duration = subcmd.time;

    let cons_ptr = Rc::from(RefCell::new(cons));

    if is_monitored {
        cons_ptr.borrow_mut().append_log(log!(Note, "command help", "You can quit parsing with '^C'."));
        let _ = parse_with_monitoring(&cons_ptr, fcpeg_file_path, input_file_path, 1, Some(600), output_tree, count_duration, disable_opt);
    } else {
        parse(&cons_ptr, fcpeg_file_path, input_file_path, output_tree, count_duration, disable_opt);
    }
}

fn proc_manual_subcommand(_: &ManualSubcommand, cons: Console) {
    let cons_ptr = Rc::from(RefCell::new(cons));

    let log = log!(Note, "command help",
        "parse:\tparse specified files",
            "\t-f:\tspecify .fcpeg file",
            "\t-i:\tspecify input files",
            "\t-o:\toutput syntax trees",
            "\t-t:\toutput processing time",
            "\t--man:\tshow help",
            "\t--mon:\tmonitor source files",
            "\t--noopt:\tdisable optimization"
    );

    cons_ptr.borrow_mut().append_log(log);
    cons_ptr.borrow().print_all();
}

fn parse(cons: &Rc<RefCell<Console>>, fcpeg_file_path: String, input_file_path: String, output_tree: bool, count_duration: bool, disable_opt: bool) {
    let start_count = Instant::now();
    // let mut file_alias_map = HashMap::<String, String>::new();
    // file_alias_map.insert("A".to_string(), "src/a.fcpeg".to_string());
    let mut parser = match FCPEGParser::load(cons.clone(), fcpeg_file_path, HashMap::<String, String>::new(), !disable_opt) {
        Ok(v) => v,
        Err(()) => {
            cons.borrow().print_all();
            cons.borrow_mut().clear_log();

            println!("--- Error End ---");
            println!();

            return;
        },
    };

    let tree = match parser.parse(input_file_path.clone()) {
        Ok(v) => v,
        Err(()) => {
            cons.borrow().print_all();
            cons.borrow_mut().clear_log();

            println!("--- Error End ---");
            println!();

            return;
        },
    };

    let duration = start_count.elapsed();

    if output_tree {
        println!("--- Syntax Tree ---");
        println!();
        println!("{}", input_file_path);
        tree.print(true);
        println!();
    }

    if count_duration {
        println!("{} msec | {} μsec", duration.as_millis(), duration.as_micros());
        println!();
    }

    cons.borrow().print_all();
    cons.borrow_mut().clear_log();

    println!("--- End ---");
    println!();
}

fn parse_with_monitoring(cons: &Rc<RefCell<Console>>, fcpeg_file_path: String, input_file_path: String, interval_sec: usize, quit_limit_sec: Option<usize>, output_tree: bool, count_duration: bool, disable_opt: bool) -> ConsoleResult<()> {
    let cfg_file_path = FileMan::rename_ext(&fcpeg_file_path, "cfg");

    let detector_target_file_paths = vec![
        &fcpeg_file_path,
        &cfg_file_path,
        &input_file_path,
    ];

    let mut detector = FileChangeDetector::load(cons.clone(), detector_target_file_paths)?;
    let mut loop_count = 0;

    parse(cons, fcpeg_file_path.clone(), input_file_path.clone(), output_tree, count_duration, disable_opt);

    loop {
        match quit_limit_sec {
            Some(v) => {
                if loop_count + 1 * interval_sec >= v {
                    break;
                }
            },
            None => (),
        }

        if detector.is_changed()? {
            parse(cons, fcpeg_file_path.clone(), input_file_path.clone(), output_tree, count_duration, disable_opt);
        }

        loop_count += 1;
        sleep(Duration::from_millis(1000));
    }

    return Ok(());
}

struct FileChangeDetector {
    cons: Rc<RefCell<Console>>,
    // note: <file_path, last_modified>
    file_map: HashMap<String, SystemTime>,
}

impl FileChangeDetector {
    pub fn load(cons: Rc<RefCell<Console>>, target_file_paths: Vec<&String>) -> ConsoleResult<FileChangeDetector> {
        let mut file_map = HashMap::<String, SystemTime>::new();

        for each_path in target_file_paths {
            let last_modified = match FileMan::get_last_modified(each_path) {
                Ok(v) => v,
                Err(e) => {
                    cons.borrow_mut().append_log(e.get_log());
                    return Err(());
                }
            };

            file_map.insert(each_path.clone(), last_modified);
        }

        let detector = FileChangeDetector {
            cons: cons,
            file_map: file_map,
        };

        return Ok(detector);
    }

    pub fn is_changed(&mut self) -> ConsoleResult<bool> {
        for (each_path, each_time) in self.file_map.clone() {
            let new_time = match FileMan::get_last_modified(&each_path) {
                Ok(v) => v,
                Err(e) => {
                    self.cons.borrow_mut().append_log(e.get_log());
                    return Err(());
                }
            };

            if each_time != new_time {
                self.file_map.insert(each_path.to_string(), new_time);
                return Ok(true);
            }
        }

        return Ok(false);
    }
}
