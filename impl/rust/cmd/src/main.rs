pub use fcpeg::*;
pub use rustnutlib::*;

fn main() {
    run_command();
}

fn run_command() {
    let mut cmd_proc_map = cmd::CommandMap::new();
    cmd_proc_map.insert("parse".to_string(), cmd_parse);
    cmd::run_command("parse", cmd_proc_map);
}

fn cmd_parse(_cmd_name: String, cmd_args: std::collections::HashMap::<String, Vec<String>>, cons: &mut console::Console) {
    if cmd_args.get("-h").is_some() {
        show_parse_cmd_help(cons);
        return;
    }

    let fcpeg_file_path = match cmd_args.get("-f") {
        Some(v) => {
            if v.len() != 1 {
                cons.log(console::ConsoleLogData::new(console::ConsoleLogKind::Error, "too few/many arguments with '-f'", vec![], vec![]), false);
                return;
            }

            v.get(0).unwrap().to_string()
        },
        None => {
            cons.log(console::ConsoleLogData::new(console::ConsoleLogKind::Error, "command argument '-f' not found", vec![], vec![]), false);
            return;
        },
    };

    let fcpeg_src_paths = match cmd_args.get("-i") {
        Some(v) => v.clone(),
        None => vec![],
    };

    // output オプション
    let output_tree = cmd_args.contains_key("-o");
    // monitor オプション
    let is_monitored = cmd_args.contains_key("-mon");
    // time オプション
    let count_duration = cmd_args.contains_key("-t");

    if is_monitored {
        parse_with_monitoring(&fcpeg_file_path, &fcpeg_src_paths, cons, 1, None, output_tree, count_duration);
    } else {
        parse(&fcpeg_file_path, &fcpeg_src_paths, cons, output_tree, count_duration);
    }
}

fn show_parse_cmd_help(cons: &mut console::Console) {
    cons.log(console::ConsoleLogData::new(console::ConsoleLogKind::Notice, "command help", vec![
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
        trees[i].print();
        println!();
    }
}

fn parse(fcpeg_file_path: &String, fcpeg_src_paths: &Vec<String>, cons: &mut console::Console, output_tree: bool, count_duration: bool) {
    let start_count = std::time::Instant::now();

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

fn parse_with_monitoring(fcpeg_file_path: &String, fcpeg_src_paths: &Vec<String>, cons: &mut console::Console, interval_sec: usize, quit_limit_sec: std::option::Option<usize>, output_tree: bool, count_duration: bool) {
    cons.log(console::ConsoleLogData::new(console::ConsoleLogKind::Notice, "command help", vec!["You can quit parsing with '^C'.".to_string()], vec![]), false);

    match quit_limit_sec {
        Some(v) => cons.log(console::ConsoleLogData::new(console::ConsoleLogKind::Notice, "command help", vec![format!("This program will automatically quit {} sec(s) later.", interval_sec * v)], vec![]), false),
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
        std::thread::sleep(std::time::Duration::from_millis(1000));
    }
}

struct FileChangeDetector {
    log_map: std::collections::HashMap<String, String>,
    target_file_paths: Vec<String>,
}

impl FileChangeDetector {
    pub fn new(target_file_paths: Vec<String>) -> Self {
        return FileChangeDetector {
            log_map: std::collections::HashMap::new(),
            target_file_paths: target_file_paths,
        };
    }

    fn detect_file_change(&mut self, file_path: &str) -> bool {
        match fileman::FileMan::read_all(file_path) {
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
            if self.detect_file_change(&each_path.to_string()) {
                return true;
            }
        }

        return false;
    }
}
