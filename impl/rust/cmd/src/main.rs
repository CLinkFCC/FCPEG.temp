pub use fcpeg::*;
pub use rustnutlib::*;

type CommandMap = std::collections::HashMap<String, fn(String, std::collections::HashMap::<String, Vec<String>>, &mut console::Console)>;

fn main() {
    run_command();
}

fn run_command() {
    let mut cmd_proc_map = CommandMap::new();
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

    // monitor オプション
    let is_monitored = cmd_args.contains_key("-mon");

    if is_monitored {
        parse_with_monitoring(fcpeg_file_path, fcpeg_src_paths, cons, 1, None);
    } else {
        parse(fcpeg_file_path, fcpeg_src_paths, cons);
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

fn parse(fcpeg_file_path: String, fcpeg_src_paths: Vec<String>, cons: &mut console::Console) {
    let _trees = match FCPEG::parse_from_paths(fcpeg_file_path.to_string(), fcpeg_src_paths) {
        Err(e) => {
            cons.log(e.get_console_data(), false);
            return;
        },
        Ok(v) => v,
    };
}

fn parse_with_monitoring(fcpeg_file_path: String, fcpeg_src_paths: Vec<String>, cons: &mut console::Console, interval_sec: usize, quit_limit_sec: std::option::Option<usize>) {
    cons.log(console::ConsoleLogData::new(console::ConsoleLogKind::Notice, "command help", vec!["You can quit parsing with '^C'.".to_string()], vec![]), false);

    match quit_limit_sec {
        Some(v) => cons.log(console::ConsoleLogData::new(console::ConsoleLogKind::Notice, "command help", vec![format!("This program will automatically quit {} sec(s) later.", interval_sec * v)], vec![]), false),
        None => (),
    }

    let mut detector_target_file_paths = vec![fcpeg_file_path.clone()];
    detector_target_file_paths.append(&mut fcpeg_src_paths.clone());
    let mut detector = FileChangeDetector::new(detector_target_file_paths);
    let mut loop_count = 0;

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
            parse(fcpeg_file_path.to_string(), fcpeg_src_paths.clone(), cons);
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
