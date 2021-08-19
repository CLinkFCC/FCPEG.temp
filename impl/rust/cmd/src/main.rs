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
    println!("args");
    for (each_arg_key, each_arg_value) in &cmd_args {
        println!("{}: {}", each_arg_key, each_arg_value.join(" | "));
    }
    println!();

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

    let is_monitored = cmd_args.contains_key("--monitor") || cmd_args.contains_key("-mon");

    if is_monitored {
        parse_with_monitoring(fcpeg_file_path, fcpeg_src_paths, cons);
    } else {
        parse(fcpeg_file_path, fcpeg_src_paths, cons);
    }
}

fn parse(fcpeg_file_path: String, fcpeg_src_paths: Vec<String>, cons: &mut console::Console) {
    // C:\Users\Garnet3106\Desktop\Media\Docs\Repos\FunCobal-family\fcpeg\impl\rust_tester\src\syntax.fcpeg
    // C:\Users\Garnet3106\Desktop\Media\Docs\Repos\FunCobal-family\fcpeg\impl\rust_tester\src\source.ches

    let _trees = match FCPEG::parse_from_paths(fcpeg_file_path.to_string(), fcpeg_src_paths) {
        Err(e) => {
            cons.log(e.get_console_data(), false);
            return;
        },
        Ok(v) => v,
    };

    println!();
    println!("--- end ---");
}

fn parse_with_monitoring(fcpeg_file_path: String, fcpeg_src_paths: Vec<String>, cons: &mut console::Console) {
    cons.log(console::ConsoleLogData::new(console::ConsoleLogKind::Notice, "command help", vec!["You can quit parsing with '^C'.".to_string()], vec![]), false);

    loop {
        parse(fcpeg_file_path.to_string(), fcpeg_src_paths.clone(), cons);
        std::thread::sleep(std::time::Duration::from_millis(1000));
    }
}
