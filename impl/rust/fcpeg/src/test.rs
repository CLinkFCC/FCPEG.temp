#![allow(dead_code)]

use {
    std::{
        collections::HashMap,
        time::{
            Duration,
            SystemTime
        },
        thread::sleep,
    },

    cons_util::file::*,
};

struct FileChangeDetector<'a> {
    callback: Box<dyn Fn()>,
    interval: u64,
    is_stopped: bool,
    file_map: HashMap<&'a FilePath, SystemTime>,
}

impl<'a> FileChangeDetector<'a> {
    pub fn new(callback: Box<dyn Fn()>, interval: u64, tar_file_paths: &'a Vec<FilePath>) -> FileChangeDetector<'a> {
        let file_map = tar_file_paths.iter().map(move |each_path| {
            let last_modified = FileChangeDetector::get_last_modified(&each_path);
            (each_path, last_modified)
        }).collect::<HashMap<&FilePath, SystemTime>>();

        return FileChangeDetector {
            callback: callback,
            interval: interval,
            is_stopped: false,
            file_map: file_map,
        };
    }

    fn get_last_modified(path: &FilePath) -> SystemTime {
        return path.last_modified().expect("Failed to get metadata.");
    }

    fn is_changed(&mut self) -> bool {
        let mut is_changed = false;
        let mut changed_file_map = HashMap::<&FilePath, SystemTime>::new();

        for (each_path, old_time) in &self.file_map {
            let new_time = FileChangeDetector::get_last_modified(&each_path);

            if *old_time != new_time {
                changed_file_map.insert(each_path, new_time);
                is_changed = true;
            }
        }

        for (each_path, each_time) in changed_file_map {
            self.file_map.insert(each_path, each_time);
        }

        return is_changed;
    }

    pub async fn run(&mut self) {
        self.is_stopped = false;

        // Run for the first time.
        (*self.callback)();

        while !self.is_stopped {
            if self.is_changed() {
                (*self.callback)();
            }

            sleep(Duration::from_millis(self.interval));
        }
    }

    pub fn stop(&mut self) {
        self.is_stopped = true;
    }
}

#[tokio::test]
async fn test() {
    let file_paths = vec![
        FilePath::new("src/fcpeg/syntax.fcpil".to_string())
    ];

    let mut detector = FileChangeDetector::new(Box::new(|| parse_fcpil()), 200, &file_paths);
    detector.run().await;
}

fn parse_fcpil() {
    use crate::*;
    use crate::il::FcpilParser;
    use cons_util::*;

    let cons = &mut Console::new("ja".to_string(), ConsoleLogLimit::NoLimit);

    let src = match MultilineSource::file(FilePath::new("src/fcpeg/syntax.fcpil".to_string())).consume(cons) {
        Ok(v) => v,
        Err(_) => {
            cons.output(Vec::new());
            return;
        },
    };

    let rule_map = match FcpilParser::parse(src).consume(cons) {
        Ok(v) => v,
        Err(_) => {
            cons.output(Vec::new());
            return;
        },
    };

    cons.output(Vec::new());

    for each_rule in rule_map.0.values() {
        println!("{}", each_rule.to_fcpil());
    }
}
