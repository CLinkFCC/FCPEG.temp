pub mod data;
pub mod blockparser;
pub mod parser;
pub mod rule;
pub mod setting;

pub use rustnutlib::*;

pub enum FCPEGError {
    BlockParseErr(blockparser::BlockParseError),
    FCPEGFileManErr(blockparser::FCPEGFileManError),
    SyntaxParseErr(parser::SyntaxParseError),
}

impl FCPEGError {
    pub fn get_console_data(&self) -> console::ConsoleLogData {
        return match self {
            FCPEGError::BlockParseErr(e) => e.get_log_data(),
            FCPEGError::FCPEGFileManErr(e) => e.get_log_data(),
            FCPEGError::SyntaxParseErr(e) => e.get_log_data(),
        };
    }
}

pub struct FCPEG {}

impl FCPEG {
    pub fn parse_from_paths(fcpeg_file_path: &String, input_file_paths: &Vec<String>) -> std::result::Result<Vec<data::SyntaxTree>, FCPEGError> {
        let mut input_srcs = Vec::<String>::new();

        for each_path in input_file_paths {
            let new_src = match fileman::FileMan::read_all(each_path) {
                Err(e) => return Err(FCPEGError::FCPEGFileManErr(blockparser::FCPEGFileManError::FileManError(e))),
                Ok(v) => v,
            };

            input_srcs.push(new_src);
        }

        return FCPEG::parse_from_srcs(fcpeg_file_path.to_string(), input_srcs);
    }

    pub fn parse_from_srcs(fcpeg_file_path: String, input_srcs: Vec<String>) -> std::result::Result<Vec<data::SyntaxTree>, FCPEGError> {
        // ルートファイルのエイリアス名は空文字
        let mut fcpeg_file_man = blockparser::FCPEGFileMan::new("".to_string(), fcpeg_file_path);

        match fcpeg_file_man.load() {
            Err(e) => return Err(FCPEGError::FCPEGFileManErr(e)),
            Ok(()) => (),
        };

        match fcpeg_file_man.parse_all() {
            Err(e) => return Err(FCPEGError::BlockParseErr(e)),
            Ok(()) => (),
        };

        let rule_map = match rule::RuleMap::get_from_root_fcpeg_file_man(fcpeg_file_man) {
            Err(e) => return Err(FCPEGError::BlockParseErr(e)),
            Ok(v) => v,
        };

        if !cfg!(debug) {
            println!("--- rule map ---");
            println!("{}", rule_map);
        }

        let mut parser = match parser::SyntaxParser::new(rule_map) {
            Err(e) => return Err(FCPEGError::SyntaxParseErr(e)),
            Ok(v) => v,
        };

        let mut trees = Vec::<data::SyntaxTree>::new();

        for mut each_src in input_srcs {
            // todo: 高速化: replace() と比べてどちらが速いか検証する
            // 余分な改行コード 0x0d を排除する
            loop {
                match each_src.find(0x0d as char) {
                    Some(v) => {
                        each_src.remove(v);
                    },
                    None => break,
                }
            }

            let new_tree = match parser.get_syntax_tree(each_src) {
                Err(e) => return Err(FCPEGError::SyntaxParseErr(e)),
                Ok(v) => v,
            };

            trees.push(new_tree);
        }

        return Ok(trees);
    }
}
