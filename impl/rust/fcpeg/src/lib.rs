pub mod block;
pub mod config;
pub mod data;
pub mod file;
pub mod parser;
pub mod rule;

use std::collections::*;
use std::result::*;

use crate::block::*;
use crate::data::*;
use crate::file::*;
use crate::parser::*;

use rustnutlib::console::*;
use rustnutlib::fileman::*;

pub type FCPEGResult<T> = Result<T, FCPEGError>;

pub enum FCPEGError {
    BlockParseError { err: BlockParseError },
    FCPEGFileError { err: FCPEGFileError },
    SyntaxParseError { err: SyntaxParseError },
}

impl ConsoleLogger for FCPEGError {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            FCPEGError::BlockParseError { err } => err.get_log(),
            FCPEGError::FCPEGFileError { err } => err.get_log(),
            FCPEGError::SyntaxParseError { err } => err.get_log(),
        };
    }
}

pub struct FCPEG {}

impl FCPEG {
    pub fn parse_from_paths(fcpeg_file_path: &String, input_file_paths: &Vec<String>) -> FCPEGResult<Vec<SyntaxTree>> {
        let mut input_srcs = HashMap::<String, String>::new();

        for each_path in input_file_paths {
            let new_src = match FileMan::read_all(each_path) {
                Ok(v) => v,
                Err(e) => return Err(FCPEGError::FCPEGFileError {
                    err: FCPEGFileError::FileError { err: e },
                }),
            };

            input_srcs.insert(each_path.clone(), new_src);
        }

        return FCPEG::parse_from_srcs(fcpeg_file_path.clone(), input_srcs);
    }

    pub fn parse_from_srcs(fcpeg_file_path: String, input_srcs: HashMap<String, String>) -> FCPEGResult<Vec<SyntaxTree>> {
        let mut fcpeg_file_map = match FCPEGFileMap::load(fcpeg_file_path.clone()) {
            Ok(v) => v,
            Err(e) => return Err(FCPEGError::FCPEGFileError { err: e }),
        };

        let rule_map = match BlockParser::get_rule_map(&mut fcpeg_file_map) {
            Ok(v) => v,
            Err(e) => return Err(FCPEGError::SyntaxParseError { err: e }),
        };

        if cfg!(release) {
            println!("--- rule map ---");
            println!("{}", rule_map);
        }

        println!("rule map {}", rule_map);

        let mut parser = match SyntaxParser::new(rule_map) {
            Err(e) => return Err(FCPEGError::SyntaxParseError { err: e }),
            Ok(v) => v,
        };

        let mut trees = Vec::<SyntaxTree>::new();

        for (each_src_path, each_src_content) in input_srcs {
            let new_tree = match parser.get_syntax_tree(each_src_path.clone(), &each_src_content) {
                Err(e) => return Err(FCPEGError::SyntaxParseError { err: e }),
                Ok(v) => v,
            };

            trees.push(new_tree);
        }

        return Ok(trees);
    }
}
