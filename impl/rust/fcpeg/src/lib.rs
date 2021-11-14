pub mod block;
pub mod config;
pub mod data;
pub mod file;
pub mod parser;
pub mod rule;

use std::result::*;

use crate::block::*;
use crate::data::*;
use crate::file::*;
use crate::parser::*;

use rustnutlib::console::*;
use rustnutlib::fileman::*;

pub type FCPEGResult<T> = Result<T, FCPEGError>;

pub enum FCPEGError {
    BlockParseErr(BlockParseError),
    FCPEGFileErr(FCPEGFileError),
    SyntaxParseErr(SyntaxParseError),
}

impl ConsoleLogger for FCPEGError {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            FCPEGError::BlockParseErr(e) => e.get_log(),
            FCPEGError::FCPEGFileErr(e) => e.get_log(),
            FCPEGError::SyntaxParseErr(e) => e.get_log(),
        };
    }
}

pub struct FCPEG {}

impl FCPEG {
    pub fn parse_from_paths(fcpeg_file_path: &String, input_file_paths: &Vec<String>) -> FCPEGResult<Vec<SyntaxTree>> {
        let mut input_srcs = Vec::<String>::new();

        for each_path in input_file_paths {
            let new_src = match FileMan::read_all(each_path) {
                Err(e) => return Err(FCPEGError::FCPEGFileErr(FCPEGFileError::FileErr(e))),
                Ok(v) => v,
            };

            input_srcs.push(new_src);
        }

        return FCPEG::parse_from_srcs(fcpeg_file_path.clone(), input_srcs);
    }

    pub fn parse_from_srcs(fcpeg_file_path: String, input_srcs: Vec<String>) -> FCPEGResult<Vec<SyntaxTree>> {
        let mut fcpeg_file_map = match FCPEGFileMap::load(fcpeg_file_path) {
            Ok(v) => v,
            Err(e) => return Err(FCPEGError::FCPEGFileErr(e)),
        };

        let rule_map = match BlockParser::get_rule_map(&mut fcpeg_file_map) {
            Ok(v) => v,
            Err(e) => return Err(FCPEGError::SyntaxParseErr(e)),
        };

        if cfg!(release) {
            println!("--- rule map ---");
            println!("{}", rule_map);
        }

        println!("rule map {}", rule_map);

        let mut parser = match SyntaxParser::new(rule_map) {
            Err(e) => return Err(FCPEGError::SyntaxParseErr(e)),
            Ok(v) => v,
        };

        let mut trees = Vec::<SyntaxTree>::new();

        for each_src in input_srcs {
            let new_tree = match parser.get_syntax_tree(&each_src) {
                Err(e) => return Err(FCPEGError::SyntaxParseErr(e)),
                Ok(v) => v,
            };

            trees.push(new_tree);
        }

        return Ok(trees);
    }
}
