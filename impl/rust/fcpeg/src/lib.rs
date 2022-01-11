pub mod block;
pub mod config;
pub mod file;
pub mod parser;
pub mod rule;
pub mod tree;

use std::collections::*;
use std::result::*;

use crate::block::*;
use crate::file::*;
use crate::parser::*;
use crate::tree::*;

use rustnutlib::console::*;
use rustnutlib::file::*;

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

pub struct FCPEGParser {
    syntax_parser: SyntaxParser,
}

impl FCPEGParser {
    pub fn load(fcpeg_file_path: String, lib_fcpeg_file_map: HashMap<String, String>) -> FCPEGResult<FCPEGParser> {
        let mut fcpeg_file_map = match FCPEGFileMap::load(fcpeg_file_path, lib_fcpeg_file_map) {
            Ok(v) => v,
            Err(e) => return Err(FCPEGError::FCPEGFileError { err: e }),
        };

        let rule_map = match BlockParser::get_rule_map(&mut fcpeg_file_map) {
            Ok(v) => v,
            Err(e) => return Err(FCPEGError::SyntaxParseError { err: e }),
        };

        if cfg!(debug) {
            println!("--- rule map ---");
            println!("{}", rule_map);
        }

        let syntax_parser = match SyntaxParser::new(rule_map) {
            Err(e) => return Err(FCPEGError::SyntaxParseError { err: e }),
            Ok(v) => v,
        };

        let parser = FCPEGParser {
            syntax_parser: syntax_parser,
        };

        return Ok(parser);
    }

    pub fn parse(&mut self, input_file_path: String) -> FCPEGResult<SyntaxTree> {
        let input_file_content = match FileMan::read_all(&input_file_path) {
            Ok(v) => Box::new(v),
            Err(e) => return Err(FCPEGError::FCPEGFileError {
                err: FCPEGFileError::FileError { err: e },
            }),
        };

        let tree = match self.syntax_parser.parse(input_file_path, &input_file_content) {
            Err(e) => return Err(FCPEGError::SyntaxParseError { err: e }),
            Ok(v) => v,
        };

        return Ok(tree);
    }
}
