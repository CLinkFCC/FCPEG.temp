pub mod block;
pub mod config;
pub mod cons;
pub mod file;
pub mod parser;
pub mod rule;
pub mod tree;

use std::cell::RefCell;
use std::collections::*;
use std::rc::Rc;
use std::sync::Arc;

use crate::block::*;
use crate::file::*;
use crate::parser::*;
use crate::rule::*;
use crate::tree::*;

use rustnutlib::console::*;
use rustnutlib::file::*;

pub struct FCPEGParser {
    cons: Rc<RefCell<Console>>,
    rule_map: Arc<Box<RuleMap>>,
    enable_memoization: bool,
}

impl FCPEGParser {
    pub fn load(cons: Rc<RefCell<Console>>, fcpeg_file_path: String, lib_fcpeg_file_map: HashMap<String, String>, enable_memoization: bool) -> ConsoleResult<FCPEGParser> {
        let mut fcpeg_file_map = FCPEGFileMap::load(cons.clone(), fcpeg_file_path, lib_fcpeg_file_map)?;
        let rule_map = BlockParser::get_rule_map(cons.clone(), &mut fcpeg_file_map, true)?;

        let parser = FCPEGParser {
            cons: cons,
            rule_map: rule_map,
            enable_memoization: enable_memoization,
        };

        return Ok(parser);
    }

    pub fn parse(&mut self, input_file_path: String) -> ConsoleResult<SyntaxTree> {
        let input_file_content = match FileMan::read_all(&input_file_path) {
            Ok(v) => Box::new(v),
            Err(e) => {
                self.cons.borrow_mut().append_log(e.get_log());
                return Err(());
            },
        };

        let tree = SyntaxParser::parse(self.cons.clone(), self.rule_map.clone(), input_file_path, input_file_content, self.enable_memoization)?;
        return Ok(tree);
    }
}
