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

use cons_util::cons::*;
use cons_util::file::*;

#[derive(Copy)]
pub struct Raw<T> {
    ptr: *mut T,
}

impl<T> Raw<T> {
    pub fn new(v: T) -> Raw<T> {
        return Raw::<T> {
            ptr: Box::into_raw(Box::new(v)),
        };
    }

    pub fn from(v: *mut T) -> Raw<T> {
        return Raw::<T> {
            ptr: v,
        };
    }

    pub fn as_ref(&self) -> &T {
        return unsafe {
            &*self.ptr
        };
    }

    pub fn as_mut_ref(&self) -> &mut T {
        return unsafe {
            &mut *self.ptr
        };
    }

    pub fn borrow(self) -> Box<T> {
        return unsafe {
            Box::from_raw(self.ptr)
        };
    }

    unsafe fn raw_drop(&mut self) {
        drop(Box::from_raw(self.ptr));
    }
}

impl<T> Clone for Raw<T> {
    fn clone(&self) -> Self {
        return Raw::from(self.as_mut_ref() as *mut T);
    }
}

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

        return match SyntaxParser::parse(self.cons.clone(), self.rule_map.clone(), input_file_path, input_file_content, self.enable_memoization) {
            Ok(v) => Ok(v),
            Err(()) => Err(()),
        };
    }
}
