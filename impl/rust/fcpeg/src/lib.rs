pub mod block;
pub mod cons;
pub mod il;
pub mod js;
pub mod test;

use crate::block::*;

use std::fmt::{Display, Formatter};

use cons_util::*;
use cons_util::cons::*;
use cons_util::file::*;

use num_traits::Num;

#[derive(Clone, PartialEq)]
pub enum Infinitable<T: Clone + Display + Num + PartialEq> {
    Finite(T),
    Infinite,
}

impl<T: Clone + Display + Num + PartialEq> Infinitable<T> {
    pub fn is_infinite(&self) -> bool {
        return *self == Infinitable::<T>::Infinite;
    }
}

impl<T: Clone + Display + Num + PartialEq> Display for Infinitable<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Infinitable::Finite(v) => v.to_string(),
            Infinitable::Infinite => "Infinite".to_string(),
        };

        return write!(f, "{}", s);
    }
}

#[derive(Clone, PartialEq)]
pub enum InputSource {
    Raw { content: String },
    File { path: String, content: String },
}

impl InputSource {
    pub fn raw(content: String) -> InputSource {
        return InputSource::Raw { content: content };
    }

    pub fn file(path: String) -> FileManResult<InputSource> {
        let content = FileMan::read_all(&path)?;
        return Ok(InputSource::File { path: path, content: content });
    }

    pub fn into_content(self) -> String {
        return match self {
            InputSource::Raw { content } => content,
            InputSource::File { path: _, content } => content,
        };
    }
}

#[derive(Clone, PartialEq)]
pub struct CharacterPosition {
    pub file_path: Option<String>,
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl CharacterPosition {
    pub fn new(file_path: Option<String>, index: usize, line: usize, column: usize) -> CharacterPosition {
        return CharacterPosition {
            file_path: file_path,
            index: index,
            line: line,
            column: column,
        };
    }

    pub fn get_empty() -> CharacterPosition {
        return CharacterPosition {
            file_path: None,
            index: 0,
            line: 0,
            column: 0,
        };
    }
}

impl Display for CharacterPosition {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let file_path_text = match self.file_path.clone() {
            Some(path) => format!("{}:", path),
            None => String::new(),
        };

        return write!(f, "{}{}:{}", file_path_text, self.line + 1, self.column + 1);
    }
}

pub struct FcpegParser {
    rule_map: RuleMap,
    enable_memoization: bool,
}

impl FcpegParser {
    pub fn new(rule_map: RuleMap) -> FcpegParser {
        return FcpegParser {
            rule_map: rule_map,
            enable_memoization: true,
        };
    }

    pub fn from_fcpeg(cons: &mut Console, src: InputSource) -> ConsoleResult<FcpegParser> {
        let block_map = BlockMap::from_fcpeg(src);
        let rule_map = RuleMap::from_block_map(cons, block_map);
        return Ok(FcpegParser::new(rule_map));
    }

    pub fn from_fcpil(cons: &mut Console, src: InputSource) -> ConsoleResult<FcpegParser> {
        let content = src.into_content();
        let lines = content.split('\n').collect::<Vec<&str>>();
        let rule_map = RuleMap::from_fcpil(cons, lines)?;
        return Ok(FcpegParser::new(rule_map));
    }

    pub fn disable_memoization(mut self) -> FcpegParser {
        self.enable_memoization = false;
        return self;
    }

    pub fn parse(&self, cons: &mut Console, input: InputSource) {
        unimplemented!();
        // return SyntaxParser::parse(cons, &self.rule_map, input, self.enable_memoization);
    }
}
