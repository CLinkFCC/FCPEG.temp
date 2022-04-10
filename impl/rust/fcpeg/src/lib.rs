pub mod rule;
pub mod cons;
pub mod il;
pub mod js;
pub mod test;

use crate::il::FcpilParsingResult;
use crate::rule::*;

use std::fmt::{Display, Formatter};

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

pub type GeneralSource = Source<String>;
pub type GeneralSourceRef<'a> = Source<&'a String>;

pub type MultilineSource = Source<Vec<String>>;
pub type MultilineSourceRef<'a> = Source<&'a Vec<String>>;

#[derive(Clone, PartialEq)]
pub enum Source<T: Clone + PartialEq> {
    Raw { content: T },
    File { path: FilePath, content: T },
}

impl GeneralSource {
    pub fn file(path: FilePath) -> FileManResult<GeneralSource> {
        let content = path.read()?;
        return Ok(GeneralSource::File { path: path, content: content });
    }
}

impl MultilineSource {
    pub fn file(path: FilePath) -> FileManResult<MultilineSource> {
        let content = path.read_lines()?;
        return Ok(MultilineSource::File { path: path, content: content });
    }
}

impl<T: Clone + PartialEq> Source<T> {
    pub fn raw(content: T) -> Source<T> {
        return Source::Raw { content: content };
    }

    pub fn is_raw(&self) -> bool {
        return match self {
            Source::<T>::Raw { content: _ } => true,
            _ => false,
        };
    }

    pub fn is_file(&self) -> bool {
        return match self {
            Source::<T>::File { path: _, content: _ } => true,
            _ => false,
        };
    }

    pub fn get_file_path(&self) -> Option<&FilePath> {
        return match self {
            Source::<T>::File { path, content: _ } => Some(&path),
            _ => None,
        };
    }

    pub fn as_content_ref(&self) -> &T {
        return match self {
            Source::<T>::Raw { content } => &content,
            Source::<T>::File { path: _, content } => &content,
        };
    }

    pub fn into_content(self) -> T {
        return match self {
            Source::<T>::Raw { content } => content,
            Source::<T>::File { path: _, content } => content,
        };
    }
}

#[derive(Clone, PartialEq)]
pub enum SourcePosition {
    Line { line: usize },
    Column { index: usize, line: usize, column: usize },
    File { file_path: String },
    FileLine { file_path: String, line: usize },
    FileColumn { file_path: String, index: usize, line: usize, column: usize },
}

impl SourcePosition {
    pub fn column_from<T: Clone + PartialEq>(src: &Source<T>, index: usize, line: usize, column: usize) -> SourcePosition {
        if src.is_raw() {
            return SourcePosition::Column { index: index, line: line, column: column };
        }

        let file_path = src.get_file_path();

        match file_path {
            Some(v) => return SourcePosition::FileColumn { file_path: v.to_string(), index: index, line: line, column: column },
            None => (),
        }

        unreachable!();
    }
}

impl Display for SourcePosition {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let s = match self {
            SourcePosition::Line { line } => format!("<string>:{}", line),
            SourcePosition::Column { index: _, line, column } => format!("<string>:{}:{}", line, column),
            SourcePosition::File { file_path } => format!("{}", file_path),
            SourcePosition::FileLine { file_path, line } => format!("{}:{}", file_path, line),
            SourcePosition::FileColumn { file_path, index: _, line, column } => format!("{}:{}:{}", file_path, line, column),
        };

        return write!(f, "{}", s);
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

    pub fn from_fcpeg(cons: &mut Console, src: GeneralSource) -> ConsoleResult<FcpegParser> {
        let block_map = BlockMap::from_fcpeg(src);
        let rule_map = RuleMap::from_block_map(cons, block_map);
        return Ok(FcpegParser::new(rule_map));
    }

    pub fn from_fcpil(src: MultilineSource) -> FcpilParsingResult<FcpegParser> {
        let rule_map = RuleMap::from_fcpil(src)?;
        return Ok(FcpegParser::new(rule_map));
    }

    pub fn disable_memoization(&mut self) {
        self.enable_memoization = false;
    }

    pub fn parse(&self, _cons: &mut Console, _input: GeneralSource) {
        todo!();
    }
}
