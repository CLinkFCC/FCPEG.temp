use std::collections::*;

// use crate::blocklexer::*;
use crate::config::*;
use crate::blockparser::*;
use crate::data::*;

use rustnutlib::console::*;
use rustnutlib::fileman::*;

pub enum FCPEGFileManError {
    Unknown(),
    FileManError(FileManError),
    ConfigFileError(ConfigFileError),
}

impl FCPEGFileManError {
    pub fn get_log_data(&self) -> ConsoleLogData {
        match self {
            FCPEGFileManError::Unknown() => ConsoleLogData::new(ConsoleLogKind::Error, "unknown", vec![], vec![]),
            FCPEGFileManError::FileManError(err) => err.get_log_data(),
            FCPEGFileManError::ConfigFileError(err) => err.get_log_data(),
        }
    }
}

pub struct FCPEGFileMan {
    is_loaded: bool,
    // ルートのエイリアス名は空文字
    pub file_alias_name: String,
    pub sub_file_aliase_map: HashMap<String, FCPEGFileMan>,
    config_file: ConfigFile,
    pub fcpeg_file_path: String,
    pub fcpeg_file_content: String,
    pub block_map: HashMap<String, Block>,
}

impl FCPEGFileMan {
    pub fn new(file_alias_name: String, fcpeg_file_path: String) -> FCPEGFileMan {
        return FCPEGFileMan {
            is_loaded: false,
            file_alias_name: file_alias_name,
            sub_file_aliase_map: HashMap::new(),
            config_file: ConfigFile::new(),
            fcpeg_file_path: fcpeg_file_path,
            fcpeg_file_content: String::new(),
            block_map: HashMap::new(),
        }
    }

    fn add_sub_file_alias(&mut self, file_alias_name: String, file_path: String) -> Result<(), FCPEGFileManError> {
        let mut alias = FCPEGFileMan::new(file_alias_name.clone(), file_path);
        alias.load()?;
        self.sub_file_aliase_map.insert(file_alias_name.clone(), alias);

        return Ok(());
    }

    pub fn load(&mut self) -> Result<(), FCPEGFileManError> {
        self.fcpeg_file_content = match FileMan::read_all(&self.fcpeg_file_path) {
            Err(e) => return Err(FCPEGFileManError::FileManError(e)),
            Ok(v) => v,
        };

        let config_file_path = FileMan::rename_ext(&self.fcpeg_file_path, "cfg");

        match self.config_file.load(config_file_path) {
            Err(e) => return Err(FCPEGFileManError::ConfigFileError(e)),
            Ok(()) => (),
        }

        self.is_loaded = true;

        if cfg!(release) {
            self.config_file.print();
        }

        for (alias_name, alias_path) in self.config_file.file_alias_map.clone() {
            match self.add_sub_file_alias(alias_name, alias_path) {
                Err(e) => return Err(e),
                Ok(()) => (),
            };
        }

        return Ok(());
    }

    pub fn parse_all(&mut self) -> Result<(), BlockParseError> {
        if !self.is_loaded {
            return Err(BlockParseError::InternalErr(format!("file manager not loaded (path: '{}')", self.fcpeg_file_path)));
        }

        // let mut block_parser = BlockParser::new();
        // self.block_map = match BlockParserA::get_rule_map(&mut self) {
        //     Ok(v) => v,
        //     Err(_) => panic!(),
        // };
        // let tokens = BlockLexer::get_tokens(&self.fcpeg_file_content)?;

        if cfg!(release) {
            self.print_parsing_info();
        }

        for alias_item in self.sub_file_aliase_map.values_mut() {
            alias_item.parse_all()?;
        }

        return Ok(());
    }

    pub fn print_parsing_info(&self) {
        println!();
        println!("parsing: {}", self.fcpeg_file_path);
        println!();

        for (block_name, each_block) in &self.block_map {
            println!("block {}", block_name);

            for cmd in &each_block.cmds {
                println!("\t{}", cmd);
            }
        }

        println!();
    }
}
