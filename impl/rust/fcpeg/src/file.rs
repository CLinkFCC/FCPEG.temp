use std::collections::*;

use crate::config::*;

use rustnutlib::*;
use rustnutlib::console::*;
use rustnutlib::fileman::*;

pub type FCPEGFileResult<T> = Result<T, FCPEGFileError>;

pub enum FCPEGFileError {
    Unknown {},
    FileError { err: FileManError },
    ConfigFileError { err: ConfigFileError },
}

impl ConsoleLogger for FCPEGFileError {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            FCPEGFileError::Unknown {} => log!(Error, "unknown"),
            FCPEGFileError::FileError { err } => err.get_log_data(),
            FCPEGFileError::ConfigFileError { err } => err.get_log(),
        };
    }
}

pub struct FCPEGFileMap {
    file_map: HashMap<String, FCPEGFile>,
}

impl FCPEGFileMap {
    pub fn load(main_file_path: String) -> FCPEGFileResult<FCPEGFileMap> {
        // note: ルートファイルのエイリアス名は空文字; 除外エイリアスなし
        let file_map = match FCPEGFile::load(String::new(), main_file_path, &mut vec![]) {
            Ok(v) => v,
            Err(e) => return Err(e),
        };

        let file_map_wrapper = FCPEGFileMap {
            file_map: file_map,
        };

        return Ok(file_map_wrapper);
    }

    pub fn iter(&self) -> hash_map::Iter<String, FCPEGFile> {
        return self.file_map.iter();
    }
}

pub struct FCPEGFile {
    // note: ルートのエイリアス名は空文字
    alias_name: String,
    file_path: String,
    file_content: String,
    config_file: ConfigFile,
}

impl FCPEGFile {
    pub fn load(alias_name: String, file_path: String, filtered_alias_name: &mut Vec<String>) -> FCPEGFileResult<HashMap<String, FCPEGFile>> {
        let file_content = match FileMan::read_all(&file_path) {
            Err(e) => return Err(FCPEGFileError::FileError { err: e }),
            Ok(v) => v,
        };

        let mut config_file = ConfigFile::new();
        let config_file_path = FileMan::rename_ext(&file_path, "cfg");

        match config_file.load(config_file_path) {
            Err(e) => return Err(FCPEGFileError::ConfigFileError { err: e }),
            Ok(()) => (),
        }

        if cfg!(release) {
            config_file.print();
        }

        let mut files = HashMap::<String, FCPEGFile>::new();

        // note: 無限再帰防止; 現在ロード中のエイリアスをロード対象から除外する
        filtered_alias_name.push(alias_name.clone());

        for (sub_alias_name, sub_file_path) in config_file.file_alias_map.clone() {
            let sub_file = FCPEGFile::load(sub_alias_name, sub_file_path, filtered_alias_name)?;

            for (v1, v2) in sub_file {
                files.insert(v1, v2);
            }
        }

        let root_file = FCPEGFile {
            alias_name: alias_name.clone(),
            file_path: file_path,
            file_content: file_content,
            config_file: config_file,
        };

        files.insert(alias_name.clone(), root_file);
        return Ok(files);
    }

    pub fn get_file_path(&self) -> String {
        return self.file_path.clone();
    }

    pub fn get_file_content(&self) -> &String {
        return &self.file_content;
    }
}
