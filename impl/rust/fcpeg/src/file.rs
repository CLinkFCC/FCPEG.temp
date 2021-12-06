use std::collections::*;

use crate::config::*;

use rustnutlib::*;
use rustnutlib::console::*;
use rustnutlib::file::*;

pub type FCPEGFileResult<T> = Result<T, FCPEGFileError>;

pub enum FCPEGFileError {
    Unknown {},
    FileError { err: FileError },
    ConfigurationError { err: ConfigurationError },
}

impl ConsoleLogger for FCPEGFileError {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            FCPEGFileError::Unknown {} => log!(Error, "unknown"),
            FCPEGFileError::FileError { err } => err.get_log(),
            FCPEGFileError::ConfigurationError { err } => err.get_log(),
        };
    }
}

pub struct FCPEGFileMap {
    pub file_map: HashMap<String, FCPEGFile>,
}

impl FCPEGFileMap {
    // todo: config 読んでサブファイル対応
    pub fn load(fcpeg_file_path: String, lib_fcpeg_file_map: HashMap<String, String>) -> FCPEGFileResult<FCPEGFileMap> {
        // note: ルートファイルのエイリアス名は空文字; 除外エイリアスなし
        let file_map = match FCPEGFile::load(String::new(), fcpeg_file_path, lib_fcpeg_file_map, &mut vec![]) {
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
    pub alias_name: String,
    pub file_path: String,
    pub file_content: Box<String>,
    pub config: Configuration,
}

impl FCPEGFile {
    // ret: サブファイルのマップ
    pub fn load(alias_name: String, file_path: String, lib_fcpeg_file_map: HashMap<String, String>, filtered_alias_name: &mut Vec<String>) -> FCPEGFileResult<HashMap<String, FCPEGFile>> {
        let file_content = match FileMan::read_all(&file_path) {
            Err(e) => return Err(FCPEGFileError::FileError { err: e }),
            Ok(v) => v,
        };

        let config_file_path = FileMan::rename_ext(&file_path, "cfg");

        let config = match Configuration::load(&config_file_path) {
            Ok(v) => v,
            Err(e) => return Err(FCPEGFileError::ConfigurationError { err: e }),
        };

        if cfg!(debug) {
            config.print();
        }

        let mut files = HashMap::<String, FCPEGFile>::new();

        // note: 無限再帰防止; 現在ロード中のエイリアスをロード対象から除外する
        filtered_alias_name.push(alias_name.clone());

        for (alias_name, file_path) in lib_fcpeg_file_map {
            let sub_file = FCPEGFile::load(alias_name, file_path, HashMap::new(), filtered_alias_name)?;

            for (v1, v2) in sub_file {
                files.insert(v1, v2);
            }
        }

        for (sub_alias_name, sub_file_path) in &config.file_alias_map {
            let sub_file = FCPEGFile::load(sub_alias_name.clone(), sub_file_path.clone(), HashMap::new(), filtered_alias_name)?;

            for (v1, v2) in sub_file {
                files.insert(v1, v2);
            }
        }

        let root_file = FCPEGFile {
            alias_name: alias_name.clone(),
            file_path: file_path,
            file_content: Box::new(file_content),
            config: config,
        };

        files.insert(alias_name.clone(), root_file);
        return Ok(files);
    }
}
