use std::cell::RefCell;
use std::collections::*;
use std::rc::Rc;

use crate::config::*;

use rustnutlib::console::*;
use rustnutlib::file::*;

pub struct FCPEGFileMap {
    pub file_map: HashMap<String, FCPEGFile>,
}

impl FCPEGFileMap {
    // todo: config 読んでサブファイル対応
    pub fn load(cons: Rc<RefCell<Console>>,fcpeg_file_path: String, lib_fcpeg_file_map: HashMap<String, String>) -> ConsoleResult<FCPEGFileMap> {
        // note: ルートファイルのエイリアス名は空文字; 除外エイリアスなし
        let file_map = FCPEGFile::load(cons, String::new(), fcpeg_file_path, lib_fcpeg_file_map, &mut Vec::new())?;

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
    pub fn load(cons: Rc<RefCell<Console>>, alias_name: String, file_path: String, lib_fcpeg_file_map: HashMap<String, String>, filtered_alias_name: &mut Vec<String>) -> ConsoleResult<HashMap<String, FCPEGFile>> {
        let file_content = match FileMan::read_all(&file_path) {
            Ok(v) => v,
            Err(e) => {
                cons.borrow_mut().append_log(e.get_log());
                return Err(());
            },
        };

        let config_file_path = FileMan::rename_ext(&file_path, "cfg");
        let config = Configuration::load(cons.clone(), &config_file_path)?;

        if cfg!(debug) {
            config.print();
        }

        let mut files = HashMap::<String, FCPEGFile>::new();

        // note: 無限再帰防止; 現在ロード中のエイリアスをロード対象から除外する
        filtered_alias_name.push(alias_name.clone());

        for (alias_name, file_path) in lib_fcpeg_file_map {
            let sub_file = FCPEGFile::load(cons.clone(), alias_name, file_path, HashMap::new(), filtered_alias_name)?;

            for (v1, v2) in sub_file {
                files.insert(v1, v2);
            }
        }

        for (sub_alias_name, sub_file_path) in &config.file_alias_map {
            let sub_file = FCPEGFile::load(cons.clone(), sub_alias_name.clone(), sub_file_path.clone(), HashMap::new(), filtered_alias_name)?;

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
