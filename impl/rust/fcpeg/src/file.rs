use std::cell::RefCell;
use std::collections::*;
use std::rc::Rc;
use std::sync::Arc;

use crate::config::*;

use rustnutlib::console::*;
use rustnutlib::file::*;

pub struct FCPEGFileMap {
    pub file_map: HashMap<String, FCPEGFile>,
    // spec: メインファイルを参照するエイリアス名; ID 変換時にエイリアスを空文字に置換する
    pub cross_refered_alias_names: Arc<Vec<String>>,
}

impl FCPEGFileMap {
    // todo: config 読んでサブファイル対応
    pub fn load(cons: Rc<RefCell<Console>>, fcpeg_file_path: String, lib_fcpeg_file_map: HashMap<String, String>) -> ConsoleResult<FCPEGFileMap> {
        // note: ルートファイルのエイリアス名は空文字; 除外エイリアスなし
        let (file_map, cross_refered_alias_names) = FCPEGFileLoader::load(cons, fcpeg_file_path, lib_fcpeg_file_map)?;

        let file_map_wrapper = FCPEGFileMap {
            cross_refered_alias_names: Arc::new(cross_refered_alias_names),
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

struct FCPEGFileLoader {
    cons: Rc<RefCell<Console>>,
    file_map_result: HashMap<String, FCPEGFile>,
    loaded_fcpeg_file_paths: Vec<String>,
    loaded_file_aliases: Vec<String>,
    cross_refered_alias_names: Vec<String>,
}

impl FCPEGFileLoader {
    pub fn load(cons: Rc<RefCell<Console>>, fcpeg_file_path: String, lib_fcpeg_file_map: HashMap<String, String>) -> ConsoleResult<(HashMap<String, FCPEGFile>, Vec<String>)> {
        let mut loader = FCPEGFileLoader {
            cons: cons,
            file_map_result: HashMap::new(),
            loaded_fcpeg_file_paths: Vec::new(),
            loaded_file_aliases: Vec::new(),
            cross_refered_alias_names: Vec::new(),
        };

        // note: メインファイルのエイリアス名は空文字
        loader.load_file(String::new(), fcpeg_file_path)?;

        for (each_alias_name, each_fcpeg_file_path) in lib_fcpeg_file_map {
            loader.load_file(each_alias_name, each_fcpeg_file_path)?;
        }

        return Ok((loader.file_map_result, loader.cross_refered_alias_names));
    }

    // ret: サブファイルのマップ
    fn load_file(&mut self, alias_name: String, fcpeg_file_path: String) -> ConsoleResult<()> {
        let file_content = match FileMan::read_all(&fcpeg_file_path) {
            Ok(v) => v,
            Err(e) => {
                self.cons.borrow_mut().append_log(e.get_log());
                return Err(());
            },
        };

        let config_file_path = FileMan::rename_ext(&fcpeg_file_path, "cfg");
        let config = Configuration::load(self.cons.clone(), &config_file_path)?;
        let sub_file_alias_map = config.file_alias_map.clone();

        let new_file = FCPEGFile {
            alias_name: alias_name.clone(),
            file_path: fcpeg_file_path.clone(),
            file_content: Box::new(file_content),
            config: config,
        };

        self.loaded_file_aliases.push(alias_name.clone());
        self.file_map_result.insert(alias_name.clone(), new_file);
        // note: 無限再帰防止; 現在ロード中のエイリアスをロード対象から除外する
        self.loaded_fcpeg_file_paths.push(fcpeg_file_path.clone());

        'map_loop: for (sub_alias_name, sub_file_path) in sub_file_alias_map {
            if self.loaded_file_aliases.contains(&sub_alias_name) {
                self.cons.borrow_mut().append_log(ConfigurationLog::DuplicateFileAliasName {
                    alias_name: sub_alias_name.clone(),
                }.get_log());

                return Err(());
            }

            // note: ロード済みであれば無視
            for each_path in &*self.loaded_fcpeg_file_paths {
                match FileMan::is_same(each_path, &sub_file_path) {
                    Ok(is_same_path) => {
                        if is_same_path {
                            self.cross_refered_alias_names.push(sub_alias_name.clone());
                            continue 'map_loop;
                        }
                    },
                    Err(e) => {
                        self.cons.borrow_mut().append_log(e.get_log());
                        return Err(());
                    },
                }
            }

            self.load_file(sub_alias_name, sub_file_path)?;
        }

        return Ok(());
    }
}
