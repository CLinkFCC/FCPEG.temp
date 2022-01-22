use std::cell::RefCell;
use std::collections::*;
use std::fmt::*;
use std::rc::Rc;

use rustnutlib::*;
use rustnutlib::console::*;
use rustnutlib::file::*;

// note: HashMap<name, (values, sub_map)>
type PropertyMap = HashMap<String, (Vec<String>, PropertySubMap)>;
type PropertySubMap = HashMap::<String, Vec<String>>;

pub enum ConfigurationLog {
    DuplicatePropertyName { prop_name: String },
    InvalidPropertyValue { prop_name: String, prop_value: String },
    InvalidPropertyValueLength { prop_name: String },
    InvalidSyntax { line: usize, msg: String },
    UnknownPropertyName { prop_name: String },
    UnknownRegexMode { input: String },
}

impl ConsoleLogger for ConfigurationLog {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            ConfigurationLog::DuplicatePropertyName { prop_name } => log!(Error, "duplicate property name", format!("property name:\t{}", prop_name)),
            ConfigurationLog::InvalidPropertyValue { prop_name, prop_value } => log!(Error, "invalid property value", format!("property name:\t{}", prop_name), format!("property value:\t{}", prop_value)),
            ConfigurationLog::InvalidPropertyValueLength { prop_name } => log!(Error, "invalid property value length", format!("property name:\t{}", prop_name)),
            ConfigurationLog::InvalidSyntax { line, msg } => log!(Error, "invalid syntax", format!("{}", msg), format!("line:\t{}", line)),
            ConfigurationLog::UnknownPropertyName { prop_name } => log!(Error, format!("unknown property name '{}'", prop_name)),
            ConfigurationLog::UnknownRegexMode { input } => log!(Error, format!("unknown regex mode '{}'", input)),
        };
    }
}

#[derive(Clone, PartialEq)]
pub enum RegexMode {
    Default,
    Onise,
    Posix,
}

impl RegexMode {
    fn get_regex_mode(regex_mode_value: &str) -> std::option::Option<RegexMode> {
        return match regex_mode_value.to_lowercase().as_str() {
            "default" => Some(RegexMode::Default),
            "onise" => Some(RegexMode::Onise),
            "posix" => Some(RegexMode::Posix),
            _ => None,
        }
    }
}

impl Display for RegexMode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        return match self {
            RegexMode::Default => write!(f, "Default"),
            RegexMode::Onise => write!(f, "Onise"),
            RegexMode::Posix => write!(f, "POSIX"),
        };
    }
}

pub struct Configuration {
    pub file_alias_map: HashMap<String, String>,
    pub regex_mode: RegexMode,
    pub reverse_ast_reflection: bool,
}

impl Configuration {
    pub fn load(cons: Rc<RefCell<Console>>, file_path: &String) -> ConsoleResult<Configuration> {
        let lines = match FileMan::read_lines(file_path) {
            Err(e) => {
                cons.borrow_mut().append_log(e.get_log());
                return Err(());
            },
            Ok(v) => v,
        };

        let mut file_alias_map = HashMap::<String, String>::new();
        let mut reverse_ast_reflection = false;
        let mut regex_mode = RegexMode::Default;

        let prop_map = Configuration::get_prop_map(cons.clone(), &lines)?;

        for (prop_name, (prop_values, prop_sub_map)) in prop_map.iter() {
            match prop_name.as_str() {
                "Regex" => {
                    // 正規表現モード

                    let regex_mode_str = match prop_values.get(0) {
                        Some(v) => v,
                        None => {
                            cons.borrow_mut().append_log(ConfigurationLog::InvalidPropertyValueLength {
                                prop_name: prop_name.clone(),
                            }.get_log());

                            return Err(());
                        },
                    };

                    regex_mode = match RegexMode::get_regex_mode(regex_mode_str) {
                        Some(v) => v,
                        None => {
                            cons.borrow_mut().append_log(ConfigurationLog::UnknownRegexMode {
                                input: regex_mode_str.clone(),
                            }.get_log());

                            return Err(());
                        },
                    }
                },
                "FileAliases" => {
                    // ファイルエイリアス

                    for (alias_name, alias_path_vec) in prop_sub_map {
                        let alias_path = match alias_path_vec.get(0) {
                            Some(v) => v,
                            None => {
                                cons.borrow_mut().append_log(ConfigurationLog::InvalidPropertyValueLength {
                                    prop_name: alias_name.clone()
                                }.get_log());

                                return Err(());
                            },
                        };

                        file_alias_map.insert(alias_name.clone(), alias_path.clone());
                    }
                },
                "ASTReflect" => {
                    // AST へのノード反映

                    let ast_reflect = match prop_values.get(0) {
                        Some(v) => v,
                        None => {
                            cons.borrow_mut().append_log(ConfigurationLog::InvalidPropertyValueLength {
                                prop_name: prop_name.clone(),
                            }.get_log());

                            return Err(());
                        },
                    };

                    reverse_ast_reflection = match ast_reflect.to_lowercase().as_str() {
                        "normal" => false,
                        "reversed" => true,
                        _ => {
                            cons.borrow_mut().append_log(ConfigurationLog::InvalidPropertyValue {
                                prop_name: prop_name.clone(),
                                prop_value: ast_reflect.clone(),
                            }.get_log());

                            return Err(());
                        },
                    };
                },
                _ => {
                    cons.borrow_mut().append_log(ConfigurationLog::UnknownPropertyName {
                        prop_name: prop_name.clone(),
                    }.get_log());

                    return Err(());
                },
            }
        }

        let config = Configuration {
            file_alias_map: HashMap::new(),
            regex_mode: regex_mode,
            reverse_ast_reflection: reverse_ast_reflection,
        };

        return Ok(config);
    }

    fn get_prop_map(cons: Rc<RefCell<Console>>, lines: &Vec<String>) -> ConsoleResult<PropertyMap> {
        let mut prop_map = PropertyMap::new();

        // ネスト用の一時的なプロップ名
        let mut tmp_prop_name = std::option::Option::<String>::None;
        let mut tmp_prop_sub_map = PropertySubMap::new();

        for (line_i, each_line) in lines.iter().enumerate() {
            if each_line == "" {
                continue;
            }

            let last_char = each_line[each_line.len() - 1..each_line.len()].to_string();

            match last_char.as_str() {
                "," => {
                    let is_nested = each_line.starts_with("||");
                    let pure_line_start_i = if is_nested { "||".len() } else { 0 };

                    // コンマを除いた行
                    let pure_line = each_line[pure_line_start_i..each_line.len() - 1].to_string();
                    // 右辺と左辺
                    let both_sides: Vec<&str> = pure_line.split(": ").collect();

                    if both_sides.len() != 2 {
                        cons.borrow_mut().append_log(ConfigurationLog::InvalidSyntax {
                            line: line_i,
                            msg: "expected ':'".to_string(),
                        }.get_log());

                        return Err(());
                    }

                    let prop_name = both_sides.get(0).unwrap().to_string();

                    if prop_map.contains_key(&prop_name) {
                        cons.borrow_mut().append_log(ConfigurationLog::DuplicatePropertyName {
                            prop_name: prop_name,
                        }.get_log());

                        return Err(());
                    }

                    let prop_values_orig: Vec<&str> = both_sides.get(1).unwrap().split(" ").collect();
                    let prop_values: Vec<String> = prop_values_orig.iter().map(|s| s.to_string()).collect();

                    if prop_values == vec![""] {
                        cons.borrow_mut().append_log(ConfigurationLog::InvalidSyntax {
                            line: line_i,
                            msg: "property value not found".to_string(),
                        }.get_log());

                        return Err(());
                    }

                    if is_nested {
                        if tmp_prop_name.is_none() {
                            cons.borrow_mut().append_log(ConfigurationLog::InvalidSyntax {
                                line: line_i,
                                msg: "unexpected '||'".to_string(),
                            }.get_log());

                            return Err(());
                        }

                        tmp_prop_sub_map.insert(prop_name, prop_values);
                    } else {
                        tmp_prop_name = None;
                        prop_map.insert(prop_name, (prop_values, PropertySubMap::new()));
                    }
                },
                ":" => {
                    match tmp_prop_name {
                        Some(prop_name) => {
                            prop_map.insert(prop_name, (Vec::new(), tmp_prop_sub_map.clone()));
                        }
                        None => (),
                    }

                    // コロンを除いた行
                    let pure_line = each_line[..each_line.len() - 1].to_string();
                    tmp_prop_name = Some(pure_line.clone());
                },
                _ => {
                    cons.borrow_mut().append_log(ConfigurationLog::InvalidSyntax {
                        line: line_i,
                        msg: "expected ',' and ':' at the end".to_string(),
                    }.get_log());

                    return Err(());
                },
            }
        }

        match tmp_prop_name {
            Some(prop_name) => {
                prop_map.insert(prop_name, (Vec::new(), tmp_prop_sub_map));
            }
            None => (),
        }

        return Ok(prop_map);
    }

    pub fn print(&self) {
        println!("-- Config Data --");
        println!();
        println!("regex mode: {}", self.regex_mode);
        println!("file aliases:");

        for (alias_name, alias_path) in self.file_alias_map.iter() {
            println!("\t{}: {}", alias_name, alias_path);
        }
    }
}
