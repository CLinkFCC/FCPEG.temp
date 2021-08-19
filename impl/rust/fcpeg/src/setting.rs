use rustnutlib::*;

// HashMap<name, (values, sub_map)>
type PropertyMap = std::collections::HashMap<String, (Vec<String>, PropertySubMap)>;
type PropertySubMap = std::collections::HashMap::<String, Vec<String>>;

#[derive(Debug)]
pub enum SettingFileError {
    Unknown(),
    DuplicatedPropName(String),
    FileManError(fileman::FileManError),
    InvalidPropValueLength(String),
    InvalidSyntax(usize, String),
    UnknownPropertyName(String),
    UnknownRegexMode(String),
}

impl std::fmt::Display for SettingFileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        return write!(f, "[SettingFileError]");
    }
}

impl SettingFileError {
    pub fn get_log_data(&self) -> console::ConsoleLogData {
        match self {
            SettingFileError::Unknown() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown", vec![], vec![]),
            SettingFileError::DuplicatedPropName(prop_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "duplicated property name", vec![], vec![format!("property name: {}", prop_name)]),
            SettingFileError::FileManError(err) => err.get_log_data(),
            SettingFileError::InvalidPropValueLength(prop_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "invalid property value length", vec![], vec![format!("property name: {}", prop_name)]),
            SettingFileError::InvalidSyntax(line, msg) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "invalid syntax", vec![format!("{}", msg)], vec![format!("line: {}", line)]),
            SettingFileError::UnknownPropertyName(prop_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unknown property name '{}'", prop_name), vec![], vec![]),
            SettingFileError::UnknownRegexMode(input) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown regex mode", vec![], vec![format!("input: {}", input)]),
        }
    }
}

pub enum RegexMode {
    Default,
    Onise,
    Posix,
}

impl std::fmt::Display for RegexMode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        return match self {
            RegexMode::Default => write!(f, "Default"),
            RegexMode::Onise => write!(f, "Onise"),
            RegexMode::Posix => write!(f, "POSIX"),
        };
    }
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

pub struct SettingFile {
    pub file_alias_map: std::collections::HashMap<String, String>,
    pub regex_mode: RegexMode,
}

impl SettingFile {
    pub fn new() -> Self {
        return SettingFile {
            file_alias_map: std::collections::HashMap::new(),
            regex_mode: RegexMode::Default,
        }
    }

    pub fn load(&mut self, src_path: String) -> std::result::Result<(), SettingFileError> {
        let lines = match fileman::FileMan::read_lines(&src_path) {
            Err(e) => return Err(SettingFileError::FileManError(e)),
            Ok(v) => v,
        };

        let prop_map = SettingFile::get_prop_map(&lines)?;

        for (prop_name, (prop_values, prop_sub_map)) in prop_map.iter() {
            match prop_name.as_str() {
                "Regex" => {
                    // 正規表現モード

                    let regex_mode_str = match prop_values.get(0) {
                        Some(v) => v,
                        None => return Err(SettingFileError::InvalidPropValueLength(prop_name.to_string())),
                    };

                    self.regex_mode = match RegexMode::get_regex_mode(regex_mode_str) {
                        Some(v) => v,
                        None => return Err(SettingFileError::UnknownRegexMode(regex_mode_str.to_string())),
                    }
                },
                "FileAliases" => {
                    // ファイルエイリアス

                    for (alias_name, alias_path_vec) in prop_sub_map {
                        let alias_path = match alias_path_vec.get(0) {
                            Some(v) => v,
                            None => return Err(SettingFileError::InvalidPropValueLength(alias_name.to_string())),
                        };

                        self.file_alias_map.insert(alias_name.clone(), alias_path.clone());
                    }
                },
                _ => return Err(SettingFileError::UnknownPropertyName(prop_name.to_string())),
            }
        }

        return Ok(());
    }

    fn get_prop_map(lines: &Vec<String>) -> std::result::Result<PropertyMap, SettingFileError> {
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
                        return Err(SettingFileError::InvalidSyntax(line_i, "expected ':'".to_string()));
                    }

                    let prop_name = both_sides.get(0).unwrap().to_string();

                    if prop_map.contains_key(&prop_name) {
                        return Err(SettingFileError::DuplicatedPropName(prop_name))
                    }

                    let prop_values_orig: Vec<&str> = both_sides.get(1).unwrap().split(" ").collect();
                    let prop_values: Vec<String> = prop_values_orig.iter().map(|s| s.to_string()).collect();

                    if prop_values == vec![""] {
                        return Err(SettingFileError::InvalidSyntax(line_i, "property value not found".to_string()));
                    }

                    if is_nested {
                        if tmp_prop_name.is_none() {
                            return Err(SettingFileError::InvalidSyntax(line_i, "unexpected '||'".to_string()));
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
                            prop_map.insert(prop_name, (vec![], tmp_prop_sub_map.clone()));
                        }
                        None => (),
                    }

                    // コロンを除いた行
                    let pure_line = each_line[..each_line.len() - 1].to_string();
                    tmp_prop_name = Some(pure_line.to_string());
                },
                _ => return Err(SettingFileError::InvalidSyntax(line_i, "expected ',' and ':' at the end".to_string())),
            }
        }

        match tmp_prop_name {
            Some(prop_name) => {
                prop_map.insert(prop_name, (vec![], tmp_prop_sub_map));
            }
            None => (),
        }

        return Ok(prop_map);
    }

    pub fn print(&self) {
        println!("-- Setting File Data --");
        println!();
        println!("regex mode: {}", self.regex_mode);
        println!("file aliases:");

        for (alias_name, alias_path) in self.file_alias_map.iter() {
            println!("\t{}: {}", alias_name, alias_path);
        }
    }
}
