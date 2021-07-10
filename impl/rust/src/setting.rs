use rustnutlib::*;

#[derive(Debug)]
pub enum SettingFileError {
    Unknown(),
    DuplicatedPropName(String),
    FileManError(fileman::FileManError),
    InvalidPropValueLength(String),
    InvalidSyntax(usize),
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
            SettingFileError::InvalidSyntax(line) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "invalid syntax", vec![], vec![format!("line: {}", line)]),
            SettingFileError::UnknownRegexMode(input) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown regex mode", vec![], vec![format!("input: {}", input)]),
        }
    }
}

pub enum RegexMode {
    Default,
    Onise,
}

impl std::fmt::Display for RegexMode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        return match self {
            RegexMode::Default => write!(f, "Default"),
            RegexMode::Onise => write!(f, "Onise"),
        };
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

        for (prop_name, prop_values) in prop_map.iter() {
            match prop_name.as_str() {
                "Regex" => {
                    // 正規表現モード

                    let regex_mode_str = match prop_values.get(0) {
                        Some(v) => v,
                        None => return Err(SettingFileError::InvalidPropValueLength(prop_name.to_string())),
                    };

                    self.regex_mode = SettingFile::get_regex_mode(regex_mode_str.to_string())?;
                },
                _ => {
                    // ファイルエイリアス

                    let file_alias_path = match prop_values.get(0) {
                        Some(v) => v,
                        None => return Err(SettingFileError::InvalidPropValueLength(prop_name.to_string())),
                    };

                    self.file_alias_map.insert(prop_name.to_string(), file_alias_path.to_string());
                }
            }
        }

        return Ok(());
    }

    fn get_prop_map(lines: &Vec<String>) -> std::result::Result<std::collections::HashMap::<String, Vec<String>>, SettingFileError> {
        let mut prop_map = std::collections::HashMap::<String, Vec<String>>::new();

        for (line_i, each_line) in lines.iter().enumerate() {
            if each_line == "" {
                continue;
            }

            if each_line[each_line.len() - 1..each_line.len()].to_string() != "," {
                return Err(SettingFileError::InvalidSyntax(line_i));
            }

            // コンマを除いた行
            let pure_line = each_line[0..each_line.len() - 1].to_string();
            // 右辺と左辺
            let both_sides: Vec<&str> = pure_line.split(": ").collect();

            if both_sides.len() != 2 {
                return Err(SettingFileError::InvalidSyntax(line_i));
            }

            let prop_name = both_sides.get(0).unwrap().to_string();

            if prop_map.contains_key(&prop_name) {
                return Err(SettingFileError::DuplicatedPropName(prop_name))
            }

            let prop_values_orig: Vec<&str> = both_sides.get(1).unwrap().split(" ").collect();
            let prop_values: Vec<String> = prop_values_orig.iter().map(|s| s.to_string()).collect();

            if prop_values == vec![""] {
                return Err(SettingFileError::InvalidSyntax(line_i));
            }

            prop_map.insert(prop_name, prop_values);
        }

        return Ok(prop_map);
    }

    // todo: match を使う
    fn get_regex_mode(_regex_mode_str: String) -> std::result::Result<RegexMode, SettingFileError> {
        return Ok(RegexMode::Onise);
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
