use crate::data;
use rustnutlib::*;

#[derive(Debug)]
pub enum RuleParserError {
    AlreadyParsed(),
    FileManError(fileman::FileManError),
    InvalidPath(String),
}

// impl RuleParserError {
//     pub fn get_log_data(&self) -> console::ConsoleLogData {
//         match self {
//             RuleParserError::FileManError(err) => err.get_log_data(),
//             RuleParserError::AlreadyParsed() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "{^file.err.1069}", vec![], vec![format!("{{^console.spec_link}}: https://ches.gant.work/en/spec/console/file/error/1069/index.html")]),
//         }
//     }
// }

impl std::error::Error for RuleParserError {}

impl std::fmt::Display for RuleParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        return write!(f, "[RuleParserError]");
    }
}

pub struct RuleParser {
    has_parsed: bool,
    files: std::collections::HashMap<String, String>,
    rules: std::collections::HashMap<String, data::Rule>,
}

impl RuleParser {
    pub fn new() -> Self {
        return RuleParser {
            has_parsed: false,
            files: std::collections::HashMap::new(),
            rules: vec![],
        };
    }

    pub fn add_file(&mut self, file_path: String) -> std::result::Result<(), fileman::FileManError> {
        let file_content = fileman::FileMan::read_all(&file_path)?;
        self.files.insert(file_path, file_content);
        return Ok(());
    }

    pub fn parse(&mut self, main_file_path: String) -> std::result::Result<(), RuleParserError> {
        if self.has_parsed {
            return Err(RuleParserError::AlreadyParsed())
        }

        match self.add_file(main_file_path.clone()) {
            Ok(()) => (),
            Err(e) => return Err(RuleParserError::FileManError(e)),
        };

        self.parse_file(&main_file_path);

        self.has_parsed = true;
        return Ok(());
    }

    fn parse_file(&self, file_key: &String) -> std::result::Result<std::collections::HashMap<String, data::Rule>, RuleParserError> {
        if self.files.contains_key(file_key) {
            return Err(RuleParserError::InvalidPath(file_key.to_string()));
        }

        

        return rule_map;
    }

    fn get_blocks(src: &String) -> std::result::Result<std::collections::HashMap<String, data::Block>, RuleParserError> {

        return block_map;
    }
}
