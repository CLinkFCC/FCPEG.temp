use crate::data;
use rustnutlib::*;

#[derive(Debug)]
pub enum FCPEGFileManError {
    Unknown(),
    FileManError(fileman::FileManError),
}

impl std::fmt::Display for FCPEGFileManError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        return write!(f, "[FCPEGFileManError]");
    }
}

impl FCPEGFileManError {
    pub fn get_log_data(&self) -> console::ConsoleLogData {
        match self {
            FCPEGFileManError::Unknown() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown", vec![], vec![]),
            FCPEGFileManError::FileManError(err) => err.get_log_data(),
        }
    }
}

#[derive(Debug)]
pub enum BlockSyntaxError {
    Unknown(),
    ExpectedID(),
    UnexpectedEOF(String),
    UnexpectedToken(String, String),
}

impl std::error::Error for BlockSyntaxError {}

impl std::fmt::Display for BlockSyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        return write!(f, "[BlockSyntaxError]");
    }
}

impl BlockSyntaxError {
    pub fn get_log_data(&self) -> console::ConsoleLogData {
        match self {
            BlockSyntaxError::Unknown() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown", vec![], vec![]),
            BlockSyntaxError::ExpectedID() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "expected ID", vec![], vec![]),
            BlockSyntaxError::UnexpectedEOF(expected_str) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unexpected EOF, expected '{}'", expected_str), vec![], vec![]),
            BlockSyntaxError::UnexpectedToken(unexpected_token, expected_token) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unexpected token '{}', expected '{}'", unexpected_token, expected_token), vec![], vec![]),
        }
    }
}

pub struct FCPEGFileMan {
    // alias_name, (alias_path, alias_content)
    src_file_map: std::collections::HashMap<String, (String, String)>,
    file_alias_blocks: std::collections::HashMap<String, std::collections::HashMap<String, data::Block>>,
}

impl FCPEGFileMan {
    pub fn new() -> Self {
        return FCPEGFileMan {
            src_file_map: std::collections::HashMap::new(),
            file_alias_blocks: std::collections::HashMap::new(),
        }
    }

    pub fn add_file(&mut self, alias_name: String, file_path: String) -> std::result::Result<(), fileman::FileManError> {
        // match self.add_file(main_file_path.clone()) {
        //     Ok(()) => (),
        //     Err(e) => return Err(FCPEGFileManError::FileManError(e)),
        // };

        let file_content = fileman::FileMan::read_all(&file_path)?;
        self.src_file_map.insert(alias_name, (file_path, file_content));
        return Ok(());
    }

    pub fn parse_all(&mut self) -> std::result::Result<(), BlockSyntaxError> {
        for (alias_name, alias_item) in self.src_file_map.iter() {
            println!("parsing all: {}", alias_name);
            self.file_alias_blocks.insert(alias_name.to_string(), BlockParser::parse(&alias_item.1)?);
        }

        return Ok(());
    }
}

pub struct BlockParser {}

impl BlockParser {
    pub fn parse(src_content: &String) -> std::result::Result<std::collections::HashMap<String, data::Block>, BlockSyntaxError> {
        // let rule_map = std::collections::HashMap::<String, data::Rule>::new();
        let blocks = BlockParser::get_blocks(src_content)?;

        return Ok(blocks);
    }

    fn get_blocks(src: &String) -> std::result::Result<std::collections::HashMap<String, data::Block>, BlockSyntaxError> {
        let block_map = std::collections::HashMap::<String, data::Block>::new();

        let id_regex = regex::Regex::new("[a-zA-Z0-9_-]").unwrap();

        let chars: Vec<char> = src.chars().collect();

        let mut is_in_commentout = false;

        let mut char_i: usize = 0;

        while char_i < chars.len() {
            if chars[char_i] == '%' {
                is_in_commentout = true;
                char_i += 1;
                continue;
            }

            if is_in_commentout {
                if chars[char_i] == '\n' {
                    is_in_commentout = false;
                }

                char_i += 1;
                continue;
            }

            if chars[char_i] == '[' {
                char_i += 1;
                let mut block_id = "".to_string();

                while char_i < src.len() {
                    if id_regex.is_match(&chars[char_i].to_string()) {
                        block_id = format!("{}{}", block_id, chars[char_i]);
                        char_i += 1;
                    } else {
                        if block_id.len() == 0 {
                            return Err(BlockSyntaxError::ExpectedID());
                        }

                        break;
                    }
                }

                // 閉じ角括弧
                match chars.get(char_i) {
                    Some(v) => {
                        if *v != ']' {
                            return Err(BlockSyntaxError::UnexpectedToken(v.to_string(), "]".to_string()));
                        }
                    },
                    None => return Err(BlockSyntaxError::UnexpectedEOF("]".to_string())),
                }

                // 開き中括弧

                char_i += 1;

                match chars.get(char_i) {
                    Some(v) => {
                        if *v != '{' {
                            return Err(BlockSyntaxError::UnexpectedEOF("{{".to_string()));
                        }
                    },
                    None => return Err(BlockSyntaxError::UnexpectedEOF("{{".to_string())),
                }

                println!("id {}", block_id);

                BlockParser::get_block_contents(chars.clone(), &mut char_i)?;

                continue;
            }

            char_i += 1;
        }

        return Ok(block_map);
    }

    fn get_block_contents(chars: Vec<char>, index: &mut usize) -> std::result::Result<(Vec<data::Command>, Vec<data::Rule>), BlockSyntaxError> {
        let commands: Vec<data::Command> = vec![];
        let rules: Vec<data::Rule> = vec![];

        while index < &mut(chars.len()) {
            let each_char = chars[*index];

            if each_char == '}' {
                *index += 1;
                break;
            }

            // index = &mut(*index + 1);
            *index += 1;

            if *index + 1 == chars.len() {
                return Err(BlockSyntaxError::UnexpectedEOF("}".to_string()));
            }
        }

        return Ok((commands, rules));
    }

    // fn get_block_content_
}
