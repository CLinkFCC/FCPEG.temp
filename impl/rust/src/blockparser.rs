use crate::data;
use crate::setting;
use rustnutlib::*;

#[derive(Debug)]
pub enum FCPEGFileManError {
    Unknown(),
    FileManError(fileman::FileManError),
    SettingFileError(setting::SettingFileError),
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
            FCPEGFileManError::SettingFileError(err) => err.get_log_data(),
        }
    }
}

#[derive(Debug)]
pub enum BlockSyntaxError {
    Unknown(),
    ExpectedBlockDef(usize),
    ExpectedID(usize),
    UnexpectedEOF(usize, String),
    UnexpectedToken(usize, String, String),
    UnknownSyntax(usize, String),
    UnknownToken(usize, String),
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
            BlockSyntaxError::Unknown() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown error", vec![], vec![]),
            BlockSyntaxError::ExpectedBlockDef(line) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "expected block definition", vec![format!("line: {}", line + 1)], vec![]),
            BlockSyntaxError::ExpectedID(line) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "expected ID", vec![format!("line: {}", line + 1)], vec![]),
            BlockSyntaxError::UnexpectedEOF(line, expected_str) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unexpected EOF, expected '{}'", expected_str), vec![format!("line: {}", line + 1)], vec![]),
            BlockSyntaxError::UnexpectedToken(line, unexpected_token, expected_token) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unexpected token '{}', expected '{}'", unexpected_token, expected_token), vec![format!("line: {}", line + 1)], vec![]),
            BlockSyntaxError::UnknownSyntax(line, target_token) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unknown syntax"), vec![format!("line: {}", line + 1), format!("target token: '{}'", target_token)], vec![]),
            BlockSyntaxError::UnknownToken(line, unknown_token) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unknown token '{}'", unknown_token), vec![format!("line: {}", line + 1)], vec![]),
        }
    }
}

pub struct FCPEGFileMan {
    setting_file: setting::SettingFile,
    // alias_name, (alias_path, alias_content)
    src_file_map: std::collections::HashMap<String, (String, String)>,
    file_alias_blocks: std::collections::HashMap<String, std::collections::HashMap<String, data::Block>>,
}

impl FCPEGFileMan {
    pub fn new() -> Self {
        return FCPEGFileMan {
            setting_file: setting::SettingFile::new(),
            src_file_map: std::collections::HashMap::new(),
            file_alias_blocks: std::collections::HashMap::new(),
        }
    }

    fn add_file(&mut self, alias_name: String, file_path: String) -> std::result::Result<(), fileman::FileManError> {
        let file_content = fileman::FileMan::read_all(&file_path)?;
        self.src_file_map.insert(alias_name, (file_path, file_content));
        return Ok(());
    }

    pub fn load(&mut self, fcpeg_file_path: String) -> std::result::Result<(), FCPEGFileManError> {
        let setting_file_path = fileman::FileMan::rename_ext(&fcpeg_file_path, "cfg");

        match self.setting_file.load(setting_file_path) {
            Err(e) => return Err(FCPEGFileManError::SettingFileError(e)),
            Ok(()) => (),
        }

        self.setting_file.print();

        match self.add_file("".to_string(), fcpeg_file_path.to_string()) {
            Err(e) => return Err(FCPEGFileManError::FileManError(e)),
            Ok(()) => (),
        };

        for (alias_name, alias_path) in self.setting_file.file_alias_map.clone() {
            match self.add_file(alias_name.to_string(), alias_path.to_string()) {
                Err(e) => return Err(FCPEGFileManError::FileManError(e)),
                Ok(()) => (),
            };
        }

        return Ok(());
    }

    pub fn parse_all(&mut self) -> std::result::Result<(), BlockSyntaxError> {
        let mut block_parser = BlockParser::new();

        for (alias_name, alias_item) in self.src_file_map.iter() {
            println!("parsing all: {} {}", alias_name, alias_item.0);

            let tokens = BlockParser::get_tokens(&alias_item.1)?;
            self.file_alias_blocks.insert(alias_name.to_string(), block_parser.parse(tokens)?);
        }

        return Ok(());
    }
}

pub struct BlockParser {
    token_i: usize,
    tokens: Vec<data::Token>
}

impl BlockParser {
    pub fn new() -> Self {
        return BlockParser {
            token_i: 0,
            tokens: vec![],
        }
    }

    pub fn get_tokens(src_content: &String) -> std::result::Result<Vec<data::Token>, BlockSyntaxError> {
        let mut tokens: Vec<data::Token> = vec![];

        let id_regex = regex::Regex::new(r"[a-zA-Z0-9_]").unwrap();
        let mut tmp_id = "".to_string();

        let symbol_regex = regex::Regex::new(r"[!?\-+*.,:{}<>]").unwrap();

        let mut line_i: usize = 0;

        let chars: Vec<char> = src_content.chars().collect();
        let mut char_i: usize = 0;

        while char_i < chars.len() {
            let each_char = chars.get(char_i).unwrap();
            let each_char_str = each_char.to_string();

            // ID 判定
            if id_regex.is_match(&each_char_str) {
                tmp_id.push(*each_char);

                char_i += 1;
                continue;
            } else if tmp_id.len() != 0 {
                tokens.push(data::Token::new(line_i, data::TokenKind::ID, tmp_id.to_string()));
                tmp_id = "".to_string();
            }

            // コメント命令
            if *each_char == '%' {
                char_i += 1;

                while char_i < chars.len() {
                    let next_char = chars.get(char_i).unwrap();

                    char_i += 1;

                    if *next_char == '\n' {
                        line_i += 1;
                        break;
                    }
                }

                continue;
            }

            // 文字列判定
            if *each_char == '"' {
                let mut string = each_char_str.to_string();

                char_i += 1;

                // 一文字ずつ解析する
                loop {
                    // 文字列中に EOF が来た場合は弾く
                    let next_char = match chars.get(char_i) {
                        Some(v) => v,
                        None => return Err(BlockSyntaxError::UnexpectedEOF(line_i, "\"".to_string())),
                    };

                    // 文字列中の改行を弾く
                    if *next_char == '\n' {
                        return Err(BlockSyntaxError::UnexpectedToken(line_i, "\\n".to_string(), "\"".to_string()));
                    }

                    char_i += 1;

                    string.push(next_char.clone());

                    // エスケープシーケンスの解析
                    if *next_char == '\\' {
                        // エスケープ文字の後ろが EOF である場合は弾く
                        let esc_char = match chars.get(char_i) {
                            Some(v) => v,
                            None => return Err(BlockSyntaxError::UnexpectedEOF(line_i, "\"".to_string())),
                        };

                        string.push(esc_char.clone());

                        char_i += 1;
                        continue;
                    }

                    if *next_char == '"' {
                        break;
                    }
                }

                tokens.push(data::Token::new(line_i, data::TokenKind::String, string));
                continue;
            }

            // 角括弧内の文字列判定
            if *each_char == '[' {
                let mut string = each_char_str.to_string();

                char_i += 1;

                // 一文字ずつ解析する
                loop {
                    // 文字列中に EOF が来た場合は弾く
                    let next_char = match chars.get(char_i) {
                        Some(v) => v,
                        None => return Err(BlockSyntaxError::UnexpectedEOF(line_i, "]".to_string())),
                    };

                    char_i += 1;

                    string.push(next_char.clone());

                    // エスケープシーケンスの解析
                    if *next_char == '\\' {
                        // エスケープ文字の後ろが EOF である場合は弾く
                        let esc_char = match chars.get(char_i) {
                            Some(v) => v,
                            None => return Err(BlockSyntaxError::UnexpectedEOF(line_i, "]".to_string())),
                        };

                        string.push(esc_char.clone());

                        char_i += 1;
                        continue;
                    }

                    if *next_char == ']' {
                        break;
                    }
                }

                tokens.push(data::Token::new(line_i, data::TokenKind::StringInBracket, string));
                continue;
            }

            // スペース判定
            if *each_char == ' ' {
                tokens.push(data::Token::new(line_i, data::TokenKind::Space, each_char_str.to_string()));
                char_i += 1;

                // スペースが連続している場合はその分スキップする
                loop {
                    let is_next_char_space = match chars.get(char_i) {
                        Some(v) => *v == ' ',
                        None => return Err(BlockSyntaxError::UnexpectedEOF(line_i, "\"".to_string())),
                    };

                    if !is_next_char_space {
                        break;
                    }

                    char_i += 1;
                }

                continue;
            }

            // 改行を無視, line_i を加算
            if *each_char == '\n' {
                line_i += 1;
                char_i += 1;
                continue;
            }

            // 余計な改行コードを無視 ... CR 0x0a, LF 0x0d
            if *each_char as i32 == 10 || *each_char as i32 == 13 {
                char_i += 1;
                continue;
            }

            // シンボル判定
            if symbol_regex.is_match(&each_char_str) {
                tokens.push(data::Token::new(line_i, data::TokenKind::Symbol, each_char_str.to_string()));

                char_i += 1;
                continue;
            }

            char_i += 1;

            println!("+ {} {}", *each_char as i32, *each_char);
            return Err(BlockSyntaxError::UnknownToken(line_i, each_char.to_string()));
        }

        // 最後に tmp_id が残っていないかチェック
        if tmp_id.len() != 0 {
            tokens.push(data::Token::new(line_i, data::TokenKind::ID, tmp_id.to_string()));
            tmp_id = "".to_string();
        }

        for (token_i, each_token) in tokens.iter().enumerate() {
            println!("[{};{}] {}\t\t{}", each_token.line, token_i, each_token.value, each_token.kind);
        }

        return Ok(tokens);
    }

    // フィールドが初期化されるためインスタンスを使い回せる
    pub fn parse(&mut self, tokens: Vec<data::Token>) -> std::result::Result<std::collections::HashMap<String, data::Block>, BlockSyntaxError> {
        // フィールド初期化
        self.token_i = 0;
        self.tokens = tokens;

        let rule_map = std::collections::HashMap::<String, data::Rule>::new();
        let block_map = self.get_blocks()?;

        return Ok(block_map);
    }

    fn get_blocks(&mut self) -> std::result::Result<std::collections::HashMap<String, data::Block>, BlockSyntaxError> {
        let mut block_map = std::collections::HashMap::<String, data::Block>::new();

        while self.token_i < self.tokens.len() {
            let each_token = self.tokens.get(self.token_i).unwrap();

            // ブロック定義前にスペースがあれば飛ばす
            if each_token.kind == data::TokenKind::Space {
                self.token_i += 1;
                continue;
            }

            if each_token.kind == data::TokenKind::StringInBracket {
                self.token_i += 1;

                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind != data::TokenKind::Symbol || v.value != "{" {
                            return Err(BlockSyntaxError::UnknownToken(each_token.line, each_token.value.clone()));
                        }

                        ()
                    },
                    None => return Err(BlockSyntaxError::UnknownToken(each_token.line, each_token.value.clone())),
                }

                self.token_i += 1;

                // token_i がブロック終了位置 + 1 に設定される
                let block = self.get_block_content()?;
                block_map.insert(block.name.to_string(), block);

                continue;
            }

            return Err(BlockSyntaxError::ExpectedBlockDef(each_token.line));
        }

        return Ok(block_map);
    }

    // token_i がブロックの中身の開始位置であること
    // token_i はブロック終了位置 ( 閉じ括弧の位置 ) + 1 に設定される
    fn get_block_content(&mut self) -> std::result::Result<data::Block, BlockSyntaxError> {
        while self.token_i < self.tokens.len() {
            let each_token = self.tokens.get(self.token_i).unwrap();

            if each_token.kind == data::TokenKind::Symbol && each_token.value == "}" {
                self.token_i += 1;

                let block = data::Block::new("".to_string(), vec![], vec![]);
                return Ok(block);
            }

            self.token_i += 1;
        }

        // 閉じ括弧が見つからないためエラーを返す

        if self.tokens.len() == 0 {
            return Err(BlockSyntaxError::UnexpectedEOF(0, "}".to_string()));
        }

        let last_token = self.tokens.get(self.tokens.len() - 1).unwrap();
        return Err(BlockSyntaxError::UnexpectedEOF(last_token.line, "}".to_string()));
    }

    fn get_commands(&mut self) {
        while self.token_i < self.tokens.len() {
            let each_token = self.tokens.get(self.token_i).unwrap();

            
        }
    }
}
