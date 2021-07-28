use crate::data;
use crate::setting;
use rustnutlib::*;

pub enum FCPEGFileManError {
    Unknown(),
    FileManError(fileman::FileManError),
    SettingFileError(setting::SettingFileError),
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
pub enum BlockParseError {
    Unknown(),
    BlockAliasNotFound(usize, String),
    DuplicatedBlockAliasName(usize, String),
    DuplicatedBlockName(usize, String),
    DuplicatedStartCmd(),
    ExpectedBlockDef(usize),
    ExpectedToken(usize, String),
    InternalErr(String),
    InvalidCharClassFormat(usize, String),
    MainBlockNotFound(),
    NoStartCmdInMainBlock(),
    RuleHasNoChoice(String),
    RuleInMainBlock(),
    StartCmdOutsideMainBlock(),
    UnexpectedEOF(usize, String),
    UnexpectedToken(usize, String, String),
    UnknownPragmaName(usize, String),
    UnknownSyntax(usize, String),
    UnknownToken(usize, String),
}

impl BlockParseError {
    pub fn get_log_data(&self) -> console::ConsoleLogData {
        match self {
            BlockParseError::Unknown() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown error", vec![], vec![]),
            BlockParseError::BlockAliasNotFound(line, block_alias_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("block alias '{}' not found", block_alias_name), vec![format!("line: {}", line + 1)], vec![]),
            BlockParseError::DuplicatedBlockAliasName(line, block_alias_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("duplicated block alias name '{}'", block_alias_name), vec![format!("line: {}", line + 1)], vec![]),
            BlockParseError::DuplicatedBlockName(line, block_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("duplicated block name '{}'", block_name), vec![format!("line: {}", line + 1)], vec![]),
            BlockParseError::DuplicatedStartCmd() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "duplicated start command", vec![], vec![]),
            BlockParseError::ExpectedBlockDef(line) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "expected block definition", vec![format!("line: {}", line + 1)], vec![]),
            BlockParseError::ExpectedToken(line, expected_str) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("expected token {}", expected_str), vec![format!("line: {}", line + 1)], vec![]),
            BlockParseError::InternalErr(err_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("internal error: {}", err_name), vec![], vec![]),
            BlockParseError::InvalidCharClassFormat(line, value) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("invalid character class format '{}'", value), vec![format!("line: {}", line + 1)], vec![]),
            BlockParseError::MainBlockNotFound() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "main block not found", vec![], vec![]),
            BlockParseError::NoStartCmdInMainBlock() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "no start command in main block", vec![], vec![]),
            BlockParseError::RuleHasNoChoice(rule_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("rule '{}' has no choice", rule_name), vec![], vec![]),
            BlockParseError::RuleInMainBlock() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "rule in main block", vec![], vec![]),
            BlockParseError::StartCmdOutsideMainBlock() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "start command outside main block", vec![], vec![]),
            BlockParseError::UnexpectedEOF(line, expected_str) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unexpected EOF, expected {}", expected_str), vec![format!("line: {}", line + 1)], vec![]),
            BlockParseError::UnexpectedToken(line, unexpected_token, expected_str) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unexpected token '{}', expected {}", unexpected_token, expected_str), vec![format!("line: {}", line + 1)], vec![]),
            BlockParseError::UnknownPragmaName(line, unknown_pragma_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown pragma name", vec![format!("line: {}", line + 1), format!("pragma name: {}", unknown_pragma_name)], vec![]),
            BlockParseError::UnknownSyntax(line, target_token) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown syntax", vec![format!("line: {}", line + 1), format!("target token: '{}'", target_token)], vec![]),
            BlockParseError::UnknownToken(line, unknown_token) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unknown token '{}'", unknown_token), vec![format!("line: {}", line + 1)], vec![]),
        }
    }
}

pub struct FCPEGFileMan {
    is_loaded: bool,
    // ルートのエイリアス名は空文字
    pub file_alias_name: String,
    pub sub_file_aliase_map: std::collections::HashMap<String, FCPEGFileMan>,
    setting_file: setting::SettingFile,
    fcpeg_file_path: String,
    fcpeg_file_content: String,
    pub block_map: std::collections::HashMap<String, data::Block>,
}

impl FCPEGFileMan {
    pub fn new(file_alias_name: String, fcpeg_file_path: String) -> Self {
        return FCPEGFileMan {
            is_loaded: false,
            file_alias_name: file_alias_name,
            sub_file_aliase_map: std::collections::HashMap::new(),
            setting_file: setting::SettingFile::new(),
            fcpeg_file_path: fcpeg_file_path,
            fcpeg_file_content: "".to_string(),
            block_map: std::collections::HashMap::new(),
        }
    }

    fn add_sub_file_alias(&mut self, file_alias_name: String, file_path: String) -> std::result::Result<(), FCPEGFileManError> {
        let mut alias = FCPEGFileMan::new(file_alias_name.to_string(), file_path);
        alias.load()?;
        self.sub_file_aliase_map.insert(file_alias_name.to_string(), alias);

        return Ok(());
    }

    pub fn load(&mut self) -> std::result::Result<(), FCPEGFileManError> {
        self.fcpeg_file_content = match fileman::FileMan::read_all(&self.fcpeg_file_path) {
            Err(e) => return Err(FCPEGFileManError::FileManError(e)),
            Ok(v) => v,
        };

        let setting_file_path = fileman::FileMan::rename_ext(&self.fcpeg_file_path, "cfg");

        match self.setting_file.load(setting_file_path) {
            Err(e) => return Err(FCPEGFileManError::SettingFileError(e)),
            Ok(()) => (),
        }

        self.is_loaded = true;
        self.setting_file.print();

        for (alias_name, alias_path) in self.setting_file.file_alias_map.clone() {
            match self.add_sub_file_alias(alias_name.to_string(), alias_path.to_string()) {
                Err(e) => return Err(e),
                Ok(()) => (),
            };
        }

        return Ok(());
    }

    pub fn parse_all(&mut self) -> std::result::Result<(), BlockParseError> {
        if !self.is_loaded {
            return Err(BlockParseError::InternalErr(format!("file manager not loaded (path: '{}')", self.fcpeg_file_path)));
        }

        let mut block_parser = BlockParser::new();

        let tokens = BlockParser::get_tokens(&self.fcpeg_file_content)?;
        self.block_map = block_parser.parse(self.file_alias_name.to_string(), tokens)?;

        println!();
        println!("parsing: {}", self.fcpeg_file_path);
        println!();

        for (block_name, each_block) in &self.block_map {
            println!("block {}", block_name);

            for cmd in &each_block.cmds {
                println!("\t{}", cmd);
            }
        }

        println!();

        for alias_item in self.sub_file_aliase_map.values_mut() {
            alias_item.parse_all()?;
        }

        return Ok(());
    }
}

pub struct BlockParser {
    file_alias_name: String,
    token_i: usize,
    tokens: Vec<data::Token>
}

impl BlockParser {
    pub fn new() -> Self {
        return BlockParser {
            file_alias_name: "".to_string(),
            token_i: 0,
            tokens: vec![],
        }
    }

    pub fn get_tokens(src_content: &String) -> std::result::Result<Vec<data::Token>, BlockParseError> {
        let mut tokens: Vec<data::Token> = vec![];

        let id_regex = regex::Regex::new(r"[a-zA-Z0-9_]").unwrap();
        let mut tmp_id = "".to_string();

        let symbol_regex = regex::Regex::new(r"[&!?\-+*.,:(){}<>]").unwrap();

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
                        None => return Err(BlockParseError::UnexpectedEOF(line_i, "'\"'".to_string())),
                    };

                    // 文字列中の改行を弾く
                    if *next_char == '\n' {
                        return Err(BlockParseError::UnexpectedToken(line_i, "\\n".to_string(), "'\"'".to_string()));
                    }

                    char_i += 1;

                    // エスケープシーケンスの解析
                    if *next_char == '\\' {
                        // エスケープ文字の後ろが EOF である場合は弾く
                        let esc_char = match chars.get(char_i) {
                            Some(v) => v,
                            None => return Err(BlockParseError::UnexpectedEOF(line_i, "'\"'".to_string())),
                        };

                        let escaped_char = match esc_char {
                            '\\' => '\\',
                            'n' => '\n',
                            't' => '\t',
                            _ => return Err(BlockParseError::UnexpectedToken(line_i, esc_char.to_string(), "'\\', 'n' and 't'".to_string())),
                        };

                        string.push(escaped_char.clone());

                        char_i += 1;
                        continue;
                    }

                    string.push(next_char.clone());

                    if *next_char == '"' {
                        break;
                    }
                }

                // 文字列が空であれば構文エラー
                if string == "\"\"" {
                    return Err(BlockParseError::UnexpectedToken(line_i, "\"".to_string(), "character of string".to_string()));
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
                        None => return Err(BlockParseError::UnexpectedEOF(line_i, "']'".to_string())),
                    };

                    char_i += 1;

                    // エスケープシーケンスの解析
                    if *next_char == '\\' {
                        // エスケープ文字の後ろが EOF である場合は弾く
                        let esc_char = match chars.get(char_i) {
                            Some(v) => v,
                            None => return Err(BlockParseError::UnexpectedEOF(line_i, "']'".to_string())),
                        };

                        let escaped_char = match esc_char {
                            '\\' => '\\',
                            '[' => '[',
                            ']' => ']',
                            'n' => '\n',
                            't' => '\t',
                            _ => return Err(BlockParseError::UnexpectedToken(line_i, esc_char.to_string(), "'\\', '[', ']', 'n' and 't'".to_string())),
                        };

                        string.push(escaped_char.clone());

                        char_i += 1;
                        continue;
                    }

                    if *next_char == '[' {
                        return Err(BlockParseError::UnexpectedToken(line_i, next_char.to_string(), "'\\' before '['".to_string()));
                    }

                    string.push(next_char.clone());

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
                        None => return Err(BlockParseError::UnexpectedEOF(line_i, "'\"'".to_string())),
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

            println!("+ {} {}", *each_char as i32, *each_char);
            return Err(BlockParseError::UnknownToken(line_i, each_char.to_string()));
        }

        // 最後に tmp_id が残っていないかチェック
        if tmp_id.len() != 0 {
            tokens.push(data::Token::new(line_i, data::TokenKind::ID, tmp_id.to_string()));
        }

        for (token_i, each_token) in tokens.iter().enumerate() {
            println!("[{};{}] {}\t\t{}", each_token.line, token_i, each_token.value, each_token.kind);
        }

        return Ok(tokens);
    }

    // フィールドが初期化されるためインスタンスを使い回せる
    pub fn parse(&mut self, file_alias_name: String, tokens: Vec<data::Token>) -> std::result::Result<std::collections::HashMap<String, data::Block>, BlockParseError> {
        // フィールド初期化
        self.file_alias_name = file_alias_name;
        self.token_i = 0;
        self.tokens = tokens;

        let block_map = self.get_blocks()?;
        return Ok(block_map);
    }

    // 初期位置: パース対象ソースの開始位置
    fn get_blocks(&mut self) -> std::result::Result<std::collections::HashMap<String, data::Block>, BlockParseError> {
        let mut block_map = std::collections::HashMap::<String, data::Block>::new();

        while self.token_i < self.tokens.len() {
            let each_token = self.tokens.get(self.token_i).unwrap().clone();

            // ブロック定義前にスペースがあれば飛ばす
            if each_token.kind == data::TokenKind::Space {
                self.token_i += 1;
                continue;
            }

            // ブロック名を判定
            if each_token.kind == data::TokenKind::StringInBracket {
                self.token_i += 1;

                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Space {
                            self.token_i += 1;
                        }

                        ()
                    },
                    None => return Err(BlockParseError::UnknownToken(each_token.line, each_token.value.clone())),
                }

                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind != data::TokenKind::Symbol || v.value != "{" {
                            return Err(BlockParseError::UnknownToken(each_token.line, each_token.value.clone()));
                        }

                        ()
                    },
                    None => return Err(BlockParseError::UnknownToken(each_token.line, each_token.value.clone())),
                }

                self.token_i += 1;

                let block_name_token = each_token.value.to_string();

                // token_i がブロック終了位置 + 1 に設定される

                let block = self.get_block_content(block_name_token[1..block_name_token.len() - 1].to_string())?;
                let block_name = block.name.to_string();

                // 角括弧内にブロック名がない場合はエラー
                if block_name == "" {
                    return Err(BlockParseError::UnexpectedToken(each_token.line, "]".to_string(), "ID".to_string()));
                }

                // ブロック名が重複している場合はエラー
                if block_map.contains_key(&block_name) {
                    return Err(BlockParseError::DuplicatedBlockName(each_token.line, block_name.to_string()));
                }

                block_map.insert(block_name, block);

                continue;
            }

            return Err(BlockParseError::ExpectedBlockDef(each_token.line));
        }

        return Ok(block_map);
    }

    // 各ブロックの中身を取得する
    // token_i の条件は get_next_command_content() と同様
    fn get_block_content(&mut self, block_name: String) -> std::result::Result<data::Block, BlockParseError> {
        let cmds = self.get_commands()?;
        let block = data::Block::new(block_name, cmds);
        return Ok(block);
    }

    // ブロック内のすべてのコマンドを取得する
    // token_i の条件は get_next_command_content() と同様
    fn get_commands(&mut self) -> std::result::Result<Vec<data::Command>, BlockParseError> {
        let mut cmds = Vec::<data::Command>::new();
        let mut new_cmd = self.get_next_command_content()?;

        // get_next_command_content() の返り値が None になるまで続ける
        while new_cmd.is_some() {
            cmds.push(new_cmd.unwrap());
            new_cmd = self.get_next_command_content()?;
        }

        return Ok(cmds);
    }

    // 各コマンドの中身を取得する
    // token_i がブロックの中身の開始位置もしくは前の命令の終了記号位置 + 1 であること
    // token_i は各コマンドの終了記号位置 + 1 に設定される
    fn get_next_command_content(&mut self) -> std::result::Result<std::option::Option<data::Command>, BlockParseError> {
        let mut pragma_args = Vec::<data::Token>::new();

        while self.token_i < self.tokens.len() {
            let each_token = self.tokens.get(self.token_i).unwrap();

            // スペースがあれば除去する
            if each_token.kind == data::TokenKind::Space {
                self.token_i += 1;
                continue;
            }

            if each_token.kind == data::TokenKind::Symbol && each_token.value == "}" {
                self.token_i += 1;
                return Ok(None);
            }

            if each_token.kind == data::TokenKind::Symbol && each_token.value == "+" {
                self.token_i += 1;

                let mut last_token_line_num = each_token.line;
                // NULL 文字の場合は未設定
                let mut pragma_name = "\0".to_string();

                while self.token_i < self.tokens.len() {
                    let next_token = match self.tokens.get(self.token_i) {
                        Some(v) => {
                            last_token_line_num = v.line;
                            v
                        },
                        None => {
                            // Unexpected EOF エラーを返す
                            if pragma_name == "\0" {
                                return Err(BlockParseError::UnexpectedEOF(last_token_line_num, "pragma name".to_string()));
                            } else {
                                return Err(BlockParseError::UnexpectedEOF(last_token_line_num, ",".to_string()));
                            }
                        }
                    };

                    // スペースがあれば除去する
                    if next_token.kind == data::TokenKind::Space {
                        self.token_i += 1;
                        continue;
                    }

                    if pragma_name == "\0" {
                        if next_token.kind == data::TokenKind::ID {
                            // マクロ名が未設定であれば設定する
                            pragma_name = next_token.value.to_string();
                            self.token_i += 1;
                            continue;
                        } else {
                            // マクロ名にあたるトークンが見つからない場合はエラー
                            return Err(BlockParseError::UnexpectedToken(last_token_line_num, next_token.value.to_string(), "pragma name".to_string()));
                        }
                    }

                    if next_token.kind == data::TokenKind::ID || next_token.kind == data::TokenKind::String || (next_token.kind == data::TokenKind::Symbol && next_token.value == ".") {
                        self.token_i += 1;
                        pragma_args.push(next_token.clone());
                        continue;
                    }

                    if next_token.kind == data::TokenKind::Symbol && next_token.value == "," {
                        self.token_i += 1;

                        let cmd = self.get_command_from_data(next_token.line, pragma_name, pragma_args)?;
                        return Ok(Some(cmd));
                    }

                    return Err(BlockParseError::UnknownSyntax(each_token.line, each_token.value.to_string()));
                }
            }

            if each_token.kind == data::TokenKind::ID {
                let mut line_num = each_token.line;

                // 規則名前のスペース
                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Space {
                            self.token_i += 1;
                            line_num = v.line;
                        }
                    },
                    None => return Err(BlockParseError::UnexpectedEOF(line_num, "' '".to_string())),
                }

                // 規則名
                let rule_name_token = match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::ID {
                            self.token_i += 1;
                            line_num = v.line;
                            v
                        } else {
                            return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "ID".to_string()));
                        }
                    },
                    None => return Err(BlockParseError::UnexpectedEOF(line_num, "'ID'".to_string())),
                };

                // 規則名後のスペース
                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Space {
                            self.token_i += 1;
                            line_num = v.line;
                        }
                    },
                    None => return Err(BlockParseError::UnexpectedEOF(line_num, "' '".to_string())),
                }

                // 規則定義の記号 <
                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Symbol && v.value == "<" {
                            self.token_i += 1;
                            line_num = v.line;
                        } else {
                            return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "'<'".to_string()));
                        }
                    },
                    None => return Err(BlockParseError::UnexpectedEOF(line_num, "'<'".to_string())),
                }

                // 規則定義の記号 -
                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Symbol && v.value == "-" {
                            self.token_i += 1;
                            line_num = v.line;
                        } else {
                            return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "'-'".to_string()));
                        }
                    },
                    None => return Err(BlockParseError::UnexpectedEOF(line_num, "'-'".to_string())),
                }

                // 規則定義の記号後のスペース
                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Space {
                            self.token_i += 1;
                        }
                    },
                    None => return Err(BlockParseError::UnexpectedEOF(line_num, "' '".to_string())),
                }

                let mut pragma_args = Vec::<data::Token>::new();
                pragma_args.push(rule_name_token.clone());

                while self.token_i < self.tokens.len() {
                    let next_token = self.tokens.get(self.token_i).unwrap();

                    // スペースがあれば除去する
                    if next_token.kind == data::TokenKind::Space {
                        self.token_i += 1;
                        continue;
                    }

                    if next_token.kind == data::TokenKind::Symbol && next_token.value == "," {
                        self.token_i += 1;

                        let cmd = self.get_command_from_data(next_token.line, "define".to_string(), pragma_args)?;
                        return Ok(Some(cmd));
                    }

                    pragma_args.push(next_token.clone());
                    self.token_i += 1;
                }
            }

            // 構文がマッチしなかった場合はエラー
            return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "'+' and ID".to_string()));
        }

        // let cmd = data::Command::;
        // return Ok(cmd);

        // ブロック終了記号の検知はどうする？
        // これ以上コマンドが見つからないため None を返す

        return Ok(None);
    }

    // pragma_arg: プラグマ名が define の場合、長さは 0 であってならない
    fn get_command_from_data(&self, line_num: usize, pragma_name: String, pragma_args: Vec<data::Token>) -> std::result::Result<data::Command, BlockParseError> {
        let cmd = match pragma_name.as_str() {
            "define" => {
                if pragma_args.len() == 0 {
                    return Err(BlockParseError::InternalErr("invalid pragma argument length".to_string()));
                }

                let rule = BlockParser::get_define_command(pragma_args.get(0).unwrap().value.to_string(), pragma_args[1..].to_vec())?;
                data::Command::Define(line_num, rule)
            },
            "start" => {
                if pragma_args.len() == 0 {
                    return Err(BlockParseError::UnexpectedToken(line_num, ",".to_string(), "pragma argument".to_string()));
                }

                if pragma_args.len() != 3 {
                    return Err(BlockParseError::UnexpectedToken(line_num, pragma_args.get(0).unwrap().value.to_string(), "','".to_string()));
                }

                // ブロック名を取得

                let block_name_token = pragma_args.get(0).unwrap();

                if block_name_token.kind != data::TokenKind::ID {
                    return Err(BlockParseError::UnexpectedToken(line_num, block_name_token.value.to_string(), "ID".to_string()));
                }

                // 識別子間のピリオドをチェック

                let period_token = pragma_args.get(1).unwrap();

                if period_token.kind != data::TokenKind::Symbol || period_token.value != "." {
                    return Err(BlockParseError::UnexpectedToken(line_num, period_token.value.to_string(), "'.'".to_string()));
                }

                let rule_name_token = pragma_args.get(2).unwrap();

                if rule_name_token.kind != data::TokenKind::ID {
                    return Err(BlockParseError::UnexpectedToken(line_num, period_token.value.to_string(), "ID".to_string()));
                }

                let block_name = block_name_token.value.to_string();
                let rule_name = rule_name_token.value.to_string();

                data::Command::Start(line_num, self.file_alias_name.to_string(), block_name, rule_name)
            },
            "use" => {
                if pragma_args.len() == 0 {
                    return Err(BlockParseError::UnexpectedToken(line_num, ",".to_string(), "pragma argument".to_string()));
                }

                // ブロック名を取得

                let block_name_token = pragma_args.get(0).unwrap();

                if block_name_token.kind != data::TokenKind::ID {
                    return Err(BlockParseError::UnexpectedToken(line_num, block_name_token.value.to_string(), "ID".to_string()));
                }

                let block_name = block_name_token.value.to_string();

                // ブロックエイリアス名を取得

                let block_alias_name = block_name.to_string();

                /* todo: エイリアス機能を後で作る
                if pragma_args.len() == 5 {
                    match pragma_args.get(1) {
                        Some(v) => {
                            if v.kind != data::TokenKind::Space {
                                return Err(BlockParseError::UnexpectedToken(line_num, v.value, "' '".to_string()));
                            }
                        },
                        None => return Err(BlockParseError::ExpectedToken(line_num, "' '".to_string())),
                    }

                    match pragma_args.get(2) {
                        Some(v) => {
                            if v.kind != data::TokenKind::Space {
                                return Err(BlockParseError::UnexpectedToken(line_num, v.value, "ID".to_string()));
                            }
                        },
                        None => return Err(BlockParseError::ExpectedToken(line_num, "'as'".to_string())),
                    }

                    match pragma_args.get(3) {
                        Some(v) => {
                            if v.kind != data::TokenKind::Space {
                                return Err(BlockParseError::UnexpectedToken(line_num, v.value, "' '".to_string()));
                            }
                        },
                        None => return Err(BlockParseError::ExpectedToken(line_num, "' '".to_string())),
                    }

                    let unexpected_token_value = match pragma_args.get(2).unwrap().get(0) {
                        Some(v) => v.value,
                        None => "".to_string(),
                    };

                    return Err(BlockParseError::UnexpectedToken(line_num, unexpected_token_value, "','".to_string()));
                }
                */

                data::Command::Use(line_num, self.file_alias_name.to_string(), block_name, block_alias_name)
            },
            _ => return Err(BlockParseError::UnknownPragmaName(line_num, pragma_name.to_string())),
        };

        return Ok(cmd);
    }

    fn get_define_command(rule_name: String, tokens: Vec<data::Token>) -> std::result::Result<data::Rule, BlockParseError> {
        if tokens.len() == 0 {
            return Err(BlockParseError::RuleHasNoChoice(rule_name.to_string()));
        }

        let mut token_i = 0;

        let mut choices = Vec::<data::RuleChoice>::new();
        let mut tmp_seq_groups = Vec::<data::RuleSequenceGroup>::new();
        let mut tmp_seqs = Vec::<data::RuleSequence>::new();
        let mut tmp_exprs = Vec::<data::RuleExpression>::new();

        let mut previous_lookahead_kind = data::RuleExpressionLookaheadKind::None;
        // 括弧内でのキャッシュ
        let mut previous_group_seq_lookahead_kind = data::RuleExpressionLookaheadKind::None;

        let mut is_in_paren = false;

        while token_i < tokens.len() {
            let each_token = tokens.get(token_i).unwrap();

            // 1つ先のループ文字を取得
            // トークンが1個の expression にのみ対応
            // ここと括弧開始時以外ではループ文字が予期しないトークンとして扱われるため特段エラー処理は必要ない
            let previous_loop_kind = match tokens.get(token_i + 1) {
                Some(v) => {
                    match data::RuleExpressionLoopKind::to_loop_kind(v.value.to_string()) {
                        Some(v2) => {
                            token_i += 1;
                            v2
                        },
                        None => data::RuleExpressionLoopKind::One,
                    }
                },
                None => data::RuleExpressionLoopKind::One,
            };

            // スペース
            if each_token.kind == data::TokenKind::Space {
                token_i += 1;
                continue;
            }

            // 文字クラス
            if each_token.kind == data::TokenKind::StringInBracket {
                tmp_exprs.push(data::RuleExpression::new(each_token.line, data::RuleExpressionKind::CharClass, previous_loop_kind.clone(), previous_lookahead_kind.clone(), each_token.value.to_string()));

                previous_lookahead_kind = data::RuleExpressionLookaheadKind::None;
                token_i += 1;

                continue;
            }

            // ID
            if each_token.kind == data::TokenKind::ID {
                let mut is_id_token_len_one = true;
                token_i += 1;

                // 次のピリオドがあれば続ける
                match tokens.get(token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Symbol && v.value == "." {
                            is_id_token_len_one = false;
                        }
                    },
                    None => (),
                }

                if is_id_token_len_one {
                    // トークンが1個であるためループ文字チェックは必要ない
                    tmp_exprs.push(data::RuleExpression::new(each_token.line, data::RuleExpressionKind::ID, previous_loop_kind.clone(), previous_lookahead_kind.clone(), each_token.value.to_string()));
                    previous_lookahead_kind = data::RuleExpressionLookaheadKind::None;
                    continue;
                }

                token_i += 1;

                // 2つ目の ID を取得
                let second_id = match tokens.get(token_i) {
                    Some(v) => {
                        if v.kind != data::TokenKind::ID {
                            return Err(BlockParseError::UnexpectedToken(each_token.line, v.value.to_string(), "ID".to_string()));
                        }

                        v.value.to_string()
                    },
                    None => return Err(BlockParseError::UnexpectedEOF(each_token.line, "ID".to_string())),
                };

                let mut is_id_token_len_two = true;
                token_i += 1;

                // 次のピリオドがあれば続ける
                match tokens.get(token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Symbol && v.value == "." {
                            is_id_token_len_two = false;
                        }
                    },
                    None => (),
                }

                if is_id_token_len_two {
                    let loop_kind = match tokens.get(token_i) {
                        Some(v) => {
                            match data::RuleExpressionLoopKind::to_loop_kind(v.value.to_string()) {
                                Some(v2) => {
                                    token_i += 1;
                                    v2
                                },
                                None => data::RuleExpressionLoopKind::One,
                            }
                        },
                        None => data::RuleExpressionLoopKind::One,
                    };

                    tmp_exprs.push(data::RuleExpression::new(each_token.line, data::RuleExpressionKind::ID, loop_kind, previous_lookahead_kind.clone(), format!("{}.{}", each_token.value, second_id)));
                    previous_lookahead_kind = data::RuleExpressionLookaheadKind::None;
                    continue;
                }

                token_i += 1;

                // 3つ目の ID を取得
                let third_id = match tokens.get(token_i) {
                    Some(v) => {
                        if v.kind != data::TokenKind::ID {
                            return Err(BlockParseError::UnexpectedToken(each_token.line, v.value.to_string(), "ID".to_string()));
                        }

                        v.value.to_string()
                    },
                    None => return Err(BlockParseError::UnexpectedEOF(each_token.line, "ID".to_string())),
                };

                token_i += 1;

                let loop_kind = match tokens.get(token_i) {
                    Some(v) => {
                        match data::RuleExpressionLoopKind::to_loop_kind(v.value.to_string()) {
                            Some(v2) => {
                                token_i += 1;
                                v2
                            },
                            None => data::RuleExpressionLoopKind::One,
                        }
                    },
                    None => data::RuleExpressionLoopKind::One,
                };

                tmp_exprs.push(data::RuleExpression::new(each_token.line, data::RuleExpressionKind::ID, loop_kind, previous_lookahead_kind.clone(), format!("{}.{}.{}", each_token.value, second_id, third_id)));
                previous_lookahead_kind = data::RuleExpressionLookaheadKind::None;
                continue;
            }

            // 文字列
            if each_token.kind == data::TokenKind::String {
                tmp_exprs.push(data::RuleExpression::new(each_token.line, data::RuleExpressionKind::String, previous_loop_kind.clone(), previous_lookahead_kind.clone(), each_token.value.to_string()));

                previous_lookahead_kind = data::RuleExpressionLookaheadKind::None;
                token_i += 1;

                continue;
            }

            // 記号
            if each_token.kind == data::TokenKind::Symbol {
                // ワイルドカード
                if each_token.value == "." {
                    tmp_exprs.push(data::RuleExpression::new(each_token.line, data::RuleExpressionKind::Wildcard, previous_loop_kind.clone(), previous_lookahead_kind.clone(), each_token.value.to_string()));

                    previous_lookahead_kind = data::RuleExpressionLookaheadKind::None;
                    token_i += 1;

                    continue;
                }

                // 括弧開始
                if each_token.value == "(" {
                    // ループ文字が括弧開始の後ろに来た場合は構文エラー
                    if previous_loop_kind != data::RuleExpressionLoopKind::One {
                        return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "expression and some symbols".to_string()));
                    }

                    if is_in_paren {
                        return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "')'".to_string()));
                    }

                    if tmp_exprs.len() != 0 {
                        tmp_seqs.push(data::RuleSequence::new(tmp_exprs.clone()));
                        tmp_exprs.clear();
                    }

                    if tmp_seqs.len() != 0 {
                        tmp_seq_groups.push(data::RuleSequenceGroup::new(tmp_seqs.clone(), data::RuleExpressionLoopKind::One, data::RuleExpressionLookaheadKind::None));
                        tmp_seqs.clear();
                    }

                    // 先読みの種類を括弧終了時に使えるようにキャッシュを取る
                    previous_group_seq_lookahead_kind = previous_lookahead_kind.clone();
                    previous_lookahead_kind = data::RuleExpressionLookaheadKind::None;

                    is_in_paren = true;
                    token_i += 1;

                    continue;
                }

                // 括弧終了
                if each_token.value == ")" {
                    // 先読み文字が括弧終了の前に来た場合は構文エラー
                    if previous_lookahead_kind != data::RuleExpressionLookaheadKind::None {
                        return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "expression and '('".to_string()));
                    }

                    if !is_in_paren {
                        return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "'('".to_string()));
                    }

                    if tmp_exprs.len() != 0 {
                        tmp_seqs.push(data::RuleSequence::new(tmp_exprs.clone()));
                        tmp_exprs.clear();
                    } else {
                        return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "expression".to_string()));
                    }

                    tmp_seq_groups.push(data::RuleSequenceGroup::new(tmp_seqs.clone(), previous_loop_kind.clone(), previous_group_seq_lookahead_kind.clone()));
                    previous_group_seq_lookahead_kind = data::RuleExpressionLookaheadKind::None;

                    tmp_seqs.clear();

                    is_in_paren = false;
                    token_i += 1;

                    continue;
                }

                // 選択区切り
                if each_token.value == ":" {
                    // 先読み文字が区切り文字の前に来た場合は構文エラー
                    if previous_lookahead_kind != data::RuleExpressionLookaheadKind::None {
                        return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "expression and '('".to_string()));
                    }

                    // 括弧内で選択の区切りはできないため構文エラー
                    if is_in_paren {
                        return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "expression".to_string()));
                    }

                    if tmp_exprs.len() != 0 {
                        tmp_seqs.push(data::RuleSequence::new(tmp_exprs.clone()));
                        tmp_exprs.clear();
                    }

                    if tmp_seqs.len() != 0 {
                        tmp_seq_groups.push(data::RuleSequenceGroup::new(tmp_seqs.clone(), data::RuleExpressionLoopKind::One, data::RuleExpressionLookaheadKind::None));
                        tmp_seqs.clear();
                    }

                    if tmp_seq_groups.len() == 0 {
                        return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "expression".to_string()));
                    }

                    choices.push(data::RuleChoice::new(tmp_seq_groups.clone()));
                    tmp_seq_groups.clear();

                    token_i += 1;

                    continue;
                }

                // 肯定先読み
                if each_token.value == "&" {
                    // 先読み文字が先読み文字の前に来た場合は構文エラー
                    if previous_lookahead_kind != data::RuleExpressionLookaheadKind::None {
                        return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "expression and '('".to_string()));
                    }

                    previous_lookahead_kind = data::RuleExpressionLookaheadKind::Positive;
                    token_i += 1;

                    continue;
                }

                // 否定先読み
                if each_token.value == "!" {
                    // 先読み文字が先読み文字の前に来た場合は構文エラー
                    if previous_lookahead_kind != data::RuleExpressionLookaheadKind::None {
                        return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "expression and '('".to_string()));
                    }

                    previous_lookahead_kind = data::RuleExpressionLookaheadKind::Negative;
                    token_i += 1;

                    continue;
                }
            }

            // マッチするトークンがない場合は構文エラー
            return Err(BlockParseError::UnexpectedToken(each_token.line, each_token.value.to_string(), "expression and some symbols".to_string()));
        }

        // トークンの長さは最初にチェックしているので unwrap() できる
        let last_token_line_num = tokens.get(tokens.len() - 1).unwrap().line;

        // 最後に先読み文字が来た場合は構文エラー
        if previous_lookahead_kind != data::RuleExpressionLookaheadKind::None {
            return Err(BlockParseError::UnexpectedToken(last_token_line_num, previous_lookahead_kind.to_symbol_string(), "expression and '('".to_string()));
        }

        // 最後まで括弧が閉じられていない場合はエラー
        if is_in_paren {
            return Err(BlockParseError::UnexpectedToken(last_token_line_num, ",".to_string(), "')'".to_string()));
        }

        // 最後に残った expression などを choice に追加する

        if tmp_exprs.len() == 0 && tmp_seq_groups.len() == 0 {
            return Err(BlockParseError::UnexpectedToken(tokens.get(tokens.len() - 1).unwrap().line, ",".to_string(), "expression".to_string()));
        }

        if tmp_exprs.len() != 0 {
            tmp_seqs.push(data::RuleSequence::new(tmp_exprs.clone()));
            tmp_exprs.clear();
        }

        if tmp_seqs.len() != 0 {
            tmp_seq_groups.push(data::RuleSequenceGroup::new(tmp_seqs.clone(), data::RuleExpressionLoopKind::One, data::RuleExpressionLookaheadKind::None));
            tmp_seqs.clear();
        }

        if tmp_seq_groups.len() != 0 {
            choices.push(data::RuleChoice::new(tmp_seq_groups.clone()));
        }

        if choices.len() == 0 {
            return Err(BlockParseError::RuleHasNoChoice(rule_name.to_string()));
        }

        return Ok(data::Rule::new(rule_name.to_string(), choices));
    }
}
