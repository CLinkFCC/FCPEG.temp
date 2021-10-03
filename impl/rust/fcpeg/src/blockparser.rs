use crate::data;
use crate::rule;
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
    InvalidToken(usize, String),
    MainBlockNotFound(),
    NoChoiceOrExpressionContent(usize),
    NoStartCmdInMainBlock(),
    RuleHasNoChoice(String),
    RuleInMainBlock(),
    StartCmdOutsideMainBlock(),
    TooBigNumber(usize, String),
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
            BlockParseError::BlockAliasNotFound(line, block_alias_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("block alias '{}' not found", block_alias_name), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::DuplicatedBlockAliasName(line, block_alias_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("duplicated block alias name '{}'", block_alias_name), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::DuplicatedBlockName(line, block_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("duplicated block name '{}'", block_name), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::DuplicatedStartCmd() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "duplicated start command", vec![], vec![]),
            BlockParseError::ExpectedBlockDef(line) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "expected block definition", vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::ExpectedToken(line, expected_str) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("expected token {}", expected_str), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::InternalErr(err_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("internal error: {}", err_name), vec![], vec![]),
            BlockParseError::InvalidCharClassFormat(line, value) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("invalid character class format '{}'", value), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::InvalidToken(line, value) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("invalid token '{}'", value), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::MainBlockNotFound() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "main block not found", vec![], vec![]),
            BlockParseError::NoChoiceOrExpressionContent(line) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "no choice or expression content", vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::NoStartCmdInMainBlock() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "no start command in main block", vec![], vec![]),
            BlockParseError::RuleHasNoChoice(rule_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("rule '{}' has no choice", rule_name), vec![], vec![]),
            BlockParseError::RuleInMainBlock() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "rule in main block", vec![], vec![]),
            BlockParseError::StartCmdOutsideMainBlock() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "start command outside main block", vec![], vec![]),
            BlockParseError::TooBigNumber(line, number) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("too big number {}", number), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::UnexpectedEOF(line, expected_str) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unexpected EOF, expected {}", expected_str), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::UnexpectedToken(line, unexpected_token, expected_str) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unexpected token '{}', expected {}", unexpected_token, expected_str), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::UnknownPragmaName(line, unknown_pragma_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown pragma name", vec![format!("line:\t{}", line + 1), format!("pragma name:\t{}", unknown_pragma_name)], vec![]),
            BlockParseError::UnknownSyntax(line, target_token) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown syntax", vec![format!("line: {}", line + 1), format!("target token:\t'{}'", target_token)], vec![]),
            BlockParseError::UnknownToken(line, unknown_token) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unknown token '{}'", unknown_token), vec![format!("line:\t{}", line + 1)], vec![]),
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

        if !cfg!(debug) {
            self.setting_file.print();
        }

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

        if !cfg!(debug) {
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
        }

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

        let id_num_regex = regex::Regex::new(r"[a-zA-Z0-9_]").unwrap();
        let mut tmp_id_num = "".to_string();

        let symbol_regex = regex::Regex::new(r"[#&!?\-+*.,:^(){}<>]").unwrap();

        let mut line_i: usize = 0;

        let chars: Vec<char> = src_content.chars().collect();
        let mut char_i: usize = 0;

        while char_i < chars.len() {
            let each_char = chars.get(char_i).unwrap();
            let each_char_str = each_char.to_string();

            // ID, 数値判定
            if id_num_regex.is_match(&each_char_str) {
                tmp_id_num.push(*each_char);
                char_i += 1;
                continue;
            } else if tmp_id_num.len() != 0 {
                // トークンの種類を判定
                let token_kind = match tmp_id_num.parse::<i32>() {
                    Err(e) => {
                        // 数値が大きすぎる場合はエラー
                        if e.to_string() == "number too large to fit in target type" {
                            return Err(BlockParseError::TooBigNumber(line_i, tmp_id_num));
                        }

                        // それ以外の場合は ID と判定する
                        data::TokenKind::ID
                    },
                    Ok(_v) => data::TokenKind::Number,
                };

                tokens.push(data::Token::new(line_i, token_kind, tmp_id_num.to_string()));
                tmp_id_num = "".to_string();
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
                            '"' => '"',
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

            if !cfg!(debug) {
                println!("+ {} {}", *each_char as i32, *each_char);
            }

            return Err(BlockParseError::UnknownToken(line_i, each_char.to_string()));
        }

        // 最後に tmp_id が残っていないかチェック
        if tmp_id_num.len() != 0 {
            // トークンの種類を判定
            let token_kind = match tmp_id_num.parse::<i32>() {
                Err(_e) => data::TokenKind::ID,
                Ok(_v) => data::TokenKind::Number,
            };

            tokens.push(data::Token::new(line_i, token_kind, tmp_id_num.to_string()));
        }

        if !cfg!(debug) {
            for (token_i, each_token) in tokens.iter().enumerate() {
                println!("[{};{}] {}\t\t{}", each_token.line, token_i, each_token.value, each_token.kind);
            }
        }

        return Ok(tokens);
    }

    // フィールドが初期化されるためブロックパーサのインスタンスを使い回せる
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

                // 丸括弧
                let mut paren_nest = 0;
                // 中括弧
                let mut brace_nest = 0;

                while self.token_i < self.tokens.len() {
                    let next_token = self.tokens.get(self.token_i).unwrap();

                    if next_token.kind == data::TokenKind::Symbol {
                        match next_token.value.as_str() {
                            "(" => {
                                paren_nest += 1;
                            },
                            ")" => {
                                if paren_nest == 0 {
                                    return Err(BlockParseError::UnexpectedToken(next_token.line, next_token.value.to_string(), "'('".to_string()));
                                }

                                paren_nest -= 1;
                            },
                            "{" => {
                                brace_nest += 1;
                            },
                            "}" => {
                                if brace_nest == 0 {
                                    return Err(BlockParseError::UnexpectedToken(next_token.line, next_token.value.to_string(), "'{'".to_string()));
                                }

                                brace_nest -= 1;
                            },
                            _ => (),
                        }
                    }

                    if next_token.kind == data::TokenKind::Symbol && next_token.value == "," && paren_nest == 0 && brace_nest == 0 {
                        self.token_i += 1;

                        let cmd = self.get_command_from_data(next_token.line, "define".to_string(), pragma_args)?;
                        return Ok(Some(cmd));
                    }

                    pragma_args.push(next_token.clone());
                    self.token_i += 1;
                }

                return Err(BlockParseError::ExpectedToken(each_token.line, "','".to_string()));
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

                let rule_name = pragma_args.get(0).unwrap().value.to_string();
                let choices = BlockParser::get_choice_vec(line_num, rule_name.to_string(), &pragma_args[1..].to_vec())?;
                let rule = rule::Rule::new(rule_name.to_string(), choices);
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

                let mut arg_i = 0usize;

                // ブロック名を取得

                let block_name_token = pragma_args.get(0).unwrap();

                if block_name_token.kind != data::TokenKind::ID {
                    return Err(BlockParseError::UnexpectedToken(line_num, block_name_token.value.to_string(), "ID".to_string()));
                }

                let mut file_alias_name = self.file_alias_name.to_string();
                let mut block_name = block_name_token.value.to_string();

                arg_i += 1;

                match pragma_args.get(arg_i) {
                    Some(period_token) => {
                        if period_token.kind == data::TokenKind::Symbol && period_token.value == "." {
                            arg_i += 1;

                            match pragma_args.get(arg_i) {
                                Some(id_token) => {
                                    if id_token.kind != data::TokenKind::ID {
                                        return Err(BlockParseError::UnexpectedToken(line_num, id_token.value.clone(), "ID".to_string()));
                                    }

                                    file_alias_name = block_name;
                                    block_name = id_token.value.to_string();
                                    arg_i += 1;
                                },
                                None => (),
                            }
                        }
                    },
                    None => (),
                }

                // ブロックエイリアス名を取得

                let mut block_alias_name = block_name.to_string();

                match pragma_args.get(arg_i) {
                    Some(v) => {
                        if v.kind != data::TokenKind::ID || v.value != "as" {
                            return Err(BlockParseError::UnexpectedToken(line_num, v.value.clone(), "'as'".to_string()));
                        }

                        arg_i += 1;

                        match pragma_args.get(arg_i) {
                            Some(v) => {
                                if v.kind != data::TokenKind::ID {
                                    return Err(BlockParseError::UnexpectedToken(line_num, v.value.clone(), "ID".to_string()));
                                }

                                block_alias_name = v.value.to_string();
                            },
                            None => return Err(BlockParseError::ExpectedToken(line_num, "ID".to_string())),
                        }
                    },
                    None => (),
                }

                data::Command::Use(line_num, file_alias_name, block_name, block_alias_name)
            },
            _ => return Err(BlockParseError::UnknownPragmaName(line_num, pragma_name.to_string())),
        };

        return Ok(cmd);
    }

    fn get_choice_vec(line_num: usize, rule_name: String, tokens: &Vec<data::Token>) -> std::result::Result<Vec<Box<rule::RuleChoice>>, BlockParseError> {
        if tokens.len() == 0 {
            return Err(BlockParseError::RuleHasNoChoice(rule_name.to_string()));
        }

        let mut choices = Vec::<Box<rule::RuleChoice>>::new();
        let primitive_choice = rule::RuleChoice::new(rule::RuleLookaheadKind::None, (1, 1), None, false, (1, 1), false);

        let mut token_i = 0;
        let mut choice_start_i = 0;

        let mut paren_nest = 0usize;
        let mut brace_nest = 0usize;

        let mut is_random_order_syntax = std::option::Option::<bool>::None;

        while token_i < tokens.len() {
            let each_token = tokens.get(token_i).unwrap();

            if each_token.kind == data::TokenKind::Symbol {
                match each_token.value.as_str() {
                    ":" => {
                        if paren_nest == 0 && brace_nest == 0 {
                            match is_random_order_syntax {
                                Some(v) => {
                                    if v {
                                        return Err(BlockParseError::UnexpectedToken(line_num, each_token.value.to_string(), ",".to_string()));
                                    }
                                },
                                None => is_random_order_syntax = Some(false),
                            }

                            let mut choice_tokens = tokens[choice_start_i..token_i].to_vec();
                            let mut new_choice = primitive_choice.clone();
                            BlockParser::get_choice(line_num, rule_name.to_string(), &mut new_choice, &mut choice_tokens)?;
                            choices.push(Box::new(new_choice));
                            choice_start_i = token_i + 1;
                        }
                    },
                    "," => {
                        if paren_nest == 0 && brace_nest == 0 {
                            match is_random_order_syntax {
                                Some(v) => {
                                    if !v {
                                        return Err(BlockParseError::UnexpectedToken(line_num, each_token.value.to_string(), ":".to_string()));
                                    }
                                },
                                None => is_random_order_syntax = Some(true),
                            }

                            let mut choice_tokens = tokens[choice_start_i..token_i].to_vec();
                            let mut new_choice = primitive_choice.clone();
                            BlockParser::get_choice(line_num, rule_name.to_string(), &mut new_choice, &mut choice_tokens)?;
                            choices.push(Box::new(new_choice));
                            choice_start_i = token_i + 1;
                        }
                    },
                    "(" => {
                        paren_nest += 1;
                    },
                    ")" => {
                        if paren_nest == 0 {
                            return Err(BlockParseError::UnexpectedToken(line_num, each_token.value.to_string(), "'('".to_string()));
                        }

                        paren_nest -= 1;
                    },
                    "{" => {
                        brace_nest += 1;
                    },
                    "}" => {
                        if brace_nest == 0 {
                            return Err(BlockParseError::UnexpectedToken(line_num, each_token.value.to_string(), "'{'".to_string()));
                        }

                        brace_nest -= 1;
                    },
                    _ => (),
                }
            }

            token_i += 1;
        }

        if paren_nest != 0 {
            // 最後まで閉じ括弧がなければ構文エラー
            return Err(BlockParseError::ExpectedToken(line_num, "')'".to_string()));
        }

        let mut choice_tokens = tokens[choice_start_i..tokens.len()].to_vec();
        let mut new_choice = primitive_choice;
        BlockParser::get_choice(line_num, rule_name.to_string(), &mut new_choice, &mut choice_tokens)?;
        choices.push(Box::new(new_choice));
        return Ok(choices);
    }

    fn get_elem_tokens(tokens: &Vec<data::Token>) -> std::result::Result<Vec<Vec<data::Token>>, BlockParseError> {
        let mut token_i = 0;

        let mut elem_tokens = Vec::<Vec<data::Token>>::new();
        let mut elem_start_i = 0;

        let mut paren_nest: usize = 0;

        while token_i < tokens.len() {
            let each_token = tokens.get(token_i).unwrap();

            if each_token.kind == data::TokenKind::Space {
                if paren_nest == 0 {
                    // トークン列を追加

                    let new_elem_tokens = tokens[elem_start_i..token_i].to_vec();

                    if new_elem_tokens.len() != 0 {
                        elem_tokens.push(new_elem_tokens);
                        elem_start_i = token_i + 1;
                    }
                }
            }

            if each_token.kind == data::TokenKind::Symbol {
                match each_token.value.as_str() {
                    "(" => {
                        paren_nest += 1;
                    },
                    ")" => {
                        paren_nest -= 1;
                    },
                    _ => (),
                }
            }

            token_i += 1;

            if token_i >= tokens.len() {
                // 残ったトークン列を追加

                let new_elem_tokens = tokens[elem_start_i..tokens.len()].to_vec();

                if new_elem_tokens.len() != 0 {
                    elem_tokens.push(new_elem_tokens);
                }
            }
        }

        // println!();
        // println!("--- elem ranges ---");

        // for each_token_vec in &elem_tokens {
        //     for each_token in each_token_vec {
        //         print!("{},", each_token.value);
        //     }

        //     println!();
        // }

        return Ok(elem_tokens);
    }

    // arg: tokens: 両端のスペースは削除される
    // note: 実際には choice と expr 両方の解析をする?
    fn get_choice(line_num: usize, rule_name: String, choice: &mut rule::RuleChoice, tokens: &mut Vec<data::Token>) -> std::result::Result<(), BlockParseError> {
        // 最初にスペースがあれば削除
        match tokens.get(0) {
            Some(v) => {
                if v.kind == data::TokenKind::Space {
                    tokens.remove(0);
                }

                if tokens.len() == 0 {
                    return Err(BlockParseError::NoChoiceOrExpressionContent(line_num));
                }
            },
            None => return Err(BlockParseError::NoChoiceOrExpressionContent(line_num)),
        }

        // 最後にスペースがあれば削除
        match tokens.get(tokens.len() - 1) {
            Some(v) => {
                if v.kind == data::TokenKind::Space {
                    tokens.pop();
                }

                if tokens.len() == 0 {
                    return Err(BlockParseError::NoChoiceOrExpressionContent(line_num));
                }
            },
            None => return Err(BlockParseError::NoChoiceOrExpressionContent(line_num)),
        }

        // トークン列を要素ごとに分割する
        let elem_tokens = BlockParser::get_elem_tokens(tokens)?;

        for each_tokens in &elem_tokens {
            let starts_with_open_paren = match each_tokens.get(0) {
                Some(v) => v.kind == data::TokenKind::Symbol && v.value == "(",
                None => false,
            };

            // 記号を処理 -> expr や choice を取得

            let mut token_i: usize = 0;

            let lookahead_kind = match each_tokens.get(token_i) {
                Some(v) => {
                    let kind = rule::RuleLookaheadKind::to_lookahead_kind(&v.value);

                    if kind != rule::RuleLookaheadKind::None {
                        token_i += 1;
                    }

                    kind
                },
                None => rule::RuleLookaheadKind::None,
            };

            let content_start_i = token_i;
            let mut content_end_i = each_tokens.len();

            let mut loop_count = (1i32, 1i32);

            let mut is_choice = false;
            let mut has_choices = false;
            let mut is_random_order_syntax = false;
            let mut paren_nest = 0;

            let mut ast_reflect = Option::<String>::None;

            let mut tmp_token_i = token_i;

            while tmp_token_i < each_tokens.len() {
                let token = each_tokens.get(tmp_token_i).unwrap();
                tmp_token_i += 1;

                let tmp_loop_count = rule::RuleCountConverter::loop_symbol_to_count(&token.value);

                if tmp_loop_count != (1, 1) {
                    if paren_nest == 0 {
                        loop_count = tmp_loop_count;
                        content_end_i -= 1;
                        token_i += 1;
                        break;
                    }
                } else if token.kind == data::TokenKind::Symbol {
                    match token.value.as_str() {
                        "(" => {
                            is_choice = true;
                            paren_nest += 1;
                        },
                        ")" => {
                            paren_nest -= 1;
                        },
                        ":" => {
                            if paren_nest == if starts_with_open_paren { 1 } else { 0 } {
                                is_random_order_syntax = false;
                                has_choices = true;
                            }
                        },
                        "," => {
                            if paren_nest == if starts_with_open_paren { 1 } else { 0 } {
                                is_random_order_syntax = true;
                                has_choices = true;
                            }
                        },
                        _ => (),
                    }
                }
            }

            if loop_count != (1, 1) {
                token_i = tmp_token_i;
            }

            let mut is_random_order = false;
            let mut occurrence_count = (1i32, 1i32);

            paren_nest = 0;

            while token_i < each_tokens.len() {
                match each_tokens.get(token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Symbol {
                            match v.value.as_str() {
                                "^" => {
                                    if paren_nest != 0 {
                                        token_i += 1;
                                        continue;
                                    }

                                    is_random_order = true;
                                    content_end_i -= 1;
                                    token_i += 1;

                                    match each_tokens.get(token_i) {
                                        Some(v) => {
                                            if v.kind != data::TokenKind::StringInBracket {
                                                return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "string in bracket".to_string()));
                                            }

                                            let nums = v.value[1..v.value.len() - 1].split("-").collect::<Vec<&str>>();

                                            match nums.len() {
                                                1 => {
                                                    let arg = nums.get(0).unwrap();

                                                    if arg.len() != 0 {
                                                        occurrence_count = match arg.parse::<i32>() {
                                                            Err(_e) => return Err(BlockParseError::InvalidToken(line_num, v.value.to_string())),
                                                            Ok(v) => (v, v),
                                                        };
                                                    }
                                                },
                                                2 => {
                                                    let mut occurrence_min_count = 0i32;
                                                    let mut occurrence_max_count = -1i32;

                                                    let left_arg = nums.get(0).unwrap();
                                                    let right_arg = nums.get(1).unwrap();

                                                    if left_arg.len() != 0 {
                                                        occurrence_min_count = match left_arg.parse::<i32>() {
                                                            Err(_e) => return Err(BlockParseError::InvalidToken(line_num, v.value.to_string())),
                                                            Ok(v) => v,
                                                        };
                                                    }

                                                    if right_arg.len() != 0 {
                                                        occurrence_max_count = match right_arg.parse::<i32>() {
                                                            Err(_e) => return Err(BlockParseError::InvalidToken(line_num, v.value.to_string())),
                                                            Ok(v) => v,
                                                        };
                                                    }

                                                    occurrence_count = (occurrence_min_count, occurrence_max_count);
                                                },
                                                _ => return Err(BlockParseError::InvalidToken(line_num, v.value.to_string())),
                                            }

                                            content_end_i -= 1;
                                            token_i += 1;
                                        },
                                        None => break,
                                    }

                                    continue;
                                },
                                "#" => {
                                    if paren_nest != 0 {
                                        token_i += 1;
                                        continue;
                                    }

                                    token_i += 1;
                                    content_end_i -= 1;

                                    // ID が続いていればノード名として保持
                                    ast_reflect = match each_tokens.get(token_i) {
                                        Some(v) if v.kind == data::TokenKind::ID => {
                                            token_i += 1;
                                            content_end_i -= 1;
                                            Some(v.value.clone())
                                        },
                                        _ => Some(String::new()),
                                    }
                                }
                                "{" => {
                                    if paren_nest != 0 {
                                        token_i += 1;
                                        continue;
                                    }

                                    if loop_count != (1, 1) {
                                        return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "nothing".to_string()));
                                    }

                                    content_end_i -= 1;
                                    token_i += 1;

                                    let next_token = match each_tokens.get(token_i + 1) {
                                        Some(v) => v,
                                        None => return Err(BlockParseError::ExpectedToken(line_num, "number".to_string())),
                                    };

                                    // 先のトークンが '}' であれば単体の数値が指定されたものとして扱う
                                    if next_token.kind == data::TokenKind::Symbol && next_token.value == "}" {
                                        match each_tokens.get(token_i) {
                                            Some(num_token) => {
                                                if num_token.kind != data::TokenKind::Number {
                                                    return Err(BlockParseError::UnexpectedToken(line_num, num_token.value.to_string(), "number".to_string()));
                                                }
        
                                                let conved_num = num_token.value.parse::<i32>().unwrap();
                                                loop_count = (conved_num, conved_num);
        
                                                content_end_i -= 2;
                                                token_i += 2;
                                            },
                                            None => return Err(BlockParseError::ExpectedToken(line_num, "number".to_string())),
                                        }
                                    } else {
                                        let loop_min_count;
                                        let loop_max_count;

                                        match each_tokens.get(token_i) {
                                            Some(v) => {
                                                if v.kind == data::TokenKind::Number {
                                                    loop_min_count = v.value.parse::<i32>().unwrap();
                                                    content_end_i -= 1;
                                                    token_i += 1;
                                                } else {
                                                    if v.kind != data::TokenKind::Symbol || v.value != "," {
                                                        return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "','".to_string()));
                                                    }

                                                    loop_min_count = 0;
                                                }
                                            },
                                            None => return Err(BlockParseError::ExpectedToken(line_num, "number".to_string())),
                                        }

                                        match each_tokens.get(token_i) {
                                            Some(v) => {
                                                if v.kind != data::TokenKind::Symbol || v.value != "," {
                                                    return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "','".to_string()));
                                                }

                                                content_end_i -= 1;
                                            },
                                            None => return Err(BlockParseError::ExpectedToken(line_num, "','".to_string())),
                                        }

                                        token_i += 1;

                                        match each_tokens.get(token_i) {
                                            Some(v) => {
                                                if v.kind == data::TokenKind::Number {
                                                    loop_max_count = v.value.parse::<i32>().unwrap();
                                                    content_end_i -= 1;
                                                    token_i += 1;
                                                } else {
                                                    if v.kind != data::TokenKind::Symbol || v.value != "}" {
                                                        return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "'}'".to_string()));
                                                    }

                                                    loop_max_count = -1;
                                                }
                                            },
                                            None => return Err(BlockParseError::ExpectedToken(line_num, "number".to_string())),
                                        }

                                        match each_tokens.get(token_i) {
                                            Some(v) => {
                                                if v.kind != data::TokenKind::Symbol || v.value != "}" {
                                                    return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "'}'".to_string()));
                                                }

                                                content_end_i -= 1;
                                            },
                                            None => return Err(BlockParseError::ExpectedToken(line_num, "'}'".to_string())),
                                        }

                                        loop_count = (loop_min_count, loop_max_count);
                                        token_i += 1;
                                    }

                                    continue;
                                },
                                "(" => {
                                    paren_nest += 1;
                                    token_i += 1;
                                },
                                ")" => {
                                    paren_nest -= 1;
                                    token_i += 1;
                                },
                                _ => {
                                    token_i += 1;
                                }
                            }

                            continue;
                        }

                        token_i += 1;
                    },
                    None => (),
                }
            }

            if token_i != each_tokens.len() {
                if !cfg!(debug) {
                    println!("{} {}", token_i, each_tokens.len());
                }

                let unexpected_token = each_tokens.get(token_i).unwrap();
                return Err(BlockParseError::UnexpectedToken(line_num, unexpected_token.value.to_string(), "'^', '{', etc".to_string()));
            }

            let content_tokens = each_tokens[content_start_i..content_end_i].to_vec();

            if content_tokens.len() == 0 {
                return Err(BlockParseError::NoChoiceOrExpressionContent(line_num));
            }

            if !cfg!(debug) {
                print!("-- {} ", lookahead_kind.to_symbol_string());
                for tk in &content_tokens {
                    print!("{},", tk.value);
                }
                print!(" {}", rule::RuleCountConverter::count_to_string(&loop_count, true, "{", ",", "}"));
                print!(" {}", if is_random_order { "^" } else { "" });
                print!(" {}", rule::RuleCountConverter::count_to_string(&occurrence_count, false, "[", "-", "]"));
                print!(" {}", match &ast_reflect { Some(v) => format!("#{}", v), None => String::new() });
                println!();
            }

            if is_choice {
                if has_choices && is_random_order != is_random_order_syntax {
                    let (unexpected_token, expected_token) = if is_random_order_syntax {
                        (",", "':'")
                    } else {
                        (":", "','")
                    };

                    return Err(BlockParseError::UnexpectedToken(line_num, unexpected_token.to_string(), expected_token.to_string()));
                }

                let mut new_choice = rule::RuleChoice::new(lookahead_kind, loop_count, ast_reflect.clone(), is_random_order, occurrence_count, has_choices);
                // 選択の括弧などを取り除いてから渡す
                let choice_tokens = &each_tokens[content_start_i + 1..content_end_i - 1].to_vec();

                if !cfg!(debug) {
                    print!("*choice: ");
                    for each_token in choice_tokens {
                        print!("{},", each_token.value);
                    }
                    println!();
                }

                let sub_choices = BlockParser::get_choice_vec(line_num, rule_name.to_string(), choice_tokens)?;

                match rule::RuleChoice::is_hierarchy_omission_needed(&sub_choices, is_random_order) {
                    Some(v) if loop_count == (1, 1) => {
                        new_choice.elem_containers = v.elem_containers;
                        new_choice.has_choices = v.has_choices;
                    },
                    _ => {
                        for each_choice in sub_choices {
                            new_choice.elem_containers.push(rule::RuleElementContainer::RuleChoice(each_choice));
                        }
                    }
                }

                choice.elem_containers.push(rule::RuleElementContainer::RuleChoice(Box::new(new_choice)));
            } else {
                if is_random_order {
                    return Err(BlockParseError::UnexpectedToken(line_num, "^".to_string(), "nothing".to_string()));
                }

                let expr_tokens = each_tokens[content_start_i..content_end_i].to_vec();

                if !cfg!(debug) {
                    print!("*expr: ");
                    for each_token in &expr_tokens {
                        print!("{},", each_token.value);
                    }
                    println!(" ({}:{}~{})", expr_tokens.get(0).unwrap().kind, content_start_i, content_end_i);
                }

                let new_expr = BlockParser::get_expr(line_num, lookahead_kind, loop_count, ast_reflect, expr_tokens)?;
                choice.elem_containers.push(rule::RuleElementContainer::RuleExpression(Box::new(new_expr)));
            }
        }

        return Ok(());
    }

    fn get_expr(line_num: usize, lookahead_kind: rule::RuleLookaheadKind, loop_count: (i32, i32), ast_reflect: Option<String>, tokens: Vec<data::Token>) -> std::result::Result<rule::RuleExpression, BlockParseError> {
        if tokens.len() == 0 {
            return Err(BlockParseError::ExpectedToken(line_num, "id".to_string()));
        }

        let first_token = tokens.get(0).unwrap();

        let new_expr = match first_token.kind {
            data::TokenKind::ID => {
                let mut id = first_token.value.to_string();
                let mut token_i = 1;

                while token_i < tokens.len() {
                    match tokens.get(token_i) {
                        Some(v) => {
                            if v.kind == data::TokenKind::Symbol && v.value == "." {
                                match tokens.get(token_i + 1) {
                                    Some(v) => {
                                        if v.kind == data::TokenKind::ID {
                                            id += &format!(".{}", v.value);
                                            token_i += 2;
                                        } else {
                                            return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "id".to_string()));
                                        }
                                    },
                                    None => return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "id".to_string())),
                                }
                            } else {
                                return Err(BlockParseError::UnexpectedToken(line_num, v.value.to_string(), "'.'".to_string()));
                            }
                        },
                        None => break,
                    }
                }

                rule::RuleExpression::new(line_num, rule::RuleExpressionKind::ID, lookahead_kind, loop_count, ast_reflect, id)
            },
            data::TokenKind::String => {
                if tokens.len() >= 2 {
                    let unexpected_token = tokens.get(1).unwrap();
                    return Err(BlockParseError::UnexpectedToken(line_num, unexpected_token.value.to_string(), "spacing, ':' and ','".to_string()));
                }

                let value = first_token.value[1..first_token.value.len() - 1].to_string();
                rule::RuleExpression::new(line_num, rule::RuleExpressionKind::String, lookahead_kind, loop_count, ast_reflect, value)
            },
            data::TokenKind::StringInBracket => {
                if tokens.len() >= 2 {
                    let unexpected_token = tokens.get(1).unwrap();
                    return Err(BlockParseError::UnexpectedToken(line_num, unexpected_token.value.to_string(), "spacing, ':' and ','".to_string()));
                }

                rule::RuleExpression::new(line_num, rule::RuleExpressionKind::CharClass, lookahead_kind, loop_count, ast_reflect, first_token.value.to_string())
            },
            data::TokenKind::Symbol => {
                if tokens.len() >= 2 {
                    let unexpected_token = tokens.get(1).unwrap();
                    return Err(BlockParseError::UnexpectedToken(line_num, unexpected_token.value.to_string(), "spacing, ':' and ','".to_string()));
                }

                if first_token.value != "." {
                    return Err(BlockParseError::UnexpectedToken(line_num, first_token.value.to_string(), "'.'".to_string()));
                }

                rule::RuleExpression::new(line_num, rule::RuleExpressionKind::Wildcard, lookahead_kind, loop_count, ast_reflect, ".".to_string())
            },
            _ => return Err(BlockParseError::UnexpectedToken(line_num, first_token.value.to_string(), "expression".to_string())),
        };

        return Ok(new_expr);
    }
}
