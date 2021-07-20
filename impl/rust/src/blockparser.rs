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
    DuplicatedBlockName(usize, String),
    ExpectedBlockDef(usize),
    ExpectedToken(usize, String),
    InternalErr(String),
    UnexpectedEOF(usize, String),
    UnexpectedToken(usize, String, String),
    UnknownPragmaName(usize, String),
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
            BlockSyntaxError::DuplicatedBlockName(line, block_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("duplicated block name '{}'", block_name), vec![format!("line: {}", line + 1)], vec![]),
            BlockSyntaxError::ExpectedBlockDef(line) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "expected block definition", vec![format!("line: {}", line + 1)], vec![]),
            BlockSyntaxError::ExpectedToken(line, expected_str) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("expected token {}", expected_str), vec![format!("line: {}", line + 1)], vec![]),
            BlockSyntaxError::InternalErr(err_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("internal error: {}", err_name), vec![], vec![]),
            BlockSyntaxError::UnexpectedEOF(line, expected_str) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unexpected EOF, expected {}", expected_str), vec![format!("line: {}", line + 1)], vec![]),
            BlockSyntaxError::UnexpectedToken(line, unexpected_token, expected_str) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unexpected token '{}', expected {}", unexpected_token, expected_str), vec![format!("line: {}", line + 1)], vec![]),
            BlockSyntaxError::UnknownPragmaName(line, unknown_pragma_name) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unknown pragma name"), vec![format!("line: {}", line + 1), format!("pragma name: {}", unknown_pragma_name)], vec![]),
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
            println!();
            println!("parsing all: {} {}", alias_name, alias_item.0);
            println!();
            println!();

            let tokens = BlockParser::get_tokens(&alias_item.1)?;
            let blocks = block_parser.parse(tokens)?;

            for (block_name, each_block) in &blocks {
                println!("block {}", block_name);

                for cmd in &each_block.cmds {
                    println!("\t{}", cmd);
                }
            }

            self.file_alias_blocks.insert(alias_name.to_string(), blocks);
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
                        None => return Err(BlockSyntaxError::UnexpectedEOF(line_i, "'\"'".to_string())),
                    };

                    // 文字列中の改行を弾く
                    if *next_char == '\n' {
                        return Err(BlockSyntaxError::UnexpectedToken(line_i, "\\n".to_string(), "'\"'".to_string()));
                    }

                    char_i += 1;

                    string.push(next_char.clone());

                    // エスケープシーケンスの解析
                    if *next_char == '\\' {
                        // エスケープ文字の後ろが EOF である場合は弾く
                        let esc_char = match chars.get(char_i) {
                            Some(v) => v,
                            None => return Err(BlockSyntaxError::UnexpectedEOF(line_i, "'\"'".to_string())),
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
                        None => return Err(BlockSyntaxError::UnexpectedEOF(line_i, "']'".to_string())),
                    };

                    char_i += 1;

                    string.push(next_char.clone());

                    // エスケープシーケンスの解析
                    if *next_char == '\\' {
                        // エスケープ文字の後ろが EOF である場合は弾く
                        let esc_char = match chars.get(char_i) {
                            Some(v) => v,
                            None => return Err(BlockSyntaxError::UnexpectedEOF(line_i, "']'".to_string())),
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
                        None => return Err(BlockSyntaxError::UnexpectedEOF(line_i, "'\"'".to_string())),
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

    // 初期位置: パース対象ソースの開始位置
    fn get_blocks(&mut self) -> std::result::Result<std::collections::HashMap<String, data::Block>, BlockSyntaxError> {
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
                    None => return Err(BlockSyntaxError::UnknownToken(each_token.line, each_token.value.clone())),
                }

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

                let block_name_token = each_token.value.to_string();

                // token_i がブロック終了位置 + 1 に設定される

                let block = self.get_block_content(block_name_token[1..block_name_token.len() - 1].to_string())?;
                let block_name = block.name.to_string();

                // 角括弧内にブロック名がない場合はエラー
                if block_name == "" {
                    return Err(BlockSyntaxError::UnexpectedToken(each_token.line, "]".to_string(), "ID".to_string()));
                }

                // ブロック名が重複している場合はエラー
                if block_map.contains_key(&block_name) {
                    return Err(BlockSyntaxError::DuplicatedBlockName(each_token.line, block_name.to_string()));
                }

                block_map.insert(block_name, block);

                continue;
            }

            return Err(BlockSyntaxError::ExpectedBlockDef(each_token.line));
        }

        return Ok(block_map);
    }

    // 各ブロックの中身を取得する
    // token_i の条件は get_next_command_content() と同様
    fn get_block_content(&mut self, block_name: String) -> std::result::Result<data::Block, BlockSyntaxError> {
        let cmds = self.get_commands()?;
        let block = data::Block::new(block_name, cmds);
        return Ok(block);
    }

    // ブロック内のすべてのコマンドを取得する
    // token_i の条件は get_next_command_content() と同様
    fn get_commands(&mut self) -> std::result::Result<Vec<data::Command>, BlockSyntaxError> {
        let mut cmds = Vec::<data::Command>::new();
        let mut new_cmd = self.get_next_command_content()?;

        // get_next_command_content() の返り値が None になるまで続ける
        while new_cmd.is_some() {
            cmds.push(new_cmd.unwrap());
            new_cmd = self.get_next_command_content()?;
        }

        return Ok(cmds);

        // コマンドが見つからないためエラーを返す

        // if self.tokens.len() == 0 {
        //     return Err(BlockSyntaxError::UnexpectedEOF(0, "'}'".to_string()));
        // }

        // let last_token = self.tokens.get(self.tokens.len() - 1).unwrap();
        // return Err(BlockSyntaxError::UnexpectedEOF(last_token.line, "'}'".to_string()));
    }

    // 各コマンドの中身を取得する
    // token_i がブロックの中身の開始位置もしくは前の命令の終了記号位置 + 1 であること
    // token_i は各コマンドの終了記号位置 + 1 に設定される
    fn get_next_command_content(&mut self) -> std::result::Result<std::option::Option<data::Command>, BlockSyntaxError> {
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
                                return Err(BlockSyntaxError::UnexpectedEOF(last_token_line_num, "pragma name".to_string()));
                            } else {
                                return Err(BlockSyntaxError::UnexpectedEOF(last_token_line_num, ",".to_string()));
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
                            return Err(BlockSyntaxError::UnexpectedToken(last_token_line_num, next_token.value.to_string(), "pragma name".to_string()));
                        }
                    }

                    if next_token.kind == data::TokenKind::ID || next_token.kind == data::TokenKind::String || (next_token.kind == data::TokenKind::Symbol && next_token.value == ".") {
                        self.token_i += 1;
                        pragma_args.push(next_token.clone());
                        continue;
                    }

                    if next_token.kind == data::TokenKind::Symbol && next_token.value == "," {
                        self.token_i += 1;

                        let cmd = BlockParser::get_command_from_data(next_token.line, pragma_name, pragma_args)?;
                        return Ok(Some(cmd));
                    }

                    return Err(BlockSyntaxError::UnknownSyntax(each_token.line, each_token.value.to_string()));
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
                    None => return Err(BlockSyntaxError::UnexpectedEOF(line_num, "' '".to_string())),
                }

                // 規則名
                let rule_name_token = match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::ID {
                            self.token_i += 1;
                            line_num = v.line;
                            v
                        } else {
                            return Err(BlockSyntaxError::UnexpectedToken(line_num, v.value.to_string(), "ID".to_string()));
                        }
                    },
                    None => return Err(BlockSyntaxError::UnexpectedEOF(line_num, "'ID'".to_string())),
                };

                // 規則名後のスペース
                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Space {
                            self.token_i += 1;
                            line_num = v.line;
                        }
                    },
                    None => return Err(BlockSyntaxError::UnexpectedEOF(line_num, "' '".to_string())),
                }

                // 規則定義の記号 <
                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Symbol && v.value == "<" {
                            self.token_i += 1;
                            line_num = v.line;
                        } else {
                            return Err(BlockSyntaxError::UnexpectedToken(line_num, v.value.to_string(), "'<'".to_string()));
                        }
                    },
                    None => return Err(BlockSyntaxError::UnexpectedEOF(line_num, "'<'".to_string())),
                }

                // 規則定義の記号 -
                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Symbol && v.value == "-" {
                            self.token_i += 1;
                            line_num = v.line;
                        } else {
                            return Err(BlockSyntaxError::UnexpectedToken(line_num, v.value.to_string(), "'-'".to_string()));
                        }
                    },
                    None => return Err(BlockSyntaxError::UnexpectedEOF(line_num, "'-'".to_string())),
                }

                // 規則定義の記号後のスペース
                match self.tokens.get(self.token_i) {
                    Some(v) => {
                        if v.kind == data::TokenKind::Space {
                            self.token_i += 1;
                            line_num = v.line;
                        }
                    },
                    None => return Err(BlockSyntaxError::UnexpectedEOF(line_num, "' '".to_string())),
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

                        let cmd = BlockParser::get_command_from_data(next_token.line, "define".to_string(), pragma_args)?;
                        return Ok(Some(cmd));
                    }

                    pragma_args.push(next_token.clone());
                    self.token_i += 1;
                }
            }

            // 構文がマッチしなかった場合はエラー
            return Err(BlockSyntaxError::UnexpectedToken(each_token.line, each_token.value.to_string(), "'+' and ID".to_string()));
        }

        // let cmd = data::Command::;
        // return Ok(cmd);

        // ブロック終了記号の検知はどうする？
        // これ以上コマンドが見つからないため None を返す

        return Ok(None);
    }

    // pragma_arg: プラグマ名が define の場合、長さは 0 であってならない
    fn get_command_from_data(line_num: usize, pragma_name: String, pragma_args: Vec<data::Token>) -> std::result::Result<data::Command, BlockSyntaxError> {
        let cmd = match pragma_name.as_str() {
            "define" => {
                if pragma_args.len() == 0 {
                    return Err(BlockSyntaxError::InternalErr("invalid pragma argument length".to_string()));
                }

                let rule = BlockParser::get_define_command(pragma_args.get(0).unwrap().value.to_string(), pragma_args[1..].to_vec())?;
                data::Command::Define(rule)
            },
            "start" => {
                if pragma_args.len() == 0 {
                    return Err(BlockSyntaxError::UnexpectedToken(line_num, ",".to_string(), "pragma argument".to_string()));
                }

                if pragma_args.len() >= 2 {
                    return Err(BlockSyntaxError::UnexpectedToken(line_num, pragma_args.get(1).unwrap().value.to_string(), "','".to_string()));
                }

                // ブロック名を取得

                let block_name_token = pragma_args.get(0).unwrap();

                if block_name_token.kind != data::TokenKind::ID {
                    return Err(BlockSyntaxError::UnexpectedToken(line_num, block_name_token.value.to_string(), "ID".to_string()));
                }

                let block_name = block_name_token.value.to_string();

                data::Command::Start(block_name)
            },
            "use" => {
                if pragma_args.len() == 0 {
                    return Err(BlockSyntaxError::UnexpectedToken(line_num, ",".to_string(), "pragma argument".to_string()));
                }

                // ブロック名を取得

                let block_name_token = pragma_args.get(0).unwrap();

                if block_name_token.kind != data::TokenKind::ID {
                    return Err(BlockSyntaxError::UnexpectedToken(line_num, block_name_token.value.to_string(), "ID".to_string()));
                }

                let block_name = block_name_token.value.to_string();

                // ブロックエイリアス名を取得

                let block_alias_name = block_name.to_string();

                /* todo: エイリアス機能を後で作る
                if pragma_args.len() == 5 {
                    match pragma_args.get(1) {
                        Some(v) => {
                            if v.kind != data::TokenKind::Space {
                                return Err(BlockSyntaxError::UnexpectedToken(line_num, v.value, "' '".to_string()));
                            }
                        },
                        None => return Err(BlockSyntaxError::ExpectedToken(line_num, "' '".to_string())),
                    }

                    match pragma_args.get(2) {
                        Some(v) => {
                            if v.kind != data::TokenKind::Space {
                                return Err(BlockSyntaxError::UnexpectedToken(line_num, v.value, "ID".to_string()));
                            }
                        },
                        None => return Err(BlockSyntaxError::ExpectedToken(line_num, "'as'".to_string())),
                    }

                    match pragma_args.get(3) {
                        Some(v) => {
                            if v.kind != data::TokenKind::Space {
                                return Err(BlockSyntaxError::UnexpectedToken(line_num, v.value, "' '".to_string()));
                            }
                        },
                        None => return Err(BlockSyntaxError::ExpectedToken(line_num, "' '".to_string())),
                    }

                    let unexpected_token_value = match pragma_args.get(2).unwrap().get(0) {
                        Some(v) => v.value,
                        None => "".to_string(),
                    };

                    return Err(BlockSyntaxError::UnexpectedToken(line_num, unexpected_token_value, "','".to_string()));
                }
                */

                data::Command::Use(block_name.to_string(), block_alias_name.to_string())
            },
            _ => return Err(BlockSyntaxError::UnknownPragmaName(line_num, pragma_name.to_string())),
        };

        return Ok(cmd);
    }

    fn get_define_command(rule_name: String, tokens: Vec<data::Token>) -> std::result::Result<data::Rule, BlockSyntaxError> {
        let mut token_i = 0;

        let mut choices = Vec::<data::RuleChoice>::new();
        let mut tmp_seqs = Vec::<data::RuleSequence>::new();
        let mut tmp_exprs = Vec::<data::RuleExpression>::new();

        while token_i < tokens.len() {
            let each_token = tokens.get(token_i).unwrap();

            if each_token.kind == data::TokenKind::Space {
                tmp_seqs.push(data::RuleSequence::new(tmp_exprs.clone()));
                tmp_exprs.clear();

                token_i += 1;
                continue;
            }

            if each_token.kind == data::TokenKind::String {
                tmp_exprs.push(data::RuleExpression::new(data::RuleExpressionKind::String, data::RuleExpressionLoopKind::One, data::RuleExpressionLookaheadKind::None, each_token.value.to_string()));

                token_i += 1;
                continue;
            }

            if each_token.kind == data::TokenKind::Symbol && each_token.value == ":" {
                tmp_seqs.push();
            }

            token_i += 1;
        }

        return Ok(data::Rule::new(rule_name.to_string(), vec![]));
    }
}
