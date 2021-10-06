use crate::blockparser::*;

#[derive(PartialOrd, PartialEq, Debug, Clone)]
pub enum BlockTokenKind {
    ID,
    Number,
    Space,
    String,
    StringInBracket,
    Symbol,
}

impl std::fmt::Display for BlockTokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BlockTokenKind::ID => return write!(f, "ID"),
            BlockTokenKind::Number => return write!(f, "Number"),
            BlockTokenKind::Space => return write!(f, "Space"),
            BlockTokenKind::String => return write!(f, "String"),
            BlockTokenKind::StringInBracket => return write!(f, "StringInBracket"),
            BlockTokenKind::Symbol => return write!(f, "Symbol"),
        }
    }
}

#[derive(Clone)]
pub struct BlockToken {
    pub line: usize,
    pub kind: BlockTokenKind,
    pub value: String,
}

impl BlockToken {
    pub fn new(line: usize, kind: BlockTokenKind, value: String) -> BlockToken {
        return BlockToken {
            line: line,
            kind: kind,
            value: value,
        }
    }
}

pub struct BlockLexer {}

impl BlockLexer {
    pub fn get_tokens(src_content: &String) -> Result<Vec<BlockToken>, BlockParseError> {
        let mut tokens: Vec<BlockToken> = vec![];

        let id_num_regex = regex::Regex::new(r"[a-zA-Z0-9_]").unwrap();
        let mut tmp_id_num = String::new();

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
                        BlockTokenKind::ID
                    },
                    Ok(_v) => BlockTokenKind::Number,
                };

                tokens.push(BlockToken::new(line_i, token_kind, tmp_id_num.clone()));
                tmp_id_num = String::new();
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
                let mut string = each_char_str.clone();

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

                tokens.push(BlockToken::new(line_i, BlockTokenKind::String, string));
                continue;
            }

            // 角括弧内の文字列判定
            if *each_char == '[' {
                let mut string = each_char_str.clone();

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

                tokens.push(BlockToken::new(line_i, BlockTokenKind::StringInBracket, string));
                continue;
            }

            // スペース判定
            if *each_char == ' ' {
                tokens.push(BlockToken::new(line_i, BlockTokenKind::Space, each_char_str.clone()));
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
                tokens.push(BlockToken::new(line_i, BlockTokenKind::Symbol, each_char_str.clone()));

                char_i += 1;
                continue;
            }

            if cfg!(release) {
                println!("+ {} {}", *each_char as i32, *each_char);
            }

            return Err(BlockParseError::UnknownToken(line_i, each_char.to_string()));
        }

        // 最後に tmp_id が残っていないかチェック
        if tmp_id_num.len() != 0 {
            // トークンの種類を判定
            let token_kind = match tmp_id_num.parse::<i32>() {
                Err(_e) => BlockTokenKind::ID,
                Ok(_v) => BlockTokenKind::Number,
            };

            tokens.push(BlockToken::new(line_i, token_kind, tmp_id_num.clone()));
        }

        if cfg!(release) {
            for (token_i, each_token) in tokens.iter().enumerate() {
                println!("[{};{}] {}\t\t{}", each_token.line, token_i, each_token.value, each_token.kind);
            }
        }

        return Ok(tokens);
    }
}
