use crate::blockparser;
use regex;

#[derive(PartialOrd, PartialEq, Debug, Clone)]
pub enum TokenKind {
    ID,
    Space,
    String,
    StringInBracket,
    Symbol,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenKind::ID => return write!(f, "ID"),
            TokenKind::Space => return write!(f, "Space"),
            TokenKind::String => return write!(f, "String"),
            TokenKind::StringInBracket => return write!(f, "StringInBracket"),
            TokenKind::Symbol => return write!(f, "Symbol"),
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub line: usize,
    pub kind: TokenKind,
    pub value: String,
}

impl Token {
    pub fn new(line: usize, kind: TokenKind, value: String) -> Self {
        return Token {
            line: line,
            kind: kind,
            value: value,
        }
    }
}

#[derive(Clone)]
pub struct SyntaxNode {
    pub name: String,
    pub nodes: Vec<SyntaxNode>,
    pub leaves: Vec<String>,
}

impl SyntaxNode {
    pub fn new(name: String) -> Self {
        return SyntaxNode {
            name: name,
            nodes: vec![],
            leaves: vec![],
        };
    }

    pub fn print(&self, nest: usize) {
        println!("{}{}: {}", "    ".repeat(nest), self.name, self.leaves.join(",").replace("\\", "\\\\").replace("\n", "\\n").replace(" ", "\\s").replace("\t", "\\t"));

        for each_node in &self.nodes {
            each_node.print(nest + 1);
        }
    }
}

#[derive(Clone)]
pub struct Block {
    pub name: String,
    pub cmds: Vec<Command>,
}

impl Block {
    pub fn new(name: String, cmds: Vec<Command>) -> Self {
        return Block {
            name: name,
            cmds: cmds,
        };
    }
}

#[derive(Clone)]
pub enum Command {
    Define(usize, Rule),
    Start(usize, String, String, String),
    Use(usize, String, String, String),
}

impl std::fmt::Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Command::Define(line, rule) => return write!(f, "{}| rule {}", line, rule),
            Command::Start(line, file_alias_name, block_name, rule_name) => return write!(f, "{}| start rule '{}.{}.{}'", line, file_alias_name, block_name, rule_name),
            Command::Use(line, file_alias_name, block_name, block_alias_name) => return write!(f, "{}| use block '{}.{}' as '{}'", line, file_alias_name, block_name, block_alias_name),
        }
    }
}

pub struct RuleMap {
    pub rule_map: std::collections::HashMap<String, Rule>,
    pub regex_map: std::collections::HashMap::<String, regex::Regex>,
    pub start_rule_id: String,
}

impl std::fmt::Display for RuleMap {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut rule_text_lines = Vec::<String>::new();

        for each_rule in self.rule_map.values() {
            rule_text_lines.push(each_rule.to_string());
        }

        for each_key in self.regex_map.keys() {
            rule_text_lines.push(format!("reg {}", each_key));
        }

        return writeln!(f, "{}", rule_text_lines.join("\n"));
    }
}

impl RuleMap {
    pub fn new(start_rule_id: String) -> Self {
        return RuleMap {
            rule_map: std::collections::HashMap::new(),
            regex_map: std::collections::HashMap::new(),
            start_rule_id: start_rule_id,
        };
    }

    pub fn add_rule(&mut self, file_alias_name: String, block_name: String, rule_name: String, rule: Rule) {
        let rule_id = file_alias_name + "." + block_name.as_str() + "." + rule_name.as_str();
        self.rule_map.insert(rule_id, rule);
    }

    #[inline(always)]
    pub fn get_rule(&mut self, rule_id: &String) -> std::option::Option<&Rule> {
        return self.rule_map.get(rule_id);
    }

    // ここでは規則 ID の存在チェックは行われない
    pub fn get_from_root_fcpeg_file_man(fcpeg_file_man: blockparser::FCPEGFileMan) -> std::result::Result<RuleMap, blockparser::BlockParseError> {
        let main_block = match fcpeg_file_man.block_map.get("Main") {
            Some(v) => v,
            None => return Err(blockparser::BlockParseError::MainBlockNotFound()),
        };

        let mut has_start_cmd = false;
        let mut start_rule_id = "".to_string();

        // <block_alias_name, block_id>
        let mut block_alias_map = std::collections::HashMap::<String, String>::new();

        // メインブロックの use コマンドを処理する
        // define ブロックがあればエラー
        for each_cmd in &main_block.cmds {
            match each_cmd {
                Command::Define(_line, _rule) => {
                    return Err(blockparser::BlockParseError::RuleInMainBlock());
                },
                Command::Use(line, file_alias_name, block_name, block_alias_name) => {
                    if block_alias_map.contains_key(&block_alias_name.to_string()) {
                        return Err(blockparser::BlockParseError::DuplicatedBlockAliasName(line.clone(), block_alias_name.to_string()));
                    }

                    let rule_id = format!("{}.{}", file_alias_name, block_name);
                    block_alias_map.insert(block_alias_name.to_string(), rule_id);
                },
                _ => (),
            }
        }

        // start コマンドを処理する
        for each_cmd in &main_block.cmds {
            match each_cmd {
                Command::Start(_line, file_alias_name, block_name, rule_name) => {
                    if has_start_cmd {
                        return Err(blockparser::BlockParseError::DuplicatedStartCmd());
                    }

                    has_start_cmd = true;

                    if file_alias_name == "" && block_alias_map.contains_key(block_name) {
                        start_rule_id = format!("{}.{}", block_alias_map.get(block_name).unwrap(), rule_name);
                    } else {
                        start_rule_id = format!("{}.{}.{}", file_alias_name, block_name, rule_name);
                    }
                },
                _ => (),
            }
        }

        if !has_start_cmd {
            return Err(blockparser::BlockParseError::NoStartCmdInMainBlock());
        }

        let mut rule_map = RuleMap::new(start_rule_id);
        rule_map.add_rules_from_fcpeg_file_man(&fcpeg_file_man)?;

        return Ok(rule_map);
    }

    pub fn add_rules_from_fcpeg_file_man(&mut self, fcpeg_file_man: &blockparser::FCPEGFileMan) -> std::result::Result<(), blockparser::BlockParseError> {
        for each_block in fcpeg_file_man.block_map.values() {
            if each_block.name == "Main" {
                continue;
            }

            let mut block_alias_map = std::collections::HashMap::<String, String>::new();

            // start コマンドを排除しながら use コマンドを処理する
            for each_cmd in &each_block.cmds {
                match each_cmd {
                    Command::Start(_line, _file_alias_name, _block_name, _rule_name) => return Err(blockparser::BlockParseError::StartCmdOutsideMainBlock()),
                    Command::Use(line, file_alias_name, block_name, block_alias_name) => {
                        if block_alias_map.contains_key(&block_alias_name.to_string()) {
                            return Err(blockparser::BlockParseError::DuplicatedBlockAliasName(line.clone(), block_alias_name.to_string()));
                        }

                        let rule_id = format!("{}.{}", file_alias_name.to_string(), block_name.to_string());
                        block_alias_map.insert(block_alias_name.to_string(), rule_id);
                    },
                    _ => (),
                }
            }

            // define コマンドを処理する
            for each_cmd in &each_block.cmds {
                match each_cmd {
                    Command::Define(_line, rule) => {
                        let mut each_rule = rule.clone();
                        each_rule.name = format!("{}.{}.{}", fcpeg_file_man.file_alias_name, each_block.name, each_rule.name);

                        for each_choice in each_rule.choices.iter_mut() {
                            for each_seq_group in each_choice.seq_groups.iter_mut() {
                                for each_seq in each_seq_group.seqs.iter_mut() {
                                    for each_expr in each_seq.exprs.iter_mut() {
                                        if each_expr.kind == RuleExpressionKind::ID {
                                            let id_tokens: Vec<&str> = each_expr.value.split(".").collect();

                                            if id_tokens.len() == 1 {
                                                each_expr.value = format!("{}.{}.{}", fcpeg_file_man.file_alias_name, each_block.name, each_expr.value);
                                                continue;
                                            }

                                            if id_tokens.len() == 2 {
                                                let block_name = id_tokens.get(0).unwrap();
                                                let rule_name = id_tokens.get(1).unwrap();

                                                if block_alias_map.contains_key(&block_name.to_string()) {
                                                    // ブロック名がエイリアスである場合
                                                    each_expr.value = format!("{}.{}", block_alias_map.get(&block_name.to_string()).unwrap(), rule_name);
                                                } else {
                                                    // ブロック名がエイリアスでない場合
                                                    return Err(blockparser::BlockParseError::BlockAliasNotFound(each_expr.line, block_name.to_string()));
                                                }

                                                continue;
                                            }

                                            if id_tokens.len() == 3 {
                                                let file_alias_name = id_tokens.get(0).unwrap();
                                                let block_name = id_tokens.get(1).unwrap();
                                                let rule_name = id_tokens.get(2).unwrap();

                                                each_expr.value = format!("{}.{}.{}", file_alias_name, block_name, rule_name);
                                                continue;
                                            }

                                            return Err(blockparser::BlockParseError::InternalErr(format!("invalid id expression '{}'", each_expr.value)));
                                        }

                                        if each_expr.kind == RuleExpressionKind::CharClass {
                                            if self.regex_map.contains_key(&each_expr.value) {
                                                continue;
                                            }

                                            let pattern = match regex::Regex::new(&each_expr.to_string()) {
                                                Err(_e) => return Err(blockparser::BlockParseError::InvalidCharClassFormat(each_expr.line, each_expr.to_string())),
                                                Ok(v) => v,
                                            };

                                            self.regex_map.insert(each_expr.value.to_string(), pattern);
                                        }
                                    }
                                }
                            }
                        }

                        self.add_rule(fcpeg_file_man.file_alias_name.to_string(), each_block.name.to_string(), rule.name.to_string(), each_rule);
                    },
                    _ => (),
                }
            }
        }

        for each_file_man in fcpeg_file_man.sub_file_aliase_map.values() {
            self.add_rules_from_fcpeg_file_man(each_file_man)?;
        }

        return Ok(());
    }
}

#[derive(Clone)]
pub struct Rule {
    pub name: String,
    pub choices: Vec<RuleChoice>,
}

impl Rule {
    pub fn new(name: String, choices: Vec<RuleChoice>) -> Self {
        return Rule {
            name: name,
            choices: choices,
        };
    }
}

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut choice_text = Vec::<String>::new();

        for each_choice in &self.choices {
            choice_text.push(each_choice.to_string());
        }

        write!(f, "{} <- {}", self.name, choice_text.join(" : "))
    }
}

#[derive(Clone)]
pub struct RuleChoice {
    pub seq_groups: Vec<RuleSequenceGroup>,
}

impl RuleChoice {
    pub fn new(seq_groups: Vec<RuleSequenceGroup>) -> Self {
        return RuleChoice {
            seq_groups: seq_groups,
        };
    }
}

impl std::fmt::Display for RuleChoice {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut seq_group_text = Vec::<String>::new();

        for each_group in &self.seq_groups {
            seq_group_text.push(format!("{}{}{}", each_group.lookahead_type.to_symbol_string(), each_group.to_string(), each_group.loop_type.to_symbol_string()));
        }

        write!(f, "{}", seq_group_text.join(" "))
    }
}

#[derive(Clone)]
pub struct RuleSequenceGroup {
    pub seqs: Vec<RuleSequence>,
    pub loop_type: RuleExpressionLoopKind,
    pub lookahead_type: RuleExpressionLookaheadKind,
}

impl RuleSequenceGroup {
    pub fn new(seqs: Vec<RuleSequence>, loop_type: RuleExpressionLoopKind, lookahead_type: RuleExpressionLookaheadKind) -> Self {
        return RuleSequenceGroup {
            seqs: seqs,
            loop_type: loop_type,
            lookahead_type: lookahead_type,
        };
    }
}

impl std::fmt::Display for RuleSequenceGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut seq_text = Vec::<String>::new();

        for each_seq in &self.seqs {
            seq_text.push(format!("({})", each_seq));
        }

        write!(f, "{}", seq_text.join(" "))
    }
}

#[derive(Clone)]
pub struct RuleSequence {
    pub exprs: Vec<RuleExpression>,
}

impl RuleSequence {
    pub fn new(exprs: Vec<RuleExpression>) -> Self {
        return RuleSequence {
            exprs: exprs,
        };
    }
}

impl std::fmt::Display for RuleSequence {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut expr_text = Vec::<String>::new();

        for each_expr in &self.exprs {
            expr_text.push(each_expr.to_string());
        }

        write!(f, "{}", expr_text.join(" "))
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleExpressionKind {
    CharClass,
    ID,
    String,
    Wildcard,
}

impl RuleExpressionKind {
    pub fn to_token_string(&self, value: String) -> String {
        return match self {
            RuleExpressionKind::CharClass => value.to_string(),
            RuleExpressionKind::ID => value.to_string(),
            RuleExpressionKind::String => value.to_string(),
            RuleExpressionKind::Wildcard => ".".to_string(),
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleExpressionLoopKind {
    One,
    OneOrMore,
    ZeroOrOne,
    ZeroOrMore,
}

impl RuleExpressionLoopKind {
    // ret: 文字がマッチすれば Some() 、マッチしなければ None
    pub fn to_loop_kind(value: String) -> std::option::Option<RuleExpressionLoopKind> {
        return match value.as_str() {
            "+" => Some(RuleExpressionLoopKind::OneOrMore),
            "?" => Some(RuleExpressionLoopKind::ZeroOrOne),
            "*" => Some(RuleExpressionLoopKind::ZeroOrMore),
            _ => None,
        }
    }

    pub fn to_symbol_string(&self) -> String {
        return match self {
            RuleExpressionLoopKind::One => "".to_string(),
            RuleExpressionLoopKind::OneOrMore => "+".to_string(),
            RuleExpressionLoopKind::ZeroOrOne => "?".to_string(),
            RuleExpressionLoopKind::ZeroOrMore => "*".to_string(),
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleExpressionLookaheadKind {
    None,
    Positive,
    Negative,
}

impl RuleExpressionLookaheadKind {
    pub fn to_symbol_string(&self) -> String {
        return match self {
            RuleExpressionLookaheadKind::None => "".to_string(),
            RuleExpressionLookaheadKind::Positive => "&".to_string(),
            RuleExpressionLookaheadKind::Negative => "!".to_string(),
        }
    }
}

#[derive(Clone)]
pub struct RuleExpression {
    pub line: usize,
    pub kind: RuleExpressionKind,
    pub loop_type: RuleExpressionLoopKind,
    pub lookahead_type: RuleExpressionLookaheadKind,
    pub value: String,
}

impl RuleExpression {
    pub fn new(line: usize, kind: RuleExpressionKind, loop_type: RuleExpressionLoopKind, lookahead_type: RuleExpressionLookaheadKind, value: String,) -> Self {
        return RuleExpression {
            line: line,
            kind: kind,
            loop_type: loop_type,
            lookahead_type: lookahead_type,
            value: value,
        }
    }
}

impl std::fmt::Display for RuleExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}{}", self.lookahead_type.to_symbol_string(), self.kind.to_token_string(self.value.to_string()), self.loop_type.to_symbol_string())
    }
}
