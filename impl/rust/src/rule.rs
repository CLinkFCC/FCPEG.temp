use crate::blockparser;
use crate::data;
use regex;

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
                data::Command::Define(_line, _rule) => {
                    return Err(blockparser::BlockParseError::RuleInMainBlock());
                },
                data::Command::Use(line, file_alias_name, block_name, block_alias_name) => {
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
                data::Command::Start(_line, file_alias_name, block_name, rule_name) => {
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
        // todo: コメントアウト外し
        /*
        for each_block in fcpeg_file_man.block_map.values() {
            if each_block.name == "Main" {
                continue;
            }

            let mut block_alias_map = std::collections::HashMap::<String, String>::new();

            // start コマンドを排除しながら use コマンドを処理する
            for each_cmd in &each_block.cmds {
                match each_cmd {
                    data::Command::Start(_line, _file_alias_name, _block_name, _rule_name) => return Err(blockparser::BlockParseError::StartCmdOutsideMainBlock()),
                    data::Command::Use(line, file_alias_name, block_name, block_alias_name) => {
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
                    data::Command::Define(_line, rule) => {
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
        */

        return Ok(());
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleLookaheadKind {
    None,
    Positive,
    Negative,
}

impl RuleLookaheadKind {
    // ret: 文字がマッチしなければ RuleLookaheadKind::None
    pub fn to_lookahead_kind(value: &str) -> RuleLookaheadKind {
        return match value {
            "&" => RuleLookaheadKind::Positive,
            "!" => RuleLookaheadKind::Negative,
            _ => RuleLookaheadKind::None,
        }
    }

    pub fn to_symbol_string(&self) -> String {
        return match self {
            RuleLookaheadKind::None => "".to_string(),
            RuleLookaheadKind::Positive => "&".to_string(),
            RuleLookaheadKind::Negative => "!".to_string(),
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleLoopKind {
    One,
    OneOrMore,
    ZeroOrOne,
    ZeroOrMore,
}

impl RuleLoopKind {
    // ret: 文字がマッチすれば Some() 、マッチしなければ None
    pub fn to_loop_kind(value: &str) -> std::option::Option<RuleLoopKind> {
        return match value {
            "+" => Some(RuleLoopKind::OneOrMore),
            "?" => Some(RuleLoopKind::ZeroOrOne),
            "*" => Some(RuleLoopKind::ZeroOrMore),
            _ => None,
        }
    }

    pub fn to_symbol_string(&self) -> String {
        return match self {
            RuleLoopKind::One => "".to_string(),
            RuleLoopKind::OneOrMore => "+".to_string(),
            RuleLoopKind::ZeroOrOne => "?".to_string(),
            RuleLoopKind::ZeroOrMore => "*".to_string(),
        }
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

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleElementContainerKind {
    RuleChoice,
    RuleSequence,
}

#[derive(Clone)]
pub struct RuleChoice {
    pub elem_container_kinds: Vec<RuleElementContainerKind>,
    pub choices: Vec<RuleChoice>,
    pub seqs: Vec<RuleSequence>,
    pub lookahead_kind: RuleLookaheadKind,
    pub loop_kind: RuleLoopKind,
}

impl std::fmt::Display for RuleChoice {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut seq_text = Vec::<String>::new();

        for each_seq in &self.seqs {
            seq_text.push(format!("{}{}{}{}", each_seq.lookahead_kind.to_symbol_string(), each_seq.to_string(), each_seq.loop_kind.to_symbol_string(), if each_seq.is_random_order { "^" } else { "" }));
        }

        return write!(f, "{}", seq_text.join(" "));
    }
}

impl RuleChoice {
    pub fn new(lookahead_kind: RuleLookaheadKind, loop_kind: RuleLoopKind) -> Self {
        return RuleChoice {
            elem_container_kinds: vec![],
            choices: vec![],
            seqs: vec![],
            lookahead_kind: lookahead_kind,
            loop_kind: loop_kind,
        };
    }

    pub fn add_choice(&mut self, new_choice: RuleChoice) {
        self.elem_container_kinds.push(RuleElementContainerKind::RuleChoice);
        self.choices.push(new_choice);
    }

    pub fn add_seq(&mut self, new_seq: RuleChoice) {
        self.elem_container_kinds.push(RuleElementContainerKind::RuleSequence);
        self.choices.push(new_seq);
    }
}

#[derive(Clone)]
pub struct RuleSequence {
    pub elem_container_kinds: Vec<RuleElementContainerKind>,
    pub choices: Vec<RuleChoice>,
    pub seqs: Vec<RuleSequence>,
    pub exprs: Vec<RuleExpression>,
    pub lookahead_kind: RuleLookaheadKind,
    pub loop_kind: RuleLoopKind,
    pub is_random_order: bool,
}

impl std::fmt::Display for RuleSequence {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut expr_text = Vec::<String>::new();

        for each_expr in &self.exprs {
            expr_text.push(each_expr.to_string());
        }

        return write!(f, "{}", expr_text.join(" "));
    }
}

impl RuleSequence {
    pub fn new(exprs: Vec<RuleExpression>, lookahead_kind: RuleLookaheadKind, loop_kind: RuleLoopKind, is_random_order: bool) -> Self {
        return RuleSequence {
            elem_container_kinds: vec![],
            choices: vec![],
            seqs: vec![],
            exprs: exprs,
            lookahead_kind: lookahead_kind,
            loop_kind: loop_kind,
            is_random_order: is_random_order,
        };
    }

    pub fn add_choice(&mut self, new_choice: RuleChoice) {
        self.elem_container_kinds.push(RuleElementContainerKind::RuleChoice);
        self.choices.push(new_choice);
    }

    pub fn add_seq(&mut self, new_seq: RuleChoice) {
        self.elem_container_kinds.push(RuleElementContainerKind::RuleSequence);
        self.choices.push(new_seq);
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

#[derive(Clone)]
pub struct RuleExpression {
    pub line: usize,
    pub kind: RuleExpressionKind,
    pub lookahead_type: RuleLookaheadKind,
    pub loop_type: RuleLoopKind,
    pub value: String,
}

impl RuleExpression {
    pub fn new(line: usize, kind: RuleExpressionKind, lookahead_type: RuleLookaheadKind, loop_type: RuleLoopKind, value: String,) -> Self {
        return RuleExpression {
            line: line,
            kind: kind,
            lookahead_type: lookahead_type,
            loop_type: loop_type,
            value: value,
        }
    }
}

impl std::fmt::Display for RuleExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}{}", self.lookahead_type.to_symbol_string(), self.kind.to_token_string(self.value.to_string()), self.loop_type.to_symbol_string())
    }
}
