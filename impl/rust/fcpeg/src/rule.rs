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
                            self.proc_define_cmd(each_choice, &each_rule.name, &each_block.name, fcpeg_file_man, &block_alias_map)?;
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

    pub fn proc_define_cmd(&mut self, choice: &mut RuleChoice, rule_id: &String, block_name: &String, fcpeg_file_man: &blockparser::FCPEGFileMan, block_alias_map: &std::collections::HashMap<String, String>) -> std::result::Result<(), blockparser::BlockParseError> {
        for each_elem in choice.elem_containers.iter_mut() {
            match each_elem {
                RuleElementContainer::RuleChoice(each_choice) => {
                    self.proc_define_cmd(each_choice, rule_id, block_name, fcpeg_file_man, block_alias_map)?;
                },
                RuleElementContainer::RuleExpression(each_expr) => {
                    if each_expr.kind == RuleExpressionKind::ID {
                        let id_tokens: Vec<&str> = each_expr.value.split(".").collect();

                        if id_tokens.len() == 1 {
                            each_expr.value = format!("{}.{}.{}", fcpeg_file_man.file_alias_name, block_name, each_expr.value);
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

                        let pattern = match regex::Regex::new(&each_expr.value.to_string()) {
                            Err(_e) => return Err(blockparser::BlockParseError::InvalidCharClassFormat(each_expr.line, each_expr.to_string())),
                            Ok(v) => v,
                        };

                        self.regex_map.insert(each_expr.value.to_string(), pattern);
                    }
                },
            }
        }

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

pub struct RuleCountConverter {}

impl RuleCountConverter {
    pub fn count_to_string(count: &(i32, i32), is_loop_count: bool, prefix: &str, separator: &str, suffix: &str) -> String {
        if *count == (1, 1) {
            return "".to_string();
        }

        if is_loop_count {
            match *count {
                (0, -1) => return "*".to_string(),
                (1, -1) => return "+".to_string(),
                (0, 1) => return "?".to_string(),
                _ => (),
            }
        }

        let min_count = if count.0 == 0 { "".to_string() } else { count.0.to_string() };
        let max_count = if count.1 == -1 { "".to_string() } else { count.1.to_string() };
        return format!("{}{}{}{}{}", prefix, min_count, separator, max_count, suffix);
    }

    pub fn loop_symbol_to_count(value: &str) -> (i32, i32) {
        return match value {
            "*" => (0, -1),
            "+" => (1, -1),
            "?" => (0, 1),
            _ => (1, 1),
        }
    }
}

#[derive(Clone)]
pub struct Rule {
    pub name: String,
    pub choices: Vec<Box<RuleChoice>>,
}

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut choice_text = Vec::<String>::new();

        for each_choice in &self.choices {
            choice_text.push(each_choice.to_string());
        }

        return write!(f, "{} <- {}", self.name, choice_text.join(" : "))
    }
}

impl Rule {
    pub fn new(name: String, choices: Vec<Box<RuleChoice>>) -> Self {
        return Rule {
            name: name,
            choices: choices,
        };
    }
}

#[derive(Clone)]
pub enum RuleElementContainer {
    RuleChoice(Box<RuleChoice>),
    RuleExpression(Box<RuleExpression>),
}

impl std::fmt::Display for RuleElementContainer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        return match self {
            RuleElementContainer::RuleChoice(choice) => write!(f, "{}", choice),
            RuleElementContainer::RuleExpression(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Clone)]
pub struct RuleChoice {
    pub elem_containers: Vec<RuleElementContainer>,
    pub lookahead_kind: RuleLookaheadKind,
    pub loop_count: (i32, i32),
    pub ast_reflection: data::ASTReflection,
    pub is_random_order: bool,
    pub occurrence_count: (i32, i32),
    pub has_choices: bool,
}

impl std::fmt::Display for RuleChoice {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut seq_text = Vec::<String>::new();

        for each_container in &self.elem_containers {
            match each_container {
                RuleElementContainer::RuleChoice(each_choice) => {
                    seq_text.push(each_choice.to_string());
                },
                RuleElementContainer::RuleExpression(each_expr) => {
                    seq_text.push(format!("{}", each_expr));
                },
            }
        }

        let separator = if self.has_choices { if self.is_random_order { ", " } else { " : " } } else { " " };
        let loop_text = RuleCountConverter::count_to_string(&self.loop_count, true, "{", ",", "}");
        let random_order_symbol = if self.is_random_order { "^" } else { "" };
        let random_order_count = RuleCountConverter::count_to_string(&self.occurrence_count, false, "[", "-", "]");
        let ast_reflection_text = match &self.ast_reflection {
            data::ASTReflection::Reflectable(elem_name) => format!("#{}", elem_name.to_string()),
            data::ASTReflection::Unreflectable() => String::new()
        };

        return write!(f, "{}", format!("{}({}){}{}{}{}", self.lookahead_kind.to_symbol_string(), seq_text.join(separator), loop_text, random_order_symbol, random_order_count, ast_reflection_text));
    }
}

impl RuleChoice {
    pub fn new(lookahead_kind: RuleLookaheadKind, loop_count: (i32, i32), ast_reflection: data::ASTReflection, is_random_order: bool, occurrence_count: (i32, i32), has_choices: bool) -> Self {
        return RuleChoice {
            elem_containers: vec![],
            lookahead_kind: lookahead_kind,
            loop_count: loop_count,
            ast_reflection: ast_reflection,
            is_random_order: is_random_order,
            occurrence_count: occurrence_count,
            has_choices: has_choices,
        };
    }

    pub fn has_choices(&self) -> bool {
        for each_elem in &self.elem_containers {
            match each_elem {
                RuleElementContainer::RuleChoice(_v) => return true,
                _ => (),
            }
        }

        return false;
    }

    pub fn is_hierarchy_omission_needed(choices: &Vec<Box<RuleChoice>>, is_random_order: bool) -> std::option::Option<Box<RuleChoice>> {
        match choices.get(0) {
            Some(v) => {
                if choices.len() == 1 && v.lookahead_kind == RuleLookaheadKind::None && v.loop_count == (1, 1) && v.occurrence_count == (1, 1) {
                    if is_random_order {
                        match v.elem_containers.get(0) {
                            Some(v2) => {
                                match v2 {
                                    RuleElementContainer::RuleChoice(v3) => {
                                        if v3.has_choices() {
                                            return Some(v.clone());
                                        }
                                    },
                                    _ => (),
                                }
                            },
                            None => (),
                        }
                    } else {
                        return Some(v.clone());
                    }
                }
            },
            None => (),
        };

        return None;
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
    pub fn to_token_string(&self, value: &str) -> String {
        return match self {
            RuleExpressionKind::CharClass => value.to_string(),
            RuleExpressionKind::ID => value.to_string(),
            RuleExpressionKind::String => format!("\"{}\"", value),
            RuleExpressionKind::Wildcard => ".".to_string(),
        }
    }
}

#[derive(Clone)]
pub struct RuleExpression {
    pub line: usize,
    pub kind: RuleExpressionKind,
    pub lookahead_kind: RuleLookaheadKind,
    pub loop_count: (i32, i32),
    pub ast_reflection: data::ASTReflection,
    pub value: String,
}

impl std::fmt::Display for RuleExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let loop_text = RuleCountConverter::count_to_string(&self.loop_count, true, "{", ",", "}");
        let ast_reflection_text = match &self.ast_reflection {
            data::ASTReflection::Reflectable(elem_name) => format!("#{}", elem_name.to_string()),
            data::ASTReflection::Unreflectable() => String::new()
        };

        return write!(f, "{}{}{}{}", self.lookahead_kind.to_symbol_string(), self.kind.to_token_string(&self.value), loop_text, ast_reflection_text);
    }
}

impl RuleExpression {
    pub fn new(line: usize, kind: RuleExpressionKind, lookahead_kind: RuleLookaheadKind, loop_count: (i32, i32), ast_reflection: data::ASTReflection, value: String) -> Self {
        return RuleExpression {
            line: line,
            kind: kind,
            lookahead_kind: lookahead_kind,
            loop_count: loop_count,
            ast_reflection: ast_reflection,
            value: value,
        }
    }
}
