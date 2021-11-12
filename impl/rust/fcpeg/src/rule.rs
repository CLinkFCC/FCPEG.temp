use std::collections::*;
use std::fmt::*;
use std::option::*;

use crate::block::*;
use crate::data::*;

use regex::*;

#[derive(Clone)]
pub struct RuleMap {
    pub rule_map: HashMap<String, Rule>,
    pub regex_map: HashMap::<String, Regex>,
    pub start_rule_id: String,
}

impl Display for RuleMap {
    fn fmt(&self, f: &mut Formatter) -> Result {
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
    pub fn new(start_rule_id: String) -> RuleMap {
        return RuleMap {
            rule_map: HashMap::new(),
            regex_map: HashMap::new(),
            start_rule_id: start_rule_id,
        };
    }

    pub fn get_rule_id(file_alias_name: String, block_name: String, rule_name: String) -> String {
        return format!("{}.{}.{}", file_alias_name, block_name, rule_name);
    }

    pub fn insert_rule(&mut self, rule_id: String, rule: Rule) {
        self.rule_map.insert(rule_id, rule);
    }

    #[inline(always)]
    pub fn get_rule(&mut self, rule_id: &String) -> Option<&Rule> {
        return self.rule_map.get(rule_id);
    }

    // ここでは規則 ID の存在チェックは行われない
    pub fn get_from_root_fcpeg_file_man(file_alias_name: &String, block_map: &mut BlockMap) -> std::result::Result<RuleMap, BlockParseError> {
        println!("{}", block_map.len());
        let main_block = match block_map.get("Main") {
            Some(v) => v,
            None => return Err(BlockParseError::MainBlockNotFound()),
        };

        let mut has_start_cmd = false;
        let mut start_rule_id = String::new();

        // <block_alias_name, block_id>
        let mut block_alias_map = HashMap::<String, String>::new();

        // メインブロックの use コマンドを処理する
        // define ブロックがあればエラー
        for each_cmd in &main_block.cmds {
            match each_cmd {
                BlockCommand::Define(_line, _rule) => {
                    return Err(BlockParseError::RuleInMainBlock());
                },
                BlockCommand::Use(line, file_alias_name, block_name, block_alias_name) => {
                    if block_alias_map.contains_key(&block_alias_name.clone()) {
                        return Err(BlockParseError::DuplicatedBlockAliasName(line.clone(), block_alias_name.clone()));
                    }

                    let rule_id = format!("{}.{}", file_alias_name, block_name);
                    block_alias_map.insert(block_alias_name.clone(), rule_id);
                },
                _ => (),
            }
        }

        // start コマンドを処理する
        for each_cmd in &main_block.cmds {
            match each_cmd {
                BlockCommand::Start(_line, file_alias_name, block_name, rule_name) => {
                    if has_start_cmd {
                        return Err(BlockParseError::DuplicatedStartCmd());
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
            return Err(BlockParseError::NoStartCmdInMainBlock());
        }

        let mut rule_map = RuleMap::new(start_rule_id);
        rule_map.format_block_map(file_alias_name, block_map)?;

        return Ok(rule_map);
    }

    pub fn format_block_map(&mut self, file_alias_name: &String, block_map: &mut BlockMap) -> std::result::Result<(), BlockParseError> {
        for (block_name, each_block) in block_map {
            if block_name == "Main" {
                continue;
            }

            let mut block_alias_map = HashMap::<String, String>::new();

            // start コマンドを排除しながら use コマンドを処理する
            for each_cmd in &each_block.cmds {
                match each_cmd {
                    BlockCommand::Start(_line, _file_alias_name, _block_name, _rule_name) => return Err(BlockParseError::StartCmdOutsideMainBlock()),
                    BlockCommand::Use(line, file_alias_name, block_name, block_alias_name) => {
                        if block_alias_map.contains_key(&block_alias_name.clone()) {
                            return Err(BlockParseError::DuplicatedBlockAliasName(line.clone(), block_alias_name.clone()));
                        }

                        let rule_id = format!("{}.{}", file_alias_name.clone(), block_name.clone());
                        block_alias_map.insert(block_alias_name.clone(), rule_id);
                    },
                    _ => (),
                }
            }

            // define コマンドを処理する
            for each_cmd in &each_block.cmds {
                match each_cmd {
                    BlockCommand::Define(_line, rule) => {
                        let mut each_rule = rule.clone();
                        each_rule.name = format!("{}.{}.{}", file_alias_name, each_block.name, each_rule.name);
                        self.proc_define_cmd(&mut *each_rule.group, &each_rule.name, &each_block.name, file_alias_name, &block_alias_map)?;
                        let rule_id = RuleMap::get_rule_id(file_alias_name.clone(), each_block.name.clone(), rule.name.clone());
                        self.insert_rule(rule_id, each_rule);
                    },
                    _ => (),
                }
            }
        }

        return Ok(());
    }

    pub fn proc_define_cmd(&mut self, group: &mut RuleGroup, rule_id: &String, block_name: &String, file_alias_name: &String, block_alias_map: &HashMap<String, String>) -> std::result::Result<(), BlockParseError> {
        for each_elem in group.sub_elems.iter_mut() {
            match each_elem {
                RuleElement::Group(each_group) => {
                    self.proc_define_cmd(each_group, rule_id, block_name, file_alias_name, block_alias_map)?;
                },
                RuleElement::Expression(each_expr) => {
                    if each_expr.kind == RuleExpressionKind::ID {
                        let id_tokens: Vec<&str> = each_expr.value.split(".").collect();

                        if id_tokens.len() == 1 {
                            each_expr.value = format!("{}.{}.{}", file_alias_name, block_name, each_expr.value);
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
                                return Err(BlockParseError::BlockAliasNotFound(each_expr.line, block_name.to_string()));
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

                        return Err(BlockParseError::InternalErr(format!("invalid id expression '{}'", each_expr.value)));
                    }

                    if each_expr.kind == RuleExpressionKind::CharClass {
                        if self.regex_map.contains_key(&each_expr.value) {
                            continue;
                        }

                        let pattern = match Regex::new(&each_expr.value.clone()) {
                            Err(_e) => return Err(BlockParseError::InvalidCharClassFormat(each_expr.line, each_expr.to_string())),
                            Ok(v) => v,
                        };

                        self.regex_map.insert(each_expr.value.clone(), pattern);
                    }
                },
            }
        }

        return Ok(());
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleElementLookaheadKind {
    None,
    Positive,
    Negative,
}

impl RuleElementLookaheadKind {
    // ret: 文字がマッチしなければ RuleElementLookaheadKind::None
    pub fn new(value: &str) -> RuleElementLookaheadKind {
        return match value {
            "&" => RuleElementLookaheadKind::Positive,
            "!" => RuleElementLookaheadKind::Negative,
            _ => RuleElementLookaheadKind::None,
        }
    }

    pub fn is_none(&self) -> bool {
        return *self == RuleElementLookaheadKind::None;
    }
}

impl Display for RuleElementLookaheadKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            RuleElementLookaheadKind::None => "",
            RuleElementLookaheadKind::Positive => "&",
            RuleElementLookaheadKind::Negative => "!",
        };

        return write!(f, "{}", s);
    }
}

#[derive(Clone)]
pub struct Rule {
    pub name: String,
    pub macro_args: Vec<String>,
    pub group: Box<RuleGroup>,
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let macro_args_text = if self.macro_args.len() == 0 {
            String::new()
        } else {
            format!("({})", self.macro_args.join(", "))
        };

        return write!(f, "{}{} <- {}", self.name, macro_args_text, self.group);
    }
}

impl Rule {
    pub fn new(name: String, macro_args: Vec<String>, group: Box<RuleGroup>) -> Rule {
        return Rule {
            name: name,
            macro_args: macro_args,
            group: group,
        };
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Infinitable<T: Clone + PartialEq + PartialOrd> {
    Normal(T),
    Infinite,
}

impl<T: Clone + PartialEq + PartialOrd> Infinitable<T> {
    pub fn is_infinite(&self) -> bool {
        return *self == Infinitable::<T>::Infinite;
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub struct RuleElementLoopCount {
    pub min: usize,
    pub max: Infinitable<usize>,
}

impl RuleElementLoopCount {
    pub fn new(min: usize, max: Infinitable<usize>) -> RuleElementLoopCount {
        return RuleElementLoopCount {
            min: min,
            max: max,
        };
    }

    pub fn from_symbol(value: &str) -> RuleElementLoopCount {
        return match value {
            "?" => RuleElementLoopCount::new(0, Infinitable::Normal(1)),
            "*" => RuleElementLoopCount::new(0, Infinitable::Infinite),
            "+" => RuleElementLoopCount::new(1, Infinitable::Infinite),
            _ => RuleElementLoopCount::new(1, Infinitable::Normal(1)),
        }
    }

    pub fn get_single_loop() -> RuleElementLoopCount {
        return RuleElementLoopCount::new(1, Infinitable::Normal(1));
    }

    pub fn is_single_loop(&self) -> bool {
        return self.min == 1 && self.max == Infinitable::Normal(1);
    }

    pub fn to_string(&self, is_loop_count: bool, prefix: &str, separator: &str, suffix: &str) -> String {
        if self.is_single_loop() {
            return String::new();
        }

        if is_loop_count {
            match self.to_tuple() {
                (0, 1) => return "?".to_string(),
                (0, -1) => return "*".to_string(),
                (1, -1) => return "+".to_string(),
                _ => (),
            }
        }

        let min_count = if self.min == 0 { String::new() } else { self.min.to_string() };
        let max_count = match self.max { Infinitable::Normal(max_num) => max_num.to_string(), Infinitable::Infinite => String::new(), };
        return format!("{}{}{}{}{}", prefix, min_count, separator, max_count, suffix);
    }

    pub fn to_tuple(&self) -> (usize, i32) {
        let max_num = match self.max {
            Infinitable::Normal(num) => num as i32,
            Infinitable::Infinite => -1,
        };

        return (self.min, max_num)
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleElementOrder {
    Random(RuleElementLoopCount),
    Sequential,
}

impl RuleElementOrder {
    pub fn is_random(&self) -> bool {
        return *self != RuleElementOrder::Sequential;
    }
}

impl Display for RuleElementOrder {
    fn fmt(&self, f: &mut Formatter) -> Result {
        return match self {
            RuleElementOrder::Random(loop_count) => write!(f, "Random({})", loop_count.to_string(true, "{", ",", "}")),
            RuleElementOrder::Sequential => write!(f, "Sequential"),
        }
    }
}

#[derive(Clone)]
pub enum RuleElement {
    Group(Box<RuleGroup>),
    Expression(Box<RuleExpression>),
}

impl Display for RuleElement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        return match self {
            RuleElement::Group(group) => write!(f, "{}", group),
            RuleElement::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleGroupKind {
    Choice,
    Sequence,
}

impl Display for RuleGroupKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            RuleGroupKind::Choice => "Choice",
            RuleGroupKind::Sequence => "Sequence",
        };

        return write!(f, "{}", s);
    }
}

#[derive(Clone)]
pub struct RuleGroup {
    pub kind: RuleGroupKind,
    pub sub_elems: Vec<RuleElement>,
    pub ast_reflection_style: ASTReflectionStyle,
    pub lookahead_kind: RuleElementLookaheadKind,
    pub loop_count: RuleElementLoopCount,
    pub elem_order: RuleElementOrder,
}

impl RuleGroup {
    pub fn new(kind: RuleGroupKind) -> RuleGroup {
        return RuleGroup {
            kind: kind,
            sub_elems: vec![],
            lookahead_kind: RuleElementLookaheadKind::None,
            loop_count: RuleElementLoopCount::get_single_loop(),
            ast_reflection_style: ASTReflectionStyle::Reflection(String::new()),
            elem_order: RuleElementOrder::Sequential,
        };
    }

    // todo: 関数の役割を検証
    pub fn has_choices(&self) -> bool {
        for each_elem in &self.sub_elems {
            match each_elem {
                RuleElement::Group(_) => return true,
                _ => (),
            }
        }

        return false;
    }

    pub fn is_hierarchy_omission_needed(choices: &Vec<Box<RuleGroup>>, is_random_order: bool) -> Option<Box<RuleGroup>> {
        match choices.get(0) {
            Some(v) => {
                if choices.len() == 1 && v.lookahead_kind == RuleElementLookaheadKind::None && v.loop_count.to_tuple() == (1, 1) && match &v.elem_order { RuleElementOrder::Random(loop_count) => loop_count.to_tuple() == (1, 1), RuleElementOrder::Sequential => false, } {
                    if is_random_order {
                        match v.sub_elems.get(0) {
                            Some(v2) => {
                                match v2 {
                                    RuleElement::Group(v3) => {
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

impl Display for RuleGroup {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let mut seq_text = Vec::<String>::new();

        for each_elem in &self.sub_elems {
            match each_elem {
                RuleElement::Group(each_group) => {
                    seq_text.push(each_group.to_string());
                },
                RuleElement::Expression(each_expr) => {
                    seq_text.push(format!("{}", each_expr));
                },
            }
        }

        let separator = match self.kind {
            RuleGroupKind::Choice => {
                match self.elem_order {
                    RuleElementOrder::Random(_) => ", ",
                    RuleElementOrder::Sequential => " : ",
                }
            },
            RuleGroupKind::Sequence => " ",
        };
        let loop_text = self.loop_count.to_string(true, "{", ",", "}");
        let order_text = match &self.elem_order { RuleElementOrder::Random(loop_count) => loop_count.to_string(false, "^[", "-", "]"), RuleElementOrder::Sequential => String::new(), };

        return write!(f, "{}", format!("{}({}){}{}{}", self.lookahead_kind, seq_text.join(separator), loop_text, order_text, self.ast_reflection_style));
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleExpressionKind {
    CharClass,
    ID,
    String,
    Wildcard,
}

impl Display for RuleExpressionKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            RuleExpressionKind::CharClass => "CharClass",
            RuleExpressionKind::ID => "ID",
            RuleExpressionKind::String => "String",
            RuleExpressionKind::Wildcard => "Wildcard",
        };

        write!(f, "{}", s)
    }
}

#[derive(Clone)]
pub struct RuleExpression {
    pub line: usize,
    pub kind: RuleExpressionKind,
    pub value: String,
    pub ast_reflection_style: ASTReflectionStyle,
    pub lookahead_kind: RuleElementLookaheadKind,
    pub loop_count: RuleElementLoopCount,
}

impl RuleExpression {
    pub fn new(line: usize, kind: RuleExpressionKind, value: String) -> RuleExpression {
        return RuleExpression {
            line: line,
            kind: kind,
            value: value,
            ast_reflection_style: ASTReflectionStyle::NoReflection,
            lookahead_kind: RuleElementLookaheadKind::None,
            loop_count: RuleElementLoopCount::get_single_loop(),
        }
    }
}

impl Display for RuleExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let loop_text = self.loop_count.to_string(true, "{", ",", "}");
        let value_text = match self.kind {
            RuleExpressionKind::CharClass => self.value.clone(),
            RuleExpressionKind::ID => self.value.clone(),
            RuleExpressionKind::String => format!("\"{}\"", self.value),
            RuleExpressionKind::Wildcard => ".".to_string(),
        };

        return write!(f, "{}{}{}{}", self.lookahead_kind, value_text, loop_text, self.ast_reflection_style);
    }
}
