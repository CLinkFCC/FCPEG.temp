use std::collections::*;
use std::fmt::*;

use crate::tree::*;

use num_traits::Num;

use cons_util::cons::ConsoleResult;

use uuid::Uuid;

pub type AttributeMap = HashMap<String, Attribute>;

#[derive(Clone, PartialEq)]
pub enum AttributeKind {
    Skip,
}

impl AttributeKind {
    pub fn from(v: &str) -> Option<AttributeKind> {
        let kind = match v {
            "skip" => AttributeKind::Skip,
            _ => return None,
        };

        return Some(kind);
    }
}

#[derive(Clone, PartialEq)]
pub struct Attribute {
    pub pos: CharacterPosition,
    pub name: String,
    pub values: Vec<AttributeValue>,
}

impl Attribute {
    pub fn new(pos: CharacterPosition, name: String, values: Vec<AttributeValue>) -> Attribute {
        return Attribute {
            pos: pos,
            name: name,
            values: values,
        };
    }

    pub fn values(&self) -> Vec<String> {
        return self.values.iter().map(|v| v.value.clone()).collect::<Vec<String>>();
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let values_txt = self.values.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", ");
        return write!(f, "@{}[{}]", self.name, values_txt);
    }
}

#[derive(Clone, PartialEq)]
pub struct AttributeValue {
    pub pos: CharacterPosition,
    pub is_negative: bool,
    pub value: String,
}

impl AttributeValue {
    pub fn new(pos: CharacterPosition, is_negative: bool, value: String) -> AttributeValue {
        return AttributeValue {
            pos: pos,
            is_negative: is_negative,
            value: value,
        };
    }
}

impl Display for AttributeValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let negative_symbol = if self.is_negative { "!" } else { "" };
        return write!(f, "{}{}", negative_symbol, self.value);
    }
}

pub type BlockId = String;
pub type BlockMap = HashMap<BlockId, Box<Block>>;

#[derive(Clone)]
pub struct Block {
    pub name: String,
    pub cmds: Vec<BlockCommand>,
    pub attr_map: AttributeMap,
}

impl Block {
    pub fn new(name: String, cmds: Vec<BlockCommand>, attr_map: AttributeMap) -> Block {
        return Block {
            name: name,
            cmds: cmds,
            attr_map: attr_map,
        };
    }

    pub fn print(&self) {
        println!("[{}]{{{}}}", self.name, self.cmds.iter().map(|v| format!("    {}", v)).collect::<Vec<String>>().join(""));
    }
}

#[derive(Clone)]
pub enum BlockCommand {
    Comment { pos: CharacterPosition, value: String },
    Define { pos: CharacterPosition, rule: Rule, attr_map: AttributeMap },
    Start { pos: CharacterPosition, file_alias_name: String, block_name: String, rule_name: String },
    Use { pos: CharacterPosition, file_alias_name: String, block_name: String, block_alias_name: String },
}

impl Display for BlockCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            BlockCommand::Comment { pos, value } => format!("{}| %{},", pos.line, value),
            // todo: 属性マップを表示
            BlockCommand::Define { pos, rule, attr_map: _ } => format!("{}| rule {}", pos.line, rule),
            BlockCommand::Start { pos, file_alias_name, block_name, rule_name } => format!("{}| start rule '{}.{}.{}'", pos.line, file_alias_name, block_name, rule_name),
            BlockCommand::Use { pos, file_alias_name, block_name, block_alias_name } => format!("{}| use block '{}.{}' as '{}'", pos.line, file_alias_name, block_name, block_alias_name),
        };

        return write!(f, "{}", s);
    }
}

pub type RuleId = String;

pub struct RuleMap {
    pub rule_map: HashMap<RuleId, Box<Rule>>,
    pub start_rule_pos: CharacterPosition,
    pub start_rule_id: RuleId,
}

impl RuleMap {
    pub fn new(block_map: Vec<BlockMap>, start_rule_id: RuleId) -> ConsoleResult<RuleMap> {
        let raw_rule_map = RuleMap::to_rule_map(block_map)?;

        let start_rule_pos = match raw_rule_map.get(&start_rule_id) {
            Some(v) => v.pos.clone(),
            None => CharacterPosition::get_empty(),
        };

        let rule_map = RuleMap {
            rule_map: raw_rule_map,
            start_rule_pos: start_rule_pos,
            start_rule_id: start_rule_id,
        };

        return Ok(rule_map);
    }

    fn to_rule_map(block_maps: Vec<BlockMap>) -> ConsoleResult<HashMap<String, Box<Rule>>> {
        let mut rule_map = HashMap::<String, Box<Rule>>::new();

        for each_block_map in block_maps {
            for (_, each_block) in each_block_map {
                for each_cmd in each_block.cmds {
                    match each_cmd {
                        BlockCommand::Define { pos: _, rule, attr_map: _ } => {
                            rule_map.insert(rule.id.clone(), Box::new(rule));
                        },
                        _ => (),
                    }
                }
            }
        }

        return Ok(rule_map);
    }
}

impl Display for RuleMap {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let mut rule_text_lines = Vec::<String>::new();

        for each_rule in self.rule_map.values() {
            rule_text_lines.push(each_rule.to_string());
        }

        return writeln!(f, "{}", rule_text_lines.join("\n"));
    }
}

#[derive(Clone)]
pub struct Rule {
    pub pos: CharacterPosition,
    pub id: String,
    pub name: String,
    pub generics_arg_ids: Vec<String>,
    pub template_arg_ids: Vec<String>,
    pub skipping_tar_ids: Vec<RuleId>,
    pub group: Box<RuleGroup>,
}

impl Rule {
    pub fn new(pos: CharacterPosition, id: String, name: String, generics_arg_ids: Vec<String>, template_arg_ids: Vec<String>, skipping_tar_ids: Vec<RuleId>, group: Box<RuleGroup>) -> Rule {
        return Rule {
            pos: pos,
            id: id,
            name: name,
            generics_arg_ids: generics_arg_ids,
            template_arg_ids: template_arg_ids,
            skipping_tar_ids: skipping_tar_ids,
            group: group,
        };
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let generics_arg_id_text = if self.generics_arg_ids.len() == 0 {
            String::new()
        } else {
            format!("({})", self.generics_arg_ids.iter().map(|s| format!("${}", s)).collect::<Vec<String>>().join(", "))
        };

        return write!(f, "{}{} <- {}", self.name, generics_arg_id_text, self.group);
    }
}

#[derive(Clone, PartialEq)]
pub enum LookaheadKind {
    None,
    Positive,
    Negative,
}

impl LookaheadKind {
    // ret: 文字がマッチしなければ LookaheadKind::None
    pub fn new(value: &str) -> LookaheadKind {
        return match value {
            "&" => LookaheadKind::Positive,
            "!" => LookaheadKind::Negative,
            _ => LookaheadKind::None,
        }
    }

    pub fn is_none(&self) -> bool {
        return *self == LookaheadKind::None;
    }
}

impl Display for LookaheadKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            LookaheadKind::None => "",
            LookaheadKind::Positive => "&",
            LookaheadKind::Negative => "!",
        };

        return write!(f, "{}", s);
    }
}

#[derive(Clone, PartialEq)]
pub enum Infinitable<T: Clone + Display + Num + PartialEq> {
    Finite(T),
    Infinite,
}

impl<T: Clone + Display + Num + PartialEq> Infinitable<T> {
    pub fn is_infinite(&self) -> bool {
        return *self == Infinitable::<T>::Infinite;
    }
}

impl<T: Clone + Display + Num + PartialEq> Display for Infinitable<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            Infinitable::Finite(v) => v.to_string(),
            Infinitable::Infinite => "Infinite".to_string(),
        };

        return write!(f, "{}", s);
    }
}

#[derive(Clone, PartialEq)]
pub struct LoopRange {
    pub min: usize,
    pub max: Infinitable<usize>,
}

impl LoopRange {
    pub fn new(min: usize, max: Infinitable<usize>) -> LoopRange {
        return LoopRange {
            min: min,
            max: max,
        };
    }

    pub fn from(value: &str) -> LoopRange {
        return match value {
            "?" => LoopRange::new(0, Infinitable::Finite(1)),
            "*" => LoopRange::new(0, Infinitable::Infinite),
            "+" => LoopRange::new(1, Infinitable::Infinite),
            _ => LoopRange::new(1, Infinitable::Finite(1)),
        }
    }

    pub fn get_single_loop() -> LoopRange {
        return LoopRange::new(1, Infinitable::Finite(1));
    }

    pub fn is_single_loop(&self) -> bool {
        return self.min == 1 && self.max == Infinitable::Finite(1);
    }

    pub fn to_symbol_string(&self) -> Option<String> {
        return match self.to_tuple() {
            (0, 1) => Some("?".to_string()),
            (0, -1) => Some("*".to_string()),
            (1, -1) => Some("+".to_string()),
            _ => None,
        }
    }

    pub fn to_string(&self, is_loop_count: bool, prefix: &str, opening: &str, separator: &str, closing: &str) -> String {
        if self.is_single_loop() {
            return prefix.to_string();
        }

        if is_loop_count {
            match self.to_symbol_string() {
                Some(v) => return v,
                None => (),
            }
        }

        let min_count = if self.min == 0 {
            String::new()
        } else {
            self.min.to_string()
        };

        let max_count = match self.max {
            Infinitable::Finite(max_num) => max_num.to_string(),
            Infinitable::Infinite => String::new(),
        };

        return format!("{}{}{}{}{}{}", prefix, opening, min_count, separator, max_count, closing);
    }

    pub fn to_tuple(&self) -> (usize, isize) {
        let max_num = match self.max {
            Infinitable::Finite(num) => num as isize,
            Infinitable::Infinite => -1,
        };

        return (self.min, max_num)
    }
}

#[derive(Clone, PartialEq)]
pub enum ElementOrder {
    Random(LoopRange),
    Sequential,
}

impl ElementOrder {
    pub fn is_random(&self) -> bool {
        return *self != ElementOrder::Sequential;
    }
}

impl Display for ElementOrder {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            ElementOrder::Random(loop_range) => format!("{}", loop_range.to_string(false, "^", "[", "-", "]")),
            ElementOrder::Sequential => format!(""),
        };

        return write!(f, "{}", s);
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

#[derive(Clone, PartialEq)]
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
    pub uuid: Uuid,
    pub kind: RuleGroupKind,
    pub subelems: Vec<RuleElement>,
    pub ast_reflection_style: ASTReflectionStyle,
    pub lookahead_kind: LookaheadKind,
    pub loop_range: LoopRange,
    pub elem_order: ElementOrder,
}

impl RuleGroup {
    pub fn new(kind: RuleGroupKind) -> RuleGroup {
        return RuleGroup {
            uuid: Uuid::new_v4(),
            kind: kind,
            subelems: Vec::new(),
            lookahead_kind: LookaheadKind::None,
            loop_range: LoopRange::get_single_loop(),
            ast_reflection_style: ASTReflectionStyle::Reflection(String::new()),
            elem_order: ElementOrder::Sequential,
        };
    }
}

impl Display for RuleGroup {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let mut seq_text = Vec::<String>::new();

        for each_elem in &self.subelems {
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
                    ElementOrder::Random(_) => ", ",
                    ElementOrder::Sequential => " : ",
                }
            },
            RuleGroupKind::Sequence => " ",
        };

        let loop_text = self.loop_range.to_string(true, "", "{", ",", "}");

        return write!(f, "{}", format!("{}({}){}{}{}", self.lookahead_kind, seq_text.join(separator), loop_text, self.elem_order, self.ast_reflection_style));
    }
}

#[derive(Clone)]
pub enum RuleExpressionKind {
    ArgId,
    CharClass,
    Id,
    IdWithArgs { generics_args: Vec<Box<RuleGroup>>, template_args: Vec<Box<RuleGroup>> },
    String,
    Wildcard,
}

impl Display for RuleExpressionKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            RuleExpressionKind::ArgId => "ArgID",
            RuleExpressionKind::CharClass => "CharClass",
            RuleExpressionKind::Id => "ID",
            RuleExpressionKind::IdWithArgs { generics_args: _, template_args: _ } => "ID",
            RuleExpressionKind::String => "String",
            RuleExpressionKind::Wildcard => "Wildcard",
        };

        write!(f, "{}", s)
    }
}

#[derive(Clone)]
pub struct RuleExpression {
    pub pos: CharacterPosition,
    pub kind: RuleExpressionKind,
    pub value: String,
    pub ast_reflection_style: ASTReflectionStyle,
    pub lookahead_kind: LookaheadKind,
    pub loop_range: LoopRange,
}

impl RuleExpression {
    pub fn new(pos: CharacterPosition, kind: RuleExpressionKind, value: String) -> RuleExpression {
        return RuleExpression {
            pos: pos,
            kind: kind,
            value: value,
            ast_reflection_style: ASTReflectionStyle::NoReflection,
            lookahead_kind: LookaheadKind::None,
            loop_range: LoopRange::get_single_loop(),
        }
    }
}

impl Display for RuleExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let loop_text = self.loop_range.to_string(true, "", "{", ",", "}");
        let value_text = match self.kind.clone() {
            RuleExpressionKind::ArgId => format!("${}", self.value),
            RuleExpressionKind::CharClass => self.value.clone(),
            RuleExpressionKind::Id => self.value.clone(),
            RuleExpressionKind::IdWithArgs { generics_args, template_args } => {
                let generics_text = {
                    if generics_args.len() != 0 {
                        let generics_arg_text = generics_args.iter().map(|v| v.to_string()).collect::<Vec<String>>();
                        format!("{}<{}>", self.value, generics_arg_text.join(", "))
                    } else {
                        String::new()
                    }
                };

                let template_text = {
                    if template_args.len() != 0 {
                        let template_arg_text = template_args.iter().map(|v| v.to_string()).collect::<Vec<String>>();
                        format!("{}({})", self.value, template_arg_text.join(", "))
                    } else {
                        String::new()
                    }
                };

                format!("{}{}{}", self.value, generics_text, template_text)
            },
            RuleExpressionKind::String => format!("\"{}\"", self.value),
            RuleExpressionKind::Wildcard => ".".to_string(),
        }.replace("\0", "\\0").replace("\n", "\\n");

        return write!(f, "{}{}{}{}", self.lookahead_kind, value_text, loop_text, self.ast_reflection_style);
    }
}
