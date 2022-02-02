use std::collections::*;
use std::fmt::*;

use crate::block::*;
use crate::tree::*;

use rustnutlib::console::ConsoleResult;

use uuid::Uuid;

#[derive(Clone)]
pub struct RuleMap {
    pub rule_map: HashMap<String, Box<Rule>>,
    pub start_rule_pos: CharacterPosition,
    pub start_rule_id: String,
}

impl RuleMap {
    pub fn new(block_map: Vec<BlockMap>, start_rule_id: String) -> ConsoleResult<RuleMap> {
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
                        BlockCommand::Define { pos: _, rule } => {
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
    pub pos: CharacterPosition,
    pub id: String,
    pub name: String,
    pub generics_arg_ids: Vec<String>,
    pub template_arg_ids: Vec<String>,
    pub group: Box<RuleGroup>,
}

impl Rule {
    pub fn new(pos: CharacterPosition, id: String, name: String, generics_arg_ids: Vec<String>, template_arg_ids: Vec<String>, group: Box<RuleGroup>) -> Rule {
        return Rule {
            pos: pos,
            id: id,
            name: name,
            generics_arg_ids: generics_arg_ids,
            template_arg_ids: template_arg_ids,
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

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Infinitable<T: Clone + Display + PartialEq + PartialOrd> {
    Finite(T),
    Infinite,
}

impl<T: Clone + Display + PartialEq + PartialOrd> Infinitable<T> {
    pub fn is_infinite(&self) -> bool {
        return *self == Infinitable::<T>::Infinite;
    }
}

impl<T: Clone + Display + PartialEq + PartialOrd> Display for Infinitable<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            Infinitable::Finite(v) => v.to_string(),
            Infinitable::Infinite => "Infinite".to_string(),
        };

        return write!(f, "{}", s);
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub struct RuleElementLoopRange {
    pub min: usize,
    pub max: Infinitable<usize>,
}

impl RuleElementLoopRange {
    pub fn new(min: usize, max: Infinitable<usize>) -> RuleElementLoopRange {
        return RuleElementLoopRange {
            min: min,
            max: max,
        };
    }

    pub fn from(value: &str) -> RuleElementLoopRange {
        return match value {
            "?" => RuleElementLoopRange::new(0, Infinitable::Finite(1)),
            "*" => RuleElementLoopRange::new(0, Infinitable::Infinite),
            "+" => RuleElementLoopRange::new(1, Infinitable::Infinite),
            _ => RuleElementLoopRange::new(1, Infinitable::Finite(1)),
        }
    }

    pub fn get_single_loop() -> RuleElementLoopRange {
        return RuleElementLoopRange::new(1, Infinitable::Finite(1));
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

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleElementOrder {
    Random(RuleElementLoopRange),
    Sequential,
}

impl RuleElementOrder {
    pub fn is_random(&self) -> bool {
        return *self != RuleElementOrder::Sequential;
    }
}

impl Display for RuleElementOrder {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let s = match self {
            RuleElementOrder::Random(loop_range) => format!("{}", loop_range.to_string(false, "^", "[", "-", "]")),
            RuleElementOrder::Sequential => format!(""),
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
    pub uuid: Uuid,
    pub kind: RuleGroupKind,
    pub sub_elems: Vec<RuleElement>,
    pub ast_reflection_style: ASTReflectionStyle,
    pub lookahead_kind: RuleElementLookaheadKind,
    pub loop_range: RuleElementLoopRange,
    pub elem_order: RuleElementOrder,
}

impl RuleGroup {
    pub fn new(kind: RuleGroupKind) -> RuleGroup {
        return RuleGroup {
            uuid: Uuid::new_v4(),
            kind: kind,
            sub_elems: Vec::new(),
            lookahead_kind: RuleElementLookaheadKind::None,
            loop_range: RuleElementLoopRange::get_single_loop(),
            ast_reflection_style: ASTReflectionStyle::Reflection(String::new()),
            elem_order: RuleElementOrder::Sequential,
        };
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
    pub lookahead_kind: RuleElementLookaheadKind,
    pub loop_range: RuleElementLoopRange,
}

impl RuleExpression {
    pub fn new(pos: CharacterPosition, kind: RuleExpressionKind, value: String) -> RuleExpression {
        return RuleExpression {
            pos: pos,
            kind: kind,
            value: value,
            ast_reflection_style: ASTReflectionStyle::NoReflection,
            lookahead_kind: RuleElementLookaheadKind::None,
            loop_range: RuleElementLoopRange::get_single_loop(),
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
