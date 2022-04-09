use crate::{GeneralSource, MultilineSource};
use crate::il::FcpilParser;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use cons_util::cons::{Console, ConsoleResult};

use regex::Regex;

pub struct BlockMap {}

impl BlockMap {
    pub fn from_fcpeg(_src: GeneralSource) -> BlockMap {
        unimplemented!();
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct RuleId(pub String);

impl RuleId {
    pub fn get_file_name(&self) -> String {
        return self.get_token_at(0);
    }

    pub fn get_block_name(&self) -> String {
        return self.get_token_at(1);
    }

    pub fn get_rule_name(&self) -> String {
        return self.get_token_at(2);
    }

    pub fn get_token_at(&self, index: usize) -> String {
        let tokens = self.0.split(".").collect::<Vec<&str>>();

        return match tokens.get(index) {
            Some(v) => v.to_string(),
            None => String::new(),
        };
    }
}

impl std::fmt::Debug for RuleId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{:?}", self.0);
    }
}

impl Into<String> for RuleId {
    fn into(self) -> String {
        return self.0;
    }
}

impl Display for RuleId {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        return write!(f, "{}", self.0);
    }
}

impl From<String> for RuleId {
    fn from(id: String) -> RuleId {
        return RuleId(id);
    }
}

pub struct RuleMap(pub HashMap<RuleId, Rule>);

impl RuleMap {
    pub fn new(map: HashMap<RuleId, Rule>) -> RuleMap {
        return RuleMap(map);
    }

    pub fn from_block_map(_cons: &mut Console, _block_map: BlockMap) -> RuleMap {
        unimplemented!();
    }

    pub fn from_fcpil(cons: &mut Console, src: MultilineSource) -> ConsoleResult<RuleMap> {
        return FcpilParser::parse(cons, src);
    }
}

impl std::fmt::Debug for RuleMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{:?}", self.0);
    }
}

#[derive(Clone, Debug)]
pub struct Rule {
    id: RuleId,
    elem: RuleElement,
}

impl Rule {
    pub fn new(id: RuleId, elem: RuleElement) -> Rule {
        return Rule {
            id: id,
            elem: elem,
        };
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        return write!(f, "{} <- {}", self.id.get_rule_name(), self.elem);
    }
}

#[derive(Clone, Debug)]
pub enum RuleElement {
    Group(RuleGroup),
    Expression(RuleExpression),
}

impl Display for RuleElement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        return match self {
            RuleElement::Group(group) => write!(f, "{}", group),
            RuleElement::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstReflectionStyle {
    // arg: 反映名 (reflection name)
    Reflective(String),
    Expansive,
    Unreflective,
}

impl AstReflectionStyle {
    pub fn is_reflective(&self) -> bool {
        return *self != AstReflectionStyle::Unreflective;
    }

    pub fn is_expansive(&self) -> bool {
        return *self == AstReflectionStyle::Expansive;
    }
}

impl Display for AstReflectionStyle {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let s = match self {
            AstReflectionStyle::Reflective(reflect_name) => format!("#{}", reflect_name.clone()),
            AstReflectionStyle::Expansive => "##".to_string(),
            AstReflectionStyle::Unreflective => String::new(),
        };

        return write!(f, "{}", s);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum RuleGroupKind {
    Choice,
    Sequence,
}

impl Display for RuleGroupKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let s = match self {
            RuleGroupKind::Choice => "Choice",
            RuleGroupKind::Sequence => "Sequence",
        };

        return write!(f, "{}", s);
    }
}

thread_local!{
    static RULE_GROUP_INDEX_COUNT: RefCell<usize> = RefCell::new(0);
}

#[derive(Clone, Debug)]
pub struct RuleGroup {
    pub index: usize,
    pub kind: RuleGroupKind,
    pub subelems: Vec<RuleElement>,
    pub ast_reflection_style: AstReflectionStyle,
}

impl RuleGroup {
    pub fn new(kind: RuleGroupKind, subelems: Vec<RuleElement>, ast_reflection_style: AstReflectionStyle) -> RuleGroup {
        /*
         * Initialize with 0 due to the following compilation error:
         * > borrow of possibly-uninitialized variable: `index`
         */
        let mut index = 0usize;

        RULE_GROUP_INDEX_COUNT.with(|count| {
            index = *count.borrow();
            *count.borrow_mut() += 1;
        });

        return RuleGroup {
            index: index,
            kind: kind,
            subelems: subelems,
            ast_reflection_style: ast_reflection_style,
        };
    }
}

impl Display for RuleGroup {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut seq_text = Vec::<String>::new();

        for each_elem in &self.subelems {
            match each_elem {
                RuleElement::Group(each_group) => seq_text.push(each_group.to_string()),
                RuleElement::Expression(each_expr) => seq_text.push(format!("{}", each_expr)),
            }
        }

        let separator = " : ";
        return write!(f, "{}", format!("({}){}", seq_text.join(separator), self.ast_reflection_style));
    }
}

#[derive(Clone, Debug)]
pub enum RuleExpressionKind {
    Argument(String),
    CharacterClass(Regex),
    Id(RuleId),
    String(String),
    Wildcard,
}

impl Display for RuleExpressionKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let s = match self {
            RuleExpressionKind::Argument(name) => name.to_string(),
            RuleExpressionKind::CharacterClass(regex) => regex.to_string(),
            RuleExpressionKind::Id(id) => id.to_string(),
            RuleExpressionKind::String(value) => format!("\"{}\"", value)
                .replace("\0", "\\0")
                .replace("\n", "\\n")
                .replace("\"", "\\\""),
            RuleExpressionKind::Wildcard => ".".to_string(),
        };

        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug)]
pub struct RuleExpression {
    pub kind: RuleExpressionKind,
    pub ast_reflection_style: AstReflectionStyle,
}

impl RuleExpression {
    pub fn new(kind: RuleExpressionKind) -> RuleExpression {
        return RuleExpression {
            kind: kind,
            ast_reflection_style: AstReflectionStyle::Unreflective,
        }
    }
}

impl Display for RuleExpression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let expr_txt = self.kind.to_string();
        return write!(f, "{}{}", expr_txt, self.ast_reflection_style);
    }
}
