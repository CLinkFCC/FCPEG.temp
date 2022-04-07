use crate::CharacterPosition;
use crate::block::*;
use crate::cons::Translator;

use std::collections::HashMap;

use cons_util::*;
use cons_util::cons::*;

pub type FcpilParsingResult<T> = Result<T, FcpilParsingLog>;

#[derive(Clone, PartialEq)]
pub enum FcpilParsingLog {
    UnexpectedEof { pos: CharacterPosition },
}

impl ConsoleLogger for FcpilParsingLog {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            FcpilParsingLog::UnexpectedEof { pos } => log!(Error, Translator::UnexpectedEof, Translator::AtDescription { pos: pos.clone() }),
        };
    }
}

pub struct FcpilParser;

impl FcpilParser {
    pub fn parse(cons: &mut Console, lines: Vec<&str>) -> ConsoleResult<RuleMap> {
        let mut map = HashMap::<RuleId, Rule>::new();

        for each_line in lines {
            let (rule_id, rule) = FcpilParser::parse_line(each_line).consume(cons)?;
            map.insert(rule_id, rule);
        }

        let rule_map = RuleMap::new(map);

        return Ok(rule_map);
    }

    pub fn parse_line(line: &str) -> FcpilParsingResult<(RuleId, Rule)> {
        // let str_pat: &'static Regex = &Regex::new("[a-zA-Z_]").unwrap();

        let mut raw_rule_id = String::new();

        for c in line.chars() {
            if c == ' ' {
                break;
            } else {
                raw_rule_id.push(c);
            }
        }

        let elems = RuleElement::Group(RuleGroup::new(RuleGroupKind::Choice, Vec::new(), AstReflectionStyle::Reflective(String::new())));
        let rule_id = RuleId(raw_rule_id);
        let rule = Rule::new(rule_id.clone(), elems);
        return Ok((rule_id, rule));
    }
}
