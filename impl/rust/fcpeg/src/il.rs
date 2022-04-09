use crate::*;
use crate::block::*;
use crate::cons::Translator;

use std::collections::HashMap;

use cons_util::*;
use cons_util::cons::*;

pub type FcpilParsingResult<T> = Result<T, FcpilParsingLog>;

#[derive(Clone, PartialEq)]
pub enum FcpilParsingLog {
    UnexpectedEof { pos: SourcePosition },
}

impl ConsoleLogger for FcpilParsingLog {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            FcpilParsingLog::UnexpectedEof { pos } => log!(Error, Translator::UnexpectedEof, Translator::AtDescription { pos: pos.clone() }),
        };
    }
}

pub struct FcpilParser {
    src: MultilineSource,
    line_i: usize,
    char_i: usize,
}

impl FcpilParser {
    fn new(src: MultilineSource) -> FcpilParser {
        return FcpilParser {
            src: src,
            line_i: 0,
            char_i: 0,
        };
    }

    pub fn parse(cons: &mut Console, src: MultilineSource) -> ConsoleResult<RuleMap> {
        let mut map = HashMap::<RuleId, Rule>::new();
        let lines = src.as_content_ref();
        let mut parser = FcpilParser::new(src);

        for each_line in lines {
            let (rule_id, rule) = parser.parse_line(each_line).consume(cons)?;
            map.insert(rule_id, rule);
        }

        return Ok(RuleMap::new(map));
    }

    fn parse_line(&mut self, line: &String) -> FcpilParsingResult<(RuleId, Rule)> {
        let mut column_i = 0usize;
        let mut chars = line.chars();
        let mut raw_rule_id = String::new();

        let mut next = || -> Option<char> {
            column_i += 1;
            self.char_i += 1;
            return chars.next();
        };

        let get_src_pos = |parser: &FcpilParser, column_i: usize| -> SourcePosition {
            return SourcePosition::column_from(&parser.src, parser.char_i, parser.line_i, column_i);
        };

        loop {
            match next() {
                Some(c) => {
                    if c == ' ' {
                        break;
                    } else {
                        raw_rule_id.push(c);
                    }
                },
                None => return Err(FcpilParsingLog::UnexpectedEof { pos: get_src_pos(&self, column_i) }),
            };
        }

        for c in chars.by_ref().take(self.char_i) {
            print!("{}", c);
        }

        println!();

        // add index of newline
        self.line_i += 1;
        self.char_i += 1;

        let elems = RuleElement::Group(RuleGroup::new(RuleGroupKind::Choice, Vec::new(), AstReflectionStyle::Reflective(String::new())));
        let rule_id = RuleId(raw_rule_id);
        let rule = Rule::new(rule_id.clone(), elems);
        return Ok((rule_id, rule));
    }
}
