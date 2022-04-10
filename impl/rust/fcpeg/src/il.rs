use crate::*;
use crate::rule::*;
use crate::cons::Translator;

use std::collections::HashMap;

use cons_util::*;
use cons_util::cons::*;

pub type FcpilParsingResult<T> = Result<T, FcpilParsingLog>;

#[derive(Clone, PartialEq)]
pub enum FcpilParsingLog {
    ExpectedExpressionOrGroupFoundLinebreak { pos: SourcePosition },
    RuleIdIsDuplicate { pos: SourcePosition, rule_id: RuleId },
}

impl ConsoleLogger for FcpilParsingLog {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            FcpilParsingLog::ExpectedExpressionOrGroupFoundLinebreak { pos } => log!(Error, Translator::ExpectedExpressionOrGroupFoundLinebreak, Translator::AtDescription { pos: pos.clone() }),
            FcpilParsingLog::RuleIdIsDuplicate { pos, rule_id } => log!(Error, Translator::RuleIdIsDuplicate { rule_id: rule_id.clone() }, Translator::AtDescription { pos: pos.clone() }),
        };
    }
}

pub struct FcpilParser<'a> {
    src: &'a MultilineSource,
    rule_map: HashMap<RuleId, Rule>,
    line_i: usize,
    char_i: usize,
}

impl<'a> FcpilParser<'a> {
    fn new(src: &'a MultilineSource) -> FcpilParser<'a> {
        return FcpilParser {
            src: src,
            rule_map: HashMap::new(),
            line_i: 0,
            char_i: 0,
        };
    }

    pub fn parse(src: MultilineSource) -> FcpilParsingResult<RuleMap> {
        let mut parser = FcpilParser::new(&src);

        for each_line in src.as_content_ref() {
            match parser.parse_line(each_line)? {
                Some(rule) => {
                    parser.rule_map.insert(rule.id.clone(), rule);
                },
                None => (),
            }
        }

        return Ok(RuleMap::new(parser.rule_map));
    }

    fn parse_line(&mut self, line: &String) -> FcpilParsingResult<Option<Rule>> {
        if line.len() == 0 || line.starts_with("--") {
            return Ok(None);
        }

        let mut column_i = 0usize;
        let mut chars = line.chars();
        let mut raw_rule_id = String::new();

        macro_rules! next {
            () => {
                {
                    column_i += 1;
                    self.char_i += 1;
                    chars.next()
                }
            };
        }

        macro_rules! pos {
            () => {
                SourcePosition::column_from(&self.src, self.char_i, self.line_i, column_i)
            };

            ($column_diff:expr) => {
                {
                    let corrected_char_i = (self.char_i as i64 + $column_diff) as usize;
                    let corrected_column_i = (column_i as i64 + $column_diff) as usize;
                    SourcePosition::column_from(&self.src, corrected_char_i, self.line_i, corrected_column_i)
                }
            };
        }

        let begin_pos = pos!();

        /* Rule ID */

        loop {
            match next!() {
                Some(c) => {
                    if c == ' ' {
                        if line.len() == column_i {
                            return Err(FcpilParsingLog::ExpectedExpressionOrGroupFoundLinebreak { pos: pos!() });
                        }

                        break;
                    } else {
                        raw_rule_id.push(c);
                    }
                },
                None => return Err(FcpilParsingLog::ExpectedExpressionOrGroupFoundLinebreak { pos: pos!(-1) }),
            };
        }

        let rule_id = RuleId(raw_rule_id);

        // Return error if rule ID is duplicate.
        if self.rule_map.contains_key(&rule_id) {
            return Err(FcpilParsingLog::RuleIdIsDuplicate { pos: begin_pos.clone(), rule_id: rule_id.clone() });
        }

        /* Expressions and Groups */

        /* Finalization */

        // Add index of newline.
        self.line_i += 1;
        self.char_i += 1;

        let elems = RuleElement::Group(RuleGroup::new(RuleGroupKind::Choice, Vec::new(), AstReflectionStyle::Reflective(String::new())));
        let rule = Rule::new(begin_pos, rule_id, elems);
        return Ok(Some(rule));
    }
}
