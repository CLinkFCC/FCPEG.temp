use {
    crate::{
        *,
        rule::*,
    },

    std::collections::HashMap,

    cons_util::cons::*,
};

pub type FcpilParsingResult<T> = Result<T, FcpilParsingLog>;

#[derive(Clone, cons_util_derive::ConsoleLogTranslator, Debug, PartialEq)]
pub enum FcpilParsingLog {
    #[translate(
        kind = "E",
        en = "expected expression or group, found linebreak\n\tat: {pos}",
        ja = "表現字句もしくはグループが必要ですが、改行が見つかりました\n\t位置: {pos}",
    )]
    ExpectedExpressionOrGroupFoundLinebreak { pos: SourcePosition },

    #[translate(
        kind = "E",
        en = "rule ID `{rule_id}` is duplicate\n\tat: {pos}",
        ja = "規則 ID `{rule_id}` が重複しています\n\t位置: {pos}",
    )]
    RuleIdIsDuplicate { pos: SourcePosition, rule_id: RuleId },
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
