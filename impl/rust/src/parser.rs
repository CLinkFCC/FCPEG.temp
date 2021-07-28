use crate::data;
use rustnutlib::console;

pub enum SyntaxParseError {
    Unknown(),
    NoMatchedRule(String),
    UnknownRuleID(String),
}

impl SyntaxParseError {
    pub fn get_log_data(&self) -> console::ConsoleLogData {
        match self {
            SyntaxParseError::Unknown() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown error", vec![], vec![]),
            SyntaxParseError::NoMatchedRule(rule_id) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("no matched rule '{}'", rule_id), vec![], vec![]),
            SyntaxParseError::UnknownRuleID(rule_id) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unknown rule id '{}'", rule_id), vec![], vec![]),
        }
    }
}

pub struct SyntaxParser {
    rule_map: data::RuleMap,
    src_i: usize,
    src_content: String,
}

impl SyntaxParser {
    pub fn new(rule_map: data::RuleMap) -> std::result::Result<Self, SyntaxParseError> {
        return Ok(SyntaxParser {
            rule_map: rule_map,
            src_i: 0,
            src_content: "".to_string(),
        });
    }

    pub fn get_syntax_tree(&mut self, src_content: String) -> std::result::Result<data::SyntaxNode, SyntaxParseError> {
        // フィールドを初期化
        self.src_i = 0;
        self.src_content = src_content;

        let mut tree = data::SyntaxNode::new("root".to_string());

        let (nodes, leaves) = match self.is_rule_successful(&self.rule_map.start_rule_id.to_string())? {
            Some(v) => v,
            None => return Err(SyntaxParseError::NoMatchedRule(self.rule_map.start_rule_id.to_string())),
        };

        tree.nodes = nodes;
        tree.leaves = leaves;

        return Ok(tree);
    }

    #[inline(always)]
    fn is_rule_successful(&mut self, rule_id: &String) -> std::result::Result<std::option::Option<(Vec<data::SyntaxNode>, Vec<String>)>, SyntaxParseError> {
        let rule = match self.rule_map.get_rule(rule_id) {
            Some(v) => v.clone(),
            None => return Err(SyntaxParseError::UnknownRuleID(rule_id.to_string())),
        };

        for each_choice in &rule.choices {
            match self.is_choice_successful(each_choice)? {
                Some(v) => return Ok(Some(v)),
                None => continue,
            }
        }

        return Ok(None);
    }

    #[inline(always)]
    fn is_choice_successful(&mut self, choice: &data::RuleChoice) -> std::result::Result<std::option::Option<(Vec<data::SyntaxNode>, Vec<String>)>, SyntaxParseError> {
        for each_seq_group in &choice.seq_groups {
            match self.is_seq_group_successful(each_seq_group)? {
                Some(v) => return Ok(Some(v)),
                None => continue,
            }
        }

        return Ok(None);
    }

    #[inline(always)]
    fn is_seq_group_successful(&mut self, seq_group: &data::RuleSequenceGroup) -> std::result::Result<std::option::Option<(Vec<data::SyntaxNode>, Vec<String>)>, SyntaxParseError> {
        let mut result_nodes = Vec::<data::SyntaxNode>::new();
        let mut result_leaves = Vec::<String>::new();

        for each_seq in &seq_group.seqs {
            match self.is_seq_successful(each_seq)? {
                Some((nodes, leaves)) => {
                    let mut mut_result = (nodes, leaves);
                    result_nodes.append(&mut mut_result.0);
                    result_leaves.append(&mut mut_result.1);
                },
                None => return Ok(None),
            }
        }

        return Ok(Some((result_nodes, result_leaves)));
    }

    // ret: 成功すれば expr のベクタ列; 失敗すれば None
    #[inline(always)]
    fn is_seq_successful(&mut self, seq: &data::RuleSequence) -> std::result::Result<std::option::Option<(Vec<data::SyntaxNode>, Vec<String>)>, SyntaxParseError> {
        let mut nodes = Vec::<data::SyntaxNode>::new();
        let mut leaves = Vec::<String>::new();

        for each_expr in &seq.exprs {
            match self.is_expr_successful(each_expr, &mut nodes, &mut leaves)? {
                Some(()) => (),
                None => return Ok(None),
            }
        }

        return Ok(Some((nodes, leaves)));
    }

    #[inline(always)]
    fn is_expr_successful(&mut self, expr: &data::RuleExpression, nodes: &mut Vec<data::SyntaxNode>, leaves: &mut Vec<String>) -> std::result::Result<std::option::Option<()>, SyntaxParseError> {
        return Ok(Some(()));
    }
}
