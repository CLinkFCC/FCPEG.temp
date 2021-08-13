use crate::data;
use crate::rule;
use rustnutlib::console;

pub enum SyntaxParseError {
    Unknown(),
    InternalErr(String),
    NoSucceededRule(String, usize),
    TooDeepRecursion(usize),
    UnknownRuleID(String),
}

impl SyntaxParseError {
    pub fn get_log_data(&self) -> console::ConsoleLogData {
        match self {
            SyntaxParseError::Unknown() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown error", vec![], vec![]),
            SyntaxParseError::InternalErr(err_msg) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("internal error: {}", err_msg), vec![], vec![]),
            SyntaxParseError::NoSucceededRule(rule_id, src_i) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("no succeeded rule '{}' at {} in the source", rule_id, src_i + 1), vec![], vec![]),
            SyntaxParseError::TooDeepRecursion(max_recur_count) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "too deep recursion", vec![format!("max recursion count: {}", max_recur_count)], vec![]),
            SyntaxParseError::UnknownRuleID(rule_id) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("unknown rule id '{}'", rule_id), vec![], vec![]),
        }
    }
}

pub struct SyntaxParser {
    rule_map: rule::RuleMap,
    src_i: usize,
    src_content: String,
    recursion_count: usize,
    max_recursion_count: usize,
}

impl SyntaxParser {
    pub fn new(rule_map: rule::RuleMap) -> std::result::Result<Self, SyntaxParseError> {
        return Ok(SyntaxParser {
            rule_map: rule_map,
            src_i: 0,
            src_content: "".to_string(),
            recursion_count: 1,
            max_recursion_count: 128,
        });
    }

    pub fn get_syntax_tree(&mut self, src_content: String) -> std::result::Result<data::SyntaxTree, SyntaxParseError> {
        // フィールドを初期化
        self.src_i = 0;
        self.src_content = src_content;
        // self.recursion_count = 1;

        let start_rule_id = self.rule_map.start_rule_id.to_string();
        let mut tree = data::SyntaxTree::new(start_rule_id.to_string());

        if self.src_content.len() == 0 {
            return Ok(tree);
        }

        // self.recursion_count += 1;

        let child = match self.is_rule_successful(&start_rule_id)? {
            Some(v) => v,
            None => return Err(SyntaxParseError::NoSucceededRule(start_rule_id.to_string(), self.src_i)),
        };

        tree.children.push(child);

        if self.src_i < self.src_content.len() {
            return Err(SyntaxParseError::NoSucceededRule(start_rule_id.to_string(), self.src_i));
        }

        // self.recursion_count -= 1;
        return Ok(tree);
    }

    #[inline(always)]
    fn is_rule_successful(&mut self, rule_id: &String) -> std::result::Result<std::option::Option<data::SyntaxNodeElement>, SyntaxParseError> {
        let rule = match self.rule_map.get_rule(rule_id) {
            Some(v) => v.clone(),
            None => return Err(SyntaxParseError::UnknownRuleID(rule_id.to_string())),
        };

        for each_choice in &rule.choices {
            let start_src_i = self.src_i;

            match self.is_choice_successful(each_choice)? {
                Some(v) => {
                    let new_node = data::SyntaxNodeElement::NodeList(rule_id.to_string(), v);
                    return Ok(Some(new_node));
                }
                None => {
                    self.src_i = start_src_i;
                    continue;
                },
            }
        }

        return Ok(None);
    }

    #[inline(always)]
    fn is_choice_successful(&mut self, choice: &rule::RuleChoice) -> std::result::Result<std::option::Option<Vec<data::SyntaxNodeElement>>, SyntaxParseError> {
        let mut children = Vec::<data::SyntaxNodeElement>::new();

        for each_elem in &choice.elem_containers {
            let start_src_i = self.src_i;

            match each_elem {
                rule::RuleElementContainer::RuleChoice(each_choice) => {
                    match self.is_choice_successful(each_choice)? {
                        Some(v) => {
                            if choice.elem_containers.len() != 1 {
                                let new_child = data::SyntaxNodeElement::NodeList("".to_string(), v);
                                children.push(new_child);
                            } else {
                                children = v;
                            }
 
                            continue;
                        },
                        None => {
                            self.src_i = start_src_i;
                            return Ok(None);
                        },
                    }
                },
                rule::RuleElementContainer::RuleExpression(each_expr) => {
                    match self.is_expr_successful(each_expr)? {
                        Some(node_elems) => {
                            for each_elem in node_elems {
                                children.push(each_elem);
                            }

                            continue;
                        },
                        None => {
                            self.src_i = start_src_i;
                            return Ok(None);
                        },
                    }
                }
            }
        }

        return Ok(Some(children));
    }

    /*
    #[inline(always)]
    fn is_seq_group_successful(&mut self, seq_group: &data::RuleSequenceGroup) -> std::result::Result<std::option::Option<(Vec<data::SyntaxNode>, Vec<String>)>, SyntaxParseError> {
        let mut result_nodes = Vec::<data::SyntaxNode>::new();
        let mut result_leaves = Vec::<String>::new();

        for each_seq in &seq_group.seqs {
            let start_src_i = self.src_i;

            match self.is_seq_successful(each_seq)? {
                Some((nodes, leaves)) => {
                    let mut mut_result = (nodes, leaves);
                    result_nodes.append(&mut mut_result.0);
                    result_leaves.append(&mut mut_result.1);
                },
                None => {
                    self.src_i = start_src_i;
                    return Ok(None);
                },
            }
        }

        return Ok(Some((result_nodes, result_leaves)));
    }
    */

    /*
    #[inline(always)]
    fn is_seq_successful(&mut self, seq: &data::RuleSequence) -> std::result::Result<std::option::Option<(Vec<data::SyntaxNode>, Vec<String>)>, SyntaxParseError> {
        let mut nodes = Vec::<data::SyntaxNode>::new();
        let mut leaves = Vec::<String>::new();

        for each_expr in &seq.exprs {
            match each_expr.loop_type {
                data::RuleExpressionLoopKind::One => {
                    match self.is_expr_vec_successful(each_expr, &mut nodes, &mut leaves)? {
                        Some(()) => continue,
                        None => return Ok(None),
                    }
                },
                data::RuleExpressionLoopKind::OneOrMore => {
                    match self.is_expr_vec_successful(each_expr, &mut nodes, &mut leaves)? {
                        Some(()) => (),
                        None => return Ok(None),
                    }

                    while self.is_expr_vec_successful(each_expr, &mut nodes, &mut leaves)?.is_some() {}

                    continue;
                },
                data::RuleExpressionLoopKind::ZeroOrMore => {
                    while self.is_expr_vec_successful(each_expr, &mut nodes, &mut leaves)?.is_some() {}

                    continue;
                },
                data::RuleExpressionLoopKind::ZeroOrOne => {
                    self.is_expr_vec_successful(each_expr, &mut nodes, &mut leaves)?;
                    continue;
                },
            }
        }

        return Ok(Some((nodes, leaves)));
    }
    */

    /*
    #[inline(always)]
    fn is_expr_vec_successful(&mut self, expr: &data::RuleExpression, nodes: &mut Vec<data::SyntaxNode>, leaves: &mut Vec<String>) -> std::result::Result<std::option::Option<()>, SyntaxParseError> {
        let start_src_i = self.src_i;

        return match self.is_expr_successful(expr, nodes, leaves)? {
            Some(()) => Ok(Some(())),
            None => {
                self.src_i = start_src_i;
                Ok(None)
            },
        };
    }
    */

    #[inline(always)]
    fn is_expr_successful(&mut self, expr: &rule::RuleExpression) -> std::result::Result<std::option::Option<Vec<data::SyntaxNodeElement>>, SyntaxParseError> {
        return self.is_lookahead_expr_successful(expr);
    }

    #[inline(always)]
    fn is_lookahead_expr_successful(&mut self, expr: &rule::RuleExpression) -> std::result::Result<std::option::Option<Vec<data::SyntaxNodeElement>>, SyntaxParseError> {
        match expr.lookahead_kind {
            rule::RuleLookaheadKind::None => {
                return self.is_loop_expr_successful(expr);
            },
            rule::RuleLookaheadKind::Positive | rule::RuleLookaheadKind::Negative => {
                let start_src_i = self.src_i;
                let is_lookahead_positive = expr.lookahead_kind == rule::RuleLookaheadKind::Positive;

                let is_expr_successful = self.is_loop_expr_successful(expr)?;
                self.src_i = start_src_i;

                if is_expr_successful.is_some() == is_lookahead_positive {
                    return Ok(Some(vec![]));
                } else {
                    return Ok(None);
                }
            },
        }
    }

    #[inline(always)]
    fn is_loop_expr_successful(&mut self, expr: &rule::RuleExpression) -> std::result::Result<std::option::Option<Vec<data::SyntaxNodeElement>>, SyntaxParseError> {
        let min_count = expr.loop_count.0;
        let max_count = expr.loop_count.1;

        if min_count == -1 || (max_count != -1 && min_count > max_count) {
            return Err(SyntaxParseError::InternalErr(format!("invalid loop count {{{},{}}}", min_count, max_count)));
        }

        let mut children = Vec::<data::SyntaxNodeElement>::new();
        let mut loop_count = 0;

        loop {
            match self.is_each_expr_matched(expr)? {
                Some(node_elem) => {
                    children.push(node_elem);
                    loop_count += 1;

                    if max_count != -1 && loop_count == max_count {
                        return Ok(Some(children));
                    }
                },
                None => {
                    if loop_count >= min_count && (max_count == -1 || loop_count <= max_count) {
                        return Ok(Some(children));
                    } else {
                        return Ok(None);
                    }
                },
            }
        }
    }

    #[inline(always)]
    fn is_each_expr_matched(&mut self, expr: &rule::RuleExpression) -> std::result::Result<std::option::Option<data::SyntaxNodeElement>, SyntaxParseError> {
        match expr.kind {
            rule::RuleExpressionKind::CharClass => {
                if self.src_content.len() < self.src_i + 1 {
                    return Ok(None);
                }

                let pattern = match self.rule_map.regex_map.get(&expr.value) {
                    Some(v) => v,
                    None => return Err(SyntaxParseError::InternalErr("regex pattern of character class not found".to_string())),
                };

                let tar_char = self.src_content[self.src_i..self.src_i + 1].to_string();

                if pattern.is_match(&tar_char) {
                    let new_leaf = data::SyntaxNodeElement::Leaf(tar_char);
                    self.src_i += 1;
                    return Ok(Some(new_leaf));
                } else {
                    return Ok(None);
                }
            },
            rule::RuleExpressionKind::ID => {
                self.recursion_count += 1;

                if self.max_recursion_count < self.recursion_count {
                    return Err(SyntaxParseError::TooDeepRecursion(self.max_recursion_count));
                }

                match self.is_rule_successful(&expr.value.to_string())? {
                    Some(node_elem) => {
                        self.recursion_count -= 1;
                        return Ok(Some(node_elem));
                    },
                    None => {
                        self.recursion_count -= 1;
                        return Ok(None);
                    },
                };
            },
            rule::RuleExpressionKind::String => {
                if self.src_content.len() < self.src_i + expr.value.len() {
                    return Ok(None);
                }

                if self.src_content[self.src_i..self.src_i + expr.value.len()] == expr.value {
                    let new_leaf = data::SyntaxNodeElement::Leaf(expr.value.to_string());
                    self.src_i += expr.value.len();
                    return Ok(Some(new_leaf));
                } else {
                    return Ok(None);
                }
            },
            rule::RuleExpressionKind::Wildcard => {
                if self.src_content.len() < self.src_i + 1 {
                    return Ok(None);
                }

                let expr_value = self.src_content[self.src_i..self.src_i + 1].to_string();
                let new_leaf = data::SyntaxNodeElement::Leaf(expr_value);
                self.src_i += 1;
                return Ok(Some(new_leaf));
            },
        }
    }
}
