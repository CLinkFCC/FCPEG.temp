use crate::data;
use crate::rule;
use rustnutlib::console;

pub enum SyntaxParseError {
    Unknown(),
    InternalErr(String),
    NoSucceededRule(String, usize),
    TooDeepRecursion(usize),
    TooLongRepeat(usize),
    UnknownRuleID(String),
}

impl SyntaxParseError {
    pub fn get_log_data(&self) -> console::ConsoleLogData {
        match self {
            SyntaxParseError::Unknown() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown error", vec![], vec![]),
            SyntaxParseError::InternalErr(err_msg) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("internal error: {}", err_msg), vec![], vec![]),
            SyntaxParseError::NoSucceededRule(rule_id, src_i) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("no succeeded rule '{}' at {} in the source", rule_id, src_i + 1), vec![], vec![]),
            SyntaxParseError::TooDeepRecursion(max_recur_count) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("too deep recursion over {}", max_recur_count), vec![], vec![]),
            SyntaxParseError::TooLongRepeat(max_loop_count) => console::ConsoleLogData::new(console::ConsoleLogKind::Error, &format!("too long repeat over {}", max_loop_count), vec![], vec![]),
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
    max_loop_count: usize,
}

impl SyntaxParser {
    pub fn new(rule_map: rule::RuleMap) -> std::result::Result<Self, SyntaxParseError> {
        return Ok(SyntaxParser {
            rule_map: rule_map,
            src_i: 0,
            src_content: "".to_string(),
            recursion_count: 1,
            max_recursion_count: 32,
            max_loop_count: 65536,
        });
    }

    pub fn get_syntax_tree(&mut self, src_content: String) -> std::result::Result<data::SyntaxTree, SyntaxParseError> {
        // フィールドを初期化
        self.src_i = 0;
        self.src_content = src_content;
        self.recursion_count = 1;

        let start_rule_id = self.rule_map.start_rule_id.to_string();
        let mut tree = data::SyntaxTree::new(start_rule_id.to_string());

        if self.src_content.len() == 0 {
            return Ok(tree);
        }

        self.recursion_count += 1;

        let child = match self.is_rule_successful(&start_rule_id)? {
            Some(v) => v,
            None => return Err(SyntaxParseError::NoSucceededRule(start_rule_id.to_string(), self.src_i)),
        };

        tree.children.push(child);

        if self.src_i < self.src_content.len() {
            return Err(SyntaxParseError::NoSucceededRule(start_rule_id.to_string(), self.src_i));
        }

        self.recursion_count -= 1;
        return Ok(tree);
    }

    fn is_rule_successful(&mut self, rule_id: &String) -> std::result::Result<std::option::Option<data::SyntaxNodeElement>, SyntaxParseError> {
        let rule = match self.rule_map.get_rule(rule_id) {
            Some(v) => v.clone(),
            None => return Err(SyntaxParseError::UnknownRuleID(rule_id.to_string())),
        };

        for each_choice in &rule.choices {
            let start_src_i = self.src_i;
            let occurrence_count = if each_choice.is_random_order {
                Some(each_choice.occurrence_count)
            } else {
                None
            };

            match self.is_choice_successful(&occurrence_count, each_choice)? {
                Some(v) => {
                    let new_node = data::SyntaxNodeElement::NodeList(rule_id.to_string(), v);
                    return Ok(Some(new_node));
                },
                None => {
                    self.src_i = start_src_i;
                    continue;
                },
            }
        }

        return Ok(None);
    }

    fn is_choice_successful(&mut self, parent_occurrence_count: &std::option::Option<(i32, i32)>, choice: &std::boxed::Box<rule::RuleChoice>) -> std::result::Result<std::option::Option<Vec<data::SyntaxNodeElement>>, SyntaxParseError> {
        return self.is_lookahead_choice_successful(parent_occurrence_count, choice);
    }

    fn is_lookahead_choice_successful(&mut self, parent_occurrence_count: &std::option::Option<(i32, i32)>, choice: &std::boxed::Box<rule::RuleChoice>) -> std::result::Result<std::option::Option<Vec<data::SyntaxNodeElement>>, SyntaxParseError> {
        match choice.lookahead_kind {
            rule::RuleLookaheadKind::None => return self.is_loop_choice_successful(parent_occurrence_count, choice),
            rule::RuleLookaheadKind::Positive | rule::RuleLookaheadKind::Negative => {
                let start_src_i = self.src_i;
                let is_lookahead_positive = choice.lookahead_kind == rule::RuleLookaheadKind::Positive;

                let is_choice_successful = self.is_loop_choice_successful(parent_occurrence_count, choice)?;
                self.src_i = start_src_i;

                if is_choice_successful.is_some() == is_lookahead_positive {
                    return Ok(Some(vec![]));
                } else {
                    return Ok(None);
                }
            },
        }
    }

    fn is_loop_choice_successful(&mut self, parent_occurrence_count: &std::option::Option<(i32, i32)>, choice: &std::boxed::Box<rule::RuleChoice>) -> std::result::Result<std::option::Option<Vec<data::SyntaxNodeElement>>, SyntaxParseError> {
        let (min_count, max_count) = match parent_occurrence_count {
            Some(tmp_occr_count) => {
                let (mut tmp_min_count, mut tmp_max_count) = *tmp_occr_count;

                // todo: 0 だった場合大丈夫かを確認
                tmp_min_count += choice.loop_count.0 - 1;

                if tmp_max_count != -1 {
                    tmp_max_count += choice.loop_count.1 - 1;
                }

                (tmp_min_count, tmp_max_count)
            },
            None => choice.loop_count,
        };

        if min_count == -1 || (max_count != -1 && min_count > max_count) {
            return Err(SyntaxParseError::InternalErr(format!("invalid loop count {{{},{}}}", min_count, max_count)));
        }

        let mut children = Vec::<data::SyntaxNodeElement>::new();
        let mut loop_count = 0i32;

        while self.src_i < self.src_content.len() {
            if loop_count > self.max_loop_count as i32 {
                return Err(SyntaxParseError::TooLongRepeat(self.max_loop_count as usize));
            }

            match self.is_each_choice_matched(choice)? {
                Some(node_elems) => {
                    for each_elem in node_elems {
                        children.push(each_elem);
                    }

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

        if loop_count >= min_count && (max_count == -1 || loop_count <= max_count) {
            return Ok(Some(children));
        } else {
            return Ok(None);
        }
    }

    fn is_each_choice_matched(&mut self, choice: &std::boxed::Box<rule::RuleChoice>) -> std::result::Result<std::option::Option<Vec<data::SyntaxNodeElement>>, SyntaxParseError> {
        let mut children = Vec::<data::SyntaxNodeElement>::new();

        for each_elem in &choice.elem_containers {
            let start_src_i = self.src_i;

            match each_elem {
                rule::RuleElementContainer::RuleChoice(each_choice) => {
                    if each_choice.is_random_order {
                        let mut new_sub_children = Vec::<data::SyntaxNodeElement>::new();
                        let mut matched_choices = [false].repeat(each_choice.elem_containers.len());

                        for _i in 0..each_choice.elem_containers.len() {
                            for (sub_elem_i, each_sub_elem) in each_choice.elem_containers.iter().enumerate() {
                                let is_check_done = *matched_choices.get(sub_elem_i).unwrap();
                                let elem_start_src_i = self.src_i;

                                match each_sub_elem {
                                    rule::RuleElementContainer::RuleChoice(each_sub_choice) if !is_check_done => {
                                        match self.is_choice_successful(&Some(each_choice.occurrence_count), each_sub_choice)? {
                                            Some(v) => {
                                                for each_result_sub_elem in v {
                                                    new_sub_children.push(each_result_sub_elem);
                                                }

                                                matched_choices[sub_elem_i] = true;
                                                break;
                                            },
                                            None => {
                                                self.src_i = elem_start_src_i;
                                                continue;
                                            },
                                        }
                                    },
                                    _ => (),
                                }
                            }
                        }

                        if matched_choices.contains(&false) {
                            return Ok(None);
                        }

                        let new_child = data::SyntaxNodeElement::NodeList("".to_string(), new_sub_children);
                        children.push(new_child);
                    } else if each_choice.has_choices {
                        let mut is_successful = false;

                        for each_sub_elem in &each_choice.elem_containers {
                            match each_sub_elem {
                                rule::RuleElementContainer::RuleChoice(each_sub_choice) => {
                                    match self.is_choice_successful(&Some(each_choice.occurrence_count), each_sub_choice)? {
                                        Some(v) => {
                                            if choice.elem_containers.len() != 1 {
                                                let new_child = data::SyntaxNodeElement::NodeList("".to_string(), v);
                                                children.push(new_child);
                                            } else {
                                                children = v;
                                            }

                                            is_successful = true;
                                            break;
                                        },
                                        None => {
                                            self.src_i = start_src_i;
                                        },
                                    }
                                },
                                _ => (),
                            }
                        }

                        if !is_successful {
                            return Ok(None);
                        }
                    } else {
                        match self.is_choice_successful(&Some(each_choice.occurrence_count), each_choice)? {
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

    #[inline(always)]
    fn is_expr_successful(&mut self, expr: &Box<rule::RuleExpression>) -> std::result::Result<std::option::Option<Vec<data::SyntaxNodeElement>>, SyntaxParseError> {
        return self.is_lookahead_expr_successful(expr);
    }

    #[inline(always)]
    fn is_lookahead_expr_successful(&mut self, expr: &Box<rule::RuleExpression>) -> std::result::Result<std::option::Option<Vec<data::SyntaxNodeElement>>, SyntaxParseError> {
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
    fn is_loop_expr_successful(&mut self, expr: &Box<rule::RuleExpression>) -> std::result::Result<std::option::Option<Vec<data::SyntaxNodeElement>>, SyntaxParseError> {
        let (min_count, max_count) = expr.loop_count;

        if min_count == -1 || (max_count != -1 && min_count > max_count) {
            return Err(SyntaxParseError::InternalErr(format!("invalid loop count {{{},{}}}", min_count, max_count)));
        }

        let mut children = Vec::<data::SyntaxNodeElement>::new();
        let mut loop_count = 0;

        while self.src_i < self.src_content.len() {
            if loop_count > self.max_loop_count as i32 {
                return Err(SyntaxParseError::TooLongRepeat(self.max_loop_count as usize));
            }

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

        if loop_count >= min_count && (max_count == -1 || loop_count <= max_count) {
            return Ok(Some(children));
        } else {
            return Ok(None);
        }
    }

    #[inline(always)]
    fn is_each_expr_matched(&mut self, expr: &Box<rule::RuleExpression>) -> std::result::Result<std::option::Option<data::SyntaxNodeElement>, SyntaxParseError> {
        if self.src_i >= self.src_content.len() {
            return Ok(None);
        }

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
