use crate::block::*;
use crate::data::*;
use crate::rule::*;

use rustnutlib::console::*;

pub type SyntaxParseResult<T> = Result<T, SyntaxParseError>;

pub enum SyntaxParseError {
    Unknown(),
    BlockParseErr(BlockParseError),
    EmptyStringInExpression(),
    InternalErr(String),
    InvalidSyntaxTreeStruct(String),
    NoSucceededRule(String, usize, Vec<(usize, String)>),
    TooDeepRecursion(usize),
    TooLongRepeat(usize),
    UnknownRuleID(String),
}

impl SyntaxParseError {
    pub fn get_log_data(&self) -> ConsoleLogData {
        match self {
            SyntaxParseError::Unknown() => ConsoleLogData::new(ConsoleLogKind::Error, "unknown error", vec![], vec![]),
            SyntaxParseError::EmptyStringInExpression() => ConsoleLogData::new(ConsoleLogKind::Error, "empty string in expression", vec![], vec![]),
            SyntaxParseError::BlockParseErr(err) => err.get_log_data(),
            SyntaxParseError::InternalErr(err_msg) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("internal error: {}", err_msg), vec![], vec![]),
            SyntaxParseError::InvalidSyntaxTreeStruct(cause) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("invalid syntax tree structure ({})", cause), vec![], vec![]),
            SyntaxParseError::NoSucceededRule(rule_id, src_i, rule_stack) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("no succeeded rule '{}' at {} in the source", rule_id, src_i + 1), vec![format!("rule stack: {:?}", rule_stack)], vec![]),
            SyntaxParseError::TooDeepRecursion(max_recur_count) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("too deep recursion over {}", max_recur_count), vec![], vec![]),
            SyntaxParseError::TooLongRepeat(max_loop_count) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("too long repeat over {}", max_loop_count), vec![], vec![]),
            SyntaxParseError::UnknownRuleID(rule_id) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("unknown rule id '{}'", rule_id), vec![], vec![]),
        }
    }
}

pub struct SyntaxParser {
    rule_map: RuleMap,
    src_i: usize,
    src_content: String,
    recursion_count: usize,
    max_recursion_count: usize,
    max_loop_count: usize,
    rule_stack: Vec<(usize, String)>,
}

impl SyntaxParser {
    pub fn new(rule_map: RuleMap) -> SyntaxParseResult<SyntaxParser> {
        return Ok(SyntaxParser {
            rule_map: rule_map,
            src_i: 0,
            src_content: String::new(),
            recursion_count: 1,
            max_recursion_count: 65536,
            max_loop_count: 65536,
            rule_stack: vec![],
        });
    }

    pub fn get_syntax_tree(&mut self, src_content: &String) -> SyntaxParseResult<SyntaxTree> {
        let mut tmp_src_content = src_content.clone();

        // todo: 高速化: replace() と比べてどちらが速いか検証する
        // note: 余分な改行コード 0x0d を排除する
        loop {
            match tmp_src_content.find(0x0d as char) {
                Some(v) => {
                    tmp_src_content.remove(v);
                },
                None => break,
            }
        }

        // フィールドを初期化
        self.src_i = 0;
        self.src_content = tmp_src_content;
        self.recursion_count = 1;

        let start_rule_id = self.rule_map.start_rule_id.clone();

        if self.src_content.len() == 0 {
            return Ok(SyntaxTree::from_node_args(vec![], ASTReflectionStyle::from_config(false, String::new())));
        }

        self.recursion_count += 1;

        let mut root_node = match self.is_rule_successful(&start_rule_id)? {
            Some(v) => v,
            None => return Err(SyntaxParseError::NoSucceededRule(start_rule_id.clone(), self.src_i, self.rule_stack.clone())),
        };

        // ルートは常に Reflectable
        root_node.set_ast_reflection(ASTReflectionStyle::Reflection(start_rule_id.clone()));

        if self.src_i < self.src_content.len() {
            return Err(SyntaxParseError::NoSucceededRule(start_rule_id.clone(), self.src_i, self.rule_stack.clone()));
        }

        self.recursion_count -= 1;
        return Ok(SyntaxTree::from_node(root_node));
    }

    fn is_rule_successful(&mut self, rule_id: &String) -> SyntaxParseResult<Option<SyntaxNodeElement>> {
        let rule = match self.rule_map.get_rule(rule_id) {
            Some(v) => v.clone(),
            None => return Err(SyntaxParseError::UnknownRuleID(rule_id.clone())),
        };

        self.rule_stack.push((self.src_i, rule_id.clone()));

        return match self.is_choice_successful(&rule.group.elem_order, &rule.group)? {
            Some(v) => {
                let mut ast_reflection_style = match &rule.group.sub_elems.get(0) {
                    Some(v) => {
                        match v {
                            RuleElement::Group(sub_choice) => sub_choice.ast_reflection_style.clone(),
                            RuleElement::Expression(_) => rule.group.ast_reflection_style.clone(),
                        }
                    },
                    _ => rule.group.ast_reflection_style.clone(),
                };

                match &ast_reflection_style {
                    ASTReflectionStyle::Reflection(elem_name) => {
                        if *elem_name == String::new() {
                            ast_reflection_style = ASTReflectionStyle::from_config(true, rule_id.clone())
                        }
                    },
                    _ => (),
                };

                self.rule_stack.pop().unwrap();
                let new_node = SyntaxNodeElement::from_node_args(v, ast_reflection_style);
                Ok(Some(new_node))
            },
            None => Ok(None),
        }
    }

    fn is_choice_successful(&mut self, parent_elem_order: &RuleElementOrder, group: &std::boxed::Box<RuleGroup>) -> SyntaxParseResult<Option<Vec<SyntaxNodeElement>>> {
        return self.is_lookahead_choice_successful(parent_elem_order, group);
    }

    fn is_lookahead_choice_successful(&mut self, parent_elem_order: &RuleElementOrder, group: &std::boxed::Box<RuleGroup>) -> SyntaxParseResult<Option<Vec<SyntaxNodeElement>>> {
        return if group.lookahead_kind.is_none() {
            self.is_loop_choice_successful(parent_elem_order, group)
        } else {
            let start_src_i = self.src_i;
            let is_lookahead_positive = group.lookahead_kind == RuleElementLookaheadKind::Positive;

            let is_choice_successful = self.is_loop_choice_successful(parent_elem_order, group)?;
            self.src_i = start_src_i;

            if is_choice_successful.is_some() == is_lookahead_positive {
                Ok(Some(vec![]))
            } else {
                Ok(None)
            }
        }
    }

    fn is_loop_choice_successful(&mut self, parent_elem_order: &RuleElementOrder, group: &std::boxed::Box<RuleGroup>) -> SyntaxParseResult<Option<Vec<SyntaxNodeElement>>> {
        let (min_count, max_count) = match parent_elem_order {
            RuleElementOrder::Random(tmp_occurrence_count) => {
                let (mut tmp_min_count, mut tmp_max_count) = tmp_occurrence_count.to_tuple();

                // todo: 0 だった場合大丈夫かを確認
                tmp_min_count += group.loop_count.min - 1;

                if tmp_max_count != -1 {
                    let max_num = match group.loop_count.max {
                        Infinitable::Normal(v) => v as i32,
                        Infinitable::Infinite => -1,
                    };

                    tmp_max_count += max_num - 1;
                }

                (tmp_min_count, tmp_max_count)
            },
            RuleElementOrder::Sequential => group.loop_count.to_tuple(),
        };

        if max_count != -1 && min_count as i32 > max_count {
            return Err(SyntaxParseError::InternalErr(format!("invalid loop count {{{},{}}}", min_count, max_count)));
        }

        let mut children = Vec::<SyntaxNodeElement>::new();
        let mut loop_count = 0i32;

        while self.src_i < self.src_content.len() {
            if loop_count > self.max_loop_count as i32 {
                return Err(SyntaxParseError::TooLongRepeat(self.max_loop_count as usize));
            }

            match self.is_each_choice_matched(group)? {
                Some(node_elems) => {
                    for each_elem in node_elems {
                        match &each_elem {
                            SyntaxNodeElement::Node(node) => {
                                if node.sub_elems.len() != 0 {
                                    children.push(each_elem);
                                }
                            },
                            _ => children.push(each_elem),
                        }
                    }

                    loop_count += 1;

                    if max_count != -1 && loop_count == max_count {
                        return Ok(Some(children));
                    }
                },
                None => {
                    if loop_count >= min_count as i32 && (max_count == -1 || loop_count <= max_count) {
                        return Ok(Some(children));
                    } else {
                        return Ok(None);
                    }
                },
            }
        }

        if loop_count >= min_count as i32 && (max_count == -1 || loop_count <= max_count) {
            return Ok(Some(children));
        } else {
            return Ok(None);
        }
    }

    fn is_each_choice_matched(&mut self, group: &std::boxed::Box<RuleGroup>) -> SyntaxParseResult<Option<Vec<SyntaxNodeElement>>> {
        let mut children = Vec::<SyntaxNodeElement>::new();

        for each_elem in &group.sub_elems {
            let start_src_i = self.src_i;

            match each_elem {
                RuleElement::Group(each_group) => {
                    match &each_group.elem_order {
                        RuleElementOrder::Random(_) => {
                            let mut new_sub_children = Vec::<SyntaxNodeElement>::new();
                            let mut matched_choices = [false].repeat(each_group.sub_elems.len());

                            for _i in 0..each_group.sub_elems.len() {
                                for (sub_elem_i, each_sub_elem) in each_group.sub_elems.iter().enumerate() {
                                    let is_check_done = *matched_choices.get(sub_elem_i).unwrap();
                                    let elem_start_src_i = self.src_i;

                                    match each_sub_elem {
                                        RuleElement::Group(each_sub_choice) if !is_check_done => {
                                            match self.is_choice_successful(&each_group.elem_order, each_sub_choice)? {
                                                Some(v) => {
                                                    for each_result_sub_elem in v {
                                                        match each_result_sub_elem {
                                                            SyntaxNodeElement::Node(node) if node.sub_elems.len() == 0 => (),
                                                            _ => {
                                                                match each_result_sub_elem {
                                                                    SyntaxNodeElement::Node(result_node) if result_node.ast_reflection_style.is_expandable() => {
                                                                        new_sub_children.append(&mut result_node.sub_elems.clone());
                                                                    },
                                                                    _ => new_sub_children.push(each_result_sub_elem),
                                                                }
                                                            },
                                                        }
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

                            let new_child = SyntaxNodeElement::from_node_args(new_sub_children, each_group.ast_reflection_style.clone());

                            match new_child {
                                SyntaxNodeElement::Node(node) if node.sub_elems.len() == 0 => (),
                                _ => children.push(new_child),
                            }
                        },
                        RuleElementOrder::Sequential => {
                            match each_group.kind {
                                RuleGroupKind::Choice => {
                                    let mut is_successful = false;

                                    for each_sub_elem in &each_group.sub_elems {
                                        match each_sub_elem {
                                            RuleElement::Group(each_sub_group) => {
                                                match self.is_choice_successful(&each_group.elem_order, each_sub_group)? {
                                                    Some(v) => {
                                                        if group.sub_elems.len() != 1 {
                                                            let new_child = SyntaxNodeElement::from_node_args(v, each_sub_group.ast_reflection_style.clone());

                                                            match new_child {
                                                                SyntaxNodeElement::Node(node) if node.sub_elems.len() == 0 => (),
                                                                _ => {
                                                                    match new_child {
                                                                        SyntaxNodeElement::Node(new_node) if new_node.ast_reflection_style.is_expandable() => {
                                                                            children.append(&mut new_node.sub_elems.clone());
                                                                        },
                                                                        _ => children.push(new_child),
                                                                    }
                                                                },
                                                            }
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
                                },
                                RuleGroupKind::Sequence => {
                                    match self.is_choice_successful(&each_group.elem_order, each_group)? {
                                        Some(v) => {
                                            if group.sub_elems.len() != 1 {
                                                let new_child = SyntaxNodeElement::from_node_args(v, each_group.ast_reflection_style.clone());

                                                match new_child {
                                                    SyntaxNodeElement::Node(node) if node.sub_elems.len() == 0 => (),
                                                    _ => {
                                                        match new_child {
                                                            SyntaxNodeElement::Node(new_node) if new_node.ast_reflection_style.is_expandable() => {
                                                                children.append(&mut new_node.sub_elems.clone());
                                                            },
                                                            _ => children.push(new_child),
                                                        }
                                                    },
                                                }
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
                            }
                        },
                    }
                },
                RuleElement::Expression(each_expr) => {
                    match self.is_expr_successful(each_expr)? {
                        Some(node_elems) => {
                            for each_elem in node_elems {
                                match each_elem {
                                    SyntaxNodeElement::Node(node) if node.sub_elems.len() == 0 => (),
                                    _ => children.push(each_elem),
                                }
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

    fn is_expr_successful(&mut self, expr: &Box<RuleExpression>) -> SyntaxParseResult<Option<Vec<SyntaxNodeElement>>> {
        return self.is_lookahead_expr_successful(expr);
    }

    fn is_lookahead_expr_successful(&mut self, expr: &Box<RuleExpression>) -> SyntaxParseResult<Option<Vec<SyntaxNodeElement>>> {
        return if expr.lookahead_kind.is_none() {
            self.is_loop_expr_successful(expr)
        } else {
            let start_src_i = self.src_i;
            let is_lookahead_positive = expr.lookahead_kind == RuleElementLookaheadKind::Positive;

            let is_expr_successful = self.is_loop_expr_successful(expr)?;
            self.src_i = start_src_i;

            if is_expr_successful.is_some() == is_lookahead_positive {
                Ok(Some(vec![]))
            } else {
                Ok(None)
            }
        }
    }

    fn is_loop_expr_successful(&mut self, expr: &Box<RuleExpression>) -> SyntaxParseResult<Option<Vec<SyntaxNodeElement>>> {
        let (min_count, max_count) = expr.loop_count.to_tuple();

        if max_count != -1 && min_count as i32 > max_count {
            return Err(SyntaxParseError::InternalErr(format!("invalid loop count {{{},{}}}", min_count, max_count)));
        }

        let mut children = Vec::<SyntaxNodeElement>::new();
        let mut loop_count = 0usize;

        while self.src_i < self.src_content.len() {
            if loop_count > self.max_loop_count {
                return Err(SyntaxParseError::TooLongRepeat(self.max_loop_count as usize));
            }

            match self.is_each_expr_matched(expr)? {
                Some(node) => {
                    for each_node in node {
                        match each_node {
                            SyntaxNodeElement::Node(node) if node.sub_elems.len() == 0 => (),
                            _ => children.push(each_node),
                        }
                    }

                    loop_count += 1;

                    if max_count != -1 && loop_count as i32 == max_count {
                        return Ok(Some(children));
                    }
                },
                None => {
                    return if loop_count >= min_count && (max_count == -1 || loop_count as i32 <= max_count) {
                        Ok(Some(children))
                    } else {
                        Ok(None)
                    }
                },
            }
        }

        return if loop_count >= min_count && (max_count == -1 || loop_count as i32 <= max_count) {
            Ok(Some(children))
        } else {
            Ok(None)
        }
    }

    fn is_each_expr_matched(&mut self, expr: &Box<RuleExpression>) -> SyntaxParseResult<Option<Vec<SyntaxNodeElement>>> {
        if self.src_i >= self.src_content.len() {
            return Ok(None);
        }

        match expr.kind {
            RuleExpressionKind::CharClass => {
                if self.src_content.len() < self.src_i + 1 {
                    return Ok(None);
                }

                let pattern = match self.rule_map.regex_map.get(&expr.value) {
                    Some(v) => v,
                    None => return Err(SyntaxParseError::InternalErr("regex pattern of character class not found".to_string())),
                };

                let tar_char = self.src_content[self.src_i..self.src_i + 1].to_string();

                if pattern.is_match(&tar_char) {
                    let new_leaf = SyntaxNodeElement::from_leaf_args(tar_char, expr.ast_reflection_style.clone());
                    self.src_i += 1;
                    return Ok(Some(vec![new_leaf]));
                } else {
                    return Ok(None);
                }
            },
            RuleExpressionKind::ID => {
                self.recursion_count += 1;

                if self.max_recursion_count < self.recursion_count {
                    return Err(SyntaxParseError::TooDeepRecursion(self.max_recursion_count));
                }

                match self.is_rule_successful(&expr.value.clone())? {
                    Some(node_elem) => {
                        self.recursion_count += 1;

                        let conv_node_elems = match &node_elem {
                            SyntaxNodeElement::Node(node) => {
                                let sub_ast_reflection = match &expr.ast_reflection_style {
                                    ASTReflectionStyle::Reflection(elem_name) => {
                                        let conv_elem_name = if elem_name == "" {
                                            expr.value.clone()
                                        } else {
                                            elem_name.clone()
                                        };

                                        ASTReflectionStyle::Reflection(conv_elem_name)
                                    },
                                    _ => expr.ast_reflection_style.clone(),
                                };

                                let node = SyntaxNodeElement::from_node_args(node.sub_elems.clone(), sub_ast_reflection);

                                if expr.ast_reflection_style.is_expandable() {
                                    match node {
                                        SyntaxNodeElement::Node(node) => node.sub_elems,
                                        _ => vec![node],
                                    }
                                } else {
                                    vec![node]
                                }
                            },
                            SyntaxNodeElement::Leaf(_) => vec![node_elem],
                        };

                        return Ok(Some(conv_node_elems));
                    },
                    None => {
                        self.recursion_count -= 1;
                        return Ok(None);
                    },
                };
            },
            RuleExpressionKind::String => {
                if expr.value.len() == 0 {
                    return Err(SyntaxParseError::EmptyStringInExpression());
                }

                if self.src_content.len() < self.src_i + expr.value.len() {
                    return Ok(None);
                }

                if self.src_content[self.src_i..self.src_i + expr.value.len()] == expr.value {
                    let new_leaf = SyntaxNodeElement::from_leaf_args(expr.value.clone(), expr.ast_reflection_style.clone());
                    self.src_i += expr.value.len();
                    return Ok(Some(vec![new_leaf]));
                } else {
                    return Ok(None);
                }
            },
            RuleExpressionKind::Wildcard => {
                if self.src_content.len() < self.src_i + 1 {
                    return Ok(None);
                }

                let expr_value = self.src_content[self.src_i..self.src_i + 1].to_string();
                let new_leaf = SyntaxNodeElement::from_leaf_args(expr_value, expr.ast_reflection_style.clone());
                self.src_i += 1;
                return Ok(Some(vec![new_leaf]));
            },
        }
    }
}
