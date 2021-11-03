use crate::block::*;
use crate::data::*;
use crate::rule::*;

use rustnutlib::console::*;

pub enum SyntaxParseError {
    Unknown(),
    BlockParseErr(BlockParseError),
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
    pub fn new(rule_map: RuleMap) -> std::result::Result<SyntaxParser, SyntaxParseError> {
        return Ok(SyntaxParser {
            rule_map: rule_map,
            src_i: 0,
            src_content: String::new(),
            recursion_count: 1,
            max_recursion_count: 128,
            max_loop_count: 65536,
            rule_stack: vec![],
        });
    }

    pub fn get_syntax_tree(&mut self, mut src_content: String) -> std::result::Result<SyntaxTree, SyntaxParseError> {
        // todo: 高速化: replace() と比べてどちらが速いか検証する
        // note: 余分な改行コード 0x0d を排除する
        loop {
            match src_content.find(0x0d as char) {
                Some(v) => {
                    src_content.remove(v);
                },
                None => break,
            }
        }

        // フィールドを初期化
        self.src_i = 0;
        self.src_content = src_content;
        self.recursion_count = 1;

        let start_rule_id = self.rule_map.start_rule_id.clone();

        if self.src_content.len() == 0 {
            return Ok(SyntaxTree::from_node_list_args(vec![], ASTReflection::from_config(false, String::new())));
        }

        self.recursion_count += 1;

        let mut root_node = match self.is_rule_successful(&start_rule_id)? {
            Some(v) => v,
            None => return Err(SyntaxParseError::NoSucceededRule(start_rule_id.clone(), self.src_i, self.rule_stack.clone())),
        };

        // ルートは常に Reflectable
        root_node.set_ast_reflection(ASTReflection::Reflectable(start_rule_id.clone()));

        if self.src_i < self.src_content.len() {
            return Err(SyntaxParseError::NoSucceededRule(start_rule_id.clone(), self.src_i, self.rule_stack.clone()));
        }

        self.recursion_count -= 1;
        return Ok(SyntaxTree::from_node_list(root_node));
    }

    fn is_rule_successful(&mut self, rule_id: &String) -> std::result::Result<std::option::Option<SyntaxNodeElement>, SyntaxParseError> {
        println!("rule {}", rule_id);

        let rule = match self.rule_map.get_rule(rule_id) {
            Some(v) => v.clone(),
            None => return Err(SyntaxParseError::UnknownRuleID(rule_id.clone())),
        };

        for each_choice in &rule.choices {
            self.rule_stack.push((self.src_i, rule_id.clone()));

            let start_src_i = self.src_i;
            let occurrence_count = if each_choice.is_random_order {
                Some(each_choice.occurrence_count)
            } else {
                None
            };

            match self.is_choice_successful(&occurrence_count, each_choice)? {
                Some(v) => {
                    let mut ast_reflection = match &each_choice.elem_containers.get(0) {
                        Some(v) => {
                            match v {
                                RuleElementContainer::RuleChoice(sub_choice) => sub_choice.ast_reflection.clone(),
                                RuleElementContainer::RuleExpression(_) => each_choice.ast_reflection.clone(),
                            }
                        },
                        _ => each_choice.ast_reflection.clone(),
                    };

                    match &ast_reflection {
                        ASTReflection::Reflectable(elem_name) => {
                            if *elem_name == String::new() {
                                ast_reflection = ASTReflection::from_config(true, rule_id.clone())
                            }
                        },
                        _ => (),
                    };

                    self.rule_stack.pop().unwrap();
                    let new_node = SyntaxNodeElement::from_node_list_args(v, ast_reflection);
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

    fn is_choice_successful(&mut self, parent_occurrence_count: &std::option::Option<(i32, i32)>, choice: &std::boxed::Box<RuleChoice>) -> std::result::Result<std::option::Option<Vec<SyntaxNodeElement>>, SyntaxParseError> {
        return self.is_lookahead_choice_successful(parent_occurrence_count, choice);
    }

    fn is_lookahead_choice_successful(&mut self, parent_occurrence_count: &std::option::Option<(i32, i32)>, choice: &std::boxed::Box<RuleChoice>) -> std::result::Result<std::option::Option<Vec<SyntaxNodeElement>>, SyntaxParseError> {
        match choice.lookahead_kind {
            RuleLookaheadKind::None => return self.is_loop_choice_successful(parent_occurrence_count, choice),
            RuleLookaheadKind::Positive | RuleLookaheadKind::Negative => {
                let start_src_i = self.src_i;
                let is_lookahead_positive = choice.lookahead_kind == RuleLookaheadKind::Positive;

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

    fn is_loop_choice_successful(&mut self, parent_occurrence_count: &std::option::Option<(i32, i32)>, choice: &std::boxed::Box<RuleChoice>) -> std::result::Result<std::option::Option<Vec<SyntaxNodeElement>>, SyntaxParseError> {
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

        let mut children = Vec::<SyntaxNodeElement>::new();
        let mut loop_count = 0i32;

        while self.src_i < self.src_content.len() {
            if loop_count > self.max_loop_count as i32 {
                return Err(SyntaxParseError::TooLongRepeat(self.max_loop_count as usize));
            }

            match self.is_each_choice_matched(choice)? {
                Some(node_elems) => {
                    for each_elem in node_elems {
                        match &each_elem {
                            SyntaxNodeElement::NodeList(node_list) => {
                                if node_list.elems.len() != 0 {
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

    fn is_each_choice_matched(&mut self, choice: &std::boxed::Box<RuleChoice>) -> std::result::Result<std::option::Option<Vec<SyntaxNodeElement>>, SyntaxParseError> {
        let mut children = Vec::<SyntaxNodeElement>::new();

        for each_elem in &choice.elem_containers {
            let start_src_i = self.src_i;

            match each_elem {
                RuleElementContainer::RuleChoice(each_choice) => {
                    if each_choice.is_random_order {
                        let mut new_sub_children = Vec::<SyntaxNodeElement>::new();
                        let mut matched_choices = [false].repeat(each_choice.elem_containers.len());

                        for _i in 0..each_choice.elem_containers.len() {
                            for (sub_elem_i, each_sub_elem) in each_choice.elem_containers.iter().enumerate() {
                                let is_check_done = *matched_choices.get(sub_elem_i).unwrap();
                                let elem_start_src_i = self.src_i;

                                match each_sub_elem {
                                    RuleElementContainer::RuleChoice(each_sub_choice) if !is_check_done => {
                                        match self.is_choice_successful(&Some(each_choice.occurrence_count), each_sub_choice)? {
                                            Some(v) => {
                                                for each_result_sub_elem in v {
                                                    match each_result_sub_elem {
                                                        SyntaxNodeElement::NodeList(node_list) if node_list.elems.len() == 0 => (),
                                                        _ => {
                                                            match each_result_sub_elem {
                                                                SyntaxNodeElement::NodeList(result_node_list) if result_node_list.ast_reflection.is_expandable() => {
                                                                    println!("expandable");
                                                                    new_sub_children.append(&mut result_node_list.elems.clone());
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

                        let new_child = SyntaxNodeElement::from_node_list_args(new_sub_children, each_choice.ast_reflection.clone());

                        match new_child {
                            SyntaxNodeElement::NodeList(node_list) if node_list.elems.len() == 0 => (),
                            _ => children.push(new_child),
                        }
                    } else if each_choice.has_choices {
                        let mut is_successful = false;

                        for each_sub_elem in &each_choice.elem_containers {
                            match each_sub_elem {
                                RuleElementContainer::RuleChoice(each_sub_choice) => {
                                    match self.is_choice_successful(&Some(each_choice.occurrence_count), each_sub_choice)? {
                                        Some(v) => {
                                            if choice.elem_containers.len() != 1 {
                                                let new_child = SyntaxNodeElement::from_node_list_args(v, each_sub_choice.ast_reflection.clone());

                                                match new_child {
                                                    SyntaxNodeElement::NodeList(node_list) if node_list.elems.len() == 0 => (),
                                                    _ => {
                                                        match new_child {
                                                            SyntaxNodeElement::NodeList(new_node_list) if new_node_list.ast_reflection.is_expandable() => {
                                                                    println!("expandable");
                                                                    children.append(&mut new_node_list.elems.clone());
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
                    } else {
                        match self.is_choice_successful(&Some(each_choice.occurrence_count), each_choice)? {
                            Some(v) => {
                                if choice.elem_containers.len() != 1 {
                                    let new_child = SyntaxNodeElement::from_node_list_args(v, each_choice.ast_reflection.clone());

                                    match new_child {
                                        SyntaxNodeElement::NodeList(node_list) if node_list.elems.len() == 0 => (),
                                        _ => {
                                            match new_child {
                                                SyntaxNodeElement::NodeList(new_node_list) if new_node_list.ast_reflection.is_expandable() => {
                                                    println!("expandable");
                                                    children.append(&mut new_node_list.elems.clone());
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
                    }
                },
                RuleElementContainer::RuleExpression(each_expr) => {
                    match self.is_expr_successful(each_expr)? {
                        Some(node_elems) => {
                            for each_elem in node_elems {
                                match each_elem {
                                    SyntaxNodeElement::NodeList(node_list) if node_list.elems.len() == 0 => (),
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

    fn is_expr_successful(&mut self, expr: &Box<RuleExpression>) -> std::result::Result<std::option::Option<Vec<SyntaxNodeElement>>, SyntaxParseError> {
        return self.is_lookahead_expr_successful(expr);
    }

    fn is_lookahead_expr_successful(&mut self, expr: &Box<RuleExpression>) -> std::result::Result<std::option::Option<Vec<SyntaxNodeElement>>, SyntaxParseError> {
        match expr.lookahead_kind {
            RuleLookaheadKind::None => {
                return self.is_loop_expr_successful(expr);
            },
            RuleLookaheadKind::Positive | RuleLookaheadKind::Negative => {
                let start_src_i = self.src_i;
                let is_lookahead_positive = expr.lookahead_kind == RuleLookaheadKind::Positive;

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

    fn is_loop_expr_successful(&mut self, expr: &Box<RuleExpression>) -> std::result::Result<std::option::Option<Vec<SyntaxNodeElement>>, SyntaxParseError> {
        let (min_count, max_count) = expr.loop_count;

        if min_count == -1 || (max_count != -1 && min_count > max_count) {
            return Err(SyntaxParseError::InternalErr(format!("invalid loop count {{{},{}}}", min_count, max_count)));
        }

        let mut children = Vec::<SyntaxNodeElement>::new();
        let mut loop_count = 0;

        while self.src_i < self.src_content.len() {
            if loop_count > self.max_loop_count as i32 {
                return Err(SyntaxParseError::TooLongRepeat(self.max_loop_count as usize));
            }

            match self.is_each_expr_matched(expr)? {
                Some(node) => {
                    for each_node in node {
                        match each_node {
                            SyntaxNodeElement::NodeList(node_list) if node_list.elems.len() == 0 => (),
                            _ => children.push(each_node),
                        }
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

    fn is_each_expr_matched(&mut self, expr: &Box<RuleExpression>) -> std::result::Result<std::option::Option<Vec<SyntaxNodeElement>>, SyntaxParseError> {
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
                    let new_leaf = SyntaxNodeElement::from_leaf_args(tar_char, expr.ast_reflection.clone());
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
                        self.recursion_count -= 1;

                        let conv_node_elems = match &node_elem {
                            SyntaxNodeElement::NodeList(node_list) => {
                                let sub_ast_reflection = match &expr.ast_reflection {
                                    ASTReflection::Reflectable(elem_name) => {
                                        let conv_elem_name = if elem_name == "" {
                                            expr.value.clone()
                                        } else {
                                            elem_name.clone()
                                        };

                                        ASTReflection::Reflectable(conv_elem_name)
                                    },
                                    _ => expr.ast_reflection.clone(),
                                };

                                let node = SyntaxNodeElement::from_node_list_args(node_list.elems.clone(), sub_ast_reflection);

                                if expr.ast_reflection.is_expandable() {
                                    match node {
                                        SyntaxNodeElement::NodeList(node) => node.elems,
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
                if self.src_content.len() < self.src_i + expr.value.len() {
                    return Ok(None);
                }

                if self.src_content[self.src_i..self.src_i + expr.value.len()] == expr.value {
                    let new_leaf = SyntaxNodeElement::from_leaf_args(expr.value.clone(), expr.ast_reflection.clone());
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
                let new_leaf = SyntaxNodeElement::from_leaf_args(expr_value, expr.ast_reflection.clone());
                self.src_i += 1;
                return Ok(Some(vec![new_leaf]));
            },
        }
    }
}
