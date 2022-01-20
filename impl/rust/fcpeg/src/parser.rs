use std::collections::*;
use std::rc::Rc;
use std::cell::RefCell;

use crate::rule::*;
use crate::tree::*;

use regex::*;

use rustnutlib::*;
use rustnutlib::console::*;

use uuid::Uuid;

pub enum SyntaxParseError {
    Unknown {},
    InternalError { msg: String },
    InvalidCharClassFormat { value: String },
    InvalidGenericsArgumentLength { arg_ids: Vec<String> },
    InvalidFunctionArgumentLength { arg_ids: Vec<String> },
    InvalidSyntaxTreeStructure { cause: String },
    NoSucceededRule { pos: CharacterPosition, rule_id: String, rule_stack: Vec<(CharacterPosition, String)> },
    TooLongRepetition { max_loop_count: usize },
    UnknownArgumentID { arg_id: String },
    UnknownRuleID { pos: CharacterPosition, rule_id: String },
}

impl ConsoleLogger for SyntaxParseError {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            SyntaxParseError::Unknown {} => log!(Error, "unknown error"),
            SyntaxParseError::InternalError { msg } => log!(Error, &format!("internal error: {}", msg)),
            SyntaxParseError::InvalidCharClassFormat { value } => log!(Error, &format!("invalid character class format '{}'", value)),
            SyntaxParseError::InvalidGenericsArgumentLength { arg_ids } => log!(Error, &format!("invalid generics argument length ({:?})", arg_ids)),
            SyntaxParseError::InvalidFunctionArgumentLength { arg_ids } => log!(Error, &format!("invalid function argument length ({:?})", arg_ids)),
            SyntaxParseError::InvalidSyntaxTreeStructure { cause } => log!(Error, &format!("invalid syntax tree structure ({})", cause)),
            SyntaxParseError::NoSucceededRule { pos, rule_id, rule_stack } => log!(Error, &format!("no succeeded rule '{}'", rule_id), format!("at:\t{}", pos), format!("rule stack:\t{}", rule_stack.iter().map(|(each_pos, each_rule_id)| format!("\n\t\t{} at {}", each_rule_id, each_pos)).collect::<Vec<String>>().join(""))),
            SyntaxParseError::TooLongRepetition { max_loop_count } => log!(Error, &format!("too long repetition over {}", max_loop_count)),
            SyntaxParseError::UnknownArgumentID { arg_id } => log!(Error, &format!("unknown argument id '{}'", arg_id)),
            SyntaxParseError::UnknownRuleID { pos, rule_id } => log!(Error, &format!("unknown rule id '{}'", rule_id), &format!("at: {}", pos)),
        };
    }
}

pub type ArgumentMap = HashMap<String, Argument>;

// todo: 取り外す
#[allow(unused)]
pub struct Argument {
    kind: ArgumentKind,
    rule_group: Box<RuleGroup>,
}

impl Argument {
    pub fn new(kind: ArgumentKind, rule_group: Box<RuleGroup>) -> Argument {
        return Argument {
            kind: kind,
            rule_group: rule_group,
        };
    }
}

pub enum ArgumentKind {
    Function,
    Generics,
}

pub struct MemoizationMap {
    // note: HashMap<(group_uuid, src_i), (src_len, result)>
    map: HashMap<(Uuid, usize), (usize, Option<Vec<SyntaxNodeElement>>)>,
}

impl MemoizationMap {
    pub fn new() -> MemoizationMap {
        return MemoizationMap {
            map: HashMap::new(),
        };
    }

    pub fn push(&mut self, group_uuid: Uuid, src_i: usize, src_len: usize, result: Option<Vec<SyntaxNodeElement>>) {
        self.map.insert((group_uuid, src_i), (src_len, result));
    }

    pub fn find(&self, pattern: &Uuid, src_i: usize) -> Option<(usize, Option<Vec<SyntaxNodeElement>>)> {
        return match self.map.get(&(*pattern, src_i)) {
            Some((src_len, result)) => Some((*src_len, result.clone())),
            None => None,
        };
    }
}

pub struct SyntaxParser {
    cons: Rc<RefCell<Console>>,
    rule_map: Box<RuleMap>,
    src_i: usize,
    src_line: usize,
    src_latest_line_i: usize,
    src_path: String,
    src_content: String,
    max_loop_count: usize,
    arg_maps: Box<Vec<ArgumentMap>>,
    rule_stack: Vec<(CharacterPosition, String)>,
    regex_map: Box<HashMap<String, Regex>>,
    memoized_map: Box<MemoizationMap>,
    enable_memoization: bool,
}

impl SyntaxParser {
    pub fn new(cons: Rc<RefCell<Console>>, rule_map: Box<RuleMap>, enable_memoization: bool) -> ConsoleResult<SyntaxParser> {
        return Ok(SyntaxParser {
            cons: cons,
            rule_map: rule_map,
            src_i: 0,
            src_line: 0,
            src_latest_line_i: 0,
            src_path: String::new(),
            src_content: String::new(),
            max_loop_count: 65536,
            arg_maps: Box::new(Vec::new()),
            rule_stack: Vec::new(),
            regex_map: Box::new(HashMap::new()),
            memoized_map: Box::new(MemoizationMap::new()),
            enable_memoization: enable_memoization,
        });
    }

    pub fn parse(&mut self, src_path: String, src_content: &Box<String>) -> ConsoleResult<SyntaxTree> {
        let mut tmp_src_content = *src_content.clone();

        // note: 余分な改行コード 0x0d を排除する
        loop {
            match tmp_src_content.find(0x0d as char) {
                Some(v) => {
                    let _ = tmp_src_content.remove(v);
                },
                None => break,
            }
        }

        // EOF 用のヌル文字
        tmp_src_content += "\0";

        // フィールドを初期化
        self.src_i = 0;
        self.src_path = src_path;
        self.src_content = tmp_src_content;

        let start_rule_id = self.rule_map.start_rule_id.clone();

        if self.src_content.chars().count() == 0 {
            return Ok(SyntaxTree::from_node_args(vec![], ASTReflectionStyle::Reflection(String::new())));
        }

        // todo: CharacterPosition を修正
        let mut root_node = match self.parse_rule(&start_rule_id, &CharacterPosition::get_empty())? {
            Some(v) => v,
            None => {
                self.cons.borrow_mut().append_log(SyntaxParseError::NoSucceededRule {
                    rule_id: start_rule_id.clone(),
                    pos: self.get_char_position(),
                    rule_stack: self.rule_stack.clone(),
                }.get_log());

                return Err(());
            },
        };

        // note: ルートは常に Reflectable
        root_node.set_ast_reflection_style(ASTReflectionStyle::Reflection(start_rule_id.clone()));

        // note: 入力位置が length を超えると失敗
        if self.src_i < self.src_content.chars().count() {
            self.cons.borrow_mut().append_log(SyntaxParseError::NoSucceededRule {
                rule_id: start_rule_id.clone(),
                pos: self.get_char_position(),
                rule_stack: self.rule_stack.clone(),
            }.get_log());

            return Err(());
        }

        return Ok(SyntaxTree::from_node(root_node));
    }

    fn parse_rule(&mut self, rule_id: &String, pos: &CharacterPosition) -> ConsoleResult<Option<SyntaxNodeElement>> {
        let rule_group = match self.rule_map.rule_map.get(rule_id) {
            Some(rule) => rule.group.clone(),
            None => {
                self.cons.borrow_mut().append_log(SyntaxParseError::UnknownRuleID {
                    pos: pos.clone(),
                    rule_id: rule_id.clone(),
                }.get_log());

                return Err(());
            },
        };

        self.rule_stack.push((self.get_char_position(), rule_id.clone()));

        return match self.parse_group(&rule_group.elem_order, &rule_group)? {
            Some(v) => {
                let mut ast_reflection_style = match &rule_group.sub_elems.get(0) {
                    Some(v) => {
                        match v {
                            RuleElement::Group(sub_choice) => sub_choice.ast_reflection_style.clone(),
                            RuleElement::Expression(_) => rule_group.ast_reflection_style.clone(),
                        }
                    },
                    _ => rule_group.ast_reflection_style.clone(),
                };

                match &ast_reflection_style {
                    ASTReflectionStyle::Reflection(elem_name) if *elem_name == String::new() => {
                        // todo: 構成ファイルを ASTReflection に反映
                        ast_reflection_style = ASTReflectionStyle::from_config(false, true, rule_id.clone());
                    },
                    _ => (),
                };

                self.rule_stack.pop().unwrap();
                let new_node = SyntaxNodeElement::from_node_args(v, ast_reflection_style);
                Ok(Some(new_node))
            },
            None => {
                Ok(None)
            },
        }
    }

    fn parse_group(&mut self, parent_elem_order: &RuleElementOrder, group: &Box<RuleGroup>) -> ConsoleResult<Option<Vec<SyntaxNodeElement>>> {
        if self.enable_memoization {
            match self.memoized_map.find(&group.uuid, self.src_i) {
                Some((src_len, result)) => {
                    self.src_i += src_len;
                    return Ok(result);
                },
                None => (),
            }
        }

        let tmp_i = self.src_i;
        let result = self.parse_lookahead_group(parent_elem_order, group)?;

        if self.enable_memoization {
            if self.src_i != tmp_i {
                self.memoized_map.push(group.uuid.clone(), tmp_i, self.src_i - tmp_i, result.clone());
            }
        }

        return Ok(result);
    }

    fn parse_lookahead_group(&mut self, parent_elem_order: &RuleElementOrder, group: &Box<RuleGroup>) -> ConsoleResult<Option<Vec<SyntaxNodeElement>>> {
        return if group.lookahead_kind.is_none() {
            self.parse_loop_group(parent_elem_order, group)
        } else {
            let start_src_i = self.src_i;
            let is_lookahead_positive = group.lookahead_kind == RuleElementLookaheadKind::Positive;

            let result = self.parse_loop_group(parent_elem_order, group)?;
            self.src_i = start_src_i;

            if result.is_some() == is_lookahead_positive {
                Ok(Some(vec![]))
            } else {
                Ok(None)
            }
        };
    }

    fn parse_loop_group(&mut self, parent_elem_order: &RuleElementOrder, group: &Box<RuleGroup>) -> ConsoleResult<Option<Vec<SyntaxNodeElement>>> {
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
            self.cons.borrow_mut().append_log(SyntaxParseError::InternalError {
                msg: format!("invalid loop count {{{},{}}}", min_count, max_count),
            }.get_log());

            return Err(());
        }

        let mut children = Vec::<SyntaxNodeElement>::new();
        let mut loop_count = 0i32;

        while self.src_i < self.src_content.chars().count() {
            if loop_count > self.max_loop_count as i32 {
                self.cons.borrow_mut().append_log(SyntaxParseError::TooLongRepetition {
                    max_loop_count: self.max_loop_count as usize,
                }.get_log());

                return Err(());
            }

            match self.parse_raw_choice(group)? {
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

    fn parse_raw_choice(&mut self, group: &Box<RuleGroup>) -> ConsoleResult<Option<Vec<SyntaxNodeElement>>> {
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
                                            match self.parse_group(&each_group.elem_order, each_sub_choice)? {
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
                                                match self.parse_group(&each_group.elem_order, each_sub_group)? {
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
                                    match self.parse_group(&each_group.elem_order, each_group)? {
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
                    match self.parse_expr(each_expr)? {
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

    fn parse_expr(&mut self, expr: &Box<RuleExpression>) -> ConsoleResult<Option<Vec<SyntaxNodeElement>>> {
        return self.parse_lookahead_expr(expr);
    }

    fn parse_lookahead_expr(&mut self, expr: &Box<RuleExpression>) -> ConsoleResult<Option<Vec<SyntaxNodeElement>>> {
        return if expr.lookahead_kind.is_none() {
            self.parse_loop_expr(expr)
        } else {
            let start_src_i = self.src_i;
            let is_lookahead_positive = expr.lookahead_kind == RuleElementLookaheadKind::Positive;

            let result = self.parse_loop_expr(expr)?;
            self.src_i = start_src_i;

            if result.is_some() == is_lookahead_positive {
                Ok(Some(vec![]))
            } else {
                Ok(None)
            }
        }
    }

    fn parse_loop_expr(&mut self, expr: &Box<RuleExpression>) -> ConsoleResult<Option<Vec<SyntaxNodeElement>>> {
        let (min_count, max_count) = expr.loop_count.to_tuple();

        if max_count != -1 && min_count as i32 > max_count {
            self.cons.borrow_mut().append_log(SyntaxParseError::InternalError {
                msg: format!("invalid loop count {{{},{}}}", min_count, max_count),
            }.get_log());

            return Err(());
        }

        let mut children = Vec::<SyntaxNodeElement>::new();
        let mut loop_count = 0usize;

        while self.src_i < self.src_content.chars().count() {
            if loop_count > self.max_loop_count {
                self.cons.borrow_mut().append_log(SyntaxParseError::TooLongRepetition {
                    max_loop_count: self.max_loop_count as usize
                }.get_log());

                return Err(());
            }

            match self.parse_raw_expr(expr)? {
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

    fn parse_raw_expr(&mut self, expr: &Box<RuleExpression>) -> ConsoleResult<Option<Vec<SyntaxNodeElement>>> {
        if self.src_i >= self.src_content.chars().count() {
            return Ok(None);
        }

        match &expr.kind {
            // todo: argid に引数 kind を追加
            RuleExpressionKind::ArgID => {
                let mut group = Option::<Box<RuleGroup>>::None;

                for each_arg_map in &*self.arg_maps {
                    match each_arg_map.get(&expr.value) {
                        Some(v) => {
                            group = Some(v.rule_group.clone());
                            break;
                        },
                        None => (),
                    };
                }

                let result = match &group {
                    Some(v) => self.parse_group(&RuleElementOrder::Sequential, &v),
                    None => {
                        self.cons.borrow_mut().append_log(SyntaxParseError::UnknownArgumentID {
                            arg_id: expr.value.clone(),
                        }.get_log());

                        return Err(());
                    },
                };

                return if !expr.ast_reflection_style.is_reflectable() {
                    match &result {
                        Ok(v) => {
                            match v {
                                Some(node_elems) => {
                                    match node_elems.get(0) {
                                        Some(each_node_elem) => {
                                            let mut new_node_elem = each_node_elem.clone();
                                            new_node_elem.set_ast_reflection_style(expr.ast_reflection_style.clone());
                                            Ok(Some(vec![new_node_elem]))
                                        },
                                        _ => result,
                                    }
                                },
                                None => result,
                            }
                        },
                        Err(()) => result,
                    }
                } else {
                    result
                };
            },
            RuleExpressionKind::CharClass => {
                if self.src_content.chars().count() < self.src_i + 1 {
                    return Ok(None);
                }

                // note: Regex パターンが見つからない場合は新しく追加する
                let pattern = match self.regex_map.get(&expr.value) {
                    Some(v) => v,
                    None => {
                        let pattern = match Regex::new(&expr.value.clone()) {
                            Ok(v) => v,
                            Err(_) => {
                                self.cons.borrow_mut().append_log(SyntaxParseError::InvalidCharClassFormat {
                                    value: expr.to_string(),
                                }.get_log());

                                return Err(());
                            },
                        };

                        self.regex_map.insert(expr.value.clone(), pattern);
                        self.regex_map.get(&expr.value).unwrap()
                    },
                };

                let tar_char = self.substring_src_content(self.src_i, 1);

                if pattern.is_match(&tar_char) {
                    let new_leaf = SyntaxNodeElement::from_leaf_args(self.get_char_position(), tar_char.clone(), expr.ast_reflection_style.clone());
                    self.add_source_index_by_string(&tar_char);

                    return Ok(Some(vec![new_leaf]));
                } else {
                    return Ok(None);
                }
            },
            RuleExpressionKind::Generics(args) => {
                let rule_id = &expr.value;

                let mut new_arg_map = ArgumentMap::new();
                let new_arg_ids = match self.rule_map.rule_map.get(rule_id) {
                    Some(rule) => &rule.generics_arg_ids,
                    None => {
                        self.cons.borrow_mut().append_log(SyntaxParseError::UnknownRuleID {
                            pos: expr.pos.clone(),
                            rule_id: rule_id.clone(),
                        }.get_log());

                        return Err(());
                    },
                };

                if args.len() != new_arg_ids.len() {
                    self.cons.borrow_mut().append_log(SyntaxParseError::InvalidGenericsArgumentLength {
                        arg_ids: new_arg_ids.clone(),
                    }.get_log());

                    return Err(());
                }

                for i in 0..args.len() {
                    let new_arg_id = match new_arg_ids.get(i) {
                        Some(v) => v,
                        None => {
                            self.cons.borrow_mut().append_log(SyntaxParseError::InternalError {
                                msg: "invalid operation".to_string(),
                            }.get_log());

                            return Err(());
                        },
                    };

                    let new_arg_group = match args.get(i) {
                        Some(v) => v,
                        None => {
                            self.cons.borrow_mut().append_log(SyntaxParseError::InternalError {
                                msg: "invalid operation".to_string(),
                            }.get_log());

                            return Err(());
                        }
                    };

                    new_arg_map.insert(new_arg_id.clone(), Argument::new(ArgumentKind::Generics, new_arg_group.clone()));
                }

                self.arg_maps.push(new_arg_map);
                let result = self.parse_id_expr(expr);
                self.arg_maps.pop();
                return result;
            },
            RuleExpressionKind::Func(args) => {
                let rule_id = &expr.value;

                // note: プリミティブ関数
                match rule_id.as_str() {
                    "JOIN" => {
                        match args.get(0) {
                            Some(tar_arg) if args.len() == 1 => {
                                return match self.parse_group(&RuleElementOrder::Sequential, tar_arg)? {
                                    Some(result_elems) => {
                                        let mut joined_str = String::new();

                                        for each_elem in result_elems {
                                            match each_elem {
                                                SyntaxNodeElement::Node(node) if node.is_reflectable() => joined_str += &node.join_child_leaf_values(),
                                                SyntaxNodeElement::Leaf(leaf) if leaf.is_reflectable() => joined_str += &leaf.value,
                                                _ => (),
                                            }
                                        }

                                        let new_leaf = SyntaxNodeElement::from_leaf_args(self.get_char_position(), joined_str, expr.ast_reflection_style.clone());
                                        Ok(Some(vec![new_leaf]))
                                    },
                                    None => Ok(None),
                                };
                            },
                            _ => {
                                self.cons.borrow_mut().append_log(SyntaxParseError::InvalidFunctionArgumentLength {
                                    arg_ids: vec!["Item".to_string()],
                                }.get_log());

                                return Err(());
                            },
                        }
                    },
                    _ => (),
                }

                let mut new_arg_map = ArgumentMap::new();
                let new_arg_ids = match self.rule_map.rule_map.get(rule_id) {
                    Some(rule) => &rule.func_arg_ids,
                    None => {
                        self.cons.borrow_mut().append_log(SyntaxParseError::UnknownRuleID {
                            pos: expr.pos.clone(), rule_id: rule_id.clone(),
                        }.get_log());

                        return Err(());
                    },
                };

                if args.len() != new_arg_ids.len() {
                    self.cons.borrow_mut().append_log(SyntaxParseError::InvalidFunctionArgumentLength {
                        arg_ids: new_arg_ids.clone(),
                    }.get_log());

                    return Err(());
                }

                for i in 0..args.len() {
                    let new_arg_id = match new_arg_ids.get(i) {
                        Some(v) => v,
                        None => {
                            self.cons.borrow_mut().append_log(SyntaxParseError::InternalError {
                                msg: "invalid operation".to_string(),
                            }.get_log());

                            return Err(());
                        },
                    };

                    let new_arg_group = match args.get(i) {
                        Some(v) => v,
                        None => {
                            self.cons.borrow_mut().append_log(SyntaxParseError::InternalError {
                                msg: "invalid operation".to_string(),
                            }.get_log());

                            return Err(());
                        },
                    };

                    new_arg_map.insert(new_arg_id.clone(), Argument::new(ArgumentKind::Function, new_arg_group.clone()));
                }

                self.arg_maps.push(new_arg_map);
                let result = self.parse_id_expr(expr);
                self.arg_maps.pop();
                return result;
            },
            RuleExpressionKind::ID => {
                return self.parse_id_expr(expr);
            },
            RuleExpressionKind::String => {
                if self.src_content.chars().count() < self.src_i + expr.value.chars().count() {
                    return Ok(None);
                }

                if self.substring_src_content(self.src_i, expr.value.chars().count()) == expr.value {
                    let new_leaf = SyntaxNodeElement::from_leaf_args(self.get_char_position(), expr.value.clone(), expr.ast_reflection_style.clone());
                    self.add_source_index_by_string(&expr.value);

                    return Ok(Some(vec![new_leaf]));
                } else {
                    return Ok(None);
                }
            },
            RuleExpressionKind::Wildcard => {
                if self.src_content.chars().count() < self.src_i + 1 {
                    return Ok(None);
                }

                let expr_value = self.substring_src_content(self.src_i, 1);
                let new_leaf = SyntaxNodeElement::from_leaf_args(self.get_char_position(), expr_value.clone(), expr.ast_reflection_style.clone());
                self.add_source_index_by_string(&expr_value);

                return Ok(Some(vec![new_leaf]));
            },
        }
    }

    fn parse_id_expr(&mut self, expr: &Box<RuleExpression>) -> ConsoleResult<Option<Vec<SyntaxNodeElement>>> {
        match self.parse_rule(&expr.value, &expr.pos)? {
            Some(node_elem) => {
                let conv_node_elems = match &node_elem {
                    SyntaxNodeElement::Node(node) => {
                        let sub_ast_reflection_style = match &expr.ast_reflection_style {
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

                        let node = SyntaxNodeElement::from_node_args(node.sub_elems.clone(), sub_ast_reflection_style);

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
                return Ok(None);
            },
        };
    }

    fn substring_src_content(&self, start_i: usize, len: usize) -> String {
        return self.src_content.chars().skip(start_i).take(len).collect::<String>();
    }

    // todo: 文字列と進めるインデックスのペアとしてキャッシュを取る (メモ化で解決できる)
    fn add_source_index_by_string(&mut self, expr_str: &String) {
        let mut new_line_indexes = Vec::<usize>::new();
        let mut char_i = 0usize;

        for each_char in expr_str.chars().rev() {
            if each_char == '\n' {
                new_line_indexes.push(char_i);

                if new_line_indexes.len() >= 2 {
                    break;
                }
            }

            char_i += 1;
        }

        match new_line_indexes.pop() {
            Some(latest_new_line_i) => {
                self.src_line += expr_str.match_indices("\n").count();
                self.src_latest_line_i = match new_line_indexes.last() {
                    Some(second_latest_new_line_i) => self.src_i + latest_new_line_i - second_latest_new_line_i + 1,
                    None => self.src_i + latest_new_line_i + 1,
                };
            },
            None => (),
        }

        self.src_i += expr_str.chars().count();
    }

    fn get_char_position(&self) -> CharacterPosition {
        // note: 検査に失敗すると src_i < src_latest_line_i になる; その場合は src_latest_line_i の値を使用する
        let column = match self.src_i.checked_sub(self.src_latest_line_i) {
            Some(v) => v,
            None => self.src_latest_line_i,
        };

        return CharacterPosition::new(Some(self.src_path.clone()), self.src_i, self.src_line, column);
    }
}
