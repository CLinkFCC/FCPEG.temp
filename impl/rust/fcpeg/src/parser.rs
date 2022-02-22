use std::cell::RefCell;
use std::collections::*;
use std::rc::Rc;
use std::sync::Arc;

use crate::block::*;
use crate::cons::*;
use crate::rule::*;
use crate::tree::*;

use colored::*;

use regex::*;

use cons_util::*;
use cons_util::cons::*;

use uuid::Uuid;

#[derive(Clone, PartialEq)]
pub enum SyntaxParsingLog {
    CharacterClassFormatIsInvalid { value: String },
    ExpectedGenericsArgumentsProvided { pos: CharacterPosition, unexpected_len: usize, expected_len: usize },
    ExpectedTemplateArgumentsProvided { pos: CharacterPosition, unexpected_len: usize, expected_len: usize },
    GenericsArgumentIDNotFound { arg_id: String },
    LoopRangeIsInvalidOnParsing { loop_range: String },
    ParsingFailedAtRule { pos: CharacterPosition, rule_id: String, rule_stack: Vec<(CharacterPosition, String)> },
    PrimitiveRuleUncovered { pos: CharacterPosition, rule_name: String },
    RepetitionExceededLoopLimit { loop_limit: usize },
    RuleIDNotFoundOnParsing { pos: CharacterPosition, rule_id: String },
    SkippingPrimitiveRuleSpecifiedWithoutSkipingRules { pos: CharacterPosition },
    StructureOfRuleElementIsInvalid { elem_uuid: Uuid, msg: String },
    TemplateArgumentIDNotFound { arg_id: String },
}

impl ConsoleLogger for SyntaxParsingLog {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            SyntaxParsingLog::CharacterClassFormatIsInvalid { value } => log!(Error, Translator::CharacterClassFormatIsInvalid { value: value.clone() }),
            SyntaxParsingLog::ExpectedGenericsArgumentsProvided { pos, unexpected_len, expected_len } => log!(Error, Translator::ExpectedGenericsArgumentsProvided { unexpected_len: *unexpected_len, expected_len: *expected_len }, Translator::AtDescription { pos: pos.clone() }),
            SyntaxParsingLog::ExpectedTemplateArgumentsProvided { pos, unexpected_len, expected_len } => log!(Error, Translator::ExpectedTemplateArgumentsProvided { unexpected_len: *unexpected_len, expected_len: *expected_len }, Translator::AtDescription { pos: pos.clone() }),
            SyntaxParsingLog::GenericsArgumentIDNotFound { arg_id } => log!(Error, Translator::GenericsArgumentIDNotFound { arg_id: arg_id.clone() }),
            SyntaxParsingLog::LoopRangeIsInvalidOnParsing { loop_range } => log!(Error, Translator::LoopRangeIsInvalidOnParsing { loop_range: loop_range.clone() }),
            SyntaxParsingLog::ParsingFailedAtRule { pos, rule_id, rule_stack } => log!(Error, Translator::ParsingFailedAtRule { rule_id: rule_id.clone() }, Translator::AtDescription { pos: pos.clone() }, Translator::RawDescription { msg: rule_stack.iter().map(|(each_pos, each_rule_id)| format!("\t\t{} at {}", each_rule_id, each_pos)).collect::<Vec<String>>().join("\n") }),
            SyntaxParsingLog::PrimitiveRuleUncovered { pos, rule_name } => log!(Error, Translator::PrimitiveRuleUncovered { rule_name: rule_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            SyntaxParsingLog::RepetitionExceededLoopLimit { loop_limit } => log!(Error, Translator::RepetitionExceededLoopLimit { loop_limit: *loop_limit }),
            SyntaxParsingLog::RuleIDNotFoundOnParsing { pos, rule_id } => log!(Error, Translator::RuleIDNotFoundOnParsing { rule_id: rule_id.clone() }, Translator::AtDescription { pos: pos.clone() }),
            SyntaxParsingLog::SkippingPrimitiveRuleSpecifiedWithoutSkipingRules { pos } => log!(Error, Translator::SkippingPrimitiveRuleSpecifiedWithoutSkipingRules, Translator::AtDescription { pos: pos.clone() }),
            SyntaxParsingLog::StructureOfRuleElementIsInvalid { elem_uuid, msg } => log!(Error, Translator::StructureOfRuleElementIsInvalid { elem_uuid: elem_uuid.clone() }, Translator::RawDescription { msg: msg.bright_black().to_string() }),
            SyntaxParsingLog::TemplateArgumentIDNotFound { arg_id } => log!(Error, Translator::TemplateArgumentIDNotFound { arg_id: arg_id.clone() }),
        };
    }
}

#[derive(Clone)]
pub struct ArgumentMap {
    generics_group_map: HashMap<String, Rc<Box<RuleGroup>>>,
    template_group_map: HashMap<String, Rc<Box<RuleGroup>>>,
}

impl ArgumentMap {
    pub fn new() -> ArgumentMap {
        return ArgumentMap {
            generics_group_map: HashMap::new(),
            template_group_map: HashMap::new(),
        };
    }
}

pub struct MemoizationMap {
    // note: HashMap<(group_uuid, src_i), (src_len, result)>
    map: HashMap<(Uuid, usize), (usize, SyntaxParsingResult)>,
}

impl MemoizationMap {
    pub fn new() -> MemoizationMap {
        return MemoizationMap {
            map: HashMap::new(),
        };
    }

    pub fn push(&mut self, group_uuid: Uuid, src_i: usize, src_len: usize, result: SyntaxParsingResult) {
        self.map.insert((group_uuid, src_i), (src_len, result));
    }

    pub fn find(&self, pattern: &Uuid, src_i: usize) -> Option<(usize, SyntaxParsingResult)> {
        return match self.map.get(&(*pattern, src_i)) {
            Some((src_len, result)) => Some((*src_len, result.clone())),
            None => None,
        };
    }
}

#[derive(Clone)]
pub enum SyntaxParsingResult {
    Success(Vec<SyntaxNodeChild>),
    Failure,
}

impl SyntaxParsingResult {
    pub fn is_successful(&self) -> bool {
        return match self {
            SyntaxParsingResult::Success(_) => true,
            SyntaxParsingResult::Failure => false
        };
    }
}

pub type RegexMap = HashMap<String, Regex>;

pub struct SyntaxParser {
    cons: Rc<RefCell<Console>>,
    rule_map: Arc<Box<RuleMap>>,
    src_i: usize,
    src_line: usize,
    src_latest_line_i: usize,
    src_path: String,
    src_content: Box<String>,
    loop_limit: usize,
    arg_maps: Vec<ArgumentMap>,
    rule_stack: Vec<(CharacterPosition, String)>,
    regex_map: HashMap<String, Regex>,
    memoized_map: MemoizationMap,
    enable_memoization: bool,
    // spec: スキッピング規則のパースをしているかどうか
    is_skipping_now: bool,
    // spec: NOSKIP プリミティブ規則の後ろ側のスキッピングを防止するフラグ
    is_skipped_by_noskip_once: bool,
    skipping_stack: Vec<Vec<String>>,
}

impl SyntaxParser {
    pub fn parse(cons: Rc<RefCell<Console>>, rule_map: Arc<Box<RuleMap>>, src_path: String, src_content: Box<String>, enable_memoization: bool) -> ConsoleResult<SyntaxTree> {
        let mut parser = SyntaxParser {
            cons: cons,
            rule_map: rule_map,
            src_i: 0,
            src_line: 0,
            src_latest_line_i: 0,
            src_path: src_path,
            src_content: src_content,
            loop_limit: 65536,
            arg_maps: Vec::new(),
            rule_stack: Vec::new(),
            regex_map: RegexMap::new(),
            memoized_map: MemoizationMap::new(),
            enable_memoization: enable_memoization,
            is_skipping_now: false,
            is_skipped_by_noskip_once: false,
            skipping_stack: Vec::new(),
        };

        // note: 余分な改行コード 0x0d を排除する
        loop {
            match parser.src_content.find(0x0d as char) {
                Some(v) => {
                    let _ = parser.src_content.remove(v);
                },
                None => break,
            }
        }

        // EOF 用のヌル文字
        *parser.src_content += "\0";

        let start_rule_id = parser.rule_map.start_rule_id.clone();

        if parser.src_content.chars().count() == 0 {
            let result = Ok(SyntaxTree::from_node_child_args(Vec::new(), AstReflectionStyle::Reflection(String::new())));
            return result;
        }

        let start_rule_pos = parser.rule_map.start_rule_pos.clone();

        let mut root_node = match &mut parser.parse_rule(&start_rule_id, &start_rule_pos)? {
            SyntaxParsingResult::Success(children) => children.remove(0),
            SyntaxParsingResult::Failure => {
                parser.cons.borrow_mut().append_log(SyntaxParsingLog::ParsingFailedAtRule {
                    pos: parser.get_char_position(),
                    rule_id: start_rule_id.clone(),
                    rule_stack: parser.rule_stack.clone(),
                }.get_log());

                return Err(());
            },
        };

        // note: ルートは常に Reflectable
        root_node.set_ast_reflection_style(AstReflectionStyle::Reflection(start_rule_id.clone()));

        // note: 入力位置が length を超えると失敗
        if parser.src_i < parser.src_content.chars().count() {
            parser.cons.borrow_mut().append_log(SyntaxParsingLog::ParsingFailedAtRule {
                pos: parser.get_char_position(),
                rule_id: start_rule_id.clone(),
                rule_stack: parser.rule_stack.clone(),
            }.get_log());

            return Err(());
        }

        return Ok(SyntaxTree::from_node_child(root_node));
    }

    fn parse_rule(&mut self, rule_id: &String, pos: &CharacterPosition) -> ConsoleResult<SyntaxParsingResult> {
        let rule_group = match self.rule_map.rule_map.get(rule_id) {
            Some(rule) => {
                self.skipping_stack.push(rule.skipping_tar_ids.clone());
                rule.group.clone()
            },
            None => {
                self.cons.borrow_mut().append_log(SyntaxParsingLog::RuleIDNotFoundOnParsing {
                    pos: pos.clone(),
                    rule_id: rule_id.clone(),
                }.get_log());

                return Err(());
            },
        };

        if !self.is_skipping_now {
            self.rule_stack.push((self.get_char_position(), rule_id.clone()));
        }

        return match self.parse_group(&rule_group.elem_order, &rule_group)? {
            SyntaxParsingResult::Success(result_children) => {
                let mut ast_reflection_style = match &rule_group.subelems.get(0) {
                    Some(v) => {
                        match v {
                            RuleElement::Group(subchoice) => subchoice.ast_reflection_style.clone(),
                            RuleElement::Expression(_) => rule_group.ast_reflection_style.clone(),
                        }
                    },
                    _ => rule_group.ast_reflection_style.clone(),
                };

                match &ast_reflection_style {
                    AstReflectionStyle::Reflection(elem_name) if *elem_name == String::new() => {
                        // todo: 構成ファイルを ASTReflection に反映
                        ast_reflection_style = AstReflectionStyle::from_config(false, true, rule_id.clone());
                    },
                    _ => (),
                };

                if !self.is_skipping_now {
                    self.rule_stack.pop().unwrap();
                }

                self.skipping_stack.pop();

                let new_node = SyntaxNodeChild::from_node_args(result_children, ast_reflection_style);
                Ok(SyntaxParsingResult::Success(vec![new_node]))
            },
            SyntaxParsingResult::Failure => {
                self.skipping_stack.pop();
                Ok(SyntaxParsingResult::Failure)
            },
        }
    }

    fn parse_group(&mut self, parent_elem_order: &ElementOrder, group: &Rc<Box<RuleGroup>>) -> ConsoleResult<SyntaxParsingResult> {
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
            // note: 実引数をメモ化から除外する
            // fix: 実引数のメモ化手法
            if self.src_i != tmp_i && !SyntaxParser::has_argument_in_group(group) {
                self.memoized_map.push(group.uuid.clone(), tmp_i, self.src_i - tmp_i, result.clone());
            }
        }

        return Ok(result);
    }

    fn parse_lookahead_group(&mut self, parent_elem_order: &ElementOrder, group: &Rc<Box<RuleGroup>>) -> ConsoleResult<SyntaxParsingResult> {
        return if group.lookahead_kind.is_none() {
            self.parse_loop_group(parent_elem_order, group)
        } else {
            let start_src_i = self.src_i;
            let is_lookahead_positive = group.lookahead_kind == LookaheadKind::Positive;

            let result = self.parse_loop_group(parent_elem_order, group)?;
            self.src_i = start_src_i;

            if result.is_successful() == is_lookahead_positive {
                Ok(SyntaxParsingResult::Success(Vec::new()))
            } else {
                Ok(SyntaxParsingResult::Failure)
            }
        };
    }

    fn parse_loop_group(&mut self, parent_elem_order: &ElementOrder, group: &Rc<Box<RuleGroup>>) -> ConsoleResult<SyntaxParsingResult> {
        let (min_count, max_count) = group.loop_range.to_tuple();

        if max_count != -1 && min_count as isize > max_count {
            self.cons.borrow_mut().append_log(SyntaxParsingLog::LoopRangeIsInvalidOnParsing {
                loop_range: format!("{{{},{}}}", min_count, max_count),
            }.get_log());

            return Err(());
        }

        let mut children = Vec::<SyntaxNodeChild>::new();
        let mut loop_count = 0isize;

        while self.src_i < self.src_content.chars().count() {
            if loop_count > self.loop_limit as isize {
                self.cons.borrow_mut().append_log(SyntaxParsingLog::RepetitionExceededLoopLimit {
                    loop_limit: self.loop_limit as usize,
                }.get_log());

                return Err(());
            }

            match self.parse_element_order_group(parent_elem_order, group)? {
                SyntaxParsingResult::Success(node_children) => {
                    for each_elem in node_children {
                        match &each_elem {
                            SyntaxNodeChild::Node(node) => {
                                if node.as_ref().subelems.len() != 0 {
                                    children.push(each_elem);
                                }
                            },
                            _ => children.push(each_elem),
                        }
                    }

                    loop_count += 1;

                    if max_count != -1 && loop_count == max_count {
                        return Ok(SyntaxParsingResult::Success(children));
                    }
                },
                SyntaxParsingResult::Failure => {
                    return if loop_count >= min_count as isize && (max_count == -1 || loop_count <= max_count) {
                        Ok(SyntaxParsingResult::Success(children))
                    } else {
                        Ok(SyntaxParsingResult::Failure)
                    }
                },
            }
        }

        return if loop_count >= min_count as isize && (max_count == -1 || loop_count <= max_count) {
            Ok(SyntaxParsingResult::Success(children))
        } else {
            Ok(SyntaxParsingResult::Failure)
        }
    }

    fn parse_element_order_group(&mut self, parent_elem_order: &ElementOrder, group: &Rc<Box<RuleGroup>>) -> ConsoleResult<SyntaxParsingResult> {
        let mut children = Vec::<SyntaxNodeChild>::new();

        return match parent_elem_order {
            ElementOrder::Random(random_order_loop_range) => {
                let tar_elems = match group.subelems.get(0) {
                    Some(tar_parent_elem) => {
                        match tar_parent_elem {
                            RuleElement::Group(tar_parent_group) => &tar_parent_group.subelems,
                            _ => {
                                self.cons.borrow_mut().append_log(SyntaxParsingLog::StructureOfRuleElementIsInvalid {
                                    elem_uuid: group.uuid.clone(),
                                    msg: "child element of random order group must be a group".to_string(),
                                }.get_log());

                                return Err(());
                            },
                        }
                    },
                    None => {
                        self.cons.borrow_mut().append_log(SyntaxParsingLog::StructureOfRuleElementIsInvalid {
                            elem_uuid: group.uuid.clone(),
                            msg: "random order group must have a child group".to_string(),
                        }.get_log());

                        return Err(());
                    },
                };

                let random_order_start_src_i = self.src_i;
                let mut subgroup_matching_list = vec![false; tar_elems.len()];
                let mut subgroup_i = 0usize;

                for _ in 0..tar_elems.len() {
                    let elem_start_src_i = self.src_i;
                    for subelem in tar_elems {
                        match subelem {
                            RuleElement::Group(subgroup) => {
                                // fix: クローンを除去
                                let mut conved_subgroup = (**subgroup).clone();
                                conved_subgroup.loop_range = random_order_loop_range.clone();

                                match self.parse_group(&ElementOrder::Sequential, &Rc::new(conved_subgroup))? {
                                    SyntaxParsingResult::Success(node_children) => {
                                        if subgroup_matching_list[subgroup_i] {
                                            subgroup_i += 1;
                                            continue;
                                        }

                                        for each_elem in node_children {
                                            match &each_elem {
                                                SyntaxNodeChild::Node(node) => {
                                                    if node.as_ref().subelems.len() != 0 {
                                                        children.push(each_elem);
                                                    }
                                                },
                                                _ => children.push(each_elem),
                                            }
                                        }

                                        subgroup_matching_list[subgroup_i] = true;
                                        break;
                                    },
                                    SyntaxParsingResult::Failure => {
                                        self.src_i = elem_start_src_i;
                                    },
                                }
                            },
                            _ => (),
                        }

                        subgroup_i += 1;
                    }

                    if subgroup_matching_list.iter().find(|v| !**v).is_none() {
                        return Ok(SyntaxParsingResult::Success(children));
                    }

                    subgroup_i = 0;
                }

                self.src_i = random_order_start_src_i;
                return Ok(SyntaxParsingResult::Failure);
            },
            ElementOrder::Sequential => self.parse_raw_group(group),
        };
    }

    // fix: parse_sequential_group に改名?
    fn parse_raw_group(&mut self, group: &Rc<Box<RuleGroup>>) -> ConsoleResult<SyntaxParsingResult> {
        let mut children = Vec::<SyntaxNodeChild>::new();

        for each_elem in &group.subelems {
            let start_src_i = self.src_i;

            match each_elem {
                RuleElement::Group(each_group) => {
                    match each_group.kind {
                        RuleGroupKind::Choice => {
                            let mut is_successful = false;

                            for each_subelem in &each_group.subelems {
                                match each_subelem {
                                    RuleElement::Group(each_subgroup) => {
                                        let result = self.parse_group(&each_group.elem_order, each_subgroup)?;

                                        match result {
                                            SyntaxParsingResult::Success(v) => {
                                                if group.subelems.len() != 1 {
                                                    let new_child = SyntaxNodeChild::from_node_args(v, each_subgroup.ast_reflection_style.clone());

                                                    match new_child {
                                                        SyntaxNodeChild::Node(node) if node.as_ref().subelems.len() == 0 => (),
                                                        _ => {
                                                            match new_child {
                                                                SyntaxNodeChild::Node(new_node) if new_node.as_ref().ast_reflection_style.is_expandable() => {
                                                                    children.append(&mut new_node.as_ref().subelems.clone());
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
                                            SyntaxParsingResult::Failure => self.src_i = start_src_i,
                                        }
                                    },
                                    _ => (),
                                }
                            }

                            if !is_successful {
                                return Ok(SyntaxParsingResult::Failure);
                            }
                        },
                        RuleGroupKind::Sequence => {
                            let result = self.parse_group(&each_group.elem_order, each_group)?;

                            match result {
                                SyntaxParsingResult::Success(v) => {
                                    if group.subelems.len() != 1 {
                                        let new_child = SyntaxNodeChild::from_node_args(v, each_group.ast_reflection_style.clone());

                                        match new_child {
                                            SyntaxNodeChild::Node(node) if node.as_ref().subelems.len() == 0 => (),
                                            _ => {
                                                match new_child {
                                                    SyntaxNodeChild::Node(new_node) if new_node.as_ref().ast_reflection_style.is_expandable() => {
                                                        children.append(&mut new_node.as_ref().subelems.clone());
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
                                SyntaxParsingResult::Failure => {
                                    self.src_i = start_src_i;
                                    return Ok(SyntaxParsingResult::Failure);
                                },
                            }
                        },
                    }
                },
                RuleElement::Expression(each_expr) => {
                    let result = self.parse_expr(each_expr)?;

                    match result {
                        SyntaxParsingResult::Success(node_children) => {
                            for each_elem in node_children {
                                match each_elem {
                                    SyntaxNodeChild::Node(node) if node.as_ref().subelems.len() == 0 => (),
                                    _ => children.push(each_elem),
                                }
                            }

                            continue;
                        },
                        SyntaxParsingResult::Failure => {
                            self.src_i = start_src_i;
                            return Ok(SyntaxParsingResult::Failure);
                        },
                    }
                }
            }
        }

        return Ok(SyntaxParsingResult::Success(children));
    }

    fn parse_expr(&mut self, expr: &Box<RuleExpression>) -> ConsoleResult<SyntaxParsingResult> {
        return self.parse_lookahead_expr(expr);
    }

    fn parse_lookahead_expr(&mut self, expr: &Box<RuleExpression>) -> ConsoleResult<SyntaxParsingResult> {
        return if expr.lookahead_kind.is_none() {
            self.parse_loop_expr(expr)
        } else {
            let start_src_i = self.src_i;
            let is_lookahead_positive = expr.lookahead_kind == LookaheadKind::Positive;

            let result = self.parse_loop_expr(expr)?;
            self.src_i = start_src_i;

            if result.is_successful() == is_lookahead_positive {
                Ok(SyntaxParsingResult::Success(Vec::new()))
            } else {
                Ok(SyntaxParsingResult::Failure)
            }
        }
    }

    fn parse_loop_expr(&mut self, expr: &Box<RuleExpression>) -> ConsoleResult<SyntaxParsingResult> {
        let (min_count, max_count) = expr.loop_range.to_tuple();

        if max_count != -1 && min_count as isize > max_count {
            self.cons.borrow_mut().append_log(SyntaxParsingLog::LoopRangeIsInvalidOnParsing {
                loop_range: format!("{{{},{}}}", min_count, max_count),
            }.get_log());

            return Err(());
        }

        let mut children = Vec::<SyntaxNodeChild>::new();
        let mut loop_count = 0usize;

        while self.src_i < self.src_content.chars().count() {
            if loop_count > self.loop_limit {
                self.cons.borrow_mut().append_log(SyntaxParsingLog::RepetitionExceededLoopLimit {
                    loop_limit: self.loop_limit as usize
                }.get_log());

                return Err(());
            }

            let result = self.parse_raw_expr(expr)?;

            match result {
                SyntaxParsingResult::Success(node) => {
                    for each_node in node {
                        match each_node {
                            SyntaxNodeChild::Node(node) if node.as_ref().subelems.len() == 0 => (),
                            _ => children.push(each_node),
                        }
                    }

                    loop_count += 1;

                    if max_count != -1 && loop_count as isize == max_count {
                        return Ok(SyntaxParsingResult::Success(children));
                    }
                },
                SyntaxParsingResult::Failure => {
                    return if loop_count >= min_count && (max_count == -1 || loop_count as isize <= max_count) {
                        Ok(SyntaxParsingResult::Success(children))
                    } else {
                        Ok(SyntaxParsingResult::Failure)
                    }
                },
            }
        }

        return if loop_count >= min_count && (max_count == -1 || loop_count as isize <= max_count) {
            Ok(SyntaxParsingResult::Success(children))
        } else {
            Ok(SyntaxParsingResult::Failure)
        }
    }

    fn parse_raw_expr(&mut self, expr: &Box<RuleExpression>) -> ConsoleResult<SyntaxParsingResult> {
        if self.src_i >= self.src_content.chars().count() {
            return Ok(SyntaxParsingResult::Failure);
        }

        let mut is_skip_primitive_rule = false;

        let (ignore_skipping, allow_skipping_omission) = match &expr.kind {
            RuleExpressionKind::Id => (true, true),
            RuleExpressionKind::IdWithArgs { generics_args, template_args } => {
                match expr.value.as_ref() {
                    "NOSKIP" | "SKIP" => {
                        if generics_args.len() != 0 {
                            self.cons.borrow_mut().append_log(SyntaxParsingLog::ExpectedGenericsArgumentsProvided {
                                pos: expr.pos.clone(),
                                unexpected_len: generics_args.len(),
                                expected_len: 0,
                            }.get_log());

                            return Err(());
                        }

                        if template_args.len() != 0 {
                            self.cons.borrow_mut().append_log(SyntaxParsingLog::ExpectedTemplateArgumentsProvided {
                                pos: expr.pos.clone(),
                                unexpected_len: template_args.len(),
                                expected_len: 0,
                            }.get_log());

                            return Err(());
                        }

                        for each_tar_id in &self.skipping_stack {
                            if each_tar_id.len() == 0 {
                                self.cons.borrow_mut().append_log(SyntaxParsingLog::SkippingPrimitiveRuleSpecifiedWithoutSkipingRules {
                                    pos: expr.pos.clone(),
                                }.get_log());

                                return Err(());
                            }
                        }

                        self.is_skipped_by_noskip_once = true;

                        if expr.value == "NOSKIP" {
                            (true, true)
                        } else {
                            is_skip_primitive_rule = true;
                            (false, false)
                        }
                    },
                    _ => (false, true),
                }
            },
            _ => (false, true),
        };

        // note: 無限再帰防止のためスキッピング時は無視
        if !self.is_skipping_now && !ignore_skipping {
            if !self.is_skipped_by_noskip_once || is_skip_primitive_rule {
                self.is_skipping_now = true;

                for each_tar_id in self.skipping_stack[self.skipping_stack.len() - 1].clone() {
                    let result = self.parse_rule(&each_tar_id, &CharacterPosition::get_empty())?;

                    // note: スキッピングの省略を許さないかつ検査が失敗した場合
                    if !allow_skipping_omission && !result.is_successful() {
                        return Ok(SyntaxParsingResult::Failure);
                    }
                }

                self.is_skipping_now = false;
            } else {
                self.is_skipped_by_noskip_once = false;
            }
        }

        match &expr.kind {
            RuleExpressionKind::ArgId => {
                let mut generics_group = Option::<Rc<Box<RuleGroup>>>::None;

                for each_arg_map in self.arg_maps.iter().rev() {
                    match each_arg_map.generics_group_map.get(&expr.value) {
                        Some(v) => {
                            generics_group = Some(v.clone());
                            break;
                        },
                        None => (),
                    };
                }

                let result = match &generics_group {
                    Some(v) => self.parse_group(&ElementOrder::Sequential, &v),
                    None => {
                        self.cons.borrow_mut().append_log(SyntaxParsingLog::GenericsArgumentIDNotFound {
                            arg_id: expr.value.clone(),
                        }.get_log());

                        return Err(());
                    },
                };

                return if !expr.ast_reflection_style.is_reflectable() {
                    match &result {
                        Ok(v) => {
                            match v {
                                SyntaxParsingResult::Success(node_children) => {
                                    match node_children.get(0) {
                                        Some(each_node_child) => {
                                            let mut new_node_child = each_node_child.clone();
                                            new_node_child.set_ast_reflection_style(expr.ast_reflection_style.clone());
                                            Ok(SyntaxParsingResult::Success(vec![new_node_child]))
                                        },
                                        _ => result,
                                    }
                                },
                                SyntaxParsingResult::Failure => result,
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
                    return Ok(SyntaxParsingResult::Failure);
                }

                // note: Regex パターンが見つからない場合は新しく追加する
                let pattern = match self.regex_map.get(&expr.value) {
                    Some(v) => v,
                    None => {
                        let pattern = match Regex::new(&expr.value.clone()) {
                            Ok(v) => v,
                            Err(_) => {
                                self.cons.borrow_mut().append_log(SyntaxParsingLog::CharacterClassFormatIsInvalid {
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

                return if pattern.is_match(&tar_char) {
                    let new_leaf = SyntaxNodeChild::from_leaf_args(self.get_char_position(), tar_char.clone(), expr.ast_reflection_style.clone());
                    self.add_source_index_by_string(&tar_char);

                    Ok(SyntaxParsingResult::Success(vec![new_leaf]))
                } else {
                    Ok(SyntaxParsingResult::Failure)
                }
            },
            RuleExpressionKind::Id => self.parse_id_expr(expr),
            RuleExpressionKind::IdWithArgs { generics_args, template_args } => {
                let rule_id = &expr.value;
                let mut new_arg_map = ArgumentMap::new();

                match rule_id.as_str() {
                    "JOIN" => {
                        match generics_args.get(0) {
                            Some(tar_arg) if generics_args.len() == 1 => {
                                if template_args.len() != 0 {
                                    self.cons.borrow_mut().append_log(SyntaxParsingLog::ExpectedTemplateArgumentsProvided {
                                        pos: expr.pos.clone(),
                                        unexpected_len: template_args.len(),
                                        expected_len: 0,
                                    }.get_log());

                                    return Err(());
                                }

                                let result = self.parse_group(&ElementOrder::Sequential, tar_arg)?;

                                return match result {
                                    SyntaxParsingResult::Success(result_elems) => {
                                        let mut joined_str = String::new();

                                        for each_elem in result_elems {
                                            match each_elem {
                                                SyntaxNodeChild::Node(node) if node.as_ref().is_reflectable() => joined_str += &node.as_ref().join_child_leaf_values(),
                                                SyntaxNodeChild::Leaf(leaf) if leaf.as_ref().is_reflectable() => joined_str += &leaf.as_ref().value,
                                                _ => (),
                                            }
                                        }

                                        let new_leaf = SyntaxNodeChild::from_leaf_args(self.get_char_position(), joined_str, expr.ast_reflection_style.clone());
                                        Ok(SyntaxParsingResult::Success(vec![new_leaf]))
                                    },
                                    SyntaxParsingResult::Failure => Ok(SyntaxParsingResult::Failure),
                                };
                            },
                            _ => {
                                self.cons.borrow_mut().append_log(SyntaxParsingLog::ExpectedGenericsArgumentsProvided {
                                    pos: expr.pos.clone(),
                                    unexpected_len: generics_args.len(),
                                    expected_len: 1,
                                }.get_log());

                                return Err(());
                            },
                        }
                    },
                    // note: スキッピング省略時に処理済み
                    "NOSKIP" => return Ok(SyntaxParsingResult::Success(Vec::new())),
                    "SKIP" => return Ok(SyntaxParsingResult::Success(Vec::new())),
                    _ => {
                        if PRIMITIVE_RULE_NAMES.contains(&rule_id.as_str()) {
                            self.cons.borrow_mut().append_log(SyntaxParsingLog::PrimitiveRuleUncovered {
                                pos: expr.pos.clone(),
                                rule_name: rule_id.clone(),
                            }.get_log());

                            return Err(());
                        }
                    },
                }

                let (generics_arg_ids, template_arg_ids) = match self.rule_map.rule_map.get(rule_id) {
                    Some(rule) => (&rule.generics_arg_ids, &rule.template_arg_ids),
                    None => {
                        self.cons.borrow_mut().append_log(SyntaxParsingLog::RuleIDNotFoundOnParsing {
                            pos: expr.pos.clone(),
                            rule_id: rule_id.clone(),
                        }.get_log());

                        return Err(());
                    },
                };

                if generics_args.len() != generics_arg_ids.len() {
                    self.cons.borrow_mut().append_log(SyntaxParsingLog::ExpectedGenericsArgumentsProvided {
                        pos: expr.pos.clone(),
                        unexpected_len: generics_args.len(),
                        expected_len: generics_arg_ids.len(),
                    }.get_log());

                    return Err(());
                }

                if template_args.len() != template_arg_ids.len() {
                    self.cons.borrow_mut().append_log(SyntaxParsingLog::ExpectedTemplateArgumentsProvided {
                        pos: expr.pos.clone(),
                        unexpected_len: template_args.len(),
                        expected_len: template_arg_ids.len(),
                    }.get_log());

                    return Err(());
                }

                for i in 0..generics_arg_ids.len() {
                    let new_arg_id = match generics_arg_ids.get(i) {
                        Some(v) => v,
                        None => {
                            self.cons.borrow_mut().append_log(SyntaxParsingLog::GenericsArgumentIDNotFound {
                                arg_id: format!("[{}]", i),
                            }.get_log());

                            return Err(());
                        },
                    };

                    let new_arg_group = match generics_args.get(i) {
                        Some(v) => v,
                        None => {
                            self.cons.borrow_mut().append_log(SyntaxParsingLog::GenericsArgumentIDNotFound {
                                arg_id: format!("[{}]", i),
                            }.get_log());

                            return Err(());
                        }
                    };

                    new_arg_map.generics_group_map.insert(new_arg_id.clone(), new_arg_group.clone());
                }

                for i in 0..template_arg_ids.len() {
                    let new_arg_id = match template_arg_ids.get(i) {
                        Some(v) => v,
                        None => {
                            self.cons.borrow_mut().append_log(SyntaxParsingLog::TemplateArgumentIDNotFound {
                                arg_id: format!("[{}]", i),
                            }.get_log());

                            return Err(());
                        },
                    };

                    let new_arg_group = match template_args.get(i) {
                        Some(v) => v,
                        None => {
                            self.cons.borrow_mut().append_log(SyntaxParsingLog::TemplateArgumentIDNotFound {
                                arg_id: format!("[{}]", i),
                            }.get_log());

                            return Err(());
                        }
                    };

                    new_arg_map.template_group_map.insert(new_arg_id.clone(), new_arg_group.clone());
                }

                self.arg_maps.push(new_arg_map);
                let result = self.parse_id_expr(expr);
                self.arg_maps.pop();
                return result;
            },
            RuleExpressionKind::String => {
                if self.src_content.chars().count() < self.src_i + expr.value.chars().count() {
                    return Ok(SyntaxParsingResult::Failure);
                }

                return if self.substring_src_content(self.src_i, expr.value.chars().count()) == expr.value {
                    let new_leaf = SyntaxNodeChild::from_leaf_args(self.get_char_position(), expr.value.clone(), expr.ast_reflection_style.clone());
                    self.add_source_index_by_string(&expr.value);

                    Ok(SyntaxParsingResult::Success(vec![new_leaf]))
                } else {
                    Ok(SyntaxParsingResult::Failure)
                };
            },
            RuleExpressionKind::Wildcard => {
                if self.src_content.chars().count() < self.src_i + 1 {
                    return Ok(SyntaxParsingResult::Failure);
                }

                let expr_value = self.substring_src_content(self.src_i, 1);
                let new_leaf = SyntaxNodeChild::from_leaf_args(self.get_char_position(), expr_value.clone(), expr.ast_reflection_style.clone());
                self.add_source_index_by_string(&expr_value);

                return Ok(SyntaxParsingResult::Success(vec![new_leaf]));
            },
        }
    }

    fn parse_id_expr(&mut self, expr: &Box<RuleExpression>) -> ConsoleResult<SyntaxParsingResult> {
        let result = self.parse_rule(&expr.value, &expr.pos)?;

        return match result {
            SyntaxParsingResult::Success(node_children) => {
                let conv_node_children = match node_children.get(0).unwrap() {
                    SyntaxNodeChild::Node(node) => {
                        let sub_ast_reflection_style = match &expr.ast_reflection_style {
                            AstReflectionStyle::Reflection(elem_name) => {
                                let conv_elem_name = if elem_name == "" {
                                    expr.value.clone()
                                } else {
                                    elem_name.clone()
                                };

                                AstReflectionStyle::Reflection(conv_elem_name)
                            },
                            _ => expr.ast_reflection_style.clone(),
                        };

                        let new_node = SyntaxNodeChild::from_node_args(node.as_ref().subelems.clone(), sub_ast_reflection_style);

                        if expr.ast_reflection_style.is_expandable() {
                            match new_node {
                                SyntaxNodeChild::Node(tmp_node) => tmp_node.borrow().subelems,
                                _ => vec![new_node],
                            }
                        } else {
                            vec![new_node]
                        }
                    },
                    SyntaxNodeChild::Leaf(_) => node_children,
                };

                Ok(SyntaxParsingResult::Success(conv_node_children))
            },
            SyntaxParsingResult::Failure => {
                Ok(SyntaxParsingResult::Failure)
            },
        };
    }

    fn has_argument_in_group(group: &Rc<Box<RuleGroup>>) -> bool {
        for each_child in &group.subelems {
            let has_arg = match each_child {
                RuleElement::Group(each_group) => SyntaxParser::has_argument_in_group(each_group),
                RuleElement::Expression(each_expr) => each_expr.kind == RuleExpressionKind::ArgId,
            };

            if has_arg {
                return true;
            }
        }

        return false;
    }

    fn substring_src_content(&self, start_i: usize, len: usize) -> String {
        return self.src_content.chars().skip(start_i).take(len).collect::<String>();
    }

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
