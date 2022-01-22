use std::collections::*;
use std::rc::Rc;
use std::cell::RefCell;

use crate::*;
use crate::parser::*;
use crate::rule::*;
use crate::tree::*;

use colored::*;

use rustnutlib::*;
use rustnutlib::console::*;

macro_rules! block_map {
    ($($block_name:expr => $func_name:ident), *,) => {
        {
            let mut block_map = BlockMap::new();
            $(block_map.insert($block_name.to_string(), Box::new(FCPEGBlock::$func_name()));)*
            block_map
        }
    };
}

macro_rules! block {
    ($block_name:expr, $cmds:expr) => {
        Block::new($block_name.to_string(), $cmds)
    };
}

macro_rules! rule {
    ($rule_name:expr, $($choice:expr), *,) => {
        {
            let sub_elems = vec![$(
                match $choice {
                    RuleElement::Group(_) => $choice,
                    _ => panic!(),
                }
            )*];

            let mut root_group = Box::new(RuleGroup::new(RuleGroupKind::Choice));
            root_group.sub_elems = sub_elems;
            root_group.ast_reflection_style = ASTReflectionStyle::Expansion;

            let rule = Rule::new(CharacterPosition::get_empty(), $rule_name.to_string(), String::new(), Vec::new(), Vec::new(), root_group);
            BlockCommand::Define { pos: CharacterPosition::get_empty(), rule: rule }
        }
    };
}

macro_rules! start_cmd {
    ($file_alias_name:expr, $block_name:expr, $rule_name:expr) => {
        BlockCommand::Start { pos: CharacterPosition::get_empty(), file_alias_name: $file_alias_name.to_string(), block_name: $block_name.to_string(), rule_name: $rule_name.to_string() }
    };
}

macro_rules! choice {
    ($options:expr, $($sub_elem:expr), *,) => {
        {
            let mut group = RuleGroup::new(RuleGroupKind::Sequence);
            group.sub_elems = vec![$($sub_elem,)*];
            group.ast_reflection_style = ASTReflectionStyle::Reflection(String::new());

            for opt in $options {
                match opt {
                    "&" | "!" => group.lookahead_kind = RuleElementLookaheadKind::new(opt),
                    "?" | "*" | "+" => group.loop_range = RuleElementLoopRange::from_symbol(opt),
                    "#" => group.ast_reflection_style = ASTReflectionStyle::NoReflection,
                    "##" => group.ast_reflection_style = ASTReflectionStyle::Expansion,
                    ":" => group.kind = RuleGroupKind::Choice,
                    _ if opt.len() >= 2 && opt.starts_with("#") =>
                        group.ast_reflection_style = ASTReflectionStyle::Reflection(opt[1..].to_string()),
                    _ => panic!(),
                }
            }

            RuleElement::Group(Box::new(group))
        }
    };
}

macro_rules! expr {
    ($kind:ident, $value:expr $(, $option:expr) *) => {
        {
            let mut expr = RuleExpression::new(CharacterPosition::get_empty(), RuleExpressionKind::$kind, $value.to_string());

            let leaf_name = match RuleExpressionKind::$kind {
                RuleExpressionKind::Id => $value.to_string(),
                _ => String::new(),
            };

            expr.ast_reflection_style = ASTReflectionStyle::Reflection(leaf_name);

            $(
                match $option {
                    "&" | "!" => expr.lookahead_kind = RuleElementLookaheadKind::new($option),
                    "?" | "*" | "+" => expr.loop_range = RuleElementLoopRange::from_symbol($option),
                    "#" => expr.ast_reflection_style = ASTReflectionStyle::NoReflection,
                    "##" => expr.ast_reflection_style = ASTReflectionStyle::Expansion,
                    _ if $option.len() >= 2 && $option.starts_with("#") =>
                        expr.ast_reflection_style = ASTReflectionStyle::Reflection($option[1..].to_string()),
                    _ => panic!(),
                }
            )*

            RuleElement::Expression(Box::new(expr))
        }
    };
}

pub type BlockMap = HashMap<String, Box<Block>>;

pub enum BlockParseLog {
    BlockAliasNotFound { pos: CharacterPosition, block_alias_name: String },
    AttemptToAccessPrivateItem { pos: CharacterPosition, item_id: String },
    DuplicateBlockName { pos: CharacterPosition, block_name: String },
    DuplicateArgumentID { pos: CharacterPosition, arg_id: String },
    DuplicateRuleName { pos: CharacterPosition, rule_name: String },
    DuplicateStartCommand { pos: CharacterPosition },
    InternalError { msg: String },
    InvalidID { pos: CharacterPosition, id: String },
    InvalidLoopRangeItem { pos: CharacterPosition, msg: String },
    NamingRuleViolation { pos: CharacterPosition, id: String },
    NoStartCommandInMainBlock {},
    StartCommandOutsideMainBlock { pos: CharacterPosition },
    UnknownEscapeSequenceCharacter { pos: CharacterPosition },
    UnknownBlockID { pos: CharacterPosition, block_id: String },
    UnknownRuleID { pos: CharacterPosition, rule_id: String },
    UnnecessaryLoopRangeItem { pos: CharacterPosition, msg: String },
}

impl ConsoleLogger for BlockParseLog {
    fn get_log(&self) -> ConsoleLog {
        match self {
            BlockParseLog::BlockAliasNotFound { pos, block_alias_name } => log!(Error, format!("block alias '{}' not found", block_alias_name), format!("at:\t{}", pos)),
            BlockParseLog::AttemptToAccessPrivateItem { pos, item_id } => log!(Warning, "attempt to access private item", format!("at:\t{}", pos), format!("id:\t{}", item_id)),
            BlockParseLog::DuplicateBlockName { pos, block_name } => log!(Error, format!("duplicate block name '{}'", block_name), format!("at:\t{}", pos)),
            BlockParseLog::DuplicateArgumentID { pos, arg_id } => log!(Error, format!("duplicate argument id '{}'", arg_id), format!("at:\t{}", pos)),
            BlockParseLog::DuplicateRuleName { pos, rule_name } => log!(Error, format!("duplicate rule name '{}'", rule_name), format!("at:\t{}", pos)),
            BlockParseLog::DuplicateStartCommand { pos } => log!(Error, "duplicate start command", format!("at:\t{}", pos)),
            BlockParseLog::InternalError { msg } => log!(Error, format!("internal error: {}", msg)),
            BlockParseLog::InvalidID { pos, id } => log!(Error, format!("invalid id '{}'", id), format!("at:\t{}", pos)),
            BlockParseLog::InvalidLoopRangeItem { pos, msg } => log!(Error, format!("invalid loop range item"), format!("at:\t{}", pos), format!("{}", msg.bright_black())),
            BlockParseLog::NamingRuleViolation { pos, id } => log!(Warning, "naming rule violation", format!("at:\t{}", pos), format!("id:\t{}", id)),
            BlockParseLog::NoStartCommandInMainBlock {} => log!(Error, "no start command in main block"),
            BlockParseLog::StartCommandOutsideMainBlock { pos } => log!(Error, "start command outside main block", format!("at:\t{}", pos)),
            BlockParseLog::UnknownEscapeSequenceCharacter { pos } => log!(Error, "unknown escape sequence character", format!("at:\t{}", pos)),
            BlockParseLog::UnknownBlockID { pos, block_id } => log!(Error, format!("unknown block id '{}'", block_id), format!("at: {}", pos)),
            BlockParseLog::UnknownRuleID { pos, rule_id } => log!(Error, format!("unknown rule id '{}'", rule_id), format!("at: {}", pos)),
            BlockParseLog::UnnecessaryLoopRangeItem { pos, msg } => log!(Warning, format!("unnecessary loop range item"), format!("at:\t{}", pos), format!("{}", msg.bright_black())),
        }
    }
}

// note: プリミティブ関数の名前一覧
const PRIM_FUNC_NAMES: &[&'static str] = &["JOIN"];

pub struct BlockParser {
    cons: Rc<RefCell<Console>>,
    start_rule_id: Option<String>,
    file_alias_name: String,
    used_block_ids: Box<HashMap<String, CharacterPosition>>,
    used_rule_ids: Box<HashMap<String, CharacterPosition>>,
    block_name: String,
    block_alias_map: HashMap<String, String>,
    block_id_map: Vec::<String>,
    file_path: String,
    file_content: Box<String>,
}

impl BlockParser {
    // note: FileMap から最終的な RuleMap を取得する
    pub fn get_rule_map(cons: Rc<RefCell<Console>>, fcpeg_file_map: &mut FCPEGFileMap, enable_memoization: bool) -> ConsoleResult<Box<RuleMap>> {
        let block_map = FCPEGBlock::get_block_map();
        let rule_map = Box::new(RuleMap::new(vec![block_map], ".Syntax.FCPEG".to_string())?);

        let mut parser = SyntaxParser::new(cons.clone(), rule_map, enable_memoization)?;
        let mut block_maps = Vec::<BlockMap>::new();

        // note: HashMap<エイリアス名, ブロックマップ>
        let mut used_block_ids = Box::new(HashMap::<String, CharacterPosition>::new());
        let mut used_rule_ids = Box::new(HashMap::<String, CharacterPosition>::new());
        let mut block_id_map = Vec::<String>::new();

        let mut start_rule_id = Option::<String>::None;

        for (file_alias_name, fcpeg_file) in fcpeg_file_map.iter() {
            let mut block_parser = BlockParser {
                cons: cons.clone(),
                start_rule_id: None,
                file_alias_name: file_alias_name.clone(),
                used_block_ids: used_block_ids,
                used_rule_ids: used_rule_ids,
                block_name: String::new(),
                block_alias_map: HashMap::new(),
                block_id_map: block_id_map,
                file_path: fcpeg_file.file_path.clone(),
                file_content: fcpeg_file.file_content.clone(),
            };

            let tree = Box::new(block_parser.to_syntax_tree(&mut parser)?);
            block_maps.push(block_parser.to_block_map(tree)?);

            if block_parser.file_alias_name == "" {
                start_rule_id = block_parser.start_rule_id.clone();
            }

            used_block_ids = block_parser.used_block_ids;
            used_rule_ids = block_parser.used_rule_ids;
            block_id_map = block_parser.block_id_map;
        }

        let rule_map = match start_rule_id {
            Some(id) => Box::new(RuleMap::new(block_maps, id)?),
            None => {
                cons.borrow_mut().append_log(BlockParseLog::NoStartCommandInMainBlock {}.get_log());
                return Err(());
            },
        };

        let mut has_id_error = false;

        for (each_block_id, each_pos) in *used_block_ids {
            if !block_id_map.contains(&each_block_id) {
                cons.borrow_mut().append_log(BlockParseLog::UnknownBlockID {
                    pos: each_pos,
                    block_id: each_block_id,
                }.get_log());

                has_id_error = true;
            }
        }

        for (each_rule_id, each_pos) in *used_rule_ids {
            if !rule_map.rule_map.contains_key(&each_rule_id) {
                cons.borrow_mut().append_log(BlockParseLog::UnknownRuleID {
                    pos: each_pos,
                    rule_id: each_rule_id,
                }.get_log());

                has_id_error = true;
            }
        }

        return if has_id_error {
            Err(())
        } else {
            Ok(rule_map)
        };
    }

    fn to_syntax_tree(&mut self, parser: &mut SyntaxParser) -> ConsoleResult<SyntaxTree> {
        let tree = parser.parse(self.file_path.clone(), &self.file_content)?;
        return Ok(tree);
    }

    // note: FCPEG コードの構文木 → ブロックマップの変換
    fn to_block_map(&mut self, tree: Box<SyntaxTree>) -> ConsoleResult<BlockMap> {
        let mut block_map = BlockMap::new();
        let root = tree.get_child_ref();
        let block_nodes = match root.get_node(&self.cons)?.get_node_child_at(&self.cons, 0) {
            Ok(v) => v.get_reflectable_children(),
            Err(()) => {
                self.cons.borrow_mut().pop_log();
                return Ok(block_map)
            },
        };

        for each_block_elem in &block_nodes {
            let each_block_node = each_block_elem.get_node(&self.cons)?;
            let block_name_node = each_block_node.get_node_child_at(&self.cons, 0)?;
            let block_pos = block_name_node.get_position(&self.cons)?;
            self.block_name = block_name_node.join_child_leaf_values();

            if !BlockParser::is_pascal_case(&self.block_name) {
                self.cons.borrow_mut().append_log(BlockParseLog::NamingRuleViolation {
                    pos: block_pos.clone(),
                    id: self.block_name.clone(),
                }.get_log());
            }

            if block_map.contains_key(&self.block_name) {
                self.cons.borrow_mut().append_log(BlockParseLog::DuplicateBlockName {
                    pos: block_name_node.get_position(&self.cons)?,
                    block_name: self.block_name.clone(),
                }.get_log());

                return Err(());
            }

            let mut cmds = Vec::<BlockCommand>::new();
            let mut rule_names = Vec::<String>::new();

            match each_block_node.get_node_child_at(&self.cons, 1) {
                Ok(cmd_elems) => {
                    for each_cmd_elem in &cmd_elems.get_reflectable_children() {
                        let each_cmd_node = each_cmd_elem.get_node(&self.cons)?.get_node_child_at(&self.cons, 0)?;
                        let new_cmd = self.to_block_cmd(each_cmd_node)?;

                        // ルール名の重複チェック
                        match &new_cmd {
                            BlockCommand::Define { pos: _, rule } => {
                                if rule_names.contains(&rule.name) {
                                    self.cons.borrow_mut().append_log(BlockParseLog::DuplicateRuleName {
                                        pos: rule.pos.clone(),
                                        rule_name: rule.name.clone(),
                                    }.get_log());

                                    return Err(());
                                }

                                rule_names.push(rule.name.clone())
                            },
                            _ => (),
                        }

                        cmds.push(new_cmd);
                    }
                },
                Err(()) => self.cons.borrow_mut().pop_log(),
            }

            self.block_id_map.push(format!("{}.{}", self.file_alias_name, self.block_name));
            block_map.insert(self.block_name.clone(), Box::new(Block::new(self.block_name.clone(), cmds)));
            self.block_alias_map.clear();
        }

        block_map.insert(String::new(), Box::new(Block::new("Main".to_string(), Vec::new())));
        return Ok(block_map);
    }

    fn to_block_cmd(&mut self, cmd_node: &SyntaxNode) -> ConsoleResult<BlockCommand> {
        return match &cmd_node.ast_reflection_style {
            ASTReflectionStyle::Reflection(node_name) => match node_name.as_str() {
                ".Block.CommentCmd" => self.to_comment_cmd(cmd_node),
                ".Block.DefineCmd" => self.to_define_cmd(cmd_node),
                ".Block.StartCmd" => {
                    let start_cmd = self.to_start_cmd(cmd_node)?;

                    match start_cmd.clone() {
                        BlockCommand::Start { pos, file_alias_name, block_name, rule_name } => {
                            if self.block_name != "Main" {
                                self.cons.borrow_mut().append_log(BlockParseLog::StartCommandOutsideMainBlock {
                                    pos: pos,
                                }.get_log());

                                return Err(());
                            }

                            if self.file_alias_name == "" {
                                if self.start_rule_id.is_some() {
                                    self.cons.borrow_mut().append_log(BlockParseLog::DuplicateStartCommand {
                                        pos: pos,
                                    }.get_log());

                                    return Err(());
                                }

                                let rule_id = format!("{}.{}.{}", file_alias_name, block_name, rule_name);

                                if !self.used_rule_ids.contains_key(&rule_id) {
                                    self.used_rule_ids.insert(rule_id.clone(), pos.clone());
                                }

                                self.start_rule_id = Some(rule_id);
                            }
                        }
                        _ => (),
                    }

                    Ok(start_cmd)
                },
                ".Block.UseCmd" => {
                    let use_cmd = self.to_use_cmd(cmd_node)?;

                    match &use_cmd {
                        BlockCommand::Use { pos, file_alias_name, block_name, block_alias_name } => {
                            let block_id = format!("{}.{}", file_alias_name, block_name);
                            self.block_alias_map.insert(block_alias_name.clone(), block_id.clone());

                            if !self.used_block_ids.contains_key(&block_id) {
                                self.used_block_ids.insert(block_id, pos.clone());
                            }
                        },
                        _ => (),
                    }

                    Ok(use_cmd)
                },
                _ => {
                    self.cons.borrow_mut().append_log(SyntaxParseLog::UnknownNodeName {
                        uuid: cmd_node.uuid.clone(),
                        name: node_name.clone(),
                    }.get_log());

                    return Err(());
                },
            },
            _ => {
                self.cons.borrow_mut().append_log(SyntaxParseLog::UnknownNodeName {
                    uuid: cmd_node.uuid.clone(),
                    name: "[no name]".to_string(),
                }.get_log());

                return Err(());
            },
        };
    }

    fn to_comment_cmd(&mut self, cmd_node: &SyntaxNode) -> ConsoleResult<BlockCommand> {
        return Ok(BlockCommand::Comment { pos: cmd_node.get_position(&self.cons)?, value: cmd_node.join_child_leaf_values() });
    }

    fn to_define_cmd(&mut self, cmd_node: &SyntaxNode) -> ConsoleResult<BlockCommand> {
        let rule_name_node = cmd_node.get_node_child_at(&self.cons, 0)?;
        let rule_pos = rule_name_node.get_position(&self.cons)?;
        let rule_name = rule_name_node.join_child_leaf_values();

        if !BlockParser::is_pascal_case(&rule_name) {
            self.cons.borrow_mut().append_log(BlockParseLog::NamingRuleViolation {
                pos: rule_pos.clone(),
                id: rule_name.clone(),
            }.get_log());
        }

        let generics_args = match cmd_node.find_first_child_node(vec![".Block.DefineCmdGenericsIDs"]) {
            Some(generics_ids_node) => self.to_define_cmd_arg_ids(generics_ids_node)?,
            None => Vec::new(),
        };

        let func_args = match cmd_node.find_first_child_node(vec![".Block.DefineCmdFuncIDs"]) {
            Some(generics_ids_node) => self.to_define_cmd_arg_ids(generics_ids_node)?,
            None => Vec::new(),
        };

        let new_choice = match cmd_node.find_first_child_node(vec![".Rule.PureChoice"]) {
            Some(choice_node) => Box::new(self.to_rule_choice_elem(choice_node, &generics_args)?),
            None => {
                self.cons.borrow_mut().append_log(SyntaxParseLog::InternalError {
                    msg: "pure choice not found".to_string(),
                }.get_log());

                return Err(());
            },
        };
        let rule_id = format!("{}.{}.{}", self.file_alias_name, self.block_name, rule_name);
        let rule = Rule::new(rule_pos.clone(), rule_id, rule_name, generics_args, func_args, new_choice);
        return Ok(BlockCommand::Define { pos: rule_pos, rule: rule });
    }

    fn to_define_cmd_arg_ids(&mut self, cmd_node: &SyntaxNode) -> ConsoleResult<Vec<String>> {
        let mut args = Vec::<String>::new();

        for each_elem in &cmd_node.sub_elems {
            match each_elem {
                SyntaxNodeElement::Node(each_node) => {
                    if each_node.ast_reflection_style == ASTReflectionStyle::Reflection(".Rule.ArgID".to_string()) {
                        let new_arg = each_node.join_child_leaf_values();

                        if args.contains(&new_arg) {
                            self.cons.borrow_mut().append_log(BlockParseLog::DuplicateArgumentID {
                                pos: each_node.get_position(&self.cons)?,
                                arg_id: new_arg.clone(),
                            }.get_log());
                        }

                        args.push(new_arg);
                    }
                },
                _ => (),
            }
        }

        return Ok(args);
    }

    fn to_start_cmd(&mut self, cmd_node: &SyntaxNode) -> ConsoleResult<BlockCommand> {
        let raw_id_node = cmd_node.get_node_child_at(&self.cons, 0)?;
        let raw_id = self.to_chain_id(raw_id_node)?;
        let divided_raw_id = raw_id.split(".").collect::<Vec<&str>>();

        let cmd = match divided_raw_id.len() {
            2 => BlockCommand::Start { pos: cmd_node.get_position(&self.cons)?, file_alias_name: String::new(), block_name: divided_raw_id.get(0).unwrap().to_string(), rule_name: divided_raw_id.get(1).unwrap().to_string() },
            3 => BlockCommand::Start { pos: cmd_node.get_position(&self.cons)?, file_alias_name: divided_raw_id.get(0).unwrap().to_string(), block_name: divided_raw_id.get(1).unwrap().to_string(), rule_name: divided_raw_id.get(2).unwrap().to_string() },
            _ => {
                self.cons.borrow_mut().append_log(BlockParseLog::InvalidID {
                    pos: raw_id_node.get_node_child_at(&self.cons, 0)?.get_position(&self.cons)?,
                    id: raw_id,
                }.get_log());

                return Err(());
            },
        };

        return Ok(cmd);
    }

    fn to_use_cmd(&mut self, cmd_node: &SyntaxNode) -> ConsoleResult<BlockCommand> {
        let raw_id = self.to_chain_id(cmd_node.get_node_child_at(&self.cons, 0)?)?;
        let divided_raw_id = raw_id.split(".").collect::<Vec<&str>>();

        let (file_alias_name, block_name) = match divided_raw_id.len() {
            1 => (self.file_alias_name.clone(), divided_raw_id.get(0).unwrap().to_string()),
            2 => (divided_raw_id.get(0).unwrap().to_string(), divided_raw_id.get(1).unwrap().to_string()),
            _ => {
                self.cons.borrow_mut().append_log(SyntaxParseLog::InternalError {
                    msg: "invalid chain ID length on use command".to_string(),
                }.get_log());

                return Err(());
            },
        };

        let block_alias_name = match cmd_node.find_first_child_node(vec![".Block.UseCmdBlockAlias"]) {
            Some(v) => v.get_node_child_at(&self.cons, 0)?.join_child_leaf_values(),
            None => block_name.clone(),
        };

        return match divided_raw_id.len() {
            1 => Ok(BlockCommand::Use { pos: cmd_node.get_position(&self.cons)?, file_alias_name: file_alias_name, block_name: block_name, block_alias_name: block_alias_name }),
            2 => Ok(BlockCommand::Use { pos: cmd_node.get_position(&self.cons)?, file_alias_name: file_alias_name, block_name: block_name, block_alias_name: block_alias_name }),
            _ => {
                self.cons.borrow_mut().append_log(SyntaxParseLog::InternalError {
                    msg: "invalid chain ID length on use command".to_string(),
                }.get_log());

                return Err(());
            },
        };
    }

    // note: Seq を解析する
    fn to_seq_elem(&mut self, seq_node: &SyntaxNode, generics_args: &Vec<String>) -> ConsoleResult<RuleElement> {
        let mut children = Vec::<RuleElement>::new();

        // note: SeqElem ノードをループ
        for each_seq_elem_elem in &seq_node.get_reflectable_children() {
            let each_seq_elem_node = each_seq_elem_elem.get_node(&self.cons)?;

            // note: Lookahead ノード
            let lookahead_kind = match each_seq_elem_node.find_first_child_node(vec![".Rule.Lookahead"]) {
                Some(lookahead_node) => {
                    let kind_str = lookahead_node.get_leaf_child_at(&self.cons, 0)?.value.as_str();
                    let kind = RuleElementLookaheadKind::new(kind_str);

                    match kind {
                        RuleElementLookaheadKind::None => {
                            self.cons.borrow_mut().append_log(SyntaxParseLog::UnknownLookaheadKind {
                                uuid: lookahead_node.uuid,
                                kind: kind_str.to_string(),
                            }.get_log());

                            return Err(());
                        },
                        _ => kind,
                    }
                },
                None => RuleElementLookaheadKind::None,
            };

            // note: Loop ノード
            let loop_range = match each_seq_elem_node.find_first_child_node(vec![".Rule.Loop"]) {
                Some(v) => {
                    match v.get_child_at(&self.cons, 0)? {
                        SyntaxNodeElement::Node(range_node) => {
                            let range_node_pos = range_node.get_position(&self.cons)?;

                            let (min_num, is_min_num_specified) = match range_node.find_child_nodes(vec!["MinNum"]).get(0) {
                                Some(min_num_node) => {
                                    let min_str = min_num_node.join_child_leaf_values();

                                    match min_str.parse::<usize>() {
                                        Ok(v) => (v, true),
                                        Err(_) => {
                                            self.cons.borrow_mut().append_log(BlockParseLog::InvalidLoopRangeItem {
                                                pos: min_num_node.get_position(&self.cons)?,
                                                msg: format!("'{}' is too long or not a number", min_str),
                                            }.get_log());

                                            return Err(());
                                        },
                                    }
                                },
                                None => (0usize, false),
                            };

                            let (max_num, max_num_pos, is_max_num_specified) = match range_node.find_child_nodes(vec!["MaxNumGroup"]).get(0) {
                                Some(max_node_group) => {
                                    match max_node_group.find_child_nodes(vec!["MaxNum"]).get(0) {
                                        Some(max_num_node) => {
                                            // note: {n,m} の場合 (#MaxNumGroup 内に #MaxNum が存在する)
                                            let max_num_pos = max_num_node.get_position(&self.cons)?;
                                            let max_str = max_num_node.join_child_leaf_values();

                                            match max_str.parse::<usize>() {
                                                Ok(v) => (Infinitable::Finite(v), Some(max_num_pos), true),
                                                Err(_) => {
                                                    self.cons.borrow_mut().append_log(BlockParseLog::InvalidLoopRangeItem {
                                                        pos: max_num_pos,
                                                        msg: format!("'{}' is too long or not a number", max_str),
                                                    }.get_log());

                                                    return Err(());
                                                },
                                            }
                                        },
                                        None => (Infinitable::Infinite, None, false),
                                    }
                                },
                                // note: {n} の場合 (#MaxNumGroup が存在しない)
                                None => {
                                    if !is_min_num_specified {
                                        // note: 最小, 最大回数どちらも指定されていない場合
                                        self.cons.borrow_mut().append_log(BlockParseLog::InvalidLoopRangeItem {
                                            pos: range_node_pos,
                                            msg: format!("no number specified"),
                                        }.get_log());

                                        return Err(());
                                    }

                                    (Infinitable::Finite(min_num), None, false)
                                },
                            };

                            let raw_loop_range_txt = match &max_num {
                                Infinitable::Finite(v) => format!("{{{},{}}}", min_num, v),
                                Infinitable::Infinite => format!("{{{},}}", min_num),
                            };

                            match &max_num {
                                Infinitable::Finite(max_v) => {
                                    if min_num > *max_v {
                                        // note: 最小回数が最大回数より大きかった場合
                                        self.cons.borrow_mut().append_log(BlockParseLog::InvalidLoopRangeItem {
                                            pos: range_node_pos,
                                            msg: format!("min value '{}' is bigger than max value '{}'", min_num, max_v),
                                        }.get_log());

                                        return Err(());
                                    }

                                    if is_min_num_specified && !is_max_num_specified && min_num == 0 && *max_v == 0 {
                                        // note: {0} の場合
                                        self.cons.borrow_mut().append_log(BlockParseLog::InvalidLoopRangeItem {
                                            pos: range_node_pos,
                                            msg: format!("loop range '{{0}}' is invalid"),
                                        }.get_log());

                                        return Err(());
                                    } else if min_num == 1 && *max_v == 1 {
                                        if is_min_num_specified && !is_max_num_specified {
                                            // note: {1} の場合
                                            self.cons.borrow_mut().append_log(BlockParseLog::UnnecessaryLoopRangeItem {
                                                pos: range_node_pos,
                                                msg: format!("loop range '{{1}}' is unnecessary"),
                                            }.get_log());
                                        } else {
                                            // note: {1,1} の場合
                                            self.cons.borrow_mut().append_log(BlockParseLog::UnnecessaryLoopRangeItem {
                                                pos: range_node_pos,
                                                msg: format!("loop range '{{1,1}}' is unnecessary"),
                                            }.get_log());
                                        }
                                    } else if *max_v == 0 {
                                        // note: 最大回数に 0 が指定された場合
                                        self.cons.borrow_mut().append_log(BlockParseLog::InvalidLoopRangeItem {
                                            pos: max_num_pos.unwrap(),
                                            msg: format!("max number '{}' is invalid", min_num),
                                        }.get_log());

                                        return Err(());
                                    } else if is_max_num_specified && min_num == *max_v {
                                        // note: 最小回数と最大回数が同じだった場合
                                        self.cons.borrow_mut().append_log(BlockParseLog::UnnecessaryLoopRangeItem {
                                            pos: range_node_pos,
                                            msg: format!("modify '{}' to '{{{}}}'", raw_loop_range_txt, min_num),
                                        }.get_log());
                                    } else if is_min_num_specified && min_num == 0 {
                                        // note: 最小回数に 0 が指定された場合
                                        self.cons.borrow_mut().append_log(BlockParseLog::UnnecessaryLoopRangeItem {
                                            pos: range_node_pos,
                                            msg: format!("modify '{}' to '{{,{}}}'", raw_loop_range_txt, max_v),
                                        }.get_log());
                                    }
                                },
                                Infinitable::Infinite if is_min_num_specified && min_num == 0 => {
                                    // note: 最小回数に 0 が指定された場合
                                    self.cons.borrow_mut().append_log(BlockParseLog::UnnecessaryLoopRangeItem {
                                        pos: range_node_pos,
                                        msg: format!("modify '{}' to '{{,}}'", raw_loop_range_txt),
                                    }.get_log());
                                },
                                _ => (),
                            }

                            RuleElementLoopRange::new(min_num, max_num)
                        },
                        SyntaxNodeElement::Leaf(leaf) => {
                            let kind_str = leaf.value.as_str();

                            match kind_str {
                                "?" | "*" | "+" => RuleElementLoopRange::from_symbol(&leaf.value),
                                _ => {
                                    self.cons.borrow_mut().append_log(SyntaxParseLog::UnknownLookaheadKind {
                                        uuid: leaf.uuid.clone(),
                                        kind: kind_str.to_string(),
                                    }.get_log());

                                    return Err(());
                                },
                            }
                        }
                    }
                },
                None => RuleElementLoopRange::get_single_loop(),
            };

            // note: ASTReflectionStyle ノード
            // todo: 構成ファイルによって切り替える
            let ast_reflection_style = match each_seq_elem_node.find_first_child_node(vec![".Rule.ASTReflectionStyle"]) {
                Some(style_node) => {
                    match style_node.get_leaf_child_at(&self.cons, 0) {
                        Ok(leaf) => {
                            if leaf.value == "##" {
                                ASTReflectionStyle::Expansion
                            } else {
                                ASTReflectionStyle::Reflection(style_node.join_child_leaf_values())
                            }
                        },
                        Err(()) => {
                            self.cons.borrow_mut().pop_log();
                            ASTReflectionStyle::from_config(false, true, String::new())
                        },
                    }
                },
                None => ASTReflectionStyle::from_config(false, false, String::new()),
            };

            // note: Choice または Expr ノード
            let choice_or_expr_node = match each_seq_elem_node.find_first_child_node(vec![".Rule.Choice", ".Rule.Expr"]) {
                Some(v) => v,
                None => {
                    self.cons.borrow_mut().append_log(SyntaxParseLog::ChoiceOrExpressionChildNotMatched {
                        parent_uuid: each_seq_elem_node.uuid.clone(),
                    }.get_log());

                    return Err(());
                },
            };

            match &choice_or_expr_node.ast_reflection_style {
                ASTReflectionStyle::Reflection(name) => {
                    let new_elem = match name.as_str() {
                        ".Rule.Choice" => {
                            let mut new_choice = Box::new(self.to_rule_choice_elem(choice_or_expr_node.get_node_child_at(&self.cons, 0)?, generics_args)?);
                            new_choice.lookahead_kind = lookahead_kind;
                            new_choice.loop_range = loop_range;
                            new_choice.ast_reflection_style = ast_reflection_style;
                            RuleElement::Group(new_choice)
                        },
                        ".Rule.Expr" => {
                            let mut new_expr = Box::new(self.to_rule_expr_elem(choice_or_expr_node, generics_args)?);
                            new_expr.lookahead_kind = lookahead_kind;
                            new_expr.loop_range = loop_range;
                            new_expr.ast_reflection_style = ast_reflection_style;
                            RuleElement::Expression(new_expr)
                        },
                        _ => {
                            self.cons.borrow_mut().append_log(SyntaxParseLog::UnknownNodeName {
                                uuid: choice_or_expr_node.uuid.clone(),
                                name: name.to_string(),
                            }.get_log());

                            return Err(());
                        },
                    };

                    children.push(new_elem);
                },
                _ => {
                    self.cons.borrow_mut().append_log(SyntaxParseLog::UnknownNodeName {
                        uuid: choice_or_expr_node.uuid.clone(),
                        name: "[no name]".to_string(),
                    }.get_log());
    
                    return Err(());
                },
            };
        }

        let mut seq = Box::new(RuleGroup::new(RuleGroupKind::Sequence));
        seq.sub_elems = children;
        return Ok(RuleElement::Group(seq));
    }

    // note: Rule.PureChoice ノードの解析
    fn to_rule_choice_elem(&mut self, choice_node: &SyntaxNode, generics_args: &Vec<String>) -> ConsoleResult<RuleGroup> {
        let mut children = Vec::<RuleElement>::new();
        let mut group_kind = RuleGroupKind::Sequence;

        // Seq ノードをループ
        for seq_elem in &choice_node.get_reflectable_children() {
            match &seq_elem {
                SyntaxNodeElement::Node(node) => {
                    match &seq_elem.get_ast_reflection_style() {
                        ASTReflectionStyle::Reflection(name) => if name == ".Rule.Seq" {
                            let new_child = self.to_seq_elem(node, generics_args)?;
                            children.push(new_child);
                        },
                        _ => (),
                    }
                },
                SyntaxNodeElement::Leaf(leaf) => {
                    match leaf.value.as_str() {
                        ":" | "," => group_kind = RuleGroupKind::Choice,
                        _ => (),
                    }
                },
            }
        }

        let mut group = Box::new(RuleGroup::new(group_kind.clone()));
        group.sub_elems = children;

        let mut tmp_root_group = RuleGroup::new(RuleGroupKind::Sequence);
        tmp_root_group.sub_elems = vec![RuleElement::Group(group)];

        return Ok(tmp_root_group);
    }

    fn to_rule_expr_elem(&mut self, expr_node: &SyntaxNode, generics_args: &Vec<String>) -> ConsoleResult<RuleExpression> {
        let expr_child_node = expr_node.get_node_child_at(&self.cons, 0)?;
        let (pos, kind, value) = match &expr_child_node.ast_reflection_style {
            ASTReflectionStyle::Reflection(name) => {
                match name.as_str() {
                    ".Rule.ArgID" => (expr_child_node.get_position(&self.cons)?, RuleExpressionKind::ArgId, expr_child_node.join_child_leaf_values()),
                    ".Rule.CharClass" => (expr_child_node.get_position(&self.cons)?, RuleExpressionKind::CharClass, format!("[{}]", expr_child_node.join_child_leaf_values())),
                    ".Rule.Generics" | ".Rule.Func" => {
                        let mut args = Vec::<Box<RuleGroup>>::new();
                        for instant_pure_choice_node in expr_child_node.find_child_nodes(vec![".Rule.InstantPureChoice"]) {
                            args.push(Box::new(self.to_rule_choice_elem(instant_pure_choice_node, generics_args)?));
                        }

                        let parent_node = expr_child_node.get_node_child_at(&self.cons, 0)?.get_node_child_at(&self.cons, 0)?;
                        let pos = parent_node.get_position(&self.cons)?;

                        let generics = match name.as_str() {
                            ".Rule.Generics" => RuleExpressionKind::Generics(args),
                            ".Rule.Func" => RuleExpressionKind::Func(args),
                            _ => {
                                self.cons.borrow_mut().append_log(SyntaxParseLog::InternalError {
                                    msg: "invalid operation".to_string(),
                                }.get_log());

                                return Err(());
                            },
                        };

                        let raw_id = BlockParser::to_string_vec(&self.cons, expr_child_node.get_node_child_at(&self.cons, 0)?)?;
                        let joined_raw_id = raw_id.join(".");
                        let id = if name == ".Rule.Func" && PRIM_FUNC_NAMES.contains(&joined_raw_id.as_str()) {
                            joined_raw_id.clone()
                        } else {
                            match BlockParser::to_rule_id(&self.cons, &pos, &raw_id, &self.block_alias_map, &self.file_alias_name, &self.block_name) {
                                Ok(id) => {
                                    if !self.used_rule_ids.contains_key(&id) {
                                        self.used_rule_ids.insert(id.clone(), pos.clone());
                                    }

                                    id
                                },
                                Err(()) => return Err(()),
                            }
                        };

                        (pos, generics, id)
                    },
                    ".Rule.ID" => {
                        let chain_id_node = expr_child_node.get_node_child_at(&self.cons, 0)?;
                        let parent_node = chain_id_node.get_node_child_at(&self.cons, 0)?;
                        let pos = parent_node.get_position(&self.cons)?;

                        let id = match BlockParser::to_rule_id(&self.cons, &pos, &BlockParser::to_string_vec(&self.cons, chain_id_node)?, &self.block_alias_map, &self.file_alias_name, &self.block_name) {
                            Ok(id) => {
                                if !self.used_rule_ids.contains_key(&id) {
                                    self.used_rule_ids.insert(id.clone(), pos.clone());
                                }

                                id
                            },
                            Err(()) => return Err(()),
                        };

                        (pos, RuleExpressionKind::Id, id)
                    },
                    ".Rule.Str" => (expr_child_node.get_position(&self.cons)?, RuleExpressionKind::String, self.to_string_value(expr_child_node)?),
                    ".Rule.Wildcard" => (expr_child_node.get_position(&self.cons)?, RuleExpressionKind::Wildcard, ".".to_string()),
                    _ => {
                        self.cons.borrow_mut().append_log(SyntaxParseLog::InternalError {
                            msg: format!("unknown expression name '{}'", name),
                        }.get_log());

                        return Err(());
                    },
                }
            },
            _ => {
                self.cons.borrow_mut().append_log(SyntaxParseLog::InternalError {
                    msg: "invalid operation".to_string(),
                }.get_log());

                return Err(());
            },
        };

        let expr = RuleExpression::new(pos, kind, value);
        return Ok(expr);
    }

    fn to_string_vec(cons: &Rc<RefCell<Console>>, str_vec_node: &SyntaxNode) -> ConsoleResult<Vec<String>> {
        let mut str_vec = Vec::<String>::new();

        for str_elem in str_vec_node.get_reflectable_children() {
            str_vec.push(str_elem.get_node(cons)?.join_child_leaf_values());
        }

        return Ok(str_vec);
    }

    fn to_rule_id(cons: &Rc<RefCell<Console>>, pos: &CharacterPosition, id_tokens: &Vec<String>, block_alias_map: &HashMap<String, String>, file_alias_name: &String, block_name: &String) -> ConsoleResult<String> {
        let (new_id, _id_file_alias_name, id_block_name, id_rule_name) = match id_tokens.len() {
            1 => {
                let id_rule_name = id_tokens.get(0).unwrap();
                let new_id = format!("{}.{}.{}", file_alias_name, block_name, id_rule_name);

                (new_id, file_alias_name.as_str(), block_name, id_rule_name.clone())
            },
            2 => {
                let block_name = id_tokens.get(0).unwrap().to_string();
                let rule_name = id_tokens.get(1).unwrap().to_string();

                if block_alias_map.contains_key(&block_name.to_string()) {
                    let block_name = block_alias_map.get(&block_name.to_string()).unwrap();
                    // note: ブロック名がエイリアスである場合
                    let new_id = format!("{}.{}", block_name, rule_name);

                    (new_id, "", block_name, rule_name.clone())
                } else {
                    // note: ブロック名がエイリアスでない場合
                    cons.borrow_mut().append_log(BlockParseLog::BlockAliasNotFound {
                        pos: pos.clone(),
                        block_alias_name: block_name.to_string(),
                    }.get_log());

                    return Err(());
                }
            },
            3 => {
                let file_alias_name = id_tokens.get(0).unwrap();
                let block_name = id_tokens.get(1).unwrap();
                let rule_name = id_tokens.get(2).unwrap();
                let new_id = format!("{}.{}.{}", file_alias_name, block_name, rule_name);

                (new_id, file_alias_name.as_str(), block_name, rule_name.to_string())
            },
            _ => {
                cons.borrow_mut().append_log(BlockParseLog::InternalError {
                    msg: format!("invalid id expression"),
                }.get_log());

                return Err(());
            },
        };

        // note: プライベート規則の外部アクセスを除外
        // todo: プライベートブロックに対応
        // todo: 異なるファイルでの同ブロック名を除外
        if id_rule_name.starts_with("_") && *block_name != *id_block_name {
            cons.borrow_mut().append_log(BlockParseLog::AttemptToAccessPrivateItem {
                pos: pos.clone(),
                item_id: new_id.clone(),
            }.get_log());
        }

        return Ok(new_id);
    }

    fn to_string_value(&mut self, str_node: &SyntaxNode) -> ConsoleResult<String> {
        let mut s = String::new();

        for each_elem in &str_node.sub_elems {
            match each_elem {
                SyntaxNodeElement::Node(node) => {
                    match node.ast_reflection_style {
                        ASTReflectionStyle::Reflection(_) => {
                            s += match node.get_leaf_child_at(&self.cons, 0)?.value.as_str() {
                                "\\" => "\\",
                                "\"" => "\"",
                                "n" => "\n",
                                "t" => "\t",
                                "z" => "\0",
                                _ => {
                                    self.cons.borrow_mut().append_log(BlockParseLog::UnknownEscapeSequenceCharacter {
                                        pos: node.get_position(&self.cons)?,
                                    }.get_log());

                                    return Err(());
                                },
                            };
                        },
                        _ => (),
                    }
                },
                SyntaxNodeElement::Leaf(leaf) => {
                    match leaf.ast_reflection_style {
                        ASTReflectionStyle::Reflection(_) => s += leaf.value.as_ref(),
                        _ => (),
                    }
                },
            }
        }

        return Ok(s);
    }

    fn to_chain_id(&mut self, chain_id_node: &SyntaxNode) -> ConsoleResult<String> {
        let mut ids = Vec::<String>::new();

        for chain_id_elem in &chain_id_node.get_reflectable_children() {
            ids.push(chain_id_elem.get_node(&self.cons)?.join_child_leaf_values());
        }

        return Ok(ids.join("."));
    }

    // ret: 空文字の場合は false
    fn is_pascal_case(id: &String) -> bool {
        let mut id_chars = id.chars();

        return match id_chars.next() {
            Some(first_char) => {
                if first_char != '_' {
                    first_char.is_uppercase()
                } else {
                    match id_chars.next() {
                        Some(second_char) => second_char.is_uppercase(),
                        None => false,
                    }
                }
            },
            None => false,
        };
    }
}

struct FCPEGBlock {}

impl FCPEGBlock {
    pub fn get_block_map() -> BlockMap {
        return block_map!{
            "Main" => get_main_block,
            "Syntax" => get_syntax_block,
            "Symbol" => get_symbol_block,
            "Misc" => get_misc_block,
            "Block" => get_block_block,
            "Rule" => get_rule_block,
        };
    }

    fn get_main_block() -> Block {
        let start_cmd = start_cmd!("", "Syntax", "FCPEG");
        return block!("Main", vec![start_cmd]);
    }

    fn get_syntax_block() -> Block {
        // code: FCPEG <- Symbol.Space*# Symbol.LineEnd*# (Block.Block Symbol.LineEnd+#)* Symbol.LineEnd*# Symbol.Space*# Symbol.EOF#,
        let fcpeg_rule = rule!{
            ".Syntax.FCPEG",
            choice!{
                vec![],
                expr!(Id, ".Symbol.Space", "*", "#"),
                expr!(Id, ".Symbol.LineEnd", "*", "#"),
                choice!{
                    vec!["*"],
                    choice!{
                        vec![],
                        expr!(Id, ".Block.Block"),
                        expr!(Id, ".Symbol.LineEnd", "+", "#"),
                    },
                },
                expr!(Id, ".Symbol.LineEnd", "*", "#"),
                expr!(Id, ".Symbol.Space", "*", "#"),
                expr!(String, "\0", "#"),
            },
        };

        return block!(".Syntax", vec![fcpeg_rule]);
    }

    fn get_symbol_block() -> Block {
        // code: Space <- " ",
        let space_rule = rule!{
            ".Symbol.Space",
            choice!{
                vec![],
                expr!(String, " "),
            },
        };

        // code: LineEnd <- Space* "\n" Space*,
        let line_end_rule = rule!{
            ".Symbol.LineEnd",
            choice!{
                vec![],
                expr!(Id, ".Symbol.Space", "*"),
                expr!(String, "\n"),
                expr!(Id, ".Symbol.Space", "*"),
            },
        };

        // code: Div <- Space : "\n",
        let div_rule = rule!{
            ".Symbol.Div",
            choice!{
                vec![],
                choice!{
                    vec![":"],
                    choice!{
                        vec![],
                        expr!(Id, ".Symbol.Space"),
                    },
                    choice!{
                        vec![],
                        expr!(String, "\n"),
                    },
                },
            },
        };

        // note: CommaDiv <- Div* (",," LineEnd Div* : "," Space*),
        let comma_div_rule = rule!{
            ".Symbol.CommaDiv",
            choice!{
                vec![],
                expr!(Id, ".Symbol.Div", "*"),
                choice!{
                    vec![":"],
                    choice!{
                        vec![],
                        expr!(String, ",,"),
                        expr!(Id, ".Symbol.LineEnd"),
                        expr!(Id, ".Symbol.Div", "*"),
                    },
                    choice!{
                        vec![],
                        expr!(String, ","),
                        expr!(Id, ".Symbol.Space", "*"),
                    },
                },
            },
        };

        // code: EOF <- "\z",
        let eof_rule = rule!{
            ".Symbol.EOF",
            choice!{
                vec![],
                expr!(String, "\0", "#"),
            },
        };

        return block!(".Symbol", vec![space_rule, line_end_rule, div_rule, comma_div_rule, eof_rule]);
    }

    fn get_misc_block() -> Block {
        // code: SingleID <- [a-zA-Z_] [a-zA-Z0-9_]*,
        let single_id_rule = rule!{
            ".Misc.SingleID",
            choice!{
                vec![],
                expr!(CharClass, "[a-zA-Z_]"),
                expr!(CharClass, "[a-zA-Z0-9_]", "*"),
            },
        };

        // code: ChainID <- SingleID ("."# SingleID)*##,
        let chain_id_rule = rule!{
            ".Misc.ChainID",
            choice!{
                vec![],
                expr!(Id, ".Misc.SingleID"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec![],
                        expr!(String, ".", "#"),
                        expr!(Id, ".Misc.SingleID"),
                    },
                },
            },
        };

        return block!(".Misc", vec![single_id_rule, chain_id_rule]);
    }

    fn get_block_block() -> Block {
        // code: Block <- "["# Symbol.Div*# Misc.SingleID Symbol.Div*# "]"# Symbol.Div*# "{"# Symbol.LineEnd+# (Cmd Symbol.LineEnd+#)* "}"#,
        let block_rule = rule!{
            ".Block.Block",
            choice!{
                vec![],
                expr!(String, "[", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Misc.SingleID"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "]", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "{", "#"),
                expr!(Id, ".Symbol.LineEnd", "+", "#"),
                choice!{
                    vec!["*"],
                    choice!{
                        vec![],
                        expr!(Id, ".Block.Cmd"),
                        expr!(Id, ".Symbol.LineEnd", "+", "#"),
                    },
                },
                expr!(String, "}", "#"),
            },
        };

        // code: Cmd <- CommentCmd : DefineCmd : StartCmd : UseCmd,
        let cmd_rule = rule!{
            ".Block.Cmd",
            choice!{
                vec![":"],
                choice!{
                    vec![],
                    expr!(Id, ".Block.CommentCmd"),
                },
                choice!{
                    vec![],
                    expr!(Id, ".Block.DefineCmd"),
                },
                choice!{
                    vec![],
                    expr!(Id, ".Block.StartCmd"),
                },
                choice!{
                    vec![],
                    expr!(Id, ".Block.UseCmd"),
                },
            },
        };

        // code: CommentCmd <- "%"# (!"," . : ",,")*## ","#,
        let comment_rule = rule!{
            ".Block.CommentCmd",
            choice!{
                vec![],
                expr!(String, "%", "#"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec![":"],
                        choice!{
                            vec![],
                            expr!(String, ",", "!"),
                            expr!(Wildcard, "."),
                        },
                        choice!{
                            vec![],
                            expr!(String, ",,"),
                        },
                    },
                },
                expr!(String, ",", "#"),
            },
        };

        // code: DefineCmd <- Misc.SingleID DefineCmdGenericsIDs? DefineCmdFuncIDs? Symbol.Div*# "<-"# Symbol.Div*# Rule.PureChoice Symbol.Div*# ","#,
        let define_cmd_rule = rule!{
            ".Block.DefineCmd",
            choice!{
                vec![],
                expr!(Id, ".Misc.SingleID"),
                expr!(Id, ".Block.DefineCmdGenericsIDs", "?"),
                expr!(Id, ".Block.DefineCmdFuncIDs", "?"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "<-", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.PureChoice"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, ",", "#"),
            },
        };

        // code: DefineCmdGenericsIDs <- Symbol.Div*# "<"# Symbol.Div*# Rule.ArgID (Symbol.Div*# ","# Symbol.Div*# Rule.ArgID)*## Symbol.Div*# ">"# Symbol.Div*#,
        let define_cmd_generics_ids_rule = rule!{
            ".Block.DefineCmdGenericsIDs",
            choice!{
                vec![],
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "<", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.ArgID"),
                choice!{
                    vec!["*", "##"],
                    expr!(Id, ".Symbol.Div", "*", "#"),
                    expr!(String, ",", "#"),
                    expr!(Id, ".Symbol.Div", "*", "#"),
                    expr!(Id, ".Rule.ArgID"),
                },
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, ">", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
            },
        };

        // code: DefineCmdFuncIDs <- Symbol.Div*# "("# Symbol.Div*# Rule.ArgID (Symbol.Div*# ","# Symbol.Div*# Rule.ArgID)*## Symbol.Div*# ")"# Symbol.Div*#,
        let define_cmd_func_ids_rule = rule!{
            ".Block.DefineCmdFuncIDs",
            choice!{
                vec![],
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "(", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.ArgID"),
                choice!{
                    vec!["*", "##"],
                    expr!(Id, ".Symbol.Div", "*", "#"),
                    expr!(String, ",", "#"),
                    expr!(Id, ".Symbol.Div", "*", "#"),
                    expr!(Id, ".Rule.ArgID"),
                },
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, ")", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
            },
        };

        // code: StartCmd <- "+"# Symbol.Div*# "start"# Symbol.Div+# Misc.ChainID Symbol.Div*# ","#,
        let start_cmd_rule = rule!{
            ".Block.StartCmd",
            choice!{
                vec![],
                expr!(String, "+", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "start", "#"),
                expr!(Id, ".Symbol.Div", "+", "#"),
                expr!(Id, ".Misc.ChainID"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, ",", "#"),
            },
        };

        // code: UseCmd <- "+"# Symbol.Div*# "use"# Symbol.Div+# Misc.ChainID UseCmdBlockAlias? Symbol.Div*# ","#,
        let use_cmd_rule = rule!{
            ".Block.UseCmd",
            choice!{
                vec![],
                expr!(String, "+", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "use", "#"),
                expr!(Id, ".Symbol.Div", "+", "#"),
                expr!(Id, ".Misc.ChainID"),
                expr!(Id, ".Block.UseCmdBlockAlias", "?"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, ",", "#"),
            },
        };

        // code: UseCmdBlockAlias <- Symbol.Div+# "as" Symbol.Div+# Misc.SingleID,
        let use_cmd_block_alias_rule = rule!{
            ".Block.UseCmdBlockAlias",
            choice!{
                vec![],
                expr!(Id, ".Symbol.Div", "+", "#"),
                expr!(String, "as", "#"),
                expr!(Id, ".Symbol.Div", "+", "#"),
                expr!(Id, ".Misc.SingleID"),
            },
        };

        return block!(".Block", vec![block_rule, cmd_rule, comment_rule, define_cmd_rule, define_cmd_generics_ids_rule, define_cmd_func_ids_rule, start_cmd_rule, use_cmd_rule, use_cmd_block_alias_rule]);
    }

    fn get_rule_block() -> Block {
        // code: InstantPureChoice <- Seq ":" Symbol.Space# Seq)*##,
        let instant_pure_choice_rule = rule!{
            ".Rule.InstantPureChoice",
            choice!{
                vec![],
                expr!(Id, ".Rule.Seq"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec!["##"],
                        expr!(String, ":"),
                        expr!(Id, ".Symbol.Space", "#"),
                        expr!(Id, ".Rule.Seq"),
                    },
                },
            },
        };

        // code: PureChoice <- Seq ((Symbol.Div+# ":" Symbol.Div+# : ",")## Seq)*##,
        let pure_choice_rule = rule!{
            ".Rule.PureChoice",
            choice!{
                vec![],
                expr!(Id, ".Rule.Seq"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec!["##"],
                        choice!{
                            vec![":"],
                            choice!{
                                vec!["##"],
                                expr!(Id, ".Symbol.Div", "+", "#"),
                                expr!(String, ":"),
                                expr!(Id, ".Symbol.Div", "+", "#"),
                            },
                            choice!{
                                vec!["##"],
                                expr!(String, ","),
                                expr!(Id, ".Symbol.Space", "#"),
                            },
                        },
                        expr!(Id, ".Rule.Seq"),
                    },
                },
            },
        };

        // code: Choice <- "("# PureChoice ")"#,
        let choice_rule = rule!{
            ".Rule.Choice",
            choice!{
                vec![],
                expr!(String, "(", "#"),
                expr!(Id, ".Rule.PureChoice"),
                expr!(String, ")", "#"),
            },
        };

        // code: Seq <- SeqElem (Symbol.Div+# SeqElem)*##,
        let seq_rule = rule!{
            ".Rule.Seq",
            choice!{
                vec![],
                expr!(Id, ".Rule.SeqElem"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec![],
                        choice!{
                            vec![],
                            expr!(Id, ".Symbol.Div", "+", "#"),
                            expr!(Id, ".Rule.SeqElem"),
                        },
                    },
                },
            },
        };

        // code: SeqElem <- Lookahead? (Choice : Expr) Loop? RandomOrder? ASTReflectionStyle?,
        let seq_elem_rule = rule!{
            ".Rule.SeqElem",
            choice!{
                vec![],
                expr!(Id, ".Rule.Lookahead", "?"),
                choice!{
                    vec!["##"],
                    choice!{
                        vec![":"],
                        choice!{
                            vec![],
                            expr!(Id, ".Rule.Choice"),
                        },
                        choice!{
                            vec![],
                            expr!(Id, ".Rule.Expr"),
                        },
                    },
                },
                expr!(Id, ".Rule.Loop", "?"),
                expr!(Id, ".Rule.RandomOrder", "?"),
                expr!(Id, ".Rule.ASTReflectionStyle", "?"),
            },
        };

        // code: Expr <- Generics : ArgID : Func : ID : Str : CharClass : Wildcard,
        let expr_rule = rule!{
            ".Rule.Expr",
            choice!{
                vec![],
                choice!{
                    vec![":"],
                    choice!{
                        vec![],
                        expr!(Id, ".Rule.ArgID"),
                    },
                    choice!{
                        vec![],
                        expr!(Id, ".Rule.Generics"),
                    },
                    choice!{
                        vec![],
                        expr!(Id, ".Rule.Func"),
                    },
                    choice!{
                        vec![],
                        expr!(Id, ".Rule.ID"),
                    },
                    choice!{
                        vec![],
                        expr!(Id, ".Rule.Str"),
                    },
                    choice!{
                        vec![],
                        expr!(Id, ".Rule.CharClass"),
                    },
                    choice!{
                        vec![],
                        expr!(Id, ".Rule.Wildcard"),
                    },
                },
            },
        };

        // code: Lookahead <- "!" : "&",
        let lookahead_rule = rule!{
            ".Rule.Lookahead",
            choice!{
                vec![],
                choice!{
                    vec![":"],
                    choice!{
                        vec![],
                        expr!(String, "!"),
                    },
                    choice!{
                        vec![],
                        expr!(String, "&"),
                    },
                },
            },
        };

        // code: Loop <- "?" : "*" : "+" : LoopRange,
        let loop_rule = rule!{
            ".Rule.Loop",
            choice!{
                vec![],
                choice!{
                    vec![":"],
                    choice!{
                        vec![],
                        expr!(String, "?"),
                    },
                    choice!{
                        vec![],
                        expr!(String, "*"),
                    },
                    choice!{
                        vec![],
                        expr!(String, "+"),
                    },
                    choice!{
                        vec![],
                        expr!(Id, ".Rule.LoopRange"),
                    },
                },
            },
        };

        // code: LoopRange <- "{"# Symbol.Div*# Num?#MinNum (Symbol.CommaDiv# Num?#MaxNum)?#MaxNumGroup Symbol.Div*# "}"#,
        let loop_range_rule = rule!{
            ".Rule.LoopRange",
            choice!{
                vec![],
                expr!(String, "{", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.Num", "?", "#MinNum"),
                choice!{
                    vec!["?", "#MaxNumGroup"],
                    expr!(Id, ".Symbol.CommaDiv", "#"),
                    expr!(Id, ".Rule.Num", "?", "#MaxNum"),
                },
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "}", "#"),
            },
        };

        // expr: RandomOrder <- "^"# RandomOrderRange?,
        let random_order_rule = rule!{
            ".Rule.RandomOrder",
            choice!{
                vec![],
                expr!(String, "^", "#"),
                expr!(Id, ".Rule.RandomOrderRange", "?"),
            },
        };

        // code: RandomOrderRange <- "["# Symbol.Div*# Num? Symbol.CommaDiv# Num? Symbol.Div*# "]"#,
        let random_order_range_rule = rule!{
            ".Rule.RandomOrderRange",
            choice!{
                vec![],
                expr!(String, "[", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.Num", "?"),
                expr!(Id, ".Symbol.CommaDiv", "#"),
                expr!(Id, ".Rule.Num", "?"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "]", "#"),
            },
        };

        // code: ASTReflectionStyle <- "##" : "#"# Misc.SingleID?##,
        let ast_reflection_rule = rule!{
            ".Rule.ASTReflectionStyle",
            choice!{
                vec![],
                choice!{
                    vec![":"],
                    choice!{
                        vec![],
                        expr!(String, "##"),
                    },
                    choice!{
                        vec![],
                        expr!(String, "#", "#"),
                        expr!(Id, ".Misc.SingleID", "?", "##"),
                    },
                },
            },
        };

        // code: Num <- [0-9]+,
        let num_rule = rule!{
            ".Rule.Num",
            choice!{
                vec![],
                expr!(CharClass, "[0-9]+", "+"),
            },
        };

        // code: ID <- Misc.ChainID,
        let id_rule = rule!{
            ".Rule.ID",
            choice!{
                vec![],
                expr!(Id, ".Misc.ChainID"),
            },
        };

        // code: ArgID <- "$"# Misc.SingleID##,
        let arg_id_rule = rule!{
            ".Rule.ArgID",
            choice!{
                vec![],
                expr!(String, "$", "#"),
                expr!(Id, ".Misc.SingleID", "##"),
            },
        };

        // code: Generics <- Misc.ChainID "<"# Symbol.Div*# InstantPureChoice (Symbol.Div*# ","# Symbol.Div*# InstantPureChoice)*## Symbol.Div*# ">"#,
        let generics_rule = rule!{
            ".Rule.Generics",
            choice!{
                vec![],
                expr!(Id, ".Misc.ChainID"),
                expr!(String, "<", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.InstantPureChoice"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec!["##"],
                        expr!(Id, ".Symbol.Div", "*", "#"),
                        expr!(String, ",", "#"),
                        expr!(Id, ".Symbol.Div", "*", "#"),
                        expr!(Id, ".Rule.InstantPureChoice"),
                    },
                },
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, ">", "#"),
            },
        };

        // code: Func <- Misc.ChainID "("# Symbol.Div*# InstantPureChoice (Symbol.Div*# ","# Symbol.Div*# InstantPureChoice)*## Symbol.Div*# ")"#,
        let func_rule = rule!{
            ".Rule.Func",
            choice!{
                vec![],
                expr!(Id, ".Misc.ChainID"),
                expr!(String, "(", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.InstantPureChoice"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec!["##"],
                        expr!(Id, ".Symbol.Div", "*", "#"),
                        expr!(String, ",", "#"),
                        expr!(Id, ".Symbol.Div", "*", "#"),
                        expr!(Id, ".Rule.InstantPureChoice"),
                    },
                },
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, ")", "#"),
            },
        };

        // code: EscSeq <- "\\"# ("\\" : "\"" : "n" : "t" : "z")##,
        let esc_seq_rule = rule!{
            ".Rule.EscSeq",
            choice!{
                vec![],
                expr!(String, "\\", "#"),
                choice!{
                    vec!["##"],
                    choice!{
                        vec![":"],
                        choice!{
                            vec![],
                            expr!(String, "\\"),
                        },
                        choice!{
                            vec![],
                            expr!(String, "\""),
                        },
                        choice!{
                            vec![],
                            expr!(String, "n"),
                        },
                        choice!{
                            vec![],
                            expr!(String, "t"),
                        },
                        choice!{
                            vec![],
                            expr!(String, "z"),
                        },
                    },
                },
            },
        };

        // code: Str <- "\""# ((EscSeq : !(("\\" : "\"")) .))*## "\""#,
        let str_rule = rule!{
            ".Rule.Str",
            choice!{
                vec![],
                expr!(String, "\"", "#"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec![":"],
                        choice!{
                            vec![],
                            expr!(Id, ".Rule.EscSeq"),
                        },
                        choice!{
                            vec![],
                            choice!{
                                vec!["!"],
                                choice!{
                                    vec![":"],
                                    choice!{
                                        vec![],
                                        expr!(String, "\\"),
                                    },
                                    choice!{
                                        vec![],
                                        expr!(String, "\""),
                                    },
                                },
                            },
                            expr!(Wildcard, "."),
                        },
                    },
                },
                expr!(String, "\"", "#"),
            },
        };

        // code: CharClass <- "["# (!"[" !"]" !Symbol.LineEnd (("\\[" : "\\]" : "\\\\" : .))##)+## "]"#,
        let char_class_rule = rule!{
            ".Rule.CharClass",
            choice!{
                vec![],
                expr!(String, "[", "#"),
                choice!{
                    vec!["+", "##"],
                    expr!(String, "[", "!"),
                    expr!(String, "]", "!"),
                    expr!(Id, ".Symbol.LineEnd", "!"),
                    choice!{
                        vec!["##"],
                        choice!{
                            vec![":"],
                            choice!{
                                vec![],
                                expr!(String, "\\["),
                            },
                            choice!{
                                vec![],
                                expr!(String, "\\]"),
                            },
                            choice!{
                                vec![],
                                expr!(String, "\\\\"),
                            },
                            choice!{
                                vec![],
                                expr!(Wildcard, "."),
                            },
                        },
                    },
                },
                expr!(String, "]", "#"),
            },
        };

        // code: Wildcard <- ".",
        let wildcard_rule = rule!{
            ".Rule.Wildcard",
            choice!{
                vec![],
                expr!(String, "."),
            },
        };

        return block!(".Rule", vec![instant_pure_choice_rule, pure_choice_rule, choice_rule, seq_rule, seq_elem_rule, expr_rule, lookahead_rule, loop_rule, loop_range_rule, random_order_rule, random_order_range_rule, ast_reflection_rule, num_rule, id_rule, arg_id_rule, generics_rule, func_rule, esc_seq_rule, str_rule, char_class_rule, wildcard_rule]);
    }
}
