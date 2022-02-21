use std::collections::*;
use std::rc::Rc;
use std::cell::RefCell;
use std::sync::Arc;

use crate::*;
use crate::config::*;
use crate::cons::*;
use crate::parser::*;
use crate::rule::*;
use crate::tree::*;

use colored::*;

use cons_util::*;
use cons_util::cons::*;

use uuid::Uuid;

#[macro_export]
macro_rules! block_map {
    ($($block_name:expr => $block:expr), *,) => {
        {
            let mut block_map = BlockMap::new();
            $(block_map.insert($block_name.to_string(), Box::new($block));)*
            block_map
        }
    };
}

#[macro_export]
macro_rules! block {
    ($block_name:expr, $cmds:expr) => {
        Block::new($block_name.to_string(), $cmds, AttributeMap::new())
    };
}

#[macro_export]
macro_rules! rule {
    ($rule_id:expr, $($choice:expr), *,) => {
        {
            let subelems = vec![$(
                match $choice {
                    RuleElement::Group(_) => $choice,
                    _ => panic!(),
                }
            )*];

            let mut root_group = Box::new(RuleGroup::new(RuleGroupKind::Choice));
            root_group.subelems = subelems;
            root_group.ast_reflection_style = AstReflectionStyle::Expansion;

            let rule = Rule::new(CharacterPosition::get_empty(), $rule_id.to_string(), String::new(), Vec::new(), Vec::new(), Vec::new(), Rc::new(root_group));
            BlockCommand::Define { pos: CharacterPosition::get_empty(), rule: rule, attr_map: AttributeMap::new() }
        }
    };
}

#[macro_export]
macro_rules! group {
    ($options:expr, $($subelem:expr), *,) => {
        {
            let mut group = RuleGroup::new(RuleGroupKind::Sequence);
            group.subelems = vec![$($subelem,)*];
            group.ast_reflection_style = AstReflectionStyle::Reflection(String::new());

            for opt in $options {
                match opt {
                    "&" | "!" => group.lookahead_kind = LookaheadKind::new(opt),
                    "?" | "*" | "+" => group.loop_range = LoopRange::from(opt),
                    "#" => group.ast_reflection_style = AstReflectionStyle::NoReflection,
                    "##" => group.ast_reflection_style = AstReflectionStyle::Expansion,
                    ":" => group.kind = RuleGroupKind::Choice,
                    _ if opt.len() >= 2 && opt.starts_with("#") =>
                        group.ast_reflection_style = AstReflectionStyle::Reflection(opt[1..].to_string()),
                    _ => panic!(),
                }
            }

            RuleElement::Group(Rc::new(Box::new(group)))
        }
    };
}

#[macro_export]
macro_rules! expr {
    ($expr:expr) => {
        RuleElement::Expression(Box::new($expr))
    };

    ($kind:ident, $value:expr $(, $option:expr) *) => {
        {
            let mut expr = RuleExpression::new(CharacterPosition::get_empty(), RuleExpressionKind::$kind, $value.to_string());

            let leaf_name = match RuleExpressionKind::$kind {
                RuleExpressionKind::Id => $value.to_string(),
                RuleExpressionKind::IdWithArgs { generics_args:_, template_args: _ } => $value.to_string(),
                _ => String::new(),
            };

            expr.ast_reflection_style = AstReflectionStyle::Reflection(leaf_name);

            $(
                match $option {
                    "&" | "!" => expr.lookahead_kind = LookaheadKind::new($option),
                    "?" | "*" | "+" => expr.loop_range = LoopRange::from($option),
                    "#" => expr.ast_reflection_style = AstReflectionStyle::NoReflection,
                    "##" => expr.ast_reflection_style = AstReflectionStyle::Expansion,
                    _ if $option.len() >= 2 && $option.starts_with("#") =>
                        expr.ast_reflection_style = AstReflectionStyle::Reflection($option[1..].to_string()),
                    _ => panic!(),
                }
            )*

            RuleElement::Expression(Box::new(expr))
        }
    };
}

#[derive(Clone, PartialEq)]
pub enum BlockParsingLog {
    ArgumentIDIsDuplicate { pos: CharacterPosition, arg_id: String },
    AttemptToAccessPrivateItem { pos: CharacterPosition, item_id: String },
    AttributeNameIsDuplicate { pos: CharacterPosition, attr_name: String },
    AttributeNameNotFound { pos: CharacterPosition, attr_name: String },
    BlockAliasNameIsDuplicate { pos: CharacterPosition, alias_name: String },
    BlockAliasNameIsUnnecessary { pos: CharacterPosition, alias_name: String },
    BlockAliasNotFoundOrDeclaredUse { pos: CharacterPosition, alias_name: String },
    BlockAlreadyDeclaredUse { pos: CharacterPosition, block_name: String },
    BlockIDNotFound { pos: CharacterPosition, block_id: String },
    BlockNameIsDuplicate { pos: CharacterPosition, block_name: String },
    BlockNameIsViolatingNamingRule { pos: CharacterPosition, block_name: String },
    CannotSpecifyRandomOrderToExpression { pos: CharacterPosition },
    ChildNodeInNodeUnexpectedExpected { parent_node_uuid: Uuid, unexpected_name: String, expected_name: String },
    DeclaringStartOfMainRuleIsUnnecessary { pos: CharacterPosition },
    DeclaringUseOfSelfBlockIsUnnecessary { pos: CharacterPosition },
    EscapeSequenceCharacterIsUnknown { pos: CharacterPosition, escseq_char: String },
    IDIsInvalid { pos: CharacterPosition, id: String },
    LookaheadSymbolAtNodeIsUnknown { node_uuid: Uuid, symbol: String },
    LoopRangeIsInvalid { pos: CharacterPosition, loop_range: String, msg: String },
    LoopRangeIsUnrecommended { pos: CharacterPosition, loop_range: String, msg: String },
    LoopSymbolAtLeafIsUnknown { leaf_uuid: Uuid, symbol: String },
    NodeExpectedToBeName { node_uuid: Uuid, expected_name: String },
    NodeUnexpectedExpected { node_uuid: Uuid, unexpected_name: String, expected_name: String },
    RuleIDNotFound { pos: CharacterPosition, rule_id: String },
    RuleNameIsDuplicate { pos: CharacterPosition, rule_name: String },
    RuleNameIsViolatingNamingRule { pos: CharacterPosition, rule_name: String },
    SkippingNameNotFound { pos: CharacterPosition, skipping_name: String },
    StartCommandAlreadyDeclared { pos: CharacterPosition },
    StartCommandDeclaredOutsideMainBlock { pos: CharacterPosition },
}

impl ConsoleLogger for BlockParsingLog {
    fn get_log(&self) -> ConsoleLog {
        match self {
            BlockParsingLog::ArgumentIDIsDuplicate { pos, arg_id } => log!(Error, Translator::ArgumentIDIsDuplicate { arg_id: arg_id.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::AttemptToAccessPrivateItem { pos, item_id } => log!(Warning, Translator::AttemptToAccessPrivateItem { item_id: item_id.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::AttributeNameIsDuplicate { pos, attr_name } => log!(Error, Translator::AttributeNameIsDuplicate { attr_name: attr_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::AttributeNameNotFound { pos, attr_name } => log!(Error, Translator::AttributeNameNotFound { attr_name: attr_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::BlockAliasNameIsDuplicate { pos, alias_name } => log!(Error, Translator::BlockAliasNameIsDuplicate { alias_name: alias_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::BlockAliasNameIsUnnecessary { pos, alias_name } => log!(Warning, Translator::BlockAliasNameIsUnnecessary { alias_name: alias_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::BlockAliasNotFoundOrDeclaredUse { pos, alias_name } => log!(Error, Translator::BlockAliasNotFoundOrDeclaredUse { alias_name: alias_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::BlockAlreadyDeclaredUse { pos, block_name } => log!(Error, Translator::BlockAlreadyDeclaredUse { block_name: block_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::BlockIDNotFound { pos, block_id } => log!(Error, Translator::BlockIDNotFound { block_id: block_id.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::BlockNameIsDuplicate { pos, block_name } => log!(Error, Translator::BlockNameIsDuplicate { block_name: block_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::BlockNameIsViolatingNamingRule { pos, block_name } => log!(Warning, Translator::BlockNameIsViolatingNamingRule { block_name: block_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::CannotSpecifyRandomOrderToExpression { pos } => log!(Error, Translator::CannotSpecifyRandomOrderToExpression, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::ChildNodeInNodeUnexpectedExpected { parent_node_uuid, unexpected_name, expected_name } => log!(Error, Translator::ChildNodeInNodeUnexpectedExpected { parent_node_uuid: parent_node_uuid.clone(), unexpected_name: unexpected_name.clone(), expected_name: expected_name.clone() }),
            BlockParsingLog::DeclaringStartOfMainRuleIsUnnecessary { pos } => log!(Warning, Translator::DeclaringStartOfMainRuleIsUnnecessary, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::DeclaringUseOfSelfBlockIsUnnecessary { pos } => log!(Warning, Translator::DeclaringUseOfSelfBlockIsUnnecessary, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::EscapeSequenceCharacterIsUnknown { pos, escseq_char } => log!(Error, Translator::EscapeSequenceCharacterIsUnknown { escseq_char: escseq_char.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::IDIsInvalid { pos, id } => log!(Error, Translator::IDIsInvalid { id: id.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::LookaheadSymbolAtNodeIsUnknown { node_uuid, symbol } => log!(Error, Translator::LookaheadSymbolAtNodeIsUnknown { node_uuid: node_uuid.clone(), symbol: symbol.clone() }),
            BlockParsingLog::LoopRangeIsInvalid { pos, loop_range, msg } => log!(Error, Translator::LoopRangeIsInvalid { loop_range: loop_range.clone() }, Translator::AtDescription { pos: pos.clone() }, Translator::RawDescription { msg: msg.bright_black().to_string() }),
            BlockParsingLog::LoopRangeIsUnrecommended { pos, loop_range, msg } => log!(Warning, Translator::LoopRangeIsUnrecommended { loop_range: loop_range.clone() }, Translator::AtDescription { pos: pos.clone() }, Translator::RawDescription { msg: msg.bright_black().to_string() }),
            BlockParsingLog::LoopSymbolAtLeafIsUnknown { leaf_uuid, symbol } => log!(Error, Translator::LoopSymbolAtLeafIsUnknown { leaf_uuid: leaf_uuid.clone(), symbol: symbol.clone() }),
            BlockParsingLog::NodeExpectedToBeName { node_uuid, expected_name } => log!(Error, Translator::NodeExpectedToBeName { node_uuid: node_uuid.clone(), expected_name: expected_name.clone() }),
            BlockParsingLog::NodeUnexpectedExpected { node_uuid, unexpected_name, expected_name } => log!(Error, Translator::NodeUnexpectedExpected { node_uuid: node_uuid.clone(), unexpected_name: unexpected_name.clone(), expected_name: expected_name.clone() }),
            BlockParsingLog::RuleIDNotFound { pos, rule_id } => log!(Error, Translator::RuleIDNotFound { rule_id: rule_id.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::RuleNameIsDuplicate { pos, rule_name } => log!(Error, Translator::RuleNameIsDuplicate { rule_name: rule_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::RuleNameIsViolatingNamingRule { pos, rule_name } => log!(Warning, Translator::RuleNameIsViolatingNamingRule { rule_name: rule_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::SkippingNameNotFound { pos, skipping_name } => log!(Error, Translator::SkippingNameNotFound { skipping_name: skipping_name.clone() }, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::StartCommandAlreadyDeclared { pos } => log!(Warning, Translator::StartCommandAlreadyDeclared, Translator::AtDescription { pos: pos.clone() }),
            BlockParsingLog::StartCommandDeclaredOutsideMainBlock { pos } => log!(Error, Translator::StartCommandDeclaredOutsideMainBlock, Translator::AtDescription { pos: pos.clone() }),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct RawRange {
    pub min_num: usize,
    pub max_num: Infinitable<usize>,
    pub is_min_num_specified: bool,
    pub is_max_num_specified: bool,
    pub range_node_pos: CharacterPosition,
    pub min_num_pos: Option<CharacterPosition>,
    pub max_num_pos: Option<CharacterPosition>,
}

// note: プリミティブ規則名の一覧
pub const PRIMITIVE_RULE_NAMES: &[&'static str] = &["JOIN", "NOSKIP", "SKIP"];
// note: デフォルトの開始規則 ID
pub const DEFAULT_START_RULE_ID: &'static str = ".Main.Main";

pub struct BlockParser {
    cons: Rc<RefCell<Console>>,
    config: Rc<Configuration>,
    start_rule_id: Option<String>,
    file_alias_name: String,
    replaced_file_alias_names: Arc<HashMap<String, String>>,
    used_block_ids: Box<HashMap<String, CharacterPosition>>,
    used_rule_ids: Box<HashMap<String, CharacterPosition>>,
    block_name: String,
    block_attr_map: AttributeMap,
    // note: <ブロックエイリアス名, ブロック ID>
    block_alias_map: HashMap<String, String>,
    block_id_map: Vec::<String>,
    rule_name: String,
    file_path: String,
    file_content: Box<String>,
}

impl BlockParser {
    // note: FileMap から最終的な RuleMap を取得する
    pub fn get_rule_map(cons: Rc<RefCell<Console>>, fcpeg_file_map: &mut FCPEGFileMap, enable_memoization: bool) -> ConsoleResult<Arc<Box<RuleMap>>> {
        let block_map = FCPEGBlock::get_block_map();
        let rule_map = Arc::new(Box::new(RuleMap::new(vec![block_map], ".Main.Main".to_string())?));
        let mut block_maps = Vec::<BlockMap>::new();

        let mut used_block_ids = Box::new(HashMap::<String, CharacterPosition>::new());
        let mut used_rule_ids = Box::new(HashMap::<String, CharacterPosition>::new());
        let mut block_id_map = Vec::<String>::new();

        let mut start_rule_id = Option::<String>::None;

        for (file_alias_name, fcpeg_file) in fcpeg_file_map.iter() {
            let mut block_parser = BlockParser {
                cons: cons.clone(),
                config: fcpeg_file.config.clone(),
                start_rule_id: None,
                file_alias_name: file_alias_name.clone(),
                replaced_file_alias_names: fcpeg_file_map.replaced_file_alias_names.clone(),
                used_block_ids: used_block_ids,
                used_rule_ids: used_rule_ids,
                block_name: String::new(),
                block_attr_map: AttributeMap::new(),
                block_alias_map: HashMap::new(),
                block_id_map: block_id_map,
                rule_name: String::new(),
                file_path: fcpeg_file.file_path.clone(),
                file_content: fcpeg_file.file_content.clone(),
            };

            let tree = Box::new(block_parser.to_syntax_tree(rule_map.clone(), enable_memoization)?);
            block_maps.push(block_parser.to_block_map(tree)?);

            if block_parser.file_alias_name == "" {
                start_rule_id = block_parser.start_rule_id.clone();
            }

            used_block_ids = block_parser.used_block_ids;
            used_rule_ids = block_parser.used_rule_ids;
            block_id_map = block_parser.block_id_map;
        }

        let start_rule_id_str = match start_rule_id {
            Some(v) => v,
            None => DEFAULT_START_RULE_ID.to_string(),
        };

        let rule_map = Arc::new(Box::new(RuleMap::new(block_maps, start_rule_id_str)?));

        let mut has_id_error = false;

        for (each_block_id, each_pos) in *used_block_ids {
            if !block_id_map.contains(&each_block_id) {
                cons.borrow_mut().append_log(BlockParsingLog::BlockIDNotFound {
                    pos: each_pos,
                    block_id: each_block_id,
                }.get_log());

                has_id_error = true;
            }
        }

        for (each_rule_id, each_pos) in *used_rule_ids {
            if !rule_map.rule_map.contains_key(&each_rule_id) && !PRIMITIVE_RULE_NAMES.contains(&each_rule_id.as_str()) {
                cons.borrow_mut().append_log(BlockParsingLog::RuleIDNotFound {
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

    fn to_syntax_tree(&mut self, rule_map: Arc<Box<RuleMap>>, enable_memoization: bool) -> ConsoleResult<SyntaxTree> {
        let tree = SyntaxParser::parse(self.cons.clone(), rule_map, self.file_path.clone(), self.file_content.clone(), enable_memoization).0?;
        return Ok(tree);
    }

    // note: FCPEG コードの構文木 → ブロックマップの変換
    fn to_block_map(&mut self, tree: Box<SyntaxTree>) -> ConsoleResult<BlockMap> {
        let mut block_map = BlockMap::new();
        let root = tree.get_child_ref();

        let block_nodes = match root.get_node(self.cons.clone())?.get_node_child_at(self.cons.clone(), 0) {
            Ok(v) => v.get_reflectable_children(),
            Err(()) => {
                self.cons.borrow_mut().pop_log();
                return Ok(block_map)
            },
        };

        for each_block_elem in &block_nodes {
            let each_block_node = each_block_elem.get_node(self.cons.clone())?;

            let attr_map = match each_block_node.find_first_child_node(vec![".Attr.AttrList"]) {
                Some(attr_list_node) => self.to_attribute_map(attr_list_node)?,
                None => AttributeMap::new(),
            };

            let block_name = {
                let id_node_name = ".Misc.SingleID";

                let (block_name, block_pos) = match each_block_node.find_first_child_node(vec![id_node_name]) {
                    Some(id_node) => (id_node.join_child_leaf_values(), id_node.get_position(Some(self.cons.clone()))?),
                    None => {
                        self.cons.borrow_mut().append_log(BlockParsingLog::NodeExpectedToBeName {
                            node_uuid: each_block_node.uuid.clone(),
                            expected_name: format!("'{}'", id_node_name),
                        }.get_log());

                        return Err(());
                    },
                };

                // note: 命名規則チェック
                if !BlockParser::is_pascal_case(&block_name) {
                    self.cons.borrow_mut().append_log(BlockParsingLog::BlockNameIsViolatingNamingRule {
                        pos: block_pos.clone(),
                        block_name: block_name.clone(),
                    }.get_log());
                }

                // note: ブロック名重複チェック
                if block_map.contains_key(&block_name) {
                    self.cons.borrow_mut().append_log(BlockParsingLog::BlockNameIsDuplicate {
                        pos: block_pos.clone(),
                        block_name: block_name.clone(),
                    }.get_log());

                    return Err(());
                }

                block_name
            };

            self.block_attr_map = attr_map.clone();
            self.block_name = block_name;

            let cmds = {
                let mut cmds = Vec::<BlockCommand>::new();
                // note: 規則名の重複チェック用
                let mut rule_names = Vec::<String>::new();

                // note: cmds に命令を追加
                for each_cmd_node in &each_block_node.find_child_nodes(vec![".Block.Cmd"]) {
                    let new_cmd = self.to_block_cmd(each_cmd_node.get_node_child_at(self.cons.clone(), 0)?)?;

                    // note: 規則名の重複チェック
                    match &new_cmd {
                        BlockCommand::Define { pos: _, rule, attr_map: _ } => {
                            if rule_names.contains(&rule.name) {
                                self.cons.borrow_mut().append_log(BlockParsingLog::RuleNameIsDuplicate {
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

                cmds
            };

            self.block_id_map.push(BlockParser::to_block_id_from_elements(&self.replaced_file_alias_names, &self.file_alias_name, &self.block_name));
            block_map.insert(self.block_name.clone(), Box::new(Block::new(self.block_name.clone(), cmds, attr_map)));
            // note: ファイルを抜けるためクリア
            self.block_alias_map.clear();
        }

        // fix: これいる？
        // block_map.insert(String::new(), Box::new(Block::new("Main".to_string(), Vec::new(), AttributeMap::new())));
        return Ok(block_map);
    }

    fn to_block_cmd(&mut self, cmd_node: &SyntaxNode) -> ConsoleResult<BlockCommand> {
        return match &cmd_node.ast_reflection_style {
            AstReflectionStyle::Reflection(node_name) => match node_name.as_str() {
                ".Block.CommentCmd" => self.to_comment_cmd(cmd_node),
                ".Block.DefineCmd" => self.to_define_cmd(cmd_node),
                ".Block.StartCmd" => {
                    let start_cmd = self.to_start_cmd(cmd_node)?;

                    match start_cmd.clone() {
                        BlockCommand::Start { pos, file_alias_name, block_name, rule_name } => {
                            if self.block_name != "Main" {
                                self.cons.borrow_mut().append_log(BlockParsingLog::StartCommandDeclaredOutsideMainBlock {
                                    pos: pos,
                                }.get_log());

                                return Err(());
                            }

                            if self.file_alias_name == "" {
                                if self.start_rule_id.is_some() {
                                    self.cons.borrow_mut().append_log(BlockParsingLog::StartCommandAlreadyDeclared {
                                        pos: pos,
                                    }.get_log());

                                    return Err(());
                                }

                                let rule_id = BlockParser::to_rule_id_from_elements(&self.replaced_file_alias_names, &file_alias_name, &block_name, &rule_name);

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
                            let block_id = BlockParser::to_block_id_from_elements(&self.replaced_file_alias_names, file_alias_name, block_name);
                            let used_from = BlockParser::to_block_id_from_elements(&self.replaced_file_alias_names, &self.file_alias_name, &self.block_name);

                            // note: ブロック ID が自身のブロックと同じであれば警告
                            if block_id == used_from {
                                self.cons.borrow_mut().append_log(BlockParsingLog::DeclaringUseOfSelfBlockIsUnnecessary {
                                    pos: pos.clone(),
                                }.get_log());
                            }

                            let mut disable_map_insert = false;

                            // note: ブロック ID / ブロックエイリアスがすでに use されていれば警告
                            for (each_alias_name, each_id) in &self.block_alias_map {
                                if *each_id == block_id {
                                    self.cons.borrow_mut().append_log(BlockParsingLog::BlockAlreadyDeclaredUse {
                                        pos: pos.clone(),
                                        block_name: block_name.clone(),
                                    }.get_log());

                                    disable_map_insert = true;
                                } else if each_alias_name == block_alias_name {
                                    self.cons.borrow_mut().append_log(BlockParsingLog::BlockAliasNameIsDuplicate {
                                        pos: pos.clone(),
                                        alias_name: block_alias_name.clone(),
                                    }.get_log());

                                    disable_map_insert = true;
                                }
                            }

                            // note: 先に use されたほうが優先; 警告が出れば弾く
                            if !disable_map_insert {
                                if !self.used_block_ids.contains_key(&block_id) {
                                    self.used_block_ids.insert(block_id.clone(), pos.clone());
                                }

                                self.block_alias_map.insert(block_alias_name.clone(), block_id);
                            }
                        },
                        _ => (),
                    }

                    Ok(use_cmd)
                },
                _ => {
                    self.cons.borrow_mut().append_log(BlockParsingLog::NodeUnexpectedExpected {
                        node_uuid: cmd_node.uuid.clone(),
                        unexpected_name: format!("'{}'", node_name),
                        expected_name: "block command node name".to_string(),
                    }.get_log());

                    return Err(());
                },
            },
            _ => {
                self.cons.borrow_mut().append_log(BlockParsingLog::NodeUnexpectedExpected {
                    node_uuid: cmd_node.uuid.clone(),
                    unexpected_name: "no name".to_string(),
                    expected_name: "block command node name".to_string(),
                }.get_log());

                return Err(());
            },
        };
    }

    fn to_comment_cmd(&mut self, cmd_node: &SyntaxNode) -> ConsoleResult<BlockCommand> {
        return Ok(BlockCommand::Comment { pos: cmd_node.get_position(Some(self.cons.clone()))?, value: cmd_node.join_child_leaf_values() });
    }

    fn to_define_cmd(&mut self, cmd_node: &SyntaxNode) -> ConsoleResult<BlockCommand> {
        let id_node_name = ".Misc.SingleID";

        let (rule_name, rule_pos) = match cmd_node.find_first_child_node(vec![id_node_name]) {
            Some(id_node) => {
                let rule_name = id_node.join_child_leaf_values();
                self.rule_name = rule_name.clone();
                (rule_name, id_node.get_position(Some(self.cons.clone()))?)
            },
            None => {
                self.cons.borrow_mut().append_log(BlockParsingLog::NodeExpectedToBeName {
                    node_uuid: cmd_node.uuid.clone(),
                    expected_name: format!("'{}'", id_node_name),
                }.get_log());

                return Err(());
            }
        };

        // note: 規則名の命名規則チェック
        if !BlockParser::is_pascal_case(&rule_name) {
            self.cons.borrow_mut().append_log(BlockParsingLog::RuleNameIsViolatingNamingRule {
                pos: rule_pos.clone(),
                rule_name: rule_name.clone(),
            }.get_log());
        }

        let attr_map = match cmd_node.find_first_child_node(vec![".Attr.AttrList"]) {
            Some(attr_list_node) => self.to_attribute_map(attr_list_node)?,
            None => AttributeMap::new(),
        };

        let skipping_tar_ids = {
            let mut skipping_tar_ids = Vec::<String>::new();

            BlockParser::analyze_attribute(self.cons.clone(), &self.config, &self.block_attr_map, &mut skipping_tar_ids)?;
            BlockParser::analyze_attribute(self.cons.clone(), &self.config, &attr_map, &mut skipping_tar_ids)?;

            skipping_tar_ids
        };

        let generics_arg_ids = match cmd_node.find_first_child_node(vec![".Block.DefineCmdGenerics"]) {
            Some(generics_ids_node) => self.to_define_cmd_arg_ids(generics_ids_node, rule_name.clone())?,
            None => Vec::new(),
        };

        let template_arg_ids = match cmd_node.find_first_child_node(vec![".Block.DefineCmdTemplate"]) {
            Some(generics_ids_node) => self.to_define_cmd_arg_ids(generics_ids_node, rule_name.clone())?,
            None => Vec::new(),
        };

        let new_choice = match cmd_node.find_first_child_node(vec![".Rule.PureChoice"]) {
            Some(choice_node) => Box::new(self.to_rule_choice_elem(choice_node, &generics_arg_ids)?),
            None => {
                self.cons.borrow_mut().append_log(BlockParsingLog::ChildNodeInNodeUnexpectedExpected {
                    parent_node_uuid: cmd_node.uuid.clone(),
                    unexpected_name: "unknown".to_string(),
                    expected_name: "pure choice node".to_string(),
                }.get_log());

                return Err(());
            },
        };

        let rule_id = BlockParser::to_rule_id_from_elements(&self.replaced_file_alias_names, &self.file_alias_name, &self.block_name, &rule_name);
        let rule = Rule::new(rule_pos.clone(), rule_id, rule_name, generics_arg_ids, template_arg_ids, skipping_tar_ids, Rc::new(new_choice));
        return Ok(BlockCommand::Define { pos: rule_pos, rule: rule, attr_map: attr_map });
    }

    fn to_define_cmd_arg_ids(&mut self, cmd_node: &SyntaxNode, rule_name: String) -> ConsoleResult<Vec<String>> {
        let mut args = Vec::<String>::new();

        for each_elem in &cmd_node.subelems {
            match each_elem {
                SyntaxNodeChild::Node(each_node) => {
                    if each_node.as_ref().ast_reflection_style == AstReflectionStyle::Reflection(".Rule.ArgID".to_string()) {
                        let new_arg_id = each_node.as_ref().join_child_leaf_values();

                        if args.contains(&new_arg_id) {
                            self.cons.borrow_mut().append_log(BlockParsingLog::ArgumentIDIsDuplicate {
                                pos: each_node.as_ref().get_position(Some(self.cons.clone()))?,
                                arg_id: new_arg_id.clone(),
                            }.get_log());
                        }

                        args.push(self.to_argument_id(rule_name.clone(), new_arg_id));
                    }
                },
                _ => (),
            }
        }

        return Ok(args);
    }

    fn to_start_cmd(&mut self, cmd_node: &SyntaxNode) -> ConsoleResult<BlockCommand> {
        let raw_id_node = cmd_node.get_node_child_at(self.cons.clone(), 0)?;
        let raw_id = self.to_chain_id(raw_id_node)?;
        let divided_raw_id = raw_id.split(".").collect::<Vec<&str>>();

        let (file_alias_name, block_name, rule_name) = match divided_raw_id.len() {
            2 => (String::new(), divided_raw_id.get(0).unwrap().to_string(), divided_raw_id.get(1).unwrap().to_string()),
            3 => (divided_raw_id.get(0).unwrap().to_string(), divided_raw_id.get(1).unwrap().to_string(), divided_raw_id.get(2).unwrap().to_string()),
            _ => {
                let pos = raw_id_node.get_position(Some(self.cons.clone()))?;

                self.cons.clone().borrow_mut().append_log(BlockParsingLog::IDIsInvalid {
                    pos: pos,
                    id: raw_id,
                }.get_log());

                return Err(());
            },
        };

        // note: ブロック ID がデフォルトと同じであれば警告
        if DEFAULT_START_RULE_ID == BlockParser::to_rule_id_from_elements(&self.replaced_file_alias_names, &file_alias_name, &block_name, &rule_name) {
            self.cons.borrow_mut().append_log(BlockParsingLog::DeclaringStartOfMainRuleIsUnnecessary {
                pos: cmd_node.get_position(Some(self.cons.clone()))?,
            }.get_log());
        }

        let cmd = BlockCommand::Start {
            pos: cmd_node.get_position(Some(self.cons.clone()))?,
            file_alias_name: file_alias_name,
            block_name: block_name,
            rule_name: rule_name,
        };

        return Ok(cmd);
    }

    fn to_use_cmd(&mut self, cmd_node: &SyntaxNode) -> ConsoleResult<BlockCommand> {
        let raw_id_node = cmd_node.get_node_child_at(self.cons.clone(), 0)?;
        let raw_id = self.to_chain_id(raw_id_node)?;
        let divided_raw_id = raw_id.split(".").collect::<Vec<&str>>();

        let (file_alias_name, block_name) = match divided_raw_id.len() {
            1 => (self.file_alias_name.clone(), divided_raw_id.get(0).unwrap().to_string()),
            2 => (divided_raw_id.get(0).unwrap().to_string(), divided_raw_id.get(1).unwrap().to_string()),
            _ => {
                self.cons.borrow_mut().append_log(BlockParsingLog::IDIsInvalid {
                    pos: raw_id_node.get_position(Some(self.cons.clone()))?,
                    id: raw_id,
                }.get_log());

                return Err(());
            },
        };

        let block_alias_name = match cmd_node.find_first_child_node(vec![".Block.UseCmdBlockAlias"]) {
            Some(block_alias_node) => {
                let block_alias_name = block_alias_node.get_node_child_at(self.cons.clone(), 0)?.join_child_leaf_values();

                // note: ブロック名とエイリアス名が同じであれば警告
                if block_name == block_alias_name {
                    self.cons.borrow_mut().append_log(BlockParsingLog::BlockAliasNameIsUnnecessary {
                        pos: block_alias_node.get_position(Some(self.cons.clone()))?,
                        alias_name: block_name.clone(),
                    }.get_log());
                }

                block_alias_name
            },
            None => block_name.clone(),
        };

        return match divided_raw_id.len() {
            1 | 2 => Ok(BlockCommand::Use { pos: cmd_node.get_position(Some(self.cons.clone()))?, file_alias_name: file_alias_name, block_name: block_name, block_alias_name: block_alias_name }),
            _ => {
                self.cons.borrow_mut().append_log(BlockParsingLog::IDIsInvalid {
                    pos: raw_id_node.get_position(Some(self.cons.clone()))?,
                    id: raw_id,
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
            let each_seq_elem_node = each_seq_elem_elem.get_node(self.cons.clone())?;

            // note: Lookahead ノード
            let lookahead_kind = match each_seq_elem_node.find_first_child_node(vec![".Rule.Lookahead"]) {
                Some(lookahead_node) => {
                    let kind_str = lookahead_node.get_leaf_child_at(self.cons.clone(), 0)?.value.as_str();
                    let kind = LookaheadKind::new(kind_str);

                    match kind {
                        LookaheadKind::None => {
                            self.cons.borrow_mut().append_log(BlockParsingLog::LookaheadSymbolAtNodeIsUnknown {
                                node_uuid: lookahead_node.uuid.clone(),
                                symbol: kind_str.to_string(),
                            }.get_log());

                            return Err(());
                        },
                        _ => kind,
                    }
                },
                None => LookaheadKind::None,
            };

            // note: Loop ノード
            let loop_range = match each_seq_elem_node.find_first_child_node(vec![".Rule.Loop"]) {
                Some(loop_node) => {
                    match loop_node.get_child_at(self.cons.clone(), 0)? {
                        SyntaxNodeChild::Node(range_node) => {
                            let raw_range = self.to_raw_range(range_node.as_ref())?;

                            let raw_loop_range_txt = {
                                let min_num_txt = if raw_range.is_min_num_specified {
                                    raw_range.min_num.to_string()
                                } else {
                                    String::new()
                                };

                                match &raw_range.max_num {
                                    Infinitable::Finite(v) => format!("{{{},{}}}", min_num_txt, v),
                                    Infinitable::Infinite => format!("{{{},}}", min_num_txt),
                                }
                            };

                            match &raw_range.max_num {
                                Infinitable::Finite(max_v) => {
                                    if raw_range.min_num > *max_v {
                                        // note: 最小回数が最大回数より大きかった場合
                                        self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsInvalid {
                                            pos: raw_range.range_node_pos.clone(),
                                            loop_range: raw_loop_range_txt.clone(),
                                            msg: format!("min value '{}' is bigger than max value '{}'", raw_range.min_num, max_v),
                                        }.get_log());

                                        return Err(());
                                    }

                                    if raw_range.is_min_num_specified && !raw_range.is_max_num_specified && raw_range.min_num == 0 && *max_v == 0 {
                                        // note: {0} の場合
                                        self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsInvalid {
                                            pos: raw_range.range_node_pos.clone(),
                                            loop_range: raw_loop_range_txt.clone(),
                                            msg: format!("loop range '{{0}}' is invalid"),
                                        }.get_log());

                                        return Err(());
                                    } else if raw_range.min_num == 1 && *max_v == 1 {
                                        if raw_range.is_min_num_specified && !raw_range.is_max_num_specified {
                                            // note: {1} の場合
                                            self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsUnrecommended {
                                                pos: raw_range.range_node_pos.clone(),
                                                loop_range: raw_loop_range_txt.clone(),
                                                msg: format!("loop range '{{1}}' is unnecessary"),
                                            }.get_log());
                                        } else {
                                            // note: {1,1} の場合
                                            self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsUnrecommended {
                                                pos: raw_range.range_node_pos.clone(),
                                                loop_range: raw_loop_range_txt.clone(),
                                                msg: format!("loop range '{{1,1}}' is unnecessary"),
                                            }.get_log());
                                        }
                                    } else if *max_v == 0 {
                                        // note: 最大回数に 0 が指定された場合
                                        self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsInvalid {
                                            pos: raw_range.max_num_pos.unwrap(),
                                            loop_range: raw_loop_range_txt.clone(),
                                            msg: format!("max number '{}' is invalid", raw_range.min_num),
                                        }.get_log());

                                        return Err(());
                                    } else if raw_range.is_max_num_specified && raw_range.min_num == *max_v {
                                        // note: 最小回数と最大回数が同じだった場合
                                        self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsUnrecommended {
                                            pos: raw_range.range_node_pos.clone(),
                                            loop_range: raw_loop_range_txt.clone(),
                                            msg: format!("modify '{}' to '{{{}}}'", raw_loop_range_txt, raw_range.min_num),
                                        }.get_log());
                                    } else if raw_range.is_min_num_specified && raw_range.min_num == 0 {
                                        // note: 最小回数に 0 が指定された場合
                                        self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsUnrecommended {
                                            pos: raw_range.range_node_pos.clone(),
                                            loop_range: raw_loop_range_txt.clone(),
                                            msg: format!("modify '{}' to '{{,{}}}'", raw_loop_range_txt, max_v),
                                        }.get_log());
                                    }
                                },
                                Infinitable::Infinite if raw_range.is_min_num_specified && raw_range.min_num == 0 => {
                                    // note: {0,} の場合
                                    self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsUnrecommended {
                                        pos: raw_range.range_node_pos.clone(),
                                        loop_range: raw_loop_range_txt.clone(),
                                        msg: format!("modify '{}' to '{{,}}'", raw_loop_range_txt),
                                    }.get_log());
                                },
                                _ => (),
                            }

                            let loop_range = LoopRange::new(raw_range.min_num, raw_range.max_num);

                            match loop_range.to_symbol_string() {
                                Some(symbol_str) => {
                                    self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsUnrecommended {
                                        pos: raw_range.range_node_pos.clone(),
                                        loop_range: raw_loop_range_txt.clone(),
                                        msg: format!("prefer {} to {}", raw_loop_range_txt, symbol_str),
                                    }.get_log());
                                },
                                None => (),
                            }

                            loop_range
                        },
                        SyntaxNodeChild::Leaf(leaf) => {
                            let kind_str = leaf.as_ref().value.as_str();

                            match kind_str {
                                "?" | "*" | "+" => LoopRange::from(&leaf.as_ref().value),
                                _ => {
                                    self.cons.borrow_mut().append_log(BlockParsingLog::LoopSymbolAtLeafIsUnknown {
                                        leaf_uuid: leaf.as_ref().uuid.clone(),
                                        symbol: kind_str.to_string(),
                                    }.get_log());

                                    return Err(());
                                },
                            }
                        }
                    }
                },
                None => LoopRange::get_single_loop(),
            };

            // note: RandomOrder ノード
            let (elem_order, random_order_node_pos) = match each_seq_elem_node.find_first_child_node(vec![".Rule.RandomOrder"]) {
                Some(random_order_node) => {
                    let random_order_node_pos = random_order_node.get_position(Some(self.cons.clone()))?;

                    let (min_num, max_num) = match random_order_node.find_first_child_node(vec![".Rule.RandomOrderRange"]) {
                        Some(range_node) => {
                            let raw_range = self.to_raw_range(range_node)?;

                            let raw_loop_range_txt = {
                                let min_num_txt = if raw_range.is_min_num_specified {
                                    raw_range.min_num.to_string()
                                } else {
                                    String::new()
                                };

                                match &raw_range.max_num {
                                    Infinitable::Finite(v) => format!("[{},{}]", min_num_txt, v),
                                    Infinitable::Infinite => format!("[{},]", min_num_txt),
                                }
                            };

                            match &raw_range.max_num {
                                Infinitable::Finite(max_v) => {
                                    if raw_range.min_num > *max_v {
                                        // note: 最小回数が最大回数より大きかった場合
                                        self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsInvalid {
                                            pos: raw_range.range_node_pos.clone(),
                                            loop_range: raw_loop_range_txt.clone(),
                                            msg: format!("min value '{}' is bigger than max value '{}'", raw_range.min_num, max_v),
                                        }.get_log());

                                        return Err(());
                                    }

                                    if raw_range.is_min_num_specified && !raw_range.is_max_num_specified && raw_range.min_num == 0 && *max_v == 0 {
                                        // note: [0] の場合
                                        self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsInvalid {
                                            pos: raw_range.range_node_pos.clone(),
                                            loop_range: raw_loop_range_txt.clone(),
                                            msg: format!("loop range '[0]' is invalid"),
                                        }.get_log());

                                        return Err(());
                                    } else if raw_range.min_num == 1 && *max_v == 1 {
                                        if raw_range.is_min_num_specified && !raw_range.is_max_num_specified {
                                            // note: [1] の場合
                                            self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsUnrecommended {
                                                pos: raw_range.range_node_pos.clone(),
                                                loop_range: raw_loop_range_txt.clone(),
                                                msg: format!("loop range '[1]' is unnecessary"),
                                            }.get_log());
                                        } else {
                                            // note: [1,1] の場合
                                            self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsUnrecommended {
                                                pos: raw_range.range_node_pos.clone(),
                                                loop_range: raw_loop_range_txt.clone(),
                                                msg: format!("loop range [1-1]' is unnecessary"),
                                            }.get_log());
                                        }
                                    } else if *max_v == 0 {
                                        // note: 最大回数に 0 が指定された場合
                                        self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsInvalid {
                                            pos: raw_range.max_num_pos.unwrap(),
                                            loop_range: raw_loop_range_txt.clone(),
                                            msg: format!("max number '{}' is invalid", raw_range.min_num),
                                        }.get_log());

                                        return Err(());
                                    } else if raw_range.is_max_num_specified && raw_range.min_num == *max_v {
                                        // note: 最小回数と最大回数が同じだった場合
                                        self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsUnrecommended {
                                            pos: raw_range.range_node_pos.clone(),
                                            loop_range: raw_loop_range_txt.clone(),
                                            msg: format!("modify '{}' to '[{}]'", raw_loop_range_txt, raw_range.min_num),
                                        }.get_log());
                                    } else if raw_range.is_min_num_specified && raw_range.min_num == 0 {
                                        // note: 最小回数に 0 が指定された場合
                                        self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsUnrecommended {
                                            pos: raw_range.range_node_pos.clone(),
                                            loop_range: raw_loop_range_txt.clone(),
                                            msg: format!("modify '{}' to '[-{}]'", raw_loop_range_txt, max_v),
                                        }.get_log());
                                    }
                                },
                                Infinitable::Infinite if raw_range.is_min_num_specified && raw_range.min_num == 0 => {
                                    // note: [0-] の場合
                                    self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsUnrecommended {
                                        pos: raw_range.range_node_pos.clone(),
                                        loop_range: raw_loop_range_txt.clone(),
                                        msg: format!("modify '{}' to '[,]'", raw_loop_range_txt),
                                    }.get_log());
                                },
                                _ => (),
                            }

                            (raw_range.min_num, raw_range.max_num)
                        },
                        None => (1, Infinitable::Finite(1)),
                    };

                    let loop_range = LoopRange::new(min_num, max_num);
                    (ElementOrder::Random(loop_range), random_order_node_pos)
                },
                None => (ElementOrder::Sequential, CharacterPosition::get_empty()),
            };

            // note: ASTReflectionStyle ノード
            // todo: 構成ファイルによって切り替える
            let ast_reflection_style = match each_seq_elem_node.find_first_child_node(vec![".Rule.ASTReflectionStyle"]) {
                Some(style_node) => {
                    match style_node.get_leaf_child_at(self.cons.clone(), 0) {
                        Ok(leaf) => {
                            if leaf.value == "##" {
                                AstReflectionStyle::Expansion
                            } else {
                                AstReflectionStyle::Reflection(style_node.join_child_leaf_values())
                            }
                        },
                        Err(()) => {
                            self.cons.borrow_mut().pop_log();
                            AstReflectionStyle::from_config(false, true, String::new())
                        },
                    }
                },
                None => AstReflectionStyle::from_config(false, false, String::new()),
            };

            // note: Choice または Expr ノード
            let choice_or_expr_node = match each_seq_elem_node.find_first_child_node(vec![".Rule.Choice", ".Rule.Expr"]) {
                Some(v) => v,
                None => {
                    self.cons.borrow_mut().append_log(BlockParsingLog::ChildNodeInNodeUnexpectedExpected {
                        parent_node_uuid: each_seq_elem_node.uuid.clone(),
                        unexpected_name: "unknown".to_string(),
                        expected_name: "choice or expression node".to_string(),
                    }.get_log());

                    return Err(());
                },
            };

            match &choice_or_expr_node.ast_reflection_style {
                AstReflectionStyle::Reflection(name) => {
                    let new_elem = match name.as_str() {
                        ".Rule.Choice" => {
                            let mut new_choice = Box::new(self.to_rule_choice_elem(choice_or_expr_node.get_node_child_at(self.cons.clone(), 0)?, generics_args)?);
                            new_choice.ast_reflection_style = ast_reflection_style;
                            new_choice.lookahead_kind = lookahead_kind;
                            new_choice.loop_range = loop_range;
                            new_choice.elem_order = elem_order;
                            RuleElement::Group(Rc::new(new_choice))
                        },
                        ".Rule.Expr" => {
                            if elem_order.is_random() {
                                self.cons.borrow_mut().append_log(BlockParsingLog::CannotSpecifyRandomOrderToExpression {
                                    pos: random_order_node_pos,
                                }.get_log());

                                return Err(());
                            }

                            let mut new_expr = Box::new(self.to_rule_expr_elem(choice_or_expr_node, generics_args)?);
                            new_expr.ast_reflection_style = ast_reflection_style;
                            new_expr.lookahead_kind = lookahead_kind;
                            new_expr.loop_range = loop_range;
                            RuleElement::Expression(new_expr)
                        },
                        _ => {
                            self.cons.borrow_mut().append_log(BlockParsingLog::NodeUnexpectedExpected {
                                node_uuid: choice_or_expr_node.uuid.clone(),
                                unexpected_name: format!("'{}'", name),
                                expected_name: "choice or expression node".to_string(),
                            }.get_log());

                            return Err(());
                        },
                    };

                    children.push(new_elem);
                },
                _ => {
                    self.cons.borrow_mut().append_log(BlockParsingLog::NodeUnexpectedExpected {
                        node_uuid: choice_or_expr_node.uuid.clone(),
                        unexpected_name: "no name".to_string(),
                        expected_name: "choice or expression node".to_string(),
                    }.get_log());

                    return Err(());
                },
            };
        }

        let mut seq = Box::new(RuleGroup::new(RuleGroupKind::Sequence));
        seq.subelems = children;
        return Ok(RuleElement::Group(Rc::new(seq)));
    }

    fn to_raw_range(&mut self, range_node: &SyntaxNode) -> ConsoleResult<RawRange> {
        let range_node_pos = range_node.get_position(Some(self.cons.clone()))?;

        let (min_num, min_num_pos, is_min_num_specified) = match range_node.find_child_nodes(vec!["MinNum"]).get(0) {
            Some(min_num_node) => {
                let min_num_pos = min_num_node.get_position(Some(self.cons.clone()))?;
                let min_str = min_num_node.join_child_leaf_values();

                match min_str.parse::<usize>() {
                    Ok(v) => (v, Some(min_num_pos), true),
                    Err(_) => {
                        self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsInvalid {
                            pos: min_num_node.get_position(Some(self.cons.clone()))?,
                            loop_range: "<unparsed>".to_string(),
                            msg: format!("'{}' is too long or not a number", min_str),
                        }.get_log());

                        return Err(());
                    },
                }
            },
            None => (0usize, None, false),
        };

        let (max_num, max_num_pos, is_max_num_specified) = match range_node.find_child_nodes(vec!["MaxNumGroup"]).get(0) {
            Some(max_node_group) => {
                match max_node_group.find_child_nodes(vec!["MaxNum"]).get(0) {
                    Some(max_num_node) => {
                        // note: {n,m} の場合 (#MaxNumGroup 内に #MaxNum が存在する)
                        let max_num_pos = max_num_node.get_position(Some(self.cons.clone()))?;
                        let max_str = max_num_node.join_child_leaf_values();

                        match max_str.parse::<usize>() {
                            Ok(v) => (Infinitable::Finite(v), Some(max_num_pos), true),
                            Err(_) => {
                                self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsInvalid {
                                    pos: max_num_pos,
                                loop_range: "<unparsed>".to_string(),
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
                    self.cons.borrow_mut().append_log(BlockParsingLog::LoopRangeIsInvalid {
                        pos: range_node_pos,
                        loop_range: "{{,}}".to_string(),
                        msg: format!("no number specified"),
                    }.get_log());

                    return Err(());
                }

                (Infinitable::Finite(min_num), None, false)
            },
        };

        let raw_range = RawRange {
            min_num: min_num,
            max_num: max_num,
            is_min_num_specified: is_min_num_specified,
            is_max_num_specified: is_max_num_specified,
            range_node_pos: range_node_pos,
            min_num_pos: min_num_pos,
            max_num_pos: max_num_pos,
        };

        return Ok(raw_range);
    }

    // note: Rule.PureChoice ノードの解析
    fn to_rule_choice_elem(&mut self, choice_node: &SyntaxNode, generics_args: &Vec<String>) -> ConsoleResult<RuleGroup> {
        let mut children = Vec::<RuleElement>::new();
        let mut group_kind = RuleGroupKind::Sequence;

        // Seq ノードをループ
        for seq_elem in &choice_node.get_reflectable_children() {
            match &seq_elem {
                SyntaxNodeChild::Node(node) => {
                    match &seq_elem.get_ast_reflection_style() {
                        AstReflectionStyle::Reflection(name) => if name == ".Rule.Seq" {
                            let new_child = self.to_seq_elem(node.as_ref(), generics_args)?;
                            children.push(new_child);
                        },
                        _ => (),
                    }
                },
                SyntaxNodeChild::Leaf(leaf) => {
                    match leaf.as_ref().value.as_str() {
                        ":" | "," => group_kind = RuleGroupKind::Choice,
                        _ => (),
                    }
                },
            }
        }

        let mut group = Box::new(RuleGroup::new(group_kind.clone()));
        group.subelems = children;

        let mut tmp_root_group = RuleGroup::new(RuleGroupKind::Sequence);
        tmp_root_group.subelems = vec![RuleElement::Group(Rc::new(group))];

        return Ok(tmp_root_group);
    }

    fn to_rule_expr_elem(&mut self, expr_node: &SyntaxNode, generics_args: &Vec<String>) -> ConsoleResult<RuleExpression> {
        let expr_child_node = expr_node.get_node_child_at(self.cons.clone(), 0)?;
        let (pos, kind, value) = match &expr_child_node.ast_reflection_style {
            AstReflectionStyle::Reflection(name) => {
                match name.as_str() {
                    ".Rule.ArgID" => (expr_child_node.get_position(Some(self.cons.clone()))?, RuleExpressionKind::ArgId, self.to_argument_id(self.rule_name.clone(), expr_child_node.join_child_leaf_values())),
                    ".Rule.CharClass" => (expr_child_node.get_position(Some(self.cons.clone()))?, RuleExpressionKind::CharClass, format!("[{}]", expr_child_node.join_child_leaf_values())),
                    ".Rule.ID" => {
                        let chain_id_node = expr_child_node.get_node_child_at(self.cons.clone(), 0)?;
                        let parent_node = chain_id_node.get_node_child_at(self.cons.clone(), 0)?;
                        let pos = parent_node.get_position(Some(self.cons.clone()))?;

                        let new_generics_args = match expr_child_node.find_first_child_node(vec![".Rule.Generics"]) {
                            Some(generics_node) => {
                                let mut args = Vec::<Rc<Box<RuleGroup>>>::new();

                                for seq_node in generics_node.find_child_nodes(vec![".Rule.Seq"]) {
                                    match self.to_seq_elem(seq_node, generics_args)? {
                                        RuleElement::Group(new_arg) => args.push(new_arg),
                                        _ => (),
                                    };
                                }

                                args
                            },
                            None => Vec::new(),
                        };

                        let new_template_args = match expr_child_node.find_first_child_node(vec![".Rule.Template"]) {
                            Some(template_node) => {
                                let mut args = Vec::<Rc<Box<RuleGroup>>>::new();

                                for seq_node in template_node.find_child_nodes(vec![".Rule.Seq"]) {
                                    match self.to_seq_elem(seq_node, generics_args)? {
                                        RuleElement::Group(new_arg) => args.push(new_arg),
                                        _ => (),
                                    };
                                }

                                args
                            },
                            None => Vec::new(),
                        };

                        let id = BlockParser::to_rule_id(self.cons.clone(), &pos, &BlockParser::to_string_vec(self.cons.clone(), chain_id_node)?, &self.block_alias_map, &self.file_alias_name, &self.block_name, &self.replaced_file_alias_names)?;

                        if !self.used_rule_ids.contains_key(&id) {
                            self.used_rule_ids.insert(id.clone(), pos.clone());
                        }

                        let id_expr_kind = RuleExpressionKind::IdWithArgs {
                            generics_args: new_generics_args,
                            template_args: new_template_args,
                        };

                        (pos, id_expr_kind, id)
                    },
                    ".Rule.Str" => (expr_child_node.get_position(Some(self.cons.clone()))?, RuleExpressionKind::String, self.to_string_value(expr_child_node)?),
                    ".Rule.Wildcard" => (expr_child_node.get_position(Some(self.cons.clone()))?, RuleExpressionKind::Wildcard, ".".to_string()),
                    _ => {
                        self.cons.borrow_mut().append_log(BlockParsingLog::ChildNodeInNodeUnexpectedExpected {
                            parent_node_uuid: expr_child_node.uuid.clone(),
                            unexpected_name: format!("'{}'", name),
                            expected_name: "rule expression node".to_string(),
                        }.get_log());

                        return Err(());
                    },
                }
            },
            _ => {
                self.cons.borrow_mut().append_log(BlockParsingLog::ChildNodeInNodeUnexpectedExpected {
                    parent_node_uuid: expr_child_node.uuid.clone(),
                    unexpected_name: "no name".to_string(),
                    expected_name: "rule expression node".to_string(),
                }.get_log());

                return Err(());
            },
        };

        let expr = RuleExpression::new(pos, kind, value);
        return Ok(expr);
    }

    fn to_argument_id(&self, rule_name: String, arg_name: String) -> String {
        return format!("{}.{}.{}.${}", self.file_alias_name, self.block_name, rule_name, arg_name);
    }

    fn to_string_vec(cons: Rc<RefCell<Console>>, str_vec_node: &SyntaxNode) -> ConsoleResult<Vec<String>> {
        let mut str_vec = Vec::<String>::new();

        for str_elem in str_vec_node.get_reflectable_children() {
            str_vec.push(str_elem.get_node(cons.clone())?.join_child_leaf_values());
        }

        return Ok(str_vec);
    }

    fn to_rule_id(cons: Rc<RefCell<Console>>, pos: &CharacterPosition, id_tokens: &Vec<String>, block_alias_map: &HashMap<String, String>, file_alias_name: &String, block_name: &String, replaced_file_alias_names: &Arc<HashMap<String, String>>) -> ConsoleResult<String> {
        let (new_id, id_block_name, id_rule_name) = match id_tokens.len() {
            1 => {
                let id_rule_name = id_tokens.get(0).unwrap();
                let new_id = BlockParser::to_rule_id_from_elements(replaced_file_alias_names, file_alias_name, block_name, id_rule_name);

                (new_id, block_name, id_rule_name.clone())
            },
            2 => {
                let block_name = id_tokens.get(0).unwrap().to_string();
                let rule_name = id_tokens.get(1).unwrap().to_string();

                if block_alias_map.contains_key(&block_name.to_string()) {
                    // note: ブロック名がエイリアスである場合
                    let block_name = block_alias_map.get(&block_name.to_string()).unwrap();
                    let new_id = BlockParser::to_block_id_from_elements(&replaced_file_alias_names, &block_name, &rule_name);

                    (new_id, block_name, rule_name.clone())
                } else {
                    // note: ブロック名がエイリアスでない場合; ブロックが存在しない判定になる
                    cons.borrow_mut().append_log(BlockParsingLog::BlockAliasNotFoundOrDeclaredUse {
                        pos: pos.clone(),
                        alias_name: block_name.to_string(),
                    }.get_log());

                    return Err(());
                }
            },
            3 => {
                let file_alias_name = id_tokens.get(0).unwrap();
                let block_name = id_tokens.get(1).unwrap();
                let rule_name = id_tokens.get(2).unwrap();
                let new_id = BlockParser::to_rule_id_from_elements(replaced_file_alias_names, file_alias_name, block_name, rule_name);

                (new_id, block_name, rule_name.to_string())
            },
            _ => {
                cons.borrow_mut().append_log(BlockParsingLog::IDIsInvalid {
                    pos: pos.clone(),
                    id: id_tokens.join("."),
                }.get_log());

                return Err(());
            },
        };

        // note: プライベート規則の外部アクセスを除外
        // todo: プライベートブロックに対応
        // todo: 異なるファイルでの同ブロック名を除外
        if id_rule_name.starts_with("_") && *block_name != *id_block_name {
            cons.borrow_mut().append_log(BlockParsingLog::AttemptToAccessPrivateItem {
                pos: pos.clone(),
                item_id: new_id.clone(),
            }.get_log());
        }

        return Ok(new_id);
    }

    fn to_string_value(&mut self, str_node: &SyntaxNode) -> ConsoleResult<String> {
        let mut s = String::new();

        for each_elem in &str_node.subelems {
            match each_elem {
                SyntaxNodeChild::Node(node) => {
                    match node.as_ref().ast_reflection_style {
                        AstReflectionStyle::Reflection(_) => {
                            let escseq_char = node.as_ref().get_leaf_child_at(self.cons.clone(), 0)?.value.as_str();

                            s += match escseq_char {
                                "\\" => "\\",
                                "\"" => "\"",
                                "n" => "\n",
                                "t" => "\t",
                                "z" => "\0",
                                _ => {
                                    self.cons.borrow_mut().append_log(BlockParsingLog::EscapeSequenceCharacterIsUnknown {
                                        pos: node.as_ref().get_position(Some(self.cons.clone()))?,
                                        escseq_char: escseq_char.to_string(),
                                    }.get_log());

                                    return Err(());
                                },
                            };
                        },
                        _ => (),
                    }
                },
                SyntaxNodeChild::Leaf(leaf) => {
                    match leaf.as_ref().ast_reflection_style {
                        AstReflectionStyle::Reflection(_) => s += leaf.as_ref().value.as_ref(),
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
            ids.push(chain_id_elem.get_node(self.cons.clone())?.join_child_leaf_values());
        }

        return Ok(ids.join("."));
    }

    fn to_attribute_map(&mut self, attr_list_node: &SyntaxNode) -> ConsoleResult<AttributeMap> {
        let mut attr_map = AttributeMap::new();

        for each_attr_node in &attr_list_node.find_child_nodes(vec![".Attr.Attr"]) {
            let new_attr = self.to_attribute(each_attr_node)?;

            // note: 属性名の重複チェック
            if attr_map.contains_key(&new_attr.name) {
                self.cons.borrow_mut().append_log(BlockParsingLog::AttributeNameIsDuplicate {
                    pos: new_attr.pos.clone(),
                    attr_name: new_attr.name.clone(),
                }.get_log());

                return Err(());
            }

            attr_map.insert(new_attr.name.clone(), new_attr);
        }

        return Ok(attr_map);
    }

    fn to_attribute(&mut self, attr_node: &SyntaxNode) -> ConsoleResult<Attribute> {
        let pos = attr_node.get_position(Some(self.cons.clone()))?;
        let name = attr_node.get_node_child_at(self.cons.clone(), 0)?.join_child_leaf_values();
        let mut values = Vec::<AttributeValue>::new();

        for each_attr_value_node in &attr_node.find_child_nodes(vec![".Attr.Value"]) {
            let is_negative = each_attr_value_node.find_first_child_node(vec!["Negative"]).is_some();
            let id_node_name = ".Misc.SingleID";

            match each_attr_value_node.find_first_child_node(vec![id_node_name]) {
                Some(id_node) => {
                    let new_value = AttributeValue::new(id_node.get_position(Some(self.cons.clone()))?, is_negative, id_node.join_child_leaf_values());
                    values.push(new_value);
                },
                None => {
                    self.cons.borrow_mut().append_log(BlockParsingLog::NodeExpectedToBeName {
                        node_uuid: each_attr_value_node.uuid.clone(),
                        expected_name: format!("'{}'", id_node_name),
                    }.get_log());

                    return Err(());
                }
            }
        }

        let attr = Attribute::new(pos, name, values);
        return Ok(attr);
    }

    fn analyze_attribute(cons: Rc<RefCell<Console>>, config: &Rc<Configuration>, attr_map: &AttributeMap, skipping_tar_ids: &mut Vec<String>) -> ConsoleResult<()> {
        for (attr_name, attr) in attr_map {
            let attr_kind = match AttributeKind::from(attr_name) {
                Some(v) => v,
                None => {
                    cons.borrow_mut().append_log(BlockParsingLog::AttributeNameNotFound {
                        pos: attr.pos.clone(),
                        attr_name: attr_name.clone(),
                    }.get_log());

                    return Err(());
                }
            };

            match attr_kind {
                AttributeKind::Skip => {
                    for each_attr_value in &attr.values {
                        let tar_id = match config.skipping_map.get(&each_attr_value.value) {
                            Some(v) => v.clone(),
                            None => {
                                cons.borrow_mut().append_log(BlockParsingLog::SkippingNameNotFound {
                                    pos: each_attr_value.pos.clone(),
                                    skipping_name: each_attr_value.value.clone(),
                                }.get_log());

                                return Err(());
                            }
                        };

                        skipping_tar_ids.push(tar_id);
                    }
                },
            }
        }

        return Ok(());
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

    fn to_block_id_from_elements(replaced_file_alias_names: &Arc<HashMap<String, String>>, file_alias_name: &String, block_name: &String) -> String {
        let replaced_file_alias_name = match replaced_file_alias_names.get(file_alias_name) {
            Some(v) => v,
            None => file_alias_name,
        };

        return format!("{}.{}", replaced_file_alias_name, block_name);
    }

    fn to_rule_id_from_elements(replaced_file_alias_names: &Arc<HashMap<String, String>>, file_alias_name: &String, block_name: &String, rule_name: &String) -> String {
        if PRIMITIVE_RULE_NAMES.contains(&rule_name.as_str()) {
            return rule_name.clone();
        }

        let replaced_file_alias_name = match replaced_file_alias_names.get(file_alias_name) {
            Some(v) => v,
            None => file_alias_name,
        };

        return format!("{}.{}.{}", replaced_file_alias_name, block_name, rule_name);
    }
}

struct FCPEGBlock {}

impl FCPEGBlock {
    pub fn get_block_map() -> BlockMap {
        return block_map!{
            "Main" => FCPEGBlock::get_main_block(),
            "Symbol" => FCPEGBlock::get_symbol_block(),
            "Misc" => FCPEGBlock::get_misc_block(),
            "Attr" => FCPEGBlock::get_attr_block(),
            "Block" => FCPEGBlock::get_block_block(),
            "Rule" => FCPEGBlock::get_rule_block(),
        };
    }

    fn get_main_block() -> Block {
        // code: Main <- Symbol.Space*# Symbol.LineEnd*# (Block.Block Symbol.Div*#)* "\z"#,
        let fcpeg_rule = rule!{
            ".Main.Main",
            group!{
                vec![],
                expr!(Id, ".Symbol.Space", "*", "#"),
                expr!(Id, ".Symbol.LineEnd", "*", "#"),
                group!{
                    vec!["*"],
                    expr!(Id, ".Block.Block"),
                    expr!(Id, ".Symbol.Div", "*", "#"),
                },
                expr!(String, "\0", "#"),
            },
        };

        return block!(".Syntax", vec![fcpeg_rule]);
    }

    fn get_symbol_block() -> Block {
        // code: Space <- " ",
        let space_rule = rule!{
            ".Symbol.Space",
            group!{
                vec![],
                expr!(String, " "),
            },
        };

        // code: LineEnd <- Space* "\n" Space*,
        let line_end_rule = rule!{
            ".Symbol.LineEnd",
            group!{
                vec![],
                expr!(Id, ".Symbol.Space", "*"),
                expr!(String, "\n"),
                expr!(Id, ".Symbol.Space", "*"),
            },
        };

        // code: Div <- Space : "\n",
        let div_rule = rule!{
            ".Symbol.Div",
            group!{
                vec![],
                group!{
                    vec![":"],
                    group!{
                        vec![],
                        expr!(Id, ".Symbol.Space"),
                    },
                    group!{
                        vec![],
                        expr!(String, "\n"),
                    },
                },
            },
        };

        // note: CommaDiv <- Div* (",," LineEnd Div* : "," Space*),
        let comma_div_rule = rule!{
            ".Symbol.CommaDiv",
            group!{
                vec![],
                expr!(Id, ".Symbol.Div", "*"),
                group!{
                    vec![":"],
                    group!{
                        vec![],
                        expr!(String, ",,"),
                        expr!(Id, ".Symbol.LineEnd"),
                        expr!(Id, ".Symbol.Div", "*"),
                    },
                    group!{
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
            group!{
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
            group!{
                vec![],
                expr!(CharClass, "[a-zA-Z_]"),
                expr!(CharClass, "[a-zA-Z0-9_]", "*"),
            },
        };

        // code: ChainID <- SingleID ("."# SingleID)*##,
        let chain_id_rule = rule!{
            ".Misc.ChainID",
            group!{
                vec![],
                expr!(Id, ".Misc.SingleID"),
                group!{
                    vec!["*", "##"],
                    group!{
                        vec![],
                        expr!(String, ".", "#"),
                        expr!(Id, ".Misc.SingleID"),
                    },
                },
            },
        };

        return block!(".Misc", vec![single_id_rule, chain_id_rule]);
    }

    fn get_attr_block() -> Block {
        // code: AttrList <- Attr (Symbol.LineEnd+# Attr)*## Symbol.LineEnd+#,
        let attr_list_rule = rule!{
            ".Attr.AttrList",
            group!{
                vec![],
                expr!(Id, ".Attr.Attr"),
                group!{
                    vec!["*", "##"],
                    expr!(Id, ".Symbol.LineEnd", "+", "#"),
                    expr!(Id, ".Attr.Attr"),
                },
                // fix: "+" に設定するとなぜかパースに失敗する
                expr!(Id, ".Symbol.LineEnd", "*", "#"),
            },
        };

        // code: Attr <- "@"# Symbol.Div*# Misc.SingleID Symbol.Div*# "["# Symbol.Div*# Value (Symbol.Div*# ","# Symbol.Div*# Value)*## Symbol.Div*# "]"#,
        let attr_rule = rule!{
            ".Attr.Attr",
            group!{
                vec![],
                expr!(String, "@", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Misc.SingleID"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "[", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Attr.Value"),
                group!{
                    vec!["*", "##"],
                    expr!(Id, ".Symbol.Div", "*", "#"),
                    expr!(String, ",", "#"),
                    expr!(Id, ".Symbol.Div", "*", "#"),
                    expr!(Id, ".Attr.Value"),
                },
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "]", "#"),
            },
        };

        // code: Value <- ("!")?#Negative Symbol.Div*# Misc.SingleID,
        let value_rule = rule!{
            ".Attr.Value",
            group!{
                vec![],
                group!{
                    vec!["?", "#Negative"],
                    expr!(String, "!"),
                },
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Misc.SingleID"),
            },
        };

        return block!(".Attr", vec![attr_list_rule, attr_rule, value_rule]);
    }

    fn get_block_block() -> Block {
        // code: Block <- Attr.AttrList? "["# Symbol.Div*# Misc.SingleID Symbol.Div*# "]"# Symbol.Div*# "{"# Symbol.Div*# (Cmd Symbol.Div*#)*## "}"#,
        let block_rule = rule!{
            ".Block.Block",
            group!{
                vec![],
                expr!(Id, ".Attr.AttrList", "?"),
                expr!(String, "[", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Misc.SingleID"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "]", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "{", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                group!{
                    vec!["*", "##"],
                    expr!(Id, ".Block.Cmd"),
                    expr!(Id, ".Symbol.Div", "*", "#"),
                },
                expr!(String, "}", "#"),
            },
        };

        // code: Cmd <- CommentCmd : DefineCmd : StartCmd : UseCmd,
        let cmd_rule = rule!{
            ".Block.Cmd",
            group!{
                vec![":"],
                group!{
                    vec![],
                    expr!(Id, ".Block.CommentCmd"),
                },
                group!{
                    vec![],
                    expr!(Id, ".Block.DefineCmd"),
                },
                group!{
                    vec![],
                    expr!(Id, ".Block.StartCmd"),
                },
                group!{
                    vec![],
                    expr!(Id, ".Block.UseCmd"),
                },
            },
        };

        // code: CommentCmd <- "%"# (!"," . : ",,")*## ","#,
        let comment_rule = rule!{
            ".Block.CommentCmd",
            group!{
                vec![],
                expr!(String, "%", "#"),
                group!{
                    vec!["*", "##"],
                    group!{
                        vec![":"],
                        group!{
                            vec![],
                            expr!(String, ",", "!"),
                            expr!(Wildcard, "."),
                        },
                        group!{
                            vec![],
                            expr!(String, ",,"),
                        },
                    },
                },
                expr!(String, ",", "#"),
            },
        };

        // code: DefineCmd <- Attr.AttrList? Misc.SingleID DefineCmdGenerics? DefineCmdTemplate? Symbol.Div*# "<-"# Symbol.Div*# Rule.PureChoice Symbol.Div*# ","#,
        let define_cmd_rule = rule!{
            ".Block.DefineCmd",
            group!{
                vec![],
                expr!(Id, ".Attr.AttrList", "?"),
                expr!(Id, ".Misc.SingleID"),
                expr!(Id, ".Block.DefineCmdGenerics", "?"),
                expr!(Id, ".Block.DefineCmdTemplate", "?"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "<-", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.PureChoice"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, ",", "#"),
            },
        };

        // code: DefineCmdGenerics <- Symbol.Div*# "<"# Symbol.Div*# Rule.ArgID (Symbol.Div*# ","# Symbol.Div*# Rule.ArgID)*## Symbol.Div*# ">"# Symbol.Div*#,
        let define_cmd_generics_rule = rule!{
            ".Block.DefineCmdGenerics",
            group!{
                vec![],
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "<", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.ArgID"),
                group!{
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

        // code: DefineCmdTemplate <- Symbol.Div*# "("# Symbol.Div*# Rule.ArgID (Symbol.Div*# ","# Symbol.Div*# Rule.ArgID)*## Symbol.Div*# ")"# Symbol.Div*#,
        let define_cmd_template_rule = rule!{
            ".Block.DefineCmdTemplate",
            group!{
                vec![],
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "(", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.ArgID"),
                group!{
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
            group!{
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
            group!{
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
            group!{
                vec![],
                expr!(Id, ".Symbol.Div", "+", "#"),
                expr!(String, "as", "#"),
                expr!(Id, ".Symbol.Div", "+", "#"),
                expr!(Id, ".Misc.SingleID"),
            },
        };

        return block!(".Block", vec![block_rule, cmd_rule, comment_rule, define_cmd_rule, define_cmd_generics_rule, define_cmd_template_rule, start_cmd_rule, use_cmd_rule, use_cmd_block_alias_rule]);
    }

    fn get_rule_block() -> Block {
        // code: PureChoice <- Seq ((Symbol.Div+# ":" Symbol.Div+# : ",")## Seq)*##,
        let pure_choice_rule = rule!{
            ".Rule.PureChoice",
            group!{
                vec![],
                expr!(Id, ".Rule.Seq"),
                group!{
                    vec!["*", "##"],
                    group!{
                        vec!["##"],
                        group!{
                            vec![":"],
                            group!{
                                vec!["##"],
                                expr!(Id, ".Symbol.Div", "+", "#"),
                                expr!(String, ":"),
                                expr!(Id, ".Symbol.Div", "+", "#"),
                            },
                            group!{
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
            group!{
                vec![],
                expr!(String, "(", "#"),
                expr!(Id, ".Rule.PureChoice"),
                expr!(String, ")", "#"),
            },
        };

        // code: Seq <- SeqElem (Symbol.Div+# SeqElem)*##,
        let seq_rule = rule!{
            ".Rule.Seq",
            group!{
                vec![],
                expr!(Id, ".Rule.SeqElem"),
                group!{
                    vec!["*", "##"],
                    group!{
                        vec![],
                        group!{
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
            group!{
                vec![],
                expr!(Id, ".Rule.Lookahead", "?"),
                group!{
                    vec!["##"],
                    group!{
                        vec![":"],
                        group!{
                            vec![],
                            expr!(Id, ".Rule.Choice"),
                        },
                        group!{
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

        // code: Expr <- ArgID : ID : Str : CharClass : Wildcard,
        let expr_rule = rule!{
            ".Rule.Expr",
            group!{
                vec![],
                group!{
                    vec![":"],
                    group!{
                        vec![],
                        expr!(Id, ".Rule.ArgID"),
                    },
                    group!{
                        vec![],
                        expr!(Id, ".Rule.ID"),
                    },
                    group!{
                        vec![],
                        expr!(Id, ".Rule.Str"),
                    },
                    group!{
                        vec![],
                        expr!(Id, ".Rule.CharClass"),
                    },
                    group!{
                        vec![],
                        expr!(Id, ".Rule.Wildcard"),
                    },
                },
            },
        };

        // code: Lookahead <- "!" : "&",
        let lookahead_rule = rule!{
            ".Rule.Lookahead",
            group!{
                vec![],
                group!{
                    vec![":"],
                    group!{
                        vec![],
                        expr!(String, "!"),
                    },
                    group!{
                        vec![],
                        expr!(String, "&"),
                    },
                },
            },
        };

        // code: Loop <- "?" : "*" : "+" : LoopRange,
        let loop_rule = rule!{
            ".Rule.Loop",
            group!{
                vec![],
                group!{
                    vec![":"],
                    group!{
                        vec![],
                        expr!(String, "?"),
                    },
                    group!{
                        vec![],
                        expr!(String, "*"),
                    },
                    group!{
                        vec![],
                        expr!(String, "+"),
                    },
                    group!{
                        vec![],
                        expr!(Id, ".Rule.LoopRange"),
                    },
                },
            },
        };

        // code: LoopRange <- "{"# Symbol.Div*# Num?#MinNum (Symbol.CommaDiv# Num?#MaxNum)?#MaxNumGroup Symbol.Div*# "}"#,
        let loop_range_rule = rule!{
            ".Rule.LoopRange",
            group!{
                vec![],
                expr!(String, "{", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.Num", "?", "#MinNum"),
                group!{
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
            group!{
                vec![],
                expr!(String, "^", "#"),
                expr!(Id, ".Rule.RandomOrderRange", "?"),
            },
        };

        // code: RandomOrderRange <- "["# Symbol.Div*# Num?#MinNum (Symbol.Div*# "-"# Symbol.Div*# Num?#MaxNum)?#MaxNumGroup Symbol.Div*# "]"#,
        let random_order_range_rule = rule!{
            ".Rule.RandomOrderRange",
            group!{
                vec![],
                expr!(String, "[", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.Num", "?", "#MinNum"),
                group!{
                    vec!["?", "#MaxNumGroup"],
                    expr!(Id, ".Symbol.Div", "*", "#"),
                    expr!(String, "-", "#"),
                    expr!(Id, ".Symbol.Div", "*", "#"),
                    expr!(Id, ".Rule.Num", "?", "#MaxNum"),
                },
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, "]", "#"),
            },
        };

        // code: ASTReflectionStyle <- "##" : "#"# Misc.SingleID?##,
        let ast_reflection_rule = rule!{
            ".Rule.ASTReflectionStyle",
            group!{
                vec![],
                group!{
                    vec![":"],
                    group!{
                        vec![],
                        expr!(String, "##"),
                    },
                    group!{
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
            group!{
                vec![],
                expr!(CharClass, "[0-9]+", "+"),
            },
        };

        // code: ID <- Misc.ChainID Generics? Template?,
        let id_rule = rule!{
            ".Rule.ID",
            group!{
                vec![],
                expr!(Id, ".Misc.ChainID"),
                expr!(Id, ".Rule.Generics", "?"),
                expr!(Id, ".Rule.Template", "?"),
            },
        };

        // code: ArgID <- "$"# Misc.SingleID##,
        let arg_id_rule = rule!{
            ".Rule.ArgID",
            group!{
                vec![],
                expr!(String, "$", "#"),
                expr!(Id, ".Misc.SingleID", "##"),
            },
        };

        // code: Generics <- "<"# Symbol.Div*# Seq (Symbol.Div*# ","# Symbol.Div*# Seq)*## Symbol.Div*# ">"#,
        let generics_rule = rule!{
            ".Rule.Generics",
            group!{
                vec![],
                expr!(String, "<", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.Seq"),
                group!{
                    vec!["*", "##"],
                    group!{
                        vec!["##"],
                        expr!(Id, ".Symbol.Div", "*", "#"),
                        expr!(String, ",", "#"),
                        expr!(Id, ".Symbol.Div", "*", "#"),
                        expr!(Id, ".Rule.Seq"),
                    },
                },
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, ">", "#"),
            },
        };

        // code: Template <- "("# Symbol.Div*# Seq (Symbol.Div*# ","# Symbol.Div*# Seq)*## Symbol.Div*# ")"#,
        let template_rule = rule!{
            ".Rule.Template",
            group!{
                vec![],
                expr!(String, "(", "#"),
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(Id, ".Rule.Seq"),
                group!{
                    vec!["*", "##"],
                    group!{
                        vec!["##"],
                        expr!(Id, ".Symbol.Div", "*", "#"),
                        expr!(String, ",", "#"),
                        expr!(Id, ".Symbol.Div", "*", "#"),
                        expr!(Id, ".Rule.Seq"),
                    },
                },
                expr!(Id, ".Symbol.Div", "*", "#"),
                expr!(String, ")", "#"),
            },
        };

        // code: EscSeq <- "\\"# ("\\" : "\"" : "n" : "t" : "z")##,
        let esc_seq_rule = rule!{
            ".Rule.EscSeq",
            group!{
                vec![],
                expr!(String, "\\", "#"),
                group!{
                    vec!["##"],
                    group!{
                        vec![":"],
                        group!{
                            vec![],
                            expr!(String, "\\"),
                        },
                        group!{
                            vec![],
                            expr!(String, "\""),
                        },
                        group!{
                            vec![],
                            expr!(String, "n"),
                        },
                        group!{
                            vec![],
                            expr!(String, "t"),
                        },
                        group!{
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
            group!{
                vec![],
                expr!(String, "\"", "#"),
                group!{
                    vec!["*", "##"],
                    group!{
                        vec![":"],
                        group!{
                            vec![],
                            expr!(Id, ".Rule.EscSeq"),
                        },
                        group!{
                            vec![],
                            group!{
                                vec!["!"],
                                group!{
                                    vec![":"],
                                    group!{
                                        vec![],
                                        expr!(String, "\\"),
                                    },
                                    group!{
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
            group!{
                vec![],
                expr!(String, "[", "#"),
                group!{
                    vec!["+", "##"],
                    expr!(String, "[", "!"),
                    expr!(String, "]", "!"),
                    expr!(Id, ".Symbol.LineEnd", "!"),
                    group!{
                        vec!["##"],
                        group!{
                            vec![":"],
                            group!{
                                vec![],
                                expr!(String, "\\["),
                            },
                            group!{
                                vec![],
                                expr!(String, "\\]"),
                            },
                            group!{
                                vec![],
                                expr!(String, "\\\\"),
                            },
                            group!{
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
            group!{
                vec![],
                expr!(String, "."),
            },
        };

        return block!(".Rule", vec![pure_choice_rule, choice_rule, seq_rule, seq_elem_rule, expr_rule, lookahead_rule, loop_rule, loop_range_rule, random_order_rule, random_order_range_rule, ast_reflection_rule, num_rule, id_rule, arg_id_rule, generics_rule, template_rule, esc_seq_rule, str_rule, char_class_rule, wildcard_rule]);
    }
}
