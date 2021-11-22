use std::collections::*;

use crate::*;
use crate::data::*;
use crate::parser::*;
use crate::rule::*;

use rustnutlib::*;
use rustnutlib::console::*;

macro_rules! block_map {
    ($($block_name:expr => $func_name:ident), *,) => {
        {
            let mut block_map = BlockMap::new();
            $(block_map.insert($block_name.to_string(), FCPEGBlock::$func_name());)*
            block_map
        }
    };
}

macro_rules! block {
    ($block_name:expr, $cmds:expr) => {
        Block::new($block_name.to_string(), $cmds)
    };
}

macro_rules! use_block {
    ($block_name:expr) => {
        BlockCommand::Use(0, "".to_string(), $block_name.to_string(), $block_name.to_string())
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

            let mut root_group = RuleGroup::new(RuleGroupKind::Choice);
            root_group.sub_elems = sub_elems;
            root_group.ast_reflection_style = ASTReflectionStyle::Expansion;

            let rule = Rule::new($rule_name.to_string(), vec![], Box::new(root_group));
            BlockCommand::Define(0, rule)
        }
    };
}

macro_rules! start_cmd {
    ($file_alias_name:expr, $block_name:expr, $rule_name:expr) => {
        BlockCommand::Start(0, $file_alias_name.to_string(), $block_name.to_string(), $rule_name.to_string())
    };
}

macro_rules! choice {
    ($options:expr, $($sub_elem:expr), *,) => {
        {
            let mut group = RuleGroup::new(RuleGroupKind::Sequence);
            group.sub_elems = vec![$($sub_elem,)*];
            group.ast_reflection_style = ASTReflectionStyle::Reflection("".to_string());

            for opt in $options {
                match opt {
                    "&" | "!" => group.lookahead_kind = RuleElementLookaheadKind::new(opt),
                    "?" | "*" | "+" => group.loop_count = RuleElementLoopCount::from_symbol(opt),
                    "#" => group.ast_reflection_style = ASTReflectionStyle::NoReflection,
                    "##" => group.ast_reflection_style = ASTReflectionStyle::Expansion,
                    ":" => group.kind = RuleGroupKind::Choice,
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
            let mut expr = RuleExpression::new(0, RuleExpressionKind::$kind, $value.to_string());

            let leaf_name = match RuleExpressionKind::$kind {
                RuleExpressionKind::ID => $value.to_string(),
                _ => "".to_string(),
            };

            expr.ast_reflection_style = ASTReflectionStyle::Reflection(leaf_name);

            $(
                match $option {
                    "&" | "!" => expr.lookahead_kind = RuleElementLookaheadKind::new($option),
                    "?" | "*" | "+" => expr.loop_count = RuleElementLoopCount::from_symbol($option),
                    "#" => expr.ast_reflection_style = ASTReflectionStyle::NoReflection,
                    "##" => expr.ast_reflection_style = ASTReflectionStyle::Expansion,
                    _ => panic!(),
                }
            )*

            RuleElement::Expression(Box::new(expr))
        }
    };
}

pub type BlockMap = HashMap<String, Block>;

pub type BlockParseResult<T> = Result<T, BlockParseError>;

#[derive(Debug)]
pub enum BlockParseError {
    Unknown(),
    BlockAliasNotFound(usize, String),
    DuplicatedBlockAliasName(usize, String),
    DuplicatedBlockName(usize, String),
    DuplicatedGenericsArgumentID(String),
    DuplicatedStartCmd(),
    ExpectedBlockDef(usize),
    ExpectedToken(usize, String),
    InternalErr(String),
    InvalidLoopCount(usize, String),
    InvalidToken(usize, String),
    MainBlockNotFound(),
    NoChoiceOrExpressionContent(usize),
    NoStartCmdInMainBlock(),
    RuleHasNoChoice(String),
    RuleInMainBlock(),
    StartCmdOutsideMainBlock(),
    TooBigNumber(usize, String),
    UnexpectedEOF(usize, String),
    UnexpectedToken(usize, String, String),
    UnknownPragmaName(usize, String),
    UnknownSyntax(usize, String),
    UnknownToken(usize, String),
}

impl ConsoleLogger for BlockParseError {
    fn get_log(&self) -> ConsoleLog {
        match self {
            BlockParseError::Unknown() => log!(Error, "unknown error"),
            BlockParseError::BlockAliasNotFound(line, block_alias_name) => log!(Error, &format!("block alias '{}' not found", block_alias_name), format!("line:\t{}", line + 1)),
            BlockParseError::DuplicatedBlockAliasName(line, block_alias_name) => log!(Error, &format!("duplicated block alias name '{}'", block_alias_name), format!("line:\t{}", line + 1)),
            BlockParseError::DuplicatedBlockName(line, block_name) => log!(Error, &format!("duplicated block name '{}'", block_name), format!("line:\t{}", line + 1)),
            BlockParseError::DuplicatedGenericsArgumentID(arg_name) => log!(Error, &format!("duplicated generics argument id '{}'", arg_name)),
            BlockParseError::DuplicatedStartCmd() => log!(Error, "duplicated start command"),
            BlockParseError::ExpectedBlockDef(line) => log!(Error, "expected block definition", format!("line:\t{}", line + 1)),
            BlockParseError::ExpectedToken(line, expected_str) => log!(Error, &format!("expected token {}", expected_str), format!("line:\t{}", line + 1)),
            BlockParseError::InternalErr(err_name) => log!(Error, &format!("internal error: {}", err_name)),
            BlockParseError::InvalidLoopCount(line, value) => log!(Error, &format!("invalid loop count: {}", value), format!("line:\t{}", line + 1)),
            BlockParseError::InvalidToken(line, value) => log!(Error, &format!("invalid token '{}'", value), format!("line:\t{}", line + 1)),
            BlockParseError::MainBlockNotFound() => log!(Error, "main block not found"),
            BlockParseError::NoChoiceOrExpressionContent(line) => log!(Error, "no choice or expression content", format!("line:\t{}", line + 1)),
            BlockParseError::NoStartCmdInMainBlock() => log!(Error, "no start command in main block"),
            BlockParseError::RuleHasNoChoice(rule_name) => log!(Error, &format!("rule '{}' has no choice", rule_name)),
            BlockParseError::RuleInMainBlock() => log!(Error, "rule in main block"),
            BlockParseError::StartCmdOutsideMainBlock() => log!(Error, "start command outside main block"),
            BlockParseError::TooBigNumber(line, number) => log!(Error, &format!("too big number {}", number), format!("line:\t{}", line + 1)),
            BlockParseError::UnexpectedEOF(line, expected_str) => log!(Error, &format!("unexpected EOF, expected {}", expected_str), format!("line:\t{}", line + 1)),
            BlockParseError::UnexpectedToken(line, unexpected_token, expected_str) => log!(Error, &format!("unexpected token '{}', expected {}", unexpected_token, expected_str), format!("line:\t{}", line + 1)),
            BlockParseError::UnknownPragmaName(line, unknown_pragma_name) => log!(Error, "unknown pragma name", format!("line:\t{}", line + 1), format!("pragma name:\t{}", unknown_pragma_name)),
            BlockParseError::UnknownSyntax(line, target_token) => log!(Error, "unknown syntax", format!("line: {}", line + 1), format!("target token:\t'{}'", target_token)),
            BlockParseError::UnknownToken(line, unknown_token) => log!(Error, &format!("unknown token '{}'", unknown_token), format!("line:\t{}", line + 1)),
        }
    }
}

pub struct BlockParser {}

impl BlockParser {
    // note: FileMan から最終的な RuleMap を取得する
    pub fn get_rule_map(fcpeg_file_map: &mut FCPEGFileMap) -> SyntaxParseResult<RuleMap> {
        let mut block_map = FCPEGBlock::get_block_map();
        let mut rule_map = RuleMap::new(".Syntax.FCPEG".to_string());

        match rule_map.format_block_map(&String::new(), &mut block_map) {
            Ok(()) => (),
            Err(e) => return Err(SyntaxParseError::BlockParseErr(e)),
        };

        let mut parser = SyntaxParser::new(rule_map.clone())?;
        // note: HashMap<エイリアス名, ブロックマップ>
        let mut block_maps = HashMap::<String, BlockMap>::new();

        for (alias_name, fcpeg_file) in fcpeg_file_map.iter() {
            let tree = BlockParser::to_syntax_tree(&mut parser, fcpeg_file.get_file_content())?;
            block_maps.insert(alias_name.clone(), BlockParser::to_block_map(&tree)?);
        }

        let main_block_id = "Main";
        let mut start_rule_id = Option::<String>::None;

        match block_maps.get(&String::new()) {
            Some(main_map) => {
                match main_map.get(main_block_id) {
                    Some(block) => {
                        for each_cmd in &block.cmds {
                            match each_cmd {
                                BlockCommand::Start(_, file_alias_name, block_name, rule_name) => {
                                    start_rule_id = Some(format!("{}.{}.{}", file_alias_name, block_name, rule_name));
                                },
                                _ => (),
                            }
                        }
                    },
                    None => return Err(SyntaxParseError::InternalErr(format!("unknown rule id '{}'", main_block_id))),
                }
            },
            None => return Err(SyntaxParseError::InternalErr("main file alias not found".to_string())),
        }

        let mut rule_map = match start_rule_id {
            Some(id) => RuleMap::new(id),
            None => return Err(SyntaxParseError::InternalErr(format!("start declaration not found"))),
        };

        for (file_alias_name, mut each_block_map) in block_maps.iter_mut() {
            match rule_map.format_block_map(&file_alias_name, &mut each_block_map) {
                Ok(()) => (),
                Err(e) => return Err(SyntaxParseError::BlockParseErr(e)),
            }
        }

        return Ok(rule_map);
    }

    // note: ブロックマップとファイルを元に 1 ファイルの FCPEG コードの構文木を取得する
    fn to_syntax_tree(parser: &mut SyntaxParser, fcpeg_file_content: &String) -> SyntaxParseResult<SyntaxTree> {
        let tree = parser.get_syntax_tree(fcpeg_file_content)?;

        println!("print {}", fcpeg_file_content);
        tree.print(true);

        return Ok(tree);
    }

    // note: FCPEG コードの構文木 → ブロックマップの変換
    fn to_block_map(tree: &SyntaxTree) -> SyntaxParseResult<BlockMap> {
        let mut block_map = BlockMap::new();
        let root = tree.clone_child();
        let block_nodes = root.get_node()?.get_node_child_at(0)?.get_reflectable_children();

        for each_block_elem in &block_nodes {
            let each_block_node = each_block_elem.get_node()?;
            let block_name = each_block_node.get_node_child_at(0)?.join_child_leaf_values();

            let mut cmds = Vec::<BlockCommand>::new();

            match each_block_node.get_node_child_at(1) {
                Ok(cmd_elems) => {
                    for each_cmd_elem in &cmd_elems.get_reflectable_children() {
                        let each_cmd_node = each_cmd_elem.get_node()?.get_node_child_at(0)?;
                        let new_cmd = BlockParser::to_block_cmd(each_cmd_node)?;
                        cmds.push(new_cmd);
                    }
                },
                Err(_) => (),
            }

            block_map.insert(block_name.clone(), Block::new(block_name.clone(), cmds));
        }

        block_map.insert("".to_string(), Block::new("Main".to_string(), vec![]));

        for (_, each_block) in &block_map {
            each_block.print();
            println!();
        }

        return Ok(block_map);
    }

    fn to_block_cmd(cmd_node: &SyntaxNode) -> SyntaxParseResult<BlockCommand> {
        return match &cmd_node.ast_reflection_style {
            ASTReflectionStyle::Reflection(node_name) => match node_name.as_str() {
                "CommentCmd" => BlockParser::to_comment_cmd(cmd_node),
                "DefineCmd" => BlockParser::to_define_cmd(cmd_node),
                "StartCmd" => BlockParser::to_start_cmd(cmd_node),
                "UseCmd" => BlockParser::to_use_cmd(cmd_node),
                _ => Err(SyntaxParseError::InvalidSyntaxTreeStructure(format!("invalid node name '{}'", node_name))),
            },
            _ => Err(SyntaxParseError::InvalidSyntaxTreeStructure("invalid operation".to_string())),
        };
    }

    fn to_comment_cmd(cmd_node: &SyntaxNode) -> SyntaxParseResult<BlockCommand> {
        return Ok(BlockCommand::Comment(0, cmd_node.join_child_leaf_values()));
    }

    fn to_define_cmd(cmd_node: &SyntaxNode) -> SyntaxParseResult<BlockCommand> {
        let rule_name = cmd_node.get_node_child_at(0)?.join_child_leaf_values();

        let generics_args = match cmd_node.find_first_child_node(vec!["DefineCmdGenericsIDs"]) {
            Some(generics_ids_node) => BlockParser::to_define_cmd_generics_ids(generics_ids_node)?,
            None => vec![],
        };

        let new_choice = match cmd_node.find_first_child_node(vec!["Rule.PureChoice"]) {
            Some(choice_node) => BlockParser::to_rule_choice_elem(choice_node, &generics_args)?,
            None => return Err(SyntaxParseError::InternalErr("pure choice not found".to_string())),
        };

        let rule = Rule::new(rule_name, generics_args, Box::new(new_choice));
        return Ok(BlockCommand::Define(0, rule));
    }

    fn to_define_cmd_generics_ids(cmd_node: &SyntaxNode) -> SyntaxParseResult<Vec<String>> {
        let mut args = Vec::<String>::new();

        for each_elem in &cmd_node.sub_elems {
            match each_elem {
                SyntaxNodeElement::Node(each_node) => {
                    if each_node.ast_reflection_style == ASTReflectionStyle::Reflection("Misc.SingleID".to_string()) {
                        let new_arg = each_node.join_child_leaf_values();

                        if args.contains(&new_arg) {
                            return Err(SyntaxParseError::BlockParseErr(BlockParseError::DuplicatedGenericsArgumentID(new_arg.clone())));
                        }

                        args.push(new_arg);
                    }
                },
                _ => (),
            }
        }

        return Ok(args);
    }

    fn to_start_cmd(cmd_node: &SyntaxNode) -> SyntaxParseResult<BlockCommand> {
        let raw_id = BlockParser::to_chain_id(cmd_node.get_node_child_at(0)?)?;
        let divided_raw_id = raw_id.split(".").collect::<Vec<&str>>();

        let cmd = match divided_raw_id.len() {
            2 => BlockCommand::Start(0, String::new(), divided_raw_id.get(0).unwrap().to_string(), divided_raw_id.get(1).unwrap().to_string()),
            3 => BlockCommand::Start(0, divided_raw_id.get(0).unwrap().to_string(), divided_raw_id.get(1).unwrap().to_string(), divided_raw_id.get(2).unwrap().to_string()),
            _ => return Err(SyntaxParseError::InternalErr("invalid chain ID length on start command".to_string())),
        };

        return Ok(cmd);
    }

    fn to_use_cmd(cmd_node: &SyntaxNode) -> SyntaxParseResult<BlockCommand> {
        let raw_id = BlockParser::to_chain_id(cmd_node.get_node_child_at(0)?)?;
        let divided_raw_id = raw_id.split(".").collect::<Vec<&str>>();

        let block_alias_id = match cmd_node.find_first_child_node(vec!["UseCmdBlockAlias"]) {
            Some(v) => v.get_node_child_at(0)?.join_child_leaf_values(),
            None => divided_raw_id.join("."),
        };

        return match divided_raw_id.len() {
            1 => Ok(BlockCommand::Use(0, "".to_string(), divided_raw_id.get(0).unwrap().to_string(), block_alias_id)),
            2 => Ok(BlockCommand::Use(0, divided_raw_id.get(0).unwrap().to_string(), divided_raw_id.get(1).unwrap().to_string(), block_alias_id)),
            _ => Err(SyntaxParseError::InternalErr("invalid chain ID length on use command".to_string())),
        };
    }

    // note: Seq を解析する
    fn to_seq_elem(seq_node: &SyntaxNode, generics_args: &Vec<String>) -> SyntaxParseResult<RuleElement> {
        // todo: 先読みなどの処理
        let mut children = Vec::<RuleElement>::new();

        // note: SeqElem ノードをループ
        for each_seq_elem_elem in &seq_node.get_reflectable_children() {
            let each_seq_elem_node = each_seq_elem_elem.get_node()?;

            // note: Lookahead ノード
            let lookahead_kind = match each_seq_elem_node.find_first_child_node(vec!["Lookahead"]) {
                Some(v) => {
                    match v.get_leaf_child_at(0)?.value.as_str() {
                        "&" => RuleElementLookaheadKind::Positive,
                        "!" => RuleElementLookaheadKind::Negative,
                        _ => return Err(SyntaxParseError::InvalidSyntaxTreeStructure(format!("unknown lookahead kind"))),
                    }
                },
                None => RuleElementLookaheadKind::None,
            };

            // note: Loop ノード
            let loop_count = match each_seq_elem_node.find_first_child_node(vec!["Loop"]) {
                Some(v) => {
                    match v.get_child_at(0)? {
                        SyntaxNodeElement::Node(node) => {
                            let min_str = node.get_leaf_child_at(0)?.value.clone();
                            let min_num = if min_str != "" {
                                match min_str.parse::<usize>() {
                                    Ok(v) => {
                                        if v == 0 {
                                            return Err(SyntaxParseError::BlockParseErr(BlockParseError::InvalidLoopCount(0, "zero specified at minimum count".to_string())));
                                        }

                                        v
                                    },
                                    Err(_) => return Err(SyntaxParseError::BlockParseErr(BlockParseError::InvalidLoopCount(0, "invalid minimum loop value".to_string()))),
                                }
                            } else {
                                0usize
                            };

                            let max_str = node.get_leaf_child_at(1)?.value.clone();
                            let max_num = if max_str != "" {
                                match max_str.parse::<usize>() {
                                    Ok(v) => Infinitable::Normal(v),
                                    Err(_) => return Err(SyntaxParseError::BlockParseErr(BlockParseError::InvalidLoopCount(0, "invalid maximum loop value".to_string()))),
                                }
                            } else {
                                Infinitable::Infinite
                            };

                            RuleElementLoopCount::new(min_num, max_num)
                        },
                        SyntaxNodeElement::Leaf(leaf) => {
                            match leaf.value.as_str() {
                                "?" | "*" | "+" => RuleElementLoopCount::from_symbol(&leaf.value),
                                _ => return Err(SyntaxParseError::InvalidSyntaxTreeStructure(format!("unknown lookahead kind"))),
                            }
                        }
                    }
                },
                None => RuleElementLoopCount::get_single_loop(),
            };

            // note: ASTReflectionStyle ノード
            // todo: 構成ファイルによって切り替える
            let ast_reflection_style = match each_seq_elem_node.find_first_child_node(vec!["ASTReflectionStyle"]) {
                Some(v) => {
                    match v.get_leaf_child_at(0) {
                        Ok(v) => {
                            if v.value == "##" {
                                ASTReflectionStyle::Expansion
                            } else {
                                ASTReflectionStyle::Reflection(v.value.clone())
                            }
                        },
                        Err(_) => ASTReflectionStyle::from_config(true, String::new()),
                    }
                },
                None => ASTReflectionStyle::from_config(false, String::new()),
            };

            // Choice または Expr ノード
            let choice_or_expr_node = match each_seq_elem_node.find_first_child_node(vec!["Choice", "Expr"]) {
                Some(v) => v,
                None => return Err(SyntaxParseError::InvalidSyntaxTreeStructure("invalid operation".to_string())),
            };

            match &choice_or_expr_node.ast_reflection_style {
                ASTReflectionStyle::Reflection(name) => {
                    let new_elem = match name.as_str() {
                        "Choice" => {
                            let mut new_choice = BlockParser::to_rule_choice_elem(choice_or_expr_node.get_node_child_at(0)?, generics_args)?;
                            new_choice.lookahead_kind = lookahead_kind;
                            new_choice.loop_count = loop_count;
                            new_choice.ast_reflection_style = ast_reflection_style;
                            RuleElement::Group(Box::new(new_choice))
                        },
                        "Expr" => {
                            let mut new_expr = BlockParser::to_rule_expr_elem(choice_or_expr_node, generics_args)?;
                            new_expr.lookahead_kind = lookahead_kind;
                            new_expr.loop_count = loop_count;
                            new_expr.ast_reflection_style = ast_reflection_style;
                            RuleElement::Expression(Box::new(new_expr))
                        },
                        _ => return Err(SyntaxParseError::InvalidSyntaxTreeStructure(format!("invalid node name '{}'", name))),
                    };

                    children.push(new_elem);
                },
                _ => return Err(SyntaxParseError::InvalidSyntaxTreeStructure("invalid operation".to_string())),
            };
        }

        let mut seq = RuleGroup::new(RuleGroupKind::Sequence);
        seq.sub_elems = children;
        return Ok(RuleElement::Group(Box::new(seq)));
    }

    // note: Rule.PureChoice ノードの解析
    fn to_rule_choice_elem(choice_node: &SyntaxNode, generics_args: &Vec<String>) -> SyntaxParseResult<RuleGroup> {
        let mut children = Vec::<RuleElement>::new();
        let mut group_kind = RuleGroupKind::Sequence;

        // Seq ノードをループ
        for seq_elem in &choice_node.get_reflectable_children() {
            match &seq_elem.as_ref() {
                SyntaxNodeElement::Node(node) => {
                    match &seq_elem.as_ref().get_ast_reflection() {
                        ASTReflectionStyle::Reflection(name) => if name == "Seq" {
                            let new_child = BlockParser::to_seq_elem(node, generics_args)?;
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

        let mut group = RuleGroup::new(group_kind.clone());
        group.sub_elems = children;

        let mut tmp_root_group = RuleGroup::new(RuleGroupKind::Sequence);
        tmp_root_group.sub_elems = vec![RuleElement::Group(Box::new(group))];

        return Ok(tmp_root_group);
    }

    fn to_rule_expr_elem(expr_node: &SyntaxNode, generics_args: &Vec<String>) -> SyntaxParseResult<RuleExpression> {
        let expr_child_node = expr_node.get_node_child_at(0)?;

        let (kind, value) = match &expr_child_node.ast_reflection_style {
            ASTReflectionStyle::Reflection(name) => {
                match name.as_str() {
                    "CharClass" => (RuleExpressionKind::CharClass, format!("[{}]", expr_child_node.join_child_leaf_values())),
                    "ID" => (RuleExpressionKind::ID, BlockParser::to_chain_id(expr_child_node.get_node_child_at(0)?)?),
                    "Generics" => {
                        let args = BlockParser::to_rule_choice_elem(expr_child_node.get_node_child_at(1)?.get_node_child_at(0)?, generics_args)?.extract().sub_elems;
                        let boxed_args = args.iter().map(|e| match e {
                            RuleElement::Group(group) => group.clone(),
                            RuleElement::Expression(_) => {
                                let mut new_group = RuleGroup::new(RuleGroupKind::Choice);
                                new_group.sub_elems = vec![e.clone()];
                                Box::new(new_group)
                            },
                        }).collect::<Vec<Box<RuleGroup>>>();

                        let generics = RuleExpressionKind::Generics(boxed_args);
                        let arg_id = expr_child_node.get_node_child_at(0)?.get_node_child_at(0)?.join_child_leaf_values();
                        (generics, arg_id)
                    },
                    "Str" => (RuleExpressionKind::String, BlockParser::to_string_value(expr_child_node)?),
                    "Wildcard" => (RuleExpressionKind::Wildcard, ".".to_string()),
                    _ => return Err(SyntaxParseError::InternalErr(format!("unknown expression name '{}'", name))),
                }
            },
            _ => return Err(SyntaxParseError::InternalErr("invalid operation".to_string())),
        };

        let expr = RuleExpression::new(0, kind, value);
        return Ok(expr);
    }

    fn to_string_value(str_node: &SyntaxNode) -> SyntaxParseResult<String> {
        let mut s = String::new();

        for each_elem in &str_node.sub_elems {
            match each_elem {
                SyntaxNodeElement::Node(node) => {
                    match node.ast_reflection_style {
                        ASTReflectionStyle::Reflection(_) => {
                            s += match node.get_leaf_child_at(0)?.value.as_str() {
                                "\\" => "\\",
                                "\"" => "\"",
                                "n" => "\n",
                                "t" => "\t",
                                "z" => "\0",
                                _ => return Err(SyntaxParseError::InternalErr("unknown escape sequence character".to_string())),
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

    fn to_chain_id(chain_id_node: &SyntaxNode) -> SyntaxParseResult<String> {
        let mut ids = Vec::<String>::new();

        for chain_id_elem in &chain_id_node.get_reflectable_children() {
            ids.push(chain_id_elem.get_node()?.join_child_leaf_values());
        }

        return Ok(ids.join("."));
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
        let block_use = use_block!("Block");
        let symbol_use = use_block!("Symbol");

        // code: FCPEG <- Symbol.Space*# Symbol.LineEnd*# (Block.Block Symbol.LineEnd+#)* Symbol.LineEnd*# Symbol.Space*# Symbol.EOF#,
        let fcpeg_rule = rule!{
            "FCPEG",
            choice!{
                vec![],
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(ID, "Symbol.LineEnd", "*", "#"),
                choice!{
                    vec!["*"],
                    choice!{
                        vec![],
                        expr!(ID, "Block.Block"),
                        expr!(ID, "Symbol.LineEnd", "+", "#"),
                    },
                },
                expr!(ID, "Symbol.LineEnd", "*", "#"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(String, "\0", "#"),
            },
        };

        return block!("Syntax", vec![block_use, symbol_use, fcpeg_rule]);
    }

    fn get_symbol_block() -> Block {
        // code: Space <- " ",
        let space_rule = rule!{
            "Space",
            choice!{
                vec![],
                expr!(String, " "),
            },
        };

        // code: LineEnd <- Space* "\n" Space*,
        let line_end_rule = rule!{
            "LineEnd",
            choice!{
                vec![],
                expr!(ID, "Space", "*"),
                expr!(String, "\n"),
                expr!(ID, "Space", "*"),
            },
        };

        // code: EOF <- "\z",
        let eof_rule = rule!{
            "EOF",
            choice!{
                vec![],
                expr!(String, "\0", "#"),
            },
        };

        return block!("Symbol", vec![space_rule, line_end_rule, eof_rule]);
    }

    fn get_misc_block() -> Block {
        // code: SingleID <- [a-zA-Z_] [a-zA-Z0-9_]*,
        let single_id_rule = rule!{
            "SingleID",
            choice!{
                vec![],
                expr!(CharClass, "[a-zA-Z_]"),
                expr!(CharClass, "[a-zA-Z0-9_]", "*"),
            },
        };

        // code: ChainID <- SingleID ("."# SingleID)*##,
        let chain_id_rule = rule!{
            "ChainID",
            choice!{
                vec![],
                expr!(ID, "SingleID"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec![],
                        expr!(String, ".", "#"),
                        expr!(ID, "SingleID"),
                    },
                },
            },
        };

        return block!("Misc", vec![single_id_rule, chain_id_rule]);
    }

    fn get_block_block() -> Block {
        let misc_use = use_block!("Misc");
        let rule_use = use_block!("Rule");
        let symbol_use = use_block!("Symbol");

        // code: Block <- "["# Symbol.Space*# Misc.SingleID Symbol.Space*# "]"# Symbol.Space*# "{"# Symbol.LineEnd+# (Cmd Symbol.LineEnd+#)* "}"#,
        let block_rule = rule!{
            "Block",
            choice!{
                vec![],
                expr!(String, "[", "#"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(ID, "Misc.SingleID"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(String, "]", "#"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(String, "{", "#"),
                expr!(ID, "Symbol.LineEnd", "+", "#"),
                choice!{
                    vec!["*"],
                    choice!{
                        vec![],
                        expr!(ID, "Cmd"),
                        expr!(ID, "Symbol.LineEnd", "+", "#"),
                    },
                },
                expr!(String, "}", "#"),
            },
        };

        // code: Cmd <- CommentCmd : DefineCmd : StartCmd : UseCmd,
        let cmd_rule = rule!{
            "Cmd",
            choice!{
                vec![":"],
                choice!{
                    vec![],
                    expr!(ID, "CommentCmd"),
                },
                choice!{
                    vec![],
                    expr!(ID, "DefineCmd"),
                },
                choice!{
                    vec![],
                    expr!(ID, "StartCmd"),
                },
                choice!{
                    vec![],
                    expr!(ID, "UseCmd"),
                },
            },
        };

        // code: CommentCmd <- "%"# (!"," !Symbol.LineEnd .)*## ","#,
        let comment_rule = rule!{
            "CommentCmd",
            choice!{
                vec![],
                expr!(String, "%", "#"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec![],
                        expr!(String, ",", "!"),
                        expr!(ID, "Symbol.LineEnd", "!"),
                        expr!(Wildcard, "."),
                    },
                },
                expr!(String, ",", "#"),
            },
        };

        // code: DefineCmd <- Misc.SingleID DefineCmdGenericsIDs? Symbol.Space*# "<-"# Symbol.Space*# Rule.PureChoice Symbol.Space*# ","#,
        let define_cmd_rule = rule!{
            "DefineCmd",
            choice!{
                vec![],
                expr!(ID, "Misc.SingleID"),
                expr!(ID, "DefineCmdGenericsIDs", "?"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(String, "<-", "#"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(ID, "Rule.PureChoice"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(String, ",", "#"),
            },
        };

        // code: DefineCmdGenericsIDs <- "("# Misc.SingleID (","# Symbol.Space# Misc.SingleID)*## ")"#,
        let define_cmd_generics_ids_rule = rule!{
            "DefineCmdGenericsIDs",
            choice!{
                vec![],
                expr!(String, "(", "#"),
                expr!(ID, "Misc.SingleID"),
                choice!{
                    vec!["*", "##"],
                    expr!(String, ",", "#"),
                    expr!(ID, "Symbol.Space", "#"),
                    expr!(ID, "Misc.SingleID"),
                },
                expr!(String, ")", "#"),
            },
        };

        // code: StartCmd <- "+"# Symbol.Space*# "start"# Symbol.Space+# Misc.ChainID Symbol.Space*# ","#,
        let start_cmd_rule = rule!{
            "StartCmd",
            choice!{
                vec![],
                expr!(String, "+", "#"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(String, "start", "#"),
                expr!(ID, "Symbol.Space", "+", "#"),
                expr!(ID, "Misc.ChainID"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(String, ",", "#"),
            },
        };

        // code: UseCmd <- "+"# Symbol.Space*# "use"# Symbol.Space+# Misc.ChainID UseCmdBlockAlias? Symbol.Space*# ","#,
        let use_cmd_rule = rule!{
            "UseCmd",
            choice!{
                vec![],
                expr!(String, "+", "#"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(String, "use", "#"),
                expr!(ID, "Symbol.Space", "+", "#"),
                expr!(ID, "Misc.ChainID"),
                expr!(ID, "UseCmdBlockAlias", "?"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(String, ",", "#"),
            },
        };

        // code: UseCmdBlockAlias <- Symbol.Space+# "as" Symbol.Space+# Misc.SingleID,
        let use_cmd_block_alias_rule = rule!{
            "UseCmdBlockAlias",
            choice!{
                vec![],
                expr!(ID, "Symbol.Space", "+", "#"),
                expr!(String, "as", "#"),
                expr!(ID, "Symbol.Space", "+", "#"),
                expr!(ID, "Misc.SingleID"),
            },
        };

        return block!("Block", vec![misc_use, rule_use, symbol_use, block_rule, cmd_rule, comment_rule, define_cmd_rule, define_cmd_generics_ids_rule, start_cmd_rule, use_cmd_rule, use_cmd_block_alias_rule]);
    }

    fn get_rule_block() -> Block {
        let misc_use = use_block!("Misc");
        let symbol_use = use_block!("Symbol");

        // code: PureChoice <- Seq ((Symbol.Space# ":" : ",")## Symbol.Space# Seq)*##,
        let pure_choice_rule = rule!{
            "PureChoice",
            choice!{
                vec![],
                expr!(ID, "Seq"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec!["##"],
                        choice!{
                            vec![":"],
                            choice!{
                                vec!["##"],
                                expr!(ID, "Symbol.Space", "#"),
                                expr!(String, ":"),
                            },
                            choice!{
                                vec!["##"],
                                expr!(String, ","),
                            },
                        },
                        expr!(ID, "Symbol.Space", "#"),
                        expr!(ID, "Seq"),
                    },
                },
            },
        };

        // code: Choice <- "("# PureChoice ")"#,
        let choice_rule = rule!{
            "Choice",
            choice!{
                vec![],
                expr!(String, "(", "#"),
                expr!(ID, "PureChoice"),
                expr!(String, ")", "#"),
            },
        };

        // code: Seq <- SeqElem (Symbol.Space+# SeqElem)*##,
        let seq_rule = rule!{
            "Seq",
            choice!{
                vec![],
                expr!(ID, "SeqElem"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec![],
                        choice!{
                            vec![],
                            expr!(ID, "Symbol.Space", "+", "#"),
                            expr!(ID, "SeqElem"),
                        },
                    },
                },
            },
        };

        // code: SeqElem <- Lookahead? (Choice : Expr) Loop? RandomOrder? ASTReflectionStyle?,
        let seq_elem_rule = rule!{
            "SeqElem",
            choice!{
                vec![],
                expr!(ID, "Lookahead", "?"),
                choice!{
                    vec!["##"],
                    choice!{
                        vec![":"],
                        choice!{
                            vec![],
                            expr!(ID, "Choice"),
                        },
                        choice!{
                            vec![],
                            expr!(ID, "Expr"),
                        },
                    },
                },
                expr!(ID, "Loop", "?"),
                expr!(ID, "RandomOrder", "?"),
                expr!(ID, "ASTReflectionStyle", "?"),
            },
        };

        // code: Expr <- Generics : ID : Str : CharClass : Wildcard,
        let expr_rule = rule!{
            "Expr",
            choice!{
                vec![],
                choice!{
                    vec![":"],
                    choice!{
                        vec![],
                        expr!(ID, "Generics"),
                    },
                    choice!{
                        vec![],
                        expr!(ID, "ID"),
                    },
                    choice!{
                        vec![],
                        expr!(ID, "Str"),
                    },
                    choice!{
                        vec![],
                        expr!(ID, "CharClass"),
                    },
                    choice!{
                        vec![],
                        expr!(ID, "Wildcard"),
                    },
                },
            },
        };

        // code: Lookahead <- "!" : "&",
        let lookahead_rule = rule!{
            "Lookahead",
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
            "Loop",
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
                        expr!(ID, "LoopRange"),
                    },
                },
            },
        };

        // code: LoopRange <- "{"# (Num : "")## ","# (Num : "")## "}"#,
        let loop_range_rule = rule!{
            "LoopRange",
            choice!{
                vec![],
                expr!(String, "{", "#"),
                choice!{
                    vec![":"],
                    choice!{
                        vec!["##"],
                        expr!(ID, "Num", "##"),
                    },
                    choice!{
                        vec!["##"],
                        expr!(String, ""),
                    },
                },
                expr!(String, ",", "#"),
                choice!{
                    vec![":"],
                    choice!{
                        vec!["##"],
                        expr!(ID, "Num", "##"),
                    },
                    choice!{
                        vec!["##"],
                        expr!(String, ""),
                    },
                },
                expr!(String, "}", "#"),
            },
        };

        // expr: RandomOrder <- "^"# RandomOrderRange?,
        let random_order_rule = rule!{
            "RandomOrder",
            choice!{
                vec![],
                expr!(String, "^", "#"),
                expr!(String, "RandomOrderRange", "?"),
            },
        };

        // code: RandomOrderRange <- "["# Num? ","# Num? "]"#,
        let random_order_range_rule = rule!{
            "RandomOrderRange",
            choice!{
                vec![],
                expr!(String, "[", "#"),
                expr!(ID, "Num", "?"),
                expr!(String, "ID", "#"),
            },
        };

        // code: ASTReflectionStyle <- "##" : "#"# Misc.SingleID?##,
        let ast_reflection_rule = rule!{
            "ASTReflectionStyle",
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
                        expr!(ID, "Misc.SingleID", "?", "##"),
                    },
                },
            },
        };

        // code: Num <- [0-9]+,
        let num_rule = rule!{
            "Num",
            choice!{
                vec![],
                expr!(CharClass, "[0-9]+", "+"),
            },
        };

        // code: Generics <- ID## Choice,
        let generics_rule = rule!{
            "Generics",
            choice!{
                vec![],
                expr!(ID, "ID", "##"),
                expr!(ID, "Choice"),
            },
        };

        // code: ID <- Misc.ChainID,
        let id_rule = rule!{
            "ID",
            choice!{
                vec![],
                expr!(ID, "Misc.ChainID"),
            },
        };

        // code: EscSeq <- "\\"# ("\\" : "\"" : "n" : "t" : "z")##,
        let esc_seq_rule = rule!{
            "EscSeq",
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
            "Str",
            choice!{
                vec![],
                expr!(String, "\"", "#"),
                choice!{
                    vec!["*", "##"],
                    choice!{
                        vec![":"],
                        choice!{
                            vec![],
                            expr!(ID, "EscSeq"),
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
            "CharClass",
            choice!{
                vec![],
                expr!(String, "[", "#"),
                choice!{
                    vec!["+", "##"],
                    expr!(String, "[", "!"),
                    expr!(String, "]", "!"),
                    expr!(ID, "Symbol.LineEnd", "!"),
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

        // code: Wildcard <- "."#,
        let wildcard_rule = rule!{
            "Wildcard",
            choice!{
                vec![],
                expr!(String, ".", "#"),
            },
        };

        return block!("Rule", vec![misc_use, symbol_use, pure_choice_rule, choice_rule, seq_rule, seq_elem_rule, expr_rule, lookahead_rule, loop_rule, loop_range_rule, random_order_rule, random_order_range_rule, ast_reflection_rule, num_rule, id_rule, generics_rule, esc_seq_rule, str_rule, char_class_rule, wildcard_rule]);
    }
}
