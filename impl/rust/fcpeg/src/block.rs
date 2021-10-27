use std::collections::*;

use crate::*;
use crate::data::*;
use crate::parser::*;
use crate::rule::*;

pub type BlockMap = HashMap<String, Block>;

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
            let choices = vec![$(
                match $choice {
                    RuleElementContainer::RuleChoice(v) => v,
                    _ => panic!(),
                }
            )*];

            let rule = Rule::new($rule_name.to_string(), vec![], choices);
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
    () => {
        RuleChoice {
            elem_containers: vec![],
            lookahead_kind: RuleLookaheadKind::None,
            loop_count: (1, 1),
            ast_reflection: ASTReflection::Unreflectable(),
            is_random_order: false,
            occurrence_count: (1, 1),
            has_choices: false,
        }
    };

    ($options:expr, $($sub_elem:expr), *,) => {
        {
            let mut choice = choice!();
            choice.elem_containers = vec![$($sub_elem,)*];

            for opt in $options {
                match opt {
                    "!" => choice.lookahead_kind = RuleLookaheadKind::Negative,
                    "&" => choice.lookahead_kind = RuleLookaheadKind::Positive,
                    "?" => choice.loop_count = (0, 1),
                    "*" => choice.loop_count = (0, -1),
                    "+" => choice.loop_count = (1, -1),
                    "#" => choice.ast_reflection = ASTReflection::Unreflectable(),
                    ":" => choice.has_choices = true,
                    _ => panic!(),
                }
            }

            // $(choice.$field_name = $field_value;)*
            RuleElementContainer::RuleChoice(Box::new(choice))
        }
    };
}

macro_rules! expr {
    ($kind:ident) => {
        RuleExpression {
            line: 0,
            kind: RuleExpressionKind::$kind,
            lookahead_kind: RuleLookaheadKind::None,
            loop_count: (1, 1),
            ast_reflection: ASTReflection::Reflectable("".to_string()),
            value: "".to_string(),
        }
    };

    ($kind:ident, $value:expr $(, $option:expr) *) => {
        {
            let mut expr = expr!($kind);
            expr.value = $value.to_string();

            $(
                match $option {
                    "!" => expr.lookahead_kind = RuleLookaheadKind::Negative,
                    "&" => expr.lookahead_kind = RuleLookaheadKind::Positive,
                    "?" => expr.loop_count = (0, 1),
                    "*" => expr.loop_count = (0, -1),
                    "+" => expr.loop_count = (1, -1),
                    "#" => expr.ast_reflection = ASTReflection::Unreflectable(),
                    _ => panic!(),
                }
            )*

            RuleElementContainer::RuleExpression(Box::new(expr))
        }
    };
}

pub struct BlockParserA {}

impl BlockParserA {
    // FileMan から最終的な RuleMap を取得する
    pub fn get_rule_map(fcpeg_file_man: &mut FCPEGFileMan) -> Result<RuleMap, SyntaxParseError> {
        let mut tmp_file_man = FCPEGFileMan::new("".to_string(), "".to_string());
        tmp_file_man.block_map = FCPEGBlock::get_block_map();
        let mut fcpeg_rule_map = RuleMap::new(".Syntax.FCPEG".to_string());
        match fcpeg_rule_map.add_rules_from_fcpeg_file_man(&tmp_file_man) { Ok(()) => (), Err(e) => { let mut cons = Console::new(); cons.log(e.get_log_data(), false); panic!(); } };
        let mut parser = SyntaxParser::new(fcpeg_rule_map)?;

        BlockParserA::set_block_map_to_all_files(&mut parser, fcpeg_file_man)?;

        let mut rule_map = RuleMap::new("Main".to_string());
        rule_map.add_rules_from_fcpeg_file_man(fcpeg_file_man).unwrap();

        return Ok(rule_map);
    }

    // 全ファイルに BlockMap を設定する
    fn set_block_map_to_all_files(parser: &mut SyntaxParser, fcpeg_file_man: &mut FCPEGFileMan) -> Result<(), SyntaxParseError> {
        let tree = BlockParserA::to_syntax_tree(parser, fcpeg_file_man)?;
        fcpeg_file_man.block_map = BlockParserA::to_block_map(&tree);

        for sub_file_man in fcpeg_file_man.sub_file_aliase_map.values_mut() {
            BlockParserA::set_block_map_to_all_files(parser, sub_file_man)?;
        }

        return Ok(());
    }

    // ブロックマップとファイルを元に 1 ファイルの FCPEG コードの構文木を取得する
    fn to_syntax_tree(parser: &mut SyntaxParser, fcpeg_file_man: &FCPEGFileMan) -> Result<SyntaxTree, SyntaxParseError> {
        let tree = parser.get_syntax_tree(fcpeg_file_man.fcpeg_file_content.clone())?;

        println!("print");
        tree.print(true);

        return Ok(tree);
    }

    // note: FCPEG コードの構文木 → ブロックマップの変換
    fn to_block_map(tree: &SyntaxTree) -> BlockMap {
        let mut block_map = BlockMap::new();
        block_map.insert("".to_string(), Block::new("Main".to_string(), vec![]));
        return block_map;
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
        };
    }

    fn get_main_block() -> Block {
        let start_cmd = start_cmd!("", "Syntax", "FCPEG");
        return block!("Main", vec![start_cmd]);
    }

    fn get_syntax_block() -> Block {
        let block_use = use_block!("Block");
        let symbol_use = use_block!("Symbol");

        // code: FCPEG <- Symbol.Space*# Symbol.LineEnd*# (Block.Block Symbol.LineEnd+#)* Symbol.LineEnd*# Symbol.Space*#,
        let fcpeg_rule = rule!{
            "FCPEG",
            choice!{
                vec![],
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(ID, "Symbol.LineEnd", "*", "#"),
                choice!{
                    vec!["*"],
                    expr!(ID, "Block.Block"),
                    expr!(ID, "Symbol.LineEnd", "+", "#"),
                },
                expr!(ID, "Symbol.LineEnd", "*", "#"),
                expr!(ID, "Symbol.Space", "*", "#"),
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

        return block!("Symbol", vec![space_rule, line_end_rule]);
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

        // code: ChainID <- SingleID ("." SingleID)*,
        let chain_id_rule = rule!{
            "ChainID",
            choice!{
                vec![],
                expr!(ID, "SingleID"),
                choice!{
                    vec!["*"],
                    expr!(Wildcard, "."),
                    expr!(ID, "SingleID"),
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
                    expr!(ID, "Cmd"),
                    expr!(ID, "Symbol.LineEnd", "+", "#"),
                },
                expr!(String, "}", "#"),
            },
        };

        // code: Cmd <- Comment : DefineCmd : StartCmd : UseCmd,
        let cmd_rule = rule!{
            "Cmd",
            choice!{
                vec![":"],
                expr!(ID, "Comment"),
                expr!(ID, "DefineCmd"),
                expr!(ID, "StartCmd"),
                expr!(ID, "UseCmd"),
            },
        };

        // code: Comment <- "%"# (!"," !Symbol.LineEnd .)* ",",
        let comment_rule = rule!{
            "Comment",
            choice!{
                vec![],
                expr!(String, "%", "#"),
                choice!{
                    vec!["*"],
                    expr!(String, ",", "!"),
                    expr!(ID, "Symbol.LineEnd", "!"),
                    expr!(Wildcard, "."),
                },
                expr!(String, ",", "#"),
            },
        };

        // code: DefineCmd <- Misc.SingleID Symbol.Space*# "<-"# Symbol.Space*# Rule.PureChoice Symbol.Space*# ","#,
        let define_cmd_rule = rule!{
            "DefineCmd",
            choice!{
                vec![],
                expr!(ID, "Misc.SingleID"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(String, "<-", "#"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(ID, "Rule.PureChoice"),
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(String, ",", "#"),
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
                expr!(String, "as", "+", "#"),
                expr!(ID, "Symbol.Space", "+", "#"),
                expr!(ID, "Misc.SingleID"),
            },
        };

        return block!("Block", vec![misc_use, rule_use, symbol_use, block_rule, cmd_rule, comment_rule, define_cmd_rule, start_cmd_rule, use_cmd_rule, use_cmd_block_alias_rule]);
    }
}
