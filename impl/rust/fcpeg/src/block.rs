use std::collections::*;

use crate::*;
use crate::data::*;
use crate::parser::*;
use crate::rule::*;

pub type BlockMap = HashMap<String, Block>;

macro_rules! block {
    ($block_name:expr, $cmds:expr) => {
        Block::new($block_name.to_string(), $cmds)
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
                    "?" => choice.loop_count = (0, 1),
                    "*" => choice.loop_count = (0, -1),
                    "+" => choice.loop_count = (1, -1),
                    "#" => choice.ast_reflection = ASTReflection::Unreflectable(),
                    ":" => choice.has_choices = true,
                    _ => (),
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
                    "?" => expr.loop_count = (0, 1),
                    "*" => expr.loop_count = (0, -1),
                    "+" => expr.loop_count = (1, -1),
                    "#" => expr.ast_reflection = ASTReflection::Unreflectable(),
                    _ => (),
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
        tmp_file_man.block_map = BlockParserA::get_fcpeg_block_map();
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
        tree.print(false);

        return Ok(tree);
    }

    /* FCPEG 解析用ブロック */

    fn get_fcpeg_block_map() -> BlockMap {
        let mut block_map = BlockMap::new();

        block_map.insert("Main".to_string(), BlockParserA::get_main_block());
        block_map.insert("Syntax".to_string(), BlockParserA::get_syntax_block());

        return block_map;
    }

    fn get_main_block() -> Block {
        let start_cmd = start_cmd!("", "Syntax", "FCPEG");
        return block!("Main", vec![start_cmd]);
    }

    fn get_syntax_block() -> Block {
        // code: FCPEG <- Symbol.Space*# Symbol.LineEnd*# (Block.Block Symbol.LineEnd+#)* Symbol.LineEnd*# Symbol.Space*#,
        /*let fcpeg_rule_def = rule!{
            "FCPEG",
            choice!{
                vec![],
                expr!(ID, "Symbol.Space", "*", "#"),
                expr!(ID, "Symbol.LineEnd", "*", "#"),
                choice!{
                    vec!["*", "#"],
                    expr!(ID, "Symbol.LineEnd", "+", "#"),
                },
            },
        };*/

        let fcpeg_rule_def = rule!{
            "FCPEG",
            choice!{
                vec![":"],
                expr!(String, "|"),
                expr!(String, "|"),
                expr!(String, " ", "?"),
                expr!(String, "|", "*"),
            },
        };

        return block!("Syntax", vec![fcpeg_rule_def]);
    }

    /* FCPEG コードの構文木からブロックマップへの変換 */

    fn to_block_map(tree: &SyntaxTree) -> BlockMap {
        let mut block_map = BlockMap::new();
        block_map.insert("".to_string(), Block::new("Main".to_string(), vec![]));
        return block_map;
    }
}
