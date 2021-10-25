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

macro_rules! def_cmd {
    ($rule_name:expr, $sub_elems:expr) => {
        BlockCommand::Define(0, Rule::new($rule_name.to_string(), vec![], $sub_elems))
    };
}

macro_rules! start_cmd {
    ($file_alias_name:expr, $block_name:expr, $rule_name:expr) => {
        BlockCommand::Start(0, $file_alias_name.to_string(), $block_name.to_string(), $rule_name.to_string())
    };
}

macro_rules! choice_elem {
    ($choice:expr) => {
        RuleElementContainer::RuleChoice(Box::new($choice))
    };
}

macro_rules! expr_elem {
    ($expr:expr) => {
        RuleElementContainer::RuleExpression(Box::new($expr))
    };
}

macro_rules! expr {
    ($kind:ident, $value:expr) => {
        RuleExpression {
            line: 0,
            kind: RuleExpressionKind::$kind,
            lookahead_kind: RuleLookaheadKind::None,
            loop_count: (1, 1),
            ast_reflection: ASTReflection::Reflectable("".to_string()),
            value: $value.to_string(),
        }
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

    ($($field_name:ident = $field_value:expr), *) => {
        {
            let mut choice = choice!();
            $(choice.$field_name = $field_value;)*
            choice
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
        fcpeg_rule_map.add_rules_from_fcpeg_file_man(&tmp_file_man).unwrap();
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
        let fcpeg_rule_def = def_cmd!("FCPEG", vec![
            Box::new(choice!(
                elem_containers = vec![
                    choice_elem!(choice!(
                        elem_containers = vec![
                            expr_elem!(expr!(String, "fcpeg"))
                        ]
                    ))
                ],
                ast_reflection = ASTReflection::Reflectable("Syntax".to_string()),
                has_choices = true
            )),
        ]);

        return block!("Syntax", vec![fcpeg_rule_def]);
    }

    /* FCPEG コードの構文木からブロックマップへの変換 */

    fn to_block_map(tree: &SyntaxTree) -> BlockMap {
        let mut block_map = BlockMap::new();
        block_map.insert("".to_string(), Block::new("Main".to_string(), vec![]));
        return block_map;
    }
}
