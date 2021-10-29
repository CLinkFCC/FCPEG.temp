use std::collections::*;

use crate::*;
use crate::data::*;
use crate::parser::*;
use crate::rule::*;

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
            choice.ast_reflection = ASTReflection::Reflectable("".to_string());

            for opt in $options {
                match opt {
                    "&" | "!" => choice.lookahead_kind = RuleLookaheadKind::to_lookahead_kind(opt),
                    "?" | "*" | "+" => choice.loop_count = RuleCountConverter::loop_symbol_to_count(opt),
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
            ast_reflection: ASTReflection::Unreflectable(),
            value: "".to_string(),
        }
    };

    ($kind:ident, $value:expr $(, $option:expr) *) => {
        {
            let mut expr = expr!($kind);
            expr.value = $value.to_string();

            let leaf_name = match RuleExpressionKind::$kind {
                RuleExpressionKind::ID => $value.to_string(),
                _ => "".to_string(),
            };

            expr.ast_reflection = ASTReflection::Reflectable(leaf_name);

            $(
                match $option {
                    "&" | "!" => expr.lookahead_kind = RuleLookaheadKind::to_lookahead_kind($option),
                    "?" | "*" | "+" => expr.loop_count = RuleCountConverter::loop_symbol_to_count($option),
                    "#" => expr.ast_reflection = ASTReflection::Unreflectable(),
                    _ => panic!(),
                }
            )*

            RuleElementContainer::RuleExpression(Box::new(expr))
        }
    };
}

pub type BlockMap = HashMap<String, Block>;

#[derive(PartialOrd, PartialEq, Debug, Clone)]
pub enum BlockTokenKind {
    ID,
    Number,
    Space,
    String,
    StringInBracket,
    Symbol,
}

impl std::fmt::Display for BlockTokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BlockTokenKind::ID => return write!(f, "ID"),
            BlockTokenKind::Number => return write!(f, "Number"),
            BlockTokenKind::Space => return write!(f, "Space"),
            BlockTokenKind::String => return write!(f, "String"),
            BlockTokenKind::StringInBracket => return write!(f, "StringInBracket"),
            BlockTokenKind::Symbol => return write!(f, "Symbol"),
        }
    }
}

#[derive(Clone)]
pub struct BlockToken {
    pub line: usize,
    pub kind: BlockTokenKind,
    pub value: String,
}

impl BlockToken {
    pub fn new(line: usize, kind: BlockTokenKind, value: String) -> BlockToken {
        return BlockToken {
            line: line,
            kind: kind,
            value: value,
        }
    }
}

#[derive(Debug)]
pub enum BlockParseError {
    Unknown(),
    BlockAliasNotFound(usize, String),
    DuplicatedBlockAliasName(usize, String),
    DuplicatedBlockName(usize, String),
    DuplicatedStartCmd(),
    ExpectedBlockDef(usize),
    ExpectedToken(usize, String),
    InternalErr(String),
    InvalidCharClassFormat(usize, String),
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

impl BlockParseError {
    pub fn get_log_data(&self) -> ConsoleLogData {
        match self {
            BlockParseError::Unknown() => ConsoleLogData::new(ConsoleLogKind::Error, "unknown error", vec![], vec![]),
            BlockParseError::BlockAliasNotFound(line, block_alias_name) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("block alias '{}' not found", block_alias_name), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::DuplicatedBlockAliasName(line, block_alias_name) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("duplicated block alias name '{}'", block_alias_name), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::DuplicatedBlockName(line, block_name) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("duplicated block name '{}'", block_name), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::DuplicatedStartCmd() => ConsoleLogData::new(ConsoleLogKind::Error, "duplicated start command", vec![], vec![]),
            BlockParseError::ExpectedBlockDef(line) => ConsoleLogData::new(ConsoleLogKind::Error, "expected block definition", vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::ExpectedToken(line, expected_str) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("expected token {}", expected_str), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::InternalErr(err_name) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("internal error: {}", err_name), vec![], vec![]),
            BlockParseError::InvalidCharClassFormat(line, value) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("invalid character class format '{}'", value), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::InvalidToken(line, value) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("invalid token '{}'", value), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::MainBlockNotFound() => ConsoleLogData::new(ConsoleLogKind::Error, "main block not found", vec![], vec![]),
            BlockParseError::NoChoiceOrExpressionContent(line) => ConsoleLogData::new(ConsoleLogKind::Error, "no choice or expression content", vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::NoStartCmdInMainBlock() => ConsoleLogData::new(ConsoleLogKind::Error, "no start command in main block", vec![], vec![]),
            BlockParseError::RuleHasNoChoice(rule_name) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("rule '{}' has no choice", rule_name), vec![], vec![]),
            BlockParseError::RuleInMainBlock() => ConsoleLogData::new(ConsoleLogKind::Error, "rule in main block", vec![], vec![]),
            BlockParseError::StartCmdOutsideMainBlock() => ConsoleLogData::new(ConsoleLogKind::Error, "start command outside main block", vec![], vec![]),
            BlockParseError::TooBigNumber(line, number) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("too big number {}", number), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::UnexpectedEOF(line, expected_str) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("unexpected EOF, expected {}", expected_str), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::UnexpectedToken(line, unexpected_token, expected_str) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("unexpected token '{}', expected {}", unexpected_token, expected_str), vec![format!("line:\t{}", line + 1)], vec![]),
            BlockParseError::UnknownPragmaName(line, unknown_pragma_name) => ConsoleLogData::new(ConsoleLogKind::Error, "unknown pragma name", vec![format!("line:\t{}", line + 1), format!("pragma name:\t{}", unknown_pragma_name)], vec![]),
            BlockParseError::UnknownSyntax(line, target_token) => ConsoleLogData::new(ConsoleLogKind::Error, "unknown syntax", vec![format!("line: {}", line + 1), format!("target token:\t'{}'", target_token)], vec![]),
            BlockParseError::UnknownToken(line, unknown_token) => ConsoleLogData::new(ConsoleLogKind::Error, &format!("unknown token '{}'", unknown_token), vec![format!("line:\t{}", line + 1)], vec![]),
        }
    }
}

pub struct BlockParser {}

impl BlockParser {
    // note: FileMan から最終的な RuleMap を取得する
    pub fn get_rule_map(fcpeg_file_man: &mut FCPEGFileMan) -> Result<RuleMap, SyntaxParseError> {
        let mut tmp_file_man = FCPEGFileMan::new("".to_string(), "".to_string());
        tmp_file_man.block_map = FCPEGBlock::get_block_map();
        let mut fcpeg_rule_map = RuleMap::new(".Syntax.FCPEG".to_string());
        match fcpeg_rule_map.add_rules_from_fcpeg_file_man(&tmp_file_man) { Ok(()) => (), Err(e) => { let mut cons = Console::new(); cons.log(e.get_log_data(), false); panic!(); } };
        let mut parser = SyntaxParser::new(fcpeg_rule_map)?;

        BlockParser::set_block_map_to_all_files(&mut parser, fcpeg_file_man)?;

        let mut rule_map = RuleMap::new("Main".to_string());
        rule_map.add_rules_from_fcpeg_file_man(fcpeg_file_man).unwrap();

        return Ok(rule_map);
    }

    // note: 全ファイルに BlockMap を設定する
    fn set_block_map_to_all_files(parser: &mut SyntaxParser, fcpeg_file_man: &mut FCPEGFileMan) -> Result<(), SyntaxParseError> {
        let tree = BlockParser::to_syntax_tree(parser, fcpeg_file_man)?;
        fcpeg_file_man.block_map = BlockParser::to_block_map(&tree);

        for sub_file_man in fcpeg_file_man.sub_file_aliase_map.values_mut() {
            BlockParser::set_block_map_to_all_files(parser, sub_file_man)?;
        }

        return Ok(());
    }

    // note: ブロックマップとファイルを元に 1 ファイルの FCPEG コードの構文木を取得する
    fn to_syntax_tree(parser: &mut SyntaxParser, fcpeg_file_man: &FCPEGFileMan) -> Result<SyntaxTree, SyntaxParseError> {
        let tree = parser.get_syntax_tree(fcpeg_file_man.fcpeg_file_content.clone())?;

        println!("print {}", fcpeg_file_man.fcpeg_file_content);
        tree.print(false);

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

        // code: FCPEG <- Symbol.Space*# Symbol.LineEnd*# (Block.Block Symbol.LineEnd+#)* Symbol.LineEnd*# Symbol.Space*#,
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
                    choice!{
                        vec![],
                        expr!(Wildcard, "."),
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

        // code: Cmd <- Comment : DefineCmd : StartCmd : UseCmd,
        let cmd_rule = rule!{
            "Cmd",
            choice!{
                vec![],
                choice!{
                    vec![":"],
                    choice!{
                        vec![],
                        expr!(ID, "Comment"),
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
            },
        };

        // code: Comment <- "%"# (!"," !Symbol.LineEnd .)* ","#,
        let comment_rule = rule!{
            "Comment",
            choice!{
                vec![],
                expr!(String, "%", "#"),
                choice!{
                    vec!["*"],
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
                expr!(String, "as", "#"),
                expr!(ID, "Symbol.Space", "+", "#"),
                expr!(ID, "Misc.SingleID"),
            },
        };

        return block!("Block", vec![misc_use, rule_use, symbol_use, block_rule, cmd_rule, comment_rule, define_cmd_rule, start_cmd_rule, use_cmd_rule, use_cmd_block_alias_rule]);
    }

    fn get_rule_block() -> Block {
        let misc_use = use_block!("Misc");
        let symbol_use = use_block!("Symbol");

        // code: PureChoice <- Seq (Symbol.Space# ":"# Symbol.Space# Seq)*,
        let pure_choice_rule = rule!{
            "PureChoice",
            choice!{
                vec![],
                expr!(ID, "Seq"),
                choice!{
                vec!["*"],
                    expr!(ID, "Symbol.Space", "#"),
                    expr!(String, ":", "#"),
                    expr!(ID, "Symbol.Space", "#"),
                    expr!(ID, "Seq"),
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

        // code: Seq <- SeqElem (Symbol.Space+# SeqElem)*,
        let seq_rule = rule!{
            "Seq",
            choice!{
                vec![],
                expr!(ID, "SeqElem"),
                choice!{
                    vec!["*"],
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

        // code: SeqElem <- Lookahead? (Choice : Expr) Loop? RandomOrder? ASTReflection?,
        let seq_elem_rule = rule!{
            "SeqElem",
            choice!{
                vec![],
                expr!(ID, "Lookahead", "?"),
                choice!{
                    vec![],
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
                expr!(ID, "ASTReflection", "?"),
            },
        };

        // code: Expr <- ID : Str : CharClass : Wildcard,
        let expr_rule = rule!{
            "Expr",
            choice!{
                vec![],
                choice!{
                    vec![":"],
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

        // code: LoopRange <- "{"# Num? ","# Num? "}"#,
        let loop_range_rule = rule!{
            "LoopRange",
            choice!{
                vec![],
                expr!(String, "{", "#"),
                expr!(ID, "Num", "?"),
                expr!(String, ",", "#"),
                expr!(ID, "Num", "?"),
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

        // code: ASTReflection <- "#"# Misc.SingleID?,
        let ast_reflection_rule = rule!{
            "ASTReflection",
            choice!{
                vec![],
                expr!(String, "#", "#"),
                expr!(ID, "Misc.SingleID", "?"),
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

        // code: ID <- Misc.ChainID,
        let id_rule = rule!{
            "ID",
            choice!{
                vec![],
                expr!(ID, "Misc.ChainID"),
            },
        };

        // code: EscSeq <- "\\" ("\\" : "0" : "\"" : "n"),
        let esc_seq_rule = rule!{
            "EscSeq",
            choice!{
                vec![],
                expr!(String, "\\"),
                choice!{
                    vec![],
                    choice!{
                        vec![":"],
                        choice!{
                            vec![],
                            expr!(String, "\\"),
                        },
                        choice!{
                            vec![],
                            expr!(String, "0"),
                        },
                        choice!{
                            vec![],
                            expr!(String, "\""),
                        },
                        choice!{
                            vec![],
                            expr!(String, "n"),
                        },
                    },
                },
            },
        };

        // code: Str <- "\""# ((EscSeq : !(("\\" : "\"")) .))+ "\""#,
        let str_rule = rule!{
            "Str",
            choice!{
                vec![],
                expr!(String, "\"", "#"),
                choice!{
                    vec!["+"],
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

        // code: CharClass <- "["# (!"[" !"]" !Symbol.LineEnd ("\\[" : "\\]" : "\\\\" : .))+ "]"#,
        let char_class_rule = rule!{
            "CharClass",
            choice!{
                vec![],
                expr!(String, "[", "#"),
                choice!{
                    vec!["+"],
                    expr!(String, "[", "!"),
                    expr!(String, "]", "!"),
                    expr!(ID, "Symbol.LineEnd", "!"),
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

        return block!("Rule", vec![misc_use, symbol_use, pure_choice_rule, choice_rule, seq_rule, seq_elem_rule, expr_rule, lookahead_rule, loop_rule, loop_range_rule, random_order_rule, random_order_range_rule, ast_reflection_rule, num_rule, id_rule, esc_seq_rule, str_rule, char_class_rule, wildcard_rule]);
    }
}
