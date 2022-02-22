use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::*;
use std::rc::Rc;
use std::sync::Arc;

use crate::*;
use crate::block::*;
use crate::cons::*;
use crate::parser::*;
use crate::rule::*;

use cons_util::*;
use cons_util::cons::*;
use cons_util::file::*;

#[derive(Clone, PartialEq)]
pub enum ConfigurationLog {
    AstReflectionStyleIsUnknown { value: String },
    EscapeSequenceCharacterIsUnknown { escseq_char: String },
    ExpectedValuesOfPropertyProvided { prop_name: String, unexpected_len: usize, expected_len: usize },
    FileAliasNameIsDuplicate { alias_name: String },
    HierarchyStructureIsInvalid,
    PropertyNameIsDuplicate { prop_name: String },
    PropertyNameNotFound { prop_name: String },
    RegexModeIsUnknown { value: String },
    ValueOfPropertyIsInvalid { prop_name: String, prop_value: String },
}

impl ConsoleLogger for ConfigurationLog {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            ConfigurationLog::AstReflectionStyleIsUnknown { value } => log!(Error, Translator::AstReflectionStyleIsUnknown { value: value.clone() }),
            ConfigurationLog::EscapeSequenceCharacterIsUnknown { escseq_char } => log!(Error, Translator::EscapeSequenceCharacterIsUnknown { escseq_char: escseq_char.clone() }),
            ConfigurationLog::ExpectedValuesOfPropertyProvided { prop_name, unexpected_len, expected_len } => log!(Error, Translator::ExpectedValuesOfPropertyProvided { prop_name: prop_name.clone(), unexpected_len: *unexpected_len, expected_len: *expected_len }),
            ConfigurationLog::FileAliasNameIsDuplicate { alias_name } => log!(Error, Translator::FileAliasNameIsDuplicate { alias_name: alias_name.clone() }),
            ConfigurationLog::HierarchyStructureIsInvalid => log!(Error, Translator::HierarchyStructureIsInvalid),
            ConfigurationLog::PropertyNameIsDuplicate { prop_name } => log!(Error, Translator::PropertyNameIsDuplicate { prop_name: prop_name.clone() }),
            ConfigurationLog::PropertyNameNotFound { prop_name } => log!(Error, Translator::PropertyNameNotFound { prop_name: prop_name.clone() }),
            ConfigurationLog::RegexModeIsUnknown { value } => log!(Error, Translator::RegexModeIsUnknown { value: value.clone() }),
            ConfigurationLog::ValueOfPropertyIsInvalid { prop_name, prop_value } => log!(Error, Translator::ValueOfPropertyIsInvalid { prop_name: prop_name.clone(), prop_value: prop_value.clone() }),
        };
    }
}

pub type PropertyMap = HashMap<String, PropertyItem>;

#[derive(Clone, PartialEq)]
pub struct PropertyItem {
    children: Box<PropertyMap>,
    values: Vec<String>,
}

impl PropertyItem {
    pub fn new(values: Vec<String>) -> PropertyItem {
        return PropertyItem {
            children: Box::new(PropertyMap::new()),
            values: values,
        };
    }

    pub fn add_values(&mut self, cons: Rc<RefCell<Console>>, key_stack: Vec<String>, key_stack_offset: usize, key: String, values: Vec<String>) -> ConsoleResult<()> {
        if key_stack_offset >= key_stack.len() {
            if self.children.contains_key(&key) {
                cons.borrow_mut().append_log(ConfigurationLog::PropertyNameIsDuplicate {
                    prop_name: {
                        let id_div = if key_stack.len() == 0 { "" } else { "." };
                        format!("{}{}{}", key_stack.join("."), id_div, key)
                    },
                }.get_log());

                return Err(());
            }

            let new_map = PropertyItem::new(values);
            self.children.insert(key, new_map);
        } else {
            let child_key = &key_stack[key_stack_offset];

            if !self.children.contains_key(child_key) {
                let new_child = PropertyItem::new(Vec::new());
                self.children.insert(child_key.clone(), new_child);
            };

            self.children.get_mut(child_key).unwrap().add_values(cons, key_stack, key_stack_offset + 1, key, values)?;
        }

        return Ok(());
    }

    pub fn print(&self, hierarchy_count: usize, key: String) {
        if self.values.len() != 0 {
            println!("{}{}: {},", "||".repeat(hierarchy_count), key, self.values.join(", ").replace("\n", "\\n"));
        }

        if self.children.len() != 0 {
            println!("{}{}:", "||".repeat(hierarchy_count), key);

            for (each_key, each_prop_item) in &*self.children {
                each_prop_item.print(hierarchy_count + 1, each_key.clone());
            }
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum RegexMode {
    Onigase,
    Posix,
}

impl RegexMode {
    pub fn from(regex_mode_value: &str) -> std::option::Option<RegexMode> {
        return match regex_mode_value.to_lowercase().as_str() {
            "default" => Some(RegexMode::get_default_mode()),
            "onigase" => Some(RegexMode::Onigase),
            "posix" => Some(RegexMode::Posix),
            _ => None,
        }
    }

    pub fn get_default_mode() -> RegexMode {
        return RegexMode::Posix;
    }
}

impl Display for RegexMode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        return match self {
            RegexMode::Onigase => write!(f, "Onigase"),
            RegexMode::Posix => write!(f, "POSIX"),
        };
    }
}

#[derive(Clone, PartialEq)]
pub enum ConfigurationItemKind {
    ASTReflection,
    FileAliases,
    Regex,
    Skip,
}

impl ConfigurationItemKind {
    pub fn from(v: &str) -> Option<ConfigurationItemKind> {
        let kind = match v {
            "ASTReflection" => ConfigurationItemKind::ASTReflection,
            "FileAliases" => ConfigurationItemKind::FileAliases,
            "Regex" => ConfigurationItemKind::Regex,
            "Skip" => ConfigurationItemKind::Skip,
            _ => return None,
        };

        return Some(kind);
    }
}

#[derive(Clone, PartialEq)]
pub struct Configuration {
    // note: <file_alias_name, file_path>
    pub file_alias_map: HashMap<String, String>,
    // note: <skipping_name, skipping_target_id>
    pub skipping_map: HashMap<String, String>,
    pub regex_mode: RegexMode,
    pub reverse_ast_reflection_style: bool,
}

impl Configuration {
    pub fn new() -> Configuration {
        return Configuration {
            file_alias_map: HashMap::new(),
            skipping_map: HashMap::new(),
            regex_mode: RegexMode::get_default_mode(),
            reverse_ast_reflection_style: false,
        };
    }

    pub fn load(cons: Rc<RefCell<Console>>, file_path: &String) -> ConsoleResult<Configuration> {
        let file_content = match FileMan::read_all(file_path) {
            Ok(v) => Box::new(v),
            Err(e) => {
                cons.borrow_mut().append_log(e.get_log());
                return Err(());
            },
        };

        let mut file_alias_map = HashMap::new();
        let mut reverse_ast_reflection_style = false;
        let mut regex_mode = RegexMode::get_default_mode();
        let mut skipping_map = HashMap::new();

        let prop_map = ConfigurationParser::parse(cons.clone(), file_path.clone(), file_content)?;

        for (top_item_name, top_item) in &*prop_map {
            let top_item_kind = match ConfigurationItemKind::from(top_item_name) {
                Some(v) => v,
                None => {
                    cons.borrow_mut().append_log(ConfigurationLog::PropertyNameNotFound {
                        prop_name: top_item_name.clone(),
                    }.get_log());

                    return Err(());
                },
            };

            match top_item_kind {
                ConfigurationItemKind::ASTReflection => {
                    let style = match top_item.values.get(0) {
                        Some(v) => v,
                        None => {
                            cons.borrow_mut().append_log(ConfigurationLog::ExpectedValuesOfPropertyProvided {
                                prop_name: top_item_name.to_string(),
                                unexpected_len: top_item.values.len(),
                                expected_len: 1,
                            }.get_log());

                            return Err(());
                        },
                    };

                    reverse_ast_reflection_style = match style.to_lowercase().as_str() {
                        "normal" => false,
                        "reversed" => true,
                        _ => {
                            cons.borrow_mut().append_log(ConfigurationLog::AstReflectionStyleIsUnknown {
                                value: style.clone(),
                            }.get_log());

                            return Err(());
                        },
                    };
                },
                ConfigurationItemKind::FileAliases => {
                    for (alias_name, alias_path_item) in &*top_item.children {
                        let alias_path = match alias_path_item.values.get(0) {
                            Some(v) => v,
                            None => {
                                cons.borrow_mut().append_log(ConfigurationLog::ExpectedValuesOfPropertyProvided {
                                    prop_name: alias_name.clone(),
                                    unexpected_len: alias_path_item.values.len(),
                                    expected_len: 1,
                                }.get_log());

                                return Err(());
                            },
                        };

                        file_alias_map.insert(alias_name.clone(), alias_path.clone());
                    }
                },
                ConfigurationItemKind::Regex => {
                    let regex_mode_str = match top_item.values.get(0) {
                        Some(v) => v,
                        None => {
                            cons.borrow_mut().append_log(ConfigurationLog::ExpectedValuesOfPropertyProvided {
                                prop_name: top_item_name.clone(),
                                unexpected_len: top_item.values.len(),
                                expected_len: 1,
                            }.get_log());

                            return Err(());
                        },
                    };

                    regex_mode = match RegexMode::from(regex_mode_str) {
                        Some(v) => v,
                        None => {
                            cons.borrow_mut().append_log(ConfigurationLog::RegexModeIsUnknown {
                                value: regex_mode_str.clone(),
                            }.get_log());

                            return Err(());
                        },
                    }
                },
                ConfigurationItemKind::Skip => {
                    for (skipping_name, skipping_target_id_item) in &*top_item.children {
                        let skipping_target_id = match skipping_target_id_item.values.get(0) {
                            Some(v) => v,
                            None => {
                                cons.borrow_mut().append_log(ConfigurationLog::ExpectedValuesOfPropertyProvided {
                                    prop_name: skipping_name.clone(),
                                    unexpected_len: skipping_target_id_item.values.len(),
                                    expected_len: 1,
                                }.get_log());

                                return Err(());
                            },
                        };

                        skipping_map.insert(skipping_name.clone(), skipping_target_id.clone());
                    }
                },
            }
        }

        let config = Configuration {
            file_alias_map: file_alias_map,
            skipping_map: skipping_map,
            regex_mode: regex_mode,
            reverse_ast_reflection_style: reverse_ast_reflection_style,
        };

        return Ok(config);
    }
}

struct ConfigurationParser {
    cons: Rc<RefCell<Console>>,
    // spec: 親要素の階層 (キー一覧)
    key_stack: Vec<String>,
}

impl ConfigurationParser {
    fn parse(cons: Rc<RefCell<Console>>, src_path: String, src_content: Box<String>) -> ConsoleResult<Box<PropertyMap>> {
        let block_map = ConfigurationBlock::get_block_map();
        let rule_map = Arc::new(Box::new(RuleMap::new(vec![block_map], DEFAULT_START_RULE_ID.to_string())?));
        let tree = SyntaxParser::parse(cons.clone(), rule_map, src_path, src_content, true)?;

        let mut config_parser = ConfigurationParser {
            cons: cons.clone(),
            key_stack: Vec::new(),
        };

        let prop_map = config_parser.to_property_map(&tree)?;
        return Ok(prop_map);
    }

    fn to_property_map(&mut self, tree: &SyntaxTree) -> ConsoleResult<Box<PropertyMap>> {
        let mut root_item = PropertyItem::new(Vec::new());

        for prop_item_node in tree.get_child_ref().get_node(self.cons.clone())?.find_child_nodes(vec![".Prop.Item"]) {
            let subitem = prop_item_node.get_node_child_at(self.cons.clone(), 0)?;

            match &subitem.ast_reflection_style {
                AstReflectionStyle::Reflection(name) => {
                    match name.as_str() {
                        ".Prop.ChildItem" => {
                            let (key_stack, key, values) = self.to_child_property(subitem)?;
                            root_item.add_values(self.cons.clone(), key_stack, 0, key, values)?;
                        },
                        ".Prop.ParentItem" => self.to_parent_property(subitem)?,
                        _ => {
                            self.cons.borrow_mut().append_log(BlockParsingLog::NodeUnexpectedExpected {
                                node_uuid: subitem.uuid.clone(),
                                unexpected_name: format!("'{}'", name),
                                expected_name: "parent or child item node name".to_string(),
                            }.get_log());

                            return Err(());
                        },
                    }
                },
                _ => {
                    self.cons.borrow_mut().append_log(BlockParsingLog::NodeUnexpectedExpected {
                        node_uuid: subitem.uuid.clone(),
                        unexpected_name: "no name".to_string(),
                        expected_name: "parent or child item node name".to_string(),
                    }.get_log());

                    return Err(());
                },
            };
        }

        return Ok(root_item.children);
    }

    fn to_parent_property(&mut self, prop_item_node: &SyntaxNode) -> ConsoleResult<()> {
        let (hierarchy_count, key) = self.to_property_key(prop_item_node.get_node_child_at(self.cons.clone(), 0)?)?;
        if self.key_stack.len() < hierarchy_count {
            self.cons.borrow_mut().append_log(ConfigurationLog::HierarchyStructureIsInvalid.get_log());

            return Err(());
        }

        for _ in 0..self.key_stack.len() - hierarchy_count {
            self.key_stack.pop();
        }

        self.key_stack.push(key);
        return Ok(());
    }

    fn to_child_property(&mut self, prop_item_node: &SyntaxNode) -> ConsoleResult<(Vec<String>, String, Vec<String>)> {
        let (hierarchy_count, key) = self.to_property_key(prop_item_node.get_node_child_at(self.cons.clone(), 0)?)?;
        let values = self.to_property_values(prop_item_node.get_node_child_at(self.cons.clone(), 1)?)?;

        if self.key_stack.len() < hierarchy_count {
            self.cons.borrow_mut().append_log(ConfigurationLog::HierarchyStructureIsInvalid.get_log());

            return Err(());
        }

        for _ in 0..self.key_stack.len() - hierarchy_count {
            self.key_stack.pop();
        }

        return Ok((self.key_stack.clone(), key, values));
    }

    fn to_property_key(&mut self, key_node: &SyntaxNode) -> ConsoleResult<(usize, String)> {
        let hierarchy_count = match key_node.find_first_child_node(vec!["Pipes"]) {
            Some(v) => v.get_reflectable_children().len(),
            None => 0,
        };

        let key = match key_node.find_first_child_node(vec![".Prop.Id"]) {
            Some(v) => v.join_child_leaf_values(),
            None => {
                self.cons.borrow_mut().append_log(BlockParsingLog::NodeUnexpectedExpected {
                    node_uuid: key_node.uuid.clone(),
                    unexpected_name: "no name".to_string(),
                    expected_name: "parent or child item node name".to_string(),
                }.get_log());

                return Err(());
            },
        };

        return Ok((hierarchy_count, key));
    }

    fn to_property_values(&mut self, value_node: &SyntaxNode) -> ConsoleResult<Vec<String>> {
        let mut raw_values = String::new();

        for each_char_elem in value_node.get_reflectable_children() {
            match each_char_elem {
                // note: エスケープ文字
                SyntaxNodeChild::Node(node) => raw_values += &self.to_esc_seq_string(node.as_ref())?,
                // note: 通常文字
                SyntaxNodeChild::Leaf(leaf) => raw_values += &leaf.as_ref().value,
            }
        }

        return Ok(raw_values.split(",").collect::<Vec<&str>>().iter().map(|v| v.to_string()).collect());
    }

    fn to_esc_seq_string(&mut self, esc_seq_node: &SyntaxNode) -> ConsoleResult<String> {
        let esc_char = esc_seq_node.get_leaf_child_at(self.cons.clone(), 0)?.value.as_str();

        let value = match esc_char {
            "\\" => "\\",
            "\"" => "\"",
            "n" => "\n",
            "," => ",",
            _ => {
                self.cons.borrow_mut().append_log(ConfigurationLog::EscapeSequenceCharacterIsUnknown {
                    escseq_char: esc_char.to_string(),
                }.get_log());

                return Err(());
            },
        };

        return Ok(value.to_string());
    }
}

struct ConfigurationBlock {}

impl ConfigurationBlock {
    pub fn get_block_map() -> BlockMap {
        return block_map!{
            "Main" => ConfigurationBlock::get_main_block(),
            "Symbol" => ConfigurationBlock::get_symbol_block(),
            "Prop" => ConfigurationBlock::get_prop_block(),
        };
    }

    fn get_main_block() -> Block {
        // code: Main <- Symbol.Space*# Symbol.LineEnd*# (Prop.Item Symbol.Div*#)*## "\z"#,
        let main_rule = rule!{
            ".Main.Main",
            group!{
                vec![],
                expr!(Id, ".Symbol.Space", "*", "#"),
                expr!(Id, ".Symbol.LineEnd", "*", "#"),
                group!{
                    vec!["*", "##"],
                    group!{
                        vec![],
                        expr!(Id, ".Prop.Item"),
                        expr!(Id, ".Symbol.Div", "*", "#"),
                    },
                },
                expr!(String, "\0", "#"),
            },
        };

        return block!(".Main", vec![main_rule]);
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

        return block!(".Symbol", vec![space_rule, line_end_rule, div_rule]);
    }

    fn get_prop_block() -> Block {
        // code: Item <- ChildItem : ParentItem,
        let item_rule = rule!{
            ".Prop.Item",
            group!{
                vec![":"],
                group!{
                    vec![],
                    expr!(Id, ".Prop.ChildItem"),
                },
                group!{
                    vec![],
                    expr!(Id, ".Prop.ParentItem"),
                },
            },
        };

        // code: ParentItem <- Key,
        let parent_item_rule = rule!{
            ".Prop.ParentItem",
            group!{
                vec![],
                expr!(Id, ".Prop.Key"),
            },
        };

        // code: ChildItem <- Key Value ","#,
        let child_item_rule = rule!{
            ".Prop.ChildItem",
            group!{
            vec![],
                expr!(Id, ".Prop.Key"),
                expr!(Id, ".Prop.Value"),
                expr!(String, ",", "#"),
            },
        };

        // code: Key <- ("||"*)#Pipes Symbol.Space*# Id Symbol.Space*# ":"# Symbol.Space*#,
        let key_rule = rule!{
            ".Prop.Key",
            group!{
                vec![],
                group!{
                    vec!["#Pipes"],
                    expr!(String, "||", "*"),
                },
                expr!(Id, ".Symbol.Space", "*", "#"),
                expr!(Id, ".Prop.Id"),
                expr!(Id, ".Symbol.Space", "*", "#"),
                expr!(String, ":", "#"),
                expr!(Id, ".Symbol.Space", "*", "#"),
            },
        };

        // code: Value <- (EscSeq : !"\n" !"," !"\\" .)*##,
        let value_rule = rule!{
            ".Prop.Value",
            group!{
                vec!["*", "##"],
                group!{
                    vec![":"],
                    group!{
                        vec![],
                        expr!(Id, ".Prop.EscSeq"),
                    },
                    group!{
                        vec![],
                        expr!(String, "\n", "!"),
                        expr!(String, ",", "!"),
                        expr!(String, "\\", "!"),
                        expr!(Wildcard, "."),
                    },
                },
            },
        };

        // code: Id <- JOIN([a-zA-Z_] [a-zA-Z0-9_]*),
        let id_rule = rule!{
            ".Prop.Id",
            group!{
                vec![],
                expr!(CharClass, "[a-zA-Z_]"),
                expr!(CharClass, "[a-zA-Z0-9_]", "*"),
            },
        };

        // code: EscSeq <- "\\"# ("\\" : "n" : "t" : ",")##,
        let esc_seq_rule = rule!{
            ".Prop.EscSeq",
            group!{
                vec![],
                expr!(String, "\\", "#"),
                group!{
                    vec![":", "##"],
                    group!{
                        vec!["##"],
                        expr!(String, "\\"),
                    },
                    group!{
                        vec!["##"],
                        expr!(String, "n"),
                    },
                    group!{
                        vec!["##"],
                        expr!(String, "t"),
                    },
                    group!{
                        vec!["##"],
                        expr!(String, ","),
                    },
                },
            },
        };

        return block!(".Prop", vec![item_rule, parent_item_rule, child_item_rule, key_rule, value_rule, id_rule, esc_seq_rule]);
    }
}
