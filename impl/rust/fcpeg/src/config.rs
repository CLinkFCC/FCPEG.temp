use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::*;
use std::rc::Rc;
use std::sync::Arc;

use crate::*;
use crate::block::*;
use crate::parser::*;
use crate::rule::*;

use rustnutlib::*;
use rustnutlib::console::*;
use rustnutlib::file::*;

pub enum ConfigurationLog {
    DuplicateFileAliasName { alias_name: String },
    DuplicatePropertyName { prop_name: String },
    InvalidHierarchy { hierarchy_count: usize },
    InvalidPropertyValue { prop_name: String, prop_value: String },
    InvalidPropertyValueLength { prop_name: String },
    InvalidSyntax { line: usize, msg: String },
    UnknownASTReflectionValue { id: String, value: String },
    UnknownEscapeCharacter { esc_char: String },
    UnknownPropertyName { prop_name: String },
    UnknownRegexMode { input: String },
}

impl ConsoleLogger for ConfigurationLog {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            ConfigurationLog::DuplicateFileAliasName { alias_name } => log!(Error, "duplicate alias name", format!("alias name:\t{}", alias_name)),
            ConfigurationLog::DuplicatePropertyName { prop_name } => log!(Error, "duplicate property name", format!("property name:\t{}", prop_name)),
            ConfigurationLog::InvalidHierarchy { hierarchy_count } => log!(Error, "invalid hierarchy", format!("hierarchy count:\t{}", hierarchy_count)),
            ConfigurationLog::InvalidPropertyValue { prop_name, prop_value } => log!(Error, "invalid property value", format!("property name:\t{}", prop_name), format!("property value:\t{}", prop_value)),
            ConfigurationLog::InvalidPropertyValueLength { prop_name } => log!(Error, "invalid property value length", format!("property name:\t{}", prop_name)),
            ConfigurationLog::InvalidSyntax { line, msg } => log!(Error, "invalid syntax", format!("{}", msg), format!("line:\t{}", line)),
            ConfigurationLog::UnknownASTReflectionValue { id, value } => log!(Error, "unknown AST reflection value", format!("id:\t{}", id), format!("value:\t{}", value.replace("\n", "\\n"))),
            ConfigurationLog::UnknownEscapeCharacter { esc_char } => log!(Error, "unknown escape character", format!("escape character:\t{}", esc_char)),
            ConfigurationLog::UnknownPropertyName { prop_name } => log!(Error, format!("unknown property name '{}'", prop_name)),
            ConfigurationLog::UnknownRegexMode { input } => log!(Error, format!("unknown regex mode '{}'", input)),
        };
    }
}

pub type PropertyMap = HashMap<String, PropertyItem>;

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

    pub fn add_values(&mut self, cons: &Rc<RefCell<Console>>, key_stack: Vec<String>, key_stack_offset: usize, key: String, values: Vec<String>) -> ConsoleResult<()> {
        if key_stack_offset >= key_stack.len() {
            if self.children.contains_key(&key) {
                cons.borrow_mut().append_log(ConfigurationLog::DuplicatePropertyName {
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

pub enum ConfigurationItemKind {
    ASTReflection,
    FileAliases,
    Regex,
}

impl ConfigurationItemKind {
    pub fn from(v: &str) -> Option<ConfigurationItemKind> {
        let kind = match v {
            "ASTReflection" => ConfigurationItemKind::ASTReflection,
            "FileAliases" => ConfigurationItemKind::FileAliases,
            "Regex" => ConfigurationItemKind::Regex,
            _ => return None,
        };

        return Some(kind);
    }
}

pub struct Configuration {
    pub file_alias_map: HashMap<String, String>,
    pub regex_mode: RegexMode,
    pub reverse_ast_reflection_style: bool,
}

impl Configuration {
    pub fn load(cons: Rc<RefCell<Console>>, file_path: &String) -> ConsoleResult<Configuration> {
        let file_content = match FileMan::read_all(file_path) {
            Ok(v) => Box::new(v),
            Err(e) => {
                cons.borrow_mut().append_log(e.get_log());
                return Err(());
            },
        };

        let mut file_alias_map = HashMap::<String, String>::new();
        let mut reverse_ast_reflection_style = false;
        let mut regex_mode = RegexMode::get_default_mode();

        let prop_map = ConfigurationParser::parse(cons.clone(), file_path.clone(), file_content)?;

        for (top_item_name, top_item) in &*prop_map {
            let top_item_kind = match ConfigurationItemKind::from(top_item_name) {
                Some(v) => v,
                None => {
                    cons.borrow_mut().append_log(ConfigurationLog::UnknownPropertyName {
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
                            cons.borrow_mut().append_log(ConfigurationLog::InvalidPropertyValueLength {
                                prop_name: top_item_name.to_string(),
                            }.get_log());

                            return Err(());
                        },
                    };

                    reverse_ast_reflection_style = match style.to_lowercase().as_str() {
                        "normal" => false,
                        "reversed" => true,
                        _ => {
                            cons.borrow_mut().append_log(ConfigurationLog::UnknownASTReflectionValue {
                                id: top_item_name.clone(),
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
                                cons.borrow_mut().append_log(ConfigurationLog::InvalidPropertyValueLength {
                                    prop_name: alias_name.clone()
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
                            cons.borrow_mut().append_log(ConfigurationLog::InvalidPropertyValueLength {
                                prop_name: top_item_name.clone(),
                            }.get_log());

                            return Err(());
                        },
                    };

                    regex_mode = match RegexMode::from(regex_mode_str) {
                        Some(v) => v,
                        None => {
                            cons.borrow_mut().append_log(ConfigurationLog::UnknownRegexMode {
                                input: regex_mode_str.clone(),
                            }.get_log());

                            return Err(());
                        },
                    }
                },
            }
        }

        let config = Configuration {
            file_alias_map: file_alias_map,
            regex_mode: regex_mode,
            reverse_ast_reflection_style: reverse_ast_reflection_style,
        };

        config.print();

        return Ok(config);
    }

    pub fn print(&self) {
        println!("-- Config Data --");
        println!();
        println!("regex mode: {}", self.regex_mode);
        println!("file aliases:");

        for (alias_name, alias_path) in self.file_alias_map.iter() {
            println!("\t{}: {}", alias_name, alias_path);
        }

        println!();
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
        tree.print(true);

        let mut config_parser = ConfigurationParser {
            cons: cons.clone(),
            key_stack: Vec::new(),
        };

        let prop_map = config_parser.to_property_map(&tree)?;
        for (key, each_item) in &*prop_map { each_item.print(0, key.clone()) }
        return Ok(prop_map);
    }

    fn to_property_map(&mut self, tree: &SyntaxTree) -> ConsoleResult<Box<PropertyMap>> {
        let mut root_item = PropertyItem::new(Vec::new());

        for prop_item_node in tree.get_child_ref().get_node(&self.cons)?.find_child_nodes(vec![".Prop.Item"]) {
            let sub_item = prop_item_node.get_node_child_at(&self.cons, 0)?;

            match &sub_item.ast_reflection_style {
                ASTReflectionStyle::Reflection(name) => {
                    match name.as_str() {
                        ".Prop.ChildItem" => {
                            let (key_stack, key, values) = self.to_child_property(sub_item)?;
                            root_item.add_values(&self.cons, key_stack, 0, key, values)?;
                        },
                        ".Prop.ParentItem" => self.to_parent_property(sub_item)?,
                        _ => {
                            self.cons.borrow_mut().append_log(BlockParsingLog::UnexpectedNodeName {
                                uuid: sub_item.uuid.clone(),
                                unexpected: format!("'{}'", name),
                                expected: "parent or child item node name".to_string(),
                            }.get_log());

                            return Err(());
                        },
                    }
                },
                _ => {
                    self.cons.borrow_mut().append_log(BlockParsingLog::UnexpectedNodeName {
                        uuid: sub_item.uuid.clone(),
                        unexpected: "no name".to_string(),
                        expected: "parent or child item node name".to_string(),
                    }.get_log());

                    return Err(());
                },
            };
        }

        return Ok(root_item.children);
    }

    fn to_parent_property(&mut self, prop_item_node: &SyntaxNode) -> ConsoleResult<()> {
        let (hierarchy_count, key) = self.to_property_key(prop_item_node.get_node_child_at(&self.cons, 0)?)?;
        if self.key_stack.len() < hierarchy_count {
            self.cons.borrow_mut().append_log(ConfigurationLog::InvalidHierarchy {
                hierarchy_count: hierarchy_count,
            }.get_log());

            return Err(());
        }

        for _ in 0..self.key_stack.len() - hierarchy_count {
            self.key_stack.pop();
        }

        self.key_stack.push(key);
        return Ok(());
    }

    fn to_child_property(&mut self, prop_item_node: &SyntaxNode) -> ConsoleResult<(Vec<String>, String, Vec<String>)> {
        let (hierarchy_count, key) = self.to_property_key(prop_item_node.get_node_child_at(&self.cons, 0)?)?;
        let values = self.to_property_values(prop_item_node.get_node_child_at(&self.cons, 1)?)?;

        if self.key_stack.len() < hierarchy_count {
            self.cons.borrow_mut().append_log(ConfigurationLog::InvalidHierarchy {
                hierarchy_count: hierarchy_count,
            }.get_log());

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
                self.cons.borrow_mut().append_log(BlockParsingLog::UnexpectedNodeName {
                    uuid: key_node.uuid.clone(),
                    unexpected: "no name".to_string(),
                    expected: "parent or child item node name".to_string(),
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
                SyntaxNodeElement::Node(node) => raw_values += &self.to_esc_seq_string(node)?,
                // note: 通常文字
                SyntaxNodeElement::Leaf(leaf) => raw_values += &leaf.value,
            }
        }

        return Ok(raw_values.split(",").collect::<Vec<&str>>().iter().map(|v| v.to_string()).collect());
    }

    fn to_esc_seq_string(&mut self, esc_seq_node: &SyntaxNode) -> ConsoleResult<String> {
        let esc_char = esc_seq_node.get_leaf_child_at(&self.cons, 0)?.value.as_str();

        let value = match esc_char {
            "\\" => "\\",
            "\"" => "\"",
            "n" => "\n",
            "," => ",",
            _ => {
                self.cons.borrow_mut().append_log(ConfigurationLog::UnknownEscapeCharacter {
                    esc_char: esc_char.to_string(),
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
            choice!{
                vec![],
                expr!(Id, ".Symbol.Space", "*", "#"),
                expr!(Id, ".Symbol.LineEnd", "*", "#"),
                choice!{
                    vec!["*", "##"],
                    choice!{
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

        return block!(".Symbol", vec![space_rule, line_end_rule, div_rule]);
    }

    fn get_prop_block() -> Block {
        // code: Item <- ChildItem : ParentItem,
        let item_rule = rule!{
            ".Prop.Item",
            choice!{
                vec![":"],
                choice!{
                    vec![],
                    expr!(Id, ".Prop.ChildItem"),
                },
                choice!{
                    vec![],
                    expr!(Id, ".Prop.ParentItem"),
                },
            },
        };

        // code: ParentItem <- Key,
        let parent_item_rule = rule!{
            ".Prop.ParentItem",
            choice!{
                vec![],
                expr!(Id, ".Prop.Key"),
            },
        };

        // code: ChildItem <- Key Value ","#,
        let child_item_rule = rule!{
            ".Prop.ChildItem",
            choice!{
            vec![],
                expr!(Id, ".Prop.Key"),
                expr!(Id, ".Prop.Value"),
                expr!(String, ",", "#"),
            },
        };

        // code: Key <- ("||"*)#Pipes Symbol.Space*# Id Symbol.Space*# ":"# Symbol.Space*#,
        let key_rule = rule!{
            ".Prop.Key",
            choice!{
                vec![],
                choice!{
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
            choice!{
                vec!["*", "##"],
                choice!{
                    vec![":"],
                    choice!{
                        vec![],
                        expr!(Id, ".Prop.EscSeq"),
                    },
                    choice!{
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
            choice!{
                vec![],
                expr!(CharClass, "[a-zA-Z_]"),
                expr!(CharClass, "[a-zA-Z0-9_]", "*"),
            },
        };

        // code: EscSeq <- "\\"# ("\\" : "n" : "t" : ",")##,
        let esc_seq_rule = rule!{
            ".Prop.EscSeq",
            choice!{
                vec![],
                expr!(String, "\\", "#"),
                choice!{
                    vec![":", "##"],
                    choice!{
                        vec!["##"],
                        expr!(String, "\\"),
                    },
                    choice!{
                        vec!["##"],
                        expr!(String, "n"),
                    },
                    choice!{
                        vec!["##"],
                        expr!(String, "t"),
                    },
                    choice!{
                        vec!["##"],
                        expr!(String, ","),
                    },
                },
            },
        };

        return block!(".Prop", vec![item_rule, parent_item_rule, child_item_rule, key_rule, value_rule, id_rule, esc_seq_rule]);
    }
}
