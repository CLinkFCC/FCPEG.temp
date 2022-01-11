use std::fmt::*;
use std::io::*;
use std::io::Write;

use crate::parser::*;
use crate::rule::*;

#[derive(Clone, PartialEq)]
pub struct CharacterPosition {
    pub file_path: Option<String>,
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl CharacterPosition {
    pub fn new(file_path: Option<String>, index: usize, line: usize, column: usize) -> CharacterPosition {
        return CharacterPosition {
            file_path: file_path,
            index: index,
            line: line,
            column: column,
        };
    }

    pub fn get_empty() -> CharacterPosition {
        return CharacterPosition {
            file_path: None,
            index: 0,
            line: 0,
            column: 0,
        };
    }
}

impl Display for CharacterPosition {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let file_path_text = match self.file_path.clone() {
            Some(path) => format!("{}:", path),
            None => String::new(),
        };

        return write!(f, "{}{}:{}", file_path_text, self.line + 1, self.column + 1);
    }
}

#[derive(Clone, PartialEq)]
pub enum ASTReflectionStyle {
    // note: AST に反映される
    Reflection(String),
    // note: AST に反映されない
    NoReflection,
    Expansion,
}

impl ASTReflectionStyle {
    // todo: config データの扱いを修正
    pub fn from_config(reverse_ast_reflection: bool, is_reflectable: bool, elem_name: String) -> ASTReflectionStyle {
        return if is_reflectable {
            if reverse_ast_reflection {
                ASTReflectionStyle::Reflection(elem_name)
            } else {
                ASTReflectionStyle::NoReflection
            }
        } else {
            if reverse_ast_reflection {
                ASTReflectionStyle::NoReflection
            } else{
                ASTReflectionStyle::Reflection(elem_name)
            }
        }
    }

    pub fn is_reflectable(&self) -> bool {
        return *self != ASTReflectionStyle::NoReflection;
    }

    pub fn is_expandable(&self) -> bool {
        return *self == ASTReflectionStyle::Expansion;
    }
}

impl Display for ASTReflectionStyle {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let s = match self {
            ASTReflectionStyle::Reflection(elem_name) => format!("#{}", elem_name.clone()),
            ASTReflectionStyle::NoReflection => String::new(),
            ASTReflectionStyle::Expansion => "|".to_string(),
        };

        return write!(f, "{}", s);
    }
}

#[derive(Clone)]
pub enum SyntaxNodeElement {
    Node(Box<SyntaxNode>),
    Leaf(Box<SyntaxLeaf>),
}

impl SyntaxNodeElement {
    pub fn from_node_args(sub_elems: Vec<SyntaxNodeElement>, ast_reflection_style: ASTReflectionStyle) -> SyntaxNodeElement {
        return SyntaxNodeElement::Node(Box::new(SyntaxNode::new(sub_elems, ast_reflection_style)));
    }

    pub fn from_leaf_args(pos: CharacterPosition, value: String, ast_reflection: ASTReflectionStyle) -> SyntaxNodeElement {
        return SyntaxNodeElement::Leaf(Box::new(SyntaxLeaf::new(pos, value, ast_reflection)));
    }

    pub fn get_node(&self) -> SyntaxParseResult<&SyntaxNode> {
        return match self {
            SyntaxNodeElement::Node(node) => Ok(node),
            _ => return Err(SyntaxParseError::InvalidSyntaxTreeStructure { cause: "element not node list".to_string() }),
        };
    }

    pub fn get_leaf(&self) -> SyntaxParseResult<&SyntaxLeaf> {
        return match self {
            SyntaxNodeElement::Leaf(leaf) => Ok(leaf),
            _ => return Err(SyntaxParseError::InvalidSyntaxTreeStructure { cause: "element not leaf".to_string() }),
        };
    }

    pub fn is_node(&self) -> bool {
        return match self {
            SyntaxNodeElement::Node(_) => true,
            _ => false,
        };
    }

    pub fn is_reflectable(&self) -> bool {
        return match self {
            SyntaxNodeElement::Node(node) => node.is_reflectable(),
            SyntaxNodeElement::Leaf(leaf) => leaf.is_reflectable(),
        };
    }

    pub fn get_ast_reflection_style(&self) -> ASTReflectionStyle {
        return match self {
            SyntaxNodeElement::Node(node) => node.ast_reflection_style.clone(),
            SyntaxNodeElement::Leaf(leaf) => leaf.ast_reflection_style.clone(),
        };
    }

    pub fn set_ast_reflection_style(&mut self, ast_reflection_style: ASTReflectionStyle) {
        match self {
            SyntaxNodeElement::Node(node) => node.ast_reflection_style = ast_reflection_style,
            SyntaxNodeElement::Leaf(leaf) => leaf.ast_reflection_style = ast_reflection_style,
        }
    }

    pub fn print(&self, ignore_hidden_elems: bool) {
        self.print_with_details(0, &mut BufWriter::new(stdout().lock()), ignore_hidden_elems)
    }

    pub fn print_with_details(&self, nest: usize, writer: &mut BufWriter<StdoutLock>, ignore_hidden_elems: bool) {
        match self {
            SyntaxNodeElement::Node(node) => node.print_with_details(nest, writer, ignore_hidden_elems),
            SyntaxNodeElement::Leaf(leaf) => leaf.print_with_details(nest, writer, ignore_hidden_elems),
        }
    }
}

#[derive(Clone)]
pub struct SyntaxTree {
    child: SyntaxNodeElement,
}

impl SyntaxTree {
    pub fn from_node(node: SyntaxNodeElement) -> SyntaxTree {
        return SyntaxTree {
            child: node,
        };
    }

    pub fn from_node_args(sub_elems: Vec<SyntaxNodeElement>, ast_reflection_style: ASTReflectionStyle) -> SyntaxTree {
        return SyntaxTree {
            child: SyntaxNodeElement::Node(Box::new(SyntaxNode::new(sub_elems, ast_reflection_style))),
        };
    }

    pub fn print(&self, ignore_hidden_elems: bool) {
        self.child.print(ignore_hidden_elems)
    }

    pub fn get_child_ref(&self) -> &SyntaxNodeElement {
        return &self.child;
    }
}

#[derive(Clone)]
pub struct SyntaxNode {
    pub sub_elems: Vec<SyntaxNodeElement>,
    pub ast_reflection_style: ASTReflectionStyle,
}

impl SyntaxNode {
    pub fn new(sub_elems: Vec<SyntaxNodeElement>, ast_reflection_style: ASTReflectionStyle) -> SyntaxNode {
        return SyntaxNode {
            sub_elems: sub_elems,
            ast_reflection_style: ast_reflection_style,
        };
    }

    pub fn exists_child_node(&self, patterns: Vec<&str>) -> bool {
        return self.find_first_child_node(patterns).is_some();
    }

    pub fn filter_children(&self, f: fn(&SyntaxNodeElement) -> bool) -> Vec<&SyntaxNodeElement> {
        let mut elems = Vec::<&SyntaxNodeElement>::new();

        for each_elem in &self.sub_elems {
            if f(each_elem) {
                elems.push(each_elem);
            }
        }

        return elems;
    }

    pub fn get_reflectable_children(&self) -> Vec<&SyntaxNodeElement> {
        return self.filter_children(|each_elem| each_elem.is_reflectable());
    }

    // ret: 最初にマッチした Reflectable な子ノード
    pub fn find_first_child_node(&self, patterns: Vec<&str>) -> Option<&SyntaxNode> {
        for each_elem in &self.sub_elems {
            match each_elem {
                SyntaxNodeElement::Node(node) => {
                    match &node.ast_reflection_style {
                        ASTReflectionStyle::Reflection(name) if patterns.iter().any(|s| s == name) => return Some(node),
                        _ => (),
                    }
                },
                _ => (),
            }
        }

        return None;
    }

    // ret: すべてのマッチした Reflectable な子ノードの列
    pub fn find_child_nodes(&self, patterns: Vec<&str>) -> Vec<&SyntaxNode> {
        let mut nodes = Vec::<&SyntaxNode>::new();

        for each_elem in &self.sub_elems {
            match each_elem {
                SyntaxNodeElement::Node(node) => {
                    match &node.ast_reflection_style {
                        ASTReflectionStyle::Reflection(name) if patterns.iter().any(|s| s == name) => nodes.push(node),
                        _ => (),
                    }
                },
                _ => (),
            }
        }

        return nodes;
    }

    // todo: 最初に出現したリーフの位置を返す; Unreflectable なリーフも対象にする
    pub fn get_position(&self) -> SyntaxParseResult<CharacterPosition> {
        return Ok(self.get_leaf_child_at(0)?.pos.clone());
    }

    pub fn get_children(&self) -> &Vec<SyntaxNodeElement> {
        return &self.sub_elems;
    }

    pub fn get_child_at(&self, index: usize) -> SyntaxParseResult<&SyntaxNodeElement> {
        let mut elem_i = 0;
        let mut reflectable_elem_i = 0;

        for each_elem in &self.sub_elems {
            if each_elem.is_reflectable() {
                if reflectable_elem_i == index {
                    return match self.sub_elems.get(elem_i) {
                        Some(v) => Ok(&v),
                        None => return Err(SyntaxParseError::InvalidSyntaxTreeStructure { cause: "invalid operation".to_string() }),
                    };
                }

                reflectable_elem_i += 1;
            }

            elem_i += 1;
        }

        return Err(SyntaxParseError::InvalidSyntaxTreeStructure { cause: format!("{}th reflectable element not matched", index + 1) });
    }

    pub fn get_node_child_at(&self, index: usize) -> SyntaxParseResult<&SyntaxNode> {
        return self.get_child_at(index)?.get_node();
    }

    pub fn get_leaf_child_at(&self, index: usize) -> SyntaxParseResult<&SyntaxLeaf> {
        return self.get_child_at(index)?.get_leaf();
    }

    pub fn is_reflectable(&self) -> bool {
        return self.ast_reflection_style.is_reflectable();
    }

    // note: Reflectable な子孫ノードの値をすべて結合して返す
    pub fn join_child_leaf_values(&self) -> String {
        let mut s = String::new();

        for each_elem in &self.sub_elems {
            match each_elem {
                SyntaxNodeElement::Node(node) => {
                    s += node.join_child_leaf_values().as_str();
                },
                SyntaxNodeElement::Leaf(leaf) => {
                    match leaf.ast_reflection_style {
                        ASTReflectionStyle::Reflection(_) => s += leaf.value.as_ref(),
                        _ => (),
                    }
                },
            }
        }

        return s;
    }

    pub fn print(&self, ignore_hidden_elems: bool) {
        self.print_with_details(0, &mut BufWriter::new(stdout().lock()), ignore_hidden_elems);
    }

    pub fn print_with_details(&self, nest: usize, writer: &mut BufWriter<StdoutLock>, ignore_hidden_elems: bool) {
        if ignore_hidden_elems && !self.is_reflectable() {
            return;
        }

        let display_name = match &self.ast_reflection_style {
            ASTReflectionStyle::Reflection(elem_name) => {
                if elem_name == "" {
                    "[noname]".to_string()
                } else {
                    elem_name.clone()
                }
            },
            ASTReflectionStyle::NoReflection => "[hidden]".to_string(),
            ASTReflectionStyle::Expansion => "[expandable]".to_string(),
        };

        writeln!(writer, "|{} {}", "   |".repeat(nest), display_name).unwrap();

        for each_elem in &self.sub_elems {
            each_elem.print_with_details(nest + 1, writer, ignore_hidden_elems);
        }
    }
}

#[derive(Clone)]
pub struct SyntaxLeaf {
    pub pos: CharacterPosition,
    pub value: String,
    pub ast_reflection_style: ASTReflectionStyle,
}

impl SyntaxLeaf {
    pub fn new(pos: CharacterPosition, value: String, ast_reflection_style: ASTReflectionStyle) -> SyntaxLeaf {
        return SyntaxLeaf {
            pos: pos,
            value: value,
            ast_reflection_style: ast_reflection_style,
        };
    }

    pub fn is_reflectable(&self) -> bool {
        return self.ast_reflection_style.is_reflectable();
    }

    pub fn print(&self, ignore_hidden_elems: bool) {
        self.print_with_details(0, &mut BufWriter::new(stdout().lock()), ignore_hidden_elems);
    }

    pub fn print_with_details(&self, nest: usize, writer: &mut BufWriter<StdoutLock>, ignore_hidden_elems: bool) {
        if !self.is_reflectable() && ignore_hidden_elems {
            return;
        }

        let value = self.value
            .replace("\\", "\\\\")
            .replace("\n", "\\n")
            .replace("\t", "\\t");

        let ast_reflection_text = match &self.ast_reflection_style {
            ASTReflectionStyle::Reflection(elem_name) => format!("({})", elem_name.clone()),
            ASTReflectionStyle::NoReflection => "[hidden]".to_string(),
            ASTReflectionStyle::Expansion => "[expandable]".to_string(),
        };

        let pos_text = format!("{}/{}/{}", self.pos.index, self.pos.line, self.pos.column);

        writeln!(writer, "|{}- \"{}\" {} {}", "   |".repeat(nest), value, pos_text, ast_reflection_text).unwrap();
    }
}

#[derive(Clone)]
pub struct Block {
    pub name: String,
    pub cmds: Vec<BlockCommand>,
}

impl Block {
    pub fn new(name: String, cmds: Vec<BlockCommand>) -> Block {
        return Block {
            name: name,
            cmds: cmds,
        };
    }

    pub fn print(&self) {
        println!("[{}]{{", self.name);

        for each_cmd in &self.cmds {
            println!("    {}", each_cmd);
        }

        println!("}}");
    }
}

#[derive(Clone)]
pub enum BlockCommand {
    Comment { pos: CharacterPosition, value: String },
    Define { pos: CharacterPosition, rule: Rule },
    Start { pos: CharacterPosition, file_alias_name: String, block_name: String, rule_name: String },
    Use { pos: CharacterPosition, file_alias_name: String, block_name: String, block_alias_name: String },
}

impl Display for BlockCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BlockCommand::Comment { pos, value } => return write!(f, "{}| %{},", pos.line, value),
            BlockCommand::Define { pos, rule } => return write!(f, "{}| rule {}", pos.line, rule),
            BlockCommand::Start { pos, file_alias_name, block_name, rule_name } => return write!(f, "{}| start rule '{}.{}.{}'", pos.line, file_alias_name, block_name, rule_name),
            BlockCommand::Use { pos, file_alias_name, block_name, block_alias_name } => return write!(f, "{}| use block '{}.{}' as '{}'", pos.line, file_alias_name, block_name, block_alias_name),
        }
    }
}