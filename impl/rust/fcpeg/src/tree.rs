use std::cell::RefCell;
use std::fmt::*;
use std::io::*;
use std::io::Write;
use std::rc::Rc;

use crate::Raw;
use crate::cons::*;

use cons_util::*;
use cons_util::cons::*;

use uuid::Uuid;

#[derive(Clone, PartialEq)]
pub enum TreeLog {
    CharacterPositionOfNodeNotFound { node_uuid: Uuid },
    ChildElementAtInNodeNotFound { parent_node_uuid: Uuid, index: usize },
    LeafExpectedToBeNode { leaf_uuid: Uuid },
    NodeExpectedToBeLeaf { node_uuid: Uuid },
    ReflectableChildAtInNodeNotFound { parent_node_uuid: Uuid, index: usize },
}

impl ConsoleLogger for TreeLog {
    fn get_log(&self) -> ConsoleLog {
        return match self {
            TreeLog::CharacterPositionOfNodeNotFound { node_uuid } => log!(Error, Translator::CharacterPositionOfNodeNotFound { node_uuid: node_uuid.clone() }),
            TreeLog::ChildElementAtInNodeNotFound { parent_node_uuid, index } => log!(Error, Translator::ChildElementAtInNodeNotFound { parent_node_uuid: parent_node_uuid.clone(), index: *index }),
            TreeLog::LeafExpectedToBeNode { leaf_uuid } => log!(Error, Translator::LeafExpectedToBeNode { leaf_uuid: leaf_uuid.clone() }),
            TreeLog::NodeExpectedToBeLeaf { node_uuid } => log!(Error, Translator::NodeExpectedToBeLeaf { node_uuid: node_uuid.clone() }),
            TreeLog::ReflectableChildAtInNodeNotFound { parent_node_uuid, index } => log!(Error, Translator::ReflectableChildAtInNodeNotFound { parent_node_uuid: parent_node_uuid.clone(), index: *index }),
        };
    }
}

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
pub enum AstReflectionStyle {
    // note: AST に反映される
    Reflection(String),
    // note: AST に反映されない
    NoReflection,
    Expansion,
}

impl AstReflectionStyle {
    // todo: config データの扱いを修正
    pub fn from_config(reverse_ast_reflection: bool, is_reflectable: bool, elem_name: String) -> AstReflectionStyle {
        return if is_reflectable {
            if reverse_ast_reflection {
                AstReflectionStyle::Reflection(elem_name)
            } else {
                AstReflectionStyle::NoReflection
            }
        } else {
            if reverse_ast_reflection {
                AstReflectionStyle::NoReflection
            } else {
                AstReflectionStyle::Reflection(elem_name)
            }
        }
    }

    pub fn is_reflectable(&self) -> bool {
        return *self != AstReflectionStyle::NoReflection;
    }

    pub fn is_expandable(&self) -> bool {
        return *self == AstReflectionStyle::Expansion;
    }
}

impl Display for AstReflectionStyle {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let s = match self {
            AstReflectionStyle::Reflection(elem_name) => format!("#{}", elem_name.clone()),
            AstReflectionStyle::NoReflection => String::new(),
            AstReflectionStyle::Expansion => "##".to_string(),
        };

        return write!(f, "{}", s);
    }
}

pub struct SyntaxTree {
    child: SyntaxNodeChild,
}

impl SyntaxTree {
    pub fn from_node_child(node: SyntaxNodeChild) -> SyntaxTree {
        return SyntaxTree {
            child: node,
        };
    }

    pub fn from_node_child_args(subelems: Vec<SyntaxNodeChild>, ast_reflection_style: AstReflectionStyle) -> SyntaxTree {
        return SyntaxTree {
            child: SyntaxNodeChild::Node(Raw::new(SyntaxNode::new(Uuid::new_v4(), subelems, ast_reflection_style))),
        };
    }

    pub fn print(&self, ignore_hidden_elems: bool) {
        self.child.print(ignore_hidden_elems)
    }

    pub fn get_child_ref(&self) -> &SyntaxNodeChild {
        return &self.child;
    }
}

impl Drop for SyntaxTree {
    fn drop(&mut self) {
        unsafe {
            self.child.raw_drop();
        }
    }
}

#[derive(Clone)]
pub enum SyntaxNodeChild {
    Node(Raw<SyntaxNode>),
    Leaf(Raw<SyntaxLeaf>),
}

impl SyntaxNodeChild {
    pub fn from_node_args(subelems: Vec<SyntaxNodeChild>, ast_reflection_style: AstReflectionStyle) -> SyntaxNodeChild {
        return SyntaxNodeChild::Node(Raw::new(SyntaxNode::new(Uuid::new_v4(), subelems, ast_reflection_style)));
    }

    pub fn from_leaf_args(pos: CharacterPosition, value: String, ast_reflection: AstReflectionStyle) -> SyntaxNodeChild {
        return SyntaxNodeChild::Leaf(Raw::new(SyntaxLeaf::new(Uuid::new_v4(), pos, value, ast_reflection)));
    }

    pub fn get_node(&self, cons: Rc<RefCell<Console>>) -> ConsoleResult<&SyntaxNode> {
        return match self {
            SyntaxNodeChild::Node(node) => Ok(node.as_ref()),
            SyntaxNodeChild::Leaf(leaf) => {
                cons.borrow_mut().append_log(TreeLog::LeafExpectedToBeNode {
                    leaf_uuid: leaf.as_ref().uuid.clone(),
                }.get_log());

                return Err(());
            },
        };
    }

    pub fn get_leaf(&self, cons: Rc<RefCell<Console>>) -> ConsoleResult<&SyntaxLeaf> {
        return match self {
            SyntaxNodeChild::Node(node) => {
                cons.borrow_mut().append_log(TreeLog::NodeExpectedToBeLeaf {
                    node_uuid: node.as_ref().uuid.clone(),
                }.get_log());

                return Err(());
            },
            SyntaxNodeChild::Leaf(leaf) => Ok(leaf.as_ref()),
        };
    }

    pub fn is_node(&self) -> bool {
        return match self {
            SyntaxNodeChild::Node(_) => true,
            _ => false,
        };
    }

    pub fn is_reflectable(&self) -> bool {
        return match self {
            SyntaxNodeChild::Node(node) => node.as_ref().is_reflectable(),
            SyntaxNodeChild::Leaf(leaf) => leaf.as_ref().is_reflectable(),
        };
    }

    pub fn get_ast_reflection_style(&self) -> AstReflectionStyle {
        return match self {
            SyntaxNodeChild::Node(node) => node.as_ref().ast_reflection_style.clone(),
            SyntaxNodeChild::Leaf(leaf) => leaf.as_ref().ast_reflection_style.clone(),
        };
    }

    pub fn set_ast_reflection_style(&mut self, ast_reflection_style: AstReflectionStyle) {
        match self {
            SyntaxNodeChild::Node(node) => node.as_mut_ref().ast_reflection_style = ast_reflection_style,
            SyntaxNodeChild::Leaf(leaf) => leaf.as_mut_ref().ast_reflection_style = ast_reflection_style,
        }
    }

    pub fn print(&self, ignore_hidden_elems: bool) {
        self.print_with_details(0, &mut BufWriter::new(stdout().lock()), ignore_hidden_elems)
    }

    pub fn print_with_details(&self, nest: usize, writer: &mut BufWriter<StdoutLock>, ignore_hidden_elems: bool) {
        match self {
            SyntaxNodeChild::Node(node) => node.as_ref().print_with_details(nest, writer, ignore_hidden_elems),
            SyntaxNodeChild::Leaf(leaf) => leaf.as_ref().print_with_details(nest, writer, ignore_hidden_elems),
        }
    }

    unsafe fn raw_drop(&mut self) {
        match self {
            SyntaxNodeChild::Node(node) => node.raw_drop(),
            SyntaxNodeChild::Leaf(leaf) => leaf.raw_drop(),
        }
    }
}

pub struct SyntaxNode {
    pub uuid: Uuid,
    pub subelems: Vec<SyntaxNodeChild>,
    pub ast_reflection_style: AstReflectionStyle,
}

impl SyntaxNode {
    pub fn new(uuid: Uuid, subelems: Vec<SyntaxNodeChild>, ast_reflection_style: AstReflectionStyle) -> SyntaxNode {
        return SyntaxNode {
            uuid: uuid,
            subelems: subelems,
            ast_reflection_style: ast_reflection_style,
        };
    }

    pub fn exists_child_node(&self, patterns: Vec<&str>) -> bool {
        return self.find_first_child_node(patterns).is_some();
    }

    pub fn filter_children(&self, f: fn(&SyntaxNodeChild) -> bool) -> Vec<&SyntaxNodeChild> {
        let mut elems = Vec::<&SyntaxNodeChild>::new();

        for each_elem in &self.subelems {
            if f(each_elem) {
                elems.push(each_elem);
            }
        }

        return elems;
    }

    pub fn get_reflectable_children(&self) -> Vec<&SyntaxNodeChild> {
        return self.filter_children(|each_elem| each_elem.is_reflectable());
    }

    // ret: 最初にマッチした Reflectable な子ノード
    pub fn find_first_child_node(&self, patterns: Vec<&str>) -> Option<&SyntaxNode> {
        for each_elem in &self.subelems {
            match each_elem {
                SyntaxNodeChild::Node(node) => {
                    match &node.as_ref().ast_reflection_style {
                        AstReflectionStyle::Reflection(name) if patterns.iter().any(|s| s == name) => return Some(node.as_ref()),
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

        for each_elem in &self.subelems {
            match each_elem {
                SyntaxNodeChild::Node(node) => {
                    match &node.as_ref().ast_reflection_style {
                        AstReflectionStyle::Reflection(name) if patterns.iter().any(|s| s == name) => nodes.push(node.as_ref()),
                        _ => (),
                    }
                },
                _ => (),
            }
        }

        return nodes;
    }

    // todo: 最初に出現したリーフの位置を返す; Unreflectable なリーフも対象にする
    pub fn get_position(&self, cons: Option<Rc<RefCell<Console>>>) -> ConsoleResult<CharacterPosition> {
        for each_child in self.get_children() {
            match each_child {
                SyntaxNodeChild::Node(each_node) => match each_node.as_ref().get_position(cons.clone()) {
                    Ok(v) => return Ok(v),
                    Err(_) => (),
                },
                SyntaxNodeChild::Leaf(each_leaf) => return Ok(each_leaf.as_ref().pos.clone()),
            }
        };

        match cons {
            Some(cons) => {
                cons.borrow_mut().append_log(TreeLog::CharacterPositionOfNodeNotFound {
                    node_uuid: self.uuid.clone(),
                }.get_log());
            },
            None => (),
        }

        return Err(());
    }

    pub fn get_children(&self) -> &Vec<SyntaxNodeChild> {
        return &self.subelems;
    }

    pub fn get_child_at(&self, cons: Rc<RefCell<Console>>, index: usize) -> ConsoleResult<&SyntaxNodeChild> {
        let mut elem_i = 0;
        let mut reflectable_elem_i = 0;

        for each_elem in &self.subelems {
            if each_elem.is_reflectable() {
                if reflectable_elem_i == index {
                    return match self.subelems.get(elem_i) {
                        Some(v) => Ok(&v),
                        None => {
                            cons.borrow_mut().append_log(TreeLog::ChildElementAtInNodeNotFound {
                                parent_node_uuid: self.uuid.clone(),
                                index: index,
                            }.get_log());

                            return Err(());
                        },
                    };
                }

                reflectable_elem_i += 1;
            }

            elem_i += 1;
        }

        cons.borrow_mut().append_log(TreeLog::ReflectableChildAtInNodeNotFound {
            parent_node_uuid: self.uuid,
            index: index,
        }.get_log());

        return Err(());
    }

    pub fn get_node_child_at(&self, cons: Rc<RefCell<Console>>, index: usize) -> ConsoleResult<&SyntaxNode> {
        return self.get_child_at(cons.clone(), index)?.get_node(cons);
    }

    pub fn get_leaf_child_at(&self, cons: Rc<RefCell<Console>>, index: usize) -> ConsoleResult<&SyntaxLeaf> {
        return self.get_child_at(cons.clone(), index)?.get_leaf(cons);
    }

    pub fn is_reflectable(&self) -> bool {
        return self.ast_reflection_style.is_reflectable();
    }

    // note: Reflectable な子孫ノードの値をすべて結合して返す
    pub fn join_child_leaf_values(&self) -> String {
        let mut s = String::new();

        for each_elem in &self.subelems {
            match each_elem {
                SyntaxNodeChild::Node(node) => {
                    s += node.as_ref().join_child_leaf_values().as_str();
                },
                SyntaxNodeChild::Leaf(leaf) => {
                    match leaf.as_ref().ast_reflection_style {
                        AstReflectionStyle::Reflection(_) => s += leaf.as_ref().value.as_ref(),
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
            AstReflectionStyle::Reflection(elem_name) => {
                if elem_name == "" {
                    "[noname]".to_string()
                } else {
                    elem_name.clone()
                }
            },
            AstReflectionStyle::NoReflection => "[hidden]".to_string(),
            AstReflectionStyle::Expansion => "[expandable]".to_string(),
        };

        let uuid_str = self.uuid.to_string()[..8].to_string();

        writeln!(writer, "|{} {} *{}", "   |".repeat(nest), display_name, uuid_str).unwrap();

        for each_elem in &self.subelems {
            each_elem.print_with_details(nest + 1, writer, ignore_hidden_elems);
        }
    }
}

pub struct SyntaxLeaf {
    pub uuid: Uuid,
    pub pos: CharacterPosition,
    pub value: String,
    pub ast_reflection_style: AstReflectionStyle,
}

impl SyntaxLeaf {
    pub fn new(uuid: Uuid, pos: CharacterPosition, value: String, ast_reflection_style: AstReflectionStyle) -> SyntaxLeaf {
        return SyntaxLeaf {
            pos: pos,
            value: value,
            ast_reflection_style: ast_reflection_style,
            uuid: uuid,
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

        let ast_reflection_str = match &self.ast_reflection_style {
            AstReflectionStyle::Reflection(elem_name) => format!("({})", elem_name.clone()),
            AstReflectionStyle::NoReflection => "[hidden]".to_string(),
            AstReflectionStyle::Expansion => "[expandable]".to_string(),
        };

        let pos_str = format!("{}:{}", self.pos.line + 1, self.pos.column + 1);
        let uuid_str = self.uuid.to_string()[..8].to_string();

        writeln!(writer, "|{}- \"{}\" {} {} *{}", "   |".repeat(nest), value, pos_str, ast_reflection_str, uuid_str).unwrap();
    }
}
