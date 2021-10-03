use std::io::*;
use crate::rule;

#[derive(PartialOrd, PartialEq, Debug, Clone)]
pub enum TokenKind {
    ID,
    Number,
    Space,
    String,
    StringInBracket,
    Symbol,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenKind::ID => return write!(f, "ID"),
            TokenKind::Number => return write!(f, "Number"),
            TokenKind::Space => return write!(f, "Space"),
            TokenKind::String => return write!(f, "String"),
            TokenKind::StringInBracket => return write!(f, "StringInBracket"),
            TokenKind::Symbol => return write!(f, "Symbol"),
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub line: usize,
    pub kind: TokenKind,
    pub value: String,
}

impl Token {
    pub fn new(line: usize, kind: TokenKind, value: String) -> Self {
        return Token {
            line: line,
            kind: kind,
            value: value,
        }
    }
}

#[derive(Clone)]
pub enum SyntaxNodeElement {
    NodeList(SyntaxNodeList),
    Leaf(SyntaxLeaf),
}

impl SyntaxNodeElement {
    pub fn from_node_list_args(subnodes: Vec<SyntaxNodeElement>, ast_reflect: Option<String>) -> SyntaxNodeElement {
        return SyntaxNodeElement::NodeList(SyntaxNodeList::new(subnodes, ast_reflect));
    }

    pub fn from_leaf_args(value: String, ast_reflect: Option<String>) -> SyntaxNodeElement {
        return SyntaxNodeElement::Leaf(SyntaxLeaf::new(value, ast_reflect));
    }

    pub fn print(&self, nest: usize, writer: &mut BufWriter<StdoutLock>) {
        match self {
            SyntaxNodeElement::NodeList(node_list) => node_list.print(nest, writer),
            SyntaxNodeElement::Leaf(leaf) => leaf.print(nest, writer),
        }
    }
}

#[derive(Clone)]
pub struct SyntaxTree {
    child: SyntaxNodeElement,
}

impl SyntaxTree {
    pub fn from_node_list_args(subnodes: Vec<SyntaxNodeElement>, ast_reflect: Option<String>) -> Self {
        return SyntaxTree {
            child: SyntaxNodeElement::NodeList(SyntaxNodeList::new(subnodes, ast_reflect)),
        };
    }

    pub fn print(&self) {
        self.child.print(0, &mut BufWriter::new(stdout().lock()))
    }
}

#[derive(Clone)]
pub struct SyntaxNodeList {
    subnodes: Vec<SyntaxNodeElement>,
    ast_reflect: Option<String>,
}

impl SyntaxNodeList {
    pub fn new(subnodes: Vec<SyntaxNodeElement>, ast_reflect: Option<String>) -> Self {
        return SyntaxNodeList {
            subnodes: subnodes,
            ast_reflect: ast_reflect,
        };
    }

    pub fn get_ast_reflect(&self) -> Option<String> {
        return self.ast_reflect.clone();
    }

    pub fn clone_subnodes(&self) -> Vec<SyntaxNodeElement> {
        return self.subnodes.clone();
    }

    pub fn get_subnode_len(&self) -> usize {
        return self.subnodes.len();
    }

    pub fn print(&self, nest: usize, writer: &mut BufWriter<StdoutLock>) {
        println!("{}", self.ast_reflect.is_none());
        let display_name = match self.ast_reflect.clone() {
            Some(v) => v.clone(),
            None => "[hidden]".to_string(),
        };

        writeln!(writer, "|{} {}", "   |".repeat(nest), display_name).unwrap();

        for each_node in &self.subnodes {
            each_node.print(nest + 1, writer);
        }
    }
}

#[derive(Clone)]
pub struct SyntaxLeaf {
    value: String,
    ast_reflect: Option<String>,
}

impl SyntaxLeaf {
    pub fn new(value: String, ast_reflect: Option<String>) -> Self {
        return SyntaxLeaf {
            value: value,
            ast_reflect: ast_reflect,
        };
    }

    pub fn get_value(&self) -> String {
        return self.value.clone();
    }

    pub fn print(&self, nest: usize, writer: &mut BufWriter<StdoutLock>) {
        let value = self.value
            .replace("\\", "\\\\")
            .replace("\n", "\\n")
            .replace("\t", "\\t");

        let ast_reflect_text = match self.ast_reflect.clone() {
            Some(v) => format!("({})", v.clone()),
            None => "[hidden]".to_string(),
        };

        writeln!(writer, "|{}- \"{}\" {}", "   |".repeat(nest), value, ast_reflect_text).unwrap();
    }
}

#[derive(Clone)]
pub struct Block {
    pub name: String,
    pub cmds: Vec<Command>,
}

impl Block {
    pub fn new(name: String, cmds: Vec<Command>) -> Self {
        return Block {
            name: name,
            cmds: cmds,
        };
    }
}

#[derive(Clone)]
pub enum Command {
    Define(usize, rule::Rule),
    Start(usize, String, String, String),
    Use(usize, String, String, String),
}

impl std::fmt::Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Command::Define(line, rule) => return write!(f, "{}| rule {}", line, rule),
            Command::Start(line, file_alias_name, block_name, rule_name) => return write!(f, "{}| start rule '{}.{}.{}'", line, file_alias_name, block_name, rule_name),
            Command::Use(line, file_alias_name, block_name, block_alias_name) => return write!(f, "{}| use block '{}.{}' as '{}'", line, file_alias_name, block_name, block_alias_name),
        }
    }
}
