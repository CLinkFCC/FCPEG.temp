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
    pub fn from_node_list_args(name: String, subnodes: Vec<SyntaxNodeElement>) -> SyntaxNodeElement {
        return SyntaxNodeElement::NodeList(SyntaxNodeList::new(name, subnodes));
    }

    pub fn from_leaf_args(value: String) -> SyntaxNodeElement {
        return SyntaxNodeElement::Leaf(SyntaxLeaf::new(value));
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
    pub fn from_node_list_args(name: String, subnodes: Vec<SyntaxNodeElement>) -> Self {
        return SyntaxTree {
            child: SyntaxNodeElement::NodeList(SyntaxNodeList::new(name, subnodes)),
        };
    }

    pub fn print(&self) {
        self.child.print(0, &mut BufWriter::new(stdout().lock()))
    }
}

#[derive(Clone)]
pub struct SyntaxNodeList {
    name: String,
    subnodes: Vec<SyntaxNodeElement>,
}

impl SyntaxNodeList {
    pub fn new(name: String, subnodes: Vec<SyntaxNodeElement>) -> Self {
        return SyntaxNodeList {
            name: name,
            subnodes: subnodes,
        };
    }

    pub fn get_subnodes(&mut self) -> &mut Vec<SyntaxNodeElement> {
        return &mut self.subnodes;
    }

    pub fn get_subnode_len(&self) -> usize {
        return self.subnodes.len();
    }

    pub fn get_name(&self) -> String {
        return self.name.clone();
    }

    pub fn print(&self, nest: usize, writer: &mut BufWriter<StdoutLock>) {
        let display_name = if self.name == "" {
            "*choice".to_string()
        } else {
            self.name.clone()
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
}

impl SyntaxLeaf {
    pub fn new(value: String) -> Self {
        return SyntaxLeaf {
            value: value,
        };
    }

    pub fn get_value(&self) -> String {
        return self.value.clone();
    }

    pub fn print(&self, nest: usize, writer: &mut BufWriter<StdoutLock>) {
        writeln!(writer, "|{}- \"{}\"", "   |".repeat(nest), self.value.replace("\\", "\\\\").replace("\n", "\\n").replace(" ", "\\s").replace("\t", "\\t")).unwrap();
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
