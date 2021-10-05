use std::io::*;

use crate::rule::*;

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
    pub fn new(line: usize, kind: TokenKind, value: String) -> Token {
        return Token {
            line: line,
            kind: kind,
            value: value,
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum ASTReflection {
    // note: AST に反映される
    Reflectable(String),
    // note: AST に反映されない
    Unreflectable(),
}

impl ASTReflection {
    pub fn is_reflectable(self) -> bool {
        return self != ASTReflection::Unreflectable();
    }
}

#[derive(Clone)]
pub enum SyntaxNodeElement {
    NodeList(SyntaxNodeList),
    Leaf(SyntaxLeaf),
}

impl SyntaxNodeElement {
    pub fn from_node_list_args(subnodes: Vec<SyntaxNodeElement>, ast_reflection: ASTReflection) -> SyntaxNodeElement {
        return SyntaxNodeElement::NodeList(SyntaxNodeList::new(subnodes, ast_reflection));
    }

    pub fn from_leaf_args(value: String, ast_reflection: ASTReflection) -> SyntaxNodeElement {
        return SyntaxNodeElement::Leaf(SyntaxLeaf::new(value, ast_reflection));
    }

    pub fn set_ast_reflection(&mut self, ast_reflection: ASTReflection) {
        match self {
            SyntaxNodeElement::NodeList(node_list) => node_list.ast_reflection = ast_reflection,
            SyntaxNodeElement::Leaf(leaf) => leaf.ast_reflection = ast_reflection,
        }
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
    pub fn from_node_list(node_list: SyntaxNodeElement) -> SyntaxTree {
        return SyntaxTree {
            child: node_list,
        };
    }

    pub fn from_node_list_args(subnodes: Vec<SyntaxNodeElement>, ast_reflection: ASTReflection) -> SyntaxTree {
        return SyntaxTree {
            child: SyntaxNodeElement::NodeList(SyntaxNodeList::new(subnodes, ast_reflection)),
        };
    }

    pub fn print(&self) {
        self.child.print(0, &mut BufWriter::new(stdout().lock()))
    }
}

#[derive(Clone)]
pub struct SyntaxNodeList {
    pub nodes: Vec<SyntaxNodeElement>,
    pub ast_reflection: ASTReflection,
}

impl SyntaxNodeList {
    pub fn new(nodes: Vec<SyntaxNodeElement>, ast_reflection: ASTReflection) -> SyntaxNodeList {
        return SyntaxNodeList {
            nodes: nodes,
            ast_reflection: ast_reflection,
        };
    }

    pub fn print(&self, nest: usize, writer: &mut BufWriter<StdoutLock>) {
        let display_name = match &self.ast_reflection {
            ASTReflection::Reflectable(elem_name) => {
                if elem_name == "" {
                    "[noname]".to_string()
                } else {
                    elem_name.clone()
                }
            },
            ASTReflection::Unreflectable() => "[hidden]".to_string(),
        };

        writeln!(writer, "|{} {}", "   |".repeat(nest), display_name).unwrap();

        for each_node in &self.nodes {
            each_node.print(nest + 1, writer);
        }
    }
}

#[derive(Clone)]
pub struct SyntaxLeaf {
    value: String,
    ast_reflection: ASTReflection,
}

impl SyntaxLeaf {
    pub fn new(value: String, ast_reflection: ASTReflection) -> SyntaxLeaf {
        return SyntaxLeaf {
            value: value,
            ast_reflection: ast_reflection,
        };
    }

    pub fn print(&self, nest: usize, writer: &mut BufWriter<StdoutLock>) {
        let value = self.value
            .replace("\\", "\\\\")
            .replace("\n", "\\n")
            .replace("\t", "\\t");

        let ast_reflect_text = match &self.ast_reflection {
            ASTReflection::Reflectable(elem_name) => format!("({})", elem_name.clone()),
            ASTReflection::Unreflectable() => "[hidden]".to_string(),
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
    pub fn new(name: String, cmds: Vec<Command>) -> Block {
        return Block {
            name: name,
            cmds: cmds,
        };
    }
}

#[derive(Clone)]
pub enum Command {
    Define(usize, Rule),
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
