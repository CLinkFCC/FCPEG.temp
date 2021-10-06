use std::io::*;

use crate::rule::*;

#[derive(Clone, PartialEq)]
pub enum ASTReflection {
    // note: AST に反映される
    Reflectable(String),
    // note: AST に反映されない
    Unreflectable(),
}

impl ASTReflection {
    pub fn is_reflectable(&self) -> bool {
        return *self != ASTReflection::Unreflectable();
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

    pub fn is_hidden(&self) -> bool {
        return match self {
            SyntaxNodeElement::NodeList(node_list) => node_list.is_hidden(),
            SyntaxNodeElement::Leaf(leaf) => leaf.is_hidden(),
        };
    }

    pub fn set_ast_reflection(&mut self, ast_reflection: ASTReflection) {
        match self {
            SyntaxNodeElement::NodeList(node_list) => node_list.ast_reflection = ast_reflection,
            SyntaxNodeElement::Leaf(leaf) => leaf.ast_reflection = ast_reflection,
        }
    }

    pub fn print(&self, nest: usize, writer: &mut BufWriter<StdoutLock>, ignore_hidden_elems: bool) {
        match self {
            SyntaxNodeElement::NodeList(node_list) => node_list.print(nest, writer, ignore_hidden_elems),
            SyntaxNodeElement::Leaf(leaf) => leaf.print(nest, writer, ignore_hidden_elems),
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

    pub fn print(&self, ignore_hidden_elems: bool) {
        self.child.print(0, &mut BufWriter::new(stdout().lock()), ignore_hidden_elems)
    }
}

#[derive(Clone)]
pub struct SyntaxNodeList {
    pub elems: Vec<SyntaxNodeElement>,
    pub ast_reflection: ASTReflection,
}

impl SyntaxNodeList {
    pub fn new(elems: Vec<SyntaxNodeElement>, ast_reflection: ASTReflection) -> SyntaxNodeList {
        return SyntaxNodeList {
            elems: elems,
            ast_reflection: ast_reflection,
        };
    }

    pub fn is_hidden(&self) -> bool {
        return !self.ast_reflection.is_reflectable();
    }

    pub fn print(&self, nest: usize, writer: &mut BufWriter<StdoutLock>, ignore_hidden_elems: bool) {
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

        for each_elem in &self.elems {
            if !ignore_hidden_elems && !each_elem.is_hidden() {
                each_elem.print(nest + 1, writer, ignore_hidden_elems);
            }
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

    pub fn is_hidden(&self) -> bool {
        return !self.ast_reflection.is_reflectable();
    }

    pub fn print(&self, nest: usize, writer: &mut BufWriter<StdoutLock>, ignore_hidden_elems: bool) {
        if ignore_hidden_elems {
            return;
        }

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
    pub cmds: Vec<BlockCommand>,
}

impl Block {
    pub fn new(name: String, cmds: Vec<BlockCommand>) -> Block {
        return Block {
            name: name,
            cmds: cmds,
        };
    }
}

#[derive(Clone)]
pub enum BlockCommand {
    Define(usize, Rule),
    Start(usize, String, String, String),
    Use(usize, String, String, String),
}

impl std::fmt::Display for BlockCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BlockCommand::Define(line, rule) => return write!(f, "{}| rule {}", line, rule),
            BlockCommand::Start(line, file_alias_name, block_name, rule_name) => return write!(f, "{}| start rule '{}.{}.{}'", line, file_alias_name, block_name, rule_name),
            BlockCommand::Use(line, file_alias_name, block_name, block_alias_name) => return write!(f, "{}| use block '{}.{}' as '{}'", line, file_alias_name, block_name, block_alias_name),
        }
    }
}
