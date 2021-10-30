use std::io::*;
use std::fmt::*;

use crate::block::*;
use crate::config::*;
use crate::parser::*;
use crate::rule::*;

#[derive(Clone)]
pub enum Pragma {
    Unknown,
    Define(String, Vec<crate::block::BlockToken>, Vec<String>),
    Start(Vec<BlockToken>),
    Use(Vec<BlockToken>),
}

impl Display for Pragma {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        return match self {
            Pragma::Unknown => write!(f, "unknown"),
            Pragma::Define(_, _, _) => write!(f, "define"),
            Pragma::Start(_) => write!(f, "start"),
            Pragma::Use(_) => write!(f, "use"),
        };
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
    pub fn new_with_config(is_reflectable: bool, elem_name: String) -> ASTReflection {
        unsafe {
            return if is_reflectable {
                if CONFIG_DATA.reverse_ast_reflection {
                    ASTReflection::Reflectable(elem_name)
                } else {
                    ASTReflection::Unreflectable()
                }
            } else {
                if CONFIG_DATA.reverse_ast_reflection {
                    ASTReflection::Unreflectable()
                } else{
                    ASTReflection::Reflectable(elem_name)
                }
            }
        }
    }

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

    pub fn get_node_list(&self) -> std::result::Result<&SyntaxNodeList, SyntaxParseError> {
        return match self {
            SyntaxNodeElement::NodeList(node_list) => Ok(node_list),
            _ => return Err(SyntaxParseError::InvalidSyntaxTreeStruct("element not node list".to_string())),
        };
    }

    pub fn get_leaf(&self) -> std::result::Result<&SyntaxLeaf, SyntaxParseError> {
        return match self {
            SyntaxNodeElement::Leaf(leaf) => Ok(leaf),
            _ => return Err(SyntaxParseError::InvalidSyntaxTreeStruct("element not leaf".to_string())),
        };
    }

    pub fn is_node_list(&self) -> bool {
        return match self {
            SyntaxNodeElement::NodeList(_) => true,
            _ => false,
        };
    }

    pub fn is_reflectable(&self) -> bool {
        return match self {
            SyntaxNodeElement::NodeList(node_list) => node_list.is_reflectable(),
            SyntaxNodeElement::Leaf(leaf) => leaf.is_reflectable(),
        };
    }

    pub fn get_ast_reflection(&self) -> ASTReflection {
        return match self {
            SyntaxNodeElement::NodeList(node_list) => node_list.ast_reflection.clone(),
            SyntaxNodeElement::Leaf(leaf) => leaf.ast_reflection.clone(),
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

    pub fn clone_child(&self) -> SyntaxNodeElement {
        return self.child.clone();
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

    // note: 子要素をフィルタリングする
    pub fn filter(&self, f: fn(&SyntaxNodeElement) -> bool) -> Vec<Box<&SyntaxNodeElement>> {
        let mut elems = Vec::<Box::<&SyntaxNodeElement>>::new();

        for each_child in &self.elems {
            if f(each_child) {
                elems.push(Box::new(each_child));
            }
        }

        return elems;
    }

    pub fn filter_unreflectable_out(&self) -> Vec<Box<&SyntaxNodeElement>> {
        return self.filter(|each_elem| each_elem.is_reflectable());
    }

    // note: 子ノードを名前で検索する; 最初にマッチしたノードを返す
    pub fn find_child_node(&self, search_name: String) -> std::result::Result<&SyntaxNodeList, SyntaxParseError> {
        for each_elem in &self.elems {
            match each_elem {
                SyntaxNodeElement::NodeList(node_list) => {
                    match &node_list.ast_reflection {
                        ASTReflection::Reflectable(name) if *name == search_name => return Ok(node_list),
                        _ => (),
                    }
                },
                _ => (),
            }
        }

        return Err(SyntaxParseError::InvalidSyntaxTreeStruct(format!("node name '{}' not found", search_name)));
    }

    pub fn get_child(&self, index: usize) -> std::result::Result<&SyntaxNodeElement, SyntaxParseError> {
        let mut elem_i = 0;
        let mut reflectable_elem_i = 0;

        for each_elem in &self.elems {
            if each_elem.is_reflectable() {
                if reflectable_elem_i == index {
                    return match self.elems.get(elem_i) {
                        Some(v) => Ok(&v),
                        None => return Err(SyntaxParseError::InvalidSyntaxTreeStruct("invalid operation".to_string())),
                    };
                }

                reflectable_elem_i += 1;
            }

            elem_i += 1;
        }

        return Err(SyntaxParseError::InvalidSyntaxTreeStruct(format!("{}th reflectable element not matched", index + 1)));
    }

    pub fn get_node_list_child(&self, index: usize) -> std::result::Result<&SyntaxNodeList, SyntaxParseError> {
        return self.get_child(index)?.get_node_list();
    }

    pub fn get_leaf_child(&self, index: usize) -> std::result::Result<&SyntaxLeaf, SyntaxParseError> {
        return self.get_child(index)?.get_leaf();
    }

    pub fn is_reflectable(&self) -> bool {
        return self.ast_reflection.is_reflectable();
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
            ASTReflection::Unreflectable() => {
                if ignore_hidden_elems {
                    return;
                }

                "[hidden]".to_string()
            },
        };

        writeln!(writer, "|{} {}", "   |".repeat(nest), display_name).unwrap();

        for each_elem in &self.elems {
            each_elem.print(nest + 1, writer, ignore_hidden_elems);
        }
    }

    pub fn to_string(&self) -> String {
        let mut s = String::new();

        for each_elem in &self.elems {
            match each_elem {
                SyntaxNodeElement::Leaf(leaf) => {
                    match leaf.ast_reflection {
                        ASTReflection::Reflectable(_) => s += leaf.value.as_ref(),
                        _ => (),
                    }
                },
                _ => (),
            }
        }

        return s;
    }
}

#[derive(Clone)]
pub struct SyntaxLeaf {
    pub value: String,
    pub ast_reflection: ASTReflection,
}

impl SyntaxLeaf {
    pub fn new(value: String, ast_reflection: ASTReflection) -> SyntaxLeaf {
        return SyntaxLeaf {
            value: value,
            ast_reflection: ast_reflection,
        };
    }

    pub fn is_reflectable(&self) -> bool {
        return self.ast_reflection.is_reflectable();
    }

    pub fn print(&self, nest: usize, writer: &mut BufWriter<StdoutLock>, ignore_hidden_elems: bool) {
        if !self.is_reflectable() && ignore_hidden_elems {
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
