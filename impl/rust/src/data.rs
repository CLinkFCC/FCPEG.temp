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
    NodeList(String, Vec<SyntaxNodeElement>),
    Leaf(String),
}

impl SyntaxNodeElement {
    pub fn print(&self, nest: usize) {
        match self {
            SyntaxNodeElement::NodeList(name, sub_elems) => {
                if nest == 0 {
                    println!("[nodes]");
                }

                println!("{}{}", "    ".repeat(nest), name);

                for each_elem in sub_elems {
                    each_elem.print(nest + 1);
                }
            },
            SyntaxNodeElement::Leaf(leaf) => {
                println!("{}[leaf] {:?}", "    ".repeat(nest), leaf.replace("\\", "\\\\").replace("\n", "\\n").replace(" ", "\\s").replace("\t", "\\t"));
            }
        }
    }
}

#[derive(Clone)]
pub struct SyntaxTree {
    pub name: String,
    pub children: Vec<SyntaxNodeElement>,
}

impl SyntaxTree {
    pub fn new(name: String) -> Self {
        return SyntaxTree {
            name: name,
            children: vec![],
        };
    }

    pub fn print(&self) {
        for each_elem in &self.children {
            each_elem.print(0);
        }
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
