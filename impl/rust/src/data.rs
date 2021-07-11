#[derive(PartialOrd, PartialEq, Debug)]
pub enum TokenKind {
    ID,
    Space,
    String,
    StringInBracket,
    Symbol,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenKind::ID => return write!(f, "ID"),
            TokenKind::Space => return write!(f, "Space"),
            TokenKind::String => return write!(f, "String"),
            TokenKind::StringInBracket => return write!(f, "StringInBracket"),
            TokenKind::Symbol => return write!(f, "Symbol"),
        }
    }
}

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

pub struct SyntaxNode {
    pub name: String,
}

impl SyntaxNode {
    pub fn new(name: String) -> Self {
        return SyntaxNode {
            name: name,
        };
    }
}

pub struct Block {
    pub name: String,
    pub cmds: Vec<Command>,
    pub rules: Vec<Rule>,
}

impl Block {
    pub fn new(name: String, cmds: Vec<Command>, rules: Vec<Rule>) -> Self {
        return Block {
            name: name,
            cmds: cmds,
            rules: rules,
        };
    }
}

pub enum Command {
    Define(Rule),
    Start(String),
    Use(String, String),
}

pub struct Rule {
    pub name: String,
}

impl Rule {
    pub fn new(name: String) -> Self {
        return Rule {
            name: name,
        };
    }
}
