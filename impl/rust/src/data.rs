pub enum TokenKind {
    ID,
    String,
    StringInBracket,
    Symbol,
}

pub struct Token {
    pub kind: TokenKind,
    pub value: String,
}

impl Token {
    pub fn new(kind: TokenKind, value: String) -> Self {
        return Token {
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
    Import(String),
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
