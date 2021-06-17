pub struct SyntaxNode {
    name: String,
}

impl SyntaxNode {
    pub fn new(name: String) -> Self {
        return SyntaxNode {
            name: name,
        };
    }
}

pub struct Block {
    name: String,
    cmds: Vec<Command>,
    rules: Vec<Rule>,
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
    name: String,
}

impl Rule {
    pub fn new(name: String) -> Self {
        return Rule {
            name: name,
        };
    }
}
