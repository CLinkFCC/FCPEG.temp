#[derive(PartialOrd, PartialEq, Debug, Clone)]
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
    Define(Rule),
    Start(String),
    // ブロック名, ブロックエイリアス名
    Use(String, String),
}

impl std::fmt::Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Command::Define(rule) => return write!(f, "rule '{}'", rule),
            Command::Start(block_name) => return write!(f, "start block '{}'", block_name),
            Command::Use(block_name, block_alias_name) => return write!(f, "use block '{}' as '{}'", block_name, block_alias_name),
        }
    }
}

#[derive(Clone)]
pub struct Rule {
    pub name: String,
    pub choices: Vec<RuleChoice>,
}

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Rule {
    pub fn new(name: String, choices: Vec<RuleChoice>) -> Self {
        return Rule {
            name: name,
            choices: choices,
        };
    }
}

#[derive(Clone)]
pub struct RuleChoice {
    pub seqs: Vec<RuleSequence>,
}

impl RuleChoice {
    pub fn new(seqs: Vec<RuleSequence>) -> Self {
        return RuleChoice {
            seqs: seqs,
        };
    }
}

#[derive(Clone)]
pub struct RuleSequence {
    pub exprs: Vec<RuleExpression>,
}

impl RuleSequence {
    pub fn new(exprs: Vec<RuleExpression>) -> Self {
        return RuleSequence {
            exprs: exprs,
        };
    }
}

#[derive(Clone)]
pub enum RuleExpressionKind {
    CharClass,
    String,
    Wildcard,
}

#[derive(Clone)]
pub enum RuleExpressionLoopKind {
    One,
    OneOrMore,
    ZeroOrOne,
    ZeroOrMore,
}

#[derive(Clone)]
pub enum RuleExpressionLookaheadKind {
    None,
    Positive,
    Negative,
}

#[derive(Clone)]
pub struct RuleExpression {
    pub kind: RuleExpressionKind,
    pub loop_type: RuleExpressionLoopKind,
    pub lookahead_type: RuleExpressionLookaheadKind,
    pub value: String,
}

impl RuleExpression {
    pub fn new(kind: RuleExpressionKind, loop_type: RuleExpressionLoopKind, lookahead_type: RuleExpressionLookaheadKind, value: String,) -> Self {
        return RuleExpression {
            kind: kind,
            loop_type: loop_type,
            lookahead_type: lookahead_type,
            value: value,
        }
    }
}
