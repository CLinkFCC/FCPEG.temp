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
            Command::Define(rule) => return write!(f, "rule {}", rule),
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

impl Rule {
    pub fn new(name: String, choices: Vec<RuleChoice>) -> Self {
        return Rule {
            name: name,
            choices: choices,
        };
    }
}

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut choice_text = Vec::<String>::new();

        for each_choice in &self.choices {
            choice_text.push(each_choice.to_string());
        }

        write!(f, "{} <- {}", self.name, choice_text.join(" : "))
    }
}

#[derive(Clone)]
pub struct RuleChoice {
    pub seq_groups: Vec<RuleSequenceGroup>,
}

impl RuleChoice {
    pub fn new(seq_groups: Vec<RuleSequenceGroup>) -> Self {
        return RuleChoice {
            seq_groups: seq_groups,
        };
    }
}

impl std::fmt::Display for RuleChoice {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut seq_group_text = Vec::<String>::new();

        for each_group in &self.seq_groups {
            seq_group_text.push(format!("{}{}{}", each_group.lookahead_type.to_symbol_string(), each_group.to_string(), each_group.loop_type.to_symbol_string()));
        }

        write!(f, "{}", seq_group_text.join(" "))
    }
}

#[derive(Clone)]
pub struct RuleSequenceGroup {
    pub seqs: Vec<RuleSequence>,
    pub loop_type: RuleExpressionLoopKind,
    pub lookahead_type: RuleExpressionLookaheadKind,
}

impl RuleSequenceGroup {
    pub fn new(seqs: Vec<RuleSequence>, loop_type: RuleExpressionLoopKind, lookahead_type: RuleExpressionLookaheadKind) -> Self {
        return RuleSequenceGroup {
            seqs: seqs,
            loop_type: loop_type,
            lookahead_type: lookahead_type,
        };
    }
}

impl std::fmt::Display for RuleSequenceGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut seq_text = Vec::<String>::new();

        for each_seq in &self.seqs {
            seq_text.push(format!("({})", each_seq));
        }

        write!(f, "{}", seq_text.join(" "))
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

impl std::fmt::Display for RuleSequence {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut expr_text = Vec::<String>::new();

        for each_expr in &self.exprs {
            expr_text.push(each_expr.to_string());
        }

        write!(f, "{}", expr_text.join(" "))
    }
}

#[derive(Clone)]
pub enum RuleExpressionKind {
    CharClass,
    ID,
    String,
    Wildcard,
}

impl RuleExpressionKind {
    pub fn to_token_string(&self, value: String) -> String {
        return match self {
            RuleExpressionKind::CharClass => value.to_string(),
            RuleExpressionKind::ID => value.to_string(),
            RuleExpressionKind::String => value.to_string(),
            RuleExpressionKind::Wildcard => ".".to_string(),
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleExpressionLoopKind {
    One,
    OneOrMore,
    ZeroOrOne,
    ZeroOrMore,
}

impl RuleExpressionLoopKind {
    // ret: 文字がマッチすれば Some() 、マッチしなければ None
    pub fn to_loop_kind(value: String) -> std::option::Option<RuleExpressionLoopKind> {
        return match value.as_str() {
            "+" => Some(RuleExpressionLoopKind::OneOrMore),
            "?" => Some(RuleExpressionLoopKind::ZeroOrOne),
            "*" => Some(RuleExpressionLoopKind::ZeroOrMore),
            _ => None,
        }
    }

    pub fn to_symbol_string(&self) -> String {
        return match self {
            RuleExpressionLoopKind::One => "".to_string(),
            RuleExpressionLoopKind::OneOrMore => "+".to_string(),
            RuleExpressionLoopKind::ZeroOrOne => "?".to_string(),
            RuleExpressionLoopKind::ZeroOrMore => "*".to_string(),
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuleExpressionLookaheadKind {
    None,
    Positive,
    Negative,
}

impl RuleExpressionLookaheadKind {
    pub fn to_symbol_string(&self) -> String {
        return match self {
            RuleExpressionLookaheadKind::None => "".to_string(),
            RuleExpressionLookaheadKind::Positive => "&".to_string(),
            RuleExpressionLookaheadKind::Negative => "!".to_string(),
        }
    }
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

impl std::fmt::Display for RuleExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}{}", self.lookahead_type.to_symbol_string(), self.kind.to_token_string(self.value.to_string()), self.loop_type.to_symbol_string())
    }
}
