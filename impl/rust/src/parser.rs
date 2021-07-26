use crate::blockparser;
use crate::data;
use rustnutlib::console;

pub enum SyntaxParseError {
    Unknown(),
    MainBlockNotFound(),
    StartCmdNotFound(),
}

impl SyntaxParseError {
    pub fn get_log_data(&self) -> console::ConsoleLogData {
        match self {
            SyntaxParseError::Unknown() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "unknown error", vec![], vec![]),
            SyntaxParseError::MainBlockNotFound() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "main block not found", vec![], vec![]),
            SyntaxParseError::StartCmdNotFound() => console::ConsoleLogData::new(console::ConsoleLogKind::Error, "start commmand not found", vec![], vec![]),
        }
    }
}

pub struct SyntaxParser {
    rule_map: data::RuleMap,
    start_rule_name: String,
    src_i: usize,
    src_content: String,
}

impl SyntaxParser {
    pub fn new(rule_map: data::RuleMap, start_rule_name: String) -> std::result::Result<Self, SyntaxParseError> {
        // let main_block = match fcpeg_file_man.block_map.get("Main") {
        //     Some(v) => v,
        //     None => return Err(SyntaxParseError::MainBlockNotFound()),
        // };

        return Ok(SyntaxParser {
            rule_map: rule_map,
            start_rule_name: start_rule_name,
            src_i: 0,
            src_content: "".to_string(),
        });

        // return Err(SyntaxParseError::StartCmdNotFound());
    }

    pub fn parse(&mut self, src_content: String) -> std::result::Result<data::SyntaxNode, SyntaxParseError> {
        // フィールドを初期化
        self.src_i = 0;
        self.src_content = src_content;

        let tree = data::SyntaxNode::new("".to_string());

        // loop {
        //     let matched_rule_name = match self.get_next_matched_rule(self.fcpeg_file_man)? {
        //         Some(v) => v,
        //         None => break,
        //     };
        // }

        return Ok(tree);
    }

    fn get_next_matched_rule(&mut self, fcpeg_file_man: blockparser::FCPEGFileMan) -> std::result::Result<std::option::Option<bool>, SyntaxParseError> {
        // for (rule_name, each_rule) in fcpeg_file_man.block_map {

        // }

        return Ok(None);
    }

    fn is_match_with_rule(&mut self, rule: &data::Rule) -> std::result::Result<std::option::Option<String>, SyntaxParseError> {
        return Ok(None);
    }
}
