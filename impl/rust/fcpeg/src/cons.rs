use crate::SourcePosition;
use crate::rule::RuleId;

use cons_util::translate;
use cons_util::cons::*;

#[derive(Clone, PartialEq)]
pub enum Language {
    English,
    Japanese,
}

impl Language {
    pub fn from(s: &str) -> Option<Language> {
        let v = match s {
            "en" => Language::English,
            "ja" => Language::Japanese,
            _ => return None,
        };

        return Some(v);
    }
}

#[derive(Clone, PartialEq)]
pub enum Translator {
    // log titles
    ExpectedExpressionOrGroupFoundLinebreak,
    RuleIdIsDuplicate { rule_id: RuleId },
    // descriptions
    AtDescription { pos: SourcePosition },
    RawDescription { msg: String },
}

impl ConsoleLogTranslator for Translator {
    fn translate(&self, lang_name: &str) -> TranslationResult {
        let lang = match Language::from(lang_name) {
            Some(v) => v,
            None => return TranslationResult::UnknownLanguage,
        };

        let s = translate!{
            translator => self,
            lang => lang,
            // log titles
            Translator::ExpectedExpressionOrGroupFoundLinebreak => {
                Language::English => "expected expression or group, found linebreak",
                Language::Japanese => "表現字句もしくはグループが必要ですが、改行が見つかりました",
            },
            Translator::RuleIdIsDuplicate { rule_id } => {
                Language::English => format!("規則 ID `{}` が重複しています", rule_id),
                Language::Japanese => format!("rule ID {} is duplicate", rule_id),
            },
            // descriptions
            Translator::AtDescription { pos } => {
                Language::English => format!("\tat:\t{}", pos),
                Language::Japanese => format!("\t箇所:\t{}", pos),
            },
            Translator::RawDescription { msg } => {
                Language::English => msg,
                Language::Japanese => msg,
            },
        };

        return TranslationResult::Success(s.to_string());
    }
}
