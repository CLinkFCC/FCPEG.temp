use crate::CharacterPosition;

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
    UnexpectedEof,
    // descriptions
    AtDescription { pos: CharacterPosition },
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
            Translator::UnexpectedEof => {
                Language::English => "unexpected EOF",
                Language::Japanese => "予期しない EOF",
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
