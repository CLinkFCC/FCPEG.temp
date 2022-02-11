use crate::tree::CharacterPosition;

use cons_util::*;
use cons_util::cons::*;

use uuid::Uuid;

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
    // note: block parsing logs
    ArgumentIDIsDuplicate { arg_id: String },
    AttemptToAccessPrivateItem { item_id: String },
    AttributeNameIsDuplicate { attr_name: String },
    AttributeNameNotFound { attr_name: String },
    BlockAliasNameIsDuplicate { alias_name: String },
    BlockAliasNameIsUnnecessary { alias_name: String },
    BlockAliasNotFoundOrDeclaredUse { alias_name: String },
    BlockAlreadyDeclaredUse { block_name: String },
    BlockIDNotFound { block_id: String },
    BlockNameIsDuplicate { block_name: String },
    BlockNameIsViolatingNamingRule { block_name: String },
    CannotSpecifyRandomOrderToExpression,
    ChildNodeInNodeUnexpectedExpected { parent_node_uuid: Uuid, unexpected_name: String, expected_name: String },
    DeclaringStartOfMainRuleIsUnnecessary,
    DeclaringUseOfSelfBlockIsUnnecessary,
    EscapeSequenceCharacterIsUnknown { escseq_char: String },
    IDIsInvalid { id: String },
    LookaheadSymbolAtNodeIsUnknown { node_uuid: Uuid, symbol: String },
    LoopRangeIsInvalid { loop_range: String },
    LoopRangeIsUnrecommended { loop_range: String },
    LoopSymbolAtLeafIsUnknown { leaf_uuid: Uuid, symbol: String },
    NodeExpectedToBeName { node_uuid: Uuid, expected_name: String },
    NodeUnexpectedExpected { node_uuid: Uuid, unexpected_name: String, expected_name: String },
    RuleIDNotFound { rule_id: String },
    RuleNameIsDuplicate { rule_name: String },
    RuleNameIsViolatingNamingRule { rule_name: String },
    SkippingNameNotFound { skipping_name: String },
    StartCommandAlreadyDeclared,
    StartCommandDeclaredOutsideMainBlock,

    // note: syntax parsing logs
    CharacterClassFormatIsInvalid { value: String },
    ExpectedGenericsArgumentsProvided { unexpected_len: usize, expected_len: usize },
    ExpectedTemplateArgumentsProvided { unexpected_len: usize, expected_len: usize },
    GenericsArgumentIDNotFound { arg_id: String },
    LoopRangeIsInvalidOnParsing { loop_range: String },
    ParsingFailedAtRule { rule_id: String },
    PrimitiveRuleUncovered { rule_name: String },
    RepetitionExceededLoopLimit { loop_limit: usize },
    RuleIDNotFoundOnParsing { rule_id: String },
    SkippingPrimitiveRuleSpecifiedWithoutSkipingRules,
    StructureOfRuleElementIsInvalid { elem_uuid: Uuid },
    TemplateArgumentIDNotFound { arg_id: String },

    // note: tree logs
    CharacterPositionOfNodeNotFound { node_uuid: Uuid },
    ChildElementAtInNodeNotFound { parent_node_uuid: Uuid, index: usize },
    LeafExpectedToBeNode { leaf_uuid: Uuid },
    NodeExpectedToBeLeaf { node_uuid: Uuid },
    ReflectableChildAtInNodeNotFound { parent_node_uuid: Uuid, index: usize },

    // note: config logs
    ASTReflectionStyleIsUnknown { value: String },
    // note: ブロックパースログの項目と重複
    // EscapeSequenceCharacterIsUnknown { escseq_char: String },
    ExpectedValuesOfPropertyProvided { prop_name: String, unexpected_len: usize, expected_len: usize },
    FileAliasNameIsDuplicate { alias_name: String },
    HierarchyStructureIsInvalid,
    PropertyNameIsDuplicate { prop_name: String },
    PropertyNameNotFound { prop_name: String },
    RegexModeIsUnknown { value: String },
    ValueOfPropertyIsInvalid { prop_name: String, prop_value: String },

    // note: descriptions
    AtDescription { pos: CharacterPosition },
    RawDescription { msg: String },
}

impl Translator {
    // spec: 最初の 4 バイトのみを変換
    pub fn uuid_to_string(uuid: &Uuid) -> String {
        return uuid.to_string()[..8].to_string();
    }
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
            // note: block parsing logs
            Translator::ArgumentIDIsDuplicate { arg_id } => {
                Language::English => format!("argument id `{}` is duplicate", arg_id),
                Language::Japanese => format!("引数名 `{}` が重複しています", arg_id),
            },
            Translator::AttemptToAccessPrivateItem { item_id } => {
                Language::English => format!("attempt to access private item `{}`", item_id),
                Language::Japanese => format!("プライベートアイテム `{}` にアクセスしようとしています`", item_id),
            },
            Translator::AttributeNameIsDuplicate { attr_name } => {
                Language::English => format!("attribute name `{}` is duplicate", attr_name),
                Language::Japanese => format!("属性名 `{}` が重複しています", attr_name),
            },
            Translator::AttributeNameNotFound { attr_name } => {
                Language::English => format!("attribute name `{}` not found", attr_name),
                Language::Japanese => format!("属性名 `{}` が見つかりません", attr_name),
            },
            Translator::BlockAliasNameIsDuplicate { alias_name } => {
                Language::English => format!("block alias name `{}` is duplicate", alias_name),
                Language::Japanese => format!("ブロックエイリアス名 `{}` が重複しています", alias_name),
            },
            Translator::BlockAliasNameIsUnnecessary { alias_name } => {
                Language::English => format!("block alias name `{}` is unnecessary", alias_name),
                Language::Japanese => format!("ブロックエイリアス名 `{}` が不要です", alias_name),
            },
            Translator::BlockAliasNotFoundOrDeclaredUse { alias_name } => {
                Language::English => format!("block alias `{}` not found or declared use", alias_name),
                Language::Japanese => format!("ブロックエイリアス `{}` が見つからないか use 宣言されていません", alias_name),
            },
            Translator::BlockAlreadyDeclaredUse { block_name } => {
                Language::English => format!("block `{}` already desclared use", block_name),
                Language::Japanese => format!("ブロック `{}` は既に use 宣言されています", block_name),
            },
            Translator::BlockIDNotFound { block_id } => {
                Language::English => format!("block id `{}` not found", block_id),
                Language::Japanese => format!("ブロック id `{}` が見つかりません", block_id),
            },
            Translator::BlockNameIsDuplicate { block_name } => {
                Language::English => format!("block name `{}` is duplicate", block_name),
                Language::Japanese => format!("ブロック名 `{}` が重複しています", block_name),
            },
            Translator::BlockNameIsViolatingNamingRule { block_name } => {
                Language::English => format!("block name `{}` violating naming rule", block_name),
                Language::Japanese => format!("ブロック名 `{}` が命名規則に違反しています", block_name),
            },
            Translator::CannotSpecifyRandomOrderToExpression => {
                Language::English => "random order cannot specified to expression",
                Language::Japanese => "順不同は表現字句に指定できません",
            },
            Translator::ChildNodeInNodeUnexpectedExpected { parent_node_uuid, unexpected_name, expected_name } => {
                Language::English => format!("child node `{}` in node `{}` unexpected, expected `{}`", unexpected_name, Translator::uuid_to_string(parent_node_uuid), expected_name),
                Language::Japanese => format!("`{}` 内の子ノード `{}` を予期していません; `{}` が必要です", unexpected_name, Translator::uuid_to_string(parent_node_uuid), expected_name),
            },
            Translator::DeclaringStartOfMainRuleIsUnnecessary => {
                Language::English => "declaring start of main rule is unnecessary",
                Language::Japanese => "メイン規則を start 宣言する必要はありません",
            },
            Translator::DeclaringUseOfSelfBlockIsUnnecessary => {
                Language::English => "declaring use of self block is unnecessary",
                Language::Japanese => "自身のブロックを use 宣言する必要はありません",
            },
            Translator::EscapeSequenceCharacterIsUnknown { escseq_char } => {
                Language::English => format!("escape sequence character `{}` is unknown", escseq_char),
                Language::Japanese => format!("エスケープシーケンス文字 `{}` が不明です", escseq_char),
            },
            Translator::IDIsInvalid { id } => {
                Language::English => format!("id `{}` is invalid", id),
                Language::Japanese => format!("id `{}` が無効です", id),
            },
            Translator::LookaheadSymbolAtNodeIsUnknown { node_uuid, symbol } => {
                Language::English => format!("lookahead symbol `{}` at node `{}` is unknown", symbol, Translator::uuid_to_string(node_uuid)),
                Language::Japanese => format!("ノード `{}` の先読み記号 `{}` が不明です", Translator::uuid_to_string(node_uuid), symbol),
            },
            Translator::LoopRangeIsInvalid { loop_range } => {
                Language::English => format!("loop range `{}` is invalid", loop_range),
                Language::Japanese => format!("繰り返し範囲 `{}` が無効です", loop_range),
            },
            Translator::LoopRangeIsUnrecommended { loop_range } => {
                Language::English => format!("loop range is `{}` unrecommended", loop_range),
                Language::Japanese => format!("繰り返し範囲 `{}` は非推奨です", loop_range),
            },
            Translator::LoopSymbolAtLeafIsUnknown { leaf_uuid, symbol } => {
                Language::English => format!("loop symbol `{}` at leaf `{}` is unknown", symbol, Translator::uuid_to_string(leaf_uuid)),
                Language::Japanese => format!("リーフ `{}` の繰り返し記号 `{}` が不明です", Translator::uuid_to_string(leaf_uuid), symbol),
            },
            Translator::NodeExpectedToBeName { node_uuid, expected_name } => {
                Language::English => format!("node `{}` expected to be name `{}`", Translator::uuid_to_string(node_uuid), expected_name),
                Language::Japanese => format!("ノード `{}` の名前が `{}` である必要があります", Translator::uuid_to_string(node_uuid), expected_name),
            },
            Translator::NodeUnexpectedExpected { node_uuid, unexpected_name, expected_name } => {
                Language::English => format!("node `{}` (`{}`) unexpected, expected `{}`", unexpected_name, Translator::uuid_to_string(node_uuid), expected_name),
                Language::Japanese => format!("ノード `{}` (`{}`) を予期していません; `{}` が必要です", unexpected_name, Translator::uuid_to_string(node_uuid), expected_name),
            },
            Translator::RuleIDNotFound { rule_id } => {
                Language::English => format!("rule id `{}` not found", rule_id),
                Language::Japanese => format!("規則 id `{}` が見つかりません", rule_id),
            },
            Translator::RuleNameIsDuplicate { rule_name } => {
                Language::English => format!("rule name `{}` is duplicate", rule_name),
                Language::Japanese => format!("規則名 `{}` が重複しています", rule_name),
            },
            Translator::RuleNameIsViolatingNamingRule { rule_name } => {
                Language::English => format!("rule name `{}` violating naming rule", rule_name),
                Language::Japanese => format!("規則名 `{}` が命名規則に違反しています", rule_name),
            },
            Translator::SkippingNameNotFound { skipping_name } => {
                Language::English => format!("skipping name `{}` not found", skipping_name),
                Language::Japanese => format!("スキッピング名 `{}` が見つかりません", skipping_name),
            },
            Translator::StartCommandAlreadyDeclared => {
                Language::English => "start command already declared",
                Language::Japanese => "start 命令が既に宣言されています",
            },
            Translator::StartCommandDeclaredOutsideMainBlock => {
                Language::English => "start command declared outside main block",
                Language::Japanese => "start 命令がメインブロック外で宣言されています",
            },

            // note: syntax parsing logs
            Translator::CharacterClassFormatIsInvalid { value } => {
                Language::English => format!("character class format `{}` is invalid", value),
                Language::Japanese => format!("文字クラスフォーマット `{}` が無効です", value),
            },
            Translator::ExpectedGenericsArgumentsProvided { unexpected_len, expected_len } => {
                Language::English => format!("expected {} generics argument(s), provided {}", unexpected_len, expected_len),
                Language::Japanese => format!("{} 個のジェネリクス引数が必要ですが {} 個が渡されました", expected_len, unexpected_len),
            },
            Translator::ExpectedTemplateArgumentsProvided { unexpected_len, expected_len } => {
                Language::English => format!("expected {} template argument(s), provided {}", unexpected_len, expected_len),
                Language::Japanese => format!("{} 個のテンプレート引数が必要ですが {} 個が渡されました", expected_len, unexpected_len),
            },
            Translator::GenericsArgumentIDNotFound { arg_id } => {
                Language::English => format!("generics argument id `{}` not found", arg_id),
                Language::Japanese => format!("ジェネリクス引数 ID `{}` が見つかりません", arg_id),
            },
            Translator::LoopRangeIsInvalidOnParsing { loop_range } => {
                Language::English => format!("loop range `{}` is invalid on parsing", loop_range),
                Language::Japanese => format!("パース時の繰り返し範囲 `{}` が無効です", loop_range),
            },
            Translator::ParsingFailedAtRule { rule_id } => {
                Language::English => format!("parsing failed at rule `{}`", rule_id),
                Language::Japanese => format!("規則 `{}` でパースが失敗しました", rule_id),
            },
            Translator::PrimitiveRuleUncovered { rule_name } => {
                Language::English => format!("primitive rule `{}` uncovered", rule_name),
                Language::Japanese => format!("プリミティブ規則 `{}` がカバーされていません", rule_name),
            },
            Translator::RepetitionExceededLoopLimit { loop_limit } => {
                Language::English => format!("repetition exceeded loop limit {}", loop_limit),
                Language::Japanese => format!("繰り返しがループ制限 {} を超過しました", loop_limit),
            },
            Translator::RuleIDNotFoundOnParsing { rule_id } => {
                Language::English => format!("rule id `{}` not foundon parsing", rule_id),
                Language::Japanese => format!("パース時に規則 ID `{}` が見つかりません", rule_id),
            },
            Translator::SkippingPrimitiveRuleSpecifiedWithoutSkipingRules => {
                Language::English => "skipping primitive rule specified without skipping rules",
                Language::Japanese => "スキッピングプリミティブ規則がスキッピング規則なしに指定されました",
            },
            Translator::StructureOfRuleElementIsInvalid { elem_uuid } => {
                Language::English => format!("structure of rule element `{}` is invalid", Translator::uuid_to_string(elem_uuid)),
                Language::Japanese => format!("規則要素 `{}` の構造が無効です", Translator::uuid_to_string(elem_uuid)),
            },
            Translator::TemplateArgumentIDNotFound { arg_id } => {
                Language::English => format!("template argument id `{}` not found", arg_id),
                Language::Japanese => format!("テンプレート引数 ID `{}` が見つかりません", arg_id),
            },

            // note: tree logs
            Translator::CharacterPositionOfNodeNotFound { node_uuid } => {
                Language::English => format!("character position of node `{}` not found", Translator::uuid_to_string(node_uuid)),
                Language::Japanese => format!("ノード `{}` の文字位置が見つかりません", Translator::uuid_to_string(node_uuid)),
            },
            Translator::ChildElementAtInNodeNotFound { parent_node_uuid, index } => {
                Language::English => format!("child element at {} in node `{}` not found", index, Translator::uuid_to_string(parent_node_uuid)),
                Language::Japanese => format!("ノード `{}` 内の {} 番目の子要素が見つかりません", Translator::uuid_to_string(parent_node_uuid), index),
            },
            Translator::LeafExpectedToBeNode { leaf_uuid } => {
                Language::English => format!("leaf `{}` expected to be node", Translator::uuid_to_string(leaf_uuid)),
                Language::Japanese => format!("リーフ `{}` がノードである必要があります", Translator::uuid_to_string(leaf_uuid)),
            },
            Translator::NodeExpectedToBeLeaf { node_uuid } => {
                Language::English => format!("node `{}` expected to be leaf", Translator::uuid_to_string(node_uuid)),
                Language::Japanese => format!("ノード `{}` がリーフである必要があります", Translator::uuid_to_string(node_uuid)),
            },
            Translator::ReflectableChildAtInNodeNotFound { parent_node_uuid, index } => {
                Language::English => format!("reflectable child element at {} in node `{}` not found", index, Translator::uuid_to_string(parent_node_uuid)),
                Language::Japanese => format!("ノード `{}` 内の {} 番目の反映的な子要素が見つかりません", Translator::uuid_to_string(parent_node_uuid), index),
            },

            // note: config logs
            Translator::ASTReflectionStyleIsUnknown { value } => {
                Language::English => format!("AST reflection style `{}` is unknown", value),
                Language::Japanese => format!("AST 反映方式 `{}` が不明です", value),
            },
            Translator::ExpectedValuesOfPropertyProvided { prop_name, unexpected_len, expected_len } => {
                Language::English => format!("expected {} value(s) of property `{}`, provided {}", prop_name, expected_len, unexpected_len),
                Language::Japanese => format!("プロパティ `{}` に {} 個のプロパティ値が必要ですが {} 個が渡されました", prop_name, expected_len, unexpected_len),
            },
            Translator::FileAliasNameIsDuplicate { alias_name } => {
                Language::English => format!("file alias name `{}` is duplicate", alias_name),
                Language::Japanese => format!("ファイルエイリアス名 `{}` が重複しています", alias_name),
            },
            Translator::HierarchyStructureIsInvalid => {
                Language::English => "hierarchy structure is invalid",
                Language::Japanese => "階層構造が無効です",
            },
            Translator::PropertyNameIsDuplicate { prop_name } => {
                Language::English => format!("property name `{}` is duplicate", prop_name),
                Language::Japanese => format!("プロパティ名 `{}` が重複しています", prop_name),
            },
            Translator::PropertyNameNotFound { prop_name } => {
                Language::English => format!("property name `{}` not found", prop_name),
                Language::Japanese => format!("プロパティ名 `{}` が見つかりません", prop_name),
            },
            Translator::RegexModeIsUnknown { value } => {
                Language::English => format!("regex mode `{}` is unknown", value),
                Language::Japanese => format!("正規表現モード `{}` が見つかりません", value),
            },
            Translator::ValueOfPropertyIsInvalid { prop_name, prop_value } => {
                Language::English => format!("value `{}` of property `{}` is invalid", prop_value, prop_name),
                Language::Japanese => format!("プロパティ `{}` の値 `{}` が無効です", prop_name, prop_value),
            },

            // note: descriptions
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
