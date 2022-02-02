# block モジュール

ブロック解析 (block parsing; FCPEG コードの構文解析) を行う。

## ログ

`BlockParsingLog` 列挙型でログ項目を定義する。

ログ項目については `block.rs` を参照。

## FCPEGBlock 構造体

FCPEG の構文を定義する。

`get_block_map()` で全体のブロックマップが取得可能。

構文定義のために以下のマクロを定義する。

- `block_map!` ... ブロックマップの生成
- `block!` ... ブロックの生成
- `rule!` ... 規則の生成
- `group!` ... グループの生成
- `expr!` ... 記号字句の生成

`fcpeg/src/syntax/fcpeg.fcpeg` で同等の構文を FCPEG 形式で記述している。

## BlockParser 構造体

PEG 解析の準備として、パース対象の FCPEG コードを規則マップに変換する。

`BlockParser` 内の関数:

- `get_rule_map()` ... 以下関数によりブロック解析を行い規則マップを取得
- `to_syntax_tree()` ... AST を取得
- `to_block_map()` ... AST をブロックマップに変換
- 以降の関数 ... 各 AST 要素を任意の型に変換

`get_rule_map()` の処理手順は以下のとおり:

1. FCPEG 構文のブロックマップを取得し、規則マップに変換
2. 規則マップからパース対象の各 FCPEG コードを AST に変換し、それぞれブロックマップを生成
3. ブロックマップを規則マップに変換
