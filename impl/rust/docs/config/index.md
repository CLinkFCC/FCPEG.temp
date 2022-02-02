# config モジュール

構成ファイル (cfg) を処理する。

## PropertyMap 型

プロパティマップ: プロパティのテーブルを表す `HashMap<PropertyName, PropertyItem>` の型エイリアス。

キーはプロパティ名、値はプロパティアイテム。

## PropertyItem 構造体

- 子マップリスト `children`
- プロパティ値リスト `values`

## RegexMode 列挙型

正規表現モードを定義する。

現在はモードに関わらず POSIX を利用する。

- `Onigase` ... 鬼瀬エンジンを利用
- `Posix` ... POSIX エンジンを利用

## ConfigurationItemKind 列挙型

構成項目の種別を定義する。

- `ASTReflection` ... AST 反映方式
- `FileAliases` ... ファイルエイリアス一覧
- `Regex` ... 正規表現モード

## Configuration 構造体

- ファイルエイリアスマップ `file_alias_map`
- 正規表現モード `regex_mode`
- AST 反映性を反転するかどうか `reverse_ast_reflection_style`

## ConfigurationParser 構造体

構成ファイルのソースコードをパースする。

内部的な処理は [BlockParser 構造体](../block/index.md#BlockParser%20構造体) と同様。

## ConfigurationBlock 構造体

`get_block_map()` で全体のブロックマップが取得可能。

`fcpeg/src/syntax/config.fcpeg` で同等の構文を FCPEG 形式で記述している。
