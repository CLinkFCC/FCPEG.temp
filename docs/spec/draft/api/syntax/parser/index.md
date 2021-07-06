# 使用草案 > 構文パーサ

## 概要

ここでは定義された構文によるパースについて記述する。

## コンポーネント

### SyntaxParser クラス

#### フィールド

#### メソッド

##### new(blocks)

##### load_rules_by_file(peg_file_path: string)

FCPEG ファイルにより規則を読み込む。

###### 引数

- peg_file_path ... FCPEG 形式のファイルパス

###### 実装

生成されたブロックのロードには `load_rules_by_blocks()` を使用する。

##### load_rules_by_blocks(blocks: vector\<[Block](../block/index.md#Block%20%構造体)>)

規則をブロックのベクタから読み込む。

###### 引数

- peg_file_path ... FCPEG 形式のファイルパス

##### [SyntaxTree](../tree/index.md#SyntaxTree%20%構造体) parse(source: string)

ソースをパースして生成された構文木を返す。

###### 引数

- source ... パース対象のソース。

###### 返り値

生成された構文木。

###### 実装

パース前に `index` を `0` に設定する。

`index` のインデックスが `source` の長さを超えるまで `get_syntax_node(string)` を実行する。

##### [SyntaxNode](../tree/node/index.md#SyntaxNode%20%構造体) get_syntax_node(source: string)

ソースをパースして生成された構文木を返す。

###### 引数

- source ... パース対象のソース。

###### 返り値

生成された構文木。
