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

##### load_rules_by_blocks(blocks: vector\<[Block](../block/index.md#Block%20%構造体)>)

規則をブロックのベクタから読み込む。

###### 引数

- peg_file_path ... FCPEG 形式のファイルパス

##### SyntaxTree parse(source: string)

ソースをパースして生成された構文木を返す。

###### 引数

- source ... パース対象のソース。

###### 返り値

生成された構文木。
