# 使用草案 > ルールパーサ

## 概要

## コンポーネント

### RuleParser 構造体

#### フィールド

##### source: string

パース対象の FCPEG ソース。

##### index: usize

パースに使用する `source` のインデックス。

###### 初期値

`0`

#### メソッド

##### new(source: string)

構造体のコンストラクタ。

###### 引数

- `source` ... パース対象の FCPEG ソース。

###### 実装

Rust を除き、フィールドの初期化には `init(string)` を用いる。

##### init(source: string)

フィールドを初期化してインスタンスを再利用できるようにする。

###### 引数

- `source` ... パース対象の FCPEG ソース。

###### 実装

`source` は引数として渡されたソース、`index` は `0` で初期化する。

##### vector\<[Block](../block/index.md#Block%20%構造体)> parse() except RuleParseError

ソースをパースして生成されたブロックの一覧を返す。

###### 返り値

生成されたブロックの一覧。

##### priv [Block](../block/index.md#Block%20%構造体) get_next_block() except RuleParseError

次のブロックを取得する。

###### 返り値

生成されたブロック。

次のブロックが存在しない場合は `null` 。

###### 実装

`index` から検査を始め、ブロックの開始を検知する。
