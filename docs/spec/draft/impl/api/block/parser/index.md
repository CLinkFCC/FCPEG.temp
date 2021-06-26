# 使用草案 > ブロック > ブロックパーサ

## 概要

## コンポーネント

### BlockParser 構造体

#### フィールド

##### source: string

パース対象の FCPEG ソース。

###### 初期値

`""`

##### tokens: vector\<string>

トークンに分割されたパース対象の FCPEG ソース。

###### 初期値

`{}`

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

##### priv load_source(fcpeg_file_path: string)

###### 引数

- `fcpeg_file_path` ... FCPEG ソースのファイルパス

###### 実装

- `get_tokens(&string)` を実行する。

##### priv static vector\<string> get_tokens(source: &string)

`source` をトークンに分割して返す。

###### 返り値

- 分割されたトークン列

##### vector\<[Block](../block/index.md#Block%20%構造体)> parse(fcpeg_file_path: string) except RuleParseError

読み込んだ FCPEG ソースをパースして生成されたブロックの一覧を返す。

###### 返り値

- 生成されたブロックの一覧。

###### 実装

`load_source(string)` と `get_tokens(source)` を実行する。

##### priv vector\<[Block](../block/index.md#Block%20%構造体)> parse_blocks() except RuleParseError

ブロックの一覧をパースする。

###### 返り値

- 生成されたブロック。

##### priv tuple\<vector\<[Operation](../operation/index.md#Operation%20%構造体)>, vector\<[Rule](../operation/index.md#Rule%20%構造体)>> parse_block_contents(usize begin, usize end) except RuleParseError

ブロック内の命令と規則をパースする。

###### 引数

- `begin` ... ブロックの中身の開始位置
- `end` ... ブロックの中身の終了位置

###### 返り値

- 生成された命令と規則の一覧。

##### priv vector\<[Choice](../block/rule/index.md#Choice%20%構造体)> get_choices(usize begin, usize end) except RuleParseError

###### 引数

- `begin` ... 規則の右辺の開始位置
- `end` ... 規則の右辺の終了位置

###### 返り値

- 生成された選択の一覧。
