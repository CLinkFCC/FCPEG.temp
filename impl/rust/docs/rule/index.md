# rule モジュール

## RuleId 型

規則 ID: 規則を識別する `String` の型エイリアス。

基本的に `ファイルエイリアス名.ブロック名.規則名` という形式。

ただしプリミティブ規則の場合は `規則名` となる。

## BlockId 型

ブロック ID: ブロックを識別する `String` の型エイリアス。

`ファイルエイリアス名.ブロック名` という形式。

## GroupId 型

グループ ID: グループを識別する [uuid::Uuid](https://docs.rs/uuid/latest/uuid/) の型エイリアス。

## BlockMap 構造体

ブロックマップ: 命令をブロックごとにまとめたハッシュマップの型エイリアス。

内部的な構造は `HashMap<ブロック ID, 規則>` となっている。

## RuleMap 構造体

規則マップ: ブロックマップ内のすべての規則 (Define 命令) のみを展開したハッシュマップの型エイリアス。

内部的な構造は `HashMap<規則 ID, 規則>` となっている。

## Block 構造体

ブロックを定義する。

- ブロック名 `name`
- ブロックが持つ命令のリスト `cmds`

## Rule 構造体

- 規則が定義された位置 `pos`
- 規則 ID `id`
- 規則名 `name`
- ジェネリクスの仮引数一覧 `generics_arg_ids`
- テンプレートの仮引数一覧 `template_arg_ids`
- 構文として定義されたグループ要素 `group`

## LookaheadKind 列挙型

先読み方式を定義する。

- `None` ... 先読みなし
- `Positive` ... 肯定的先読み
- `Negative` ... 否定的先読み

## Infinitable<T: num_traits::Num> 列挙型

無限または有限な数を定義する。

- `Finite(T)` ... 有限な数
- `Infinite` ... 無限な数

## LoopRange 構造体

繰り返し数の範囲を定義する。

- 最小数 `min`
- 最大数 `max`

## ElementOrder 構造体

要素順を定義する。

- `Random(LoopRange)` ... 順不同的; 引数は順不同の繰り返し範囲
- `Sequential` ... 逐次的

## RuleElement 構造体

規則要素を定義する。

- `Group(Box<RuleGroup>)` ... グループ要素
- `Expression(Box<RuleExpression>)` ... 表現字句要素

## RuleGroupKind 列挙型

グループの種別を定義する。

- `Choice` ... 選択
- `Sequence` ... 連接

## RuleGroup 構造体

グループを定義する。

- グループ固有の UUID `uuid`
- グループの種別 (選択もしくは連接) `kind`
- グループが持つ規則要素 `subelems`
- AST 反映方式 `ast_reflection_style`
- 先読み方式 `lookahead_kind`
- 繰り返しの範囲 `loop_range`
- 要素順序の指定 (逐次的もしくは順不同的) `elem_order`

## RuleExpression 構造体

表現字句を定義する。

- トークン位置 `pos`
- 表現字句の種別 `kind`
- 表現字句の値 `value`
- AST 反映方式 `ast_reflection_style`
- 先読み方式 `lookahead_kind`
- 繰り返し範囲 `loop_range`
