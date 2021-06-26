# 仕様草案 > ブロック > 規則 > エクスプレッション

## 概要

## コンポーネント

### Expression 構造体

#### 例

|Expression|ExpressionKind|String Value|
|:-:|:-:|:-:|
|"e"|String|"a"|
|[a-zA-Z]|CharClass|"a-zA-Z"|
|.|Wildcard|""|

#### フィールド

##### kind: [ExpressionKind](./index.md#ExpressionKind%20%列挙体)

エクスプレッションの種類。

##### value: string

エクスプレッションの値。

##### loop_kind: [LoopKind](./index.md#LoopKind%20%列挙体)

繰り返しの種類。

##### lookahead_kind: [LookaheadKind](./index.md#LookaheadKind%20%列挙体)

先読みの種類。

### ExpressionKind 列挙体

データ型として 32 ビット整数を用いる。

|名前|値|
|:-:|:-:|
|String|0|
|CharClass|1|
|Wildcard|2|

### LoopKind 列挙体

データ型として 32 ビット整数を用いる。

|名前|値|
|:-:|:-:|
|One|0|
|OneOrMore|1|
|ZeroOrOne|2|
|ZeroOrMore|3|

### LookaheadKind 列挙体

データ型として 32 ビット整数を用いる。

|名前|値|
|:-:|:-:|
|Positive|0|
|Negative|1|
