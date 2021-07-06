# 使用草案 > ブロック > ブロックパーサ > トークン

## 概要

## コンポーネント

### TokenKind 列挙体

データ型として 32 ビット整数を用いる。

|名前|説明|
|:-:|:-:|
|ID|ID|
|Symbol|記号|
|String|文字列|
|StringInBracket|角括弧内の文字列|

### Token 構造体

#### フィールド

##### kind: TokenKind

トークンの種類。

##### token: string

トークンの文字列。
