# 仕様草案 > ブロック > 規則

## 概要

## コンポーネント

```
% For your reference...

% Rule
% Choice 1: Seq_1_1 Seq_1_2
% Choice 2: Seq_2_1, Seq_2_2
RuleName <- Seq_1_1 Seq_1_2 : Seq_2_1 Seq_2_2
```

### Rule 構造体

規則のデータを持つ。

#### フィールド

##### key: string

規則のキー。

##### choices: vector\<[Choice](./index.md#Choice%20%構造体)>

規則が持つ選択の一覧。

### Choice 構造体

選択のデータを持つ。

#### フィールド

##### seqs: Vec\<[Sequence](./index.md#Sequence%20%構造体)>

選択が持つ連接の一覧。

### Sequence 構造体

連接のデータを持つ。

#### フィールド

##### exprs: Vec\<[Expression](./expr/index.md#Expression%20%構造体)>

連接が持つエクスプレッションの一覧。
