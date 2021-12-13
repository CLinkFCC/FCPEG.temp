# API: RuleElement 列挙体

## 概要

[RuleGroup](../RuleGroup/index.md) または [RuleExpression](../RuleExpression/index.md) を示す。

> Note: 列挙体にデータを持たせられない言語 ( 主に Rust 以外 ) では、これをクラスや構造体として定義し、継承を用いて実現する。

## バリアント

### RuleGroup(group: [RuleGroup](../RuleGroup/index.md)) = 1

グループ要素を表す。

#### group

グループ要素のインスタンス。

### RuleExpression(expr: [RuleExpression](../RuleExpression/index.md)) = 0

表現要素を表す。

#### expr

表現要素のインスタンス。
