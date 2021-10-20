# API: RuleChoiceFeatures 構造体

## 概要

選択要素としての特徴を示す。

## フィールド

### sub_elems: Vec<[RuleElement](../RuleElement/index.md)>

要素がもつサブ要素の一覧。

### random_order: [RuleElementRandomOrder](../RuleElementRandomOrder/index.md)

要素の順不同性を示す。

### is_choosable: bool

サブ要素が選択要素であるか。

(例) `(e1 e2)` であれば `false` / `(e1 : e2)` であれば `true`
