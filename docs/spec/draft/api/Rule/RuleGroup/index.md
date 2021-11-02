# API: RuleGroup 構造体

## 概要

FCPEG ルールのグループ要素を表す。

## フィールド

### is_choice: bool

グループが選択要素であるか。

(例) `(e1 e2)` であれば `false` / `(e1 : e2)` であれば `true`

### sub_elems: Vec<[RuleElement](../RuleElement/index.md)>

要素がもつサブ要素の一覧。

### ast_reflection: [ASTReflection](../ASTReflection/index.md)

要素の AST への反映方法。

### lookahead_kind: [RuleElementLookaheadKind](../RuleElementLookaheadKind/index.md)

要素の先読み種別。

### loop: [RuleElementLoop](../RuleElementLoop/index.md)

要素の繰り返し情報。

### random_order: [RuleElementRandomOrder](../RuleElementRandomOrder/index.md)

要素の順不同性。
