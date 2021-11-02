# API: RuleExpression 構造体

## 概要

FCPEG ルールの表現要素を表す。

## フィールド

### position: [CharacterPosition](../../BlockLexer/CharacterPosition/index.md)

トークンの位置。

### kind: [RuleExpressionKind](../RuleExpressionKind/index.md)

表現の種別。

### value: string

表現要素の文字列値。

### ast_reflection: [ASTReflection](../ASTReflection/index.md)

要素の AST への反映方法。

### lookahead_kind: [RuleElementLookaheadKind](../RuleElementLookaheadKind/index.md)

要素の先読み種別。

### loop: [RuleElementLoop](../RuleElementLoop/index.md)

要素の繰り返し情報。

### random_order: [RuleElementRandomOrder](../RuleElementRandomOrder/index.md)

要素の順不同性。
