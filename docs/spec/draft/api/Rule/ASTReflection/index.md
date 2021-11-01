# API: ASTReflection 列挙体

## 概要

要素の AST への反映方法を表す。

## バリアント

### Reflectable(name: String) = 0

ノードとして AST に反映させる。

#### name

AST におけるノード名。

### Unreflectable = 1

ノードとして AST に反映させない。

### Expandable = 2

> todo: リンク調整

親ノードに [展開](../../../impl/ast/expansion/index.md) する。
