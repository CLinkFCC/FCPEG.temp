# API: ASTReflectionStyle 列挙体

## 概要

要素の AST への反映方式を表す。
反映方式の詳細は [抽象構文木 (AST) #AST 反映方式](../../../spec/ast/index.md#AST%20%反映方式) を参照。

## バリアント

### Reflection(name: String) = 0

ノードとして AST に反映させる。

#### name

AST におけるノード名。

### NoReflection = 1

ノードとして AST に反映させない。

### Expansion = 2

親ノードに展開する。
