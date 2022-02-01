# tree モジュール

AST に関する機能を定義する。

## CharacterPosition 構造体

ファイルにおける文字の位置を表す。

- ファイルパス `file_path`
- インデックス `index`
- 行数 `line`
- 列数 `row`

## ASTReflectionStyle 列挙型

AST 反映方式を定義する。

- `Reflection(ASTReflectionName)` ... 反映的; 引数は反映名
- `NoReflection` ... 非反映的
- `Expansion` ... 展開的

## SyntaxTree 構造体

- 子要素ノード `child`

## SyntaxChild 列挙型

構文ノードの子要素を定義する。

- `Node(Box<SyntaxNode>)` ... ノード; 引数はノードインスタンス
- `Leaf(Box<SyntaxLeaf>)` ... リーフ; 引数はリーフインスタンス

## SyntaxNode 構造体

構文ノードを定義する。

- ノード UUID `id`
- 子要素リスト `subelems`
- AST 反映方式 `ast_reflection_style`

## SyntaxLeaf 構造体

構文リーフを定義する。

- リーフ UUID `id`
- トークン位置 `pos`
- トークン文字列 `value`
- AST 反映方式 `ast_reflection_style`
