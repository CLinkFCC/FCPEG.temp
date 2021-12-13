# 仕様書: 抽象構文木 (AST)

## 概要

FCPEG では入力のパース結果として AST を出力する。
ここでは AST の構造や扱いについて説明する。

## 要素

- ノード
- リーフ

## AST 反映方式

FCPEG 規則の表現要素を AST へ反映する際の方式。

### 性質と種別

反映方式には 2 つの性質がある。

- 反映性 ... 表現要素をノードとして AST に追加するかどうか
- 展開性 ... 表現要素の子ノードを親ノードに展開するかどうか

具体的には以下の種別がある。

- 反映 (reflection) ... ノードとして AST に反映させる
- 非反映 (no-reflection) ... ノードとして AST に反映させない
- 展開 (expansion) ... 子ノードを親ノードに展開する

### コード例

FCPEG 規則

```
ReflectionTest <- Reflection NoReflection Expansion,

% 反映する,
Reflection <- "reflectable",

% 反映しない,
NoReflection <- "unreflectable",

% 展開する,
Expansion <- "expandable",
```

AST

```
root
  | reflection
    |- "reflectable"
  |- "expandable"
```
