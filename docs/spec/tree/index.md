# 仕様書: 構文木 - Syntax Tree

## 概要

FCPEG ではパース結果として構文木 (AST) を出力する。
ここでは構文木の構造や扱いについて説明する。

## 要素種別

|種別名|英名|子要素になれるか|データ|
|:-:|:-:|:-:|:-:|
|ツリー|tree|No|子要素の列|
|ノード|node|Yes|子要素の列|
|リーフ|leaf|Yes|リーフ値 (文字列)|

## 反映方式

### 性質と種別

反映方式には 2 つの性質がある。
反映性がある (要素が反映される) ことを反映的という。

- 反映性 ... 構文木に追加するかどうか
- 展開性 ... 子ノードを親ノードに展開するかどうか

反映方式の種別は以下のとおり。

- 反映 (reflection) ... 構文木に反映させる
- 非反映 (no-reflection) ... 構文木に反映させない
- 展開 (expansion) ... 子ノードを親ノードに展開する

### サンプルコード

FCPEG 規則:

```
ReflectionTest <- Reflection NoReflection Expansion,

% 反映的,
Reflection <- "reflection",

% 非反映的,
NoReflection <- "no reflection",

% 展開的,
Expansion <- "expansion",
```

構文木:

```
root
  | reflection
    |- "reflectable"
  |- "expandable"
```

## テキスト表記法

ルートは `root` で示す。
ノードの開始は `|` 、リーフの開始は `|-` である。
`|` と 3 スペースで字下げ表現をする。

```
root
|   | Node
|   |- Leaf
```

- ノード表記 ... `| node_name`
- リーフ表記 ... `|- "leaf_value"`

ノードが無名であればノード名を `[noname]` に置き換える。
リーフ値は必ず文字列であるため `"` で囲む。

要素が非反映的な場合は行末に 1 スペースと `#` をつける。

### サンプルコード

```
root
|   | Node1
|   |- "leaf-1" #
|   | [noname] #
|   |   |- "leaf-2"
|   |   | Node2
```
