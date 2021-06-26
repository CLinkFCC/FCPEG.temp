# 仕様草案 > ブロック > コマンド

## 概要

## コンポーネント

### Pragma 構造体

#### フィールド

##### kind: [PragmaKind](./index.md#PragmaKind%20%列挙体)

プラグマ命令の種類。

##### args: Vec\<string>

プラグマ命令の引数。

### PragmaKind 列挙体

データ型として 32 ビット整数を用いる。

|名前|説明|
|:-:|:-:|
|Import|import 文|
|Use|use 文|
|Start|start 文|
|Apply|apply 文|
|Sugar|sugar 文|
|Mode|mode 文|
|Set|set 文|
