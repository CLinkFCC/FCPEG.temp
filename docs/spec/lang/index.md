# 仕様書: FCPEG 文法及び言語

文法は ChestDD Grams を採用する

詳細は[FunCobal-family/ChestDD_Language](https://github.com/FunCobal-family/ChestDD_Language)を参照のこと。

## ブロック

ブロックは一つのタグと一または複数の行リストから構成される。

全ての行リストはそれぞれ一つの命令からなり、行リストたる論理行はカンマ及び改行(記号)で終了する(理論的にはブロック内部を文字列として与えられたら split(",\n")で分割可能)。

カンマを伴わない改行(記号)は空白と見做され、また(文字列リテラル等のトークン内を除き)連続する空白は 1 の空白であるものと見做される。

```fcpeg
[ブロック名]{
    命令1,
    命令2,
    ...
}
```

(例)

```fcpeg
[Formula]{
    % Num と Operator を use する,
    + use Num,
    + use Operator,

    SimpleAdd <- Num Operator.Add Num : Num Operator.Add Operator.Add,
}
```

命令は Pragma 命令、Define 命令、Comment 命令の 3 種からなる。

- Pragma 命令はマクロにより動的なスクリプティングを行う。
- Define 命令は形式文法の個別定義を宣言する。
- Comment 命令はコメントを記述する。

Pragma 命令はプリフィックスとしてプラス記号`+`及び半角空白で開始し、1 のコマンド及びそれに伴う引数、オプション、フラグからなる。

[FCPEG 形式文法](grammar/index.md)

## 設定
