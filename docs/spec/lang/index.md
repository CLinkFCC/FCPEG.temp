# 仕様書: FCPEG 文法及び言語

文法として ChestDD Grams を採用する。

詳細は [FunCobal-family/ChestDD_Language](https://github.com/FunCobal-family/ChestDD_Language) を参照。

FCPEG 構文の詳細は [FCPEG 形式文法](grammar/index.md) を参照。

## ブロック

ブロックは一つのタグと一または複数の行リストから構成される。

全ての行リストはそれぞれ一つの命令からなり、行リストたる論理行はカンマ及び改行で終了する。

カンマを伴わない改行は空白と見做され、また文字列リテラル等のトークン内を除き、連続する空白は 1 つの空白であるものと見做される。

ブロック構文:

```fcpeg
[ブロック名]{
    命令1,
    命令2,
    ...
}
```

ブロック定義の例:

```fcpeg
[Formula]{
    % Num と Operator を use する,
    + use Num,
    + use Operator,

    SimpleAdd <- Num Operator.Add Num : Num Operator.Add Operator.Add,
}
```

### 命令

命令 (macro) は以下の 3 種からなる。

| 命令名       | 構文                  | 役割                   |
| :-------: | :-----------------: | :------------------: |
| `Pragma`  | `+ command others,` | マクロにより動的なスクリプティングを行う |
| `Define`  | `rule <- syntax,`   | 規則 (形式文法の個別定義) を宣言する |
| `Comment` | `% comment,`        | コメントを記述する            |

`Pragma` 命令はプレフィックスとしてプラス記号 `+` 及び半角空白で開始し、また、1 つのコマンド及びそれに伴う引数、オプション、フラグからなる。

## 設定
