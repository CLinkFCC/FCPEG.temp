# 仕様草案 > 処理手順 > 構文パーサ

## 概要

## Pragma 命令

### ブロックの use

### ブロックの start

`Block.start_block` に設定された規則によるパースを開始する。

`BlockParser.has_parse_started` が `true` の場合はエラーを送出する。

### 規則

```
[Block]{
    Rule <- &"A" "B" : ("C" "D"?)?
}
```

#### ブロック


