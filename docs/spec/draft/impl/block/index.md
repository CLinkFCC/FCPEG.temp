# 仕様草案 > ブロック

## コンポーネント

```
% For your reference...

% Block
[BlockKey]{
    % Commands
    + cmdname CommandTarget,
    % Rules
    RuleKey <- ContentItem,
}
```

### Block 構造体

ブロックのデータを持つ。

#### フィールド

##### key: string

ブロックのキー。

##### commands: map\<string, [Command](./command/index.md#Command%20%構造体)>

ブロックが持つコマンドとそのキーのマップ。

検索時の手間を減らすためキーをもつ。

##### rules: map\<string, [Rule](./rule/index.md)>

ブロックが持つ規則とそのキーの一覧。

検索時の手間を減らすためキーをもつ。
