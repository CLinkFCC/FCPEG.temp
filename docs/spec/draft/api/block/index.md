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

<!-- ##### pragma: map\<string, [Pragma](./pragma/index.md#Pragma%20%構造体)>

ブロックが持つ Pragma 命令とそのキーのマップ。

検索時の手間を減らすためキーをもつ。 -->

##### using_blocks: map\<string, string>

use 文で記述されたブロック名と置換先のブロック名。

##### start_block: string

start 文で記述された規則名。

###### 初期値

`""` もしくは `None`

##### rules: map\<string, [Rule](./rule/index.md)>

ブロックが持つ規則とそのキーの一覧。

検索時の手間を減らすためキーをもつ。