# 草案 > 実装 > ログ

## 概要

ログはエラー, 警告, 通知を含む。詳細は [ログ種類](./index.md#ログの種類) を参照すること。

エラーコードは基本実装において使用しないが、便宜上番号を割り振っている。

## ログの種類

|名称|英称|英略称|ケース|
|:-:|:-:|:-:|:-:|
|エラー|error|err|プログラムの実行継続が致命的に不可能である|
|警告|warning|warn|実行継続は可能だが操作や状態が不適切である|
|通知|notice|note|実行に問題はないがユーザに有益な情報を通知する|

## ログ一覧

### エラー一覧

|分類|タイトル|引数|説明|
|:-:|:-:|:-:|:-:|:-:|
|Other|UnknownError|-|不明なエラー|
|File|FilePathNotFound|file_path|\<file_path> が見つからない|
|Parse|ExpectedEOF|index|EOF が見つからない|
|Parse|UnexpectedEOF|index|予期しない EOF<br>( 閉じ括弧が見つからない場合など )|

### 警告一覧

|分類|タイトル|引数|説明|
|:-:|:-:|:-:|:-:|:-:|
|Other|UnknownWarning|-|不明な警告|

### 通知

|分類|タイトル|引数|説明|
|:-:|:-:|:-:|:-:|:-:|
|Other|UnknownNotice|-|不明な通知|
|Software|NewVersionAvailable|-|新しいバージョンが有効|

