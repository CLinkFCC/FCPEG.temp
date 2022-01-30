# lib モジュール

外部クレート向けに FCPEG API を提供する。

## FCPEGParser 構造体

### load()

FCPEG ファイルを読み込んで `FCPEGParser` インスタンスを取得する。

- `lib_fcpeg_file_map` ... 追加の外部 FCPEG ファイル (エイリアス名とパスのマップ; cfg の `FileAliases` とは異なる)
- `enable_memoization` ... メモ化を用いるかどうか (基本的に `true`)

### parse_from_path()

入力ファイルパスを受け取ってパースする。

### parse_from_str()

入力文字列を受け取ってパースする。
