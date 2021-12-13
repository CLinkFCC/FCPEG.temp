# FCPEGFile 構造体/クラス

## 概要

FCPEG ファイルを 1 ファイルごとに管理する。

## フィールド

### alias_name: string

### file_path: string

### file_content: string

### config_file: ConfigFile

## メソッド

### load(alias_name: string, file_path: string)

FCPEG ファイルを読み込んで [FCPEGFile](index.md) インスタンスを取得する。
