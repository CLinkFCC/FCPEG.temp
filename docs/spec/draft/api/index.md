# API 仕様書

## 概要

## 注釈

### 命名規則

ここでは Rust 等のコーディングスタイルに基づき命名を行っている。
言語もしくはプロジェクトで定義されたスタイルに従って識別子を変更すること。

### 名前空間とモジュール

### 構造体とクラス

クラスが存在する言語では用途に応じて構造体と使い分ける。
具体的にはデータ構造を定義する目的でのみ構造体を用いる。

### メソッド定義

例えばこのように記述される。

> 定義:
> public static get_rule_map(fcpeg_file: Vec\<FCPEGFile>);
> 返り値:
> RuleMap
> エラー:
> BlockParseError

言語によって適切な定義を行うこと。
引数については必要に応じて参照を用いること。

### エラーと例外

一般的に例外を用いる言語 (e.g. Java, C#) ではエラーでなく例外を用いる。
その際、例外クラスは接尾辞 `Exception` で命名する。(e.g. `BlockParseError` -> `BlockParseException`)

## 名前空間

- [BlockLexer 名前空間](BlockLexer/index.md)
