# 仕様草案 > API > メインページ

## 概要

主に実装すべきコンポーネントについて記述する。

## コンポーネント

各コンポーネントセクションでは API 内のクラス, 構造体, インタフェイスなどを記述する。

### 名称

- クラス ... 機能を主とした、データと機能の集まり
- 構造体 ... データを主とした、データと機能の集まり
- フィールド ... クラスまたは構造体が持つデータ
- メソッド ... クラスまたは構造体が持つ機能
- 列挙体 ... 列挙的な名前付き定数の集まり

### 記述における例外

実装に用いる機能や型: 仕様書に記述された限りでなく、実装言語によって要求された機能を持つ、かつ適切なものを選ぶこと。

- (例a) 指定された型 `map<string, string>` を、Rust において `HashMap<String, String>` を用いて実装する。
- (例b) Rust において、インタフェイスの代用としてトレイトを用いる。

命名規則: 実装言語ごとに指定されたコーディング規約の命名規則に従うこと。

- (例) 指定されたフィールド名 `loop_type` を、Dart において `loopType` と命名する。

### メソッド

#### 表記

```
[修飾子] [返り値の型] メソッド名([引数])

(例a) get_string()             // void は省略する
(例b) priv string get_string() // priv = private 修飾子
```

#### 返り値

`null` ... Rust では `None` を用いる。また、返り値の型には `Option<T1, T2>` を用いる。

#### オーバーロード

オーバーロードを持たない言語があるため用いない。