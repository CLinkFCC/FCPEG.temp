# Pickets 和訳下書き

<!-- 思ったよりも文章変えちゃった -->
PackPickets is the packaging system for FCPEG, its packages are called pickets.
PackPickets は FCPEG 向けのパッケージシステムで、パッケージはピケット (picket) と呼ばれる。

You can publish pickets via dedicated hosts sites. PicketDepot is one of them, and it is officially managed.
ピケットは専用のホストサイトによって公開することができる。PicketDepot はその中の一つで公式に管理されている。

<!-- Rust のクレートでは単数形だった ("a crate"); picket は普通名詞なので小文字から -->
<!-- 読みやすさのためインラインコードは多めに使っていく -->
A picket have one `pegspec.cfg` , and at least one `.fcpeg` file in `lib` directory.
ピケットは 1 つの `pegspec.cfg` と、`lib` ディレクトリ内に最低でも 1 つの `.fcpeg` ファイルをもつ。

In `pegspec.cfg` , you describe specification and configuration of each picket.
`pegspec.cfg` では各ピケットの設計と構成を記述する。

A picket has following kinds:
ピケットには以下の種類がある:

- `product` ... normal FCPEG codes written for language, protocol, etc.
- `product` ... 言語やプロトコルなどのために記述された通常の FCPEG コード
- `library` ... an aggregation of FCPEG codes which used by external pickets.
- `library` ... 外部ピケットから利用される FCPEG コードの集合体

As options, you can put following directories in the root directory:
オプションとして、ルートディレクト下にこれらのディレクトリを置くことができる。

- `env/` ... for one or more `spec.cfg` as additional configuration
- `exec/` ... for scripts of utility tools, mainly for testing
- `docs/` ... for documentation includes specification
<!-- 修正案 instance の略はいろいろ被る (e.g.) instruction institution instrument -->
<!-- 何に向けたコード例...？ -->
- `examples/` ... for code examples of \[what]

- `env/` ... 付加的な構成としての 1 つ以上の `spec.cfg` のため
- `exec/` ... テスト目的を主とした有用なツールのスクリプトのため
- `docs/` ... 仕様を含むドキュメンテーションのため
- `examples/` ... \[what] のコード例のため

Above directories can have subdirectory as appropriate.
以上のディレクトは必要に応じてサブディレクトリをもつことができる。

<!-- 和訳が正しいかわからないので修正スキップ -->
Also generally, as Liberty Software, each Pickets has `README.md`、`LICENSE.md`、`AUTHORS.md` and `CHANGELOG.md`.
さらに通常は Liberty Software として各ピケットは `README.md`, `LICENSE.md`, `AUTHORS.md`, `CHANGELOG.md` をもちます。

## pegspec.cfg

The indent of `pegspec.cfg` id `||`, double bar, at a indent level.

The items of `pegspec.cfg` are bellow.

[BothKind Required]

- `Name`: Name of the Picket.

[BothKind Required]

- `Kind`: Kind of the Picket, `product` or `library`.

[BothKind Optional]

- `Author`: Author of the Picket.

[BothKind Required]

- `Version`: Version of the Picket.

[BothKind Required]

- `Desc`: Description of The Picket.

[BothKind Optional]

- `HomePage`: The URL of the location of the home page of this Picket (or a source code repository such as git).

[BothKind Optional]

- `Repo`: URL of the location of the git-like source code repository for the Picket.

[BothKind Optional]

- `Tracker`: URL of the location of the issue tracker for the Picket.

[BothKind Optional]

- `Doc` : For auto-documentation. This item isn't determined.

[BothKind Optional]

`Tags`: 1 ~ 5 keywords for the Picket to be found easily.

[OnlyProduct Required]

- `Lang`: Information of the language that the Picket defines for. This must have sub key-val set.

  [Key Required]

  - `Name`: The name of the language.

  [Key Required]

  - `Version`: The version of the language.

  [Key Optional]

  - `HomePage`: The URL of the location of the home page of the Picket (or a source code repository such as git)

  [Key Optional]

  - `Repo`: URL of the location of the git-like source code repository for the picket.

[BothKind Optional]

- `Dependencies`: This can be pretermitted in the Picket is not use any dependencies. This must have sub key-val set. This is allowed to have multiple sub key-val set .

  [Key Optional]

  - `{Name}`: {Name} is the Picket name that depended. Value is Version. This can have sub key-val set.

    [Key Optional]

    - `Path`: Local path or URL path that the Picket be. If this is used with `Git`, the path starts from the git Root. In publish, local path isn't be able to use.

    [Key Optional]

    - `Git`: URL that the git repository that has the Picket.

[BothKind Optional]

- `PublishTo`: Specifies where the Picket should be published. Default is "normal", for official settled.

[Product Optional]

- `EntryPoint`: The entry point to parse FCPEG source. This must have key-val set.

  [Key Optional]

  - `File`: The file name and path of entry point from package root. If use alias, the alias must covered by both `{` and `}`. Default is `main.fcpeg`.

  [Key Optional]

  - `Block`: The tag name of the block of entry point name. Default is `Main`.

[BothKind Optional]

- `FileAliases`: This must have sub key-val set. This can be pretermitted when any file isn't aliased. This is allowed to have multiple sub key-val set .

  [Key Optional]

  - `{AliasName}`: {AliasName} is the alias name. Value is file name and path that aliased. The path starts from the package root.

[BothKind Optional]

- `Includes`: This is used when you want to include other spec files. This must have key-val set. This can be pretermitted when any file isn't included. This is allowed to have multiple sub key-val set .

  [Key Optional]

  - `{AliasName}`: {AliasName} is the alias name. Value is `spec.cfg` file name and path that the `spec.cfg` includes. The path starts from the package root.

[BothKind Optional]

- `ASTReflect`: Specifies whether the elements marked with `#` will be added to the AST. Allowed Value is `normal` or `reversed`. The former adds all the basics and the latter the opposite. Default is "normal".

## Version

The format envisaged for the Version is as follows.

```fcpeg
  [Version]{
    Virsion <- VirsionCore (":" Revision)? ("-" Tag)? ("@" State)?
    VersionCore <- Major "." (Middle ".")? Miner "." Patch?,
    Major <- Number,
    Middle <- Number,
    Miner <- Number,
    Patch <- Number,
    Revision <- "R" Number,
    Number <- [1-9]?[0-9]+,
    -- Tag, such as "Alpha", "Beta", "PreRelease", "Release",
    Tag <- AlphNum,
    -- State, such as "Stable" or "Develop",
    State <- AlphNum,
    AlphNum <- [a-zA-Z][a-zA-Z]+,
  }
```

## Comment

Single Comment starts `-- `, double hyphen and single space.

Multiple Comment starts `{|`, single bra and single bar, and ends `|}`, single bar and single ket.
