# Pickets

FCPEG has a package system, PackPickets, and the Package is named as "Picket".

Each Picket may be published to any of the host sites. One of these host sites, which are officially operated, are named PicketDepot.

Each picket must has a `pegspec.cfg`, specification and configuration file.

Each picket has one or more .fcpeg files at under `lib` dir, the body of FCPEG.

Each Picket has either kind "product" or "library".

- "product" shall be some language, protocol or other specific syntactic definition.
- "library" shall be a collection of FCPEG source code that has been assembled into parts that can be called from other Pickets.

Each Picket can contain `env` dir for additional `spec.cfg` files, `exec` dir for scripts includes test and tools, `docs` dir for documentation includes specifications, and `inst` dir for example.

Each of the above directories may have subdirectories as appropriate.

Also generally, as Liberty Software, each Pickets has `README.md`、`LICENSE.md`、`AUTHORS.md` and `CHANGELOG.md`.

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
