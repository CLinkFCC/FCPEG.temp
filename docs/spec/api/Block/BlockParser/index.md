# BlockParser 構造体/クラス

## 概要

FCPEG ブロックをパースし、最終的に規則マップを取得する。

## メソッド

### get_rule_map(Vec\<FCPEGFile>)

> 定義:
> public static get_rule_map(fcpeg_file: Vec\<FCPEGFile>);
> 返り値:
> RuleMap
> エラー:
> BlockParseError

### to_syntax_tree(SyntaxParser, string)

> 定義:
> public static to_syntax_tree(parser: SyntaxParser, fcpeg_src: string);
> 返り値:
> SyntaxTree
> エラー:
> BlockParseError

### to_block_map(SyntaxTree)

> 定義:
> private static to_block_map(tree: SyntaxTree)
> 返り値:
> BlockMap
> エラー:
> BlockParseError

### to_block_cmd(cmd_node_list: SyntaxNodeList)

> 定義:
> to_block_cmd(cmd_node_list: SyntaxNodeList)
> 返り値:
> BlockCommand
> エラー:
> BlockParseError
