import "package:fcpeg/stdLib.dart";
//Block will be moved to ChestDD
class Block<T>{
  late String _tag;
  late List<T> _pcs;

  Block<T>(String tag){
    this._tag = tag;
    this._pcs = <T>[];
  }
}
class PegBlock extends Block<Map<Cmdkind,Command>>{
  List<String> _outOfBinds = <String>[];
  PegBlock(String tag, String lines){
    this._tag = tag;
    String delim = ",\n";
    List<String> linesList = lins.split(delim).where((String line)=>line!="").toList();
    this.update(lines);
  }
  void update(List<String> lines){
    lines.split(",\n")map((String vars){
      if(vars.startWith("+")){
        //pragma系命令
        this.addPragma(tag,RecKind.Com,vars);
      } else if(vars.startWith("%")) {
        //comment
        this.addComm(tag,RecKind.Com,vars);
      } else {
        //define系命令
        vars.split(" <- ").map((){
        });
            }
          });
  }
  void addPragma(){}
  void addRuleDef(){}
  void addComm(){}
}

class Command{
  factory Command(Cmdkind kind, List<String> lineTokens){
      return Pragma(lines);
    if(kind == CmdKind.Pragma){
    }else if(kind == CmdKind.RuleDef){
      return RuleDef(lines);
    }else if(kind == CmdKind.Comments){
      Comments(lines);
    }else{
      Commands._onFailed(lines);
    }
  }
  Commands._onFailed(List<String> lines){}
  static outOfBinds()
}
enum CmdKind{
  Pragma,
  RuleDef,
  Comments,
}
class Pragma extends Commands{
  List<Map<String,String>> coms = [
    {"name":"interop","desc":"interoperation with other formal grammers", "from":"v1r", "on": "both", "syntax":""},
    {"name":"pub","desc":"re-publish another", "from":"v1r", "on": "both", "opts":"", "args":"DefRule"},
    {"name":"mod","desc":"module of script", "from":"v1r", "on": "", "syntax":""},
    {"name":"alias","desc":"desuger suger-syntaxes", "from":"v1r", "on": "both", "syntax":""},
    {"name":"start","desc":"", "from":"v0", "on": "both", "syntax":""},
    {"name":"conf","desc":"", "from":"v0", "on": "file", "syntax":""},
    {"name":"use","desc":"", "from":"v0", "on": "both", "syntax":""},
  ];
}
class RuleDef extends Command{
  late NonTerminalSym _atm;
  late Expression _exp;
  final String _difn = "<-";
  late bool _dataFlag;
  Ruledef(String line){}
}
class Comment extends Command with String{
  late String _commentLines;
  late bool _isMulti;
  Comments(List<String> lines){}
}
enum ExpressionKind{
  OnPre,
  OnPost,
}
//記号(原子語句)又はメタ記号(正規組立字句又は文脈叙述字句)
class Expression{}
//原子語句
class AtomExp extends Expression{}
//非終端記号
class NonTerminalSym extends AtomExp{}
//終端記号
class TerminalSym extends AtomExp{}
//空文字列
class VoidSym extends AtomExp{}
//メタ記号
class MetaExp extends Expression{
  late String _name;
  late String _desc;
  late String _pre;
  late String _mid;
  late String _post;
  late int _expnum;
  late List<Expression> _exps;
}
//正規組立字句
class RegConst extends MetaExp{
}
//文脈叙述字句
class ContextDef extends MetaExp{}
//トークン列
class TokenLine{
  List<Expression> _env;
  Queue<Token> _que;
  TokenLine(List<Expression> env){
    this._env = env;
    this._que = Queue<Token>();
  }
  TokenLine push(Token tk){
    this._que.add(tk);
    return this;
  }
  TokenLine pushAll(List<Token> tkl){
    this._que.addAll(tkl);
    return this;
  }
  //文脈叙述字句による先読み。(predicate)
  bool expect(Token tk){
    List<Token> tkql = this._que.toList();
    return tkql.map((Token tk)=>tk.examinate(this._env)).reduce((bool curr, bool prev)=>curr||prev);
  }
  bool expectFor(List<Token> tkl){
    return tkl.map((Token tk)=>this.expect(tk)).reduce((bool curr, bool prev)=>curr||prev);
  }
  Token next(){
    return this._que.removeFirst();
  }
  List<Token> nextFor(int n){
    List<Token> ret = [];
    return this._nextForI(ret,n);
  }
  List<Token> _nextForI(List<Token> ret, int n){
    if(n < 1){
      return ret;
    }else{
      ret.add(this.next());
      return this._nextForI(ret, n - 1);
    }
  }
}
extension Token on String{
  TokenLine asToken([String delimiter=" "){
    return TokenLine().pushAll(this.split(delimiter).map((String s)=>Token(s)).toList());
  }
}
class Token{
  late String _token;
  Token(String tks){
    this._token = tks;
  }
  bool examinate(List<Expression> exps){
  }
}