class Block{
  String tag;
  List<PragmaCom> pcs;
  List<RuleDef> rds;
  List<Comments> coms;

  Block(String tag){
    this.tag = tag;
    this.pcs = List();
    this.rds = List();
    this.coms = List();
  }
  void update(String lines){
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

enum PragmaCom{}
class RuleDef{
  String
  bool dataFlag;
}
class Comments extends String{
  static docCom(List<Comments> coms){}
}

class SyntaxNode{
  String name;

  SyntaxNode(String name)=>this.name=name;
}