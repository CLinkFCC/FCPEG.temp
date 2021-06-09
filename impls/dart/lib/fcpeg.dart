enum RecKind{
  Prag,Com,Def,
}
class FCPEG{
//Str block_tag, Str kind, Str data/RuleDef rule
static FCPEG parsePEG(String imput) {
  FCPEG peg = FCPEG();
  imput.split("}\n").map((value) {
    value.split("]{\n").indexedMap((ind, val) {
      if (ind == 0){
        String tag = val.left(1);
      } else if (ind == 1) {
        val.split(",\n")map((vars){
           if(vars.startWith("+")){
              //pragma系命令
              peg.addRec(tag,RecKind.Com,vars);
           } else if(vars.startWith("%")) {
              //comment
              peg.addRec(tag,RecKind.Com,vars);
           } else {
　　　　　　　　//define系命令
              vars.split(" <- ").map((){
              });
           }
        });
      } else {
      }
    });
  });
}
void addRec(String tag, RecKind kind,){}
SyntaxNode parseCode(String im){}
}
class FCPEGRec{
  int flag;
  String tag;
  ? 
  String? com;
  RuleDef?

  }
class RuleDef{
  bool dataFlag;
}
class SyntaxNode{}