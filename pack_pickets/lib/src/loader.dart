import "packge:pack_pickets/src/pickets.dart";
import "packge:pack_pickets/src/errs.dart";

class SpecLoader extends PicketWorker {
  SpecLoader(PicketsEnv env){
    this._env = env;
    this._modifyMode = false;
  }
  PicketOrErr load(String picketName){
    if(this._env.pickets.containsKey(picketName)){
      String picketSpecPath = [this._env.pickets[picketName],"pegspec.cfg"].join("/");
      File picketSpec = File(picketSpecPath);
      if(picket.existsSync()){
        String picketSpecStr = picket.readAsStringSync();
        return picket = this.picketSpecParse(picketSpecStr);
      }else{
        return PicketOrErr.err(PicketSpecNotFoundErr(picketName));
      }
    }else{
      return PicketOrErr.err(PicketNotFoundErr(picketName));
    }
  }
  PicketOrErr picketSpecParse(String picketSpecStr){
    List<String> picketSpecLines = picketSpecStr.split("\n");
    String multiCommentsStart = "{|";
    String multiCommentsEnd = "|}";
    String singleCommentsStart = "-- ";
    String nesterStart = "||";
    List<int> multiCommentsStartLines = picketSpecLines.map((String line)=>line.startWith(multiCommentsStart)).asMap().where((int i,bool b)=>b).map((int i,bool b)=>i).toList();
    List<int> multiCommentsEndLines = picketSpecLines.map((String line)=>line.endWith(multiCommentsEnd)).asMap().where((int i,bool b)=>b).map((int i,bool b)=>i).toList();
    List<int> singleCommentsLines = picketSpecLines.map((String line)=>line.startWith(singleCommentsStart)).asMap().where((int i,bool b)=>b).map((int i,bool b)=>i).toList();
    if(multiCommentsStartLines.length != multiCommentsEndLines.length){
      return PicketOrErr.err(PicketSpecParseErr("number of multi comments start and end lines are not equal"));
    }else{}
  }
}

