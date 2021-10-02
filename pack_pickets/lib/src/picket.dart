import "dart:io";
import "packge:pack_pickets/src/errs.dart";

class Picket{
  bool operator==(const Picket other) {}
}

class PicketOrErr{
  ResultState _state;
  Picket? _p;
  PicketErr? _e;
  PicketOrErr.go(this._p):this._state=ResultState.Ok;
  PicketOrErr.die(this._e):this._state=ResultState.Err;
  bool get isOk => this._state==ResultState.Ok;
  bool get isErr => this._state==ResultState.Err;
  ResultState get state => this._state;
  bool contains(Picket p){
    if(this.isOk){
      return this._p==p;
    }else{
      return false;
    }
  }
  void okOn(void Functon(Picket) f){
    if(this.isOk){
      f(this._p);
    }else{
      assert(false);
      throw Error();
    }
  }
  void errOn(void Functon(PicketErr) f){
    if(this.isErr){
      f(this._e);
    }else{
      assert(false);
      throw Error();
    }
  }
  Picket unwrap(){
    if(this.isOk){
      return this._p;
  }else{
    assert(false);
    throw Error();
  }
  Error? okOr(void Functon(Picket?) f,Error e){
    if(this.isOk){
      f(this._p);
      return null;
    }else{
      return e;
    }
  }

}

enum ResultState{
  Ok,
  Err,
}
class PicketsEnv{
  Map<String,String> _pickets;
  PicketsEnv(){
    this._pickets = {};
  }
  PicketsEnv launch(){
    String base = this.userHomeDir();
    String filePPCfg = ".PackPickets/packages.cfg";
    String pathPPCfg = [base,filePPCfg].join("/");
    File ppCfg = File(pathPPCfg);
    if(!ppCfg.existsSync()){
      ppCfg.createSync();
    }
    List<MapEntry<String, String>> picketEntryList = ppCfg.readAsStringSync().split("\n").where((String line)=> !line.startWith("#")).map((String line)=>line.split("\t").toList()).where((List<String> ls)=>ls.length==2).map((List<String> ls)=>MapEntry<String, String>(ls[0],ls[1])).toList();
    this._pickets.addEntries(picketEntryList);
    return this;
  }
  String userHomeDir(){
    String os = Platform.operatingSystem;
    String home = "";
    Map<String, String> envVars = Platform.environment;
    if (Platform.isMacOS) {
      home = envVars["HOME"] ?? "";
    } else if (Platform.isLinux) {
    home = envVars["HOME"] ?? "";
    } else if (Platform.isWindows) {
      home = envVars["UserProfile"] ?? "";
    }
    return home;
  }
  Map<String,String> get pickets => this._pickets;
PicketsEnv writeBack(){
  String base = this.userHomeDir();
  String filePPCfg = ".PackPickets/packages.cfg";
  String pathPPCfg = [base,filePPCfg].join("/");
  File ppCfg = File(pathPPCfg);
  if(!ppCfg.existsSync()){
    ppCfg.createSync();
  }
  List<String> lines = this._pickets.entries.map((MapEntry<String, String> entry)=>[entry.key,entry.value].join("\t")).toList();
  ppCfg.writeAsStringSync(lines.join("\n"));
  return this;
}

}