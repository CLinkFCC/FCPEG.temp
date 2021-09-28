import "packge:pack_pickets/src/pickets.dart";
import "packge:pack_pickets/src/errs.dart";

class SpecLoader{
  PPPkgsEnv _env;
  PPSpecLoader(this.env);
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
  PicketErr picketSpecParse(String picketSpecStr){
  }
}

