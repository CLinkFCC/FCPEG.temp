import "dart:io";

import "packge:pack_pickets/src/pickets.dart";

//Create Picket
class Creater{
  PicketsEnv _env;
  Creater(PicketsEnv env){
    this._env=env;
  }
  PicketOrErr create(String picketName,String currentPath){
    if(this._env.pickets.containsKey(picketName)){
      if(this._env.pickets[picketName] == [currentPath,picketName].join("/")){
        PicketOrErr.err(PicketExistsAlreadyErr(picketName));
      }
    }else{
      this._env.pickets[picketName]=[currentPath,picketName].join("/");
      this._env.writeBack();
      return his.stagehand(picketName,currentPath)
    }
  }
  PicketOrErr stagehand(String picketName,String currentPath){
  }
  PicketsEnv onFinallly(){
    return this._env;
  }
}