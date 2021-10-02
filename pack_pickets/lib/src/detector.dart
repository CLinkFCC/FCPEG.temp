import "packge:pack_pickets/src/pickets.dart";

class Detector{
  PicketsEnv _env;
  Detector(PicketsEnv env){
    this._env=env;
  }
Future<bool> detectFile(String filePath){
  File file = File(filePath);
  FileSystemModifyEvent me = await file.watch(FileSystemEvent.modify) as FileSystemModifyEvent;
  return contentChanged;
}
  Future<String> detectPkg(){
  }