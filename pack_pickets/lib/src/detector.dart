import "packge:pack_pickets/src/pickets.dart";

class Detector extends PicketWorker {
  Detector(PicketsEnv env){
    this._env=env;
    this._modifyMode = false;
  }
Future<bool> detectFile(String filePath){
  File file = File(filePath);
  FileSystemModifyEvent me = await file.watch(FileSystemEvent.modify) as FileSystemModifyEvent;
  return contentChanged;
}
  Future<String> detectPkg(){
  }