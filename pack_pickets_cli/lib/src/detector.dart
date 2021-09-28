import "packge:pack_pickets/src/pickets.dart";

class Detector{
  PPPkgsEnv env;
  PPDetector(this.env);
Future<bool> detectFile(String filePath){
  File file = File(filePath);
  FileSystemModifyEvent me = await file.watch(FileSystemEvent.modify) as FileSystemModifyEvent;
  return contentChanged;
}
  Future<String> detectPkg(){
  }