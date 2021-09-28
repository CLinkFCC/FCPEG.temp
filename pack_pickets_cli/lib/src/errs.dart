class PicketErr extends Exeption{}
class PicketNotFoundErr extends PicketErr{
  String _message;
  PicketNotFoundErr([String picketName]){
    if(picketName == null){
      this._message = "Requested Picket is not found: Unknown";
    }else{
      this._message = "Requested Picket is not found: $picketName";
    }
  }
  @override
  String toString() => this._message;
}
class PicketSpecNotFoundErr extends PicketErr{
  String _message;
  PicketSpecNotFoundErr([String picketName]){
    if(picketName == null){
      this._message = "Requested Picket Spec is not found: Unknown";
    }else{
      this._message = "Requested Picket Spec is not found: $picketName";
    }
  }
  @override
  String toString() => this._message;
}