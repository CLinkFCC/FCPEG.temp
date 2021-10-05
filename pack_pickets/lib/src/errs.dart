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
class PicketExistsAlreadyErr extends PicketErr{
  String _message;
  PicketExistsAlreadyErr([String picketName]){
    if(picketName == null){
      this._message = "Picket already exists: Unknown";
    }else{
      this._message = "Picket already exists: $picketName";
    }
  }
  @override
  String toString() => this._message;
}

class PicketVersionError extends PicketErr{
  String _message;
  PicketVersionError([String massage]){
    if(massage == null){
      this._message = "Picket version error: Unknown";
    }else{
      this._message = "Picket version error: $massage";
    }
  }
  @override
  String toString() => this._message;
}
