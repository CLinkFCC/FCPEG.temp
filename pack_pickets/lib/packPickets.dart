import "packge:pack_pickets/src/pickets.dart";
import "packge:pack_pickets/src/creater.dart";
import "packge:pack_pickets/src/detector.dart";
import "packge:pack_pickets/src/publisher.dart";
import "packge:pack_pickets/src/mover.dart";

class PackPickets{
  PicketsEnvEnv _env;
  Creater _creater;
  Detector _detector;
  Publisher _publisher;
  Mover _mover;
  PackPickets(){
    this._env = PicketsEnv();
    this._creater = Creater(this._env);
    this._detector = Detector(this._env);
    this._publisher = Publisher(this._env);
    this._mover = Mover(this._env);
  }
  PicketsEnv get env => this._env;
  Creater get creater => this._creater;
  Detector get detector => this._detector;
  Publisher get publisher => this._publisher;
  Mover get mover => this._mover;
}