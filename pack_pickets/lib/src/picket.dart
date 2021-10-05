import "dart:io";
import "packge:pack_pickets/src/errs.dart";
extension MainString on String {
  String get first => this.split("")[0];
  String get last => this.split("")[this.length - 1];
  bool _isUpperCase(){
    if(this.length == 0){
      return false;
    }else if(this.length == 1){
      return this.contains(new RegExp(r"^[A-Z]$"));
    }else{
      return this.first.isUpperCase();
    }
  }
  bool _isLowerCase(){
    if(this.length == 0){
      return false;
    }else if(this.length == 1){
      return this.contains(new RegExp(r"^[a-z]$"));
    }else{
      return this.first.isLowerCase();
    }
  }
  bool _isDigit(){
    if(this.length == 0){
      return false;
    }else if(this.length == 1){
      return this.contains(new RegExp(r"^[0-9]$"));
    }else{
      return this.first.isDigit();
    }
  }
  bool _isAlpha()=>this._isUpperCase()||this._isLowerCase();
  bool _isAlphaNumeric()=>this._isAlpha()||this._isDigit();
  bool get isUpperCase => this._isUpperCase();
  bool get isLowerCase => this._isLowerCase();
  bool get isDigit => this._isDigit();
  bool get isAlpha => this._isAlpha();
  bool get isAlphaNumeric => this._isAlphaNumeric();
}
class PicketSpec{
  List<PicketEntriesKind> _mustHaveForBoth = [
    PicketEntriesKind.Name,
    PicketEntriesKind.Kind,
    PicketEntriesKind.Description,
    PicketEntriesKind.Version,
    PicketEntriesKind.Author,
    PicketEntriesKind.License,
  };
  List<PicketEntriesKind> _mustHaveForProduct = [
    PicketEntriesKind.Lang,
  ];
  List<PicketEntriesKind> _mustHaveForLibrary = [];
  PicketSpec(){}
  List<PicketEntriesKind> get mustHaveForBoth => _mustHaveForBoth;
  List<PicketEntriesKind> get mustHaveForProduct => _mustHaveForProduct;
  List<PicketEntriesKind> get mustHaveForLibrary => _mustHaveForLibrary;
}
class Picket{
  List<PicketEntriesKind> _having = [];
  bool operator==(const Picket other) {}
}
enum PicketEntriesKind{
  Comments,
  SingleComments,
  MultiComments,
  Name,
  Kind,
  Descreption,
  Lang,
  Tags,
  URL,
  Dependencies,
  Version,
  License,
  PublishTo,
  EntryPoint,
  RegExp,
  FileAliases,
  ASTReflect,
  File,
  Block,
  Alias,
}
extension PicketEntriesKindExtension on PicketEntriesKind{
  String get name{
    return ["Picket",this.toString().split(".").last].join("");
  }
  PicketEntries get instance{
    switch(this){
      case PicketEntriesKind.Comments:
        return PicketComments.abst();
      case PicketEntriesKind.SingleComments:
        return PicketSingleComments.abst();
      case PicketEntriesKind.MultiComments:
        return PicketMultiComments.abst();
      case PicketEntriesKind.Name:
        return PicketName.abst();
      case PicketEntriesKind.Kind:
        return PicketKind.abst();
      case PicketEntriesKind.Descreption:
        return PicketDescreption.abst();
      case PicketEntriesKind.Lang:
        return PicketLang.abst();
      case PicketEntriesKind.Tags: 
        return PicketTags.abst();
      case PicketEntriesKind.URL:
        return PicketURL.abst();
      case PicketEntriesKind.Dependencies:
        return PicketDependencies.abst();
      case PicketEntriesKind.Version:
        return PicketVersion.abst();
      case PicketEntriesKind.License:
        return PicketLicense.abst();
      case PicketEntriesKind.PublishTo:
        return PicketPublishTo.abst();
      case PicketEntriesKind.EntryPoint:
        return PicketEntryPoint.abst();
      case PicketEntriesKind.RegExp:
        return PicketRegExp.abst();
      case PicketEntriesKind.FileAliases:
        return PicketFileAliases.abst();
      case PicketEntriesKind.ASTReflect:
        return PicketASTReflect.abst();
      case PicketEntriesKind.File: 
        return PicketFile.abst();
      case PicketEntriesKind.Block:
        return PicketBlock.abst();
      case PicketEntriesKind.Alias:
        return PicketAlias.abst();
      default:
        throw UnsupportedPicketEntriesKindErr(this);
    }
  }
  int get nesterLevel => this.instance.nesterLevel;
  String get key => this.instance.key;
  bool get _childHoldable => this.instance.childHoldable;
  static PicketEntriesKind fromKey(String key){
    switch(key){
      case "Name":
        return PicketEntriesKind.Name;
      case "Kind":
        return PicketEntriesKind.Kind;
      case "Descreption":
        return PicketEntriesKind.Descreption;
      case "Lang":
        return PicketEntriesKind.Lang;
      case "Tags":
        return PicketEntriesKind.Tags;
      case "URL":
        return PicketEntriesKind.URL;
      case "Dependencies":
        return PicketEntriesKind.Dependencies;
      case "Version":
        return PicketEntriesKind.Version;
      case "License":
        return PicketEntriesKind.License;
      case "PublishTo":
        return PicketEntriesKind.PublishTo;
      case "EntryPoint":
        return PicketEntriesKind.EntryPoint;
      case "RegExp":
        return PicketEntriesKind.RegExp;
      case "FileAliases":
        return PicketEntriesKind.FileAliases;
      case "ASTReflect":
        return PicketEntriesKind.ASTReflect;
      case "File":
        return PicketEntriesKind.File;
      case "Block":
        return PicketEntriesKind.Block;
      default:
        if(key.first.isUpperCase&&key.split("").every((String c)=>c.isAlphaNumeric)){
          return PicketEntriesKind.Alias;
        }else{
          throw UnsupportedPicketEntriesKindErr(key);
        }
    }
  }
}
class PicketEntriesKindRelations{
  Map<PicketEntriesKind,List<PicketEntriesKind>> _inheritance = {
  };
  Map<PicketEntriesKind,List<PicketEntriesKind>> _parent = {
  };
  Map<PicketEntriesKind,List<PicketEntriesKind>> get inheritance => _inheritance;
  Map<PicketEntriesKind,List<PicketEntriesKind>> get parent => _parent;
}
class PicketEntries{
  PicketEntriesKind _kind;
  bool _childHoldable;
  bool _hasChild;
  List<PicketEntries> _children;
  String _key;
  int _nesterLevel;
  int get nesterLevel => this._nesterLevel;
  String get key => this._key;
  bool get childHoldable => this._childHoldable;
}
class PicketComments extends PicketEntries{}
class PicketSingleComments extends PicketComments{}
class PicketMultiComments extends PicketComments{}
class PicketName extends PicketEntries{
  String _value;
  PicketName(String value){
    this._value = value;
    this._kind = PicketEntriesKind.Name;
    this._childHoldable = false;
    this._hasChild = false;
    this._children = [];
    this._key = "Name";
    this._nesterLevel = 1;
  }
  PicketName.abst(){
    this._value = "";
    this._kind = PicketEntriesKind.Name;
    this._childHoldable = false;
    this._hasChild = false;
    this._children = [];
    this._key = "Name";
    this._nesterLevel = 1;
  }
}
class PicketDescription extends PicketEntries{
  String _value;
  PicketDescription(String value){
    this._value = value;
    this._kind = PicketEntriesKind.Descreption;
    this._childHoldable = false;
    this._hasChild = false;
    this._children = [];
    this._key = "Description";
    this._nesterLevel = 1;
  }
  PicketDescription.abst(){
    this._value = "";
    this._kind = PicketEntriesKind.Descreption;
    this._childHoldable = false;
    this._hasChild = false;
    this._children = [];
    this._key = "Description";
    this._nesterLevel = 1;
  }

}
class PicketTags extends PicketEntries{
  List<String> _value;
  PicketTags(List<String> value){
    this._value = value;
    this._kind = PicketEntriesKind.Tags;
    this._childHoldable = false;
    this._hasChild = false;
    this._children = [];
    this._key = "Tags";
    this._nesterLevel = 1;
  }
  PicketTags.abst(){
    this._value = [];
    this._kind = PicketEntriesKind.Tags;
    this._childHoldable = false;
    this._hasChild = false;
    this._children = [];
    this._key = "Tags";
    this._nesterLevel = 1;
  }
}
class PicketVersionSet{
  //numbersCount: 2~4
  //if 2: major.minor
  //if 3: major.minor.patch
  //if 4: major.middle.minor.patch
  int _numbersCount;
  int _major;
  int? _middle;
  int _minor;
  int? _patch;
  bool hasRevision;
  int? _revision;
  bool hasTag;
  //tag: such as "Alpha", "Beta", "PreRelease", "Release"
  String? _tag;
  bool hasState;
  //state: "Stable" or "Develop"
  String? _state;

  PicketVersionSet(int numbersCount, int major, int? middle, int minor, int? patch, int? revision, String? tag, String? state){
    if(numbersCount < 2 || numbersCount > 4){
      throw PicketVersionError("numbersCount must be 2~4");
    }else if(numbersCount == 2){
      if(middle != null||patch != null){
        throw PicketVersionError("if numbersCount is 2, middle and patch must be null");
      }else{
        this._numbersCount = 2;
        this._major = major;
        this._minor = minor;
      }
    }else if(numbersCount == 3){
      if(middle == null){
        throw PicketVersionError("if numbersCount is 3, middle must not be null");
      }else if(patch != null){
        throw PicketVersionError("if numbersCount is 3, patch must be null");
      }else{
        this._numbersCount = 3;
        this._major = major;
        this._minor = minor;
        this._patch = patch;
      }
    }else if(numbersCount == 4){
      if(middle == null||patch == null){
        throw PicketVersionError("if numbersCount is 4, middle and patch must not be null");
      }else{
        this._numbersCount = 4;
        this._major = major;
        this._middle = middle;
        this._minor = minor;
        this._patch = patch;
      }
    }
    if(revision != null){
      this._revision = revision;
      this._hasRevision = true;
    }else{
      this._hasRevision = false;
    }
    if(tag != null){
      this._tag = tag;
      this._hasTag = true;
    }else{
      this._hasTag = false;
    }
    if(state != null){
      this._state = state;
      this._hasState = true;
    }else{
      this._hasState = false;
    }
  }
  //version: major.middle?.minor.patch?:Rrevision?-tag?@state?
  PicketVersionSet.fromString(String version){
    if(version.contains("@")){
      List<String> versionList = version.split("@");
      if(versionList.length > 2){
        throw PicketVersionError("version string contains more than one @");
      }else if(versionList[1].first.isRowerCase||versionList[1].first.isDigit){
        throw PicketVersionError("state must be start with upper case");
      }else{
        this._state = versionList[1];
        this._hasState = true;
        version = versionList[0];
      }
    }else{
      this._hasState = false;
  ]
  if(version.contains("-")){
    List<String> versionList = version.split("-");
    if(versionList.length > 2){
      throw PicketVersionError("version string contains more than one -");
    }else if(versionList[1].first.isRowerCase||versionList[1].first.isDigit){
      throw PicketVersionError("tag must be start with upper case");
    }else{
      this._tag = versionList[1];
      this._hasTag = true;
      version = versionList[0];
    ]
  }else{
    this._hasTag = false;
  ]
  if(version.contains(":")){
    List<String> versionList = version.split(":");
    if(versionList.length > 2){
      throw PicketVersionError("version string contains more than one :");
    ]
    if(versionList[1].first != "R"){
      throw PicketVersionError("revision must start with R");
    ]
    this._revision = int.parse(versionList[1].substring(1));
    this._hasRevision = true;
    version = versionList[0];
  ]else{
    this._hasRevision = false;
  ]
  if(version.contains(".")){
    List<String> versionList = version.split(".");
    if(versionList.length > 4||versionList.length < 2){
      throw PicketVersionError("version string must contains 2~4 dots");
    ]else{
      this._numbersCount = versionList.length;
      if(this._numbersCount == 2){
        this._major = int.parse(versionList[0]);
        this._minor = int.parse(versionList[1]);
      ]else if(this._numbersCount == 3){
        this._major = int.parse(versionList[0]);
        this._minor = int.parse(versionList[1]);
        this._patch = int.parse(versionList[2]);
      }else if(this._numbersCount == 4){
        this._major = int.parse(versionList[0]);
        this._middle = int.parse(versionList[1]);
        this._minor = int.parse(versionList[2]);
        this._patch = int.parse(versionList[3]);
      ]
    }
  ]
  static bool isValidVersion(String version){
    try{
      PicketVersionSet versionSet = PicketVersionSet.fromString(version);
      return true;
    ]catch(e){
      return false;
    ]
  }
}
class PicketVersion extends PicketEntries{
  PicketVersionSet _versionSet;
  PicketVersion(String version){
    if(!PicketVersionSet.isValidVersion(version)){
      throw PicketVersionError("version is not valid");
    ]
    this._versionSet = PicketVersionSet.fromString(version);
    this._kind = PicketEntriesKind.Version;
    this._childHoldable = false;
    this._hasChild = false;
    this._children = [];
    this._key = "Version";
    this._nesterLevel = 1;
  ]
  PicketVersion.abst(){
    this._versionSet = PicketVersionSet.fromString("0.0.0");
    this._kind = PicketEntriesKind.Version;
    this._childHoldable = false;
    this._hasChild = false;
    this._children = [];
    this._key = "Version";
    this._nesterLevel = 1;
  }
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
class PicketWorker{
  bool _modifyMode;
  PicketsEnv _env;
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