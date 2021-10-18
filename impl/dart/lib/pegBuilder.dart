import "package:fcpeg/pegNode.dart";
import "package:fcpeg/stdLib.dart";
class PEGWorker{}
class PEGLoader extends PEGWorker{
  List<Block<Commands>> _bls;
  PEGLoader()=>this._bls=List<Block<Commands>>[];
  //Str block_tag, Str kind, Str data/RuleDef rule
  static PEGLoader parsePEG(String imput) {
    PEGLoader peg = PEGLoader();
    imput.split("}\n").map((String value) {
      value.split("]{\n").indexedMap((int ind, String val) {
        Strig tag,lines;
        if (ind == 0){
          tag = val.subString(1);
        } else if (ind == 1) {
          lines=val;
        }
        peg.addBlock(tag, lines);
      });
    });
  }
  void addBlock(String tag,String lines)=>this.bls.add(Block(tag).update(lines));
}
