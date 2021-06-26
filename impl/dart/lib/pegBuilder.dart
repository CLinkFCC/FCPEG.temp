import "pegNode.dart";

class FCPEG{
  List<Block> bls;
  FCPEG()=>List();
  //Str block_tag, Str kind, Str data/RuleDef rule
  static FCPEG parsePEG(String imput) {
    FCPEG peg = FCPEG();
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

extension IndexedMap<T, E> on List<T> {
 List<E> indexedMap<E>(E Function(int index, T item) function) {
   final list = <E>[];
   asMap().forEach((index, element) {
     list.add(function(index, element));
   });
   return list;
 }
}