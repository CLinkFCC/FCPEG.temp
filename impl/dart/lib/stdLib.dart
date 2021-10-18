extension IndexedMap<T, E> on List<T> {
 List<E> indexedMap<E>(E Function(int index, T item) function) {
   final list = <E>[];
   asMap().forEach((index, element) {
     list.add(function(index, element));
   });
   return list;
 }
}
