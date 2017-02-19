//every level is a sorted list with single character prefixes
object Main extends App {
 val word = "abc"
 val T = PrefixTrie(None, word.toList)
 println(T)
}


case class PrefixTrie(chr: Option[Char], word: List[Char], var children: List[PrefixTrie] = List[PrefixTrie]()) {
 children = addWord(word)

 private[this] def addChild(word: List[Char]): List[PrefixTrie] = word match {
  case head::tail =>
   val newNode = PrefixTrie(Option(head), tail)
   newNode :: children
 }

 def addWord(word: List[Char]): List[PrefixTrie] = word match {
  case Nil => List[PrefixTrie]()
  case head::tail =>
   for {
    child <- children
   } {
    if (child.chr.getOrElse("") == head) {
     return child.addWord(tail)
    }
   }

   //not in children
   addChild(word)
 }

}