//every level is a sorted list with single character prefixes
object Main extends App {
 val word = "abc"
 val w2 = "abc"
 val T = PrefixTrie(None, word.toList, word)
 T.addWord(w2.toList, w2)
 val matches = T.findMatches(word.toList, List[String]())
 val ms = T.findMatches(w2.toList, List[String]())
}


case class PrefixTrie(chr: Option[Char], letters: List[Char], private val word: String, var children: List[PrefixTrie] = List[PrefixTrie]()) {
 children = addWord(letters, word)
 var fullWord = ""
 setWord(word)

 def getWord: String = fullWord
 def setWord(newWord: String): Unit = {
  if (letters.isEmpty) {
   fullWord = newWord
  }
 }

 private[this] def addChild(letters: List[Char], word: String): List[PrefixTrie] = letters match {
  case head::tail =>
   val newNode = PrefixTrie(Option(head), tail, word)
   children = newNode :: children
   children
 }

 def accumulateWord(found: List[String]): List[String] = {
  val foundWord = getWord
  var foundWords = found
  if (foundWord != "") {
   foundWords = foundWord::found
  }
  foundWords
 }

 def findMatches(letters: List[Char] = List[Char](), found: List[String] = List[String]()): List[String] = letters match {
  case Nil =>
   //get all, if anymore exist
   val updatedFound = accumulateWord(found)
   getAllPossibilities(updatedFound)
  case head::tail =>
     //keep going along matching trail
     //if stop matching, return found
     val updatedFound = accumulateWord(found)
     val matchingChild = getMatchingChild(head)
     matchingChild.map(child => child.findMatches(tail, updatedFound)).getOrElse(updatedFound)
 }

 def getAllPossibilities(found: List[String]): List[String] = {
  if (children.nonEmpty) {
   children.flatMap(child => child.findMatches(List[Char](), found))
  }
  found
 }

 def getMatchingChild(chr: Char): Option[PrefixTrie] = {
  for {
   child <- children
  } {
   if (child.chr.getOrElse("") == chr) {
    return Some(child)
   }
  }
  None
 }

 def addWord(letters: List[Char], word: String): List[PrefixTrie] = letters match {
  case Nil =>
   setWord(word)
   List[PrefixTrie]()
  case head::tail =>
   for {
    child <- children
   } {
    if (child.chr.getOrElse("") == head) {
     return child.addWord(tail, word)
    }
   }

   //not in children
   addChild(letters, word)
 }

}