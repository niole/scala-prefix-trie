package main.scala.autocomplete.webapp


case class PrefixTrie(chr: Option[Char] = None, letters: List[Char] = List[Char](), private val word: String = "", var children: List[PrefixTrie] = List[PrefixTrie]()) {
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
   updatedFound ++ getAllPossibilities
  case head::tail =>
     //keep going along matching trail
     //if stop matching, return found
     val matchingChild = getMatchingChild(head)
     matchingChild.map(_.findMatches(tail, found)).getOrElse(found)
 }

 def getAllPossibilities: List[String] = children.flatMap(_.findMatches())

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