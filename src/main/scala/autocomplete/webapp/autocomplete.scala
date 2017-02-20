package autocomplete.webapp

import main.scala.autocomplete.webapp.PrefixTrie
import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom._
import window.{ clearTimeout, setTimeout }
import org.scalajs.dom.html._
import scalatags.JsDom.all._

object Autocomplete extends JSApp {
  def main(): Unit = {
    import DOM.InputBox

    new InputBox()

  }
}

object DOM {

  class InputBox {
    var wordUseHist = Map[String, Int]().withDefaultValue(0)
    val pT = PrefixTrie()
    val i: Input = input.render
    val u: UList = ul.render
    val container: Div = div.render
    val debouncer: () => Unit = getDebouncer(onKeyUp)

    i.setAttribute("id", "input")
    u.setAttribute("id", "suggestions")


    document.body.appendChild(
      container.appendChild(
        div(
          h1("Trie Autocomplete"),
          div(i),
          u
        ).render
      )
    )

    i.onkeyup = (e: dom.Event) => {
      debouncer()
    }

    def onKeyUp(): Unit = {
      val output = i.value
      val lastWord = getLastWord(output)
      addWordToHist(lastWord)

      if (output != "") {
        val chrs = lastWord.toList
        pT.addWord(chrs, lastWord)
        val LIs = getRenderedMatches(chrs)
        appendLIElements(LIs)
      } else {
        removeAllLiElements
      }
    }

    def getDebouncer(F: () => Unit): () => Unit = {
      var timeout = 0

      def debounce(): Unit = {
        clearTimeout(timeout)
        timeout = setTimeout(() => F(), 200)
      }
      debounce
    }

    def appendLIElements(liElements: List[LI]): UList = {
      removeAllLiElements
      for {
        liElt <- liElements
      } {
        u.appendChild(liElt)
      }

      u
    }

    def getPrioritizedMatches(matches: List[String]): List[String] = matches.sortWith(wordUseHist(_) > wordUseHist(_))

    def addWordToHist(word: String): Unit = wordUseHist += (word -> (wordUseHist(word) + 1))

    def removeAllLiElements: Unit = u.innerHTML = ""

    def getRenderedMatches(lastWord: List[Char]): List[LI] = {
      val matches = pT.findMatches(lastWord)
      for {
        m <- getPrioritizedMatches(matches)
      } yield li(m).render
    }

    def getLastWord(content: String): String = getWords(content).last

    def getWords(content: String): List[String] = content.split(" ").toList

  }

}
