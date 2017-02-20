package autocomplete.webapp

import main.scala.autocomplete.webapp.PrefixTrie
import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom._
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
    val pT = PrefixTrie()
    val i: Input = input.render
    i.setAttribute("id", "input")

    val u: UList = ul.render
    u.setAttribute("id", "suggestions")

    val container: Div = div.render

    document.body.appendChild(
      container.appendChild(
        div(
          h1("Autocomplete"),
          div(i),
          u
        ).render
      )
    )

    i.onkeyup = (e: dom.Event) => {
      val output = i.value
      val lastWord = getLastWord(output)

      if (output != "") {
        val chrs = lastWord.toList
        pT.addWord(chrs, lastWord)
        val LIs = getRenderedMatches(chrs)
        appendLIElements(LIs)
      } else {
        removeAllLiElements
      }
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

    def removeAllLiElements: Unit = {
      u.innerHTML = ""
    }

    def getRenderedMatches(lastWord: List[Char]): List[LI] = {
      val matches = pT.findMatches(lastWord)
      for {
        m <- matches
      } yield li(m).render
    }

    def getLastWord(content: String): String = getWords(content).last
    def getWords(content: String): List[String] = content.split(" ").toList

  }

}
