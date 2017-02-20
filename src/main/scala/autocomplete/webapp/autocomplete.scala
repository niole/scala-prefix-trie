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
    val s: Span = span.render
    val container: Div = div.render

    document.body.appendChild(
      container.appendChild(
        div(
          h1("Autocomplete"),
          div(i),
          s
        ).render
      )
    )

    i.onkeyup = (e: dom.Event) => {
      val output = i.value
      val chrs = output.toList
      pT.addWord(chrs, output)
      if (output != "") {
        s.textContent = pT.findMatches(chrs).mkString(", ")
      } else {
       s.textContent = ""
      }
    }
  }

}
