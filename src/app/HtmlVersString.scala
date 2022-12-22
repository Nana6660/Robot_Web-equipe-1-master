package app
import library._ // import tout le package

object HtmlVersString extends HtmlVersString {
  def traduire(h: Html): String = {
    h match {
      case Texte(x) => x // Return un texte

      /*
         * Return un code html
         */
      case Tag(x, y, z) => {
        var s: String = "" //Cette variable Permettra de stocker les attributs et leurs valeurs
        var q: String = "" //Cette variable Permettra de stocker les balises et leurs contenus
        if (y != null) {
          for ((i, j) <- y) {
            s += ' ' + i + '=' + '"' + j + '"'
          }
        }
        for ((i) <- z) {
          q += traduire(i)
        }
        if (q == "") {
          '<' + x + s + '/' + '>'
        } else {
          '<' + x + s + '>' + q + '<' + '/' + x + '>'
        }
      }
    }
  }
}