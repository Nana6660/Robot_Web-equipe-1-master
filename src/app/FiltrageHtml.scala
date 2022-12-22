package app
import library._ // import tout le package

class FiltrageHtml_class extends FiltrageHtml {

  /**
   * Renvoi un boolean false si la page ne contient pas l'expression
   * Renvoi true si elle contient l'expression
   */
  override def filtreHtml(h: Html, e: Expression): Boolean = {

    e match {
      case Mot(w) => {
        //En traduisant la page en String, on verifie si elle contient le texte contenu dans le mot
        val s: String = HtmlVersString.traduire(h)
        s.contains(w)
      }
      //Traite les expression comme on les traiterait en algebre binaire
      case Et(e1, e2) => { filtreHtml(h, e1) && filtreHtml(h, e2) }
      case Ou(e1, e2) => { filtreHtml(h, e1) || filtreHtml(h, e2) }
    }
  }

}