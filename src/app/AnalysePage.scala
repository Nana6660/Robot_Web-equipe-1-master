package app
import library._ // import tout le package

object AnalysePage extends AnalysePage {
  val objFiltrageUrls: FiltrageURLs = new FiltrageURLs_class
  val objFiltrageHtml: FiltrageHtml = new FiltrageHtml_class

  def resultats(url: String, exp: Expression): List[(String, String)] = {
    var ListRetour: List[(String, String)] = List()
    var List_Page: List[Html] = List() // Listes des pages possible de recherche
    var Set_url: Set[String] = Set() // compilation des urls de toutes les pages possibles de recherche

    //L'expression contient des Ou, de fait il faut traiter chaque expression de maniere differente
    // cad : peugeot and (308 or 307)
    // donne une liste :
    // peugeot+307
    // peugeot+308
    val List_String_Expression = List_String_Expr(exp)

    //recupere chaque page de recherche correspondante a chaque expressions de la liste
    for (e <- List_String_Expression) {
      try {
        List_Page = List_Page :+ OutilsWebObjet.obtenirHtml(url + e)
      } catch {
        case _: Throwable => // A defaut de traiter chaque exceptions; on ignore ce cas et l'on passe au suivant
        //      case TagNodeConversion xception("Malformed URL") =>
        //      case TagNodeConversionException("Unknown Service") =>
        //      case TagNodeConversionException("Connect Exception") =>
        //      case TagNodeConversionException("IO Exception")
        //      case TagNodeConversionException("Bug HtmlCleaner!")
      }
    }

    //conserve les urls de chaque page de la liste de pages recherche (Ces urls correspondent aux annonces)
    for (h <- List_Page) {
      for (e <- objFiltrageUrls.filtreAnnonce(h)) {
        Set_url = Set_url + (e)
      }
    }

    //filtre les annonces non pertinente et recupere le titre des annonces pertinente
    //enregistre aussi les annonce dans la liste a retourner
    for (e <- Set_url) {
      val h1 = OutilsWebObjet.obtenirHtml(e)
      if (objFiltrageHtml.filtreHtml(h1, exp)) {
        ListRetour = ListRetour :+ (recup_Titre(h1), e)
      }
    }
    ListRetour
  }

  /**
   * Ceci renvoi un produit cartesien de la liste d'expression si elle contient un ou plusieurs "Ou"
   * Exemple d'une expression type (a ou b) et (c ou d)
   * Les String retournees seraient :
   * a+c
   * a+d
   * b+c
   * b+d
   */
  def List_String_Expr(exp: Expression): List[String] = {
    var ListRetour: List[String] = List()
    exp match {
      case Mot(w) => w :: Nil
      case Et(e1, e2) => {
        val ListRetoure1 = List_String_Expr(e1)
        val ListRetoure2 = List_String_Expr(e2)
        for (ebis1 <- ListRetoure1) {
          for (ebis2 <- ListRetoure2) {
            ListRetour = ListRetour :+ ebis1 + "+" + ebis2
          }
        }
        ListRetour
      }
      case Ou(e1, e2) => {
        ListRetour = ListRetour ++ List_String_Expr(e1)
        ListRetour = ListRetour ++ List_String_Expr(e2)
        ListRetour
      }
    }
  }

  /**
   * Ne pas entrer autre chose qu'une page contenant une balise titre
   * On recupere le titre du page html si elle contient une balise <title>
   */

  def recup_Titre(h: Html): String = {
    h match {
      //traite le cas ou la balise est le titre
      case Tag("title", _, children) => {
        var titre = ""
        if (titre.isEmpty()) {
          for (e <- children) {
            e match {
              case Texte(x) => titre = x
              case _        => titre
            }
          }
        }
        titre
      }
      case Tag(_, _, children) => {
        var titre = ""
        if (titre.isEmpty()) {
          for (e <- children) {
            e match {
              case Tag(_, _, _) => titre += recup_Titre(e)
              case _            => titre
            }
          }
        }
        titre
      }
      case Texte(x) => ""
    }
  }
}