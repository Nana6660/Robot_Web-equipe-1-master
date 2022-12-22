package app
import library._ // import tout le package

class FiltrageURLs_class extends FiltrageURLs {

  override def filtreAnnonce(h: Html): List[String] = {

    // 1- extrait la liste des URLs incluses dans un documents HTML
    var listeURLS: List[String] = extraitURL(h)

    //2- retire de cette liste les URLs qui ne sont pas des URLs d'annonce du site de référence
    var listeUrlsSiteReferences: List[String] = selectionUrlSiteReference(listeURLS)

    //On retourne la liste finale des URLs du site
    listeUrlsSiteReferences
  }

  //
  def extraitURL(h: Html): List[String] = {
    var listURL: List[String] = List();
    h match {
      case Tag("a", attributes, children) => {
        listURL ++ gestionAttribute(attributes) ++ gestionChildren(children)
      }
      case Tag(_, _, children) => {
        listURL ++ gestionChildren(children)
      }
      case Texte(content) => { // pas de lien contenu dans le case class Texte
        listURL
      }
    }
  }

  // ici on a une liste du tuple clé valeur, lorque que la clé est "href", on ajoute son contenu sinon et on continu avec le rste de la liste
  def gestionAttribute(attributes: List[(String, String)]): List[String] = {
    attributes match {
      case Nil => Nil
      case (cle, valeur) :: suiteListe => {
        if (cle == "href") {
          var ajoutElement = valeur :: Nil
          ajoutElement ++ gestionAttribute(suiteListe)
        } else {
          gestionAttribute(suiteListe)
        }
      }
    }
  }

  // ici on traite la liste des children, les children etant de type html, on appelle extraitURL pour chaque element de la liste
  def gestionChildren(children: List[(Html)]): List[String] = {
    children match {
      case Nil => Nil
      case a :: suiteListe => {
        extraitURL(a) ++ gestionChildren(suiteListe)
      }
    }
  }

  // ici on ne garde que les elemement de la liste qui contiennent l'url du site de reference
  // 9 chiffres a la fin d'une annonce donc fonction verificationAnnonce
  def selectionUrlSiteReference(listURL: List[String]): List[String] = {
    val nomH: String = "https://www.vivastreet.com/"
    listURL match {
      case Nil => Nil
      case x1 :: suiteListe => {
        if ((x1.contains(nomH)) && (x1.size - 9 > 0) && (verificationAnnonce(x1.substring(x1.size - 9, x1.size)))) {
          x1 :: selectionUrlSiteReference(suiteListe)
        } else {
          selectionUrlSiteReference(suiteListe)
        }
      }
    }
  }

  def verificationAnnonce(substring: String): Boolean = {
    var b = true
    for (i <- 0 to substring.size - 1) {
      if (substring.charAt(i) < '0' || substring.charAt(i) > '9') {
        b = false
      }
    }
    b
  }

}