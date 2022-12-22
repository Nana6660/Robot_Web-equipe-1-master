package library

/**
 * Les documents HTML, vus comme des objets de type Html sont structurés de la façon suivante:
 *  Un noeud HTML de la forme: <NomTag att1="val1" att2="val2"> n1 n2 ... </NomTag>, sera représenté par un objet
 *  Tag("NomTag",List(("att1","val1"),("att2","val2")),List(n1,n2,...))
 *
 *  Un élément de texte simple sera représenté par un objet Text(s) où s est une chaîne de caractères
 */

sealed trait Html
case class Tag(name: String, attributes: List[(String, String)], children: List[Html]) extends Html
case class Texte(content: String) extends Html

/**
 * Un exemple de document html qui correspond au code HTML suivant:
 *  <html>
 *    <head>
 *      <meta chaset="utf-8" />
 *      <title>My Page</title>
 *    </head>
 *    <body>
 *      <center>
 *        <a href="http://www.irisa.fr">Lien</a>
 *      </center>
 *    </body>
 *  </html>
 */

object HtmlExample {
  val exemple = Tag("html", List(),
    List(
      Tag("head", List(),
        List(
          Tag("meta", List(("charset", "utf-8")), List()),
          Tag("title", List(), List(Texte("My Page"))))),
      Tag("body", List(), List(
        Tag("center", List(), List(
          Tag("a", List(("href", "http://www.irisa.fr")),
            List(Texte("Lien")))))))))
}
