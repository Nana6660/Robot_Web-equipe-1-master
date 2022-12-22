package app
import java.io.FileWriter
import library._

object MainApp extends App {

  val url: String = "https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords="

  var arret: Boolean = true;

  while (arret) {
    //Etape 1 : Lire la requete
    val exp: Expression = ParserExpression.lireExpression
    //Etape 2 : Recuperation de la liste de couple(Titre/url) qui satisfait la requete
    val ListeUrl: List[(String, String)] = AnalysePage.resultats(url, exp)
    //Etape 3 : Transforme la liste de couple(Titre/url) en page html
    val pagehtml: Html = ProductionResultat.resultatVersHtml(ListeUrl)
    //Etape 4 : Transforme en string la page html
    val s: String = HtmlVersString.traduire(pagehtml)
    //cree le fichier hmtl et l'enregistre a la racine du projet
    val file = new FileWriter("Resultats_Recherche.html")
    try {
      file.write(s)
    } finally file.close()

    //permet d'effectuer une nouvelle requete qui ecrase l'ancienne si "Y"
    //"n" arrete le programme
    println("Nouvelle requete ? [Y/n]")
    scala.io.StdIn.readLine() match {
      case "Y" => arret = true
      case "n" => arret = false
      case _   => arret = true
    }
  }

}