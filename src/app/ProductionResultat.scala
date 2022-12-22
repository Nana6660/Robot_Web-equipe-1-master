package app
import library._ // import tout le package

/**
   * A partir d'une liste de couples (titre,URL), produit un document Html, qui
   * liste les solutions sous la forme de liens cliquables
   *
   * @param l la liste des couples solution (titre,URL)
   * @return le document Html listant les solutions
   */
object ProductionResultat extends ProductionResultat{
  
  def resultatVersHtml(l:List[(String,String)]):Html = {
   
    var html:List[Html]=List()
    
    for((x,y)<-l){
    html=html:+ Tag("ol",List(),List(Tag("br",List(),List(Tag("a",List(("href",y)),List(Texte(x)))))))
   }
   
   if(html==List()){
     html=html:+Tag("p align='center'", List(),List(
          Tag("img src='images/noresult.png' alt='No results found !'width='650' height='400'",List(),List()))),
        
   }
    Tag("html", List(),
    List(
      Tag("head", List(),
        List(
          Tag("meta", List(("charset", "utf-8")), List()),
          Tag("title", List(), List(Texte("Resultats"))))),
      Tag("body style='background-color: rgb(189, 228, 208) '", List(), List(
           Tag("p align='center'", List(),List(
          Tag("img src='images/logo.png' alt='vivastreet logo'width='300' height='141'",List(),List()))),
        Tag("left", List(),List(
             Tag("fieldset style='background-color: rgb(221, 254, 237 );border:10px solid rgb(93, 100, 96 )'", List(),List(
                 Tag("legend style='color:rgb(67, 64, 63);font-size:50px;font-weight:bold;font-variant:small-caps'",List(),List(Texte("Resultats de votre recherche : "))),
            Tag("href", List(),html))))),
        Tag("footer", List(), List(
             Tag("p style='float:right'", List(),List(
          Tag("img src='images/rennes.png' alt='rennes1'width='200' height='141'",List(),List()))),
            Tag("br", List(), List(
          Tag("p", List(), List(Texte("Cette page vous est proposée par le groupe de projet GEN n°1 de l'université de Rennes 1 : ") )),
            Tag("p",List(),List(Texte("Mickaël, Raphaël, Nicolas, Asma, Estelle, Yaya, Manu, Nana, Erwan"))   ),
           
       )))
    ))))
  )}
}

/**
 * trait ProductionResultatT{
  
   def resultat2html(l:List[(String,String)]):Html
}
  object ProductionResultat extends ProductionResultatT{
    
    def ListUrl(liens:List[(String,String)]):List[Html]={
     liens match{
       case Nil => Nil
       case (titre, url)::rec => Tag("li", List(), List(Tag("a", List(("href", url)), List(Texte(titre))))) :: ListUrl(rec)

     }
   }
    
    
   
    def resultat2html(l:List[(String,String)]):Html={

        val site= Tag("html",List(),
                   List(Tag("head",List(),
                            List(Tag("meta",List(("content","text/html"),("charset","utf-8")),List()),
                                    Tag("title",List(),List(Texte("RECHERCHE"))))
                                ),
                        Tag("body",List(), List(
                            Tag("ol", List(),  ListUrl(l))
                         )
                      )
                   ))

              site
       }
   
    }
 */

