package library

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.io.StdIn._

/** La case classe des expression */
sealed trait Expression
case class Mot(w: String) extends Expression
case class Et(e1: Expression, e2: Expression) extends Expression
case class Ou(e1: Expression, e2: Expression) extends Expression

/** Le parseur d'expressions */
object ParserExpression {

  /**
   * La méthode principale du parseur: lit (au clavier) une chaîne de caractères de la forme "((toto and titi) and (tata or tutu))" et produit une
   * expression. La méthode itère le processus jusqu'à ce qu'une chaîne de caractère pouvant être transformée en une expression
   * est tapée.
   * @return l'expression résultat du parsing.
   */
  def lireExpression: Expression = {
    var rep = ""
    var query: Expression = Mot("")
    while (rep == "") {
      println("Donnez votre requète sous forme de mots clés et combinés avec and/or\nPar exemple: developpeur and (rennes or nantes) and (python or java)")
      rep = readLine()
      val p = LocalParser.parse(rep)
      if (p.successful) query = p.get
      else { println("Malformed query!"); rep = "" }
    }
    query
  }

  /** L'objet local qui implémente le parseur */
  object LocalParser extends StdTokenParsers {
    type Tokens = StdLexical

    val lexical = new StdLexical
    lexical.reserved += ("and", "or")
    lexical.delimiters ++= List("(", ")")

    /**
     * Lit au clavier une chaîne de caractères de la forme "((toto and titi) and (tata or tutu))" et produit une liste
     *  des identifiants recontrés dans la chaîne.
     * @param s la chaîne de caractères à analyser
     * @return un objet de type parseResult qui contient l'expression résultat du parsing, si le parsing a réussi.
     */

    def parse(s: String): ParseResult[Expression] = {
      expr(new lexical.Scanner(s))
    }

    // the parser itself
    def expr: Parser[Expression] = andExp | orExp | unExp
    def factor: Parser[Expression] = "(" ~> expr <~ ")"

    def andExp: Parser[Expression] = (unExp ~ "and" ~ expr ^^ { case x ~ _ ~ y => Et(x, y) })

    def orExp: Parser[Expression] = (unExp ~ "or" ~ expr ^^ { case x ~ _ ~ y => Ou(x, y) })

    def unExp: Parser[Expression] = word | factor

    def word: Parser[Expression] = (
      ident ^^ { s => Mot(s) }
      | numericLit ^^ { i => Mot(i.toString) })
  }

}