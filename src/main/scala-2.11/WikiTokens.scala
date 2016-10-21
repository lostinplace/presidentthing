package wikiparser

object WikiTokens extends Enumeration{
  import fastparse.all._

  type WikiTokens = Value

  val TEMPLATE_START = Value("{{")
  val TEMPLATE_END = Value("}}")

  val LINK_START = Value("[[")
  val LINK_END = Value("]]")

  val ARG_SEPARATOR = Value("|")

  val PARENS_START = Value("(")
  val PARENS_END = Value(")")

  val EQUALS = Value("=")

  val SPACE = Value(" ")

  val BOLD_QUOTES = Value("'''")

  val REF_START = Value("<ref")

  val tokenPattern = CalculateTokenRegex().r
  val negativeTokenPattern = CalculateTokenRegex(true).r
  val patternNotIncludingTokens = s"$negativeTokenPattern+".r

  private def CalculateTokenRegex(negate:Boolean = false) ={

    val tokenGroups = WikiTokens.values.map { """(\Q%s\E)""".format(_) } mkString("|")
    if(negate) s"((?!$tokenGroups).)" else s"([$tokenGroups])"

  }

  implicit def valueToString(value: WikiTokens.Value):String = value.toString
  implicit def valueToParser(value: WikiTokens.Value) = P(value.toString)

}
