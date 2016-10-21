package wikiparser

import scala.util.matching.Regex

object WikiParser {
  import fastparse.all._
  import WikiAST._
  import RegexParser._
  import WikiTokens._

  val normalExpression: P[Expression] = P(
    break | XML | boldContent | templateInvocation | parenthetical | link | sentenceFragment
  )

  val newline = P( "\n" | "\r\n" | "\r" | "\f")
  val whitespace = P( " " | "\t" | newline)
  val wsBlock = whitespace.rep

  val break = P("<br" ~/ wsBlock.? ~ (">" | "/>")) .map(_ => Break())

  val sentence = normalExpression.rep(sep=" ".rep.?) .map { Sentence(_:_*) }

  val valueSentence = normalExpression.rep(sep=wsBlock) .map { Sentence(_:_*) }

  val wikiPage = normalExpression.rep(min=1, sep=wsBlock).map { WikiPage( _:_*) }

  val boldContent = (BOLD_QUOTES ~ sentence ~ BOLD_QUOTES) map { Bold(_) }

  val word = R(WikiTokens.patternNotIncludingTokens).map(x=>Word(x.toString))

  val sentenceFragment = word.rep(min=1, sep=" ").map { SentenceFragment(_:_*) }
  val templateInvocation:P[TemplateInvocation] = P(TEMPLATE_START ~/ sentenceFragment ~/ argList.? ~/ TEMPLATE_END).map {
      case (phrase, args) => new TemplateInvocation(phrase, args)
   }

  val kvPair = word ~ wsBlock.? ~ "=" ~ wsBlock.? ~ valueSentence.? map {
    case (word, Some(sentence)) => KVPair(word, Some(sentence))
    case (word, None) => KVPair(word, None)
  }

  val link = P(LINK_START ~/ sentence ~/ (ARG_SEPARATOR ~/ sentence).? ~/ LINK_END ).map {
    case (phrase, label:Option[sentence]) => Link(phrase, label)
  }

  val arg = kvPair | sentence
  val args =  arg.rep(min=1, sep=wsBlock.? ~ ARG_SEPARATOR ~/ wsBlock.? ~ Pass)
  val argList = P(wsBlock.? ~ ARG_SEPARATOR ~/ wsBlock.? ~/ args ~ wsBlock.?)

  val parenthetical = P("(" ~/ wsBlock.? ~ sentence ~/ wsBlock.? ~ ")").map {
    case sentence => Parenthetical(sentence)
  }

  val XML = Xml.Xml.XmlContent.map(WikiXML(_))

  def TextNot(chars: String) = {
    val AllowedChars = P( CharsWhile(!(chars + "\n").contains(_)) )
    P( Index ~ AllowedChars.!.rep(1) ).map {
      case (i, x) => x.mkString
    }
  }
}
