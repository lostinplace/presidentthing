package wikiparser

import scala.util.matching.Regex

object WikiParser {
  import fastparse.all._
  import WikiAST._

  val normalExpression: P[Expression] = P(
    templateInvocation | parenthetical | break | XML | link | sentenceFragment
  )

  val newline = P( "\n" | "\r\n" | "\r" | "\f")
  val whitespace = P( " " | "\t" | newline)
  val wsBlock = whitespace.rep

  val break = P("<br" ~/ wsBlock.? ~ (">" | "/>")) .map(_ => Break())

  val sentence = normalExpression.rep(sep=" "|"") .map { Sentence(_:_*) }

  val valueSentence = normalExpression.rep(sep=wsBlock) .map { Sentence(_:_*) }

  val wikiPage = normalExpression.rep(min=1, sep="\n").map { WikiPage( _:_*) }



  val word = TextNot("|{}[]()<>= ").map { Word(_) }

  val sentenceFragment = word.rep(min=1, sep=" ").map { SentenceFragment(_:_*) }
  val templateInvocation:P[TemplateInvocation] = P("{{" ~/ sentenceFragment ~/ argList.? ~/ "}}").map {
      case (phrase, args) => new TemplateInvocation(phrase, args)
   }


  val kvPair = word ~ wsBlock.? ~ "=" ~ wsBlock.? ~ valueSentence.? map {
    case (word, Some(sentence)) => KVPair(word, Some(sentence))
    case (word, None) => KVPair(word, None)
  }

  val link = P("[[" ~/ sentence ~/ ("|" ~/ sentence).? ~/ "]]").map {
    case (phrase, label:Option[sentence]) => Link(phrase, label)
  }

  val arg = kvPair | sentence
  val args =  arg.rep(min=1, sep=wsBlock.? ~ "|" ~/ wsBlock.? ~ Pass)
  val argList = P(wsBlock.? ~ "|" ~/ wsBlock.? ~/ args ~ wsBlock.?)

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
