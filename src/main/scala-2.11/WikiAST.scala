package wikiparser

import fastparse.core.Parsed.Success

object WikiAST {

  sealed trait Expression extends Any {
    def printableContent: Any
  }

  trait NonPrinting extends Expression {
    override def printableContent = ""
  }

  trait WikiParserTerm

  case class Sentence(expressions:Expression*) extends Expression with WikiParserTerm {
    override def printableContent: Any = expressions.foldLeft("") {
      case (x,y) => x+y.printableContent
    }
  }

  case class Word(value: String) extends AnyVal with Expression {
    override def printableContent: Any = value + " "
  }

  case class SentenceFragment(value: Word*) extends Expression {
    override def printableContent: Any = value.foldLeft("") { (a,b)=> a+b.printableContent }
    override def toString = s"Phrase(${printableContent.toString})"
  }

  case class Link(resource: Sentence, label: Option[Sentence]) extends Expression {
    override def printableContent: Any = { if(label.isDefined) label else resource }
  }

  case class TemplateInvocation(label: SentenceFragment, values: Option[Seq[Expression]]) extends Expression with NonPrinting
  case class KVPair(key: Word, value: Option[Sentence]) extends Expression with NonPrinting with WikiParserTerm
  case class Parenthetical(content: Sentence) extends Expression with NonPrinting
  case class Bold(content: Expression) extends Expression with NonPrinting
  case class Italic(content: Expression) extends Expression with NonPrinting
  case class BoldItalic(content: Expression) extends Expression with NonPrinting

  case class WikiPage(content: Expression*) {
    def printableContent: Any = content.foldLeft("") { (a,b)=> s"$a ${b.printableContent}" }
  }



  case class WikiXML(content: Any) extends Expression {
    override def printableContent: Any = "XML"
  }



  case class Break() extends Expression {
    override def printableContent: Any = "\n"
  }
}