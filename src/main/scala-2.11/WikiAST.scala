package wikiparser

import fastparse.core.Parsed.Success

object WikiAST {

  sealed trait Expression extends Any

  case class Chunk(content:Expression*) extends Expression {
    override def toString = s"Chunk(${content.mkString(" ")})"
  }

  case class Word(value: String) extends AnyVal with Expression {
    override def toString: String = value
  }

  case class SentenceFragment(content: Word*) extends Expression {
    override def toString = content.mkString(" ")
  }

  case class Link(content: Expression*) extends Expression {

    def label() ={
      if(content.length>1) content(1) else content(0)
    }

    override def toString = s"Link(${content.mkString(",")})"
  }

  case class Heading(content: Chunk, level: Int) extends Expression {

    override def toString: String = s"Heading$level($content)"
  }


  case class TemplateInvocation(content: Expression*) extends Expression {
    override def toString = s"TemplateInvocation(${content.mkString(",")})"
  }

  case class KVPair(key: Word, value: Option[Seq[Chunk]]) extends Expression{
    override def toString = {
      val content = value.getOrElse(List())
      s"KVPair($key=${content.mkString(",")})"
    }
  }
  case class Parenthetical(content: Expression) extends Expression
  case class Bold(content: Expression) extends Expression
  case class Italic(content: Expression) extends Expression
  case class BoldItalic(content: Expression) extends Expression
  case class ListBullet(content: Expression) extends Expression
  case class WikiList(content: ListBullet*) extends Expression {
    override def toString: String = s"WikiList(${content.mkString(",")})"
  }

  case class WikiPage(content: Expression*) {

    override def toString = s"WikiPage(${content.mkString("\n")})"
  }

  case class WikiXML(content: Any) extends Expression {
  }

  case class Break() extends Expression {
  }
}