package wikiparser

import fastparse.core.Parsed.{Failure, Success}
import WikiParser._
import fastparse.core.Parsed

import scala.io.Source
import org.scalatest.{FlatSpec, Matchers}
import wikiparser.WikiAST._

class WikiParseTest extends FlatSpec with Matchers {

  "parser" should "correctly parse a word" in {
    val Success(aWord,_) = word.parse("testing")
    aWord should be(Word("testing"))
    val Success(aWord2,_) = word.parse("test{ing")
    aWord2 should be(Word("test{ing"))
    val Success(aWord3,_) = word.parse("test]ing")
    aWord3 should be(Word("test]ing"))
    val Success(aWord4,_) = word.parse("test]]ing")
    aWord4 should be(Word("test"))

  }

  it should "parse bold text" in {
    val text = "'''my bold text'''"
    val Success(result, _) = boldContent.parse(text)
    val Success(expectedSentence, _) = sentence.parse("my bold text")
    result should be(Bold(expectedSentence))
  }

  it should "parse very simple links" in {
    val Success(result:Link,_) = link.parse("[[test]]")
    result.resource should be(Sentence(SentenceFragment(Word("test"))))
  }

  it should "parse slightly more complex links" in {
    val Success(result:Link,_) = link.parse("[[test me]]")
    val wordList = List(Word("test"), Word("me") )
    val expectation = Sentence(SentenceFragment(wordList:_*))
    result.resource should be(expectation)
  }

  it should "parse complex links with args" in {
    val Success(result:Link,_) = link.parse("[[resource link|label text]]")
    val Success(resourceExpectation,_) =sentence.parse("resource link")
    val Success(labelExpectation,_) = sentence.parse("label text")

    result.resource should be(resourceExpectation)
    result.label.get should be(labelExpectation)
  }

  it should "parse break tags" in {
    val Success(break,_) = WikiParser.break.parse("<br>")

    break should be(Break())

    val Success(otherBreak,_) = WikiParser.break.parse("<br />")
    otherBreak should be (Break())
  }

  it should "parse refs as xml" in {
    val test = """<ref name="calendar" group=lower-alpha>Contemporary records</ref>"""
    val out = Xml.Xml.Element.parse(test)
    out match {
      case Success(result, _) => {

        result.toString should not be "null"
      }
      case x => WikiParseTest.getFailureString(x) should be("")
    }
  }

  it should "parse weird sentences correctly" in {
    val test = """hello<ref name="calendar" group=lower-alpha>Contemporary records</ref>you"""
    val out = sentence.parse(test)
    out match {
      case Success(result, _) => {

        result.toString should not be "null"
      }
      case x => WikiParseTest.getFailureString(x) should be("")
    }
  }

  it should "parse parentheticals" in {
    val fullText = "(Commanding General of the United States Army)"
    var Success(parens,_) = parenthetical.parse(fullText)

    val Success(sentenceResult, _) = sentence.parse("Commanding General of the United States Army")
    parens.content should be(sentenceResult)

  }

  it should "parse more difficult parentheticals" in {
    val test = """({{OldStyleDateDY|February 22,|1732|February 11, 1731}}<ref name="calendar" group=lower-alpha>Contemporary records</ref>)"""

    val out = parenthetical.parse(test)
    out match {
      case Success(result, _) => {

        result should not be null
      }
      case x => WikiParseTest.getFailureString(x) should be("")
    }

  }

  it should "parse xml" in {
    val fullText = "<ref>{{harvnb|Lillback|Newcombe|2006|pp=1-1187}}</ref>"
    var Success(out, _) = Xml.Xml.Element.parse(fullText)
    out.isInstanceOf[Unit] should be(true)
  }


  it should "parse preamble for George washington" in {
    val counts = List(65)
    var memo:WikiPage = null

    for( count <- counts) {
      val lines = WikiParseTest.gwSourceLines(count) + "}}"

      val out = wikiPage.parse(lines)
      out match {
        case Success(result, _) => {
          memo = result
          result should not be null
        }
        case x => WikiParseTest.getFailureString(x) should be("")
      }

    }

  }

  it should "parse preamble for jefferson" in {
    val counts = List(62)
    var memo:WikiPage = null

    for( count <- counts) {
      val lines = WikiParseTest.gwSourceLines(count, "jefferson") + "}}"

      val out = wikiPage.parse(lines)
      val Success(result, _) = out
      memo = result
      result should not be null
    }

    println(memo)

  }

}

object WikiParseTest {
  def getFailureString[T](result: Parsed[T, _, _]) = {
    val tmp = result.asInstanceOf[Parsed.Failure[WikiPage,_]]
    val full = tmp.extra.input
    val idx  = result.index
    val expected = tmp.lastParser.toString

    val end = if(full.length-1 < idx + 10) full.length -1 else idx + 10
    val begin = if(0 > idx - 10) 0 else idx -10
    val left = full.slice(begin, idx)
    val offender = full.slice(idx, idx+1)
    val right = full.slice(idx+1, end)
    val failure = s"...$left⭆$offender⭅$right..."

    s"expected: $expected , received: $failure"
  }

  def gwResourceURL(resource:String="georgewashington") = getClass.getResource(s"/$resource.md")
  def gwSourceLines(lineCount:Int = 1, resource:String ="georgewashington") = {
    val gwSource = Source.fromURL(gwResourceURL(resource))
    try gwSource.getLines.take(lineCount).mkString("\n") finally gwSource.close
  }
}
