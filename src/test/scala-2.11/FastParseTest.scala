package wikiparser

import fastparse.core.Parsed.{Failure, Success}
import WikiParser._
import fastparse.core.Parsed
import fastparse.parsers.Terminals.Literal

import scala.io.Source
import org.scalatest.{FlatSpec, Matchers}
import wikiparser.WikiAST._

import scala.runtime.BoxedUnit

class FastParseTest extends FlatSpec with Matchers {

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
    val failure = s"...$left^^>$offender<^^$right..."

    s"expected: $expected , received: $failure"
  }

  def gwResourceURL(resource:String="georgewashington") = getClass.getResource(s"/$resource.md")
  def gwSourceLines(lineCount:Int = 1, resource:String ="georgewashington") = {
    val gwSource = Source.fromURL(gwResourceURL(resource))
    try gwSource.getLines.take(lineCount).mkString("\n") finally gwSource.close
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


  it should "parse parentheticals" in {
    val fullText = "(Commanding General of the United States Army)"
    var Success(parens,_) = parenthetical.parse(fullText)

    val Success(sentenceResult, _) = sentence.parse("Commanding General of the United States Army")
    parens.content should be(sentenceResult)

  }

  it should "parse xml" in {
    val fullText = "<ref>{{harvnb|Lillback|Newcombe|2006|pp=1-1187}}</ref>"
    var Success(out, _) = Xml.Xml.Element.parse(fullText)
    out.isInstanceOf[Unit] should be(true)
  }


  it should "parse preamble for George washington" in {
    val counts = List(62)
    var memo:WikiPage = null

    for( count <- counts) {
      val lines = gwSourceLines(count) + "}}"

      val out = wikiPage.parse(lines)
      val Success(result, _) = out
      memo = result
      result should not be null
    }

    println(memo)

  }

  it should "parse preamble for jefferson" in {
    val counts = List(62)
    var memo:WikiPage = null

    for( count <- counts) {
      val lines = gwSourceLines(count,"jefferson") + "}}"

      val out = wikiPage.parse(lines)
      val Success(result, _) = out
      memo = result
      result should not be null
    }

    println(memo)

  }

}
