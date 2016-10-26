package wikiparser

import org.scalatest.{FlatSpec, Matchers}
import StringNotIn._
import fastparse.core.Parsed.{Failure, Success}

class StringNotInTest extends FlatSpec with Matchers {
  "parser" should "basically work" in {
    val p = SNI("test", "team","art")

    val Success(result,_) = p.parse("blerbtest")
    result should be("blerb")
  }

  "parser" should "fail if the string is in the list" in {
    val p = SNI("test", "team","art")

    val Failure(parser, index, extra) = p.parse("testblerb")
    parser should be(p)
    index should be(0)
  }

  "parser" should "succeed at end of input" in {
    val p = SNI("brat")

    val Success(result,_) = p.parse("blerbtest")
    result should be("blerbtest")
  }

  "parser" should "compose from tokens" in {
    val p = SNI(WikiTokens.values.map({_.toString}).toList:_*)

    val test = """bejshfdsfklj{{""".stripMargin
    val Success(result,_) = p.parse(test)
    result should be("bejshfdsfklj")

  }


}
