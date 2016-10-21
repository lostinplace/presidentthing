package wikiparser

import org.scalatest.{FlatSpec, Matchers}
import fastparse.all._
import fastparse.core.Parsed.{Failure, Success}

import scala.util.matching.Regex
import RegexParser._


object RegexParserTester {
  import fastparse.all._
  import RegexParser._

  val main = P("""[abc]+""".r).map {_.toString}
  val main2 = P("""[gef]+""".r ~ "test").map { _.toString}
  val main3 = "ab+".r ~ "test" map {_.toString}
}


class RegexParserTest extends FlatSpec with Matchers {

  "regex parser" should "basically work" in {

    val parser = R("[abc]*".r)
    val Success(result, _) = parser.parse("abcdefg")
    result.toString should be("abc")
    result.isInstanceOf[Regex.Match] should be(true)
  }

  it should "return whole string if applicable" in {

    val parser = R(".*".r)
    val Success(result, _) = parser.parse("abcdefg")
    result.toString should be("abcdefg")
    result.isInstanceOf[Regex.Match] should be(true)
  }

  it should "fail when wrong" in {
    val parser = P("[abc]+".r)
    val Failure(expected, failIndex, extra) = parser.parse("garbled")
    expected.toString should be("Regex(^[abc]+)")
    failIndex should be(0)
  }

  it should "chain appropriately" in {
    val parser = P("[abc]+".r)
    val Success(result, _) = parser.parse("abcdefg")
    result.toString should be("abc")
    result.isInstanceOf[Regex.Match] should be(true)
  }

  it should "fail on empty results" in {
    val parser = P("a*".r)
    val Failure(expected, failIndex, extra) = parser.parse("garbled")
    expected.toString should be("Regex(^a*)")
    failIndex should be(0)
  }

  "regexparsertester" should "show an implicit parser" in {
    import RegexParserTester._

    val Success(result, _) = main.parse("abc")
    result.isInstanceOf[String] should be(true)
    result should be("abc")
  }

  it should "show composition success" in {
    import RegexParserTester._

    val Success(result, _) = main2.parse("geftest")

    result.toString should be("gef")

  }

  it should "show composition failure" in {
    import RegexParserTester._

    val Failure(expected, failIndex, extra) = main2.parse("gefbtest")

    expected.toString should be(""""test"""")
    failIndex should be(3)
  }

  "regexparsertester smooth syntax" should "show composition success" in {
    import RegexParserTester._

    val Success(result, _) = main3.parse("abbbbbbtest")
    result.toString should be("abbbbbb")
  }
}
