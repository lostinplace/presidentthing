/**
  * Created by cwwhee on 10/19/16.
  */
import org.scalatest.{FlatSpec, Matchers}
import fastparse.all._
import fastparse.core.Parsed.{Failure, Success}

import scala.util.matching.Regex
import RegexParser._

class RegexParserTest extends FlatSpec with Matchers {

  "regex parser" should "basically work" in {

    val parser = R("abc".r)
    val Success(result, _) = parser.parse("abcdefg")
    result.toString should be("abc")
    result.isInstanceOf[Regex.Match] should be(true)
  }

  it should "fail when wrong" in {
    val parser = P("abc".r)
    val Failure(expected, failIndex, extra) = parser.parse("garbled")
    expected.toString should be("Regex(^abc)")
    failIndex should be(0)
  }

  it should "chain appropriately" in {
    val parser = P("abc".r)
    val Success(result, _) = parser.parse("abcdefg")
    result.toString should be("abc")
    result.isInstanceOf[Regex.Match] should be(true)
  }

}
