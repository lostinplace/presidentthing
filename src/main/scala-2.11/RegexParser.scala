import fastparse.all._
import fastparse.core.{Mutable}
import scala.util.matching.Regex

class RegexParser(pattern: Regex, leapDistance:Int = 10, maxLength:Int=100) extends Parser[Regex.Match] {
  import fastparse.core.{ParseCtx}

  private val _startsWith = """^\^.*""".r
  private val _innerPattern = _ensurePatternEvaluatesFromStart(pattern)

  override def toString: String = s"Regex(${_innerPattern.toString})"

  private def _ensurePatternEvaluatesFromStart(aPattern: Regex) = {
    pattern.toString match {
      case _startsWith(result) => pattern
      case insufficient => ("^" + insufficient).r
    }
  }

  def maxChars(newMax: Int): RegexParser = RegexParser(_innerPattern, leapDistance, newMax)
  def leap(newLeap: Int): RegexParser = RegexParser(_innerPattern, newLeap, maxLength)

  override def parseRec(cfg: ParseCtx[Char, String], index: Int): Mutable[Regex.Match, Char, String] = {
    val inputLength = cfg.input.length
    val remainingLength = inputLength - index
    
    var accumulator:String = ""
    def accIndexAfter(leap:Int) = {
      index + accumulator.length + leap
    }
    do {
      accumulator = cfg.input.slice(index, accIndexAfter(leapDistance))
      val matchCandidate = _innerPattern.findFirstMatchIn(accumulator)

      _innerPattern.findFirstMatchIn(accumulator) match {
        case Some(result) => {
          return success(cfg.success, result, index+accumulator.length, Set(), false)
        }
        case None =>
      }
    } while( accumulator.length < maxLength && accumulator.length < remainingLength )
    fail(cfg.failure, index, Set(), false)
  }
}

object RegexParser {
  implicit def regexToRegexParser(pattern: Regex): RegexParser = R(pattern)
  def R(pattern:Regex, leapDistance:Int = 10, maxLength:Int=100) = RegexParser(pattern, leapDistance, maxLength)

  def apply(pattern: Regex, leapDistance: Int = 10, maxLength: Int = 100): RegexParser =
    new RegexParser(pattern, leapDistance, maxLength)
}
