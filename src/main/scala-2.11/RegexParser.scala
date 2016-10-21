package wikiparser

import fastparse.all._
import fastparse.core.{Mutable}
import scala.util.matching.Regex

class RegexParser(pattern: Regex, leapDistance:Int = 10, maxLength:Int=100, maxTries: Int = Int.MaxValue) extends Parser[Regex.Match] {
  import fastparse.core.{ParseCtx}

  private val _startsWith = """^\^.*""".r
  private val _innerPattern = _ensurePatternEvaluatesFromStart(pattern)

  override def toString: String = s"Regex(${_innerPattern.toString})"
  private val zeroLengthMessage = s"$toString match with length > 0"

  private def _ensurePatternEvaluatesFromStart(aPattern: Regex) = {
    aPattern.toString match {
      case _startsWith(result) => aPattern
      case insufficient => ("^" + insufficient).r
    }
  }

  def withMaxChars(newMax: Int): RegexParser = RegexParser(_innerPattern, leapDistance, newMax)
  def withLeap(newLeap: Int): RegexParser = RegexParser(_innerPattern, newLeap, maxLength)
  def withTries(tryCount: Int): RegexParser = RegexParser(_innerPattern, leapDistance, maxLength, tryCount)

  override def parseRec(cfg: ParseCtx[Char, String], index: Int): Mutable[Regex.Match, Char, String] = {
    val inputLength = cfg.input.length
    val remainingLength = inputLength - index
    var failures = Set[fastparse.core.Parser[_, Char, String]]()
    var attempts = 0

    var accumulator:String = ""
    def accIndexAfter(leap:Int) = {
      index + accumulator.length + leap
    }
    do {
      attempts+=1
      accumulator = cfg.input.slice(index, accIndexAfter(leapDistance))
      val matchCandidate = _innerPattern.findFirstMatchIn(accumulator)
      val shouldExit = accumulator.length == remainingLength || attempts == maxTries

      _innerPattern.findFirstMatchIn(accumulator) match {
        case None =>
        case Some(x) if x.end == 0 => failures += this.opaque(zeroLengthMessage)
        case Some(result) => if(result.end<accumulator.length || shouldExit)
          return success(cfg.success, result, index+result.end, Set(), false)
      }
    } while( accumulator.length < maxLength && accumulator.length < remainingLength && attempts < maxTries )
    fail(cfg.failure, index, failures, false)
  }
}

object RegexParser {
  implicit def regexToRegexParser(pattern: Regex): RegexParser = R(pattern)
  def R(pattern:Regex, leapDistance:Int = 10, maxLength:Int=Int.MaxValue, tries: Int = Int.MaxValue) =
    RegexParser(pattern, leapDistance, maxLength, tries)

  def apply(pattern: Regex, leapDistance: Int = 10, maxLength: Int = 100, tries: Int = Int.MaxValue): RegexParser =
    new RegexParser(pattern, leapDistance, maxLength, tries)
}
