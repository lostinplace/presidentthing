package wikiparser

import fastparse.core.Parsed.{Failure, Success}
import WikiParser._
import fastparse.core.Parsed
import fastparse.parsers.Combinators.Not

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

  it should "parse a very simple template invocation" in {
    val test = "{{testing}}"
    val Success(result,_) = templateInvocation.parse(test)
    result.toString should not be("bleh")
  }

  it should "parse a more complex template invocation" in {
    val test = "{{testing|test=[[bleh]]|stuff stuff}}"
    val Success(result,_) = templateInvocation.parse(test)
    println(result)
    result.toString should not be("bleh")
  }

  it should "parse an even more complex template invocation" in {
    val test =
      """{{stuff}}
        |==bleh==""".stripMargin
    val Success(result,_) = wikiPage.parse(test)
    println(result)
    result.toString should not be("bleh")
  }

  it should "parse bold text" in {
    val text = "'''my bold text'''"
    val Success(result, _) = boldContent.parse(text)
    val Success(expectedSentence, _) = chunk.parse("my bold text")
    result should be(Bold(expectedSentence))
  }

  it should "parse bold italic text" in {
    val text = "'''''my bold text'''''"
    val Success(result, _) = boldContent.parse(text)
    val Success(expectedSentence, _) = chunk.parse("my bold text")
    result should be(Bold(Italic(expectedSentence)))
  }

  it should "parse bold embedded" in {
    val test = """'''George Washington''' ({{OldStyleDateDY|February 22,|1732|February 11, 1731}})"""
    val Success(result, _) = chunk.parse(test)
    val Success(expectedSentence, _) = chunk.parse("George Washington")
    println(result)
    result.toString.contains(Bold(expectedSentence).toString) should be(true)
  }

  it should "parse very simple links" in {
    val Success(expectation,_) = linkArgs.parse("test")
    val Success(result:Link,_) = link.parse("[[test]]")

    result.content should be(expectation)
    result.label.toString should be("Chunk(test)")
  }

  it should "parse slightly more complex links" in {
    val Success(result:Link,_) = link.parse("[[test me]]")

    val Success(expectation,_) = args.parse("test me")
    result.content should be(expectation)
    result.label.toString should be("Chunk(test me)")
  }

  it should "parse chunks from indent blocks" in {
    val test =
      """this is a phrase
        |:followed by an indent block
        |# followed by a list
        |
        |""".stripMargin
    val Success(result,_) = wikiPage.parse(test)
    println(result)

  }

  it should "parse complex links with args" in {
    val Success(result:Link,_) = link.parse("[[resource link|label text]]")
    val Success(resourceExpectation,_) =args.parse("resource link|label text")

    result.content should be(resourceExpectation)
    result.label.toString should be("Chunk(label text)")
    println(result)
  }

  it should "parse mixed-in lists" in {


    val test ="""{{efn|At least three modern medical authors blood volume.
      |* ''See {{harvnb|Vadakan|2005|loc=Footnotes}} for'' Shapiro ''and'' Scheidemandel ''references.''}}""".stripMargin

    val Success(result,_) = wikiPage.parse(test)
    result.toString should not be("")
    println(result)

  }

  it should "parse italics" in {
    val test ="''bleh die bleh''"
    val Success(result,_) = italicContent.parse(test)
    result.toString should not be("")
    println(result)

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

  it should "be able to parse this" in {
    val test =  """<ref>[[#GWarchive|U.S. National Archives:<br>George Washington's Professional Surveys, 2nd prgh]]</ref>"""
    val out = Xml.Xml.Element.parse(test)
    out match {
      case Success(result, _) => {

        result.toString should not be "null"
      }
      case x => WikiParseTest.getFailureString(x) should be("")
    }
  }

  it should "parse this weirdness as content" in {
    val test = """[[#GWarchive|U.S. National Archives:<br>George Washington's Professional Surveys, 2nd prgh]]"""
    val Success(result, _) = Xml.Xml.Content.parse(test)
    println(result)
  }

  it should "parse weird sentences correctly" in {
    val test = """hello<ref name="calendar" group=lower-alpha>Contemporary records</ref>you"""
    val out = chunk.parse(test)
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

    val Success(sentenceResult, _) = chunk.parse("Commanding General of the United States Army")
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


  it should "parse headings" in {
    val test =
      """
        |===test this===
      """.stripMargin
    val Success(result: Heading,_) = heading.parse(test)
    println(result)
    result.level should be(3)
  }


  it should "parse a complext normal or breakout (foundatio of arg)" in {
    val test = """At ({{harvnb|Wallenborn|1997}}, Shapiro 1975, Scheidemandel 1976)"""

    val Success(result,_) = normalOrBreakout.parse(test)
    println(result)
  }

  it should "tolerate whitespace between entries" in {
    val test = "test \n \ntest2"

    val Success(result,_) = wikiPage.parse(test)
    println(result)
  }

  it should "parse this" in {
    val test = """''The Papers of Thomas Jefferson, ''"""

    val Success(result,_) = italicContent.parse(test)
    println(result)
  }

  it should "parse this stuff" in {
    val test = """Washington's height was variously recorded as {{convert|6|ft|m|2|abbr=on}} to {{convert|6|ft|2|in|m|2|abbr=on}},<ref name=UVA.FAQ>{{cite web |title=George Washington, 1732–1799 |date=n.d. |accessdate=May 4, 2015 |website=The Papers of George Washington |url=http://gwpapers.virginia.edu/history/faq/washington/ |publisher=University of Virginia |archiveurl=https://web.archive.org/web/20150330170851/http://gwpapers.virginia.edu/history/faq/washington/ |archivedate=March 30, 2015 |deadurl=no}}</ref> and he had unusually great physical strength that amazed younger men. Jefferson called Washington "the best horseman of his age", and both American and European observers praised his riding; the horsemanship benefited his hunting, a favorite hobby. Washington was an excellent dancer and frequently attended the theater, often referencing Shakespeare in letters.<ref>{{harvnb|Chernow|2010|pp=172–176}}</ref> He drank in moderation and precisely recorded gambling wins and losses, but Washington disliked the excessive drinking, gambling, smoking, and profanity that was common in colonial Virginia. Although he grew tobacco, he eventually stopped smoking, and considered drunkenness a man's worst vice; Washington was glad that post-Revolutionary Virginia society was less likely to "force [guests] to drink and to make it an honor to send them home drunk."<ref>{{harvnb|Chernow|2010|pp=187–189}}</ref>
                 |
                 |Washington suffered from problems with his teeth throughout his life, and historians have tracked his experiences in great detail.<ref>Jennifer Van Horn, "George Washington's Dentures: Disability, Deception, and the Republican Body," '' Early American Studies'' (2016) 14#1</ref> He lost his first adult tooth when he was twenty-two and had only one left by the time he became president.<ref name=Mitchinson>{{Cite book|last = Lloyd|first = John|authorlink=John Lloyd (producer)|last2 = Mitchinson|first2=John|authorlink2=John Mitchinson (researcher)|title = The Book of General Ignorance|publisher=Harmony Books|location=New York|year = 2006|page = 97|url = https://books.google.com/?id=1Mjd2GCRPmAC&pg=PA97|isbn =978-0-307-39491-0|accessdate =July 3, 2011}}</ref> [[John Adams]] claims he lost them because he used them to crack [[Brazil nut]]s but modern historians suggest the [[mercury(II) oxide|mercury oxide]], which he was given to treat illnesses such as smallpox and malaria, probably contributed to the loss. He had several sets of false teeth made, four of them by a dentist named John Greenwood. None of the sets were made from wood. The set made when he became president was carved from hippopotamus and elephant ivory, held together with gold springs.<ref>{{cite journal|url=http://www.americanrevolution.org/dental.html|journal=The Riversdale Letter|title=George Washington—A Dental Victim|accessdate=June 30, 2006|date=Summer–Fall 1998|author=Glover, Barbara}}</ref> Prior to these, he had a set made with real human teeth,<ref>[http://emuseum.mountvernon.org/code/emuseum.asp?style=text&currentrecord=1&page=search&profile=objects&searchdesc=dentures&quicksearch=dentures&sessionid=6C8570F1-F305-4629-A1D2-BF18BB090311&action=quicksearch&style=single&currentrecord=2 Dentures, 1790–1799], George Washington's Mount Vernon Estate, Museum and Gardens</ref> likely ones he purchased from "several unnamed 'Negroes,' presumably Mount Vernon slaves" in 1784.<ref>Mary V. Thompson, [http://www.pbs.org/wgbh/pages/frontline/shows/jefferson/video/lives.html "The Private Life of George Washington's Slaves"], Frontline, PBS</ref> Dental problems left Washington in constant pain, for which he took [[laudanum]].<ref name="The Portrait—George Washington: A National Treasure">{{cite web|url=http://www.georgewashington.si.edu/portrait/face.html |title=The Portrait—George Washington:A National Treasure |publisher=Smithsonian National Portrait Gallery |accessdate=January 21, 2011}}</ref> This distress may be apparent in many of the portraits painted while he was still in office,<ref name="The Portrait—George Washington: A National Treasure"/> including the one still used on the $1 bill.<ref name="Gilbert Stuart"/>{{efn|The Smithsonian Institution states in "The Portrait—George Washington: A National Treasure" that:
                 |:Stuart admired the sculpture of Washington by French artist Jean-Antoine Houdon, probably because it was based on a life mask and therefore extremely accurate. Stuart explained, "When I painted him, he had just had a set of false teeth inserted, which accounts for the constrained expression so noticeable about the mouth and lower part of the face. Houdon's bust does not suffer from this defect. I wanted him as he looked at that time." Stuart preferred the Athenaeum pose and, except for the gaze, used the same pose for the Lansdowne painting.<ref name="The Portrait—George Washington: A National Treasure"/>}}
                 |{{test|url=https://books.google.com/?id=PmAIAAAAQAAJ&pg=PA137}}
                 |[[Category:Free speech activists]]
                 |[[Category:George Washington| ]]
                 |[[Category:Hall of Fame for Great Americans inductees]]
                 |The diagnosis of Washington's final illness and the immediate cause of his death have been subjects of debate since the day he died.<ref name=vadakan/><ref name=mitgang/><ref>{{harvnb|Wallenborn|1999}}; Medical report.</ref> In the days immediately following his death, Craik and Dick's published account stated that they felt his symptoms had been consistent with "''cynanche trachealis''", a term of that period used to describe severe inflammation of the structures of the upper airway.<ref name=mitgang /><ref name=felisati /><ref>{{cite web|title=Doctors Craik and Dick's Account of Washington's Last Illness and Death|url=http://gwpapers.virginia.edu/project/exhibit/mourning/craik.html|archiveurl=https://web.archive.org/web/20060706110550/http://gwpapers.virginia.edu/project/exhibit/mourning/craik.html|archivedate=July 6, 2006|publisher=The Papers of George Washington (University of Virginia)|accessdate=June 1, 2013|first1=James|last1=Craik|first2=Elisha|last2=Dick|date=December 31, 1799}}</ref> Even at that early date, there were accusations of medical malpractice, with some believing that Washington had been bled to death.<ref name=mitgang /><ref name=felisati /> Various modern medical authors have speculated that Washington probably died from a severe case of [[epiglottitis]] which was complicated by the given treatments (all of which were accepted medical practice in Washington's day)—most notably the massive deliberate blood loss, which almost certainly caused [[hypovolemia|hypovolemic shock]].{{efn|At least three modern medical authors ({{harvnb|Wallenborn|1997}}, Shapiro 1975, Scheidemandel 1976) concluded that Washington most probably died from acute bacterial epiglottitis complicated by the administered treatments.  These treatments included multiple doses of [[Mercury(I) chloride|calomel]] (a [[cathartic]] or [[purgative]]), and extensive bloodletting (with at least 2.365 total liters of blood being taken, which is slightly less than half of a normal adult's blood volume).
                 |* ''See {{harvnb|Vadakan|2005|loc=Footnotes}} for'' Shapiro ''and'' Scheidemandel ''references.''  Vadakan's article also directly quotes Doctors Craik and Dick's account (as published in the ''Times of Alexandria'' newspaper) of their treatment of Washington during his fatal illness.}}
                 |[[File:George Washington funeral processions, New York, December 29, 1799.png|thumb|upright|Published regulations for the funeral procession in honor of Washington (in New York City)]]
                 |Throughout the world, people were saddened by Washington's death. In the United States, memorial processions were held in major cities and thousands wore mourning clothes for months. Martha Washington wore a black mourning cape for one year. In France, First Consul [[Napoleon Bonaparte]] ordered ten days of mourning throughout the country;<ref>{{harvnb|Abbott|1860|p=137}}</ref><ref>{{harvnb|Betts|2013|pp=147–150}}</ref>
                 |
                 |{{clear}}
                 |
                 |==Personal life==
                 |
                 |As a young man
                 |
                 |[[Google Art Project.jpg|thumb|''[[Washington Crossing the Delaware]]'']]
                 |
                 |Jefferson had Patsy educated at the [[Pentemont Abbey]]. In 1786, he met and fell in love with [[Maria Cosway]], an accomplished—and married—Italian-English musician of 27. They saw each other frequently over a period of six weeks. She returned to Great Britain, but they maintained a lifelong correspondence.<ref>[[#TJFMariaCosway|TJF: Maria Cosway (Engraving)]]</ref>
                 |
                 |===Secretary of State===
                 |{{See also|First Party System}}
                 |[[File:T Jefferson by Charles Willson Peale 1791 2.jpg|right|200px|thumb|alt=Thomas Jefferson |Thomas Jefferson in 1791 at 49]]
                 |Soon after returning from France, Jefferson accepted Washington's invitation to serve as [[United States Secretary of State|Secretary of State]].<ref>[[#Tucker37|Tucker, 1837]], v.1, p. 334</ref> Jefferson had initially expected to return to France, but Washington insisted that he be on his new [[Cabinet of the United States|Cabinet]].<ref name=Randall_1996_p1>[[#Randall 1996|Randall (1996)]], p. 1</ref> Pressing issues at this time were the national debt and the permanent location of the capital. Jefferson opposed a national debt, preferring that each state retire its own, in contrast to [[United States Secretary of the Treasury|Secretary of the Treasury]] [[Alexander Hamilton]], who desired consolidation of various states' debts by the federal government.<ref>[[#Tucker37|Tucker, 1837]], v.1, pp. 364–69</ref> Hamilton also had bold plans to establish the national credit and a national bank, but Jefferson strenuously opposed this and attempted to undermine his agenda, which nearly led Washington to dismiss him from his cabinet. Jefferson later left the cabinet voluntarily; Washington never forgave him, and never spoke to him again.<ref>[[#Chernow|Chernow, 2004]], p. 427</ref>
                 |
                 |<br><big>'''Primary sources'''</big>
                 |* [https://jeffersonpapers.princeton.edu/ ''The Papers of Thomas Jefferson, '' --the Princeton University Press edition of the correspondence and papers; vol 1 appeared in 1950; vol 41 (covering part of 1803) appeared in 2014.]
                 |* {{cite web |url=http://press-pubs.uchicago.edu/founders/documents/v1ch8s41.html|ref=UCP |title=Thomas Jefferson, Resolutions Relative to the Alien and Sedition Acts |last=Jefferson|first=Thomas|date=November 10, 1798 |work=The Founder's Constitution |publisher=University of Chicago Press |accessdate=November 2, 2015 }}
                 |
                 |
                 |""".stripMargin

    val Success(result, _) =wikiPage.parse(test)

    println(result)

    result should not be("")

  }


  "full document ingestion" should "parse preamble for George washington" in {
    val counts = List(672)
    var memo:WikiPage = null

    for( count <- counts) {
      val lines = WikiParseTest.gwSourceLines(count)

      val out = wikiPage.parse(lines)
      out match {
        case Success(result, _) => {
          memo = result
          result should not be null
          println(result)
        }
        case x => {
          val out = WikiParseTest.getFailureString(x)
          println(out)
          out should be("")
        }
      }

    }
  }

  it should "parse preamble for jefferson" in {
    val counts = List(795)
    var memo:WikiPage = null

    for( count <- counts) {
      val lines = WikiParseTest.gwSourceLines(count, "jefferson")

      val out = wikiPage.parse(lines)
      out match {
        case Success(result, _) => {
          memo = result
          result should not be null
          println(result)
        }
        case x => {
          val out = WikiParseTest.getFailureString(x)
          println(out)
          out should be("")
        }
      }
    }

    println(memo)

  }

  "flatmap test" should "do a thing" in {
    import fastparse.all._
    val t = Option("test")
    val p = P( (!("{" | "]]>")).rep(1) ~ End )
    val p2 = Not("test").rep(min=1).!
//    val Success(result,_) = p2.parse("derp")
    val p3 = !StringIn("tes","blerp")
//    val p4 = StringNotIn("tes","blerp")

    val Success(result,_) = p3.parse("abctes{")
    println(result)

  }
}

object WikiParseTest {
  def getFailureString[T](result: Parsed[T, _, _]) = {
    val tmp = result.asInstanceOf[Parsed.Failure[WikiPage,_]]
    val full = tmp.extra.input
    val idx  = result.index
    val expected = tmp.lastParser.toString
    val buffer = 30
    val end = if(full.length-1 < idx + buffer) full.length -1 else idx + buffer
    val begin = if(0 > idx - buffer) 0 else idx -buffer
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
