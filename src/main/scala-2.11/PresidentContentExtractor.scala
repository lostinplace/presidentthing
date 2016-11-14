package wikiparser
import java.io.File

import fastparse.core.Parsed.{Failure, Success}
import wikiparser.WikiAST.Expression
import WikiParser._
import WikiAST._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import scala.xml.XML

object PresidentContentExtractor {
  var counter = 0;

  def extractor(item: Expression): Option[Any] = {
    val empty = List()
    item match {
      case _:TemplateInvocation => Some(" ")
      case x:Heading => {
        val Some(t) = extractor(x.content)

        Some(s"Section: $t")
      }
      case x:Link if(x.linkType!="File") => {
        val label = if(x.content.length >1) x.content(1) else x.content(0)
        extractor(label) match {
          case Some(t) => Some(s" $t ")
          case x => x
        }
      }
      case x:SentenceFragment => Some(x.toString)
      case x:SimpleFormatted => {
        extractor(x.content) match {
          case Some(s) => Some(s" $s ")
          case x => x
        }
      }
      case x:Parenthetical => {
        Some(" ")
      }
      case x:Chunk => Some(x.content.flatMap(extractor).mkString)
      case _ => None
    }
  }

  case class Section(title:String, start:Int, stop:Int)

  def sectionContent(lines:String*) = {
    var output = ""
    var counter = 1
    var currentSection = Section("Introduction",1,-1)
    val sectionPattern = """Section: (.*)""".r
    var outSections = List[Section]()

    for(line<-lines){
      line match {
        case "" =>
        case sectionPattern(x) => {
          outSections = outSections :+ Section(currentSection.title, currentSection.start, counter-1)
          currentSection = Section(x, counter, -1)
        }
        case x => {
          output += s"$x\n"
          counter += 1
        }
      }
    }
    (output, outSections)
  }

  def getOutputFileFromXmlFile(file: File) = {
    val xml = XML.loadFile(file.getAbsolutePath)
    val title:String = (xml \\ "page" \\ "title").text
    val text:String = (xml \\ "page" \\ "revision" \\ "text").text
    getOutputFileFromFileData(text, title)

  }

  case class ContentFile(id:String, content:String)

  val quotationPattern = """(?<=[^\s])(\.)(?=[^\s])"""

  def fixQuotation(value:String) = value.replaceAll("""(?<=[^\s])(\.)(?=[^\s])""","$1 ")

  def getOutputFileFromFileData(content: String, title: String) = {
    counter +=1
    val article = wikiPage.parse(content) match {
      case Success(x,_) => x
      case Failure(x,y,z) => {
        println(counter)
        throw new Exception(title)
      }
    }

    val result = article.content.flatMap(extractor)
    val (contentRaw, sectionItems) = sectionContent(result.map(x=>fixQuotation(x.toString)):_*)

    val sections = sectionItems.map(x=>{
      val ending =if(contentRaw.length<300)"" else "..."
      val sectionContent = contentRaw.split("\n").slice(x.start,x.stop).mkString("\n").trim()
      val description = s"${sectionContent.take(300)}$ending"
      (
        ("title" -> s"$title: ${x.title}") ~
          ("bounds" -> s"${x.start}-${x.stop}") ~
          ("type" -> "section") ~
          ("description" -> description)
        )
    })

    val id = java.util.UUID.nameUUIDFromBytes(s"uspresidents$title".getBytes).toString

    val dt = java.time.LocalDateTime.now().toString

    val metadata =
      ("alexaDeveloperId" -> "ASKUSSPRESIDENTS7357") ~
        ("publicationDate" -> dt)~
        ("title" -> title) ~
        ("libraryName" -> "Presidents of the United States") ~
        ("documentVersion" -> "v1.0") ~
        ("schemaVersion" -> "0.0.3") ~
        ("source"-> "Data ingested automatically from Wikipedia on September 09, 2016\n© 2005 - 2016 Wikimedia commons.") ~
        ("topics" -> getTopics(title))


    val json =
      ("id" -> id) ~
        ("content" -> contentRaw) ~
        ("annotations" -> sections) ~
        ("metadata" -> metadata)


    ContentFile(id, compact(render(json)))
  }

  def getOutFilesFromXMLFolder(folder: File) = {


    val fileList = folder.listFiles.filter(x=>x.isFile && x.getName.toLowerCase.endsWith("xml")).toList

    fileList.flatMap(x=>{
      try{
        Some(getOutputFileFromXmlFile(x))
      } catch {
        case y:Exception => {
          println(y)
          None
        }
      }

    })
  }

  def getTopics(title: String) = {
    var results = List(title)
    val withoutPeriods = title.replace(".","")
    if(withoutPeriods!=title){
      results = results :+ withoutPeriods
    }

    results = results :+ title.split(" ").last
    results
  }

  def printOutputforContent(name:String, file:String) = {
    val sourceData = ResourceInterface.SourceLines(file)

    val out = getOutputFileFromFileData(sourceData, name)
    println(out.content)
  }

  def writeContentFiles(folder:String, fileList: ContentFile*) = {
    import java.io._

    for(file <- fileList){
      val pw = new PrintWriter(new File(s"$folder/${file.id}.json" ))
      pw.write(file.content)
      pw.close
    }
  }
}
