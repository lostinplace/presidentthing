import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class XmlFileLoaderTest extends FlatSpec with Matchers {

  "loader" should "load file" in {
    import scala.xml.XML

    val path = new File(".").getAbsolutePath()
    val xml = XML.loadFile("src/test/resources/gcleveland.xml")
    val title:String = (xml \\ "page" \\ "title").text
    val text:String = (xml \\ "page" \\ "revision" \\ "text").text
    println(text)
  }

  "loader" should "load all xml files in folder" in {
    import wikiparser.PresidentContentExtractor._

    val folder = "src/test/resources"

    var fileList = List[File]()

    val d = new File(folder)
    val outFiles = getOutFilesFromXMLFolder(d)
    println(outFiles)

  }

}
