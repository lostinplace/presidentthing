package wikiparser

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by cwwhee on 10/26/16.
  */
class ExtractorTest extends FlatSpec with Matchers {
  "extractor" should "print the thing for washington" in {
    PresidentContentExtractor.printOutputforContent("George Washington", "georgewashington")
  }

  "extractor" should "print the thing for jefferson" in {
    PresidentContentExtractor.printOutputforContent("Thomas Jefferson", "jefferson")
  }

  "extractor" should "load my tmp files" in {
    val dir = new File("/tmp/enwiki/presidents/")
    val out = PresidentContentExtractor.getOutFilesFromXMLFolder(dir)
    println(out)
  }

  it should "write my files" in {
    val dir = new File("/output/presfiles/")
    val out = PresidentContentExtractor.getOutFilesFromXMLFolder(dir)
    PresidentContentExtractor.writeContentFiles("/output/contentfiles", out:_*)
  }

  it should "write john adams" in {
    val dir = new File("/output/presfiles/scratch")
    val out = PresidentContentExtractor.getOutFilesFromXMLFolder(dir)
    PresidentContentExtractor.writeContentFiles("/tmp/scratchout", out:_*)
  }

  it should "get trump" in {
    val file = new File("/output/presqueries/djtrump.xml")
    val out = PresidentContentExtractor.getOutputFileFromXmlQueryFile(file)
    PresidentContentExtractor.writeContentFiles("/tmp/scratchout", out)
  }

}
