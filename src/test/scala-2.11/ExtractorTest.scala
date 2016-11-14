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
    val dir = new File("/output/presidents/")
    val out = PresidentContentExtractor.getOutFilesFromXMLFolder(dir)
    PresidentContentExtractor.writeContentFiles("/output/presfiles", out:_*)
  }

}
