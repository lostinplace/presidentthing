package wikiparser

import scala.io.Source

/**
  * Created by cwwhee on 10/26/16.
  */
object ResourceInterface {
  def ResourceURL(resource:String="georgewashington") = getClass.getResource(s"/$resource.md")
  def SourceLines(resource:String ="georgewashington") = {
    val source = Source.fromURL(ResourceURL(resource))
    try source.mkString finally source.close
  }
}
