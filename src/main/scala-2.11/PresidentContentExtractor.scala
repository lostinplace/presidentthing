package wikiparser
import fastparse.core.Parsed.Success
import wikiparser.WikiAST.Expression
import WikiParser._
import WikiAST._

/**
  * Created by cwwhee on 10/26/16.
  */
object PresidentContentExtractor {
  def getContent(expression: Expression) =  {
    val sourceData = ResourceInterface.SourceLines("georgewashington")
    val Success(article,_) = wikiPage.parse(sourceData)
  }

}
