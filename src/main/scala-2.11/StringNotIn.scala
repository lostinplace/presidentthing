package wikiparser

import fastparse.all._
import fastparse.core.{Mutable}


case class StringNotIn(strings: String*) extends Parser[String] {
  import fastparse.core.{ParseCtx}

  private val trie = TrieNode.generateTrie(strings:_*)

  override def toString: String = s"StringNotIn(${strings.mkString(",")})"

  override def parseRec(cfg: ParseCtx[Char, String], index: Int): Mutable[String, Char, String] = {
    val baseSet = Set(trie)

    if(!cfg.input.isReachable(index+1)){
      return fail(cfg.failure, index, Set(this.opaque("end of input")) , false)
    }

    var current = index
    var candidates = Set[TrieNode]()

    while(cfg.input.isReachable(current)) {
      val currentChar = cfg.input.apply(current).toString
      candidates = candidates ++ baseSet
      candidates = TrieNode.queryTrieSet(candidates, currentChar)
      val terminalNode = candidates.find(_.isTerminal)
      terminalNode match {
        case Some(node) => {
          val lastIndex = (current - node.length) +1
          if(lastIndex == index){
            return fail(cfg.failure, index, Set(this.opaque(s"found String($node)")) , false)
          }else {
            return success(cfg.success, cfg.input.slice(index, lastIndex), lastIndex, Set(), false)
          }
        }
        case _ =>
      }
      current += 1
    }
    success(cfg.success, cfg.input.slice(index, current+1), current, Set(), false)
  }
}

object StringNotIn {

  def SNI(strings:String*) = StringNotIn(strings:_*)

}
