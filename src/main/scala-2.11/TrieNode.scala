package wikiparser

import scala.collection.immutable.HashMap

case class TrieNode(char:String, parent: TrieNode, isTerminal:Boolean, strings: String*) {
  private var children = initialize

  private val stringVal = getString
  val length = stringVal.length

  private def getString = {
    parent match {
      case null => ""
      case x:TrieNode => x.toString + char
    }
  }

  override def toString: String = getString

  def initialize(): Map[String, TrieNode] = {
    val out = strings.map {_.splitAt(1)}

    val out2 = out.groupBy(_._1).map {
      case (k, v) => {
        val children = v.map(_._2)
        val cleanChildren = children.filter(!_.isEmpty)
        val isTerminal = cleanChildren.length < children.length

        k -> new TrieNode(k, this, isTerminal, cleanChildren :_*)
      }
    }
    out2
  }

  def apply(aChar:String) = children.get(aChar)

}

object TrieNode {
  def generateTrie(strings: String*) = {
    TrieNode("", null, false, strings:_*)
  }

  def queryTrieSet(trieList:Set[TrieNode], char: String): Set[TrieNode] = {
    trieList.flatMap(x=>{
      x(char)
    })
  }
}
