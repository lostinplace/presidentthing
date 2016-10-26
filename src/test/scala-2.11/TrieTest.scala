import org.scalatest.{FlatSpec, Matchers}
import wikiparser.TrieNode


class TrieTest extends FlatSpec with Matchers {

  "trie" should "compose itself appropriately" in {

    val myTrie = new TrieNode("", null, false, "test", "team", "art", "axt", "best")

    println(myTrie)

  }

  "trie" should "query alright" in {

    val myTrie = new TrieNode("", null, false, "test", "team", "art", "axt", "best")

    val result1 = TrieNode.queryTrieSet(Set(myTrie), "t")
    val result2 = TrieNode.queryTrieSet(result1, "e")
    val result3 = TrieNode.queryTrieSet(result2, "s")
    val result4 = TrieNode.queryTrieSet(result3, "t")
    val out = result4.diff(result3)
    out.foldLeft(false)(( memo,value)=>{memo || value.isTerminal})should be(true)

    result2.toString should be("te")

  }

}
