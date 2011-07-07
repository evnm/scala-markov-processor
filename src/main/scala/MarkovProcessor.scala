package com.evnm.markovprocessor

/*
 * A Markov processor class.
 */
class MarkovProcessor {

}


/*
 * A Markov processor companion object.
 */
object MarkovProcessor {
  /*
   * Produce a list of tuples of the form (List(0, 1, ... , n-1), n) from a
   * given list.
   *
   * Example: groupWords(3, List("a", "b", "c", "d", "e"))
   *          => List((List("a", "b"), "c"), (List("b", "c"), "d"), (List("c", "d"), "e"))
   */
  def groupWords[T](n: Int, lst: List[T]) = {
    val leading_words = for (i <- List.range(0, lst.length - n + 1)) yield lst.slice(i, i + n - 1)
    val completions = lst drop (n - 1)
    leading_words zip completions
  }


  /*
   * Build an Ngram tree from a given string.
   */
  def buildTree(n: Int, text: String): BinaryTree[Ngram] =
    Node.insertList(groupWords(n, text.split("( | \n)+").toList))


  /*
   * Finds a list of leading words within an Ngram tree. Returns an Ngram option.
   */
  def find(words: List[String], tree: BinaryTree[Ngram]): Option[Ngram] = {
    tree match {
      case EmptyNode => None
      case Node(ngram @ Ngram(leading, compls), left, right) => {
        Node.stringListCompare(words, leading) match {
          case -1 => find(words, left)
          case  0 => Some(ngram)
          case  1 => find(words, right)
        }
      }
    }
  }


  /*
   * Returns a list of approximately count words that are randomly generated using the
   * given Ngram tree.
   */
  def buildList(count: Int, tree: BinaryTree[Ngram]): List[String] = {
    tree match {
      case Node(ngram, left, right) => {
        def shift(lst: List[String], n: Int): List[String] = {
          val ngram = find(lst.tail, tree)
          if (!ngram.isDefined || n == 0) lst.init
          else lst.head :: shift(ngram.get.leading_words ::: List(ngram.get.randomWord), n - 1)
        }

        val initial_words = Node.pickRandom(tree)
        shift(initial_words, count - initial_words.length)
      }
      case _ => Nil
    }
  }


}
