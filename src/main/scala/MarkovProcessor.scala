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
}
