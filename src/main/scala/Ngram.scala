package com.evnm.markovprocessor

/*
 * An Ngram class.
 */
class Ngram(val words: List[String], val count: Int, val choices: Map[String, Int]) {
  /*
   * Returns a copy of the Ngram with the argument string added to the list of
   * completions.
   */
  def +(str: String): Ngram = {
    choices get str match {
      case Some(i) => new Ngram(words, count + 1, choices + (str -> (i + 1)))
      case None => new Ngram(words, count + 1, choices + (str -> 1))
    }
  }
}
